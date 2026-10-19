const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const codegen_options = b.addOptions();
    codegen_options.addOption(bool, "source_maps", b.option(
        bool,
        "codegen-source-maps",
        "Compile source-map support into the code generator (default true)",
    ) orelse true);
    const dialect = b.option(
        std.Build.LazyPath,
        "dialect",
        "Zig source file of a dialect to compile into the parser (default: none)",
    );
    const config: ParserConfig = .{ .codegen_options = codegen_options.createModule() };

    const util = createUtil(b, target, optimize);
    const parser = config.add(b, "parser", util, dialect);

    const gen_unicode_id = b.addExecutable(.{
        .name = "gen-unicode-id",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/gen_unicode_id.zig"),
            .target = b.graph.host,
            .optimize = optimize,
        }),
    });
    b.step("generate-unicode-id", "Generate unicode identifier tables")
        .dependOn(&b.addRunArtifact(gen_unicode_id).step);

    const tools_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/root.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "util", .module = util }},
        }),
    });
    b.step("test-tools", "Run the tools tests").dependOn(&b.addRunArtifact(tools_tests).step);

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&b.addRunArtifact(b.addTest(.{ .root_module = util })).step);
    test_step.dependOn(&b.addRunArtifact(b.addTest(.{ .root_module = parser })).step);
    const zig_tests = b.addRunArtifact(b.addTest(.{ .root_module = b.createModule(.{
        .root_source_file = b.path("src/parser/testing/root.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{.{ .name = "parser", .module = parser }},
    }) }));
    // corpus tests read fixtures relative to the repo root
    zig_tests.setCwd(b.path("."));
    test_step.dependOn(&zig_tests.step);
    const dialect_parser = config.add(b, null, util, b.path("src/parser/testing/dialect.zig"));
    test_step.dependOn(&b.addRunArtifact(b.addTest(.{
        .root_module = dialect_parser.import_table.get("dialect").?,
    })).step);

    const safe_parser = config.add(b, null, createUtil(b, b.graph.host, .safe), dialect);
    const fuzz = b.addRunArtifact(b.addExecutable(.{
        .name = "fuzz",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/parser/testing/fuzz/main.zig"),
            .target = b.graph.host,
            .optimize = .safe,
            .imports = &.{.{ .name = "parser", .module = safe_parser }},
        }),
    }));
    fuzz.has_side_effects = true;
    b.step("fuzz", "Fuzz the JS/TS parser for crashes and memory bugs").dependOn(&fuzz.step);

    const codegen_reference = b.addExecutable(.{
        .name = "codegen-reference",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/parser/testing/codegen/reference.zig"),
            .target = b.graph.host,
            .optimize = .safe,
            .imports = &.{.{ .name = "parser", .module = safe_parser }},
        }),
    });
    b.step("codegen-reference", "Build the Zig printer reference the JS printer is checked against")
        .dependOn(&b.addInstallArtifact(codegen_reference, .{}).step);

    const profile_target = b.resolveTargetQuery(.{ .cpu_model = .baseline });
    const profile_parser = config.add(b, null, createUtil(b, profile_target, .fast), null);
    const profile = b.addExecutable(.{
        .name = "profile",
        .root_module = b.createModule(.{
            .root_source_file = b.path("profile/native.zig"),
            .target = profile_target,
            .optimize = .fast,
            // flamegraphs name functions from the debug info
            .strip = false,
            // CodSpeed simulation cannot measure a statically linked executable
            .link_libc = true,
            .imports = &.{
                .{ .name = "parser", .module = profile_parser },
                .{ .name = "transfer", .module = createTransfer(b, profile_parser) },
            },
        }),
    });
    b.step("profile", "Build the native profile of the Zig core")
        .dependOn(&b.addInstallArtifact(profile, .{}).step);

    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
        .cpu_model = .{ .explicit = &std.Target.wasm.cpu.mvp },
        .cpu_features_add = std.Target.wasm.featureSet(&.{.simd128}),
    });
    _ = config.add(b, "parser-wasm", createUtil(b, wasm_target, .fast), dialect);
    const yuku_wasm = addWasmFrom(b, b, "yuku");
    b.step("wasm", "Build the WebAssembly module")
        .dependOn(&b.addInstallArtifact(yuku_wasm, .{}).step);

    const main = b.addExecutable(.{
        .name = "yuku",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "parser", .module = parser }},
        }),
    });
    b.installArtifact(main);
    b.step("run", "Run the src/main.zig toolchain playground")
        .dependOn(&b.addRunArtifact(main).step);

    const generator = b.addExecutable(.{
        .name = "generate",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/generate.zig"),
            .target = b.graph.host,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "parser", .module = parser },
                .{ .name = "transfer", .module = createTransfer(b, parser) },
                .{ .name = "util", .module = util },
            },
        }),
    });
    const package_files = b.addUpdateSourceFiles();
    package_files.addCopyFileToSource(yuku_wasm.getEmittedBin(), "packages/yuku/core/yuku.wasm");
    for ([_][2][]const u8{
        .{ "decoder", "packages/yuku/core/decode.js" },
        .{ "walk_tables", "packages/yuku/ast/generated.ts" },
        .{ "token_types", "packages/yuku/types/tokens.ts" },
        .{ "identifier_tables", "packages/yuku/ast/identifier-tables.ts" },
    }) |output| {
        const run = b.addRunArtifact(generator);
        run.addArg(output[0]);
        package_files.addCopyFileToSource(run.captureStdOut(.{}), output[1]);
    }
    b.step("package", "Write the WebAssembly and generated sources of packages/yuku")
        .dependOn(&package_files.step);
}

const ParserConfig = struct {
    codegen_options: *std.Build.Module,

    // a dialect imports the parser that imports it, so every parser gets its own dialect module
    fn add(
        config: ParserConfig,
        b: *std.Build,
        name: ?[]const u8,
        util: *std.Build.Module,
        dialect: ?std.Build.LazyPath,
    ) *std.Build.Module {
        const options: std.Build.Module.CreateOptions = .{
            .root_source_file = b.path("src/parser/root.zig"),
            .target = util.resolved_target,
            .optimize = util.optimize,
            .imports = &.{
                .{ .name = "util", .module = util },
                .{ .name = "codegen_options", .module = config.codegen_options },
            },
        };
        const parser = if (name) |n| b.addModule(n, options) else b.createModule(options);
        parser.addImport("dialect", if (dialect) |source| b.createModule(.{
            .root_source_file = source,
            .target = util.resolved_target,
            .optimize = util.optimize,
            .imports = &.{.{ .name = "parser", .module = parser }},
        }) else b.addOptions().createModule());
        return parser;
    }
};

fn createUtil(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.lang.Optimize,
) *std.Build.Module {
    return b.createModule(.{
        .root_source_file = b.path("src/util/root.zig"),
        .target = target,
        .optimize = optimize,
    });
}

fn createTransfer(b: *std.Build, parser: *std.Build.Module) *std.Build.Module {
    return b.createModule(.{
        .root_source_file = b.path("src/parser/ffi/transfer/root.zig"),
        .target = parser.resolved_target,
        .optimize = parser.optimize,
        .imports = &.{.{ .name = "parser", .module = parser }},
    });
}

/// Builds `<name>.wasm` for the `wasm` option of the `yuku` npm package, with the dialect the
/// `yuku` dependency compiles in.
pub fn addWasm(
    b: *std.Build,
    yuku: *std.Build.Dependency,
    name: []const u8,
) *std.Build.Step.Compile {
    return addWasmFrom(b, yuku.builder, name);
}

fn addWasmFrom(b: *std.Build, yuku: *std.Build, name: []const u8) *std.Build.Step.Compile {
    const parser = yuku.modules.get("parser-wasm").?;
    const wasm = b.addExecutable(.{ .name = name, .root_module = b.createModule(.{
        .root_source_file = yuku.path("src/parser/ffi/wasm.zig"),
        .target = parser.resolved_target.?,
        .optimize = parser.optimize.?,
        .strip = true,
        .imports = &.{.{ .name = "parser", .module = parser }},
    }) });
    wasm.entry = .disabled;
    wasm.rdynamic = true;
    return wasm;
}
