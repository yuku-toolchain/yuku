const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const util_module = b.createModule(.{
        .root_source_file = b.path("src/util/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const enable_source_maps = b.option(
        bool,
        "codegen-source-maps",
        "Compile source-map support into the code generator (default true)",
    ) orelse true;

    const codegen_options = b.addOptions();
    codegen_options.addOption(bool, "source_maps", enable_source_maps);
    const codegen_options_module = codegen_options.createModule();

    const dialect_source = b.option(
        std.Build.LazyPath,
        "dialect",
        "Zig source file of a dialect to compile into the parser (default: none)",
    );

    const parser_module = b.addModule("parser", .{
        .root_source_file = b.path("src/parser/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    _ = addParserImports(b, parser_module, .{
        .util = util_module,
        .codegen_options = codegen_options_module,
        .dialect = dialect_source,
    });

    const gen_unicode_id = b.addExecutable(.{
        .name = "gen-unicode-id",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/gen_unicode_id.zig"),
            .target = b.graph.host,
            .optimize = optimize,
        }),
    });
    const gen_unicode_id_step = b.step("generate-unicode-id", "Generate unicode identifier tables");
    gen_unicode_id_step.dependOn(&b.addRunArtifact(gen_unicode_id).step);

    const tools_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/root.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "util", .module = util_module }},
        }),
    });
    const test_tools_step = b.step("test-tools", "Run the tools tests");
    test_tools_step.dependOn(&b.addRunArtifact(tools_tests).step);

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&b.addRunArtifact(b.addTest(.{ .root_module = util_module })).step);
    test_step.dependOn(&b.addRunArtifact(b.addTest(.{ .root_module = parser_module })).step);

    const zig_tests_module = b.createModule(.{
        .root_source_file = b.path("src/parser/testing/root.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{.{ .name = "parser", .module = parser_module }},
    });
    const run_zig_tests = b.addRunArtifact(b.addTest(.{ .root_module = zig_tests_module }));
    // corpus tests read fixtures relative to the repo root
    run_zig_tests.setCwd(b.path("."));
    test_step.dependOn(&run_zig_tests.step);

    const dialect_parser = b.createModule(.{
        .root_source_file = b.path("src/parser/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    const dialect_tests = addParserImports(b, dialect_parser, .{
        .util = util_module,
        .codegen_options = codegen_options_module,
        .dialect = b.path("src/parser/testing/dialect.zig"),
    });
    test_step.dependOn(&b.addRunArtifact(b.addTest(.{ .root_module = dialect_tests })).step);

    // host tools run fast with every assertion armed
    const safe_util = b.createModule(.{
        .root_source_file = b.path("src/util/root.zig"),
        .target = b.graph.host,
        .optimize = .safe,
    });
    const safe_parser = b.createModule(.{
        .root_source_file = b.path("src/parser/root.zig"),
        .target = b.graph.host,
        .optimize = .safe,
    });
    _ = addParserImports(b, safe_parser, .{
        .util = safe_util,
        .codegen_options = codegen_options_module,
        .dialect = dialect_source,
    });
    const fuzz_exe = b.addExecutable(.{
        .name = "fuzz",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/parser/testing/fuzz/main.zig"),
            .target = b.graph.host,
            .optimize = .safe,
            .imports = &.{.{ .name = "parser", .module = safe_parser }},
        }),
    });
    const run_fuzz = b.addRunArtifact(fuzz_exe);
    run_fuzz.has_side_effects = true;
    const fuzz_step = b.step("fuzz", "Fuzz the JS/TS parser for crashes and memory bugs");
    fuzz_step.dependOn(&run_fuzz.step);

    const codegen_reference = b.addExecutable(.{
        .name = "codegen-reference",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/parser/testing/codegen/reference.zig"),
            .target = b.graph.host,
            .optimize = .safe,
            .imports = &.{.{ .name = "parser", .module = safe_parser }},
        }),
    });
    const codegen_reference_step = b.step(
        "codegen-reference",
        "Build the Zig printer reference the JS printer is checked against",
    );
    codegen_reference_step.dependOn(&b.addInstallArtifact(codegen_reference, .{}).step);

    const profile_target = b.resolveTargetQuery(.{ .cpu_model = .baseline });
    const profile_optimize: std.lang.Optimize = .fast;
    const profile_util = b.createModule(.{
        .root_source_file = b.path("src/util/root.zig"),
        .target = profile_target,
        .optimize = profile_optimize,
    });
    const profile_parser = b.createModule(.{
        .root_source_file = b.path("src/parser/root.zig"),
        .target = profile_target,
        .optimize = profile_optimize,
    });
    _ = addParserImports(b, profile_parser, .{
        .util = profile_util,
        .codegen_options = codegen_options_module,
        .dialect = null,
    });
    const profile_transfer = b.createModule(.{
        .root_source_file = b.path("src/parser/ffi/transfer/root.zig"),
        .target = profile_target,
        .optimize = profile_optimize,
        .imports = &.{.{ .name = "parser", .module = profile_parser }},
    });
    const profile_exe = b.addExecutable(.{
        .name = "profile",
        .root_module = b.createModule(.{
            .root_source_file = b.path("profile/native.zig"),
            .target = profile_target,
            .optimize = profile_optimize,
            // flamegraphs name functions from the debug info
            .strip = false,
            // CodSpeed simulation cannot measure a statically linked executable
            .link_libc = true,
            .imports = &.{
                .{ .name = "parser", .module = profile_parser },
                .{ .name = "transfer", .module = profile_transfer },
            },
        }),
    });
    b.step("profile", "Build the native profile of the Zig core")
        .dependOn(&b.addInstallArtifact(profile_exe, .{}).step);

    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
        .cpu_model = .{ .explicit = &std.Target.wasm.cpu.mvp },
        .cpu_features_add = std.Target.wasm.featureSet(&.{.simd128}),
    });

    const wasm_optimize: std.lang.Optimize = .fast;
    const wasm_util = b.createModule(.{
        .root_source_file = b.path("src/util/root.zig"),
        .target = wasm_target,
        .optimize = wasm_optimize,
    });
    const wasm_parser = b.addModule("parser-wasm", .{
        .root_source_file = b.path("src/parser/root.zig"),
        .target = wasm_target,
        .optimize = wasm_optimize,
    });
    _ = addParserImports(b, wasm_parser, .{
        .util = wasm_util,
        .codegen_options = codegen_options_module,
        .dialect = dialect_source,
    });

    const yuku_wasm = addWasmFrom(b, b, "yuku");
    const wasm = b.addInstallArtifact(yuku_wasm, .{});
    b.step("wasm", "Build the WebAssembly module").dependOn(&wasm.step);

    const main_exe = b.addExecutable(.{
        .name = "yuku",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "parser", .module = parser_module }},
        }),
    });
    b.installArtifact(main_exe);

    const run_step = b.step("run", "Run the src/main.zig toolchain playground");
    run_step.dependOn(&b.addRunArtifact(main_exe).step);

    const ast_transfer_module = b.createModule(.{
        .root_source_file = b.path("src/parser/ffi/transfer/root.zig"),
        .target = b.graph.host,
        .optimize = optimize,
        .imports = &.{.{ .name = "parser", .module = parser_module }},
    });

    const package_files = b.addUpdateSourceFiles();
    package_files.addCopyFileToSource(
        yuku_wasm.getEmittedBin(),
        "packages/yuku/core/yuku.wasm",
    );
    for ([_]struct { name: []const u8, output: []const u8 }{
        .{ .name = "gen_decoder", .output = "packages/yuku/core/decode.js" },
        .{ .name = "gen_walk_tables", .output = "packages/yuku/ast/generated.ts" },
        .{ .name = "gen_token_types", .output = "packages/yuku/types/tokens.ts" },
        .{ .name = "gen_identifier_tables", .output = "packages/yuku/ast/identifier-tables.ts" },
    }) |generator| {
        const generator_exe = b.addExecutable(.{
            .name = generator.name,
            .root_module = b.createModule(.{
                .root_source_file = b.path(b.fmt("tools/{s}.zig", .{generator.name})),
                .target = b.graph.host,
                .optimize = optimize,
                .imports = &.{
                    .{ .name = "parser", .module = parser_module },
                    .{ .name = "transfer", .module = ast_transfer_module },
                },
            }),
        });
        const output = b.addRunArtifact(generator_exe).captureStdOut(.{});
        package_files.addCopyFileToSource(output, generator.output);
    }
    b.step("package", "Write the WebAssembly and generated sources of packages/yuku")
        .dependOn(&package_files.step);
}

const ParserImports = struct {
    util: *std.Build.Module,
    codegen_options: *std.Build.Module,
    dialect: ?std.Build.LazyPath,
};

// a dialect imports the parser that imports it, so every parser gets its own dialect module
fn addParserImports(
    b: *std.Build,
    parser: *std.Build.Module,
    imports: ParserImports,
) *std.Build.Module {
    parser.addImport("util", imports.util);
    parser.addImport("codegen_options", imports.codegen_options);
    const dialect = if (imports.dialect) |source| b.createModule(.{
        .root_source_file = source,
        .target = parser.resolved_target,
        .optimize = parser.optimize,
        .imports = &.{.{ .name = "parser", .module = parser }},
    }) else b.addOptions().createModule();
    parser.addImport("dialect", dialect);
    return dialect;
}

/// Adds the WebAssembly module of a package that compiles a dialect into Yuku, to install as
/// `<name>.wasm` and pass to the `yuku` npm package as its `wasm` option.
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
