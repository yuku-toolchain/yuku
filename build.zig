const std = @import("std");
const napi_zig = @import("napi_zig");

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

    const parser_module = b.addModule("parser", .{
        .root_source_file = b.path("src/parser/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    parser_module.addImport("util", util_module);
    parser_module.addImport("codegen_options", codegen_options.createModule());

    const gen_unicode_id_table = b.addExecutable(.{
        .name = "gen-unicode-id",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/gen_unicode_id.zig"),
            .target = b.graph.host,
            .optimize = optimize,
        }),
    });

    const run_gen_unicode_id_table = b.addRunArtifact(gen_unicode_id_table);
    const gen_unicode_id_table_step = b.step("generate-unicode-id", "Generate unicode identifier tables");
    gen_unicode_id_table_step.dependOn(&run_gen_unicode_id_table.step);

    const tools_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/root.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    tools_tests.root_module.addImport("util", util_module);

    const run_tools_tests = b.addRunArtifact(tools_tests);
    // tools test needs network connection
    const test_tools_step = b.step("test-tools", "Run the tools tests");
    test_tools_step.dependOn(&run_tools_tests.step);

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&b.addRunArtifact(b.addTest(.{ .root_module = util_module })).step);
    test_step.dependOn(&b.addRunArtifact(b.addTest(.{ .root_module = parser_module })).step);

    const zig_tests_module = b.createModule(.{
        .root_source_file = b.path("src/parser/testing/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    zig_tests_module.addImport("parser", parser_module);
    const run_zig_tests = b.addRunArtifact(b.addTest(.{ .root_module = zig_tests_module }));
    // corpus-driven tests read test/parser/suite relative to the repo root
    run_zig_tests.setCwd(b.path("."));
    test_step.dependOn(&run_zig_tests.step);

    const fuzz_util = b.createModule(.{
        .root_source_file = b.path("src/util/root.zig"),
        .target = b.graph.host,
        .optimize = .ReleaseSafe,
    });
    const fuzz_parser = b.createModule(.{
        .root_source_file = b.path("src/parser/root.zig"),
        .target = b.graph.host,
        .optimize = .ReleaseSafe,
    });
    fuzz_parser.addImport("util", fuzz_util);
    fuzz_parser.addImport("codegen_options", codegen_options.createModule());
    const fuzz_driver = b.createModule(.{
        .root_source_file = b.path("src/parser/testing/fuzz/main.zig"),
        .target = b.graph.host,
        .optimize = .ReleaseSafe,
    });
    fuzz_driver.addImport("parser", fuzz_parser);
    const fuzz_exe = b.addExecutable(.{ .name = "fuzz", .root_module = fuzz_driver });
    const run_fuzz = b.addRunArtifact(fuzz_exe);
    run_fuzz.has_side_effects = true;
    const fuzz_step = b.step("fuzz", "Fuzz the JS/TS parser for crashes and memory bugs");
    fuzz_step.dependOn(&run_fuzz.step);

    const napi_dep = b.dependency("napi_zig", .{});

    napi_zig.addLib(b, napi_dep, .{
        .name = "yuku-parser",
        .root = b.path("src/parser/ffi/parser.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "parser", .module = parser_module },
        },
        .npm = .{
            .scope = "@yuku-parser",
            .description = "High-performance JavaScript/TypeScript parser written in Zig",
            .dts = .{
                .file = b.path("src/parser/ffi/parser.d.ts"),
            },
            .repository = "https://github.com/yuku-toolchain/yuku",
        },
    });

    napi_zig.addLib(b, napi_dep, .{
        .name = "yuku-codegen",
        .root = b.path("src/parser/ffi/codegen.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "parser", .module = parser_module },
        },
        .npm = .{
            .scope = "@yuku-codegen",
            .description = "High-performance JavaScript/TypeScript code generator written in Zig",
            .dts = .{
                .file = b.path("src/parser/ffi/codegen.d.ts"),
            },
            .repository = "https://github.com/yuku-toolchain/yuku",
        },
    });

    napi_zig.addLib(b, napi_dep, .{
        .name = "yuku-analyzer",
        .root = b.path("src/parser/ffi/analyzer.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "parser", .module = parser_module },
        },
        .npm = .{
            .scope = "@yuku-analyzer",
            .description = "High-performance JavaScript/TypeScript semantic analyzer written in Zig",
            .dts = .{
                .file = b.path("src/parser/ffi/analyzer.d.ts"),
            },
            .repository = "https://github.com/yuku-toolchain/yuku",
        },
    });

    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
        .cpu_features_add = std.Target.wasm.featureSet(&.{
            .bulk_memory,
            .nontrapping_fptoint,
            .sign_ext,
            .simd128,
        }),
    });
    const wasm_step = b.step("wasm", "Build the WebAssembly modules");

    const wasm_transfer_module = b.createModule(.{
        .root_source_file = b.path("src/parser/ffi/transfer/root.zig"),
        .target = wasm_target,
        .optimize = .ReleaseSmall,
    });
    wasm_transfer_module.addImport("parser", parser_module);

    inline for ([_]struct { name: []const u8, root: []const u8 }{
        .{ .name = "yuku-parser", .root = "src/parser/ffi/wasm/parser.zig" },
        .{ .name = "yuku-codegen", .root = "src/parser/ffi/wasm/codegen.zig" },
    }) |cfg| {
        const wasm_module = b.createModule(.{
            .root_source_file = b.path(cfg.root),
            .target = wasm_target,
            .optimize = .ReleaseSmall,
            .strip = true,
        });
        wasm_module.addImport("parser", parser_module);
        wasm_module.addImport("transfer", wasm_transfer_module);

        const wasm = b.addExecutable(.{ .name = cfg.name, .root_module = wasm_module });
        wasm.entry = .disabled;
        wasm.rdynamic = true;
        wasm_step.dependOn(&b.addInstallArtifact(wasm, .{}).step);
    }

    const main_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    main_module.addImport("parser", parser_module);

    const main_exe = b.addExecutable(.{ .name = "yuku", .root_module = main_module });
    b.installArtifact(main_exe);

    const run_cmd = b.addRunArtifact(main_exe);
    const run_step = b.step("run", "Run the src/main.zig toolchain playground");
    run_step.dependOn(&run_cmd.step);

    const ast_transfer_module = b.createModule(.{
        .root_source_file = b.path("src/parser/ffi/transfer/root.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    ast_transfer_module.addImport("parser", parser_module);

    inline for ([_]struct {
        step: []const u8,
        description: []const u8,
        root: []const u8,
        output: []const u8,
    }{
        .{
            .step = "gen-parser-decoder",
            .description = "Generate decode.js for yuku-parser",
            .root = "tools/gen_parser_decoder.zig",
            .output = "decode.js",
        },
        .{
            .step = "gen-analyzer-decoder",
            .description = "Generate decode-analyzer.js for yuku-analyzer",
            .root = "tools/gen_analyzer_decoder.zig",
            .output = "decode-analyzer.js",
        },
        .{
            .step = "gen-codegen-encoder",
            .description = "Generate encode.js for yuku-codegen",
            .root = "tools/gen_codegen_encoder.zig",
            .output = "encode.js",
        },
        .{
            .step = "gen-walk-tables",
            .description = "Generate generated.ts for yuku-ast",
            .root = "tools/gen_walk_tables.zig",
            .output = "walk-tables.ts",
        },
    }) |cfg| {
        const generator_module = b.createModule(.{
            .root_source_file = b.path(cfg.root),
            .target = b.graph.host,
            .optimize = optimize,
        });
        generator_module.addImport("parser", parser_module);
        generator_module.addImport("transfer", ast_transfer_module);

        const generator_exe = b.addExecutable(.{
            .name = cfg.step,
            .root_module = generator_module,
        });

        const run_generator = b.addRunArtifact(generator_exe);
        const generator_output = run_generator.captureStdOut(.{});

        const generator_step = b.step(cfg.step, cfg.description);
        generator_step.dependOn(&b.addInstallFile(generator_output, cfg.output).step);
    }
}
