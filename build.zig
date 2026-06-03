const std = @import("std");
const napi_zig = @import("napi_zig");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const codspeed_dep = b.dependency("codspeed_zig", .{
        .target = target,
        .optimize = optimize,
    });

    const util_module = b.createModule(.{
        .root_source_file = b.path("src/util/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const parser_module = b.addModule("parser", .{
        .root_source_file = b.path("src/parser/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    parser_module.addImport("util", util_module);

    const profiler_module = b.createModule(.{
        .root_source_file = b.path("profiler/profile.zig"),
        .target = target,
        .optimize = optimize,
    });

    profiler_module.addImport("parser", parser_module);

    profiler_module.addImport("codspeed", codspeed_dep.module("codspeed"));

    const profiler_exe = b.addExecutable(.{
        .name = "profiler",
        .root_module = profiler_module,
    });

    b.installArtifact(profiler_exe);

    const profile_cmd = b.addRunArtifact(profiler_exe);
    if (b.args) |args| {
        profile_cmd.addArgs(args);
    }

    const profile_step = b.step("profile", "Run profiler");
    profile_step.dependOn(&profile_cmd.step);

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

    const fuzz_module = b.createModule(.{
        .root_source_file = b.path("src/parser/fuzz.zig"),
        .target = target,
        .optimize = optimize,
    });
    fuzz_module.addImport("util", util_module);

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&b.addRunArtifact(b.addTest(.{ .root_module = util_module })).step);
    test_step.dependOn(&b.addRunArtifact(b.addTest(.{ .root_module = fuzz_module })).step);
    test_step.dependOn(&b.addRunArtifact(b.addTest(.{ .root_module = parser_module })).step);

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

    inline for ([_]struct { name: []const u8, root: []const u8 }{
        .{ .name = "yuku-parser", .root = "src/parser/ffi/parser_wasm.zig" },
        .{ .name = "yuku-codegen", .root = "src/parser/ffi/codegen_wasm.zig" },
    }) |cfg| {
        const wasm_module = b.createModule(.{
            .root_source_file = b.path(cfg.root),
            .target = wasm_target,
            .optimize = .ReleaseSmall,
            .strip = true,
        });
        wasm_module.addImport("parser", parser_module);

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
    const run_step = b.step("run", "Parse a sample, walk the AST, and print");
    run_step.dependOn(&run_cmd.step);

    // estree decoder codegen
    const ast_transfer_module = b.createModule(.{
        .root_source_file = b.path("src/parser/ffi/transfer.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    ast_transfer_module.addImport("parser", parser_module);

    const gen_estree_module = b.createModule(.{
        .root_source_file = b.path("tools/gen_estree_decoder.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    gen_estree_module.addImport("parser", parser_module);
    gen_estree_module.addImport("transfer", ast_transfer_module);

    const gen_estree_exe = b.addExecutable(.{
        .name = "gen-estree-decoder",
        .root_module = gen_estree_module,
    });

    const run_gen_estree = b.addRunArtifact(gen_estree_exe);
    const gen_estree_output = run_gen_estree.captureStdOut(.{});

    const gen_estree_step = b.step("gen-estree-decoder", "Generate decode.js ESTree decoder from AST types");
    gen_estree_step.dependOn(&b.addInstallFile(gen_estree_output, "decode.js").step);

    // estree encoder codegen (mirror of the decoder, walks an ESTree AST into a v7 buffer).
    const estree_meta_module = b.createModule(.{
        .root_source_file = b.path("tools/estree_meta.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    estree_meta_module.addImport("parser", parser_module);

    gen_estree_module.addImport("estree_meta", estree_meta_module);

    const gen_estree_encoder_module = b.createModule(.{
        .root_source_file = b.path("tools/gen_estree_encoder.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    gen_estree_encoder_module.addImport("parser", parser_module);
    gen_estree_encoder_module.addImport("transfer", ast_transfer_module);
    gen_estree_encoder_module.addImport("estree_meta", estree_meta_module);

    const gen_estree_encoder_exe = b.addExecutable(.{
        .name = "gen-estree-encoder",
        .root_module = gen_estree_encoder_module,
    });

    const run_gen_estree_encoder = b.addRunArtifact(gen_estree_encoder_exe);
    const gen_estree_encoder_output = run_gen_estree_encoder.captureStdOut(.{});

    const gen_estree_encoder_step = b.step("gen-estree-encoder", "Generate encode.js ESTree encoder from AST types");
    gen_estree_encoder_step.dependOn(&b.addInstallFile(gen_estree_encoder_output, "encode.js").step);
}
