const std = @import("std");

const name = "yuku";

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = name,
        .root_module = exe_module,
    });

    b.installArtifact(exe);

    const lib_module = b.createModule(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lib = b.addLibrary(.{
        .name = name,
        .root_module = lib_module,
        .linkage = .static,
    });

    b.installArtifact(lib);

    const util_module = b.addModule("util", .{
        .root_source_file = b.path("src/util/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const js_module = b.addModule("js", .{
        .root_source_file = b.path("src/js/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    lib_module.addImport("js", js_module);
    js_module.addImport("util", util_module);
    exe_module.addImport("js", js_module);

    const run_step = b.step("run", "Run the app");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const gen_unicode_id_table = b.addExecutable(.{
        .name = "gen-unicode-id",
        .root_module = b.createModule(.{
            .root_source_file = b.path("scripts/gen_unicode_id.zig"),
            .target = b.graph.host,
            .optimize = optimize,
        }),
    });

    const run_gen_unicode_id_table = b.addRunArtifact(gen_unicode_id_table);
    const gen_unicode_id_table_step = b.step("generate-unicode-id", "Run unicode identifier table and utils generation");
    gen_unicode_id_table_step.dependOn(&run_gen_unicode_id_table.step);

    {
        const wasm_target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
            .cpu_features_add = std.Target.wasm.featureSet(&.{
                .bulk_memory,
                .mutable_globals,
                .nontrapping_fptoint,
                .sign_ext,
            }),
        });

        const wasm_util_module = b.addModule("util", .{
            .root_source_file = b.path("src/util/root.zig"),
            .target = wasm_target,
            .optimize = .ReleaseSmall,
        });

        const wasm_js_module = b.addModule("js", .{
            .root_source_file = b.path("src/js/root.zig"),
            .target = wasm_target,
            .optimize = .ReleaseSmall,
        });

        wasm_js_module.addImport("util", wasm_util_module);

        const wasm_module = b.createModule(.{
            .root_source_file = b.path("src/wasm.zig"),
            .target = wasm_target,
            .optimize = .ReleaseSmall,
        });

        wasm_module.addImport("js", wasm_js_module);

        const js_wasm = b.addExecutable(.{
            .name = name,
            .root_module = wasm_module,
        });

        js_wasm.entry = .disabled;
        js_wasm.rdynamic = true;
        js_wasm.initial_memory = 64 * 1024 * 1024; // 64MB initial
        js_wasm.max_memory = 256 * 1024 * 1024; // 256MB max
        js_wasm.stack_size = 1024 * 1024;

        b.installArtifact(js_wasm);
        const js_wasm_file = b.addInstallFile(js_wasm.getEmittedBin(), js_wasm.out_filename);
        b.getInstallStep().dependOn(&js_wasm_file.step);
    }
}
