const std = @import("std");

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

    const exe_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    exe_module.addImport("parser", parser_module);

    const exe = b.addExecutable(.{
        .name = "yuku",
        .root_module = exe_module,
    });

    b.installArtifact(exe);

    const profiler_module = b.createModule(.{
        .root_source_file = b.path("profiler/main.zig"),
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

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

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

    const wasm_util_module = b.createModule(.{
        .root_source_file = b.path("src/util/root.zig"),
        .target = wasm_target,
        .optimize = .ReleaseSmall,
    });

    const wasm_parser_module = b.createModule(.{
        .root_source_file = b.path("src/parser/root.zig"),
        .target = wasm_target,
        .optimize = .ReleaseSmall,
    });

    wasm_parser_module.addImport("util", wasm_util_module);

    const wasm_module = b.createModule(.{
        .root_source_file = b.path("src/parser/wasm.zig"),
        .target = wasm_target,
        .optimize = .ReleaseSmall,
    });

    wasm_module.addImport("parser", wasm_parser_module);

    const wasm_exe = b.addExecutable(.{
        .name = "yuku",
        .root_module = wasm_module,
    });

    wasm_exe.entry = .disabled;
    wasm_exe.rdynamic = true;
    wasm_exe.initial_memory = 64 * 1024 * 1024; // 64MB initial
    wasm_exe.max_memory = 256 * 1024 * 1024; // 256MB max
    wasm_exe.stack_size = 1024 * 1024;

    b.installArtifact(wasm_exe);
}
