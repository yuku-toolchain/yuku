const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const exe_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "yuku",
        .root_module = exe_module,
    });

    b.installArtifact(exe);

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
        .name = "generate-unicode-id",
        .root_module = b.createModule(.{
            .root_source_file = b.path("scripts/generate-unicode-id.zig"),
            .target = b.graph.host,
            .optimize = b.standardOptimizeOption(.{
                .preferred_optimize_mode = std.builtin.OptimizeMode.ReleaseFast,
            }),
        }),
    });

    const run_gen_unicode_id_table = b.addRunArtifact(gen_unicode_id_table);
    const gen_unicode_id_table_step = b.step("generate-unicode-id", "Run unicode identifier table and utils generation");
    gen_unicode_id_table_step.dependOn(&run_gen_unicode_id_table.step);

    const test_runner_module = b.createModule(.{
        .root_source_file = b.path("scripts/test-runner.zig"),
        .target = target,
        .optimize = optimize,
    });

    test_runner_module.addImport("js", js_module);

    const test_runner = b.addExecutable(.{
        .name = "test-runner",
        .root_module = test_runner_module,
    });

    b.installArtifact(test_runner);

    const test_step = b.step("test", "Run parser snapshot tests");

    const run_test = b.addRunArtifact(test_runner);

    run_test.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_test.addArgs(args);
    }

    test_step.dependOn(&run_test.step);
}
