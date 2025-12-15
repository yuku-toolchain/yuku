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

    const test262_module = b.createModule(.{
        .root_source_file = b.path("scripts/test262.zig"),
        .target = target,
        .optimize = optimize,
    });

    test262_module.addImport("js", js_module);

    const test262 = b.addExecutable(.{
        .name = "test262",
        .root_module = test262_module,
    });

    b.installArtifact(test262);

    const test262_step = b.step("Run Test262 parser tests suite", "");
    const run_test262 = b.addRunArtifact(test262);
    run_test262.step.dependOn(b.getInstallStep());
    test262_step.dependOn(&run_test262.step);
}
