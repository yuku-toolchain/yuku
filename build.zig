const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const util_module = b.createModule(.{
        .root_source_file = b.path("src/util/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const js_module = b.createModule(.{
        .root_source_file = b.path("src/js/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    js_module.addImport("util", util_module);

    const yuku_module = b.addModule("yuku", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    yuku_module.addImport("js", js_module);

    const exe_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    exe_module.addImport("js", js_module);

    const exe = b.addExecutable(.{
        .name = "yuku",
        .root_module = exe_module,
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const gen_unicode_id_table = b.addExecutable(.{
        .name = "gen-unicode-id",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/tools/gen_unicode_id.zig"),
            .target = b.graph.host,
            .optimize = optimize,
        }),
    });

    const run_gen_unicode_id_table = b.addRunArtifact(gen_unicode_id_table);
    const gen_unicode_id_table_step = b.step("generate-unicode-id", "Generate unicode identifier tables");
    gen_unicode_id_table_step.dependOn(&run_gen_unicode_id_table.step);

    const tools_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/tools/root.zig"),
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

    const wasm_js_module = b.createModule(.{
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
