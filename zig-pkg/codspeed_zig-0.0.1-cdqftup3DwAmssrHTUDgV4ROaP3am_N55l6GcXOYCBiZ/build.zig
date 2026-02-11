const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const update_deps_step = b.step("update-deps", "Update vendored dependencies from instrument-hooks");
    update_deps_step.dependOn(&createUpdateDepsStep(b).step);

    const codspeed = b.addModule("codspeed", .{
        .root_source_file = b.path("src/codspeed.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    codspeed.addCSourceFiles(.{ .files = &.{"vendor/core.c"}, .flags = &.{ "-std=c11", "-Wno-format" } });
    codspeed.addIncludePath(b.path("vendor"));

    const test_module = b.createModule(.{
        .root_source_file = b.path("src/codspeed.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    test_module.addCSourceFiles(.{ .files = &.{"vendor/core.c"}, .flags = &.{ "-std=c11", "-Wno-format" } });
    test_module.addIncludePath(b.path("vendor"));

    const tests = b.addTest(.{
        .root_module = test_module,
    });

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&b.addRunArtifact(tests).step);

    const smoke_module = b.createModule(.{
        .root_source_file = b.path("ci/profiler_smoke.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    smoke_module.addImport("codspeed", codspeed);

    const smoke_exe = b.addExecutable(.{
        .name = "profiler-smoke",
        .root_module = smoke_module,
    });

    const smoke_step = b.step("profile-smoke", "Compile profiler smoke executable");
    smoke_step.dependOn(&smoke_exe.step);
}

fn createUpdateDepsStep(b: *std.Build) *std.Build.Step.Run {
    const repo = "CodSpeedHQ/instrument-hooks";
    const version = b.option([]const u8, "version", "Version to fetch") orelse "main";

    return b.addSystemCommand(&.{
        "bash", "-c",
        std.fmt.allocPrint(b.allocator,
            \\VERSION={s} bash -c '
            \\set -e
            \\mkdir -p vendor
            \\BASE_URL="https://raw.githubusercontent.com/{s}/$VERSION"
            \\curl -fsSL "$BASE_URL/dist/core.c" -o vendor/core.c
            \\curl -fsSL "$BASE_URL/includes/core.h" -o vendor/core.h
            \\curl -fsSL "$BASE_URL/includes/callgrind.h" -o vendor/callgrind.h
            \\curl -fsSL "$BASE_URL/includes/valgrind.h" -o vendor/valgrind.h
            \\curl -fsSL "$BASE_URL/includes/compat.h" -o vendor/compat.h
            \\curl -fsSL "$BASE_URL/includes/zig.h" -o vendor/zig.h
            \\# Avoid glibc preprocessing failures in native Linux builds.
            \\if [ "$(head -n 1 vendor/zig.h)" = "#undef linux" ]; then
            \\  tail -n +2 vendor/zig.h > vendor/zig.h.tmp
            \\  mv vendor/zig.h.tmp vendor/zig.h
            \\fi
            \\echo "$VERSION" > vendor/VERSION
            \\echo "✅ Updated to $VERSION"
            \\'
        , .{version, repo}) catch @panic("OOM"),
    });
}
