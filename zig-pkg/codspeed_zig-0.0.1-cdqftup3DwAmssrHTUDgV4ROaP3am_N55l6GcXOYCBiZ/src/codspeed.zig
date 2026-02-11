const std = @import("std");

const c = @cImport({
    @cInclude("core.h");
});

pub const Handle = *c.InstrumentHooks;

pub const Session = struct {
    handle: Handle,
    allocator: std.mem.Allocator,

    /// Initializes a managed CodSpeed session.
    pub fn init(allocator: std.mem.Allocator) !Session {
        return .{
            .handle = try initRaw(),
            .allocator = allocator,
        };
    }

    /// Releases resources associated with this session.
    pub fn deinit(self: *Session) void {
        deinitRaw(self.handle);
    }

    /// Returns whether benchmark instrumentation is currently active.
    pub fn isInstrumented(self: *const Session) bool {
        return isInstrumentedRaw(self.handle);
    }

    /// Marks the start of a benchmark sample.
    pub fn startBenchmark(self: *const Session) !void {
        try startBenchmarkRaw(self.handle);
    }

    /// Marks the end of a benchmark sample.
    pub fn stopBenchmark(self: *const Session) !void {
        try stopBenchmarkRaw(self.handle);
    }

    /// Reports the benchmark identifier executed by process `pid`.
    pub fn setExecutedBenchmark(self: *const Session, pid: i32, uri: []const u8) !void {
        const c_uri = try self.allocator.dupeZ(u8, uri);
        defer self.allocator.free(c_uri);
        try setExecutedBenchmarkRaw(self.handle, pid, c_uri);
    }

    /// Reports integration metadata to CodSpeed.
    pub fn setIntegration(self: *const Session, name: []const u8, version: []const u8) !void {
        const c_name = try self.allocator.dupeZ(u8, name);
        defer self.allocator.free(c_name);

        const c_version = try self.allocator.dupeZ(u8, version);
        defer self.allocator.free(c_version);

        try setIntegrationRaw(self.handle, c_name, c_version);
    }

    /// Convenience helper that executes `func` as a benchmark and reports it.
    pub fn bench(self: *const Session, name: []const u8, comptime func: anytype) !void {
        try self.startBenchmark();
        defer self.stopBenchmark() catch {};

        func();

        try self.setExecutedBenchmark(currentPid(), name);
    }
};

/// Initializes a managed session.
pub fn initSession(allocator: std.mem.Allocator) !Session {
    return Session.init(allocator);
}

/// Low-level API: initializes the CodSpeed hook runtime and returns a handle.
///
/// Returns `error.InitFailed` when the underlying hooks fail to initialize.
pub fn init() !Handle {
    return initRaw();
}

/// Low-level API: releases resources associated with `handle`.
///
/// Call once for each successful `init`.
pub fn deinit(handle: Handle) void {
    deinitRaw(handle);
}

/// Low-level API: returns whether benchmark instrumentation is currently active.
pub fn isInstrumented(handle: Handle) bool {
    return isInstrumentedRaw(handle);
}

/// Low-level API: marks the start of a benchmark sample.
///
/// Pair every successful call with `stopBenchmark`.
/// Returns `error.StartFailed` if the hook backend reports a failure.
pub fn startBenchmark(handle: Handle) !void {
    try startBenchmarkRaw(handle);
}

/// Low-level API: marks the end of a benchmark sample.
///
/// Returns `error.StopFailed` if the hook backend reports a failure.
pub fn stopBenchmark(handle: Handle) !void {
    try stopBenchmarkRaw(handle);
}

/// Low-level API: reports the benchmark identifier executed by process `pid`.
pub fn setExecutedBenchmark(handle: Handle, pid: i32, uri: [:0]const u8) !void {
    try setExecutedBenchmarkRaw(handle, pid, uri);
}

/// Low-level API: reports integration metadata to CodSpeed.
pub fn setIntegration(handle: Handle, name: [:0]const u8, version: [:0]const u8) !void {
    try setIntegrationRaw(handle, name, version);
}

/// Low-level API: convenience helper that executes `func` as a benchmark.
///
/// `func` must be callable as `fn() void`.
pub fn bench(handle: Handle, name: [:0]const u8, comptime func: anytype) !void {
    try startBenchmarkRaw(handle);
    defer stopBenchmarkRaw(handle) catch {};

    func();

    try setExecutedBenchmarkRaw(handle, currentPid(), name);
}

fn initRaw() !Handle {
    const handle = c.instrument_hooks_init();
    return handle orelse error.InitFailed;
}

fn deinitRaw(handle: Handle) void {
    c.instrument_hooks_deinit(handle);
}

fn isInstrumentedRaw(handle: Handle) bool {
    return c.instrument_hooks_is_instrumented(handle);
}

fn startBenchmarkRaw(handle: Handle) !void {
    const result = c.instrument_hooks_start_benchmark(handle);
    if (result != 0) return error.StartFailed;
}

fn stopBenchmarkRaw(handle: Handle) !void {
    const result = c.instrument_hooks_stop_benchmark(handle);
    if (result != 0) return error.StopFailed;
}

fn setExecutedBenchmarkRaw(handle: Handle, pid: i32, uri: [:0]const u8) !void {
    const result = c.instrument_hooks_set_executed_benchmark(handle, pid, uri.ptr);
    if (result != 0) return error.SetBenchmarkFailed;
}

fn setIntegrationRaw(handle: Handle, name: [:0]const u8, version: [:0]const u8) !void {
    const result = c.instrument_hooks_set_integration(handle, name.ptr, version.ptr);
    if (result != 0) return error.SetIntegrationFailed;
}

fn currentPid() i32 {
    return @intCast(std.os.linux.getpid());
}

test "low-level init and deinit" {
    const handle = try init();
    defer deinit(handle);
}

test "low-level isInstrumented" {
    const handle = try init();
    defer deinit(handle);

    const instrumented = isInstrumented(handle);
    try std.testing.expectEqual(@TypeOf(instrumented), bool);
}

test "low-level start and stop benchmark" {
    const handle = try init();
    defer deinit(handle);

    try startBenchmark(handle);
    try stopBenchmark(handle);
}

test "low-level setExecutedBenchmark" {
    const handle = try init();
    defer deinit(handle);

    try setExecutedBenchmark(handle, 1234, "test_benchmark");
}

test "low-level setIntegration" {
    const handle = try init();
    defer deinit(handle);

    try setIntegration(handle, "zig-integration", "1.0.0");
}

test "low-level bench convenience function" {
    const handle = try init();
    defer deinit(handle);

    try bench(handle, "test_bench", struct {
        fn func() void {
            _ = @as(u64, 1 + 2);
        }
    }.func);
}

test "session init and deinit" {
    var session = try initSession(std.testing.allocator);
    defer session.deinit();
}

test "session setIntegration and bench" {
    var session = try initSession(std.testing.allocator);
    defer session.deinit();

    const integration_name: []const u8 = "zig";
    const benchmark_name: []const u8 = "bench/slice_input";
    try session.setIntegration(integration_name, "1.0.0");
    try session.bench(benchmark_name, struct {
        fn func() void {
            _ = @as(u64, 3 + 4);
        }
    }.func);
}
