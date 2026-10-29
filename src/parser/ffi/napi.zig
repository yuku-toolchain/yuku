const std = @import("std");
const builtin = @import("builtin");
const entries = @import("entries.zig");
const transfer = @import("transfer/root.zig");

const Error = transfer.Error || error{NodeApi};

const Env = *opaque {};
const Value = *opaque {};
const CallbackInfo = *opaque {};
const Status = enum(c_int) { ok = 0, _ };
const Callback = *const fn (Env, CallbackInfo) callconv(.c) ?Value;

const Api = struct {
    napi_create_function: *const fn (
        Env,
        [*]const u8,
        usize,
        Callback,
        ?*anyopaque,
        *Value,
    ) callconv(.c) Status,
    napi_set_named_property: *const fn (Env, Value, [*:0]const u8, Value) callconv(.c) Status,
    napi_get_cb_info: *const fn (
        Env,
        CallbackInfo,
        *usize,
        [*]Value,
        ?*Value,
        ?*?*anyopaque,
    ) callconv(.c) Status,
    napi_get_value_string_utf16: *const fn (
        Env,
        Value,
        ?[*]u16,
        usize,
        ?*usize,
    ) callconv(.c) Status,
    napi_get_value_uint32: *const fn (Env, Value, *u32) callconv(.c) Status,
    napi_create_arraybuffer: *const fn (Env, usize, *?*anyopaque, *Value) callconv(.c) Status,
    napi_throw_error: *const fn (Env, ?[*:0]const u8, [*:0]const u8) callconv(.c) Status,
    napi_get_typedarray_info: *const fn (
        Env,
        Value,
        ?*c_int,
        *usize,
        *?*anyopaque,
        ?*Value,
        ?*usize,
    ) callconv(.c) Status,
};

/// Arenas that calls on any thread borrow and return, so each call reuses the memory of an
/// earlier one.
const pool = struct {
    const arenas_max = 64;
    const retained_bytes_max = 128 * 1024 * 1024;
    const lock_attempts_max = 1024;

    var locked: std.atomic.Value(bool) = .init(false);
    var arenas: [arenas_max]*std.heap.ArenaAllocator = undefined;
    var arenas_len: u32 = 0;

    fn lock() bool {
        for (0..lock_attempts_max) |_| {
            if (locked.cmpxchgWeak(false, true, .acquire, .monotonic) == null) return true;
            std.atomic.spinLoopHint();
        }
        return false;
    }

    fn unlock() void {
        std.debug.assert(locked.load(.monotonic));
        locked.store(false, .release);
    }

    fn take() std.mem.Allocator.Error!*std.heap.ArenaAllocator {
        if (lock()) {
            defer unlock();
            std.debug.assert(arenas_len <= arenas_max);
            if (arenas_len > 0) {
                arenas_len -= 1;
                return arenas[arenas_len];
            }
        }
        const arena = try std.heap.page_allocator.create(std.heap.ArenaAllocator);
        arena.* = .init(std.heap.page_allocator);
        return arena;
    }

    fn give(arena: *std.heap.ArenaAllocator) void {
        _ = arena.reset(.{ .retain_with_limit = retained_bytes_max });
        if (lock()) {
            defer unlock();
            std.debug.assert(arenas_len <= arenas_max);
            if (arenas_len < arenas_max) {
                arenas[arenas_len] = arena;
                arenas_len += 1;
                return;
            }
        }
        arena.deinit();
        std.heap.page_allocator.destroy(arena);
    }
};

const windows = struct {
    const HMODULE = std.os.windows.HMODULE;

    extern "kernel32" fn GetModuleHandleW(name: ?[*:0]const u16) callconv(.winapi) ?HMODULE;
    extern "kernel32" fn GetProcAddress(
        module: HMODULE,
        name: [*:0]const u8,
    ) callconv(.winapi) ?*const anyopaque;

    const unbound = 0;
    const binding = 1;
    const bound = 2;

    var api: Api = undefined;
    var api_state: std.atomic.Value(u8) = .init(unbound);
};

fn bindApi() error{NodeApi}!Api {
    const fields = @typeInfo(Api).@"struct";
    var api: Api = undefined;
    if (builtin.os.tag != .windows) {
        inline for (fields.field_names, fields.field_types) |name, Function| {
            @field(api, name) = @extern(Function, .{ .name = name });
        }
        return api;
    }
    if (windows.api_state.load(.acquire) == windows.bound) return windows.api;
    const host = windows.GetModuleHandleW(null) orelse return error.NodeApi;
    inline for (fields.field_names) |name| {
        const address = windows.GetProcAddress(host, name) orelse return error.NodeApi;
        @field(api, name) = @ptrCast(@alignCast(address));
    }
    const claim = windows.api_state.cmpxchgStrong(
        windows.unbound,
        windows.binding,
        .acquire,
        .monotonic,
    );
    if (claim == null) {
        windows.api = api;
        windows.api_state.store(windows.bound, .release);
    }
    return api;
}

export fn napi_register_module_v1(env: Env, exports: Value) ?Value {
    register(env, exports) catch return null;
    return exports;
}

fn register(env: Env, exports: Value) Error!void {
    const api = try bindApi();
    try define(&api, env, exports, "parse", withSource(entries.parse));
    try define(&api, env, exports, "analyze", withSource(entries.analyze));
    try define(&api, env, exports, "printTree", printTree);
    try define(&api, env, exports, "analyzeTree", analyzeTree);
}

fn define(
    api: *const Api,
    env: Env,
    exports: Value,
    comptime name: [:0]const u8,
    comptime run: Run,
) Error!void {
    var function: Value = undefined;
    try check(api.napi_create_function(env, name.ptr, name.len, callback(run), null, &function));
    try check(api.napi_set_named_property(env, exports, name.ptr, function));
}

fn check(status: Status) error{NodeApi}!void {
    if (status != .ok) return error.NodeApi;
}

const Run = fn (*const Api, std.mem.Allocator, Env, CallbackInfo) Error!Value;

fn callback(comptime run: Run) Callback {
    return struct {
        fn call(env: Env, info: CallbackInfo) callconv(.c) ?Value {
            const api = bindApi() catch return null;
            const arena = pool.take() catch {
                _ = api.napi_throw_error(env, null, "yuku: out of memory");
                return null;
            };
            defer pool.give(arena);
            return run(&api, arena.allocator(), env, info) catch |err| {
                const message = switch (err) {
                    error.OutOfMemory => "yuku: out of memory",
                    error.NodeApi => "yuku: a Node-API call failed",
                    error.InvalidTree => "yuku: the tree is malformed",
                };
                _ = api.napi_throw_error(env, null, message);
                return null;
            };
        }
    }.call;
}

fn withSource(comptime entry: anytype) Run {
    return struct {
        fn run(api: *const Api, gpa: std.mem.Allocator, env: Env, info: CallbackInfo) Error!Value {
            const argv = try argsOf(2, api, env, info);
            var output: Output = .{ .api = api, .env = env };
            const source = try sourceOf(api, gpa, env, argv[0]);
            _ = try entry(gpa, source, try uint32Of(api, env, argv[1]), &output);
            return output.value.?;
        }
    }.run;
}

fn printTree(api: *const Api, gpa: std.mem.Allocator, env: Env, info: CallbackInfo) Error!Value {
    const argv = try argsOf(7, api, env, info);
    var output: Output = .{ .api = api, .env = env };
    const source = try sourceOf(api, gpa, env, argv[4]);
    const flags = try uint32Of(api, env, argv[5]);
    const options = try uint32Of(api, env, argv[6]);
    _ = try entries.printTree(gpa, try encodedOf(api, env, &argv), source, flags, options, &output);
    return output.value.?;
}

fn analyzeTree(api: *const Api, gpa: std.mem.Allocator, env: Env, info: CallbackInfo) Error!Value {
    const argv = try argsOf(5, api, env, info);
    var output: Output = .{ .api = api, .env = env };
    const flags = try uint32Of(api, env, argv[4]);
    _ = try entries.analyzeTree(gpa, try encodedOf(api, env, &argv), flags, &output);
    return output.value.?;
}

const Output = struct {
    api: *const Api,
    env: Env,
    value: ?Value = null,

    pub fn bytes(output: *Output, size: usize) Error![]u8 {
        std.debug.assert(size > 0);
        var data: ?*anyopaque = null;
        var value: Value = undefined;
        try check(output.api.napi_create_arraybuffer(output.env, size, &data, &value));
        const bytes_out: [*]u8 = @ptrCast(data orelse return error.OutOfMemory);
        output.value = value;
        return bytes_out[0..size];
    }
};

fn argsOf(comptime count: usize, api: *const Api, env: Env, info: CallbackInfo) Error![count]Value {
    var argc: usize = count;
    var argv: [count]Value = undefined;
    try check(api.napi_get_cb_info(env, info, &argc, &argv, null, null));
    std.debug.assert(argc == count);
    return argv;
}

fn uint32Of(api: *const Api, env: Env, value: Value) Error!u32 {
    var result: u32 = undefined;
    try check(api.napi_get_value_uint32(env, value, &result));
    return result;
}

fn sourceOf(api: *const Api, gpa: std.mem.Allocator, env: Env, value: Value) Error![]const u8 {
    var units_len: usize = undefined;
    try check(api.napi_get_value_string_utf16(env, value, null, 0, &units_len));

    const units = try gpa.alloc(u16, units_len + 1);
    var copied: usize = undefined;
    try check(api.napi_get_value_string_utf16(env, value, units.ptr, units.len, &copied));
    std.debug.assert(copied == units_len);

    const target = try gpa.alloc(u8, units_len * 3);
    return transfer.utf16.encodeUtf8(units[0..units_len], target, .replace);
}

fn encodedOf(api: *const Api, env: Env, argv: []const Value) Error!transfer.Encoded {
    return .{
        .nodes = try itemsOf(transfer.PackedNode, api, env, argv[0]),
        .extras = try itemsOf(u32, api, env, argv[1]),
        .comments = try itemsOf(transfer.PackedComment, api, env, argv[2]),
        .pool = try itemsOf(u16, api, env, argv[3]),
    };
}

fn itemsOf(comptime T: type, api: *const Api, env: Env, value: Value) Error![]const T {
    const uint16_array = 4;
    const int32_array = 5;
    var element_type: c_int = undefined;
    var len: usize = undefined;
    var data: ?*anyopaque = null;
    try check(api.napi_get_typedarray_info(env, value, &element_type, &len, &data, null, null));
    const bytes = len * @as(usize, switch (element_type) {
        uint16_array => 2,
        int32_array => 4,
        else => return error.InvalidTree,
    });
    if (bytes % @sizeOf(T) != 0) return error.InvalidTree;
    if (bytes == 0) return &.{};
    const items: [*]const T = @ptrCast(@alignCast(data orelse return error.InvalidTree));
    return items[0 .. bytes / @sizeOf(T)];
}
