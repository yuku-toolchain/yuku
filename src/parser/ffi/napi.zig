const std = @import("std");
const builtin = @import("builtin");
const parser = @import("parser");
const flag = @import("flag.zig");
const transfer = @import("transfer/root.zig");

const Error = std.mem.Allocator.Error || error{NodeApi};

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
    try define(&api, env, exports, "parse", parse);
    try define(&api, env, exports, "analyze", analyze);
}

fn define(
    api: *const Api,
    env: Env,
    exports: Value,
    comptime name: [:0]const u8,
    callback: Callback,
) Error!void {
    var function: Value = undefined;
    try check(api.napi_create_function(env, name.ptr, name.len, callback, null, &function));
    try check(api.napi_set_named_property(env, exports, name.ptr, function));
}

fn check(status: Status) error{NodeApi}!void {
    if (status != .ok) return error.NodeApi;
}

fn parse(env: Env, info: CallbackInfo) callconv(.c) ?Value {
    return call(env, info, parseBuffer);
}

fn analyze(env: Env, info: CallbackInfo) callconv(.c) ?Value {
    return call(env, info, analyzeBuffer);
}

const Run = fn (*const Api, std.mem.Allocator, Env, []const u8, u32) Error!Value;

fn call(env: Env, info: CallbackInfo, comptime run: Run) ?Value {
    const api = bindApi() catch return null;
    return withSource(&api, env, info, run) catch |err| {
        const message = switch (err) {
            error.OutOfMemory => "yuku: out of memory",
            error.NodeApi => "yuku: a Node-API call failed",
        };
        _ = api.napi_throw_error(env, null, message);
        return null;
    };
}

fn withSource(api: *const Api, env: Env, info: CallbackInfo, comptime run: Run) Error!Value {
    const arena = try pool.take();
    defer pool.give(arena);
    const gpa = arena.allocator();

    var argc: usize = 2;
    var argv: [2]Value = undefined;
    try check(api.napi_get_cb_info(env, info, &argc, &argv, null, null));
    std.debug.assert(argc == 2);

    var flags: u32 = undefined;
    try check(api.napi_get_value_uint32(env, argv[1], &flags));

    var units_len: usize = undefined;
    try check(api.napi_get_value_string_utf16(env, argv[0], null, 0, &units_len));

    const units = try gpa.alloc(u16, units_len + 1);
    var copied: usize = undefined;
    try check(api.napi_get_value_string_utf16(env, argv[0], units.ptr, units.len, &copied));
    std.debug.assert(copied == units_len);

    const target = try gpa.alloc(u8, units_len * 3);
    const source = transfer.utf16.encodeUtf8(units[0..units_len], target);
    return run(api, gpa, env, source, flags);
}

fn parseBuffer(
    api: *const Api,
    gpa: std.mem.Allocator,
    env: Env,
    source: []const u8,
    flags: u32,
) Error!Value {
    var tree = try parseTree(gpa, source, flags);
    defer tree.deinit();

    if (flags & flag.semantic_errors != 0) try parser.semantic.check(&tree);

    var offsets: transfer.utf16.Offsets = try .init(gpa, tree.source);
    defer offsets.deinit(gpa);

    const out = try arrayBuffer(api, env, transfer.bufferSize(&tree));
    const written = transfer.serializeInto(&tree, &offsets, out.bytes);
    std.debug.assert(written == out.bytes.len);
    return out.value;
}

fn analyzeBuffer(
    api: *const Api,
    gpa: std.mem.Allocator,
    env: Env,
    source: []const u8,
    flags: u32,
) Error!Value {
    var tree = try parseTree(gpa, source, flags);
    defer tree.deinit();

    const sem = try parser.semantic.analyze(&tree, .{
        .check = flags & flag.semantic_errors != 0,
    });

    const records = try parser.semantic.module_record.collect(&tree, &sem);

    var offsets: transfer.utf16.Offsets = try .init(gpa, tree.source);
    defer offsets.deinit(gpa);

    const size = transfer.semantic.bufferSize(&tree, &sem, records);
    const out = try arrayBuffer(api, env, size);
    const written = transfer.semantic.serializeInto(&tree, &sem, records, &offsets, out.bytes);
    std.debug.assert(written == out.bytes.len);
    return out.value;
}

const Buffer = struct { value: Value, bytes: []u8 };

fn arrayBuffer(api: *const Api, env: Env, size: usize) Error!Buffer {
    std.debug.assert(size > 0);
    var data: ?*anyopaque = null;
    var value: Value = undefined;
    try check(api.napi_create_arraybuffer(env, size, &data, &value));
    const bytes: [*]u8 = @ptrCast(data orelse return error.OutOfMemory);
    return .{ .value = value, .bytes = bytes[0..size] };
}

fn parseTree(
    gpa: std.mem.Allocator,
    source: []const u8,
    flags: u32,
) std.mem.Allocator.Error!parser.ast.Tree {
    std.debug.assert(flags & flag.dialect == 0);
    std.debug.assert(flags & flag.utf16 == 0);
    return parser.parse(gpa, source, flag.parseOptions(flags));
}
