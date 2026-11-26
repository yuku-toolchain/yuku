const std = @import("std");
const builtin = @import("builtin");
const entries = @import("entries.zig");
const transfer = @import("transfer/root.zig");

const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;
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

const pool = struct {
    const arenas_max = 64;
    const retained_bytes_max = 128 * 1024 * 1024;
    const lock_attempts_max = 1024;

    var locked: std.atomic.Value(bool) = .init(false);
    var buffer: [arenas_max]*Arena = undefined;
    var arenas: std.ArrayList(*Arena) = .initBuffer(&buffer);

    fn lock() bool {
        for (0..lock_attempts_max) |_| {
            if (locked.cmpxchgWeak(false, true, .acquire, .monotonic) == null) return true;
            std.atomic.spinLoopHint();
        }
        return false;
    }

    fn take() Allocator.Error!*Arena {
        if (lock()) {
            defer locked.store(false, .release);
            if (arenas.pop()) |arena| return arena;
        }
        const arena = try std.heap.page_allocator.create(Arena);
        arena.* = .init(std.heap.page_allocator);
        return arena;
    }

    fn give(arena: *Arena) void {
        _ = arena.reset(.{ .retain_with_limit = retained_bytes_max });
        if (lock()) {
            defer locked.store(false, .release);
            if (arenas.items.len < arenas_max) return arenas.appendAssumeCapacity(arena);
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
    const api = bindApi() catch return null;
    inline for (.{ "parse", "analyze", "printSource", "printTree", "analyzeTree" }) |name| {
        var function: Value = undefined;
        const run = callback(@field(entries, name));
        if (api.napi_create_function(env, name, name.len, run, null, &function) != .ok) return null;
        if (api.napi_set_named_property(env, exports, name, function) != .ok) return null;
    }
    return exports;
}

fn check(status: Status) error{NodeApi}!void {
    if (status != .ok) return error.NodeApi;
}

fn callback(comptime entry: anytype) Callback {
    const params = @typeInfo(@TypeOf(entry)).@"fn".param_types;
    const Args, const count = comptime blk: {
        var types: [params.len]type = undefined;
        var count: usize = 0;
        for (params, &types) |Param, *Arg| {
            Arg.* = Param orelse *Output;
            count += valuesOf(Param);
        }
        break :blk .{ @Tuple(&types), count };
    };
    return struct {
        fn call(env: Env, info: CallbackInfo) callconv(.c) ?Value {
            const api = bindApi() catch return null;
            return run(&api, env, info) catch |err| {
                _ = api.napi_throw_error(env, null, switch (err) {
                    error.OutOfMemory => "yuku: out of memory",
                    error.NodeApi => "yuku: a Node-API call failed",
                    error.InvalidTree => "yuku: the tree is malformed",
                });
                return null;
            };
        }

        fn run(api: *const Api, env: Env, info: CallbackInfo) Error!Value {
            const arena = try pool.take();
            defer pool.give(arena);
            const gpa = arena.allocator();

            var argc: usize = count;
            var argv: [count]Value = undefined;
            try check(api.napi_get_cb_info(env, info, &argc, &argv, null, null));
            if (argc < count) return error.NodeApi;

            var output: Output = .{ .api = api, .env = env };
            var args: Args = undefined;
            comptime var next = 0;
            inline for (params, 0..) |Param, i| {
                const values = argv[next..][0..comptime valuesOf(Param)];
                next += values.len;
                args[i] = if (Param == null)
                    &output
                else if (Param == Allocator)
                    gpa
                else if (Param == u32)
                    try uint32Of(api, env, values[0])
                else if (Param == []const u8)
                    try sourceOf(api, gpa, env, values[0])
                else
                    try encodedOf(api, env, values);
            }
            _ = try @call(.auto, entry, args);
            return output.value.?;
        }
    }.call;
}

fn valuesOf(comptime Param: ?type) usize {
    if (Param == transfer.Encoded) return 4;
    return @intFromBool(Param == u32 or Param == []const u8);
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

fn uint32Of(api: *const Api, env: Env, value: Value) Error!u32 {
    var result: u32 = undefined;
    try check(api.napi_get_value_uint32(env, value, &result));
    return result;
}

fn sourceOf(api: *const Api, gpa: Allocator, env: Env, value: Value) Error![]const u8 {
    var units_len: usize = undefined;
    try check(api.napi_get_value_string_utf16(env, value, null, 0, &units_len));

    const units = try gpa.alloc(u16, units_len + 1);
    var copied: usize = undefined;
    try check(api.napi_get_value_string_utf16(env, value, units.ptr, units.len, &copied));
    std.debug.assert(copied == units_len);

    const target = try gpa.alloc(u8, units_len * 3);
    return transfer.utf16.encodeUtf8(units[0..units_len], target, .replace);
}

fn encodedOf(api: *const Api, env: Env, arrays: *const [4]Value) Error!transfer.Encoded {
    return .{
        .nodes = try itemsOf(transfer.PackedNode, api, env, arrays[0]),
        .extras = try itemsOf(u32, api, env, arrays[1]),
        .comments = try itemsOf(transfer.PackedComment, api, env, arrays[2]),
        .pool = try itemsOf(u16, api, env, arrays[3]),
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
    if (!std.mem.isAligned(@intFromPtr(data), @alignOf(T))) return error.InvalidTree;
    const items: [*]const T = @ptrCast(@alignCast(data orelse return error.InvalidTree));
    return items[0 .. bytes / @sizeOf(T)];
}
