const napi = @import("napi-zig");
const c = napi.c;
const parser = @import("parser");
const ast = parser.ast;
const std = @import("std");
const NapiEstree = @import("napi_estree.zig").Serializer;

const allocator = std.heap.c_allocator;

// -- shared option parsing --

const ParseOptions = struct {
    source_type: ast.SourceType,
    lang: ast.Lang,
};

fn readOptions(env: napi.Env, opts_val: napi.Val) ParseOptions {
    var result: ParseOptions = .{ .source_type = .module, .lang = .js };
    const opts_type = opts_val.typeOf(env) catch return result;
    if (opts_type != .object) return result;

    if (opts_val.hasNamed(env, "sourceType") catch false) {
        var buf: [16]u8 = undefined;
        const st = (opts_val.getNamed(env, "sourceType") catch return result).stringBuf(env, &buf) catch return result;
        if (std.mem.eql(u8, st, "script")) result.source_type = .script;
    }
    if (opts_val.hasNamed(env, "lang") catch false) {
        var buf: [8]u8 = undefined;
        const l = (opts_val.getNamed(env, "lang") catch return result).stringBuf(env, &buf) catch return result;
        if (std.mem.eql(u8, l, "ts")) result.lang = .ts
        else if (std.mem.eql(u8, l, "jsx")) result.lang = .jsx
        else if (std.mem.eql(u8, l, "tsx")) result.lang = .tsx
        else if (std.mem.eql(u8, l, "dts")) result.lang = .dts;
    }
    return result;
}

// -- parseSync: synchronous parse, returns ESTree AST --

pub fn parseSync(env: napi.Env, info: napi.CallInfo) !napi.Val {
    const args = try info.get(env, 2);
    const source = try args[0].stringAlloc(env, allocator);
    defer allocator.free(source);

    const opts = readOptions(env, args[1]);

    var tree = parser.parse(allocator, source, .{
        .source_type = opts.source_type,
        .lang = opts.lang,
    }) catch {
        env.throwError("parse failed: out of memory");
        return error.napi_error;
    };
    defer tree.deinit();

    return NapiEstree.serialize(env, &tree);
}

// -- parse: async parse, returns Promise<ESTree AST> --

const AsyncParseData = struct {
    // input (set before queueing)
    source: []u8,
    source_type: ast.SourceType,
    lang: ast.Lang,

    // output (set by execute, read by complete)
    tree: ?ast.Tree = null,
    parse_error: bool = false,

    // napi handles
    deferred: c.napi_deferred,
    work: c.napi_async_work = undefined,
};

pub fn parse(env: napi.Env, info: napi.CallInfo) !napi.Val {
    const args = try info.get(env, 2);
    const source = try args[0].stringAlloc(env, allocator);
    const opts = readOptions(env, args[1]);

    const promise = env.createPromise() catch {
        allocator.free(source);
        env.throwError("failed to create promise");
        return error.napi_error;
    };

    const data = allocator.create(AsyncParseData) catch {
        allocator.free(source);
        env.throwError("out of memory");
        return error.napi_error;
    };

    data.* = .{
        .source = source,
        .source_type = opts.source_type,
        .lang = opts.lang,
        .deferred = promise.deferred,
    };

    data.work = env.createAsyncWork("yuku-parse", asyncExecute, asyncComplete, data) catch {
        allocator.free(source);
        allocator.destroy(data);
        env.throwError("failed to create async work");
        return error.napi_error;
    };

    env.queueAsyncWork(data.work) catch {
        env.deleteAsyncWork(data.work) catch {};
        allocator.free(source);
        allocator.destroy(data);
        env.throwError("failed to queue async work");
        return error.napi_error;
    };

    return promise.promise;
}

fn asyncExecute(_: c.napi_env, raw_data: ?*anyopaque) callconv(.c) void {
    const data: *AsyncParseData = @ptrCast(@alignCast(raw_data));

    data.tree = parser.parse(allocator, data.source, .{
        .source_type = data.source_type,
        .lang = data.lang,
    }) catch {
        data.parse_error = true;
        return;
    };
}

fn asyncComplete(raw_env: c.napi_env, status: c.napi_status, raw_data: ?*anyopaque) callconv(.c) void {
    const env: napi.Env = .{ .raw = raw_env };
    const data: *AsyncParseData = @ptrCast(@alignCast(raw_data));

    defer {
        if (data.tree) |*t| t.deinit();
        allocator.free(data.source);
        env.deleteAsyncWork(data.work) catch {};
        allocator.destroy(data);
    }

    if (status != .ok or data.parse_error or data.tree == null) {
        // reject the promise
        const err_msg = env.string("parse failed") catch return;
        env.rejectDeferred(data.deferred, err_msg) catch {};
        return;
    }

    // serialize on main thread (N-API calls must be on main thread)
    const result = NapiEstree.serialize(env, &data.tree.?) catch {
        const err_msg = env.string("serialization failed") catch return;
        env.rejectDeferred(data.deferred, err_msg) catch {};
        return;
    };

    env.resolveDeferred(data.deferred, result) catch {};
}

comptime {
    napi.module(@This());
}
