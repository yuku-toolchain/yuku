//! Minimal N-API addon for investigating issue #84
//! (https://github.com/yuku-toolchain/yuku/issues/84).
//!
//! It imports ONLY napi-zig (no parser) and uses the exact same napi-zig as
//! yuku-parser, so if this also segfaults under Bun on Windows the problem is
//! inherent to Zig N-API addons under Bun, not something in the parser.
//!
//! Each export exercises a progressively larger slice of the call path so one
//! CI run can bisect the trigger:
const std = @import("std");
const napi = @import("napi-zig");

/// (1) No args, no allocation, no ArrayBuffer — pure call dispatch.
pub fn hello() i32 {
    return 42;
}

/// (2) Reads a string argument. This exercises argument marshalling, which
///     allocates from napi-zig's per-call arena — the same path yuku-parser
///     hits first when it reads `source`.
pub fn strLen(s: []const u8) u32 {
    return @intCast(s.len);
}

/// (3) Allocates and returns an ArrayBuffer via napi_create_arraybuffer — the
///     same return path yuku-parser uses for its serialized AST.
pub fn makeBuffer(env: napi.Env) !napi.Val {
    const ab = try env.createArrayBuffer(8);
    @memset(ab.data, 7);
    return ab.val;
}

comptime {
    napi.module(@This());
}
