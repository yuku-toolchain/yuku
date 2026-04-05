const napi = @import("napi-zig");

comptime {
    napi.module(@This());
}
