const napi = @import("napi-zig");
const parser = @import("parser");
const transfer = @import("transfer/root.zig");

pub fn generate(
    env: napi.Env,
    buffer: napi.Val,
    options: parser.codegen.Options,
) !parser.codegen.Result {
    const allocator = env.allocator();
    const bytes = try buffer.getArrayBufferData(env);

    var tree = transfer.deserializeFromBuf(allocator, bytes, "") catch return error.DecodeFailed;
    defer tree.deinit();

    var js_options = options;
    if (js_options.source_map) |*source_map| source_map.utf16_offsets = true;
    return parser.codegen.generate(allocator, &tree, js_options);
}

comptime {
    napi.module(@This());
}
