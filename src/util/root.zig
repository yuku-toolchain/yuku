pub const Simd = @import("simd.zig");
pub const UnicodeId = @import("unicode_id.zig");
pub const Utf = @import("utf.zig");
pub const XhtmlEntities = @import("xhtml_entities.zig");

test {
    _ = Simd;
    _ = Utf;
}
