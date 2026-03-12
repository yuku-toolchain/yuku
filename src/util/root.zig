pub const UnicodeId = @import("unicode_id.zig");
pub const Utf = @import("utf.zig");
pub const string_table = @import("string_table.zig");
pub const StringTable = string_table.StringTable;
pub const StringId = string_table.StringId;

test {
    _ = Utf;
}
