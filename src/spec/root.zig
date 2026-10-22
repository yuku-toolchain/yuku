pub const escapes = @import("escapes.zig");
pub const jsx_entities = @import("jsx_entities.zig");
pub const line_terminators = @import("line_terminators.zig");
pub const number = @import("number.zig");
pub const static_semantics = @import("static_semantics.zig");
pub const unicode_id = @import("unicode_id.zig");

test {
    _ = escapes;
    _ = line_terminators;
    _ = number;
}
