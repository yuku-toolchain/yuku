const parser = @import("parser.zig");

pub const parse = parser.parse;
pub const Options = parser.Options;
pub const CommentMode = parser.CommentMode;

pub const ast = @import("ast.zig");

pub const traverser = @import("traverser/root.zig");
pub const semantic = @import("semantic/root.zig");
pub const codegen = @import("codegen/root.zig");

test {
    _ = codegen;
    _ = @import("semantic/tests.zig");
}
