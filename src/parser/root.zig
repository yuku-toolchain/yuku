const parser = @import("parser.zig");

pub const parse = parser.parse;
/// exported for drivers that embed the parser directly: `init` and `parse` are API,
/// the fields are internals and change shape between releases.
pub const Parser = parser.Parser;
pub const Options = parser.Options;
pub const CommentMode = parser.CommentMode;

pub const ast = @import("ast.zig");

pub const traverser = @import("traverser/root.zig");
pub const semantic = @import("semantic/root.zig");
pub const codegen = @import("codegen/root.zig");

test {
    _ = codegen;
    _ = @import("syntax/variables.zig");
}
