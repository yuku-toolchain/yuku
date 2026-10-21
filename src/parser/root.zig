const parser = @import("parser.zig");

pub const parse = parser.parse;
pub const Options = parser.Options;
pub const CommentMode = parser.CommentMode;

pub const ast = @import("ast.zig");
pub const lexical = @import("lexical.zig");

pub const dialect = @import("dialect/root.zig");

pub const traverser = @import("traverser/root.zig");
pub const semantic = @import("semantic/root.zig");
pub const codegen = @import("codegen/root.zig");

test {
    _ = ast;
    _ = lexical;
    _ = codegen;
}
