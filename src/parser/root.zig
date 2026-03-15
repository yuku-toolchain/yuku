const parser = @import("parser.zig");

pub const parse = parser.parse;
pub const parseTree = parser.parseTree;
pub const Options = parser.Options;

pub const ast = @import("ast.zig");
pub const estree = @import("estree.zig");

pub const traverser = @import("traverser/root.zig");
pub const semantic = @import("semantic.zig");
