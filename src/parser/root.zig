const parser = @import("parser.zig");

pub const parse = parser.parse;
pub const Options = parser.Options;

pub const ast = @import("ast.zig");
pub const estree = @import("estree.zig");
const traverser_root = @import("traverser/root.zig");
pub const traverser = struct {
    pub const walk = traverser_root.walk;
    pub const Action = traverser_root.Action;
    pub const NodeTag = traverser_root.NodeTag;
    pub const ParentStack = traverser_root.ParentStack;
    pub const basic = @import("traverser/basic.zig");
    pub const scoped = @import("traverser/scoped.zig");
};
