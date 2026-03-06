const ast = @import("../ast.zig");
const root = @import("root.zig");

const NodeTag = root.NodeTag;
const ParentStack = root.ParentStack;

/// Basic traversal context with parent stack.
/// For simple traversals that need parent/ancestor access.
pub const Ctx = struct {
    tree: *const ast.ParseTree,
    parents: ParentStack = .{},

    pub fn onEnter(self: *Ctx, index: ast.NodeIndex, _: NodeTag) void {
        self.parents.push(index);
    }

    pub fn onExit(self: *Ctx, _: ast.NodeIndex, _: NodeTag) void {
        self.parents.pop();
    }

    /// The immediate parent node (null_node at root)
    pub inline fn parent(self: *const Ctx) ast.NodeIndex {
        return self.parents.current();
    }

    /// Nth ancestor: 0 = parent, 1 = grandparent, ...
    pub inline fn ancestor(self: *const Ctx, depth_offset: usize) ast.NodeIndex {
        return self.parents.get(depth_offset);
    }

    pub inline fn parentData(self: *const Ctx) ?ast.NodeData {
        const p = self.parent();
        if (ast.isNull(p)) return null;
        return self.tree.getData(p);
    }

    /// Walk up ancestors until predicate matches. Returns the matching node index.
    pub fn findAncestor(self: *const Ctx, comptime predicate: fn (ast.NodeData) bool) ?ast.NodeIndex {
        var i: usize = 0;
        while (true) {
            const a = self.parents.get(i);
            if (ast.isNull(a)) return null;
            if (predicate(self.tree.getData(a))) return a;
            i += 1;
        }
    }

    pub inline fn nodeText(self: *const Ctx, start: u32, len: u16) []const u8 {
        return self.tree.source[start..][0..len];
    }
};

/// Convenience: traverse with the built-in Ctx (parent stack only).
pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V) void {
    var ctx = Ctx{ .tree = tree };
    root.walk(Ctx, V, visitor, &ctx);
}
