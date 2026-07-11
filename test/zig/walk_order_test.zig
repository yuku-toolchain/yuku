//! Verifies the walker enters every parent's children in source order.
//!
//! Node payload structs in `ast.zig` declare their child fields in source
//! order and the walker visits struct fields in declaration order, so this
//! sweep over the full `test/parser` corpus catches any field that drifts
//! out of order.

const std = @import("std");
const parser = @import("parser");
const utils = @import("utils.zig");

const ast = parser.ast;
const traverser = parser.traverser;
const Allocator = std.mem.Allocator;

const Ctx = struct { tree: *const ast.Tree };

const NodeTag = std.meta.Tag(ast.NodeData);

const Frame = struct {
    tag: NodeTag,
    last: u32 = 0,
    /// quasis and expressions interleave in source, but walk order is by
    /// field, matching every ESTree walker
    interleaved: bool,
};

const Violation = struct {
    parent: NodeTag,
    child: NodeTag,
    start: u32,
    prev: u32,
};

const OrderVisitor = struct {
    gpa: Allocator,
    frames: std.ArrayList(Frame) = .empty,
    violation: ?Violation = null,

    pub fn enter_node(
        self: *OrderVisitor,
        data: ast.NodeData,
        index: ast.NodeIndex,
        ctx: *Ctx,
    ) Allocator.Error!traverser.Action {
        const start = ctx.tree.span(index).start;
        if (self.frames.items.len > 0) {
            const frame = &self.frames.items[self.frames.items.len - 1];
            if (!frame.interleaved) {
                if (start < frame.last) {
                    self.violation = .{
                        .parent = frame.tag,
                        .child = data,
                        .start = start,
                        .prev = frame.last,
                    };
                    return .stop;
                }
                frame.last = start;
            }
        }
        try self.frames.append(self.gpa, .{
            .tag = data,
            .interleaved = data == .template_literal or data == .ts_template_literal_type,
        });
        return .proceed;
    }

    pub fn exit_node(
        self: *OrderVisitor,
        data: ast.NodeData,
        index: ast.NodeIndex,
        ctx: *Ctx,
    ) void {
        _ = data;
        _ = index;
        _ = ctx;
        _ = self.frames.pop();
    }
};

const OrderChecker = struct {
    gpa: Allocator,

    pub fn check(self: OrderChecker, path: []const u8, tree: *const ast.Tree) !void {
        var visitor: OrderVisitor = .{ .gpa = self.gpa };
        defer visitor.frames.deinit(self.gpa);
        var ctx: Ctx = .{ .tree = tree };
        try traverser.walk(Ctx, OrderVisitor, &visitor, &ctx);

        if (visitor.violation) |v| {
            std.debug.print(
                "{s}: {t} at offset {d} entered after a sibling at {d} inside {t}\n",
                .{ path, v.child, v.start, v.prev, v.parent },
            );
            return error.ChildrenOutOfOrder;
        }
    }
};

test "walker enters children in source order across the parser corpus" {
    const gpa = std.testing.allocator;
    try utils.forEachCorpusTree(gpa, OrderChecker{ .gpa = gpa });
}
