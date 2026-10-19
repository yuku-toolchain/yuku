const std = @import("std");
const parser = @import("parser");
const helpers = @import("../helpers.zig");

const ast = parser.ast;
const traverser = parser.traverser;
const Allocator = std.mem.Allocator;

const Ctx = struct { tree: *const ast.Tree };

const NodeTag = helpers.NodeTag;

const Frame = struct {
    tag: NodeTag,
    last: u32 = 0,
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

    pub fn exit_node(self: *OrderVisitor, _: ast.NodeData, _: ast.NodeIndex, _: *Ctx) void {
        _ = self.frames.pop();
    }
};

fn checkOrder(path: []const u8, tree: *const ast.Tree) !void {
    const gpa = std.testing.allocator;
    var visitor: OrderVisitor = .{ .gpa = gpa };
    defer visitor.frames.deinit(gpa);
    var ctx: Ctx = .{ .tree = tree };
    try traverser.walk(Ctx, OrderVisitor, &visitor, &ctx, .{ .whole_tree = false });

    if (visitor.violation) |v| {
        std.debug.print(
            "{s}: {t} at offset {d} entered after a sibling at {d} inside {t}\n",
            .{ path, v.child, v.start, v.prev, v.parent },
        );
        return error.ChildrenOutOfOrder;
    }
}

test "walker enters children in source order across the parser corpus" {
    try helpers.forEachCorpusTree(checkOrder);
}
