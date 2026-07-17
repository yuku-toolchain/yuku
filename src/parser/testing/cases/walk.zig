//! Walker mechanics. Hook dispatch order, Action semantics, enter and
//! exit pairing, NodePath, Layer ordering, and transform-traverser
//! mutation.

const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const traverser = parser.traverser;
const Action = traverser.Action;
const NodePath = traverser.NodePath;

const Allocator = std.mem.Allocator;
const testing = std.testing;

fn parseModule(source: []const u8) !ast.Tree {
    var tree = try parser.parse(testing.allocator, source, .{});
    errdefer tree.deinit();
    try testing.expect(!tree.hasErrors());
    return tree;
}

const Ctx = struct { tree: *const ast.Tree };

test "NodePath starts empty" {
    var path = NodePath{};
    try testing.expectEqual(@as(usize, 0), path.depth());
    try testing.expectEqual(@as(?ast.NodeIndex, null), path.parent());
    try testing.expectEqual(@as(?ast.NodeIndex, null), path.ancestor(0));

    var it = path.ancestors();
    try testing.expectEqual(@as(?ast.NodeIndex, null), it.next());
}

test "NodePath push/pop tracks depth, parent, and ancestors" {
    var path = NodePath{};
    const n0: ast.NodeIndex = @enumFromInt(10);
    const n1: ast.NodeIndex = @enumFromInt(20);
    const n2: ast.NodeIndex = @enumFromInt(30);

    path.push(n0);
    path.push(n1);
    path.push(n2);

    try testing.expectEqual(@as(usize, 3), path.depth());
    try testing.expectEqual(n2, path.ancestor(0).?);
    try testing.expectEqual(n1, path.parent().?);
    try testing.expectEqual(n0, path.ancestor(2).?);
    try testing.expectEqual(@as(?ast.NodeIndex, null), path.ancestor(3));

    var it = path.ancestors();
    try testing.expectEqual(n2, it.next().?);
    try testing.expectEqual(n1, it.next().?);
    try testing.expectEqual(n0, it.next().?);
    try testing.expectEqual(@as(?ast.NodeIndex, null), it.next());

    path.pop();
    try testing.expectEqual(@as(usize, 2), path.depth());
    try testing.expectEqual(n1, path.ancestor(0).?);
    try testing.expectEqual(n0, path.parent().?);
}

test "NodePath degrades gracefully past its fixed capacity" {
    var path = NodePath{};
    var i: u32 = 0;
    while (i < 300) : (i += 1) {
        path.push(@enumFromInt(i));
    }

    try testing.expectEqual(@as(usize, 300), path.depth());
    try testing.expectEqual(@as(?ast.NodeIndex, null), path.ancestor(0));
    try testing.expectEqual(@as(?ast.NodeIndex, null), path.ancestor(43));
    try testing.expectEqual(@as(ast.NodeIndex, @enumFromInt(249)), path.ancestor(50).?);
    try testing.expectEqual(@as(ast.NodeIndex, @enumFromInt(0)), path.ancestor(299).?);

    i = 0;
    while (i < 44) : (i += 1) path.pop();
    try testing.expectEqual(@as(usize, 256), path.depth());
    try testing.expectEqual(@as(ast.NodeIndex, @enumFromInt(255)), path.ancestor(0).?);
}

const CountingVisitor = struct {
    enters: usize = 0,
    exits: usize = 0,
    trigger: ?std.meta.Tag(ast.NodeData) = null,
    action_on_trigger: Action = .proceed,
    triggered: usize = 0,
    seen_binding_names: std.ArrayList([]const u8) = .empty,
    gpa: Allocator,

    pub fn enter_node(
        self: *CountingVisitor,
        data: ast.NodeData,
        index: ast.NodeIndex,
        ctx: *Ctx,
    ) Allocator.Error!Action {
        _ = index;
        self.enters += 1;
        if (data == .binding_identifier) {
            const name = ctx.tree.string(data.binding_identifier.name);
            try self.seen_binding_names.append(self.gpa, name);
        }
        if (self.trigger) |tag| {
            if (std.meta.activeTag(data) == tag) {
                self.triggered += 1;
                return self.action_on_trigger;
            }
        }
        return .proceed;
    }

    pub fn exit_node(
        self: *CountingVisitor,
        data: ast.NodeData,
        index: ast.NodeIndex,
        ctx: *Ctx,
    ) void {
        _ = data;
        _ = index;
        _ = ctx;
        self.exits += 1;
    }

    fn deinit(self: *CountingVisitor) void {
        self.seen_binding_names.deinit(self.gpa);
    }

    fn sawBinding(self: *const CountingVisitor, name: []const u8) bool {
        for (self.seen_binding_names.items) |seen| {
            if (std.mem.eql(u8, seen, name)) return true;
        }
        return false;
    }
};

test "walk visits every node with balanced enters and exits" {
    var tree = try parseModule("let a = 1; function f(b) { return b + a; }");
    defer tree.deinit();

    var visitor = CountingVisitor{ .gpa = testing.allocator };
    defer visitor.deinit();
    var ctx = Ctx{ .tree = &tree };
    try traverser.walk(Ctx, CountingVisitor, &visitor, &ctx);

    try testing.expect(visitor.enters > 0);
    try testing.expectEqual(visitor.enters, visitor.exits);
    try testing.expect(visitor.sawBinding("a"));
    try testing.expect(visitor.sawBinding("b"));
    try testing.expect(visitor.sawBinding("f"));
}

test "skip suppresses the subtree but the node's exit still runs" {
    var tree = try parseModule("function f(b) { let inner; } let outer;");
    defer tree.deinit();

    var visitor = CountingVisitor{
        .gpa = testing.allocator,
        .trigger = .function,
        .action_on_trigger = .skip,
    };
    defer visitor.deinit();
    var ctx = Ctx{ .tree = &tree };
    try traverser.walk(Ctx, CountingVisitor, &visitor, &ctx);

    try testing.expectEqual(@as(usize, 1), visitor.triggered);
    try testing.expect(!visitor.sawBinding("f"));
    try testing.expect(!visitor.sawBinding("b"));
    try testing.expect(!visitor.sawBinding("inner"));
    try testing.expect(visitor.sawBinding("outer"));
    try testing.expectEqual(visitor.enters, visitor.exits);
}

test "stop halts the traversal and unwinds exits for entered nodes" {
    var tree = try parseModule("let before; function f() {} let after;");
    defer tree.deinit();

    var visitor = CountingVisitor{
        .gpa = testing.allocator,
        .trigger = .function,
        .action_on_trigger = .stop,
    };
    defer visitor.deinit();
    var ctx = Ctx{ .tree = &tree };
    try traverser.walk(Ctx, CountingVisitor, &visitor, &ctx);

    try testing.expectEqual(@as(usize, 1), visitor.triggered);
    try testing.expect(visitor.sawBinding("before"));
    try testing.expect(!visitor.sawBinding("f"));
    try testing.expect(!visitor.sawBinding("after"));
    try testing.expectEqual(visitor.enters, visitor.exits);
}

const EventLog = struct {
    gpa: Allocator,
    events: std.ArrayList([]const u8) = .empty,

    fn append(self: *EventLog, name: []const u8) Allocator.Error!void {
        try self.events.append(self.gpa, name);
    }

    fn deinit(self: *EventLog) void {
        self.events.deinit(self.gpa);
    }

    fn indexOf(self: *const EventLog, name: []const u8) ?usize {
        for (self.events.items, 0..) |event, i| {
            if (std.mem.eql(u8, event, name)) return i;
        }
        return null;
    }

    fn expectOrder(self: *const EventLog, names: []const []const u8) !void {
        var last: ?usize = null;
        for (names) |name| {
            const pos = self.indexOf(name) orelse {
                std.debug.print("event '{s}' never fired\n", .{name});
                return error.MissingEvent;
            };
            if (last) |l| {
                if (pos <= l) {
                    std.debug.print("event '{s}' fired out of order\n", .{name});
                    return error.EventsOutOfOrder;
                }
            }
            last = pos;
        }
    }
};

const TypedHookVisitor = struct {
    log: *EventLog,

    pub fn enter_node(
        self: *TypedHookVisitor,
        data: ast.NodeData,
        index: ast.NodeIndex,
        ctx: *Ctx,
    ) Allocator.Error!Action {
        _ = index;
        _ = ctx;
        if (data == .function) try self.log.append("enter_node(function)");
        return .proceed;
    }

    pub fn enter_function(
        self: *TypedHookVisitor,
        func: ast.Function,
        index: ast.NodeIndex,
        ctx: *Ctx,
    ) Allocator.Error!Action {
        _ = index;
        _ = ctx;
        if (func.id != .null) try self.log.append("enter_function");
        return .proceed;
    }

    pub fn enter_binding_identifier(
        self: *TypedHookVisitor,
        id: ast.BindingIdentifier,
        index: ast.NodeIndex,
        ctx: *Ctx,
    ) Allocator.Error!Action {
        _ = id;
        _ = index;
        _ = ctx;
        try self.log.append("enter_binding_identifier");
        return .proceed;
    }

    pub fn exit_function(
        self: *TypedHookVisitor,
        func: ast.Function,
        index: ast.NodeIndex,
        ctx: *Ctx,
    ) void {
        _ = func;
        _ = index;
        _ = ctx;
        self.log.append("exit_function") catch unreachable;
    }

    pub fn exit_node(
        self: *TypedHookVisitor,
        data: ast.NodeData,
        index: ast.NodeIndex,
        ctx: *Ctx,
    ) void {
        _ = index;
        _ = ctx;
        if (data == .function) self.log.append("exit_node(function)") catch unreachable;
    }
};

test "typed hooks fire between the generic hooks, exits in reverse order" {
    var tree = try parseModule("function f() {}");
    defer tree.deinit();

    var log = EventLog{ .gpa = testing.allocator };
    defer log.deinit();
    var visitor = TypedHookVisitor{ .log = &log };
    var ctx = Ctx{ .tree = &tree };
    try traverser.walk(Ctx, TypedHookVisitor, &visitor, &ctx);

    try log.expectOrder(&.{
        "enter_node(function)",
        "enter_function",
        "enter_binding_identifier",
        "exit_function",
        "exit_node(function)",
    });
}

const SkippingTypedVisitor = struct {
    entered_bindings: usize = 0,
    function_exits: usize = 0,

    pub fn enter_function(
        self: *SkippingTypedVisitor,
        func: ast.Function,
        index: ast.NodeIndex,
        ctx: *Ctx,
    ) Action {
        _ = self;
        _ = func;
        _ = index;
        _ = ctx;
        return .skip;
    }

    pub fn enter_binding_identifier(
        self: *SkippingTypedVisitor,
        id: ast.BindingIdentifier,
        index: ast.NodeIndex,
        ctx: *Ctx,
    ) Action {
        _ = id;
        _ = index;
        _ = ctx;
        self.entered_bindings += 1;
        return .proceed;
    }

    pub fn exit_function(
        self: *SkippingTypedVisitor,
        func: ast.Function,
        index: ast.NodeIndex,
        ctx: *Ctx,
    ) void {
        _ = func;
        _ = index;
        _ = ctx;
        self.function_exits += 1;
    }
};

test "a typed hook returning skip suppresses children, non-error hooks allowed" {
    var tree = try parseModule("function f(a, b) {}");
    defer tree.deinit();

    var visitor = SkippingTypedVisitor{};
    var ctx = Ctx{ .tree = &tree };
    try traverser.walk(Ctx, SkippingTypedVisitor, &visitor, &ctx);

    try testing.expectEqual(@as(usize, 0), visitor.entered_bindings);
    try testing.expectEqual(@as(usize, 1), visitor.function_exits);
}

const LayerCtx = struct {
    tree: *const ast.Tree,
    log: *EventLog,

    pub fn enter(self: *LayerCtx, index: ast.NodeIndex, data: ast.NodeData) Allocator.Error!void {
        _ = index;
        _ = data;
        try self.log.append("ctx.enter");
    }

    pub fn post_enter(
        self: *LayerCtx,
        index: ast.NodeIndex,
        data: ast.NodeData,
    ) Allocator.Error!void {
        _ = index;
        _ = data;
        try self.log.append("ctx.post_enter");
    }

    pub fn exit(self: *LayerCtx, _: ast.NodeIndex, data: ast.NodeData) void {
        _ = data;
        self.log.append("ctx.exit") catch unreachable;
    }
};

const LayerUserVisitor = struct {
    log: *EventLog,

    pub fn enter_node(
        self: *LayerUserVisitor,
        data: ast.NodeData,
        index: ast.NodeIndex,
        ctx: *LayerCtx,
    ) Allocator.Error!Action {
        _ = data;
        _ = index;
        _ = ctx;
        try self.log.append("user.enter");
        return .proceed;
    }

    pub fn exit_node(
        self: *LayerUserVisitor,
        data: ast.NodeData,
        index: ast.NodeIndex,
        ctx: *LayerCtx,
    ) void {
        _ = data;
        _ = index;
        _ = ctx;
        self.log.append("user.exit") catch unreachable;
    }
};

test "Layer runs ctx.enter, user hooks, ctx.post_enter, user exit, ctx.exit" {
    var tree = try parseModule("a;");
    defer tree.deinit();

    var log = EventLog{ .gpa = testing.allocator };
    defer log.deinit();
    var visitor = LayerUserVisitor{ .log = &log };
    var ctx = LayerCtx{ .tree = &tree, .log = &log };
    var layer = traverser.Layer(LayerCtx, LayerUserVisitor){ .inner = &visitor };
    try traverser.walk(LayerCtx, traverser.Layer(LayerCtx, LayerUserVisitor), &layer, &ctx);

    const expected = [_][]const u8{
        "ctx.enter", "user.enter", "ctx.post_enter",
        "ctx.enter", "user.enter", "ctx.post_enter",
        "ctx.enter", "user.enter", "ctx.post_enter",
        "user.exit", "ctx.exit",   "user.exit",
        "ctx.exit",  "user.exit",  "ctx.exit",
    };
    try testing.expectEqual(expected.len, log.events.items.len);
    for (expected, log.events.items) |want, got| {
        try testing.expectEqualStrings(want, got);
    }
}

const LayerSkippingVisitor = struct {
    log: *EventLog,

    pub fn enter_node(
        self: *LayerSkippingVisitor,
        data: ast.NodeData,
        index: ast.NodeIndex,
        ctx: *LayerCtx,
    ) Allocator.Error!Action {
        _ = index;
        _ = ctx;
        try self.log.append("user.enter");
        return if (data == .expression_statement) .skip else .proceed;
    }

    pub fn exit_node(
        self: *LayerSkippingVisitor,
        data: ast.NodeData,
        index: ast.NodeIndex,
        ctx: *LayerCtx,
    ) void {
        _ = data;
        _ = index;
        _ = ctx;
        self.log.append("user.exit") catch unreachable;
    }
};

test "Layer keeps ctx enter/exit balanced when the user visitor skips" {
    var tree = try parseModule("a;");
    defer tree.deinit();

    var log = EventLog{ .gpa = testing.allocator };
    defer log.deinit();
    var visitor = LayerSkippingVisitor{ .log = &log };
    var ctx = LayerCtx{ .tree = &tree, .log = &log };
    var layer = traverser.Layer(LayerCtx, LayerSkippingVisitor){ .inner = &visitor };
    try traverser.walk(LayerCtx, traverser.Layer(LayerCtx, LayerSkippingVisitor), &layer, &ctx);

    var ctx_enters: usize = 0;
    var ctx_exits: usize = 0;
    var post_enters: usize = 0;
    for (log.events.items) |event| {
        if (std.mem.eql(u8, event, "ctx.enter")) ctx_enters += 1;
        if (std.mem.eql(u8, event, "ctx.exit")) ctx_exits += 1;
        if (std.mem.eql(u8, event, "ctx.post_enter")) post_enters += 1;
    }
    try testing.expectEqual(@as(usize, 2), ctx_enters);
    try testing.expectEqual(ctx_enters, ctx_exits);
    try testing.expectEqual(ctx_enters, post_enters);
}

const PathCheckVisitor = struct {
    checked: bool = false,
    failure: ?anyerror = null,

    pub fn enter_identifier_reference(
        self: *PathCheckVisitor,
        id: ast.IdentifierReference,
        index: ast.NodeIndex,
        ctx: *parser.traverser.basic.Ctx,
    ) Action {
        _ = id;
        self.checked = true;
        self.check(index, ctx) catch |err| {
            self.failure = err;
        };
        return .proceed;
    }

    fn check(
        self: *PathCheckVisitor,
        index: ast.NodeIndex,
        ctx: *parser.traverser.basic.Ctx,
    ) !void {
        _ = self;
        try testing.expectEqual(@as(usize, 5), ctx.path.depth());
        try testing.expectEqual(index, ctx.path.ancestor(0).?);
        try testing.expect(ctx.tree.data(ctx.path.parent().?) == .expression_statement);
        try testing.expect(ctx.tree.data(ctx.path.ancestor(2).?) == .block_statement);
        try testing.expect(ctx.tree.data(ctx.path.ancestor(3).?) == .block_statement);
        try testing.expect(ctx.tree.data(ctx.path.ancestor(4).?) == .program);

        var it = ctx.path.ancestors();
        var last: ast.NodeIndex = undefined;
        while (it.next()) |ancestor| last = ancestor;
        try testing.expect(ctx.tree.data(last) == .program);
    }
};

test "basic traverser tracks the root-to-current path" {
    var tree = try parseModule("{ { a; } }");
    defer tree.deinit();

    var visitor = PathCheckVisitor{};
    try parser.traverser.basic.traverse(PathCheckVisitor, &tree, &visitor);
    try testing.expect(visitor.checked);
    if (visitor.failure) |err| return err;
}

const FoldVisitor = struct {
    literal_enters: usize = 0,

    pub fn enter_binary_expression(
        self: *FoldVisitor,
        expr: ast.BinaryExpression,
        index: ast.NodeIndex,
        ctx: *parser.traverser.transform.Ctx,
    ) Action {
        _ = self;
        ctx.tree.setData(index, ctx.tree.data(expr.left));
        return .proceed;
    }

    pub fn enter_numeric_literal(
        self: *FoldVisitor,
        lit: ast.NumericLiteral,
        index: ast.NodeIndex,
        ctx: *parser.traverser.transform.Ctx,
    ) Action {
        _ = lit;
        _ = index;
        _ = ctx;
        self.literal_enters += 1;
        return .proceed;
    }
};

test "transform traverser walks the replacement after setData" {
    var tree = try parseModule("1 + 2;");
    defer tree.deinit();

    var visitor = FoldVisitor{};
    try parser.traverser.transform.traverse(FoldVisitor, &tree, &visitor);

    var replaced = false;
    var i: u32 = 0;
    while (i < tree.nodes.len) : (i += 1) {
        const data = tree.data(@enumFromInt(i));
        if (data == .binary_expression) return error.BinaryNodeSurvived;
        if (data == .numeric_literal) replaced = true;
    }
    try testing.expect(replaced);
    try testing.expectEqual(@as(usize, 0), visitor.literal_enters);
}

const ExtrasGrowingVisitor = struct {
    gpa: Allocator,
    grown: bool = false,
    visited: std.ArrayList(u8) = .empty,

    pub fn enter_identifier_reference(
        self: *ExtrasGrowingVisitor,
        id: ast.IdentifierReference,
        index: ast.NodeIndex,
        ctx: *parser.traverser.transform.Ctx,
    ) Allocator.Error!Action {
        _ = index;
        const name = ctx.tree.string(id.name);
        try self.visited.append(self.gpa, name[0]);
        if (!self.grown) {
            self.grown = true;
            var i: usize = 0;
            while (i < 512) : (i += 1) {
                const node = try ctx.tree.addNode(
                    .{ .null_literal = .{} },
                    .{ .start = 0, .end = 0 },
                );
                _ = try ctx.tree.addExtra(&.{node});
            }
        }
        return .proceed;
    }

    fn deinit(self: *ExtrasGrowingVisitor) void {
        self.visited.deinit(self.gpa);
    }
};

test "transform traverser survives extras reallocation mid-iteration" {
    var tree = try parseModule("a; b; c;");
    defer tree.deinit();

    var visitor = ExtrasGrowingVisitor{ .gpa = testing.allocator };
    defer visitor.deinit();
    try parser.traverser.transform.traverse(ExtrasGrowingVisitor, &tree, &visitor);

    try testing.expectEqualStrings("abc", visitor.visited.items);
}
