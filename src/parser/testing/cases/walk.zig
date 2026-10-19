//! Walker mechanics, from hook dispatch order to transform-traverser mutation.

const std = @import("std");
const parser = @import("parser");
const helpers = @import("../helpers.zig");

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
    var buffer: [3]ast.NodeIndex = undefined;
    var path = NodePath{ .items = &buffer };
    const n0: ast.NodeIndex = @fromBackingInt(10);
    const n1: ast.NodeIndex = @fromBackingInt(20);
    const n2: ast.NodeIndex = @fromBackingInt(30);

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

const deep_source = "let before; a" ++ helpers.repeat(".b", 1000) ++ "; let after;";

test "the path holds every ancestor past the walk's recursion" {
    var tree = try parseModule(deep_source);
    defer tree.deinit();

    const Innermost = struct {
        depth: usize = 0,
        root: ?ast.NodeIndex = null,

        pub fn enter_identifier_reference(
            self: *@This(),
            _: ast.IdentifierReference,
            _: ast.NodeIndex,
            ctx: *traverser.basic.Ctx,
        ) Action {
            self.depth = ctx.path.depth();
            self.root = ctx.path.ancestor(self.depth - 1);
            return .proceed;
        }
    };

    var innermost: Innermost = .{};
    try traverser.basic.traverse(Innermost, &tree, &innermost);
    try testing.expectEqual(@as(usize, 1000 + 3), innermost.depth);
    try testing.expectEqual(tree.root, innermost.root.?);
}

const CountingVisitor = struct {
    enters: usize = 0,
    exits: usize = 0,
    trigger: ?helpers.NodeTag = null,
    action_on_trigger: Action = .proceed,
    triggered: usize = 0,
    seen_binding_names: std.ArrayList([]const u8) = .empty,
    gpa: Allocator,

    pub fn enter_node(
        self: *CountingVisitor,
        data: ast.NodeData,
        _: ast.NodeIndex,
        ctx: *Ctx,
    ) Allocator.Error!Action {
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

    pub fn exit_node(self: *CountingVisitor, _: ast.NodeData, _: ast.NodeIndex, _: *Ctx) void {
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
    try traverser.walk(Ctx, CountingVisitor, &visitor, &ctx, .{ .whole_tree = false });

    try testing.expect(visitor.enters > 0);
    try testing.expectEqual(visitor.enters, visitor.exits);
    try testing.expect(visitor.sawBinding("a"));
    try testing.expect(visitor.sawBinding("b"));
    try testing.expect(visitor.sawBinding("f"));
}

test "a walk past the recursion pairs every enter with an exit, even on stop" {
    var tree = try parseModule(deep_source);
    defer tree.deinit();

    for ([_]Action{ .proceed, .stop }) |action| {
        var visitor = CountingVisitor{
            .gpa = testing.allocator,
            .trigger = .identifier_reference,
            .action_on_trigger = action,
        };
        defer visitor.deinit();
        var ctx = Ctx{ .tree = &tree };
        try traverser.walk(Ctx, CountingVisitor, &visitor, &ctx, .{ .whole_tree = false });

        try testing.expectEqual(@as(usize, 1), visitor.triggered);
        try testing.expectEqual(visitor.enters, visitor.exits);
        try testing.expect(visitor.sawBinding("before"));
        try testing.expectEqual(action == .proceed, visitor.sawBinding("after"));
    }
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
    try traverser.walk(Ctx, CountingVisitor, &visitor, &ctx, .{ .whole_tree = false });

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
    try traverser.walk(Ctx, CountingVisitor, &visitor, &ctx, .{ .whole_tree = false });

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
        _: ast.NodeIndex,
        _: *Ctx,
    ) Allocator.Error!Action {
        if (data == .function) try self.log.append("enter_node(function)");
        return .proceed;
    }

    pub fn enter_function(
        self: *TypedHookVisitor,
        func: ast.Function,
        _: ast.NodeIndex,
        _: *Ctx,
    ) Allocator.Error!Action {
        if (func.id != .null) try self.log.append("enter_function");
        return .proceed;
    }

    pub fn enter_binding_identifier(
        self: *TypedHookVisitor,
        _: ast.BindingIdentifier,
        _: ast.NodeIndex,
        _: *Ctx,
    ) Allocator.Error!Action {
        try self.log.append("enter_binding_identifier");
        return .proceed;
    }

    pub fn exit_function(self: *TypedHookVisitor, _: ast.Function, _: ast.NodeIndex, _: *Ctx) void {
        self.log.append("exit_function") catch unreachable;
    }

    pub fn exit_node(self: *TypedHookVisitor, data: ast.NodeData, _: ast.NodeIndex, _: *Ctx) void {
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
    try traverser.walk(Ctx, TypedHookVisitor, &visitor, &ctx, .{ .whole_tree = false });

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
        _: *SkippingTypedVisitor,
        _: ast.Function,
        _: ast.NodeIndex,
        _: *Ctx,
    ) Action {
        return .skip;
    }

    pub fn enter_binding_identifier(
        self: *SkippingTypedVisitor,
        _: ast.BindingIdentifier,
        _: ast.NodeIndex,
        _: *Ctx,
    ) Action {
        self.entered_bindings += 1;
        return .proceed;
    }

    pub fn exit_function(
        self: *SkippingTypedVisitor,
        _: ast.Function,
        _: ast.NodeIndex,
        _: *Ctx,
    ) void {
        self.function_exits += 1;
    }
};

test "a typed hook returning skip suppresses children, non-error hooks allowed" {
    var tree = try parseModule("function f(a, b) {}");
    defer tree.deinit();

    var visitor = SkippingTypedVisitor{};
    var ctx = Ctx{ .tree = &tree };
    try traverser.walk(Ctx, SkippingTypedVisitor, &visitor, &ctx, .{ .whole_tree = false });

    try testing.expectEqual(@as(usize, 0), visitor.entered_bindings);
    try testing.expectEqual(@as(usize, 1), visitor.function_exits);
}

const HookOrderCtx = struct {
    tree: *const ast.Tree,
    log: *EventLog,

    pub fn enter(
        self: *HookOrderCtx,
        comptime _: ast.NodeTag,
        _: ast.NodeIndex,
        _: ast.NodeData,
    ) Allocator.Error!void {
        try self.log.append("ctx.enter");
    }

    pub fn post_enter(
        self: *HookOrderCtx,
        comptime _: ast.NodeTag,
        _: ast.NodeIndex,
        _: ast.NodeData,
    ) Allocator.Error!void {
        try self.log.append("ctx.post_enter");
    }

    pub fn exit(
        self: *HookOrderCtx,
        comptime _: ast.NodeTag,
        _: ast.NodeIndex,
        _: ast.NodeData,
    ) void {
        self.log.append("ctx.exit") catch unreachable;
    }
};

const HookOrderVisitor = struct {
    log: *EventLog,
    skip: ?helpers.NodeTag = null,

    pub fn enter_node(
        self: *HookOrderVisitor,
        data: ast.NodeData,
        _: ast.NodeIndex,
        _: *HookOrderCtx,
    ) Allocator.Error!Action {
        try self.log.append("user.enter");
        if (self.skip) |tag| {
            if (std.meta.activeTag(data) == tag) return .skip;
        }
        return .proceed;
    }

    pub fn exit_node(
        self: *HookOrderVisitor,
        _: ast.NodeData,
        _: ast.NodeIndex,
        _: *HookOrderCtx,
    ) void {
        self.log.append("user.exit") catch unreachable;
    }
};

test "the walk runs ctx.enter, user hooks, ctx.post_enter, user exit, ctx.exit" {
    var tree = try parseModule("a;");
    defer tree.deinit();

    var log = EventLog{ .gpa = testing.allocator };
    defer log.deinit();
    var visitor = HookOrderVisitor{ .log = &log };
    var ctx = HookOrderCtx{ .tree = &tree, .log = &log };
    try traverser.walk(HookOrderCtx, HookOrderVisitor, &visitor, &ctx, .{ .whole_tree = false });

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

test "the walk keeps ctx enter/exit balanced when the user visitor skips" {
    var tree = try parseModule("a;");
    defer tree.deinit();

    var log = EventLog{ .gpa = testing.allocator };
    defer log.deinit();
    var visitor = HookOrderVisitor{ .log = &log, .skip = .expression_statement };
    var ctx = HookOrderCtx{ .tree = &tree, .log = &log };
    try traverser.walk(HookOrderCtx, HookOrderVisitor, &visitor, &ctx, .{ .whole_tree = false });

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

const HookEvent = struct { exit: bool, index: ast.NodeIndex };

const EnterCountingCtx = struct {
    tree: *const ast.Tree,
    enters: u32 = 0,

    pub fn enter(
        self: *EnterCountingCtx,
        comptime _: ast.NodeTag,
        _: ast.NodeIndex,
        _: ast.NodeData,
    ) Allocator.Error!void {
        self.enters += 1;
    }

    pub fn exit(
        _: *EnterCountingCtx,
        comptime _: ast.NodeTag,
        _: ast.NodeIndex,
        _: ast.NodeData,
    ) void {}
};

const HookRecorder = struct {
    events: *std.ArrayList(HookEvent),
    tag: helpers.NodeTag,
    action: Action,

    pub fn enter_node(
        self: *HookRecorder,
        data: ast.NodeData,
        index: ast.NodeIndex,
        _: *EnterCountingCtx,
    ) Allocator.Error!Action {
        try self.events.append(testing.allocator, .{ .exit = false, .index = index });
        return if (std.meta.activeTag(data) == self.tag) self.action else .proceed;
    }

    pub fn exit_node(
        self: *HookRecorder,
        _: ast.NodeData,
        index: ast.NodeIndex,
        _: *EnterCountingCtx,
    ) void {
        self.events.append(testing.allocator, .{ .exit = true, .index = index }) catch unreachable;
    }
};

fn walkRecorded(comptime whole_tree: bool, tree: *const ast.Tree, recorder: *HookRecorder) !u32 {
    var ctx: EnterCountingCtx = .{ .tree = tree };
    const options: traverser.Options = .{ .whole_tree = whole_tree };
    try traverser.walk(EnterCountingCtx, HookRecorder, recorder, &ctx, options);
    return ctx.enters;
}

test "a whole tree walk mutes the visitor instead of cutting the walk" {
    const cases = [_]struct { source: []const u8, tag: helpers.NodeTag }{
        .{ .source = "let before; function f(b) { let inner; } let after;", .tag = .function },
        .{ .source = deep_source, .tag = .expression_statement },
        .{ .source = deep_source, .tag = .identifier_reference },
    };
    for (cases) |case| {
        var tree = try parseModule(case.source);
        defer tree.deinit();
        var events: std.ArrayList(HookEvent) = .empty;
        defer events.deinit(testing.allocator);
        var all: HookRecorder = .{ .events = &events, .tag = case.tag, .action = .proceed };
        const nodes = try walkRecorded(false, &tree, &all);

        for ([_]Action{ .skip, .stop }) |action| {
            var plain_log: std.ArrayList(HookEvent) = .empty;
            defer plain_log.deinit(testing.allocator);
            var whole_log: std.ArrayList(HookEvent) = .empty;
            defer whole_log.deinit(testing.allocator);
            var plain: HookRecorder = .{ .events = &plain_log, .tag = case.tag, .action = action };
            var whole: HookRecorder = .{ .events = &whole_log, .tag = case.tag, .action = action };

            const plain_nodes = try walkRecorded(false, &tree, &plain);
            try testing.expectEqual(nodes, try walkRecorded(true, &tree, &whole));
            try testing.expectEqualSlices(HookEvent, plain_log.items, whole_log.items);
            if (action == .stop) try testing.expect(plain_nodes < nodes);
        }
    }
}

const PathCheckVisitor = struct {
    checked: bool = false,
    failure: ?anyerror = null,

    pub fn enter_identifier_reference(
        self: *PathCheckVisitor,
        _: ast.IdentifierReference,
        index: ast.NodeIndex,
        ctx: *traverser.basic.Ctx,
    ) Action {
        self.checked = true;
        check(index, ctx) catch |err| {
            self.failure = err;
        };
        return .proceed;
    }

    fn check(index: ast.NodeIndex, ctx: *traverser.basic.Ctx) !void {
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
    try traverser.basic.traverse(PathCheckVisitor, &tree, &visitor);
    try testing.expect(visitor.checked);
    if (visitor.failure) |err| return err;
}

const FoldVisitor = struct {
    literal_enters: usize = 0,

    pub fn enter_binary_expression(
        _: *FoldVisitor,
        expr: ast.BinaryExpression,
        index: ast.NodeIndex,
        ctx: *traverser.transform.Ctx,
    ) Action {
        ctx.tree.setData(index, ctx.tree.data(expr.left));
        return .proceed;
    }

    pub fn enter_numeric_literal(
        self: *FoldVisitor,
        _: ast.NumericLiteral,
        _: ast.NodeIndex,
        _: *traverser.transform.Ctx,
    ) Action {
        self.literal_enters += 1;
        return .proceed;
    }
};

test "transform traverser walks the replacement after setData" {
    var tree = try parseModule("1 + 2;");
    defer tree.deinit();

    var visitor = FoldVisitor{};
    try traverser.transform.traverse(FoldVisitor, &tree, &visitor);

    var replaced = false;
    var i: u32 = 0;
    while (i < tree.nodes.len) : (i += 1) {
        const data = tree.data(@fromBackingInt(i));
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
        _: ast.NodeIndex,
        ctx: *traverser.transform.Ctx,
    ) Allocator.Error!Action {
        try self.visited.append(self.gpa, ctx.tree.string(id.name)[0]);
        if (!self.grown) {
            self.grown = true;
            const span: ast.Span = .{ .start = 0, .end = 0 };
            for (0..512) |_| {
                const node = try ctx.tree.addNode(.{ .null_literal = .{} }, span);
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
    try traverser.transform.traverse(ExtrasGrowingVisitor, &tree, &visitor);

    try testing.expectEqualStrings("abc", visitor.visited.items);
}
