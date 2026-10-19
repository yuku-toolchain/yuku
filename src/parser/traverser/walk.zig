const std = @import("std");
const ast = @import("../ast.zig");

const Allocator = std.mem.Allocator;
const Tag = ast.NodeTag;

/// What the walk does after a node's enter hooks.
pub const Action = enum {
    /// Walks the node's children.
    proceed,
    /// Skips the node's children.
    skip,
    /// Stops the whole traversal.
    stop,
};

pub const Options = struct {
    /// Walks every node, so `.skip` and `.stop` only mute the visitor, as a context that builds
    /// a model of the whole tree needs.
    whole_tree: bool,
};

/// Walks the tree from `ctx.tree.root`, calling hooks such as `enter_function` or `enter_node`.
/// A context's `enter`, optional `post_enter`, and `exit` run around them.
pub fn walk(
    comptime C: type,
    comptime V: type,
    visitor: *V,
    ctx: *C,
    comptime options: Options,
) Allocator.Error!void {
    comptime validateHooks(V);
    std.debug.assert(ctx.tree.root != .null);

    var path_buffer: [walk_depth_max]ast.NodeIndex = undefined;
    if (comptime @hasField(C, "path")) ctx.path.items = &path_buffer;
    defer if (comptime @hasField(C, "path")) {
        ctx.path.items = &.{};
    };

    var walker: Walker(C, V, options) = .init(visitor, ctx);
    if (walker.node(ctx, ctx.tree.root, 0) == .fail) return error.OutOfMemory;
}

const walk_depth_max = 256;

const Step = enum(u8) { proceed, stop, fail };

fn Walker(comptime C: type, comptime V: type, comptime options: Options) type {
    // hooks on a mutable tree may replace nodes, so the walk re-reads them
    const mutates = !@typeInfo(@FieldType(C, "tree")).pointer.attrs.@"const";

    return struct {
        visitor: *V,
        // a const tree stays put during the walk, so its arrays are read once
        nodes: if (mutates) void else []const ast.NodeData,
        extras: if (mutates) void else []const ast.NodeIndex,
        muting: Action = .proceed,
        muted: u32 = 0,

        const Self = @This();

        fn init(visitor: *V, ctx: *C) Self {
            if (comptime mutates) return .{ .visitor = visitor, .nodes = {}, .extras = {} };
            return .{
                .visitor = visitor,
                .nodes = ctx.tree.nodes.items(.data),
                .extras = ctx.tree.extras.items,
            };
        }

        inline fn dataOf(self: *const Self, ctx: *C, index: ast.NodeIndex) ast.NodeData {
            if (comptime mutates) return ctx.tree.data(index);
            return self.nodes[@backingInt(index)];
        }

        inline fn node(self: *Self, ctx: *C, index: ast.NodeIndex, depth: u32) Step {
            std.debug.assert(index != .null);
            if (depth == walk_depth_max) return self.deep(ctx, index);
            return visits[@backingInt(self.dataOf(ctx, index))](self, ctx, index, depth);
        }

        // one function per tag, so every hook sees the tag as known
        const visits = table: {
            const Visit = *const fn (*Self, *C, ast.NodeIndex, u32) Step;
            var table: [@typeInfo(Tag).@"enum".field_names.len]Visit = undefined;
            for (&table, 0..) |*entry, i| entry.* = visitOf(@fromBackingInt(@intCast(i)));
            break :table table;
        };

        fn visitOf(comptime tag: Tag) *const fn (*Self, *C, ast.NodeIndex, u32) Step {
            return struct {
                fn visit(self: *Self, ctx: *C, index: ast.NodeIndex, depth: u32) Step {
                    @setEvalBranchQuota(100_000);
                    const data = tagged(tag, self.dataOf(ctx, index));
                    const action = self.enter(ctx, tag, data, index) catch return .fail;
                    if (comptime mutates) {
                        const current = ctx.tree.data(index);
                        if (current != tag) {
                            @branchHint(.cold);
                            return self.finishAny(ctx, action, index, depth);
                        }
                        return self.finish(ctx, tag, current, action, index, depth);
                    }
                    return self.finish(ctx, tag, data, action, index, depth);
                }
            }.visit;
        }

        inline fn tagged(comptime tag: Tag, data: ast.NodeData) ast.NodeData {
            return @unionInit(ast.NodeData, @tagName(tag), @field(data, @tagName(tag)));
        }

        inline fn finish(
            self: *Self,
            ctx: *C,
            comptime tag: Tag,
            data: ast.NodeData,
            action: Action,
            index: ast.NodeIndex,
            depth: u32,
        ) Step {
            var result: Step = .proceed;
            if (action == .proceed) {
                // a mutable walk calls `fields` from two places, which would outline it
                const modifier: std.builtin.CallModifier = if (mutates) .always_inline else .auto;
                const payload = @field(data, @tagName(tag));
                result = @call(modifier, fields, .{ self, ctx, payload, depth + 1 });
                if (result == .fail) return .fail;
            }
            self.exit(ctx, tag, data, index);
            return if (action == .stop) .stop else result;
        }

        fn finishAny(self: *Self, ctx: *C, action: Action, index: ast.NodeIndex, depth: u32) Step {
            @setEvalBranchQuota(100_000);
            const data = ctx.tree.data(index);
            switch (data) {
                inline else => |_, tag| return self.finish(ctx, tag, data, action, index, depth),
            }
        }

        fn fields(self: *Self, ctx: *C, payload: anytype, depth: u32) Step {
            const T = @TypeOf(payload);
            if (@typeInfo(T) != .@"struct") return .proceed;
            const info = @typeInfo(T).@"struct";
            inline for (info.field_names, info.field_types) |name, Field| {
                if (Field == ast.NodeIndex) {
                    const child = @field(payload, name);
                    if (child != .null) {
                        const step = self.node(ctx, child, depth);
                        if (step != .proceed) return step;
                    }
                } else if (Field == ast.IndexRange) {
                    const range = @field(payload, name);
                    if (comptime mutates) {
                        // a visitor may append extras and move the backing array
                        for (0..range.len) |i| {
                            const child = ctx.tree.extra(range)[i];
                            if (child == .null) continue;
                            const step = self.node(ctx, child, depth);
                            if (step != .proceed) return step;
                        }
                    } else {
                        for (self.extras[range.start..][0..range.len]) |child| {
                            if (child == .null) continue;
                            const step = self.node(ctx, child, depth);
                            if (step != .proceed) return step;
                        }
                    }
                }
            }
            return .proceed;
        }

        inline fn enter(
            self: *Self,
            ctx: *C,
            comptime tag: Tag,
            data: ast.NodeData,
            index: ast.NodeIndex,
        ) Allocator.Error!Action {
            if (comptime @hasDecl(C, "enter")) try ctx.enter(tag, index, data);
            var action: Action = .proceed;
            if (comptime !options.whole_tree) {
                action = try self.enterHooks(ctx, tag, data, index);
            } else if (self.muting == .proceed) {
                std.debug.assert(self.muted == 0);
                const visited = try self.enterHooks(ctx, tag, data, index);
                if (visited != .proceed) self.muting = visited;
            } else {
                self.muted += 1;
            }
            if (comptime @hasDecl(C, "post_enter")) try ctx.post_enter(tag, index, data);
            return action;
        }

        inline fn enterHooks(
            self: *Self,
            ctx: *C,
            comptime tag: Tag,
            data: ast.NodeData,
            index: ast.NodeIndex,
        ) Allocator.Error!Action {
            if (comptime @hasDecl(V, "enter_node")) {
                const action = try unwrapAction(self.visitor.enter_node(data, index, ctx));
                if (action != .proceed) return action;
            }
            const name = "enter_" ++ @tagName(tag);
            if (comptime !@hasDecl(V, name)) return .proceed;
            const payload = @field(data, @tagName(tag));
            return unwrapAction(@field(V, name)(self.visitor, payload, index, ctx));
        }

        inline fn exit(
            self: *Self,
            ctx: *C,
            comptime tag: Tag,
            data: ast.NodeData,
            index: ast.NodeIndex,
        ) void {
            if (!options.whole_tree or self.hooked()) {
                const name = "exit_" ++ @tagName(tag);
                if (comptime @hasDecl(V, name)) {
                    @field(V, name)(self.visitor, @field(data, @tagName(tag)), index, ctx);
                }
                if (comptime @hasDecl(V, "exit_node")) self.visitor.exit_node(data, index, ctx);
            }
            if (comptime @hasDecl(C, "exit")) ctx.exit(tag, index, data);
        }

        inline fn hooked(self: *Self) bool {
            if (self.muting == .proceed) return true;
            if (self.muted > 0) {
                self.muted -= 1;
                return false;
            }
            if (self.muting == .skip) self.muting = .proceed;
            return true;
        }

        fn enterAny(
            self: *Self,
            ctx: *C,
            data: ast.NodeData,
            index: ast.NodeIndex,
        ) Allocator.Error!Action {
            @setEvalBranchQuota(100_000);
            switch (data) {
                inline else => |_, tag| return self.enter(ctx, tag, data, index),
            }
        }

        fn exitAny(self: *Self, ctx: *C, data: ast.NodeData, index: ast.NodeIndex) void {
            @setEvalBranchQuota(100_000);
            switch (data) {
                inline else => |_, tag| self.exit(ctx, tag, data, index),
            }
        }

        noinline fn deep(self: *Self, ctx: *C, root: ast.NodeIndex) Step {
            return self.deepWalk(ctx, root) catch .fail;
        }

        fn deepWalk(self: *Self, ctx: *C, root: ast.NodeIndex) Allocator.Error!Step {
            const gpa = ctx.tree.arena.child_allocator;
            var steps: std.ArrayList(WalkStep) = .empty;
            defer steps.deinit(gpa);

            const outer_path = if (comptime @hasField(C, "path")) ctx.path.items else {};
            if (comptime @hasField(C, "path")) {
                ctx.path.items = try gpa.dupe(ast.NodeIndex, outer_path);
            }
            defer if (comptime @hasField(C, "path")) {
                gpa.free(ctx.path.items);
                ctx.path.items = outer_path;
            };

            var stopped = false;
            try steps.append(gpa, .{ .index = root, .exit = null });
            while (steps.pop()) |step| {
                const index = step.index;
                if (step.exit) |data| {
                    self.exitAny(ctx, data, index);
                    continue;
                }
                if (stopped or index == .null) continue;

                if (comptime @hasField(C, "path")) {
                    if (ctx.path.len == ctx.path.items.len) {
                        ctx.path.items = try gpa.realloc(ctx.path.items, 2 * ctx.path.len);
                    }
                }

                const action = try self.enterAny(ctx, ctx.tree.data(index), index);
                const current = ctx.tree.data(index);
                try steps.append(gpa, .{ .index = index, .exit = current });
                switch (action) {
                    .proceed => try walkDeepPushChildren(ctx.tree, current, &steps, gpa),
                    .skip => {},
                    .stop => stopped = true,
                }
            }
            return if (stopped) .stop else .proceed;
        }
    };
}

const WalkStep = struct { index: ast.NodeIndex, exit: ?ast.NodeData };

fn walkDeepPushChildren(
    tree: anytype,
    data: ast.NodeData,
    steps: *std.ArrayList(WalkStep),
    gpa: Allocator,
) Allocator.Error!void {
    const children_start = steps.items.len;
    switch (data) {
        inline else => |payload| {
            const T = @TypeOf(payload);
            if (@typeInfo(T) == .@"struct") {
                const info = @typeInfo(T).@"struct";
                inline for (info.field_names, info.field_types) |name, Field| {
                    if (Field == ast.NodeIndex) {
                        const child = @field(payload, name);
                        try steps.append(gpa, .{ .index = child, .exit = null });
                    } else if (Field == ast.IndexRange) {
                        for (tree.extra(@field(payload, name))) |child| {
                            try steps.append(gpa, .{ .index = child, .exit = null });
                        }
                    }
                }
            }
        },
    }
    std.mem.reverse(WalkStep, steps.items[children_start..]);
}

inline fn unwrapAction(result: anytype) Allocator.Error!Action {
    return result;
}

fn validateHooks(comptime V: type) void {
    for (@typeInfo(V).@"struct".decl_names) |name| {
        const node_name = if (std.mem.startsWith(u8, name, "enter_"))
            name["enter_".len..]
        else if (std.mem.startsWith(u8, name, "exit_"))
            name["exit_".len..]
        else
            continue;
        if (std.mem.eql(u8, node_name, "node")) continue;

        if (!@hasField(ast.NodeData, node_name)) {
            @compileError("Invalid visitor hook '" ++ name ++
                "': no field '" ++ node_name ++ "' exists in ast.NodeData");
        }

        const expected = @FieldType(ast.NodeData, node_name);
        const hook_param_types = @typeInfo(@TypeOf(@field(V, name))).@"fn".param_types;

        if (hook_param_types.len >= 3) {
            if (hook_param_types[1]) |actual| {
                if (actual != expected) {
                    @compileError("Visitor hook '" ++ name ++
                        "': expected payload type '" ++ @typeName(expected) ++
                        "', found '" ++ @typeName(actual) ++ "'");
                }
            }
        }
    }
}

pub fn PathCtx(comptime Tree: type) type {
    return struct {
        tree: Tree,
        path: NodePath = .{},

        pub inline fn enter(
            self: *@This(),
            comptime _: ast.NodeTag,
            index: ast.NodeIndex,
            _: ast.NodeData,
        ) Allocator.Error!void {
            self.path.push(index);
        }

        pub inline fn exit(
            self: *@This(),
            comptime _: ast.NodeTag,
            _: ast.NodeIndex,
            _: ast.NodeData,
        ) void {
            self.path.pop();
        }
    };
}

/// The node indices from the root to the current node.
pub const NodePath = struct {
    items: []ast.NodeIndex = &.{},
    len: usize = 0,

    /// The parent of the current node, or `null` at the root.
    pub inline fn parent(self: *const NodePath) ?ast.NodeIndex {
        return self.ancestor(1);
    }

    /// The nth ancestor, 0 being the current node, or `null` past the root.
    pub inline fn ancestor(self: *const NodePath, n: usize) ?ast.NodeIndex {
        if (n >= self.len) return null;
        return self.items[self.len - 1 - n];
    }

    /// The nesting depth, 1 at the root.
    pub inline fn depth(self: *const NodePath) usize {
        return self.len;
    }

    /// Iterates from the current node up to the root.
    pub fn ancestors(self: *const NodePath) AncestorIterator {
        return .{ .items = self.items[0..self.len], .pos = self.len };
    }

    /// Yields node indices from the current node up to the root.
    pub const AncestorIterator = struct {
        items: []const ast.NodeIndex,
        pos: usize,

        /// The next node up the path, or `null` past the root.
        pub fn next(self: *AncestorIterator) ?ast.NodeIndex {
            if (self.pos == 0) return null;
            self.pos -= 1;
            return self.items[self.pos];
        }
    };

    /// Pushes an entered node.
    pub fn push(self: *NodePath, index: ast.NodeIndex) void {
        std.debug.assert(index != .null);
        self.items[self.len] = index;
        self.len += 1;
    }

    /// Pops the exited node.
    pub fn pop(self: *NodePath) void {
        std.debug.assert(self.len > 0);
        self.len -= 1;
    }
};
