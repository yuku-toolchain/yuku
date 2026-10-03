const std = @import("std");
const ast = @import("../ast.zig");

const Allocator = std.mem.Allocator;

/// Controls what happens at each node during traversal.
pub const Action = enum {
    /// Keep going into children.
    proceed,
    /// Skip this node's children, move to the next sibling.
    skip,
    /// Stop the entire traversal right now.
    stop,
};

/// Walks the tree, calling visitor hooks at each node. `C` is the context
/// type with a `.tree` field, `V` the visitor type with hooks such as
/// `enter_function` or the catch-all `enter_node`.
pub fn walk(comptime C: type, comptime V: type, visitor: *V, ctx: *C) Allocator.Error!void {
    comptime validateHooks(V);
    std.debug.assert(ctx.tree.root != .null);

    var path_buffer: [walk_depth_max]ast.NodeIndex = undefined;
    if (comptime @hasField(C, "path")) ctx.path.items = &path_buffer;
    defer if (comptime @hasField(C, "path")) {
        ctx.path.items = &.{};
    };

    _ = try walkNode(C, V, visitor, ctx, ctx.tree.root, 0, undefined);
}

const walk_depth_max = 256;
// walkDeep runs each half through walkNode, as a second hook call site stops inlining
const walk_enter_only = walk_depth_max + 1;
const walk_exit_only = walk_depth_max + 2;

fn ExitData(comptime C: type) type {
    return if (@typeInfo(@FieldType(C, "tree")).pointer.is_const) void else *const ast.NodeData;
}

fn walkNode(
    comptime C: type,
    comptime V: type,
    visitor: *V,
    ctx: *C,
    index: ast.NodeIndex,
    depth: u32,
    exit_data: ExitData(C),
) Allocator.Error!Action {
    if (index == .null) return .proceed;
    const deep = depth >= walk_depth_max;
    if (deep and depth == walk_depth_max) return walkDeep(C, V, visitor, ctx, index);

    var action: Action = .proceed;
    var result: Action = .proceed;
    var current: ast.NodeData = undefined;
    if (!deep or depth == walk_enter_only) {
        const data = ctx.tree.data(index);
        action = try dispatch.enter(C, V, visitor, data, index, ctx);

        // re-read after enter so a replaced node walks the replacement's children
        current = if (comptime ExitData(C) == void) data else ctx.tree.data(index);

        if (deep) return action;
        if (action == .proceed) result = try walkChildren(C, V, visitor, ctx, current, depth + 1);
    } else {
        std.debug.assert(depth == walk_exit_only);
        current = if (comptime ExitData(C) == void) ctx.tree.data(index) else exit_data.*;
    }

    dispatch.exit(C, V, visitor, current, index, ctx);

    return if (action == .stop) .stop else result;
}

fn walkChildren(
    comptime C: type,
    comptime V: type,
    visitor: *V,
    ctx: *C,
    data: ast.NodeData,
    depth: u32,
) Allocator.Error!Action {
    switch (data) {
        inline else => |node| {
            const T = @TypeOf(node);
            if (@typeInfo(T) == .@"struct") {
                return walkStructFields(C, V, visitor, ctx, T, node, depth);
            }
            return .proceed;
        },
    }
}

fn walkStructFields(
    comptime C: type,
    comptime V: type,
    visitor: *V,
    ctx: *C,
    comptime T: type,
    payload: T,
    depth: u32,
) Allocator.Error!Action {
    const fields = @typeInfo(T).@"struct".fields;

    inline for (fields) |field| {
        if (field.type == ast.NodeIndex) {
            const child = @field(payload, field.name);
            const action = try walkNode(C, V, visitor, ctx, child, depth, undefined);
            if (action == .stop) return .stop;
        } else if (field.type == ast.IndexRange) {
            const range = @field(payload, field.name);
            if (comptime @typeInfo(@TypeOf(ctx.tree)).pointer.is_const) {
                for (ctx.tree.extra(range)) |child| {
                    const action = try walkNode(C, V, visitor, ctx, child, depth, undefined);
                    if (action == .stop) return .stop;
                }
            } else {
                // a visitor may append extras and move the backing array
                for (0..range.len) |i| {
                    const child = ctx.tree.extra(range)[i];
                    const action = try walkNode(C, V, visitor, ctx, child, depth, undefined);
                    if (action == .stop) return .stop;
                }
            }
        }
    }

    return .proceed;
}

noinline fn walkDeep(
    comptime C: type,
    comptime V: type,
    visitor: *V,
    ctx: *C,
    root: ast.NodeIndex,
) Allocator.Error!Action {
    const gpa = ctx.tree.arena.child_allocator;
    var steps: std.ArrayList(WalkStep) = .empty;
    defer steps.deinit(gpa);

    const outer_path = if (comptime @hasField(C, "path")) ctx.path.items else {};
    if (comptime @hasField(C, "path")) ctx.path.items = try gpa.dupe(ast.NodeIndex, outer_path);
    defer if (comptime @hasField(C, "path")) {
        gpa.free(ctx.path.items);
        ctx.path.items = outer_path;
    };

    var stopped = false;
    try steps.append(gpa, .{ .index = root, .exit = null });
    while (steps.pop()) |step| {
        const index = step.index;
        if (step.exit) |data| {
            const exit_data = if (comptime ExitData(C) == void) {} else &data;
            _ = try walkNode(C, V, visitor, ctx, index, walk_exit_only, exit_data);
            continue;
        }
        if (stopped or index == .null) continue;

        if (comptime @hasField(C, "path")) {
            if (ctx.path.len == ctx.path.items.len) {
                ctx.path.items = try gpa.realloc(ctx.path.items, 2 * ctx.path.len);
            }
        }

        const action = try walkNode(C, V, visitor, ctx, index, walk_enter_only, undefined);
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
                inline for (@typeInfo(T).@"struct".fields) |field| {
                    if (field.type == ast.NodeIndex) {
                        const child = @field(payload, field.name);
                        try steps.append(gpa, .{ .index = child, .exit = null });
                    } else if (field.type == ast.IndexRange) {
                        for (tree.extra(@field(payload, field.name))) |child| {
                            try steps.append(gpa, .{ .index = child, .exit = null });
                        }
                    }
                }
            }
        },
    }
    std.mem.reverse(WalkStep, steps.items[children_start..]);
}

/// Wraps a visitor so the context's hooks run around the user hooks at each
/// node. `ctx.enter` runs before the user's enter hooks, the optional
/// `ctx.post_enter` after them and before the children, and `ctx.exit` after
/// the user's exit hooks.
pub fn Layer(comptime C: type, comptime V: type) type {
    comptime validateHooks(V);
    return struct {
        inner: *V,

        pub fn enter_node(
            self: *@This(),
            data: ast.NodeData,
            index: ast.NodeIndex,
            ctx: *C,
        ) Allocator.Error!Action {
            try ctx.enter(index, data);
            const action = try dispatch.enter(C, V, self.inner, data, index, ctx);
            if (comptime @hasDecl(C, "post_enter")) try ctx.post_enter(index, data);
            return action;
        }

        pub fn exit_node(
            self: *@This(),
            data: ast.NodeData,
            index: ast.NodeIndex,
            ctx: *C,
        ) void {
            dispatch.exit(C, V, self.inner, data, index, ctx);
            ctx.exit(index, data);
        }
    };
}

/// Dispatch helpers for calling visitor hooks.
pub const dispatch = struct {
    /// Dispatches the enter phase, calls `enter_node` first, then the typed hook.
    pub fn enter(
        comptime C: type,
        comptime V: type,
        visitor: *V,
        data: ast.NodeData,
        index: ast.NodeIndex,
        ctx: *C,
    ) Allocator.Error!Action {
        if (comptime @hasDecl(V, "enter_node")) {
            switch (try unwrapAction(visitor.enter_node(data, index, ctx))) {
                .skip => return .skip,
                .stop => return .stop,
                .proceed => {},
            }
        }
        return enterTyped(C, V, visitor, data, index, ctx);
    }

    /// Dispatches only the typed enter hook (e.g. `enter_function`), skipping `enter_node`.
    pub fn enterTyped(
        comptime C: type,
        comptime V: type,
        visitor: *V,
        data: ast.NodeData,
        index: ast.NodeIndex,
        ctx: *C,
    ) Allocator.Error!Action {
        switch (data) {
            inline else => |node, tag| {
                if (comptime @hasDecl(V, "enter_" ++ @tagName(tag))) {
                    const hook = @field(V, "enter_" ++ @tagName(tag));
                    return unwrapAction(hook(visitor, node, index, ctx));
                }
                return .proceed;
            },
        }
    }

    /// Dispatches the exit phase, calls the typed hook first, then `exit_node`.
    pub fn exit(
        comptime C: type,
        comptime V: type,
        visitor: *V,
        data: ast.NodeData,
        index: ast.NodeIndex,
        ctx: *C,
    ) void {
        exitTyped(C, V, visitor, data, index, ctx);
        if (comptime @hasDecl(V, "exit_node")) {
            visitor.exit_node(data, index, ctx);
        }
    }

    /// Dispatches only the typed exit hook (e.g. `exit_function`), skipping `exit_node`.
    pub fn exitTyped(
        comptime C: type,
        comptime V: type,
        visitor: *V,
        data: ast.NodeData,
        index: ast.NodeIndex,
        ctx: *C,
    ) void {
        switch (data) {
            inline else => |node, tag| {
                if (comptime @hasDecl(V, "exit_" ++ @tagName(tag))) {
                    @field(V, "exit_" ++ @tagName(tag))(visitor, node, index, ctx);
                }
            },
        }
    }
};

inline fn unwrapAction(result: anytype) Allocator.Error!Action {
    return result;
}

fn validateHooks(comptime V: type) void {
    for (@typeInfo(V).@"struct".decls) |decl| {
        const name = decl.name;

        if (comptime std.mem.eql(u8, name, "enter_node") or std.mem.eql(u8, name, "exit_node"))
            continue;

        const node_name = if (std.mem.startsWith(u8, name, "enter_"))
            name["enter_".len..]
        else if (std.mem.startsWith(u8, name, "exit_"))
            name["exit_".len..]
        else
            continue;

        if (!@hasField(ast.NodeData, node_name)) {
            @compileError("Invalid visitor hook '" ++ name ++
                "': no field '" ++ node_name ++ "' exists in ast.NodeData");
        }

        const expected = @FieldType(ast.NodeData, node_name);
        const hook_fn_params = @typeInfo(@TypeOf(@field(V, name))).@"fn".params;

        if (hook_fn_params.len >= 3) {
            if (hook_fn_params[1].type) |actual| {
                if (actual != expected) {
                    @compileError("Visitor hook '" ++ name ++
                        "': expected payload type '" ++ @typeName(expected) ++
                        "', found '" ++ @typeName(actual) ++ "'");
                }
            }
        }
    }
}

/// Tracks the path of node indices from root to the current position.
pub const NodePath = struct {
    items: []ast.NodeIndex = &.{},
    len: usize = 0,

    /// Returns the parent node index, or `null` if at the root.
    pub inline fn parent(self: *const NodePath) ?ast.NodeIndex {
        return self.ancestor(1);
    }

    /// Returns the nth ancestor. 0 = current node, 1 = parent, 2 = grandparent, etc.
    pub inline fn ancestor(self: *const NodePath, n: usize) ?ast.NodeIndex {
        if (n >= self.len) return null;
        return self.items[self.len - 1 - n];
    }

    /// Returns the current nesting depth (1 at the root).
    pub inline fn depth(self: *const NodePath) usize {
        return self.len;
    }

    /// Returns an iterator that walks from the current node up to the root.
    pub fn ancestors(self: *const NodePath) AncestorIterator {
        return .{ .items = self.items[0..self.len], .pos = self.len };
    }

    /// Walks up from the current node to root, yielding each node index.
    pub const AncestorIterator = struct {
        items: []const ast.NodeIndex,
        pos: usize,

        /// Returns the next ancestor node index, or `null` when the root has been passed.
        pub fn next(self: *AncestorIterator) ?ast.NodeIndex {
            if (self.pos == 0) return null;
            self.pos -= 1;
            return self.items[self.pos];
        }
    };

    /// Adds a node to the path when entering it.
    pub fn push(self: *NodePath, index: ast.NodeIndex) void {
        std.debug.assert(index != .null);
        self.items[self.len] = index;
        self.len += 1;
    }

    /// Removes the current node from the path when exiting it.
    pub fn pop(self: *NodePath) void {
        std.debug.assert(self.len > 0);
        self.len -= 1;
    }
};
