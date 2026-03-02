const std = @import("std");
const ast = @import("ast.zig");

pub const TraverseCtx = struct {
    tree: *const ast.ParseTree,
    parents: ParentStack = .{},

    /// The immediate parent node (null_node at root)
    pub inline fn parent(self: *const TraverseCtx) ast.NodeIndex {
        return self.parents.current();
    }

    /// Nth ancestor: 0 = parent, 1 = grandparent, ...
    pub inline fn ancestor(self: *const TraverseCtx, d: usize) ast.NodeIndex {
        return self.parents.get(d);
    }

    pub inline fn parentData(self: *const TraverseCtx) ?ast.NodeData {
        const p = self.parent();
        if (ast.isNull(p)) return null;
        return self.tree.getData(p);
    }

    /// Walk up ancestors until predicate matches. Returns the matching node index.
    pub fn findAncestor(self: *const TraverseCtx, comptime predicate: fn (ast.NodeData) bool) ?ast.NodeIndex {
        var i: usize = 0;
        while (true) {
            const a = self.parents.get(i);
            if (ast.isNull(a)) return null;
            if (predicate(self.tree.getData(a))) return a;
            i += 1;
        }
    }

    /// Nesting depth (0 at root)
    pub inline fn depth(self: *const TraverseCtx) u32 {
        return self.parents.len;
    }

    pub inline fn getData(self: *const TraverseCtx, index: ast.NodeIndex) ast.NodeData {
        return self.tree.getData(index);
    }

    pub inline fn getSpan(self: *const TraverseCtx, index: ast.NodeIndex) ast.Span {
        return self.tree.getSpan(index);
    }

    pub inline fn getExtra(self: *const TraverseCtx, range: ast.IndexRange) []const ast.NodeIndex {
        return self.tree.getExtra(range);
    }

    pub inline fn getSourceText(self: *const TraverseCtx, start: u32, len: u16) []const u8 {
        return self.tree.getSourceText(start, len);
    }

    pub inline fn getNodeText(self: *const TraverseCtx, index: ast.NodeIndex) []const u8 {
        const span = self.tree.getSpan(index);
        return self.tree.source[span.start..span.end];
    }
};

const ParentStack = struct {
    const MAX_DEPTH = 512;

    buf: [MAX_DEPTH]ast.NodeIndex = undefined,
    len: u32 = 0,

    inline fn push(self: *ParentStack, node: ast.NodeIndex) void {
        std.debug.assert(self.len < MAX_DEPTH);
        self.buf[self.len] = node;
        self.len += 1;
    }

    inline fn pop(self: *ParentStack) void {
        std.debug.assert(self.len > 0);
        self.len -= 1;
    }

    /// Current parent (top of stack). null_node if empty.
    inline fn current(self: *const ParentStack) ast.NodeIndex {
        if (self.len == 0) return ast.null_node;
        return self.buf[self.len - 1];
    }

    /// Get ancestor by depth: 0 = parent, 1 = grandparent, ...
    inline fn get(self: *const ParentStack, depth_offset: usize) ast.NodeIndex {
        if (depth_offset >= self.len) return ast.null_node;
        return self.buf[self.len - 1 - @as(u32, @intCast(depth_offset))];
    }
};

pub const Action = enum {
    /// Continue into children (default)
    proceed,
    /// Skip this node's children, continue to next sibling
    skip,
    /// Stop the entire traversal immediately
    stop,
};

/// Traverse the AST calling visitor hooks at each node.
///
/// `V` is any struct. If it declares `enter_call_expression`, `exit_function`, etc,
/// those get called. Hooks you don't declare are simply skipped.
///
/// Enter hooks: `fn (self: *V, node: ast.NodeIndex, ctx: *TraverseCtx) Action`
/// Exit hooks:  `fn (self: *V, node: ast.NodeIndex, ctx: *TraverseCtx) void`
///
/// Example:
/// ```
/// const Counter = struct {
///     count: usize = 0,
///
///     pub fn enter_function(self: *Counter, _: ast.NodeIndex, _: *TraverseCtx) Action {
///         self.count += 1;
///         return .proceed;
///     }
/// };
///
/// var c = Counter{};
/// traverse(Counter, &tree, &c);
/// ```
pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V) void {
    var ctx = TraverseCtx{ .tree = tree };
    _ = walkNode(V, visitor, tree.program, &ctx);
}

fn walkNode(comptime V: type, visitor: *V, index: ast.NodeIndex, ctx: *TraverseCtx) Action {
    if (ast.isNull(index)) return .proceed;

    const data = ctx.tree.getData(index);

    // enter
    if (comptime hasAnyEnter(V)) {
        switch (callEnter(V, visitor, index, data, ctx)) {
            .skip => return .proceed,
            .stop => return .stop,
            .proceed => {},
        }
    }

    // push parent, walk children, pop parent
    if (comptime hasAnyHook(V)) {
        ctx.parents.push(index);
    }

    const child_result = walkChildren(V, visitor, data, ctx);

    if (comptime hasAnyHook(V)) {
        ctx.parents.pop();
    }

    // exit
    if (comptime hasAnyExit(V)) {
        if (child_result != .stop) {
            callExit(V, visitor, index, data, ctx);
        }
    }

    return child_result;
}

// enter dispatch

fn callEnter(comptime V: type, visitor: *V, index: ast.NodeIndex, data: ast.NodeData, ctx: *TraverseCtx) Action {
    // catch-all enter_node
    if (comptime @hasDecl(V, "enter_node")) {
        switch (visitor.enter_node(index, ctx)) {
            .skip => return .skip,
            .stop => return .stop,
            .proceed => {},
        }
    }

    return callEnterTyped(V, visitor, index, data, ctx);
}

fn callEnterTyped(comptime V: type, visitor: *V, index: ast.NodeIndex, data: ast.NodeData, ctx: *TraverseCtx) Action {
    switch (data) {
        inline else => |_, tag| {
            const enter_name = comptime enterNameFor(tag);
            if (comptime @hasDecl(V, enter_name)) {
                return @field(V, enter_name)(visitor, index, ctx);
            }
            return .proceed;
        },
    }
}

fn callExit(comptime V: type, visitor: *V, index: ast.NodeIndex, data: ast.NodeData, ctx: *TraverseCtx) void {
    callExitTyped(V, visitor, index, data, ctx);

    if (comptime @hasDecl(V, "exit_node")) {
        visitor.exit_node(index, ctx);
    }
}

fn callExitTyped(comptime V: type, visitor: *V, index: ast.NodeIndex, data: ast.NodeData, ctx: *TraverseCtx) void {
    switch (data) {
        inline else => |_, tag| {
            const exit_name = comptime exitNameFor(tag);
            if (comptime @hasDecl(V, exit_name)) {
                @field(V, exit_name)(visitor, index, ctx);
            }
        },
    }
}

// any field of type NodeIndex gets walked. any field of type IndexRange
// gets iterated and each element walked. everything else is skipped.

fn walkChildren(comptime V: type, visitor: *V, data: ast.NodeData, ctx: *TraverseCtx) Action {
    switch (data) {
        inline else => |payload| {
            const T = @TypeOf(payload);
            if (@typeInfo(T) == .@"struct") {
                return walkStructFields(V, visitor, T, payload, ctx);
            }
            // void payload (this_expression, null_literal, etc), leaf node
            return .proceed;
        },
    }
}

fn walkStructFields(comptime V: type, visitor: *V, comptime T: type, payload: T, ctx: *TraverseCtx) Action {
    const fields = @typeInfo(T).@"struct".fields;

    inline for (fields) |field| {
        if (field.type == ast.NodeIndex) {
            if (walkNode(V, visitor, @field(payload, field.name), ctx) == .stop) return .stop;
        } else if (field.type == ast.IndexRange) {
            const range = @field(payload, field.name);
            const children = ctx.tree.getExtra(range);
            for (children) |child| {
                if (walkNode(V, visitor, child, ctx) == .stop) return .stop;
            }
        }
    }

    return .proceed;
}

//

fn enterNameFor(comptime tag: std.meta.Tag(ast.NodeData)) []const u8 {
    return "enter_" ++ @tagName(tag);
}

fn exitNameFor(comptime tag: std.meta.Tag(ast.NodeData)) []const u8 {
    return "exit_" ++ @tagName(tag);
}

fn hasAnyEnter(comptime V: type) bool {
    if (@hasDecl(V, "enter_node")) return true;
    for (@typeInfo(ast.NodeData).@"union".fields) |f| {
        if (@hasDecl(V, "enter_" ++ f.name)) return true;
    }
    return false;
}

fn hasAnyExit(comptime V: type) bool {
    if (@hasDecl(V, "exit_node")) return true;
    for (@typeInfo(ast.NodeData).@"union".fields) |f| {
        if (@hasDecl(V, "exit_" ++ f.name)) return true;
    }
    return false;
}

fn hasAnyHook(comptime V: type) bool {
    return hasAnyEnter(V) or hasAnyExit(V);
}
