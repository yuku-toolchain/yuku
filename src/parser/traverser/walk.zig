const std = @import("std");
const ast = @import("../ast.zig");

const Allocator = std.mem.Allocator;
const NodeTag = std.meta.Tag(ast.NodeData);

/// Tracks the path of node indices from root to the current position.
/// Provides efficient access to parent and ancestor nodes during traversal.
pub const NodePath = struct {
    stack: std.ArrayList(ast.NodeIndex) = .{},

    /// Returns the parent of the current node.
    pub inline fn parent(self: *const NodePath) ?ast.NodeIndex {
        const items = self.stack.items;
        return if (items.len >= 2) items[items.len - 2] else null;
    }

    /// Returns the ancestor `n` levels up (0 = current, 1 = parent, 2 = grandparent).
    pub inline fn ancestor(self: *const NodePath, n: usize) ?ast.NodeIndex {
        const items = self.stack.items;
        return if (n < items.len) items[items.len - 1 - n] else null;
    }

    /// The current nesting depth.
    pub inline fn depth(self: *const NodePath) usize {
        return self.stack.items.len;
    }
};

/// Controls traversal flow at each node.
pub const Action = enum {
    /// Continue into children (default).
    proceed,
    /// Skip this node's children, continue to next sibling.
    skip,
    /// Stop the entire traversal immediately.
    stop,
};

/// Walk the AST, calling visitor hooks at each node.
///
/// `C` is a context type with a `.tree: *const ast.ParseTree` field.
/// It may optionally declare `onEnter`/`onExit` hooks called when
/// entering/exiting nodes. If `onEnter` returns an error, it is
/// propagated through the walk.
///
/// `V` is a visitor type with optional per-node enter/exit hooks
/// (e.g. `enter_function`, `exit_block_statement`).
pub fn walk(comptime C: type, comptime V: type, visitor: *V, ctx: *C) Allocator.Error!void {
    comptime validateHooks(V);
    _ = try walkNode(C, V, visitor, ctx.tree.program, ctx);
}

fn walkNode(comptime C: type, comptime V: type, visitor: *V, index: ast.NodeIndex, ctx: *C) Allocator.Error!Action {
    if (ast.isNull(index)) return .proceed;

    // track node path (push now, defer pop for all exit paths)
    if (comptime @hasField(C, "path")) {
        try ctx.path.stack.append(ctx.allocator, index);
    }

    defer if (comptime @hasField(C, "path")) {
        _ = ctx.path.stack.pop();
    };

    const data = ctx.tree.getData(index);
    const tag = std.meta.activeTag(data);

    // visitor enter hooks
    if (comptime hasAnyEnter(V)) {
        switch (callEnter(C, V, visitor, data, index, ctx)) {
            .skip => return .proceed,
            .stop => return .stop,
            .proceed => {},
        }
    }

    // context enter hook
    if (comptime @hasDecl(C, "onEnter")) {
        try ctx.onEnter(index, tag);
    }

    const result = try walkChildren(C, V, visitor, data, ctx);

    // visitor exit hooks
    if (comptime hasAnyExit(V)) {
        if (result != .stop) {
            callExit(C, V, visitor, data, index, ctx);
        }
    }

    // context exit hook
    if (comptime @hasDecl(C, "onExit")) ctx.onExit(index, tag);

    return result;
}

fn callEnter(comptime C: type, comptime V: type, visitor: *V, data: ast.NodeData, index: ast.NodeIndex, ctx: *C) Action {
    if (comptime @hasDecl(V, "enter_node")) {
        switch (visitor.enter_node(data, index, ctx)) {
            .skip => return .skip,
            .stop => return .stop,
            .proceed => {},
        }
    }
    return callEnterTyped(C, V, visitor, data, index, ctx);
}

fn callEnterTyped(comptime C: type, comptime V: type, visitor: *V, data: ast.NodeData, index: ast.NodeIndex, ctx: *C) Action {
    switch (data) {
        inline else => |node, tag| {
            if (comptime @hasDecl(V, "enter_" ++ @tagName(tag))) {
                return @field(V, "enter_" ++ @tagName(tag))(visitor, node, index, ctx);
            }
            return .proceed;
        },
    }
}

fn callExit(comptime C: type, comptime V: type, visitor: *V, data: ast.NodeData, index: ast.NodeIndex, ctx: *C) void {
    callExitTyped(C, V, visitor, data, index, ctx);

    if (comptime @hasDecl(V, "exit_node")) {
        visitor.exit_node(data, index, ctx);
    }
}

fn callExitTyped(comptime C: type, comptime V: type, visitor: *V, data: ast.NodeData, index: ast.NodeIndex, ctx: *C) void {
    switch (data) {
        inline else => |node, tag| {
            if (comptime @hasDecl(V, "exit_" ++ @tagName(tag))) {
                @field(V, "exit_" ++ @tagName(tag))(visitor, node, index, ctx);
            }
        },
    }
}

fn walkChildren(comptime C: type, comptime V: type, visitor: *V, data: ast.NodeData, ctx: *C) Allocator.Error!Action {
    switch (data) {
        inline else => |node| {
            const T = @TypeOf(node);
            if (@typeInfo(T) == .@"struct") {
                return walkStructFields(C, V, visitor, T, node, ctx);
            }
            return .proceed;
        },
    }
}

fn walkStructFields(comptime C: type, comptime V: type, visitor: *V, comptime T: type, payload: T, ctx: *C) Allocator.Error!Action {
    const fields = @typeInfo(T).@"struct".fields;

    inline for (fields) |field| {
        if (field.type == ast.NodeIndex) {
            if ((try walkNode(C, V, visitor, @field(payload, field.name), ctx)) == .stop) return .stop;
        } else if (field.type == ast.IndexRange) {
            for (ctx.tree.getExtra(@field(payload, field.name))) |child| {
                if ((try walkNode(C, V, visitor, child, ctx)) == .stop) return .stop;
            }
        }
    }

    return .proceed;
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

/// validates at compile time that all visitor hook names correspond to
/// actual `ast.NodeData` fields and have the correct payload types.
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
            @compileError("Invalid visitor hook '" ++ name ++ "': no field '" ++ node_name ++ "' exists in ast.NodeData");
        }

        const expected = @FieldType(ast.NodeData, node_name);
        const hook_fn_params = @typeInfo(@TypeOf(@field(V, name))).@"fn".params;

        if (hook_fn_params.len >= 3) {
            if (hook_fn_params[1].type) |actual| {
                if (actual != expected) {
                    @compileError("Visitor hook '" ++ name ++ "': expected payload type '" ++ @typeName(expected) ++ "', found '" ++ @typeName(actual) ++ "'");
                }
            }
        }
    }
}
