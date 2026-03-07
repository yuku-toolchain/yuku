const std = @import("std");
const ast = @import("../ast.zig");
const root = @import("root.zig");

const NodeTag = std.meta.Tag(ast.NodeData);

pub const Action = enum {
    /// Continue into children (default)
    proceed,
    /// Skip this node's children, continue to next sibling
    skip,
    /// Stop the entire traversal immediately
    stop,
};

/// Walk the AST, calling visitor hooks at each node.
///
/// This is the generic foundation. `C` is any context type that must have
/// a `.tree = ast.ParseTree` field. It may optionally declare `onEnter` and `onExit` hooks
/// that are called when entering/exiting each node.
pub fn walk(comptime C: type, comptime V: type, visitor: *V, ctx: *C) void {
    comptime validateHooks(V);
    _ = walkNode(C, V, visitor, ctx.tree.program, ctx);
}

fn walkNode(comptime C: type, comptime V: type, visitor: *V, index: ast.NodeIndex, ctx: *C) Action {
    if (ast.isNull(index)) return .proceed;

    const data = ctx.tree.getData(index);
    const tag = std.meta.activeTag(data);

    // enter hooks
    if (comptime hasAnyEnter(V)) {
        switch (callEnter(C, V, visitor, data, index, ctx)) {
            .skip => return .proceed,
            .stop => return .stop,
            .proceed => {},
        }
    }

    // context enter
    if (comptime @hasDecl(C, "onEnter")) ctx.onEnter(index, tag);

    const result = walkChildren(C, V, visitor, data, ctx);

    if (comptime hasAnyExit(V)) {
        if (result != .stop) {
            callExit(C, V, visitor, data, index, ctx);
        }
    }

    // context exit
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
            const enter_name = comptime enterNameFor(tag);
            if (comptime @hasDecl(V, enter_name)) {
                return @field(V, enter_name)(visitor, node, index, ctx);
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
            const exit_name = comptime exitNameFor(tag);
            if (comptime @hasDecl(V, exit_name)) {
                @field(V, exit_name)(visitor, node, index, ctx);
            }
        },
    }
}

fn walkChildren(comptime C: type, comptime V: type, visitor: *V, data: ast.NodeData, ctx: *C) Action {
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

fn walkStructFields(comptime C: type, comptime V: type, visitor: *V, comptime T: type, payload: T, ctx: *C) Action {
    const fields = @typeInfo(T).@"struct".fields;

    inline for (fields) |field| {
        if (field.type == ast.NodeIndex) {
            if (walkNode(C, V, visitor, @field(payload, field.name), ctx) == .stop) return .stop;
        } else if (field.type == ast.IndexRange) {
            const range = @field(payload, field.name);
            const children = ctx.tree.getExtra(range);
            for (children) |child| {
                if (walkNode(C, V, visitor, child, ctx) == .stop) return .stop;
            }
        }
    }

    return .proceed;
}

fn enterNameFor(comptime tag: NodeTag) []const u8 {
    return "enter_" ++ @tagName(tag);
}

fn exitNameFor(comptime tag: NodeTag) []const u8 {
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
