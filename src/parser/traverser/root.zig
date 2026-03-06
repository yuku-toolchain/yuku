const std = @import("std");
const ast = @import("../ast.zig");

const ENTER_CATCH_ALL = "enter_node";
const EXIT_CATCH_ALL = "exit_node";
const ENTER_PREFIX = "enter_";
const EXIT_PREFIX = "exit_";

// ── Core ──

pub const Action = enum {
    /// Continue into children (default)
    proceed,
    /// Skip this node's children, continue to next sibling
    skip,
    /// Stop the entire traversal immediately
    stop,
};

pub const NodeTag = std.meta.Tag(ast.NodeData);

/// Walk the AST, calling visitor hooks at each node.
///
/// This is the generic foundation. `C` is any context type that must have
/// a `.tree` field. It may optionally declare `onEnter` and `onExit` hooks
/// that are called when entering/exiting each node — resolved at comptime,
/// zero overhead when absent.
///
/// Built-in contexts: `basic.Ctx` (parent tracking) and `scoped.ScopedCtx` (scopes).
/// Users can create their own — just a struct with `.tree` and optional hooks.
pub fn walk(comptime C: type, comptime V: type, visitor: *V, ctx: *C) void {
    comptime validateHooks(V);
    _ = walkNode(C, V, visitor, ctx.tree.program, ctx);
}

// ── Utilities ──

/// Reusable parent stack. Contexts that need parent/ancestor tracking
/// can embed this. Not required — it's opt-in.
pub const ParentStack = struct {
    pub const MAX_DEPTH = 512;

    buf: [MAX_DEPTH]ast.NodeIndex = undefined,
    len: u32 = 0,

    pub inline fn push(self: *ParentStack, node: ast.NodeIndex) void {
        std.debug.assert(self.len < MAX_DEPTH);
        self.buf[self.len] = node;
        self.len += 1;
    }

    pub inline fn pop(self: *ParentStack) void {
        std.debug.assert(self.len > 0);
        self.len -= 1;
    }

    /// Current parent. null_node if empty.
    pub inline fn current(self: *const ParentStack) ast.NodeIndex {
        if (self.len == 0) return ast.null_node;
        return self.buf[self.len - 1];
    }

    /// Get ancestor by depth: 0 = parent, 1 = grandparent, ...
    pub inline fn get(self: *const ParentStack, depth_offset: usize) ast.NodeIndex {
        if (depth_offset >= self.len) return ast.null_node;
        return self.buf[self.len - 1 - @as(u32, @intCast(depth_offset))];
    }
};

// ── Walk engine ──

fn walkNode(comptime C: type, comptime V: type, visitor: *V, index: ast.NodeIndex, ctx: *C) Action {
    if (ast.isNull(index)) return .proceed;

    const data = ctx.tree.getData(index);
    const tag = std.meta.activeTag(data);

    // 1. Enter hooks
    if (comptime hasAnyEnter(V)) {
        switch (callEnter(C, V, visitor, data, ctx)) {
            .skip => return .proceed,
            .stop => return .stop,
            .proceed => {},
        }
    }

    // 2. Context enter
    if (comptime @hasDecl(C, "onEnter")) ctx.onEnter(index, tag);

    // 3. Walk children
    const result = walkChildren(C, V, visitor, data, ctx);

    // 4. Exit hooks (context state still active — scope/parents not yet popped)
    if (comptime hasAnyExit(V)) {
        if (result != .stop) {
            callExit(C, V, visitor, data, ctx);
        }
    }

    // 5. Context exit
    if (comptime @hasDecl(C, "onExit")) ctx.onExit(index, tag);

    return result;
}

// ── Enter dispatch ──

fn callEnter(comptime C: type, comptime V: type, visitor: *V, data: ast.NodeData, ctx: *C) Action {
    if (comptime @hasDecl(V, ENTER_CATCH_ALL)) {
        switch (visitor.enter_node(data, ctx)) {
            .skip => return .skip,
            .stop => return .stop,
            .proceed => {},
        }
    }
    return callEnterTyped(C, V, visitor, data, ctx);
}

fn callEnterTyped(comptime C: type, comptime V: type, visitor: *V, data: ast.NodeData, ctx: *C) Action {
    switch (data) {
        inline else => |node, tag| {
            const enter_name = comptime enterNameFor(tag);
            if (comptime @hasDecl(V, enter_name)) {
                return @field(V, enter_name)(visitor, node, ctx);
            }
            return .proceed;
        },
    }
}

// ── Exit dispatch ──

fn callExit(comptime C: type, comptime V: type, visitor: *V, data: ast.NodeData, ctx: *C) void {
    callExitTyped(C, V, visitor, data, ctx);

    if (comptime @hasDecl(V, EXIT_CATCH_ALL)) {
        visitor.exit_node(data, ctx);
    }
}

fn callExitTyped(comptime C: type, comptime V: type, visitor: *V, data: ast.NodeData, ctx: *C) void {
    switch (data) {
        inline else => |node, tag| {
            const exit_name = comptime exitNameFor(tag);
            if (comptime @hasDecl(V, exit_name)) {
                @field(V, exit_name)(visitor, node, ctx);
            }
        },
    }
}

// ── Child walking ──

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

// ── Comptime helpers ──

fn enterNameFor(comptime tag: NodeTag) []const u8 {
    return ENTER_PREFIX ++ @tagName(tag);
}

fn exitNameFor(comptime tag: NodeTag) []const u8 {
    return EXIT_PREFIX ++ @tagName(tag);
}

fn hasAnyEnter(comptime V: type) bool {
    if (@hasDecl(V, ENTER_CATCH_ALL)) return true;
    for (@typeInfo(ast.NodeData).@"union".fields) |f| {
        if (@hasDecl(V, ENTER_PREFIX ++ f.name)) return true;
    }
    return false;
}

fn hasAnyExit(comptime V: type) bool {
    if (@hasDecl(V, EXIT_CATCH_ALL)) return true;
    for (@typeInfo(ast.NodeData).@"union".fields) |f| {
        if (@hasDecl(V, EXIT_PREFIX ++ f.name)) return true;
    }
    return false;
}

fn validateHooks(comptime V: type) void {
    for (@typeInfo(V).@"struct".decls) |decl| {
        const name = decl.name;

        if (comptime std.mem.eql(u8, name, ENTER_CATCH_ALL) or std.mem.eql(u8, name, EXIT_CATCH_ALL))
            continue;

        const node_name = if (std.mem.startsWith(u8, name, ENTER_PREFIX))
            name[ENTER_PREFIX.len..]
        else if (std.mem.startsWith(u8, name, EXIT_PREFIX))
            name[EXIT_PREFIX.len..]
        else
            continue;

        if (!@hasField(ast.NodeData, node_name)) {
            @compileError("Invalid visitor hook '" ++ name ++ "': no field '" ++ node_name ++ "' exists in ast.NodeData");
        }

        const expected = @FieldType(ast.NodeData, node_name);

        const hook_fn_params = @typeInfo(@TypeOf(@field(V, name))).@"fn".params;

        if (hook_fn_params.len >= 2) {
            if (hook_fn_params[1].type) |actual| {
                if (actual != expected) {
                    @compileError("Visitor hook '" ++ name ++ "': expected payload type '" ++ @typeName(expected) ++ "', found '" ++ @typeName(actual) ++ "'");
                }
            }
        }
    }
}
