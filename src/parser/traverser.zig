const std = @import("std");
const ast = @import("ast.zig");

const ENTER_CATCH_ALL = "enter_node";
const EXIT_CATCH_ALL = "exit_node";
const ENTER_PREFIX = "enter_";
const EXIT_PREFIX = "exit_";

// ── Public types ──

pub const Action = enum {
    /// Continue into children (default)
    proceed,
    /// Skip this node's children, continue to next sibling
    skip,
    /// Stop the entire traversal immediately
    stop,
};

pub const NodeTag = std.meta.Tag(ast.NodeData);

/// Basic traversal context with parent stack only.
pub const Ctx = struct {
    tree: *const ast.ParseTree,
    parents: ParentStack = .{},

    /// The immediate parent node (null_node at root)
    pub inline fn parent(self: *const Ctx) ast.NodeIndex {
        return self.parents.current();
    }

    /// Nth ancestor: 0 = parent, 1 = grandparent, ...
    pub inline fn ancestor(self: *const Ctx, depth_offset: usize) ast.NodeIndex {
        return self.parents.get(depth_offset);
    }

    pub inline fn parentData(self: *const Ctx) ?ast.NodeData {
        const p = self.parent();
        if (ast.isNull(p)) return null;
        return self.tree.getData(p);
    }

    /// Walk up ancestors until predicate matches. Returns the matching node index.
    pub fn findAncestor(self: *const Ctx, comptime predicate: fn (ast.NodeData) bool) ?ast.NodeIndex {
        var i: usize = 0;
        while (true) {
            const a = self.parents.get(i);
            if (ast.isNull(a)) return null;
            if (predicate(self.tree.getData(a))) return a;
            i += 1;
        }
    }

    pub inline fn nodeText(self: *const Ctx, start: u32, len: u16) []const u8 {
        return self.tree.source[start..][0..len];
    }
};

// ── Entry points ──

/// Simple traversal with basic Ctx (parent stack only). Zero overhead.
pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V) void {
    var ctx = Ctx{ .tree = tree };
    walk(Ctx, V, visitor, &ctx);
}

/// Generic traversal with any context type.
///
/// `C` must have fields `.tree` and `.parents`.
/// `C` may optionally declare `onPush(index, tag)` and `onPop(index, tag)`,
/// which are called when entering/exiting each node. These are resolved at
/// comptime — when absent, zero overhead.
pub fn walk(comptime C: type, comptime V: type, visitor: *V, ctx: *C) void {
    comptime validateHooks(V);
    _ = walkNode(C, V, visitor, ctx.tree.program, ctx);
}

// ── Walk engine ──

fn walkNode(comptime C: type, comptime V: type, visitor: *V, index: ast.NodeIndex, ctx: *C) Action {
    if (ast.isNull(index)) return .proceed;

    const data = ctx.tree.getData(index);
    const tag = std.meta.activeTag(data);

    // 1. Enter hooks (visitor sees OUTER scope — correct for declaring names in parent scope)
    if (comptime hasAnyEnter(V)) {
        switch (callEnter(C, V, visitor, data, ctx)) {
            .skip => return .proceed,
            .stop => return .stop,
            .proceed => {},
        }
    }

    // 2. Push parent + notify context extension
    ctx.parents.push(index);

    if (comptime @hasDecl(C, "onPush")) ctx.onPush(index, tag);

    // 3. Walk children
    const result = walkChildren(C, V, visitor, data, ctx);

    // 4. Exit hooks (BEFORE onPop — visitor still sees inner scope)
    if (comptime hasAnyExit(V)) {
        if (result != .stop) {
            callExit(C, V, visitor, data, ctx);
        }
    }

    // 5. Pop context extension + parent
    if (comptime @hasDecl(C, "onPop")) ctx.onPop(index, tag);

    ctx.parents.pop();

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
// Any field of type NodeIndex gets walked. Any field of type IndexRange
// gets iterated and each element walked. Everything else is skipped.

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

// ── Parent stack ──

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
