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

/// Walks the AST tree, calling visitor hooks at each node.
///
/// `C` is the context type. It must have a `.tree` field so the walker
/// can access child nodes. Contexts can also define `enter`, `exit`,
/// and `post_enter` methods if used with `Layer`.
///
/// `V` is the visitor type. It can define hooks like `enter_function`,
/// `exit_block_statement`, or the catch-all `enter_node`/`exit_node`.
pub fn walk(comptime C: type, comptime V: type, visitor: *V, ctx: *C) Allocator.Error!void {
    comptime validateHooks(V);
    _ = try walkNode(C, V, visitor, ctx.tree.program, ctx);
}

fn walkNode(comptime C: type, comptime V: type, visitor: *V, index: ast.NodeIndex, ctx: *C) Allocator.Error!Action {
    if (ast.isNull(index)) return .proceed;

    const data = ctx.tree.getData(index);

    switch (try dispatch.enter(C, V, visitor, data, index, ctx)) {
        .skip => return .proceed,
        .stop => return .stop,
        .proceed => {},
    }

    const result = try walkChildren(C, V, visitor, data, ctx);

    if (result != .stop) {
        dispatch.exit(C, V, visitor, data, index, ctx);
    }

    return result;
}

// walks all child nodes by iterating over the struct fields of the
// node payload. NodeIndex fields are single children, IndexRange
// fields are lists of children stored in the extra array.
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

/// Generic layer that wires a context's tracking methods into the walk cycle.
///
/// Instead of each traverser writing its own wrapper visitor, they define
/// tracking logic as methods on their `Ctx` type, and this layer handles
/// the rest. The protocol is:
///
///   1. `ctx.enter(index, data)`      - runs before user hooks (push scopes, etc.)
///   2. dispatch to inner visitor      - user's enter_node and typed hooks fire here
///   3. `ctx.post_enter(index, data)` - runs after user hooks (optional, for
///                                      work that needs to happen after the user
///                                      saw the pre-existing state, like declaring
///                                      new symbols)
///   ... walk children ...
///   4. dispatch to inner visitor      - user's exit hooks fire here
///   5. `ctx.exit(data)`              - cleanup (pop scopes, etc.)
///
/// `C` must have `enter` and `exit`. `post_enter` is optional.
pub fn Layer(comptime C: type, comptime V: type) type {
    return struct {
        inner: *V,

        pub fn enter_node(self: *@This(), data: ast.NodeData, index: ast.NodeIndex, ctx: *C) Allocator.Error!Action {
            try ctx.enter(index, data);
            const action = try dispatch.enter(C, V, self.inner, data, index, ctx);
            if (comptime @hasDecl(C, "post_enter")) try ctx.post_enter(index, data);
            return action;
        }

        pub fn exit_node(self: *@This(), data: ast.NodeData, index: ast.NodeIndex, ctx: *C) void {
            dispatch.exit(C, V, self.inner, data, index, ctx);
            ctx.exit(data);
        }
    };
}

/// Dispatch helpers for calling visitor hooks.
///
/// `enter` calls `enter_node` first, then the typed hook (e.g. `enter_function`).
/// `exit` calls the typed hook first, then `exit_node`.
///
/// These are public so custom layers can use them to forward hooks
/// to an inner visitor. For example, `Layer` uses these to dispatch
/// to the user's visitor after doing its tracking work.
pub const dispatch = struct {
    /// Dispatches the enter phase: calls `enter_node` first, then the typed hook.
    /// Returns the action from whichever hook fires.
    pub fn enter(comptime C: type, comptime V: type, visitor: *V, data: ast.NodeData, index: ast.NodeIndex, ctx: *C) Allocator.Error!Action {
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
    pub fn enterTyped(comptime C: type, comptime V: type, visitor: *V, data: ast.NodeData, index: ast.NodeIndex, ctx: *C) Allocator.Error!Action {
        switch (data) {
            inline else => |node, tag| {
                if (comptime @hasDecl(V, "enter_" ++ @tagName(tag))) {
                    return unwrapAction(@field(V, "enter_" ++ @tagName(tag))(visitor, node, index, ctx));
                }
                return .proceed;
            },
        }
    }

    /// Dispatches the exit phase: calls the typed hook first, then `exit_node`.
    pub fn exit(comptime C: type, comptime V: type, visitor: *V, data: ast.NodeData, index: ast.NodeIndex, ctx: *C) void {
        exitTyped(C, V, visitor, data, index, ctx);
        if (comptime @hasDecl(V, "exit_node")) {
            visitor.exit_node(data, index, ctx);
        }
    }

    /// Dispatches only the typed exit hook (e.g. `exit_function`), skipping `exit_node`.
    pub fn exitTyped(comptime C: type, comptime V: type, visitor: *V, data: ast.NodeData, index: ast.NodeIndex, ctx: *C) void {
        switch (data) {
            inline else => |node, tag| {
                if (comptime @hasDecl(V, "exit_" ++ @tagName(tag))) {
                    @field(V, "exit_" ++ @tagName(tag))(visitor, node, index, ctx);
                }
            },
        }
    }
};

// lets visitor hooks return either `Action` or `Allocator.Error!Action`.
// both coerce to `Allocator.Error!Action` through zig's error union rules,
// so hooks that don't need to allocate can just return `.proceed` directly
// without wrapping it in an error union.
inline fn unwrapAction(result: anytype) Allocator.Error!Action {
    return result;
}

// checks at compile time that all visitor hook names (enter_X, exit_X)
// match actual node types in ast.NodeData and have correct payload types.
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

/// Tracks the path of node indices from root to the current position.
pub const NodePath = struct {
    const capacity = 256;

    buf: [capacity]ast.NodeIndex = undefined,
    len: usize = 0,

    /// Returns the parent node index, or `null` if at the root.
    pub inline fn parent(self: *const NodePath) ?ast.NodeIndex {
        return self.ancestor(1);
    }

    /// Returns the nth ancestor. 0 = current node, 1 = parent, 2 = grandparent, etc.
    pub inline fn ancestor(self: *const NodePath, n: usize) ?ast.NodeIndex {
        if (n >= self.len) return null;
        const pos = self.len - 1 - n;
        return if (pos < capacity) self.buf[pos] else null;
    }

    /// Returns the current nesting depth (0 at root).
    pub inline fn depth(self: *const NodePath) usize {
        return self.len;
    }

    /// Adds a node to the path when entering it.
    pub fn push(self: *NodePath, index: ast.NodeIndex) void {
        std.debug.assert(self.len < capacity);
        if (self.len < capacity) {
            self.buf[self.len] = index;
        }
        self.len += 1;
    }

    /// Removes the current node from the path when exiting it.
    pub fn pop(self: *NodePath) void {
        self.len -= 1;
    }
};
