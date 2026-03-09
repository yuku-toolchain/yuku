const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");
const walk = wk.walk;

const Allocator = std.mem.Allocator;
const NodeTag = std.meta.Tag(ast.NodeData);

pub const ScopeId = enum(u32) { root = 0, none = std.math.maxInt(u32), _ };

/// A single scope in the scope tree.
pub const Scope = struct {
    /// The AST node that introduced this scope.
    node: ast.NodeIndex,
    /// The enclosing parent scope, or `.none` for the root.
    parent: ScopeId,
    /// Nearest ancestor (or self) where `var` declarations hoist to.
    hoist_target: ScopeId,
    /// What kind of syntactic construct created this scope.
    kind: Kind,
    /// Inherited and local flags (e.g. strict mode).
    flags: Flags,

    pub const Kind = enum(u8) {
        global,
        module,
        function_params,
        function_body,
        block,
        class,
        static_block,

        /// Returns whether `var` declarations hoist to this scope kind.
        pub fn isHoistTarget(kind: Kind) bool {
            return switch (kind) {
                .global, .module, .function_body, .static_block => true,
                else => false,
            };
        }
    };

    pub const Flags = struct {
        strict: bool = false,
    };
};

pub const Ctx = struct {
    tree: *const ast.ParseTree,
    allocator: Allocator,
    path: wk.NodePath = .{},
    scopes: std.ArrayList(Scope) = .{},
    scope_stack: std.ArrayList(ScopeId) = .{},

    // a function creates two scopes, one for its parameters
    // (`function_params`) and one for its body (`function_body`). The AST
    // represents the body as a `block_statement` child of the `function` node,
    // but a `block_statement` on its own could be a standalone block (`{ ... }`).
    //
    // this flag bridges that gap, when we enter a `function` node, we push the
    // params scope and set this to `true`. when we then hit the immediate
    // `block_statement` child, we check this flag to know it's the function body
    // (not a regular block), push `function_body` instead of `block`, and
    // consume the flag.
    pending_function_body: bool = false,

    pub fn init(tree: *const ast.ParseTree, allocator: Allocator) Allocator.Error!Ctx {
        var self = Ctx{ .tree = tree, .allocator = allocator };

        const estimated_scopes: u32 = @max(16, @as(u32, @intCast(tree.nodes.len / 16)));
        try self.scopes.ensureTotalCapacity(allocator, estimated_scopes);
        try self.scope_stack.ensureTotalCapacity(allocator, 64);

        try self.pushRoot();
        return self;
    }

    fn pushRoot(self: *Ctx) Allocator.Error!void {
        self.scopes.appendAssumeCapacity(.{
            .node = self.tree.program,
            .parent = .none,
            .hoist_target = .root,
            .kind = .global,
            .flags = .{},
        });

        self.scope_stack.appendAssumeCapacity(.root);

        if (self.tree.source_type == .module) {
            try self.pushScope(.module, self.tree.program, .{ .strict = true });
        }
    }

    /// Push a new scope onto the scope stack.
    pub fn pushScope(self: *Ctx, kind: Scope.Kind, node: ast.NodeIndex, flags: Scope.Flags) Allocator.Error!void {
        const id: ScopeId = @enumFromInt(@as(u32, @intCast(self.scopes.items.len)));
        const parent = self.currentScope();

        try self.scopes.append(self.allocator, .{
            .node = node,
            .parent = self.currentScopeId(),
            .hoist_target = if (kind.isHoistTarget()) id else parent.hoist_target,
            .kind = kind,
            .flags = flags,
        });

        try self.scope_stack.append(self.allocator, id);
    }

    /// Called by the walker when entering a node.
    pub fn onEnter(self: *Ctx, index: ast.NodeIndex, tag: NodeTag) Allocator.Error!void {
        const data = self.tree.getData(index);

        switch (data) {
            .directive => |d| {
                if (std.mem.eql(u8, self.tree.getSourceText(d.value_start, d.value_len), "use strict")) {
                    self.currentScopePtr().flags.strict = true;
                }
            },
            else => {},
        }

        const flags: Scope.Flags = .{
            .strict = self.currentScope().flags.strict,
        };

        switch (tag) {
            .function, .arrow_function_expression => {
                try self.pushScope(.function_params, index, flags);
                self.pending_function_body = true;
            },
            .block_statement => {
                if (self.pending_function_body) {
                    self.pending_function_body = false;
                    try self.pushScope(.function_body, index, flags);
                } else {
                    try self.pushScope(.block, index, flags);
                }
            },
            .for_statement, .for_in_statement, .for_of_statement,
            .catch_clause, .switch_statement,
            => try self.pushScope(.block, index, flags),
            .class => try self.pushScope(.class, index, flags),
            .static_block => try self.pushScope(.static_block, index, flags),
            else => {},
        }
    }

    /// Called by the walker when exiting a node.
    pub fn onExit(self: *Ctx, _: ast.NodeIndex, tag: NodeTag) void {
        switch (tag) {
            .function, .arrow_function_expression => {
                self.pending_function_body = false;
                _ = self.scope_stack.pop();
            },
            .block_statement,
            .for_statement, .for_in_statement, .for_of_statement,
            .catch_clause, .switch_statement,
            .class, .static_block,
            => _ = self.scope_stack.pop(),
            else => {},
        }
    }

    /// Returns the id of the currently active scope.
    pub inline fn currentScopeId(self: *const Ctx) ScopeId {
        return self.scope_stack.getLast();
    }

    /// Returns the id of the nearest hoist-target scope (where `var` lands).
    pub inline fn currentHoistScopeId(self: *const Ctx) ScopeId {
        return self.currentScope().hoist_target;
    }

    /// Returns the currently active scope.
    pub inline fn currentScope(self: *const Ctx) Scope {
        return self.getScope(self.currentScopeId());
    }

    /// Returns a mutable pointer to the currently active scope.
    pub inline fn currentScopePtr(self: *Ctx) *Scope {
        return &self.scopes.items[@intFromEnum(self.currentScopeId())];
    }

    /// Returns the scope for a given id.
    pub inline fn getScope(self: *const Ctx, id: ScopeId) Scope {
        return self.scopes.items[@intFromEnum(id)];
    }

    /// Returns a mutable pointer to the scope for a given id.
    pub inline fn getScopePtr(self: *Ctx, id: ScopeId) *Scope {
        return &self.scopes.items[@intFromEnum(id)];
    }

    /// Returns whether the current scope is in strict mode.
    pub inline fn isStrict(self: *const Ctx) bool {
        return self.currentScope().flags.strict;
    }

    /// Returns an iterator over ancestor scopes starting from `start`, walking up to root.
    pub fn ancestors(self: *const Ctx, start: ScopeId) ScopeTree.AncestorIterator {
        return .{ .scopes = self.scopes.items, .current = start };
    }

    /// Returns the completed scope tree and releases resources that are
    /// no longer needed. Call after traversal is done.
    pub fn toScopeTree(self: *Ctx) ScopeTree {
        self.scope_stack.deinit(self.allocator);
        return .{ .scopes = self.scopes.items, .allocator = self.allocator, .capacity = self.scopes.capacity };
    }

    /// Release all owned memory without producing a scope tree.
    pub fn deinit(self: *Ctx) void {
        self.scopes.deinit(self.allocator);
        self.scope_stack.deinit(self.allocator);
    }
};

/// The immutable result of a scoped traversal, a flat array of scopes
/// linked by parent pointers. Owns its backing memory.
pub const ScopeTree = struct {
    scopes: []const Scope,
    allocator: Allocator,
    capacity: usize,

    /// Free the backing memory.
    pub fn deinit(self: *ScopeTree) void {
        self.allocator.free(self.scopes.ptr[0..self.capacity]);
        self.* = undefined;
    }

    /// Returns the scope for a given id.
    pub inline fn getScope(self: ScopeTree, id: ScopeId) Scope {
        return self.scopes[@intFromEnum(id)];
    }

    /// Returns an iterator over ancestor scopes starting from `start`, walking up to root.
    pub fn ancestors(self: ScopeTree, start: ScopeId) AncestorIterator {
        return .{ .scopes = self.scopes, .current = start };
    }

    /// Iterator over ancestor scopes from a starting scope up to root.
    pub const AncestorIterator = struct {
        scopes: []const Scope,
        current: ScopeId,

        /// Returns the next ancestor scope, or null when the root has been passed.
        pub fn next(self: *AncestorIterator) ?ScopeId {
            const id = self.current;
            if (id == .none) return null;
            self.current = self.scopes[@intFromEnum(id)].parent;
            return id;
        }
    };
};

/// Run a scoped traversal over the parse tree.
///
/// Walks the AST while automatically tracking scope enter/exit. The visitor
/// receives a `*scoped.Ctx` as its context, giving access to scope
/// information at every node. Returns the completed `ScopeTree`.
pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V, allocator: Allocator) Allocator.Error!ScopeTree {
    var ctx = try Ctx.init(tree, allocator);
    errdefer ctx.deinit();
    try walk(Ctx, V, visitor, &ctx);
    return ctx.toScopeTree();
}
