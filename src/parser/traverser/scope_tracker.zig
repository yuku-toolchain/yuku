const std = @import("std");
const ast = @import("../ast.zig");

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

/// The immutable result of a scoped traversal, a flat array of scopes
/// linked by parent pointers. Owns its backing memory.
pub const ScopeTree = struct {
    scopes: []const Scope,
    allocator: Allocator,

    /// Free the backing memory.
    pub fn deinit(self: *ScopeTree) void {
        self.allocator.free(self.scopes);
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

/// Mutable builder that tracks JavaScript lexical scopes during an AST walk.
/// Embed this in any context that needs scope tracking.
pub const ScopeTracker = struct {
    tree: *const ast.ParseTree,
    allocator: Allocator,
    scopes: std.ArrayList(Scope) = .{},
    scope_stack: std.ArrayList(ScopeId) = .{},

    // A function creates two scopes: one for its parameters (`function_params`)
    // and one for its body (`function_body`). The AST represents the body as a
    // `block_statement` child of the `function` node, but a `block_statement`
    // on its own could be a standalone block (`{ ... }`).
    //
    // This flag bridges that gap: when we enter a `function` node, we push the
    // params scope and set this to `true`. When we then hit the immediate
    // `block_statement` child, we check this flag to know it's the function
    // body (not a regular block), push `function_body` instead of `block`,
    // and consume the flag.
    pending_function_body: bool = false,

    pub fn init(tree: *const ast.ParseTree, allocator: Allocator) Allocator.Error!ScopeTracker {
        var self = ScopeTracker{ .tree = tree, .allocator = allocator };

        const estimated_scopes: u32 = @max(16, @as(u32, @intCast(tree.nodes.len / 16)));
        try self.scopes.ensureTotalCapacity(allocator, estimated_scopes);
        try self.scope_stack.ensureTotalCapacity(allocator, 64);

        try self.pushRoot();
        return self;
    }

    fn pushRoot(self: *ScopeTracker) Allocator.Error!void {
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
    pub fn pushScope(self: *ScopeTracker, kind: Scope.Kind, node: ast.NodeIndex, flags: Scope.Flags) Allocator.Error!void {
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

    pub fn enter(self: *ScopeTracker, index: ast.NodeIndex, tag: NodeTag) Allocator.Error!void {
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

    pub fn exit(self: *ScopeTracker, tag: NodeTag) void {
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

    pub inline fn currentScopeId(self: *const ScopeTracker) ScopeId {
        return self.scope_stack.getLast();
    }

    pub inline fn currentHoistScopeId(self: *const ScopeTracker) ScopeId {
        return self.currentScope().hoist_target;
    }

    pub inline fn currentScope(self: *const ScopeTracker) Scope {
        return self.getScope(self.currentScopeId());
    }

    pub inline fn currentScopePtr(self: *ScopeTracker) *Scope {
        return &self.scopes.items[@intFromEnum(self.currentScopeId())];
    }

    pub inline fn getScope(self: *const ScopeTracker, id: ScopeId) Scope {
        return self.scopes.items[@intFromEnum(id)];
    }

    pub inline fn getScopePtr(self: *ScopeTracker, id: ScopeId) *Scope {
        return &self.scopes.items[@intFromEnum(id)];
    }

    pub inline fn isStrict(self: *const ScopeTracker) bool {
        return self.currentScope().flags.strict;
    }

    pub fn ancestors(self: *const ScopeTracker, start: ScopeId) ScopeTree.AncestorIterator {
        return .{ .scopes = self.scopes.items, .current = start };
    }

    pub fn toScopeTree(self: *ScopeTracker) Allocator.Error!ScopeTree {
        self.scope_stack.deinit(self.allocator);
        return .{ .scopes = try self.scopes.toOwnedSlice(self.allocator), .allocator = self.allocator };
    }

    pub fn deinit(self: *ScopeTracker) void {
        self.scopes.deinit(self.allocator);
        self.scope_stack.deinit(self.allocator);
    }
};
