const std = @import("std");
const ast = @import("../ast.zig");
const walk = @import("walk.zig").walk;

const Allocator = std.mem.Allocator;
const NodeTag = std.meta.Tag(ast.NodeData);

pub const ScopeId = enum(u32) { root = 0, none = std.math.maxInt(u32), _ };

pub const Scope = struct {
    node: ast.NodeIndex,
    parent: ScopeId,
    kind: Kind,
    flags: Flags,

    pub const Kind = enum(u8) {
        global,
        module,
        function_params,
        function_body,
        block,
        class,
        static_block,

        /// Will `var` land here?
        fn isFunctionLike(kind: Kind) bool {
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

pub const ScopedCtx = struct {
    tree: *const ast.ParseTree,
    allocator: Allocator,

    // scope internals
    scopes: std.ArrayList(Scope) = .{},
    scope_stack: std.ArrayList(ScopeId) = .{},

    pending_function_body: bool = false,

    pub fn init(tree: *const ast.ParseTree, allocator: Allocator) ScopedCtx {
        var self = ScopedCtx{ .tree = tree, .allocator = allocator };

        const node_count = tree.nodes.len;

        self.scopes.ensureTotalCapacity(allocator, @max(16, @as(u32, @intCast(node_count / 16)))) catch {};
        self.scope_stack.ensureTotalCapacity(allocator, 64) catch {};

        self.pushRoot();

        return self;
    }

    fn pushRoot(self: *ScopedCtx) void {
        self.scopes.appendAssumeCapacity(.{
            .node = self.tree.program,
            .parent = .none,
            .kind = .global,
            .flags = .{},
        });

        self.scope_stack.appendAssumeCapacity(.root);

        if(self.tree.source_type == .module) {
            self.pushScope(.module, self.tree.program, .{ .strict = true });
        }
    }

    pub fn pushScope(self: *ScopedCtx, kind: Scope.Kind, node: ast.NodeIndex, flags: Scope.Flags) void {
        const id: ScopeId = @enumFromInt(@as(u32, @intCast(self.scopes.items.len)));

        self.scopes.append(self.allocator, .{
            .node = node,
            .parent = self.currentScope(),
            .kind = kind,
            .flags = flags,
        }) catch unreachable;

        self.scope_stack.append(self.allocator, id) catch unreachable;
    }

    // called by walk
    pub fn onEnter(self: *ScopedCtx, index: ast.NodeIndex, tag: NodeTag) void {
        const data = self.tree.getData(index);

        switch (data) {
            .directive => |d| {
                if(std.mem.eql(u8, self.tree.getSourceText(d.value_start, d.value_len), "use strict")) {
                    self.getCurrentScopePtr().flags.strict = true;
                }
            },
            else => {},
        }

        const is_outer_strict = self.getCurrentScope().flags.strict;

        const flags: Scope.Flags = .{
            .strict = is_outer_strict
        };

        switch (tag) {
            .function, .arrow_function_expression => {
                self.pushScope(.function_params, index, flags);
                self.pending_function_body = true;
            },
            .block_statement => {
                if (self.pending_function_body) {
                    self.pending_function_body = false;
                    self.pushScope(.function_body, index, flags);
                } else {
                    self.pushScope(.block, index, flags);
                }
            },
            .for_statement, .for_in_statement, .for_of_statement,
            .catch_clause, .switch_statement => self.pushScope(.block, index, flags),
            .class => self.pushScope(.class, index, flags),
            .static_block => self.pushScope(.static_block, index, flags),
            else => {},
        }
    }

    pub fn onExit(self: *ScopedCtx, _: ast.NodeIndex, tag: NodeTag) void {
        switch (tag) {
            .function, .arrow_function_expression => {
                self.pending_function_body = false;
                _ = self.scope_stack.pop();
            },
            .block_statement,
            .for_statement, .for_in_statement, .for_of_statement,
            .catch_clause, .switch_statement,
            .class, .static_block => _ = self.scope_stack.pop(),
            else => {},
        }
    }

    /// The currently active scope.
    pub inline fn currentScope(self: *const ScopedCtx) ScopeId {
        return self.scope_stack.getLast();
    }

    pub inline fn getCurrentScope(self: *const ScopedCtx) Scope {
        return self.getScope(self.currentScope());
    }

    pub inline fn getCurrentScopePtr(self: *const ScopedCtx) *Scope {
        return &self.scopes.items[@intFromEnum(self.currentScope())];
    }

    /// Get the Scope data for a given ScopeId.
    pub inline fn getScope(self: *const ScopedCtx, id: ScopeId) Scope {
        return self.scopes.items[@intFromEnum(id)];
    }

    pub inline fn getScopePtr(self: *const ScopedCtx, id: ScopeId) *Scope {
        return &self.scopes.items[@intFromEnum(id)];
    }

    pub inline fn currentScopeKind(self: *const ScopedCtx) Scope.Kind {
        return self.getCurrentScope().kind;
    }

    pub inline fn isStrict(self: *const ScopedCtx) bool {
        return self.getCurrentScope().flags.strict;
    }

    pub fn iterAncestors(self: *const ScopedCtx, start: ScopeId) AncestorIterator {
        return .{ .ctx = self, .current = start };
    }

    pub const AncestorIterator = struct {
        ctx: *const ScopedCtx,
        current: ScopeId,

        pub fn next(self: *AncestorIterator) ?ScopeId {
            const id = self.current;
            if (id == .none) return null;
            self.current = self.ctx.getScope(id).parent;
            return id;
        }
    };
};

/// Scoped traversal. Automatically tracks scope enter/exit.
/// Returns the complete ScopeTree after traversal.
pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V, allocator: Allocator) void {
    var ctx = ScopedCtx.init(tree, allocator);
    walk(ScopedCtx, V, visitor, &ctx);
}
