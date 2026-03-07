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
        module,
        function,
        block,
        @"for",
        @"catch",
        class,
        static_block,
        @"switch",
    };

    pub const Flags = packed struct(u8) {
        strict: bool = false,
        arrow: bool = false,
        @"async": bool = false,
        generator: bool = false,
        _pad: u4 = 0,
    };
};

pub const ScopedCtx = struct {
    tree: *const ast.ParseTree,
    allocator: Allocator,

    // scope internals
    scopes: std.ArrayList(Scope) = .{},
    scope_stack: std.ArrayList(ScopeId) = .{},

    pub fn init(tree: *const ast.ParseTree, allocator: Allocator) ScopedCtx {
        var self = ScopedCtx{ .tree = tree, .allocator = allocator };

        const node_count = tree.nodes.len;

        self.scopes.ensureTotalCapacity(allocator, @max(16, @as(u32, @intCast(node_count / 16)))) catch {};
        self.scope_stack.ensureTotalCapacity(allocator, 64) catch {};

        // root module scope
        self.scopes.appendAssumeCapacity(.{
            .node = tree.program,
            .parent = .none,
            .kind = .module,
            .flags = .{
                .strict = tree.source_type == .module
            },
        });

        self.scope_stack.appendAssumeCapacity(.root);

        return self;
    }

    // called by walk
    pub fn onEnter(self: *ScopedCtx, index: ast.NodeIndex, tag: NodeTag) void {
        const data = self.tree.getData(index);

        switch (data) {
            .directive => |d| {
                if(std.mem.eql(u8, self.tree.getSourceText(d.value_start, d.value_len), "use strict")) {
                    self.getScopePtr(self.currentScope()).flags.strict = true;
                }
            },
            else => {},
        }

        const scope_kind = scopeKindOf(tag);

        if(scope_kind) |kind| {
            const current_scope = self.currentScope();

            const is_outer_strict = self.getScope(current_scope).flags.strict;

            var flags: Scope.Flags = .{
                .strict = is_outer_strict
            };

            switch (data) {
                .arrow_function_expression => |f| {
                    flags.arrow = true;
                    flags.@"async" = f.@"async";
                },
                .function => |f| {
                    flags.@"async" = f.@"async";
                    flags.generator = f.generator;
                },
                else => {},
            }

            const id: ScopeId = @enumFromInt(@as(u32, @intCast(self.scopes.items.len)));

            self.scopes.append(self.allocator, .{
                .node = index,
                .parent = self.currentScope(),
                .kind = kind,
                .flags = flags,
            }) catch unreachable;

            self.scope_stack.append(self.allocator, id) catch unreachable;
        }
    }

    pub fn onExit(self: *ScopedCtx, _: ast.NodeIndex, tag: NodeTag) void {
        if (scopeKindOf(tag) != null) {
            _ = self.scope_stack.pop().?;
        }
    }

    /// The currently active scope.
    pub inline fn currentScope(self: *const ScopedCtx) ScopeId {
        return self.scope_stack.getLast();
    }

    /// Get the Scope data for a given ScopeId.
    pub inline fn getScope(self: *const ScopedCtx, id: ScopeId) Scope {
        return self.scopes.items[@intFromEnum(id)];
    }

    pub inline fn getScopePtr(self: *const ScopedCtx, id: ScopeId) *Scope {
        return &self.scopes.items[@intFromEnum(id)];
    }

    pub inline fn currentKind(self: *const ScopedCtx) Scope.Kind {
        return self.getScope(self.currentScope()).kind;
    }

    pub inline fn isStrict(self: *const ScopedCtx) bool {
        return self.getScope(self.currentScope()).flags.strict;
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

const scope_kinds: [std.meta.fields(ast.NodeData).len]?Scope.Kind = blk: {
    var table: [std.meta.fields(ast.NodeData).len]?Scope.Kind = @splat(null);
    table[@intFromEnum(NodeTag.function)] = .function;
    table[@intFromEnum(NodeTag.arrow_function_expression)] = .function;
    table[@intFromEnum(NodeTag.block_statement)] = .block;
    table[@intFromEnum(NodeTag.for_statement)] = .@"for";
    table[@intFromEnum(NodeTag.for_in_statement)] = .@"for";
    table[@intFromEnum(NodeTag.for_of_statement)] = .@"for";
    table[@intFromEnum(NodeTag.catch_clause)] = .@"catch";
    table[@intFromEnum(NodeTag.class)] = .class;
    table[@intFromEnum(NodeTag.static_block)] = .static_block;
    table[@intFromEnum(NodeTag.switch_statement)] = .@"switch";
    break :blk table;
};

/// returns the scope kind for a node tag, or null if it doesn't create a scope.
pub inline fn scopeKindOf(tag: NodeTag) ?Scope.Kind {
    return scope_kinds[@intFromEnum(tag)];
}

/// Scoped traversal. Automatically tracks scope enter/exit.
/// Returns the complete ScopeTree after traversal.
pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V, allocator: Allocator) void {
    var ctx = ScopedCtx.init(tree, allocator);
    walk(ScopedCtx, V, visitor, &ctx);
}
