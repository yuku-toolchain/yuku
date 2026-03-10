const std = @import("std");
const ast = @import("../../ast.zig");

const Allocator = std.mem.Allocator;

pub const ScopeId = enum(u32) { root = 0, none = std.math.maxInt(u32), _ };

pub const Scope = struct {
    node: ast.NodeIndex,
    parent: ScopeId,
    // nearest ancestor (or self) where `var` declarations hoist to.
    hoist_target: ScopeId,
    kind: Kind,
    flags: Flags,

    pub const Kind = enum(u8) {
        global,
        module,
        function,
        block,
        class,
        static_block,
        // intermediate scope for named function/class expressions.
        //
        // in js, `const x = function foo() { const foo = 1; }` is valid
        // because the expression name `foo` lives in a separate scope
        // between the outer scope and the function body. same for
        // `const x = class C { }`. without this, the name would
        // conflict with same-named bindings inside the body.
        //
        // scope structure for `const x = function foo() { ... }`:
        //   outer scope (x lives here)
        //     expression_name scope (foo lives here)
        //       function scope (body bindings live here)
        expression_name,

        pub fn isHoistTarget(kind: Kind) bool {
            return switch (kind) {
                .global, .module, .function, .static_block => true,
                else => false,
            };
        }
    };

    pub const Flags = struct {
        strict: bool = false,
    };
};

// immutable result of a scoped traversal. flat array of scopes
// linked by parent pointers. owns its backing memory.
pub const ScopeTree = struct {
    scopes: []const Scope,
    allocator: Allocator,

    pub fn deinit(self: *ScopeTree) void {
        self.allocator.free(self.scopes);
        self.* = undefined;
    }

    pub inline fn getScope(self: ScopeTree, id: ScopeId) Scope {
        return self.scopes[@intFromEnum(id)];
    }

    pub fn ancestors(self: ScopeTree, start: ScopeId) AncestorIterator {
        return .{ .scopes = self.scopes, .current = start };
    }

    // walks up from `start` to root, yielding each scope id.
    pub const AncestorIterator = struct {
        scopes: []const Scope,
        current: ScopeId,

        pub fn next(self: *AncestorIterator) ?ScopeId {
            const id = self.current;
            if (id == .none) return null;
            self.current = self.scopes[@intFromEnum(id)].parent;
            return id;
        }
    };
};

// mutable builder that tracks javascript lexical scopes during an ast walk.
//
// call `enter` when visiting a node and `exit` when leaving it.
// the tracker pushes/pops scopes for nodes that create them
// (functions, blocks, classes, etc) and ignores everything else.
pub const ScopeTracker = struct {
    tree: *const ast.ParseTree,
    allocator: Allocator,
    scopes: std.ArrayList(Scope) = .{},
    scope_stack: std.ArrayList(ScopeId) = .{},

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

    pub fn enter(self: *ScopeTracker, index: ast.NodeIndex, data: ast.NodeData) Allocator.Error!void {
        // check for "use strict" directives and mark the current scope
        switch (data) {
            .directive => |d| {
                if (std.mem.eql(u8, self.tree.getSourceText(d.value_start, d.value_len), "use strict")) {
                    self.currentScopePtr().flags.strict = true;
                }
            },
            else => {},
        }

        // inherit strict mode from parent scope
        const flags: Scope.Flags = .{
            .strict = self.currentScope().flags.strict,
        };

        switch (data) {
            .function => |func| {
                // named function expressions get an extra scope for their name.
                // we push it before the function scope so it sits between
                // outer and body. see Scope.Kind.expression_name for details.
                if (isNamedFunctionExpression(func))
                    try self.pushScope(.expression_name, index, flags);
                try self.pushScope(.function, index, flags);
            },
            .arrow_function_expression => try self.pushScope(.function, index, flags),
            .block_statement => try self.pushScope(.block, index, flags),
            .for_statement, .for_in_statement, .for_of_statement,
            .catch_clause, .switch_statement,
            => try self.pushScope(.block, index, flags),
            .class => |cls| {
                // same as named function expressions, named class expressions
                // get an extra scope for their name.
                if (isNamedClassExpression(cls))
                    try self.pushScope(.expression_name, index, flags);
                try self.pushScope(.class, index, flags);
            },
            .static_block => try self.pushScope(.static_block, index, flags),
            else => {},
        }
    }

    // mirrors enter: pops the same number of scopes that were pushed.
    // for named function/class expressions, that's two pops (body + name).
    pub fn exit(self: *ScopeTracker, data: ast.NodeData) void {
        switch (data) {
            .function => |func| {
                _ = self.scope_stack.pop();
                if (isNamedFunctionExpression(func))
                    _ = self.scope_stack.pop();
            },
            .arrow_function_expression,
            .block_statement,
            .for_statement, .for_in_statement, .for_of_statement,
            .catch_clause, .switch_statement,
            .static_block,
            => _ = self.scope_stack.pop(),
            .class => |cls| {
                _ = self.scope_stack.pop();
                if (isNamedClassExpression(cls))
                    _ = self.scope_stack.pop();
            },
            else => {},
        }
    }

    // declarations bind their name in the parent scope directly,
    // expressions bind in an intermediate expression_name scope.
    // these check if we're dealing with a named expression.

    fn isNamedFunctionExpression(func: ast.Function) bool {
        return switch (func.type) {
            .function_declaration, .ts_declare_function => false,
            else => !ast.isNull(func.id),
        };
    }

    fn isNamedClassExpression(cls: ast.Class) bool {
        return cls.type != .class_declaration and !ast.isNull(cls.id);
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

    // finalize into an immutable ScopeTree. frees the scope stack since
    // it's only needed during the walk.
    pub fn toScopeTree(self: *ScopeTracker) Allocator.Error!ScopeTree {
        self.scope_stack.deinit(self.allocator);
        return .{ .scopes = try self.scopes.toOwnedSlice(self.allocator), .allocator = self.allocator };
    }

    pub fn deinit(self: *ScopeTracker) void {
        self.scopes.deinit(self.allocator);
        self.scope_stack.deinit(self.allocator);
    }
};
