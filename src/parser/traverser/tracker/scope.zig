const std = @import("std");
const ast = @import("../../ast.zig");

const Allocator = std.mem.Allocator;

/// Typed index into the scope array. `.root` is always 0, `.none` is the sentinel.
pub const ScopeId = enum(u32) { root = 0, none = std.math.maxInt(u32), _ };

/// A single lexical scope in the JavaScript scope tree.
pub const Scope = struct {
    /// The AST node that created this scope.
    node: ast.NodeIndex,
    /// Parent scope, or `.none` for the root.
    parent: ScopeId,
    /// Nearest ancestor (or self) where `var` declarations hoist to.
    /// ECMA-262 10.2.11 FunctionDeclarationInstantiation.
    hoist_target: ScopeId,
    kind: Kind,
    flags: Flags,

    /// What kind of JavaScript construct created this scope.
    pub const Kind = enum(u8) {
        /// ECMA-262 16.1.7 GlobalDeclarationInstantiation.
        global,
        /// ECMA-262 16.2.1.6 ModuleDeclarationEnvironmentSetup.
        /// Modules are always strict (ECMA-262 11.2.2).
        module,
        /// ECMA-262 10.2.11 FunctionDeclarationInstantiation.
        function,
        /// ECMA-262 14.2.2 Block runtime semantics, 14.2.3 BlockDeclarationInstantiation.
        block,
        /// ECMA-262 15.7.14 ClassDefinitionEvaluation.
        class,
        /// ECMA-262 15.7.11 ClassStaticBlockDefinitionEvaluation.
        static_block,
        /// Intermediate scope for named function/class expression names.
        ///
        /// ECMA-262 15.2.5 InstantiateOrdinaryFunctionExpression (step 2-3):
        ///   Creates a new environment, binds the function name as immutable,
        ///   then the function body closes over that environment.
        ///
        /// ECMA-262 15.7.14 ClassDefinitionEvaluation (step 5-6):
        ///   Same pattern for class expressions with a name.
        ///
        /// In JS, `const x = function foo() { const foo = 1; }` is valid
        /// because the expression name `foo` lives in a separate scope
        /// between the outer scope and the function body. Same for
        /// `const x = class C { }`. Without this, the name would
        /// conflict with same-named bindings inside the body.
        ///
        /// Scope structure for `const x = function foo() { ... }`:
        ///   outer scope (x lives here)
        ///     expression_name scope (foo lives here)
        ///       function scope (body bindings live here)
        expression_name,

        /// Returns whether `var` declarations hoist to this scope kind.
        pub fn isHoistTarget(kind: Kind) bool {
            return switch (kind) {
                .global, .module, .function, .static_block => true,
                else => false,
            };
        }
    };

    /// Scope-level flags.
    pub const Flags = struct {
        /// Whether this scope is in strict mode (ECMA-262 11.2.1).
        strict: bool = false,
    };
};

/// Immutable result of a scoped traversal. Flat array of scopes
/// linked by parent pointers. Owns its backing memory.
pub const ScopeTree = struct {
    scopes: []const Scope,
    allocator: Allocator,

    /// Frees the backing scope array.
    pub fn deinit(self: *ScopeTree) void {
        self.allocator.free(self.scopes);
        self.* = undefined;
    }

    /// Returns the scope for the given ID.
    pub inline fn getScope(self: ScopeTree, id: ScopeId) Scope {
        return self.scopes[@intFromEnum(id)];
    }

    /// Returns an iterator that walks from `start` up to the root scope.
    pub fn ancestors(self: ScopeTree, start: ScopeId) AncestorIterator {
        return .{ .scopes = self.scopes, .current = start };
    }

    /// Walks up from `start` to root, yielding each scope ID.
    pub const AncestorIterator = struct {
        scopes: []const Scope,
        current: ScopeId,

        /// Returns the next ancestor scope ID, or `null` when the root has been passed.
        pub fn next(self: *AncestorIterator) ?ScopeId {
            const id = self.current;
            if (id == .none) return null;
            self.current = self.scopes[@intFromEnum(id)].parent;
            return id;
        }
    };
};

/// Mutable builder that tracks JavaScript lexical scopes during an AST walk.
/// Call `enter` when visiting a node and `exit` when leaving it.
/// The tracker pushes/pops scopes for nodes that create them
/// (functions, blocks, classes, etc.) and ignores everything else.
pub const ScopeTracker = struct {
    tree: *const ast.ParseTree,
    allocator: Allocator,
    scopes: std.ArrayList(Scope) = .{},
    scope_stack: std.ArrayList(ScopeId) = .{},

    /// Creates a new tracker with a root scope (and module scope for modules).
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

        // ecma262 11.2.2: module code is always strict
        if (self.tree.source_type == .module) {
            try self.pushScope(.module, self.tree.program, .{ .strict = true });
        }
    }

    /// Pushes a new child scope onto the stack.
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

    /// Processes a node on enter: pushes scopes for scope-creating nodes and
    /// detects `"use strict"` directives.
    pub fn enter(self: *ScopeTracker, index: ast.NodeIndex, data: ast.NodeData) Allocator.Error!void {
        // ecma262 11.2.1: check for "use strict" directive prologue
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
                // ecma262 15.2.5 InstantiateOrdinaryFunctionExpression:
                // named function expressions get an extra scope for their name.
                // we push it before the function scope so it sits between
                // outer and body. see Scope.Kind.expression_name for details.
                if (isNamedFunctionExpression(func))
                    try self.pushScope(.expression_name, index, flags);
                try self.pushScope(.function, index, flags);
            },
            .arrow_function_expression => try self.pushScope(.function, index, flags),
            // ecma262 14.2.2 Block
            .block_statement => try self.pushScope(.block, index, flags),
            // ecma262 14.7.4.2 ForLoopEvaluation (creates scope for let/const in initializer)
            .for_statement,
            // ecma262 14.7.5.5 ForIn/OfHeadEvaluation
            .for_in_statement, .for_of_statement,
            // ecma262 14.15.2 CatchClauseEvaluation
            .catch_clause,
            // ecma262 14.12.4 switch creates one block scope for all case clauses
            .switch_statement,
            => try self.pushScope(.block, index, flags),
            .class => |cls| {
                // ecma262 15.7.14 ClassDefinitionEvaluation (step 5-6):
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

    /// Mirrors `enter`: pops the same number of scopes that were pushed.
    /// For named function/class expressions, that's two pops (body + name).
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

    /// Returns the ID of the current scope.
    pub inline fn currentScopeId(self: *const ScopeTracker) ScopeId {
        return self.scope_stack.getLast();
    }

    /// Returns the ID of the nearest hoist target scope (where `var` lands).
    pub inline fn currentHoistScopeId(self: *const ScopeTracker) ScopeId {
        return self.currentScope().hoist_target;
    }

    /// Returns the current scope.
    pub inline fn currentScope(self: *const ScopeTracker) Scope {
        return self.getScope(self.currentScopeId());
    }

    /// Returns a mutable pointer to the current scope.
    pub inline fn currentScopePtr(self: *ScopeTracker) *Scope {
        return &self.scopes.items[@intFromEnum(self.currentScopeId())];
    }

    /// Returns the scope for the given ID.
    pub inline fn getScope(self: *const ScopeTracker, id: ScopeId) Scope {
        return self.scopes.items[@intFromEnum(id)];
    }

    /// Returns a mutable pointer to the scope for the given ID.
    pub inline fn getScopePtr(self: *ScopeTracker, id: ScopeId) *Scope {
        return &self.scopes.items[@intFromEnum(id)];
    }

    /// Returns whether the current scope is in strict mode.
    pub inline fn isStrict(self: *const ScopeTracker) bool {
        return self.currentScope().flags.strict;
    }

    /// Returns an iterator that walks from `start` up to the root scope.
    pub fn ancestors(self: *const ScopeTracker, start: ScopeId) ScopeTree.AncestorIterator {
        return .{ .scopes = self.scopes.items, .current = start };
    }

    /// Finalizes into an immutable `ScopeTree`. Frees the scope stack since
    /// it's only needed during the walk.
    pub fn toScopeTree(self: *ScopeTracker) Allocator.Error!ScopeTree {
        self.scope_stack.deinit(self.allocator);
        return .{ .scopes = try self.scopes.toOwnedSlice(self.allocator), .allocator = self.allocator };
    }

    /// Frees all resources. Only needed if the traversal is aborted early
    /// (normally, call `toScopeTree` instead which takes ownership).
    pub fn deinit(self: *ScopeTracker) void {
        self.scopes.deinit(self.allocator);
        self.scope_stack.deinit(self.allocator);
    }
};
