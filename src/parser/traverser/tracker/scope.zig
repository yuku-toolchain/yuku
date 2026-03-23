const std = @import("std");
const ast = @import("../../ast.zig");

const Allocator = std.mem.Allocator;

/// ID for a scope. `.root` is the top-level scope, `.none` means no scope.
pub const ScopeId = enum(u32) { root = 0, none = std.math.maxInt(u32), _ };

/// A single lexical scope in the JavaScript scope tree.
pub const Scope = struct {
    /// The AST node that created this scope.
    node: ast.NodeIndex,
    /// Parent scope, or `.none` for the root.
    parent: ScopeId,
    /// Nearest ancestor (or self) where `var` declarations hoist to.
    hoist_target: ScopeId,
    kind: Kind,
    flags: Flags,

    /// What kind of JavaScript construct created this scope.
    pub const Kind = enum(u8) {
        /// Section 16.1.7 GlobalDeclarationInstantiation.
        global,
        /// Section 16.2.1.6 ModuleDeclarationEnvironmentSetup.
        /// Modules are always strict.
        module,
        /// Section 10.2.11 FunctionDeclarationInstantiation.
        function,
        /// Section 14.2.2 Block runtime semantics, 14.2.3 BlockDeclarationInstantiation.
        block,
        /// Section 15.7.14 ClassDefinitionEvaluation.
        class,
        /// Section 15.7.11 ClassStaticBlockDefinitionEvaluation.
        static_block,
        /// Intermediate scope for named function/class expression names.
        ///
        /// Section 15.2.5 InstantiateOrdinaryFunctionExpression (step 2-3):
        ///   Creates a new environment, binds the function name as immutable,
        ///   then the function body closes over that environment.
        ///
        /// Section 15.7.14 ClassDefinitionEvaluation (step 5-6):
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
        /// Whether this scope is in strict mode.
        strict: bool = false,
    };
};

/// The result of a scoped traversal. Contains all scopes created during the walk.
pub const ScopeTree = struct {
    scopes: []const Scope,

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

const ScopeStack = struct {
    const capacity = 256;

    buf: [capacity]ScopeId = undefined,
    len: usize = 0,

    pub fn push(self: *ScopeStack, id: ScopeId) void {
        if (self.len < capacity) {
            self.buf[self.len] = id;
        }
        self.len += 1;
    }

    pub fn pop(self: *ScopeStack) void {
        self.len -= 1;
    }

    pub inline fn top(self: *const ScopeStack) ScopeId {
        return if (self.len > 0 and self.len <= capacity) self.buf[self.len - 1] else .root;
    }
};

pub const ScopeTracker = struct {
    tree: *const ast.Tree,
    allocator: Allocator,
    scopes: std.ArrayList(Scope) = .{},
    scope_stack: ScopeStack = .{},

    pub fn init(tree: *ast.Tree) Allocator.Error!ScopeTracker {
        const alloc = tree.allocator();
        var self = ScopeTracker{ .tree = tree, .allocator = alloc };

        const estimated_scopes: u32 = @max(16, @as(u32, @intCast(tree.nodes.len / 16)));
        try self.scopes.ensureTotalCapacity(alloc, estimated_scopes);

        self.pushRoot();
        return self;
    }

    fn pushRoot(self: *ScopeTracker) void {
        self.scopes.appendAssumeCapacity(.{
            .node = self.tree.program,
            .parent = .none,
            .hoist_target = .root,
            .kind = .global,
            .flags = .{},
        });

        self.scope_stack.push(.root);

        if (self.tree.source_type == .module) {
            self.scopes.appendAssumeCapacity(.{
                .node = self.tree.program,
                .parent = .root,
                .hoist_target = @enumFromInt(1),
                .kind = .module,
                .flags = .{ .strict = true },
            });

            self.scope_stack.push(@enumFromInt(1));
        }
    }

    // creates a new child scope under the current one.
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

        self.scope_stack.push(id);
    }

    // processes a node on enter, pushes scopes for scope-creating nodes.
    pub fn enter(self: *ScopeTracker, index: ast.NodeIndex, data: ast.NodeData) Allocator.Error!void {
        switch (data) {
            .directive => |d| {
                if (std.mem.eql(u8, self.tree.getString(d.value), "use strict")) {
                    self.currentScopeMut().flags.strict = true;
                }
            },
            .function => |func| {
                const flags = self.inheritedFlags();
                // named function expressions get an extra scope for their name.
                // we push it before the function scope so it sits between
                // outer and body. see Scope.Kind.expression_name for details.
                if (isNamedFunctionExpression(func))
                    try self.pushScope(.expression_name, index, flags);
                try self.pushScope(.function, index, flags);
            },
            .arrow_function_expression => try self.pushScope(.function, index, self.inheritedFlags()),
            .block_statement,
            .for_statement, .for_in_statement, .for_of_statement,
            .catch_clause,
            // switch creates one block scope for all case clauses
            .switch_statement,
            => try self.pushScope(.block, index, self.inheritedFlags()),
            .class => |cls| {
                const flags = self.inheritedFlags();
                // same as named function expressions, named class expressions
                // get an extra scope for their name.
                if (isNamedClassExpression(cls))
                    try self.pushScope(.expression_name, index, flags);
                try self.pushScope(.class, index, flags);
            },
            .static_block => try self.pushScope(.static_block, index, self.inheritedFlags()),
            else => {},
        }
    }

    inline fn inheritedFlags(self: *const ScopeTracker) Scope.Flags {
        return .{ .strict = self.currentScope().flags.strict };
    }

    // mirrors `enter`, pops the same number of scopes that were pushed.
    // For named function/class expressions, that's two pops (body + name).
    pub fn exit(self: *ScopeTracker, data: ast.NodeData) void {
        switch (data) {
            .function => |func| {
                self.scope_stack.pop();
                if (isNamedFunctionExpression(func))
                    self.scope_stack.pop();
            },
            .arrow_function_expression,
            .block_statement,
            .for_statement, .for_in_statement, .for_of_statement,
            .catch_clause, .switch_statement,
            .static_block,
            => self.scope_stack.pop(),
            .class => |cls| {
                self.scope_stack.pop();
                if (isNamedClassExpression(cls))
                    self.scope_stack.pop();
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
            else => func.id != .null,
        };
    }

    fn isNamedClassExpression(cls: ast.Class) bool {
        return cls.type != .class_declaration and cls.id != .null;
    }

    /// Returns the ID of the current scope.
    pub inline fn currentScopeId(self: *const ScopeTracker) ScopeId {
        return self.scope_stack.top();
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
    pub inline fn currentScopeMut(self: *ScopeTracker) *Scope {
        return &self.scopes.items[@intFromEnum(self.currentScopeId())];
    }

    /// Returns the scope for the given ID.
    pub inline fn getScope(self: *const ScopeTracker, id: ScopeId) Scope {
        return self.scopes.items[@intFromEnum(id)];
    }

    /// Returns a mutable pointer to the scope for the given ID.
    pub inline fn getScopeMut(self: *ScopeTracker, id: ScopeId) *Scope {
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

    /// Finalizes into an immutable `ScopeTree`.
    pub fn toScopeTree(self: *ScopeTracker) ScopeTree {
        return .{ .scopes = self.scopes.items };
    }
};
