const std = @import("std");
const ast = @import("../ast.zig");
const ecmascript = @import("../ecmascript.zig");

const Allocator = std.mem.Allocator;

/// ID for a scope. `.root` is the top-level scope, `.none` means no scope.
pub const ScopeId = enum(u32) { root = 0, module = 1, none = std.math.maxInt(u32), _ };

/// A single lexical scope in the scope tree.
///
/// ## Example
/// ```ts
/// function f() { { var a; let b; } }
/// //       ^ creates a function scope, a hoist target
/// //             ^ creates a block scope where `b` lives,
/// //               while `a` hoists to the function scope
/// ```
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
        /// Intermediate scope holding a named function/class expression's
        /// name (sections 15.2.5 and 15.7.14). The name binds in its own
        /// environment between the outer scope and the body, so it never
        /// conflicts with same-named bindings inside the body:
        ///
        ///   outer scope (x lives here)
        ///     expression_name scope (foo lives here)
        ///       function scope (body bindings live here)
        expression_name,

        /// TS namespace body. Acts as a var-hoist target so vars
        /// declared inside don't escape to the surrounding scope.
        ts_module,

        /// Separate var environment of a function body whose parameter
        /// list contains expressions (10.2.11 FunctionDeclarationInstantiation
        /// step 28):
        ///
        ///   function f(a = () => x) { var x }
        ///   //       ^ function scope: a
        ///   //                        ^ function_body scope: x,
        ///   //                          invisible to the default's closure
        function_body,

        /// Returns whether `var` declarations hoist to this scope kind.
        pub fn isHoistTarget(self: Kind) bool {
            return switch (self) {
                .global, .module, .function, .static_block, .ts_module, .function_body => true,
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

/// All scopes of a tree, indexed by `ScopeId`.
pub const ScopeTree = struct {
    list: []const Scope,

    /// The scope with the given id.
    pub inline fn get(self: ScopeTree, id: ScopeId) Scope {
        std.debug.assert(id != .none);
        std.debug.assert(@intFromEnum(id) < self.list.len);
        return self.list[@intFromEnum(id)];
    }

    /// Walks from `start` up to the root, yielding `start` first.
    pub fn ancestors(self: ScopeTree, start: ScopeId) AncestorIterator {
        std.debug.assert(start == .none or @intFromEnum(start) < self.list.len);
        return .{ .list = self.list, .current = start };
    }

    /// Yields each scope id from a starting scope up to the root.
    pub const AncestorIterator = struct {
        list: []const Scope,
        current: ScopeId,

        /// The next scope id, or `null` past the root.
        pub fn next(self: *AncestorIterator) ?ScopeId {
            const id = self.current;
            if (id == .none) return null;
            std.debug.assert(@intFromEnum(id) < self.list.len);
            self.current = self.list[@intFromEnum(id)].parent;
            return id;
        }
    };
};

pub const ScopeTracker = struct {
    tree: *const ast.Tree,
    allocator: Allocator,
    scopes: std.ArrayList(Scope) = .empty,
    // active scope. follow the parent chain to walk the path.
    current: ScopeId = .root,
    // scopes saved while a decorator subtree retargets `current`,
    // one entry per nested decorator. see `enter` on `.decorator`.
    decorator_saved: std.ArrayList(ScopeId) = .empty,

    pub fn init(tree: *ast.Tree) Allocator.Error!ScopeTracker {
        std.debug.assert(tree.root != .null);

        const alloc = tree.allocator();
        var self = ScopeTracker{ .tree = tree, .allocator = alloc };

        const estimated_scopes: u32 = @max(16, @as(u32, @intCast(tree.nodes.len / 16)));
        try self.scopes.ensureTotalCapacity(alloc, estimated_scopes);

        self.pushRoot();

        // exactly one root scope, plus the module wrapper when applicable
        std.debug.assert(self.scopes.items.len == if (tree.source_type == .module)
            @as(usize, 2)
        else
            1);
        return self;
    }

    fn pushRoot(self: *ScopeTracker) void {
        std.debug.assert(self.scopes.items.len == 0);

        self.scopes.appendAssumeCapacity(.{
            .node = self.tree.root,
            .parent = .none,
            .hoist_target = .root,
            .kind = .global,
            .flags = .{},
        });
        self.current = .root;

        if (self.tree.source_type == .module) {
            self.scopes.appendAssumeCapacity(.{
                .node = self.tree.root,
                .parent = .root,
                .hoist_target = .module,
                .kind = .module,
                .flags = .{ .strict = true },
            });
            self.current = .module;
        }
    }

    pub fn pushScope(
        self: *ScopeTracker,
        kind: Scope.Kind,
        node: ast.NodeIndex,
        flags: Scope.Flags,
    ) Allocator.Error!void {
        // scopes.len is the about-to-be-assigned id and must fit in u32
        std.debug.assert(self.scopes.items.len < std.math.maxInt(u32));

        const id: ScopeId = @enumFromInt(@as(u32, @intCast(self.scopes.items.len)));
        const parent = self.currentScope();

        try self.scopes.append(self.allocator, .{
            .node = node,
            .parent = self.current,
            .hoist_target = if (kind.isHoistTarget()) id else parent.hoist_target,
            .kind = kind,
            .flags = flags,
        });
        self.current = id;
    }

    fn popScope(self: *ScopeTracker) void {
        // root and module are never popped
        std.debug.assert(self.current != .none);
        std.debug.assert(self.current != .root);

        self.current = self.currentScope().parent;
    }

    pub fn enter(
        self: *ScopeTracker,
        index: ast.NodeIndex,
        parent: ast.NodeIndex,
        data: ast.NodeData,
    ) Allocator.Error!void {
        switch (data) {
            .directive => |d| {
                if (std.mem.eql(u8, self.tree.string(d.value), "use strict")) {
                    self.currentMut().flags.strict = true;
                }
            },
            .function => |func| {
                const flags: Scope.Flags =
                    if (self.hasRetroActiveUseStrict(func.body))
                        .{ .strict = true }
                    else
                        self.inheritStrictFlag();

                // the expression-name scope sits between outer and body
                if (isNamedFunctionExpression(func))
                    try self.pushScope(.expression_name, index, flags);

                try self.pushScope(.function, index, flags);
            },
            .arrow_function_expression => |expr| {
                const flags: Scope.Flags =
                    if (self.hasRetroActiveUseStrict(expr.body))
                        .{ .strict = true }
                    else
                        self.inheritStrictFlag();

                try self.pushScope(.function, index, flags);
            },
            .function_body => {
                // pushed only when the parent function's parameter list
                // contains expressions, see Kind.function_body
                const params = switch (self.tree.data(parent)) {
                    .function => |f| f.params,
                    .arrow_function_expression => |a| a.params,
                    else => return,
                };
                if (params == .null) return;
                const params_data = self.tree.data(params);
                if (params_data != .formal_parameters) return;
                if (ecmascript.findParameterExpression(self.tree, params_data.formal_parameters) != null)
                    try self.pushScope(.function_body, index, self.inheritStrictFlag());
            },
            .block_statement => {
                // 14.15.2 CatchClauseEvaluation gives the parameter and
                // the block separate environments. folding them into one
                // scope is unobservable and lets single-scope lookups
                // catch the 14.15.1 conflicts, except when the parameter
                // contains an expression that runs before the block env
                // exists:
                //
                //   catch (e)       { let e }   // shared, conflict found
                //   catch ([e = 1]) { let e }   // split, the default
                //                               // must not see the let
                const current = self.tree.data(self.currentScope().node);
                const shares_catch_scope = current == .catch_clause and
                    current.catch_clause.body == index and
                    ecmascript.findPatternExpression(self.tree, current.catch_clause.param) == null;
                if (!shares_catch_scope)
                    try self.pushScope(.block, index, self.inheritStrictFlag());
            },
            .switch_case => {
                // one case-block scope per switch, shared by all cases,
                // created at the first case. per 14.12.4 the discriminant
                // evaluates (steps 1-2) before the case env exists
                // (step 4):
                //
                //   switch (x) { case 1: let x }
                //   //      ^ outer scope    ^ case-block scope
                if (self.currentScope().node != parent)
                    try self.pushScope(.block, parent, self.inheritStrictFlag());
            },
            // tsc resolves unqualified member references inside the body
            .ts_enum_body => try self.pushScope(.block, index, self.inheritStrictFlag()),
            .for_statement,
            .for_in_statement,
            .for_of_statement,
            .catch_clause,
            .ts_interface_declaration,
            .ts_type_alias_declaration,
            .ts_function_type,
            .ts_constructor_type,
            .ts_method_signature,
            .ts_call_signature_declaration,
            .ts_construct_signature_declaration,
            .ts_index_signature,
            .ts_mapped_type,
            .ts_conditional_type,
            => try self.pushScope(.block, index, self.inheritStrictFlag()),
            .ts_module_block => try self.pushScope(.ts_module, index, self.inheritStrictFlag()),
            .class => |cls| {
                // classes are always strict mode (section 15.7.14)
                const flags = Scope.Flags{ .strict = true };

                if (isNamedClassExpression(cls))
                    try self.pushScope(.expression_name, index, flags);

                try self.pushScope(.class, index, flags);
            },
            .static_block => try self.pushScope(.static_block, index, self.inheritStrictFlag()),
            .decorator => {
                // decorators evaluate in the scope enclosing the class:
                // neither its type parameters nor an expression's own
                // name are visible, so retarget `current` for the subtree
                try self.decorator_saved.append(self.allocator, self.current);
                self.current = self.decoratorEvalScope();
            },
            else => {},
        }
    }

    // the parent of the nearest class scope, past a named class
    // expression's name scope
    fn decoratorEvalScope(self: *const ScopeTracker) ScopeId {
        var it = self.ancestors(self.current);
        while (it.next()) |id| {
            if (self.get(id).kind != .class) continue;
            const parent = self.get(id).parent;
            if (parent == .none) break;
            if (self.get(parent).kind == .expression_name) {
                return self.get(parent).parent;
            }
            return parent;
        }

        return self.current;
    }

    // "use strict" applies retroactively to the whole function including
    // its parameter list, so peek into the body and set the flag at
    // scope-creation time, before any parameter is visited
    fn hasRetroActiveUseStrict(self: *const ScopeTracker, body_index: ast.NodeIndex) bool {
        if (body_index == .null) return false;

        const body = self.tree.data(body_index);

        if (body != .function_body) return false;

        const function_body = body.function_body;

        for (self.tree.extra(function_body.body)) |s| {
            const d = self.tree.data(s);

            if (d != .directive) break;

            if (std.mem.eql(u8, self.tree.string(d.directive.value), "use strict")) {
                return true;
            }
        }

        return false;
    }

    inline fn inheritStrictFlag(self: *const ScopeTracker) Scope.Flags {
        return .{ .strict = self.currentScope().flags.strict };
    }

    // A node pops exactly the scopes recording it as creator: two for a
    // named function or class expression (name + body), zero for a
    // shared or never-created scope. Cannot drift from `enter`.
    pub fn exit(self: *ScopeTracker, index: ast.NodeIndex, data: ast.NodeData) void {
        switch (data) {
            // the root and module scopes live for the whole walk
            .program => {},
            .decorator => {
                // every decorator enter saved exactly one scope
                std.debug.assert(self.decorator_saved.items.len > 0);
                self.current = self.decorator_saved.pop().?;
            },
            else => while (self.current != .root and self.currentScope().node == index) {
                self.popScope();
            },
        }
    }

    fn isNamedFunctionExpression(func: ast.Function) bool {
        return switch (func.type) {
            .function_declaration, .ts_declare_function => false,
            else => func.id != .null,
        };
    }

    fn isNamedClassExpression(cls: ast.Class) bool {
        return cls.type != .class_declaration and cls.id != .null;
    }

    /// The current scope.
    pub inline fn currentScope(self: *const ScopeTracker) Scope {
        std.debug.assert(self.current != .none);
        return self.get(self.current);
    }

    // mutable view of the current scope, for the tracker's own
    // strict-mode bookkeeping
    inline fn currentMut(self: *ScopeTracker) *Scope {
        std.debug.assert(self.current != .none);
        return &self.scopes.items[@intFromEnum(self.current)];
    }

    /// The scope where a `var` declared here would land.
    pub inline fn hoistTarget(self: *const ScopeTracker) ScopeId {
        return self.currentScope().hoist_target;
    }

    /// The scope with the given id.
    pub inline fn get(self: *const ScopeTracker, id: ScopeId) Scope {
        std.debug.assert(id != .none);
        std.debug.assert(@intFromEnum(id) < self.scopes.items.len);
        return self.scopes.items[@intFromEnum(id)];
    }

    /// Whether the current scope is in strict mode.
    pub inline fn isStrict(self: *const ScopeTracker) bool {
        return self.currentScope().flags.strict;
    }

    /// Walks from `start` up to the root, yielding `start` first.
    pub fn ancestors(self: *const ScopeTracker, start: ScopeId) ScopeTree.AncestorIterator {
        std.debug.assert(start == .none or @intFromEnum(start) < self.scopes.items.len);
        return .{ .list = self.scopes.items, .current = start };
    }

    /// Finalizes into an immutable `ScopeTree`.
    pub fn toScopeTree(self: *ScopeTracker) ScopeTree {
        return .{ .list = self.scopes.items };
    }
};
