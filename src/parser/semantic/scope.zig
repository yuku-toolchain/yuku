const std = @import("std");
const ast = @import("../ast.zig");
const static_semantics = @import("spec").static_semantics;
const dialect = @import("../dialect/root.zig");

const Allocator = std.mem.Allocator;

/// Identifier for a `Scope`. `.root` is the global scope and `.none` means absent.
pub const ScopeId = enum(u32) { root = 0, none = std.math.maxInt(u32), _ };

pub const module_scope: ScopeId = @fromBackingInt(@intCast(1));

/// A lexical scope.
pub const Scope = struct {
    /// The node that created the scope.
    node: ast.NodeIndex,
    /// The parent scope, or `.none` for the root.
    parent: ScopeId,
    /// The nearest scope, itself included, where a `var` declared here lands.
    hoist_target: ScopeId,
    kind: Kind,
    flags: Flags,

    /// The construct that created a scope.
    pub const Kind = enum(u8) {
        /// Section 16.1.7 GlobalDeclarationInstantiation.
        global,
        /// Section 16.2.1.6 ModuleDeclarationEnvironmentSetup. Always strict.
        module,
        /// Section 10.2.11 FunctionDeclarationInstantiation.
        function,
        /// Section 14.2.2 Block runtime semantics, 14.2.3 BlockDeclarationInstantiation.
        block,
        /// Section 15.7.14 ClassDefinitionEvaluation.
        class,
        /// Section 15.7.11 ClassStaticBlockDefinitionEvaluation.
        static_block,
        /// The scope of a named function or class expression's own name,
        /// between the outer scope and the body (15.2.5, 15.7.14).
        expression_name,
        /// A TypeScript namespace body.
        ts_module,
        /// A function body's own scope. The signature sits outside it so a
        /// parameter annotation cannot see a body-local type, and body `var`s
        /// hoist past it unless the parameter list has expressions (10.2.11 step 28).
        ///
        ///   function f(a: T) { type T = {} }
        ///   //            ^ unresolved, T is local to the body
        function_body,

        /// Whether a `var` can hoist to this kind. A `function_body` may still pass it on.
        pub fn isHoistTarget(self: Kind) bool {
            return switch (self) {
                .global, .module, .function, .static_block, .ts_module, .function_body => true,
                else => false,
            };
        }
    };

    pub const Flags = struct {
        /// Whether the scope is strict mode code.
        strict: bool = false,
    };
};

/// All scopes of a tree, indexed by `ScopeId`.
pub const ScopeTree = struct {
    list: []const Scope,

    /// The scope with the given id.
    pub inline fn get(self: ScopeTree, id: ScopeId) Scope {
        std.debug.assert(id != .none);
        std.debug.assert(@backingInt(id) < self.list.len);
        return self.list[@backingInt(id)];
    }

    /// Iterates from `start` up to the root.
    pub fn ancestors(self: ScopeTree, start: ScopeId) AncestorIterator {
        std.debug.assert(start == .none or @backingInt(start) < self.list.len);
        return .{ .list = self.list, .current = start };
    }

    /// Yields scope ids from a starting scope up to the root.
    pub const AncestorIterator = struct {
        list: []const Scope,
        current: ScopeId,

        /// The next scope id, or `null` past the root.
        pub fn next(self: *AncestorIterator) ?ScopeId {
            const id = self.current;
            if (id == .none) return null;
            std.debug.assert(@backingInt(id) < self.list.len);
            self.current = self.list[@backingInt(id)].parent;
            return id;
        }
    };
};

pub const ScopeTracker = struct {
    tree: *const ast.Tree,
    allocator: Allocator,
    scopes: std.ArrayList(Scope) = .empty,
    current: ScopeId = .root,
    decorator_saved: std.ArrayList(ScopeId) = .empty,

    pub fn init(tree: *ast.Tree) Allocator.Error!ScopeTracker {
        std.debug.assert(tree.root != .null);

        const alloc = tree.allocator();
        var self = ScopeTracker{ .tree = tree, .allocator = alloc };

        const estimated_scopes: u32 = @max(16, @as(u32, @intCast(tree.nodes.len / 16)));
        try self.scopes.ensureTotalCapacity(alloc, estimated_scopes);
        std.debug.assert(self.scopes.items.len == 0);

        self.scopes.appendAssumeCapacity(.{
            .node = tree.root,
            .parent = .none,
            .hoist_target = .root,
            .kind = .global,
            .flags = .{},
        });
        if (tree.source_type == .module) {
            self.scopes.appendAssumeCapacity(.{
                .node = tree.root,
                .parent = .root,
                .hoist_target = module_scope,
                .kind = .module,
                .flags = .{ .strict = true },
            });
            self.current = module_scope;
        }

        std.debug.assert(self.scopes.items.len == if (tree.source_type == .module)
            @as(usize, 2)
        else
            1);
        return self;
    }

    fn pushScope(
        self: *ScopeTracker,
        kind: Scope.Kind,
        node: ast.NodeIndex,
        flags: Scope.Flags,
    ) Allocator.Error!void {
        std.debug.assert(self.scopes.items.len < std.math.maxInt(u32));

        const id: ScopeId = @fromBackingInt(@intCast(self.scopes.items.len));
        const parent = self.currentScope();
        std.debug.assert(opens_scope.contains(self.tree.data(node)));

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
        std.debug.assert(self.current != .none);
        std.debug.assert(self.current != .root);

        self.current = self.currentScope().parent;
    }

    pub inline fn enter(
        self: *ScopeTracker,
        comptime tag: ast.NodeTag,
        index: ast.NodeIndex,
        parent: ast.NodeIndex,
        data: ast.NodeData,
    ) Allocator.Error!void {
        switch (tag) {
            .directive => {
                const d = data.directive;
                if (std.mem.eql(u8, self.tree.string(d.value), "use strict")) {
                    self.currentMut().flags.strict = true;
                }
            },
            .function => {
                const func = data.function;
                const flags = self.functionFlags(func.body);
                const declaration = func.type == .function_declaration or
                    func.type == .ts_declare_function;
                if (!declaration and func.id != .null) {
                    try self.pushScope(.expression_name, index, flags);
                }
                try self.pushScope(.function, index, flags);
            },
            .arrow_function_expression => {
                const expr = data.arrow_function_expression;
                try self.pushScope(.function, index, self.functionFlags(expr.body));
            },
            .function_body => {
                if (parent == .null) return;
                const params = switch (self.tree.data(parent)) {
                    .function => |f| f.params,
                    .arrow_function_expression => |a| a.params,
                    else => return,
                };
                try self.pushScope(.function_body, index, self.inheritStrictFlag());
                // a separate var environment exists only with parameter expressions (10.2.11)
                if (!hasParameterExpression(self.tree, params)) {
                    const enclosing = self.currentScope().parent;
                    self.currentMut().hoist_target = self.get(enclosing).hoist_target;
                }
            },
            .block_statement => {
                // a catch body shares the parameter scope so 14.15.1 conflicts surface,
                // unless a parameter expression runs before the body exists (14.15.2)
                const current = self.tree.data(self.currentScope().node);
                const shares_catch_scope = current == .catch_clause and
                    current.catch_clause.body == index and
                    static_semantics.findPatternExpression(
                        self.tree,
                        current.catch_clause.param,
                    ) == null;
                if (!shares_catch_scope) {
                    try self.pushScope(.block, index, self.inheritStrictFlag());
                }
            },
            .switch_case => {
                // one case-block scope per switch, entered after the discriminant (14.12.4)
                if (parent == .null) return;
                if (self.currentScope().node != parent) {
                    try self.pushScope(.block, parent, self.inheritStrictFlag());
                }
            },
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
            // tsc resolves unqualified member references inside the body
            .ts_enum_body,
            => try self.pushScope(.block, index, self.inheritStrictFlag()),
            .ts_module_block => try self.pushScope(.ts_module, index, self.inheritStrictFlag()),
            // a scope per qualifier of `namespace A.B.C {}`
            .ts_module_declaration => {
                const decl = data.ts_module_declaration;
                var name = decl.id;
                while (self.tree.data(name) == .ts_qualified_name) {
                    try self.pushScope(.ts_module, index, self.inheritStrictFlag());
                    name = self.tree.data(name).ts_qualified_name.left;
                }
            },
            .class => {
                const cls = data.class;
                // class code is always strict (15.7.14)
                const flags = Scope.Flags{ .strict = true };
                if (cls.type != .class_declaration and cls.id != .null) {
                    try self.pushScope(.expression_name, index, flags);
                }
                try self.pushScope(.class, index, flags);
            },
            .static_block => try self.pushScope(.static_block, index, self.inheritStrictFlag()),
            .dialect => if (dialect.shape(data.dialect.kind).block_scope) {
                try self.pushScope(.block, index, self.inheritStrictFlag());
            },
            .decorator => {
                try self.decorator_saved.append(self.allocator, self.current);
                const on_class = parent != .null and self.tree.data(parent) == .class;
                self.current = self.decoratorScope(on_class);
            },
            else => {},
        }
    }

    fn decoratorScope(self: *const ScopeTracker, on_class: bool) ScopeId {
        var it = self.ancestors(self.current);
        const class = while (it.next()) |id| {
            if (self.get(id).kind == .class) break id;
        } else return self.current;
        if (!on_class) return class;
        const outer = self.get(class).parent;
        if (self.get(outer).kind == .expression_name) return self.get(outer).parent;
        return outer;
    }

    // a body's "use strict" also covers its parameters
    fn functionFlags(self: *const ScopeTracker, body_index: ast.NodeIndex) Scope.Flags {
        const strict = self.currentScope().flags.strict;
        return .{ .strict = strict or hasUseStrict(self.tree, body_index) };
    }

    fn hasUseStrict(tree: *const ast.Tree, body_index: ast.NodeIndex) bool {
        if (body_index == .null) return false;
        const body = tree.data(body_index);
        if (body != .function_body) return false;
        for (tree.extra(body.function_body.body)) |statement| {
            const data = tree.data(statement);
            if (data != .directive) break;
            if (std.mem.eql(u8, tree.string(data.directive.value), "use strict")) return true;
        }
        return false;
    }

    inline fn inheritStrictFlag(self: *const ScopeTracker) Scope.Flags {
        return .{ .strict = self.currentScope().flags.strict };
    }

    pub inline fn exit(
        self: *ScopeTracker,
        comptime tag: ast.NodeTag,
        index: ast.NodeIndex,
    ) void {
        switch (tag) {
            // the root and module scopes outlive the walk
            .program => {},
            .decorator => {
                std.debug.assert(self.decorator_saved.items.len > 0);
                self.current = self.decorator_saved.pop().?;
            },
            else => if (comptime opens_scope.contains(tag)) {
                while (self.current != .root and self.currentScope().node == index) {
                    self.popScope();
                }
            },
        }
    }

    // the tags `enter` pushes a scope for, a switch's case block included
    const opens_scope = std.EnumSet(ast.NodeTag).initMany(&.{
        .function,
        .arrow_function_expression,
        .function_body,
        .block_statement,
        .switch_statement,
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
        .ts_enum_body,
        .ts_module_block,
        .ts_module_declaration,
        .class,
        .static_block,
        .dialect,
    });

    fn hasParameterExpression(tree: *const ast.Tree, params: ast.NodeIndex) bool {
        if (params == .null) return false;
        const data = tree.data(params);
        if (data != .formal_parameters) return false;
        return static_semantics.findParameterExpression(tree, data.formal_parameters) != null;
    }

    inline fn currentScope(self: *const ScopeTracker) Scope {
        std.debug.assert(self.current != .none);
        return self.get(self.current);
    }

    inline fn currentMut(self: *ScopeTracker) *Scope {
        std.debug.assert(self.current != .none);
        return &self.scopes.items[@backingInt(self.current)];
    }

    /// The scope with the given id.
    pub inline fn get(self: *const ScopeTracker, id: ScopeId) Scope {
        return self.toScopeTree().get(id);
    }

    /// Iterates from `start` up to the root.
    pub fn ancestors(self: *const ScopeTracker, start: ScopeId) ScopeTree.AncestorIterator {
        return self.toScopeTree().ancestors(start);
    }

    pub inline fn toScopeTree(self: *const ScopeTracker) ScopeTree {
        return .{ .list = self.scopes.items };
    }
};
