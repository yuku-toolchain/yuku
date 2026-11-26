const std = @import("std");
const ast = @import("../ast.zig");
const static_semantics = @import("spec").static_semantics;

const Allocator = std.mem.Allocator;

/// Identifier for a `Scope`. `.root` is the global scope and `.none` means absent.
pub const ScopeId = enum(u32) { root = 0, none = std.math.maxInt(u32), _ };

pub const module_scope: ScopeId = @fromBackingInt(1);

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
        return self.list[@backingInt(id)];
    }

    /// Iterates from `start` up to the root.
    pub inline fn ancestors(self: ScopeTree, start: ScopeId) AncestorIterator {
        return .{ .list = self.list, .current = start };
    }

    /// Yields scope ids from a starting scope up to the root.
    pub const AncestorIterator = struct {
        list: []const Scope,
        current: ScopeId,

        /// The next scope id, or `null` past the root.
        pub inline fn next(self: *AncestorIterator) ?ScopeId {
            const id = self.current;
            if (id == .none) return null;
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

    const opens_block = std.EnumSet(ast.NodeTag).initMany(&.{
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
    });

    const opens_scope = opens_block.unionWith(.initMany(&.{
        .function,
        .arrow_function_expression,
        .function_body,
        .block_statement,
        .switch_statement,
        .ts_module_block,
        .ts_module_declaration,
        .class,
        .static_block,
    }));

    pub fn init(tree: *ast.Tree) Allocator.Error!ScopeTracker {
        const alloc = tree.allocator();
        var self = ScopeTracker{ .tree = tree, .allocator = alloc };
        try self.scopes.ensureTotalCapacity(alloc, @max(16, tree.nodes.len / 16));
        self.scopes.appendAssumeCapacity(.{
            .node = tree.root,
            .parent = .none,
            .hoist_target = .root,
            .kind = .global,
            .flags = .{},
        });
        if (tree.source_type == .module) try self.pushScope(.module, tree.root, true);
        return self;
    }

    fn pushScope(
        self: *ScopeTracker,
        kind: Scope.Kind,
        node: ast.NodeIndex,
        strict: bool,
    ) Allocator.Error!void {
        std.debug.assert(kind == .module or opens_scope.contains(self.tree.data(node)));
        const id: ScopeId = @fromBackingInt(@intCast(self.scopes.items.len));
        const parent = self.get(self.current);
        try self.scopes.append(self.allocator, .{
            .node = node,
            .parent = self.current,
            .hoist_target = if (kind.isHoistTarget()) id else parent.hoist_target,
            .kind = kind,
            .flags = .{ .strict = strict or parent.flags.strict },
        });
        self.current = id;
    }

    pub inline fn enter(
        self: *ScopeTracker,
        comptime tag: ast.NodeTag,
        index: ast.NodeIndex,
        parent: ast.NodeIndex,
        data: ast.NodeData,
    ) Allocator.Error!void {
        switch (tag) {
            .directive => if (isUseStrict(self.tree, data)) {
                self.scopes.items[@backingInt(self.current)].flags.strict = true;
            },
            .function => {
                const func = data.function;
                const strict = self.isStrictFunction(func.body);
                const declaration = func.type == .function_declaration or
                    func.type == .ts_declare_function;
                if (!declaration and func.id != .null) {
                    try self.pushScope(.expression_name, index, strict);
                }
                // a function an if clause holds sits alone in a block of its own (B.3.3)
                // https://tc39.es/ecma262/#sec-functiondeclarations-in-ifstatement-statement-clauses
                if (declaration and !self.tree.isTs() and parent != .null and
                    self.tree.data(parent) == .if_statement)
                {
                    try self.pushScope(.block, index, false);
                }
                try self.pushScope(.function, index, strict);
            },
            .arrow_function_expression => {
                const body = data.arrow_function_expression.body;
                try self.pushScope(.function, index, self.isStrictFunction(body));
            },
            .function_body => {
                if (parent == .null) return;
                const params = switch (self.tree.data(parent)) {
                    inline .function, .arrow_function_expression => |f| f.params,
                    else => return,
                };
                const enclosing = self.current;
                try self.pushScope(.function_body, index, false);
                // a separate var environment exists only with parameter expressions (10.2.11)
                if (!static_semantics.containsExpression(self.tree, params)) {
                    const hoist_target = self.get(enclosing).hoist_target;
                    self.scopes.items[@backingInt(self.current)].hoist_target = hoist_target;
                }
            },
            .block_statement => {
                // a catch body shares the parameter scope so 14.15.1 conflicts surface,
                // unless a parameter expression runs before the body exists (14.15.2)
                const shares_catch_scope = switch (self.tree.data(self.get(self.current).node)) {
                    .catch_clause => |clause| clause.body == index and
                        !static_semantics.containsExpression(self.tree, clause.param),
                    else => false,
                };
                if (!shares_catch_scope) try self.pushScope(.block, index, false);
            },
            .switch_case => {
                // one case-block scope per switch, entered after the discriminant (14.12.4)
                if (parent == .null or self.get(self.current).node == parent) return;
                try self.pushScope(.block, parent, false);
            },
            .ts_module_block => try self.pushScope(.ts_module, index, false),
            // a scope per qualifier of `namespace A.B.C {}`
            .ts_module_declaration => {
                var name = data.ts_module_declaration.id;
                while (self.tree.data(name) == .ts_qualified_name) {
                    try self.pushScope(.ts_module, index, false);
                    name = self.tree.data(name).ts_qualified_name.left;
                }
            },
            .class => {
                const cls = data.class;
                // class code is always strict (15.7.14)
                if (cls.type != .class_declaration and cls.id != .null) {
                    try self.pushScope(.expression_name, index, true);
                }
                try self.pushScope(.class, index, true);
            },
            .static_block => try self.pushScope(.static_block, index, false),
            .decorator => {
                try self.decorator_saved.append(self.allocator, self.current);
                const on_class = parent != .null and self.tree.data(parent) == .class;
                self.current = self.decoratorScope(on_class);
            },
            else => if (comptime opens_block.contains(tag)) {
                try self.pushScope(.block, index, false);
            },
        }
    }

    pub inline fn exit(
        self: *ScopeTracker,
        comptime tag: ast.NodeTag,
        index: ast.NodeIndex,
    ) void {
        if (tag == .decorator) {
            self.current = self.decorator_saved.pop().?;
        } else if (comptime opens_scope.contains(tag)) {
            while (self.current != .root and self.get(self.current).node == index) {
                self.current = self.get(self.current).parent;
            }
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

    fn isStrictFunction(self: *const ScopeTracker, body_index: ast.NodeIndex) bool {
        if (self.get(self.current).flags.strict) return true;
        if (body_index == .null) return false;
        const body = self.tree.data(body_index);
        if (body != .function_body) return false;
        for (self.tree.extra(body.function_body.body)) |statement| {
            const data = self.tree.data(statement);
            if (data != .directive) break;
            if (isUseStrict(self.tree, data)) return true;
        }
        return false;
    }

    fn isUseStrict(tree: *const ast.Tree, directive: ast.NodeData) bool {
        return std.mem.eql(u8, tree.string(directive.directive.value), "use strict");
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
