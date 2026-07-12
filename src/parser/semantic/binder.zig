const std = @import("std");
const ast = @import("../ast.zig");
const sc = @import("scope.zig");
const String = ast.String;

const Allocator = std.mem.Allocator;

/// Identifier for a `Symbol`. `.none` means absent.
pub const SymbolId = enum(u32) { none = std.math.maxInt(u32), _ };

/// Identifier for a `Reference`. `.none` means absent.
pub const ReferenceId = enum(u32) { none = std.math.maxInt(u32), _ };

const ScopeMap = std.StringHashMapUnmanaged(SymbolId);

/// A `(start, len)` window into a backing slice.
pub const Range = struct { start: u32, len: u32 };

/// A declared binding. Declarations that legally share a name merge
/// into one symbol (`var` redeclaration, TS function overloads,
/// `class` + `interface` and other declaration merging), so a symbol
/// can have several declaration sites, and `Semantic.decls` lists
/// them all. `flags` describes which spaces (value, type, namespace) and
/// modifiers it occupies.
pub const Symbol = struct {
    name: String,
    flags: Flags,
    /// The scope the symbol is declared in. For a hoisting `var`,
    /// this is the hoist target it lands in, not the block it is
    /// written in.
    scope: sc.ScopeId,
    // window into `Semantic.decl_nodes`, expanded by `Semantic.decls`
    decls: Range,

    pub const Flags = packed struct(u32) {
        function_scoped_var: bool = false,
        block_scoped_var: bool = false,
        function: bool = false,
        class: bool = false,
        regular_enum: bool = false,
        const_enum: bool = false,
        value_module: bool = false,
        interface: bool = false,
        type_alias: bool = false,
        type_parameter: bool = false,
        namespace_module: bool = false,
        import: bool = false,
        type_import: bool = false,
        const_var: bool = false,
        ambient: bool = false,
        parameter: bool = false,
        catch_var: bool = false,
        exported: bool = false,
        is_default: bool = false,
        _: u13 = 0,

        /// True when `a` and `b` have at least one flag in common.
        pub inline fn intersects(a: Flags, b: Flags) bool {
            return @as(u32, @bitCast(a)) & @as(u32, @bitCast(b)) != 0;
        }

        /// The union of two flag sets. Used when merging two compatible
        /// declarations into a single symbol.
        pub inline fn merge(a: Flags, b: Flags) Flags {
            return @bitCast(@as(u32, @bitCast(a)) | @as(u32, @bitCast(b)));
        }

        /// True for a `var` that hoists past intermediate blocks.
        /// Parameters and catch variables are function-scoped but do
        /// not hoist, so they return false here.
        pub inline fn isHoistingVar(self: Flags) bool {
            return self.function_scoped_var and !self.parameter and !self.catch_var;
        }

        /// True for a JavaScript value-space binding visible at
        /// runtime, meaning `var`/`let`/`const`, function, class,
        /// enum, or an instantiated namespace.
        pub inline fn inValueSpace(self: Flags) bool {
            return self.intersects(value_space);
        }

        /// True for a TypeScript type-space binding, meaning
        /// interface, type alias, type parameter, plus class and
        /// enum, which exist in both spaces.
        pub inline fn inTypeSpace(self: Flags) bool {
            return self.intersects(type_space);
        }

        /// True for declarations a hoisting `var` is forbidden to
        /// pass through, meaning block-scoped bindings (`let`,
        /// `const`), classes, and functions.
        pub inline fn isBlockScopedLike(self: Flags) bool {
            return self.intersects(block_scoped_like);
        }

        /// Human-readable category for diagnostics.
        pub fn toString(self: Flags) []const u8 {
            if (self.function) return "function";
            if (self.class) return "class";
            if (self.regular_enum or self.const_enum) return "enum";
            if (self.value_module or self.namespace_module) return "namespace";
            if (self.interface) return "interface";
            if (self.type_alias) return "type alias";
            if (self.type_import) return "type import";
            if (self.import) return "import";
            if (self.parameter) return "parameter";
            if (self.catch_var) return "catch parameter";
            if (self.type_parameter) return "type parameter";
            return "variable";
        }
    };

    // composite flag sets, mirrored into the JS decoder's SymbolFlags

    /// `var` / `let` / `const`, parameters and catch bindings included.
    pub const variable: Flags = .{ .function_scoped_var = true, .block_scoped_var = true };

    /// Any import binding, value (`import x`) or type-only (`import type x`).
    pub const any_import: Flags = .{ .import = true, .type_import = true };

    /// Declarations that live in JS value space (visible at runtime).
    pub const value_space: Flags = .{
        .function_scoped_var = true,
        .block_scoped_var = true,
        .function = true,
        .class = true,
        .regular_enum = true,
        .const_enum = true,
        .value_module = true,
    };

    /// Declarations that live in TS type space.
    pub const type_space: Flags = .{
        .class = true,
        .regular_enum = true,
        .const_enum = true,
        .interface = true,
        .type_alias = true,
        .type_parameter = true,
    };

    // names a hoisted `var` cannot pass through
    const block_scoped_like: Flags = .{
        .block_scoped_var = true,
        .class = true,
        .function = true,
    };

    /// Per-declaration redeclaration excludes. A new declaration with
    /// `Excludes.X` conflicts with any existing flag also in
    /// `Excludes.X`. Otherwise both declarations merge into one symbol.
    pub const Excludes = struct {
        pub const block_var: Flags = value_space;

        pub const function_var: Flags = blk: {
            var f = value_space;
            f.function_scoped_var = false;
            f.function = false;
            break :blk f;
        };

        /// Function in a hoist scope (function/global/static_block). TS
        /// allows function overloads, sloppy JS allows merge with `var`.
        /// The `block_var` excludes are used instead at
        /// lexical scopes (block/module).
        pub const function: Flags = blk: {
            var f = value_space;
            f.function_scoped_var = false;
            f.function = false;
            f.value_module = false;
            f.class = false;
            break :blk f;
        };

        pub const class: Flags = blk: {
            var f = value_space.merge(type_space);
            f.value_module = false;
            f.interface = false;
            break :blk f;
        };

        pub const interface: Flags = blk: {
            var f = type_space;
            f.interface = false;
            f.class = false;
            break :blk f;
        };

        pub const type_alias: Flags = type_space;

        pub const regular_enum: Flags = blk: {
            var f = value_space.merge(type_space);
            f.regular_enum = false;
            f.value_module = false;
            break :blk f;
        };

        pub const const_enum: Flags = blk: {
            var f = value_space.merge(type_space);
            f.const_enum = false;
            break :blk f;
        };

        pub const value_module: Flags = blk: {
            var f = value_space;
            f.function = false;
            f.class = false;
            f.regular_enum = false;
            f.value_module = false;
            break :blk f;
        };

        pub const namespace_module: Flags = .{};

        pub const import_binding: Flags = .{ .import = true, .type_import = true };

        pub const parameter: Flags = blk: {
            var f = value_space;
            f.function_scoped_var = false;
            break :blk f;
        };

        pub const catch_param: Flags = value_space;

        // type parameters merge (multiple `infer T` in one conditional
        // unify). duplicate explicit `<T, T>` is caught structurally
        pub const type_parameter: Flags = blk: {
            var f = type_space;
            f.type_parameter = false;
            break :blk f;
        };
    };
};

/// A use of a name. One is recorded for every `identifier_reference`
/// in the source, for JSX component tag names (`<Foo>`), and for TS
/// type-predicate parameters (`x is T`). Declaration sites live on
/// the symbol itself.
pub const Reference = struct {
    name: String,
    /// The scope the reference appears in.
    scope: sc.ScopeId,
    /// The referencing node.
    node: ast.NodeIndex,
    /// The declaring symbol this reference resolves to. `.none` for
    /// names with no local binding (globals, undeclared names).
    symbol: SymbolId = .none,
    flags: Flags = .{},

    pub const Flags = packed struct(u8) {
        /// True for a type-position use (annotations, `extends`,
        /// `implements`, type arguments). False for a runtime use.
        type_position: bool = false,
        /// True when this reference (re)assigns its binding, meaning
        /// the target of an assignment, the operand of `++`/`--`, the
        /// iteration variable of for-in/for-of, or a destructuring
        /// assignment leaf. Initializers in declarations are not
        /// references, so an initialized but never reassigned binding
        /// has no write.
        write: bool = false,
        _: u6 = 0,
    };
};

/// The complete semantic model of a tree, with every scope, symbol,
/// and reference fully resolved and cross-indexed. Backed by the
/// tree's arena and valid for the lifetime of the tree.
pub const Semantic = struct {
    /// Every scope, indexed by `ScopeId`.
    scopes: sc.ScopeTree,
    /// Every symbol, in declaration order, indexed by `SymbolId`.
    symbols: []const Symbol,
    /// Every reference, in source order, indexed by `ReferenceId`.
    references: []const Reference,

    // backing storage for the queries below
    decl_nodes: []const ast.NodeIndex,
    use_ids: []const ReferenceId,
    use_ranges: []const Range,
    scope_maps: []const ScopeMap,
    hoisting_variables: []const ScopeMap,
    node_scopes: []const sc.ScopeId,
    node_parents: []const ast.NodeIndex,
    node_symbols: []const SymbolId,
    node_references: []const ReferenceId,

    /// The symbol with the given id.
    pub inline fn symbol(self: Semantic, id: SymbolId) Symbol {
        std.debug.assert(id != .none);
        std.debug.assert(@intFromEnum(id) < self.symbols.len);
        return self.symbols[@intFromEnum(id)];
    }

    /// The reference with the given id.
    pub inline fn reference(self: Semantic, id: ReferenceId) Reference {
        std.debug.assert(id != .none);
        std.debug.assert(@intFromEnum(id) < self.references.len);
        return self.references[@intFromEnum(id)];
    }

    /// The scope with the given id.
    pub inline fn scope(self: Semantic, id: sc.ScopeId) sc.Scope {
        return self.scopes.get(id);
    }

    /// The symbol declared at `node`, or the symbol the reference at
    /// `node` resolves to. `null` when the node neither declares nor
    /// references a binding.
    pub fn symbolOf(self: Semantic, node: ast.NodeIndex) ?SymbolId {
        std.debug.assert(node != .null);
        std.debug.assert(@intFromEnum(node) < self.node_symbols.len);
        const declared = self.node_symbols[@intFromEnum(node)];
        if (declared != .none) return declared;
        const ref = self.node_references[@intFromEnum(node)];
        if (ref == .none) return null;
        const resolved = self.reference(ref).symbol;
        return if (resolved != .none) resolved else null;
    }

    /// The reference recorded at `node`, or `null` when the node is
    /// not a reference site.
    pub fn referenceOf(self: Semantic, node: ast.NodeIndex) ?ReferenceId {
        std.debug.assert(node != .null);
        std.debug.assert(@intFromEnum(node) < self.node_references.len);
        const id = self.node_references[@intFromEnum(node)];
        return if (id != .none) id else null;
    }

    /// The innermost lexical scope containing `node`. A scope-creating
    /// node (function, block, class, ...) maps to the scope it
    /// creates, and `scope(scopeOf(node)).parent` is the scope
    /// enclosing it.
    pub inline fn scopeOf(self: Semantic, node: ast.NodeIndex) sc.ScopeId {
        std.debug.assert(node != .null);
        std.debug.assert(@intFromEnum(node) < self.node_scopes.len);
        return self.node_scopes[@intFromEnum(node)];
    }

    /// The structural parent of `node`, or `null` at the root.
    pub fn parentOf(self: Semantic, node: ast.NodeIndex) ?ast.NodeIndex {
        std.debug.assert(node != .null);
        std.debug.assert(@intFromEnum(node) < self.node_parents.len);
        const parent = self.node_parents[@intFromEnum(node)];
        return if (parent != .null) parent else null;
    }

    /// Walks from `node` up to the root, yielding `node` first.
    pub fn ancestors(self: Semantic, node: ast.NodeIndex) AncestorIterator {
        std.debug.assert(node == .null or @intFromEnum(node) < self.node_parents.len);
        return .{ .node_parents = self.node_parents, .current = node };
    }

    /// The `binding_identifier` node of every declaration of `id`, in
    /// source order. Usually a single element. Symbols whose
    /// declarations merge (`var` redeclaration, TS overloads, `class`
    /// + `interface`) have one entry per declaration. `parentOf`
    /// reaches the enclosing declarator or declaration from there.
    pub fn decls(self: Semantic, id: SymbolId) []const ast.NodeIndex {
        const range = self.symbol(id).decls;
        std.debug.assert(@as(usize, range.start) + range.len <= self.decl_nodes.len);
        return self.decl_nodes[range.start..][0..range.len];
    }

    /// Every use site of `id`, in source order. Declaration sites
    /// are not uses, those are in `decls`.
    pub fn uses(self: Semantic, id: SymbolId) []const ReferenceId {
        std.debug.assert(id != .none);
        std.debug.assert(@intFromEnum(id) < self.use_ranges.len);
        const range = self.use_ranges[@intFromEnum(id)];
        return self.use_ids[range.start..][0..range.len];
    }

    /// The binding of `name` at `scope`, including a hoisting `var`
    /// passing through on its way to its hoist target. Does not walk
    /// the scope chain, see `lookup`.
    pub fn binding(self: Semantic, scope_id: sc.ScopeId, name: []const u8) ?SymbolId {
        std.debug.assert(scope_id != .none);
        std.debug.assert(@intFromEnum(scope_id) < self.scope_maps.len);
        return self.scope_maps[@intFromEnum(scope_id)].get(name) orelse
            self.hoisting_variables[@intFromEnum(scope_id)].get(name);
    }

    /// Every symbol declared directly in `scope`. A hoisting `var`
    /// appears in its hoist target's scope, not in the blocks it
    /// passes through.
    pub fn bindings(self: Semantic, scope_id: sc.ScopeId) BindingIterator {
        std.debug.assert(scope_id != .none);
        std.debug.assert(@intFromEnum(scope_id) < self.scope_maps.len);
        return .{ .inner = self.scope_maps[@intFromEnum(scope_id)].valueIterator() };
    }

    /// The nearest binding of `name` visible from `scope`, walking up
    /// the scope chain the way name resolution does at runtime.
    pub fn lookup(self: Semantic, scope_id: sc.ScopeId, name: []const u8) ?SymbolId {
        var it = self.scopes.ancestors(scope_id);
        while (it.next()) |ancestor| {
            if (self.scope_maps[@intFromEnum(ancestor)].get(name)) |id| return id;
        }
        return null;
    }

    /// Iterates every `(id, scope)` pair in creation order.
    pub fn iterScopes(self: Semantic) ScopeIterator {
        return .{ .list = self.scopes.list };
    }

    /// Iterates every `(id, symbol)` pair in declaration order.
    pub fn iterSymbols(self: Semantic) SymbolIterator {
        return .{ .symbols = self.symbols };
    }

    /// Iterates every `(id, reference)` pair in source order.
    pub fn iterReferences(self: Semantic) ReferenceIterator {
        return .{ .references = self.references };
    }

    /// A `(id, scope)` pair yielded by `iterScopes`.
    pub const ScopeEntry = struct { id: sc.ScopeId, scope: sc.Scope };

    /// A `(id, symbol)` pair yielded by `iterSymbols`.
    pub const SymbolEntry = struct { id: SymbolId, symbol: Symbol };

    /// A `(id, reference)` pair yielded by `iterReferences`.
    pub const ReferenceEntry = struct { id: ReferenceId, reference: Reference };

    /// Yields every `(id, scope)` pair in creation order.
    pub const ScopeIterator = struct {
        list: []const sc.Scope,
        index: u32 = 0,

        pub fn next(self: *ScopeIterator) ?ScopeEntry {
            if (self.index >= self.list.len) return null;
            const i = self.index;
            self.index += 1;
            return .{ .id = @enumFromInt(i), .scope = self.list[i] };
        }
    };

    /// Yields each node index from a starting node up to the root.
    pub const AncestorIterator = struct {
        node_parents: []const ast.NodeIndex,
        current: ast.NodeIndex,

        /// The next node up the chain, or `null` past the root.
        pub fn next(self: *AncestorIterator) ?ast.NodeIndex {
            const node = self.current;
            if (node == .null) return null;
            std.debug.assert(@intFromEnum(node) < self.node_parents.len);
            self.current = self.node_parents[@intFromEnum(node)];
            return node;
        }
    };

    /// Yields each symbol id declared directly in a scope.
    pub const BindingIterator = struct {
        inner: ScopeMap.ValueIterator,

        /// The next symbol id, or `null` when done.
        pub fn next(self: *BindingIterator) ?SymbolId {
            const ptr = self.inner.next() orelse return null;
            return ptr.*;
        }
    };

    /// Yields every `(id, symbol)` pair in declaration order.
    pub const SymbolIterator = struct {
        symbols: []const Symbol,
        index: u32 = 0,

        pub fn next(self: *SymbolIterator) ?SymbolEntry {
            if (self.index >= self.symbols.len) return null;
            const i = self.index;
            self.index += 1;
            return .{ .id = @enumFromInt(i), .symbol = self.symbols[i] };
        }
    };

    /// Yields every `(id, reference)` pair in source order.
    pub const ReferenceIterator = struct {
        references: []const Reference,
        index: u32 = 0,

        pub fn next(self: *ReferenceIterator) ?ReferenceEntry {
            if (self.index >= self.references.len) return null;
            const i = self.index;
            self.index += 1;
            return .{ .id = @enumFromInt(i), .reference = self.references[i] };
        }
    };
};

const PrehashCtx = struct {
    h: u64,
    pub fn hash(self: @This(), _: []const u8) u64 {
        return self.h;
    }
    pub fn eql(_: @This(), a: []const u8, b: []const u8) bool {
        return std.mem.eql(u8, a, b);
    }
};

/// Collects symbols and references during the AST walk and finalizes
/// them into a `Semantic`.
pub const SymbolTracker = struct {
    tree: *const ast.Tree,
    allocator: Allocator,
    symbols: std.ArrayList(Symbol) = .empty,
    references: std.ArrayList(Reference) = .empty,
    decl_pairs: std.ArrayList(DeclPair) = .empty,
    // first declarator of each symbol, parallel to `symbols`
    first_decls: std.ArrayList(ast.NodeIndex) = .empty,
    scope_maps: std.ArrayList(ScopeMap) = .empty,
    hoisting_variables: std.ArrayList(ScopeMap) = .empty,

    /// What the next `binding_identifier` will declare, meaning its
    /// flags, its redeclaration excludes, and the scope it lands in.
    /// Valid inside an enter hook on a `binding_identifier`.
    pending: PendingBinding = .{},
    /// Whether the next `binding_identifier` is the directly-exported
    /// name of an `export` declaration.
    export_state: ExportState = .none,
    // true inside an ambient (`declare`) context
    ambient: bool = false,

    saved_stack: std.ArrayList(SavedContext) = .empty,

    pub const ExportState = enum { none, named, default };

    /// The declaration context for the next `binding_identifier`.
    pub const PendingBinding = struct {
        flags: Symbol.Flags = .{},
        excludes: Symbol.Flags = .{},
        scope: sc.ScopeId = .root,
    };

    const DeclPair = struct { sid: SymbolId, node: ast.NodeIndex };

    // snapshot taken before a declaration node rewrites the context,
    // restored on exit
    const SavedContext = struct {
        pending: PendingBinding,
        export_state: ExportState,
        ambient: bool,
    };

    pub fn init(tree: *ast.Tree) Allocator.Error!SymbolTracker {
        std.debug.assert(tree.root != .null);

        const alloc = tree.allocator();
        var self = SymbolTracker{
            .tree = tree,
            .allocator = alloc,
            // a declaration file is ambient in its entirety
            .ambient = tree.lang == .dts,
        };

        const nodes: u32 = @intCast(tree.nodes.len);
        try self.symbols.ensureTotalCapacity(alloc, @max(16, nodes / 12));
        try self.first_decls.ensureTotalCapacity(alloc, @max(16, nodes / 12));
        try self.references.ensureTotalCapacity(alloc, @max(16, nodes / 4));
        try self.decl_pairs.ensureTotalCapacity(alloc, @max(16, nodes / 12));
        try self.scope_maps.ensureTotalCapacity(alloc, @max(8, nodes / 16));
        try self.hoisting_variables.ensureTotalCapacity(alloc, @max(8, nodes / 16));
        try self.saved_stack.ensureTotalCapacity(alloc, 32);
        return self;
    }

    /// Records the binding context for the next `binding_identifier`.
    /// Called for every node on enter so parent declaration nodes can
    /// configure the pending binding before the child identifier fires.
    pub fn setBindingContext(
        self: *SymbolTracker,
        data: ast.NodeData,
        scope: *const sc.ScopeTracker,
    ) Allocator.Error!void {
        switch (data) {
            .export_named_declaration => |decl| {
                // the declaration form always exports the binding it
                // declares, even when `export_kind` is `.type`. the pure
                // re-export form has no inner binding and stays dependent
                // on `export_kind` so a type-only re-export does not tag
                // the value side as exported.
                if (decl.declaration != .null or decl.export_kind != .type) {
                    self.export_state = .named;
                }
            },
            .export_default_declaration => self.export_state = .default,

            .variable_declaration => |decl| {
                try self.pushSavedContext();
                switch (decl.kind) {
                    .@"var" => self.pending = .{
                        .flags = .{
                            .function_scoped_var = true,
                            .ambient = decl.declare or self.ambient,
                        },
                        .excludes = Symbol.Excludes.function_var,
                        .scope = scope.hoistTarget(),
                    },
                    .@"const", .using, .await_using => self.pending = .{
                        .flags = .{
                            .block_scoped_var = true,
                            .const_var = true,
                            .ambient = decl.declare or self.ambient,
                        },
                        .excludes = Symbol.Excludes.block_var,
                        .scope = scope.current,
                    },
                    .let => self.pending = .{
                        .flags = .{
                            .block_scoped_var = true,
                            .ambient = decl.declare or self.ambient,
                        },
                        .excludes = Symbol.Excludes.block_var,
                        .scope = scope.current,
                    },
                }
            },

            .function => |func| {
                try self.pushSavedContext();
                const ambient = func.declare or
                    func.type == .ts_declare_function or
                    func.type == .ts_empty_body_function_expression or
                    self.ambient;
                const is_decl = func.type == .function_declaration or
                    func.type == .ts_declare_function;
                const target = if (is_decl) scope.currentScope().parent else exprNameScope(scope);

                // ts overloads, sloppy js annex b 3.2, and global merge
                // with var. lexical scopes (block/module) are not.
                const k = scope.get(target).kind;
                const allow_overload = self.tree.isTs() or
                    k == .function or
                    k == .global or
                    k == .static_block;

                self.pending = .{
                    .flags = .{ .function = true, .ambient = ambient },
                    .excludes = if (allow_overload)
                        Symbol.Excludes.function
                    else
                        Symbol.Excludes.block_var,
                    .scope = target,
                };

                // expression names are local
                if (!is_decl) self.export_state = .none;
            },

            .class => |cls| {
                try self.pushSavedContext();
                const is_decl = cls.type == .class_declaration;
                self.pending = .{
                    .flags = .{
                        .class = true,
                        .ambient = cls.declare or self.ambient,
                    },
                    .excludes = Symbol.Excludes.class,
                    .scope = if (is_decl) scope.currentScope().parent else exprNameScope(scope),
                };
                // expression names are local
                if (!is_decl) self.export_state = .none;
            },

            .formal_parameters => {
                try self.pushSavedContext();
                self.pending = .{
                    .flags = .{
                        .function_scoped_var = true,
                        .parameter = true,
                        .ambient = self.pending.flags.ambient,
                    },
                    .excludes = Symbol.Excludes.parameter,
                    .scope = scope.current,
                };
                self.export_state = .none;
            },

            // members are not the exported binding
            .class_body, .ts_module_block => self.export_state = .none,

            inline .import_declaration, .ts_import_equals_declaration => |decl| {
                try self.pushSavedContext();
                self.pending = .{
                    .flags = if (decl.import_kind == .type)
                        .{ .type_import = true }
                    else
                        .{ .import = true },
                    .excludes = Symbol.Excludes.import_binding,
                    .scope = scope.current,
                };
            },

            .import_specifier => |spec| {
                try self.pushSavedContext();
                if (spec.import_kind == .type) {
                    self.pending.flags = .{ .type_import = true };
                    self.pending.excludes = Symbol.Excludes.import_binding;
                }
            },

            .catch_clause => {
                try self.pushSavedContext();
                self.pending = .{
                    .flags = .{ .function_scoped_var = true, .catch_var = true },
                    .excludes = Symbol.Excludes.catch_param,
                    .scope = scope.current,
                };
            },

            // the id binds in the surrounding scope. type-parameter
            // and body bindings live in a scope pushed by the tracker.
            .ts_interface_declaration => |decl| {
                try self.pushSavedContext();
                self.pending = .{
                    .flags = .{
                        .interface = true,
                        .ambient = decl.declare or self.ambient,
                    },
                    .excludes = Symbol.Excludes.interface,
                    .scope = scope.currentScope().parent,
                };
            },

            .ts_type_alias_declaration => |decl| {
                try self.pushSavedContext();
                self.pending = .{
                    .flags = .{
                        .type_alias = true,
                        .ambient = decl.declare or self.ambient,
                    },
                    .excludes = Symbol.Excludes.type_alias,
                    .scope = scope.currentScope().parent,
                };
            },

            .ts_enum_declaration => |decl| {
                try self.pushSavedContext();
                self.pending = if (decl.is_const) .{
                    .flags = .{
                        .const_enum = true,
                        .ambient = decl.declare or self.ambient,
                    },
                    .excludes = Symbol.Excludes.const_enum,
                    .scope = scope.current,
                } else .{
                    .flags = .{
                        .regular_enum = true,
                        .ambient = decl.declare or self.ambient,
                    },
                    .excludes = Symbol.Excludes.regular_enum,
                    .scope = scope.current,
                };
            },

            .ts_module_declaration => |decl| {
                try self.pushSavedContext();
                // type-only bodies (interface/type alias/etc.) don't
                // occupy value space
                const instantiated = isNamespaceInstantiated(self.tree, decl.body);
                self.pending = .{
                    .flags = .{
                        .value_module = instantiated,
                        .namespace_module = true,
                        .ambient = decl.declare or self.ambient,
                    },
                    .excludes = if (instantiated)
                        Symbol.Excludes.value_module
                    else
                        Symbol.Excludes.namespace_module,
                    .scope = scope.current,
                };
                // `declare` makes the whole body ambient
                if (decl.declare) self.ambient = true;
            },

            .ts_global_declaration => {
                try self.pushSavedContext();
                self.ambient = true;
            },

            .ts_namespace_export_declaration => {
                try self.pushSavedContext();
                self.pending = .{
                    .flags = .{ .namespace_module = true },
                    .excludes = Symbol.Excludes.namespace_module,
                    .scope = scope.current,
                };
            },

            // ts_type_parameter wraps `<T>` and `infer U`. mapped type
            // keys `[K in T]` are bare binding_identifiers under
            // ts_mapped_type, so the same context applies.
            .ts_type_parameter, .ts_mapped_type => {
                try self.pushSavedContext();
                self.pending = .{
                    .flags = .{ .type_parameter = true },
                    .excludes = Symbol.Excludes.type_parameter,
                    .scope = scope.current,
                };
                self.export_state = .none;
            },

            else => {},
        }
    }

    inline fn pushSavedContext(self: *SymbolTracker) Allocator.Error!void {
        try self.saved_stack.append(self.allocator, .{
            .pending = self.pending,
            .export_state = self.export_state,
            .ambient = self.ambient,
        });
    }

    /// Per-node facts computed by the walker for `declareBindings`.
    pub const RefContext = struct {
        /// inside a TS type-only subtree
        in_type_position: bool,
        /// the node is an identifier in assignment-target position
        is_write: bool,
    };

    /// Materializes the pending binding context into a symbol (for
    /// `binding_identifier`) or records a reference (for
    /// `identifier_reference`). Called for every node after its enter
    /// hook, before its children.
    pub fn declareBindings(
        self: *SymbolTracker,
        index: ast.NodeIndex,
        data: ast.NodeData,
        scope: *const sc.ScopeTracker,
        ref_ctx: RefContext,
    ) Allocator.Error!void {
        if (self.scope_maps.items.len < scope.scopes.items.len)
            try self.syncScopeMaps(scope.scopes.items.len);

        switch (data) {
            .binding_identifier => |id| {
                // type-position identifiers are parameter labels.
                // only type parameters are real declarations there
                if (ref_ctx.in_type_position and !self.pending.flags.type_parameter) return;

                const sym_id = try self.declare(id.name, index);

                // register the hoisting var in each block it passes
                // through so block-scoped redeclarations see it
                if (self.pending.flags.isHoistingVar()) {
                    var iter = scope.ancestors(scope.current);
                    while (iter.next()) |s| {
                        if (s == self.pending.scope) break;
                        const table = &self.hoisting_variables.items[@intFromEnum(s)];
                        const gop = try table.getOrPut(self.allocator, self.tree.string(id.name));
                        if (!gop.found_existing) gop.value_ptr.* = sym_id;
                    }
                }
            },
            .identifier_reference => |id| {
                _ = try self.addReference(id.name, scope.current, index, .{
                    .type_position = ref_ctx.in_type_position,
                    .write = ref_ctx.is_write,
                });
            },
            // `param is T` parses the param as an `identifier_name`, but
            // it semantically references the function's parameter binding.
            // synthesize the reference so renamers update both together.
            .ts_type_predicate => |pred| {
                if (pred.parameter_name == .null) return;
                const pname = self.tree.data(pred.parameter_name);
                if (pname != .identifier_name) return;
                _ = try self.addReference(
                    pname.identifier_name.name,
                    scope.current,
                    pred.parameter_name,
                    .{ .type_position = ref_ctx.in_type_position },
                );
            },
            // the leftmost capitalized `jsx_identifier` of a tag is a js
            // binding reference. lowercase tags (`<div>`) are intrinsic
            // element names and the rest of the chain is property syntax.
            inline .jsx_opening_element, .jsx_closing_element => |el| {
                if (jsxTagRoot(self.tree, el.name)) |root_idx| {
                    const id = self.tree.data(root_idx).jsx_identifier;
                    const text = self.tree.string(id.name);
                    if (text.len > 0 and text[0] >= 'A' and text[0] <= 'Z') {
                        _ = try self.addReference(id.name, scope.current, root_idx, .{});
                    }
                }
            },
            else => {},
        }
    }

    /// Restores the context to what it was before the matching enter.
    /// Safe for any node. Only nodes that pushed in `setBindingContext`
    /// actually pop.
    pub fn exit(self: *SymbolTracker, data: ast.NodeData) void {
        switch (data) {
            .formal_parameters => {
                if (self.saved_stack.pop()) |saved| {
                    self.pending = saved.pending;
                    self.ambient = saved.ambient;
                }
            },

            .variable_declaration,
            .function,
            .class,
            .import_declaration,
            .ts_import_equals_declaration,
            .import_specifier,
            .catch_clause,
            .ts_interface_declaration,
            .ts_type_alias_declaration,
            .ts_enum_declaration,
            .ts_module_declaration,
            .ts_global_declaration,
            .ts_namespace_export_declaration,
            .ts_type_parameter,
            .ts_mapped_type,
            => {
                if (self.saved_stack.pop()) |saved| {
                    self.pending = saved.pending;
                    self.export_state = saved.export_state;
                    self.ambient = saved.ambient;
                }
            },

            .export_named_declaration, .export_default_declaration => self.export_state = .none,

            else => {},
        }
    }

    /// Declares the pending binding for `name` at `node`. If the name
    /// is already bound in the target scope with non-conflicting flags,
    /// merges into the existing symbol. If the name conflicts, keeps
    /// the existing symbol unchanged (the caller emits a diagnostic).
    /// Otherwise creates a fresh symbol. `node` is always recorded as
    /// a declarator.
    pub fn declare(self: *SymbolTracker, name: String, node: ast.NodeIndex) Allocator.Error!SymbolId {
        const target = self.pending.scope;
        std.debug.assert(target != .none);
        std.debug.assert(@intFromEnum(target) < self.scope_maps.items.len);
        std.debug.assert(node != .null);

        const name_str = self.tree.string(name);
        const target_idx = @intFromEnum(target);

        const id = if (self.scope_maps.items[target_idx].get(name_str) orelse
            self.hoisting_variables.items[target_idx].get(name_str)) |existing|
        sid: {
            const sym = &self.symbols.items[@intFromEnum(existing)];
            if (!sym.flags.intersects(self.pending.excludes)) {
                var merged = sym.flags.merge(self.pending.flags);
                merged.exported = merged.exported or self.export_state != .none;
                merged.is_default = merged.is_default or self.export_state == .default;
                // an overload implementation emits at runtime, so the
                // merged symbol is non-ambient
                merged.ambient = sym.flags.ambient and self.pending.flags.ambient;
                sym.flags = merged;
            }
            break :sid existing;
        } else sid: {
            std.debug.assert(self.symbols.items.len < std.math.maxInt(u32));
            const new_id: SymbolId = @enumFromInt(@as(u32, @intCast(self.symbols.items.len)));
            var flags = self.pending.flags;
            flags.exported = self.export_state != .none;
            flags.is_default = self.export_state == .default;
            try self.symbols.append(self.allocator, .{
                .name = name,
                .flags = flags,
                .scope = target,
                .decls = .{ .start = 0, .len = 0 },
            });
            try self.first_decls.append(self.allocator, node);
            try self.scope_maps.items[target_idx].put(self.allocator, name_str, new_id);
            break :sid new_id;
        };

        try self.decl_pairs.append(self.allocator, .{ .sid = id, .node = node });
        return id;
    }

    /// Records an identifier reference at `node` in `scope`.
    pub fn addReference(
        self: *SymbolTracker,
        name: String,
        scope: sc.ScopeId,
        node: ast.NodeIndex,
        flags: Reference.Flags,
    ) Allocator.Error!ReferenceId {
        std.debug.assert(scope != .none);
        std.debug.assert(node != .null);
        std.debug.assert(self.references.items.len < std.math.maxInt(u32));

        const id: ReferenceId = @enumFromInt(@as(u32, @intCast(self.references.items.len)));
        try self.references.append(self.allocator, .{
            .name = name,
            .scope = scope,
            .node = node,
            .flags = flags,
        });
        return id;
    }

    /// The symbol with the given id.
    pub inline fn symbol(self: *const SymbolTracker, id: SymbolId) Symbol {
        std.debug.assert(id != .none);
        std.debug.assert(@intFromEnum(id) < self.symbols.items.len);
        return self.symbols.items[@intFromEnum(id)];
    }

    /// The `binding_identifier` node of the earliest declaration of
    /// `id`. Later declarations that merge into the same symbol do
    /// not change it.
    pub fn firstDeclOf(self: *const SymbolTracker, id: SymbolId) ast.NodeIndex {
        std.debug.assert(id != .none);
        std.debug.assert(@intFromEnum(id) < self.first_decls.items.len);
        return self.first_decls.items[@intFromEnum(id)];
    }

    /// The binding of `name` declared directly in `scope`, excluding
    /// hoisting `var`s passing through.
    pub fn ownBinding(self: *const SymbolTracker, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        const idx = @intFromEnum(scope);
        if (idx >= self.scope_maps.items.len) return null;
        return self.scope_maps.items[idx].get(name);
    }

    /// The binding of `name` at `scope`, including a hoisting `var`
    /// passing through on its way to its hoist target.
    pub fn binding(self: *const SymbolTracker, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        if (self.ownBinding(scope, name)) |id| return id;
        const idx = @intFromEnum(scope);
        if (idx < self.hoisting_variables.items.len) {
            return self.hoisting_variables.items[idx].get(name);
        }
        return null;
    }

    fn syncScopeMaps(self: *SymbolTracker, count: usize) Allocator.Error!void {
        try self.scope_maps.ensureTotalCapacity(self.allocator, count);
        while (self.scope_maps.items.len < count) self.scope_maps.appendAssumeCapacity(.empty);
        try self.hoisting_variables.ensureTotalCapacity(self.allocator, count);
        while (self.hoisting_variables.items.len < count) {
            self.hoisting_variables.appendAssumeCapacity(.empty);
        }
    }

    /// Finalizes the tracker into a complete `Semantic`. Builds the
    /// declaration, use, and node indexes and resolves every reference
    /// to its declaring symbol. The result aliases the tracker's
    /// storage and stays valid for the lifetime of the source tree.
    pub fn finalize(
        self: *SymbolTracker,
        scopes: sc.ScopeTree,
        node_scopes: []const sc.ScopeId,
        node_parents: []const ast.NodeIndex,
    ) Allocator.Error!Semantic {
        // the walk must have ended balanced
        std.debug.assert(self.saved_stack.items.len == 0);
        std.debug.assert(self.export_state == .none);

        try self.syncScopeMaps(scopes.list.len);

        const allocator = self.allocator;
        const sym_count = self.symbols.items.len;

        // declaration index: count, prefix-sum, fill.
        // `decls.len` doubles as the write cursor during the fill.
        const decl_nodes = try allocator.alloc(ast.NodeIndex, self.decl_pairs.items.len);
        for (self.symbols.items) |*s| s.decls = .{ .start = 0, .len = 0 };
        for (self.decl_pairs.items) |pair| {
            self.symbols.items[@intFromEnum(pair.sid)].decls.len += 1;
        }
        var decl_offset: u32 = 0;
        for (self.symbols.items) |*s| {
            const count = s.decls.len;
            s.decls.start = decl_offset;
            s.decls.len = 0;
            decl_offset += count;
        }
        std.debug.assert(decl_offset == self.decl_pairs.items.len);
        for (self.decl_pairs.items) |pair| {
            const s = &self.symbols.items[@intFromEnum(pair.sid)];
            decl_nodes[s.decls.start + s.decls.len] = pair.node;
            s.decls.len += 1;
        }

        const node_symbols = try allocator.alloc(SymbolId, self.tree.nodes.len);
        @memset(node_symbols, .none);
        for (self.decl_pairs.items) |pair| {
            node_symbols[@intFromEnum(pair.node)] = pair.sid;
        }
        const node_references = try allocator.alloc(ReferenceId, self.tree.nodes.len);
        @memset(node_references, .none);
        for (self.references.items, 0..) |ref, i| {
            node_references[@intFromEnum(ref.node)] = @enumFromInt(@as(u32, @intCast(i)));
        }

        for (self.references.items) |*ref| {
            const name = self.tree.string(ref.name);
            const pctx = PrehashCtx{ .h = std.hash.Wyhash.hash(0, name) };
            ref.symbol = blk: {
                var it = scopes.ancestors(ref.scope);
                while (it.next()) |ancestor| {
                    const idx = @intFromEnum(ancestor);
                    if (self.scope_maps.items[idx].getAdapted(name, pctx)) |id| break :blk id;
                }
                break :blk .none;
            };
        }

        // use index: count, prefix-sum, fill.
        // `len` doubles as the write cursor during the fill.
        const use_ranges = try allocator.alloc(Range, sym_count);
        for (use_ranges) |*r| r.* = .{ .start = 0, .len = 0 };
        for (self.references.items) |ref| {
            if (ref.symbol != .none) use_ranges[@intFromEnum(ref.symbol)].len += 1;
        }
        var use_offset: u32 = 0;
        for (use_ranges) |*r| {
            r.start = use_offset;
            use_offset += r.len;
            r.len = 0;
        }
        const use_ids = try allocator.alloc(ReferenceId, use_offset);
        for (self.references.items, 0..) |ref, i| {
            if (ref.symbol == .none) continue;
            const r = &use_ranges[@intFromEnum(ref.symbol)];
            use_ids[r.start + r.len] = @enumFromInt(@as(u32, @intCast(i)));
            r.len += 1;
        }

        return .{
            .scopes = scopes,
            .symbols = self.symbols.items,
            .references = self.references.items,
            .decl_nodes = decl_nodes,
            .use_ids = use_ids,
            .use_ranges = use_ranges,
            .scope_maps = self.scope_maps.items,
            .hoisting_variables = self.hoisting_variables.items,
            .node_scopes = node_scopes,
            .node_parents = node_parents,
            .node_symbols = node_symbols,
            .node_references = node_references,
        };
    }
};

// walks a jsx tag name to its leftmost `jsx_identifier`. returns null
// for namespaced names (`<svg:path>`) and non-tag shapes.
fn jsxTagRoot(tree: *const ast.Tree, name: ast.NodeIndex) ?ast.NodeIndex {
    var cur = name;
    while (true) switch (tree.data(cur)) {
        .jsx_identifier => return cur,
        .jsx_member_expression => |m| cur = m.object,
        .jsx_namespaced_name => return null,
        else => return null,
    };
}

// expression_name scope sits between outer and the function/class
// scope, so it's the parent of the current scope.
fn exprNameScope(scope: *const sc.ScopeTracker) sc.ScopeId {
    const cur = scope.currentScope();
    return if (cur.kind == .function or cur.kind == .class) cur.parent else scope.current;
}

// a namespace occupies value space if its body has any value-producing
// statement. body-less ambient modules are instantiated by spec.
fn isNamespaceInstantiated(tree: *const ast.Tree, body_node: ast.NodeIndex) bool {
    if (body_node == .null) return true;
    const body = tree.data(body_node);
    const block = switch (body) {
        .ts_module_block => |b| b,
        else => return true,
    };
    for (tree.extra(block.body)) |stmt| {
        if (isInstantiatingStatement(tree, stmt)) return true;
    }
    return false;
}

fn isInstantiatingStatement(tree: *const ast.Tree, idx: ast.NodeIndex) bool {
    return switch (tree.data(idx)) {
        .ts_interface_declaration,
        .ts_type_alias_declaration,
        .ts_import_equals_declaration,
        => false,
        .ts_enum_declaration => |e| !e.is_const,
        .ts_module_declaration => |m| isNamespaceInstantiated(tree, m.body),
        .export_named_declaration => |e| {
            if (e.export_kind == .type) return false;
            if (e.declaration != .null) return isInstantiatingStatement(tree, e.declaration);
            return e.source != .null;
        },
        else => true,
    };
}
