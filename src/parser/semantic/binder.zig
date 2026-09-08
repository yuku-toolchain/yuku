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

/// A declared binding. Declarations that legally share a name, such as
/// `var` redeclarations, TS overloads, and `class` + `interface`
/// merging, merge into one symbol.
///
/// ## Example
/// ```ts
/// interface Shape { kind: string }
/// class Shape { kind = "circle" }
/// //    ^^^^^ one symbol with two declarations, occupying
/// //          both value space and type space
/// ```
pub const Symbol = struct {
    name: String,
    flags: Flags,
    /// The scope the symbol lands in, the hoist target for a hoisting `var`.
    scope: sc.ScopeId,
    // window into `Semantic.decl_nodes`
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
        enum_member: bool = false,
        _: u12 = 0,

        /// True when `a` and `b` have at least one flag in common.
        pub inline fn intersects(a: Flags, b: Flags) bool {
            return @as(u32, @bitCast(a)) & @as(u32, @bitCast(b)) != 0;
        }

        /// The union of two flag sets.
        pub inline fn merge(a: Flags, b: Flags) Flags {
            return @bitCast(@as(u32, @bitCast(a)) | @as(u32, @bitCast(b)));
        }

        /// True for a `var` that hoists past intermediate blocks, which
        /// excludes parameters and catch variables.
        pub inline fn isHoistingVar(self: Flags) bool {
            return self.function_scoped_var and !self.parameter and !self.catch_var;
        }

        /// True for a binding visible at runtime.
        pub inline fn inValueSpace(self: Flags) bool {
            return self.intersects(value_space);
        }

        /// True for a binding in TypeScript type space.
        pub inline fn inTypeSpace(self: Flags) bool {
            return self.intersects(type_space);
        }

        /// True for a binding a dotted type name can start from.
        pub inline fn inNamespaceSpace(self: Flags) bool {
            return self.intersects(namespace_space);
        }

        /// True when a symbol with these flags is visible in `space`.
        /// Import bindings are visible in every space.
        ///
        /// ## Example
        /// ```ts
        /// type T = string;
        /// function f() {
        ///   const T = 1;
        ///   let x: T;
        ///   //     ^ the const is not visible in .type, so
        ///   //       resolution walks on to the outer alias
        /// }
        /// ```
        pub fn visibleIn(self: Flags, space: Reference.Space) bool {
            if (self.intersects(any_import)) return true;
            return switch (space) {
                .value, .typeof => self.inValueSpace(),
                .type => self.inTypeSpace(),
                .namespace => self.inNamespaceSpace(),
                .any => true,
            };
        }

        /// True for declarations a hoisting `var` may not pass through.
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
            if (self.enum_member) return "enum member";
            return "variable";
        }
    };

    // mirrored into the JS decoder's SymbolFlags

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
        .enum_member = true,
    };

    /// Declarations that live in TS type space.
    pub const type_space: Flags = .{
        .class = true,
        .regular_enum = true,
        .const_enum = true,
        .interface = true,
        .type_alias = true,
        .type_parameter = true,
        .enum_member = true,
    };

    /// Declarations a dotted type name can start from.
    pub const namespace_space: Flags = .{
        .value_module = true,
        .namespace_module = true,
        .regular_enum = true,
        .const_enum = true,
    };

    // names a hoisted `var` cannot pass through
    const block_scoped_like: Flags = .{
        .block_scoped_var = true,
        .class = true,
        .function = true,
    };

    /// Per-declaration redeclaration excludes. A new declaration conflicts
    /// with an existing symbol whose flags intersect its excludes, otherwise
    /// the two merge.
    pub const Excludes = struct {
        pub const block_scoped_var: Flags = value_space;

        pub const function_scoped_var: Flags = blk: {
            var f = value_space;
            f.function_scoped_var = false;
            f.function = false;
            break :blk f;
        };

        /// A function in a hoist scope, where TS overloads and sloppy `var`
        /// merge with it. Lexical scopes use `block_scoped_var` instead.
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

        pub const catch_var: Flags = value_space;

        // multiple `infer T` in one conditional unify, `<T, T>` is caught structurally
        pub const type_parameter: Flags = blk: {
            var f = type_space;
            f.type_parameter = false;
            break :blk f;
        };

        pub const enum_member: Flags = value_space.merge(type_space);
    };
};

/// A use of a name, recorded for every `identifier_reference`, JSX
/// component tag, and TS type-predicate parameter. Declaration sites
/// live on the symbol.
///
/// ## Example
/// ```ts
/// let a = b; a = 2;
/// //      ^ a read of `b`
/// //         ^ a write of `a`, flags.write
/// ```
pub const Reference = struct {
    name: String,
    /// The scope the reference appears in.
    scope: sc.ScopeId,
    /// The referencing node.
    node: ast.NodeIndex,
    /// The resolved symbol, or `.none` for names with no visible binding.
    symbol: SymbolId = .none,
    flags: Flags = .{},

    pub const Flags = packed struct(u8) {
        /// True when this reference assigns its binding. Initializers in
        /// declarations are not references.
        write: bool = false,
        /// The declaration space this position resolves in.
        space: Space = .value,
        _: u4 = 0,
    };

    /// The declaration space a syntactic position resolves in, matching
    /// TypeScript name resolution. A binding outside a reference's space
    /// does not shadow.
    ///
    /// ## Example
    /// ```ts
    /// let a: T = v;
    /// //     ^ type
    /// //         ^ value
    /// let b: N.T;
    /// //     ^ namespace
    /// let c: typeof v;
    /// //            ^ typeof
    /// export { T };
    /// //       ^ any
    /// ```
    pub const Space = enum(u3) {
        /// A runtime use.
        value,
        /// A type use.
        type,
        /// The qualifier of a dotted type name, `ns` in `ns.T`.
        namespace,
        /// A value use inside a type, such as the entity of a `typeof` query.
        typeof,
        /// An alias position that accepts every space, such as `export { x }`.
        any,

        /// True for positions inside a type-only subtree.
        pub inline fn inTypePosition(self: Space) bool {
            return switch (self) {
                .type, .namespace, .typeof => true,
                .value, .any => false,
            };
        }
    };
};

/// The complete semantic model of a tree, with every scope, symbol, and
/// reference resolved and cross-indexed. Backed by the tree's arena and
/// valid for the lifetime of the tree.
///
/// ## Example
/// ```zig
/// const sem = try parser.semantic.analyze(&tree);
///
/// const sym = sem.symbolOf(node);      // declared at or resolved to
/// const sites = sem.uses(sym.?);       // every use, in source order
/// const first = sem.decls(sym.?)[0];   // first declaration node
/// const found = sem.lookup(sem.scopeOf(node), "x", .value);
/// ```
pub const Semantic = struct {
    /// Every scope, indexed by `ScopeId`.
    scopes: sc.ScopeTree,
    /// Every symbol, in declaration order, indexed by `SymbolId`.
    symbols: []const Symbol,
    /// Every reference, in source order, indexed by `ReferenceId`.
    references: []const Reference,

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

    /// The symbol declared at `node` or resolved to by the reference at
    /// `node`, or `null` for neither.
    ///
    /// ## Example
    /// ```ts
    /// let a = 1; a;
    /// //  ^ the declared symbol
    /// //         ^ the same symbol, through the reference
    /// ```
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

    /// The innermost scope containing `node`. A scope-creating node maps
    /// to the scope it creates.
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
    /// source order. A conflicting redeclaration is recorded here too, so
    /// check `tree.hasErrors()` when only legal declarations matter.
    ///
    /// ## Example
    /// ```ts
    /// var a = 1; var a = 2;
    /// //  ^ decls[0]
    /// //             ^ decls[1], same symbol
    /// ```
    pub fn decls(self: Semantic, id: SymbolId) []const ast.NodeIndex {
        const range = self.symbol(id).decls;
        std.debug.assert(@as(usize, range.start) + range.len <= self.decl_nodes.len);
        return self.decl_nodes[range.start..][0..range.len];
    }

    /// Every use site of `id`, in source order. Declaration sites are not uses.
    pub fn uses(self: Semantic, id: SymbolId) []const ReferenceId {
        std.debug.assert(id != .none);
        std.debug.assert(@intFromEnum(id) < self.use_ranges.len);
        const range = self.use_ranges[@intFromEnum(id)];
        return self.use_ids[range.start..][0..range.len];
    }

    /// The binding of `name` at `scope` alone, including a hoisting `var`
    /// passing through. Does not walk the scope chain, see `lookup`.
    ///
    /// ## Example
    /// ```ts
    /// function f() { { var a; } }
    /// //             ^ binding here finds `a`, whose symbol
    /// //               lives in the function scope
    /// ```
    pub fn binding(self: Semantic, scope_id: sc.ScopeId, name: []const u8) ?SymbolId {
        std.debug.assert(scope_id != .none);
        std.debug.assert(@intFromEnum(scope_id) < self.scope_maps.len);
        return self.scope_maps[@intFromEnum(scope_id)].get(name) orelse
            self.hoisting_variables[@intFromEnum(scope_id)].get(name);
    }

    /// Every symbol declared directly in `scope`. A hoisting `var` appears
    /// in its hoist target only.
    pub fn bindings(self: Semantic, scope_id: sc.ScopeId) BindingIterator {
        std.debug.assert(scope_id != .none);
        std.debug.assert(@intFromEnum(scope_id) < self.scope_maps.len);
        return .{ .inner = self.scope_maps[@intFromEnum(scope_id)].valueIterator() };
    }

    /// The nearest binding of `name` visible in `space` from `scope`,
    /// walking up the scope chain. A binding outside the space does not shadow.
    ///
    /// ## Example
    /// ```ts
    /// type T = string;
    /// function f() {
    ///   const T = 1;
    ///   // from here .type finds the outer alias, .value the
    ///   // local const, .any the nearest by name
    /// }
    /// ```
    pub fn lookup(
        self: Semantic,
        scope_id: sc.ScopeId,
        name: []const u8,
        space: Reference.Space,
    ) ?SymbolId {
        var it = self.scopes.ancestors(scope_id);
        while (it.next()) |ancestor| {
            const id = self.scope_maps[@intFromEnum(ancestor)].get(name) orelse continue;
            if (self.symbol(id).flags.visibleIn(space)) return id;
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
    // parallel to `symbols`
    first_decls: std.ArrayList(ast.NodeIndex) = .empty,
    scope_maps: std.ArrayList(ScopeMap) = .empty,
    hoisting_variables: std.ArrayList(ScopeMap) = .empty,

    /// What the next `binding_identifier` declares, valid inside its enter hook.
    pending: PendingBinding = .{},
    /// Whether the next `binding_identifier` is the exported name of an
    /// `export` declaration.
    export_state: ExportState = .none,
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
    /// Called for every node on enter.
    pub fn setBindingContext(
        self: *SymbolTracker,
        data: ast.NodeData,
        parent: ast.NodeIndex,
        scope: *const sc.ScopeTracker,
    ) Allocator.Error!void {
        switch (data) {
            .export_named_declaration => |decl| {
                // the declaration form exports its binding even when type-only,
                // a bare type re-export must not tag the value side
                if (decl.declaration != .null or decl.export_kind != .type) {
                    self.export_state = .named;
                }
            },
            .export_default_declaration => self.export_state = .default,

            .variable_declaration => |decl| {
                try self.pushSavedContext();
                switch (decl.kind) {
                    .@"var" => {
                        const target = scope.hoistTarget();
                        // module-level functions are lexical, elsewhere (and in ts) `var` merges
                        var excludes = Symbol.Excludes.function_scoped_var;
                        if (!self.tree.isTs() and scope.get(target).kind == .module) {
                            excludes.function = true;
                        }
                        self.pending = .{
                            .flags = .{
                                .function_scoped_var = true,
                                .ambient = decl.declare or self.ambient,
                            },
                            .excludes = excludes,
                            .scope = target,
                        };
                    },
                    .@"const", .using, .await_using => self.pending = .{
                        .flags = .{
                            .block_scoped_var = true,
                            .const_var = true,
                            .ambient = decl.declare or self.ambient,
                        },
                        .excludes = Symbol.Excludes.block_scoped_var,
                        .scope = scope.current,
                    },
                    .let => self.pending = .{
                        .flags = .{
                            .block_scoped_var = true,
                            .ambient = decl.declare or self.ambient,
                        },
                        .excludes = Symbol.Excludes.block_scoped_var,
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
                const target = if (is_decl) declNameScope(scope) else exprNameScope(scope);

                // annex B 3.2 and ts overloads merge in hoist scopes, not lexical ones
                const k = scope.get(target).kind;
                const allow_overload = self.tree.isTs() or
                    k == .function or
                    k == .function_body or
                    k == .global or
                    k == .static_block;

                self.pending = .{
                    .flags = .{ .function = true, .ambient = ambient },
                    .excludes = if (allow_overload)
                        Symbol.Excludes.function
                    else
                        Symbol.Excludes.block_scoped_var,
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
            .class_body, .ts_module_block, .ts_enum_body => self.export_state = .none,

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
                    .excludes = Symbol.Excludes.catch_var,
                    .scope = scope.current,
                };
            },

            // the id binds outside the scope the tracker pushed for the body
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

            // mapped type keys `[K in T]` are bare binding identifiers, so they share this
            .ts_type_parameter, .ts_mapped_type => {
                try self.pushSavedContext();
                // infer declares in the enclosing conditional so the true branch sees it
                //   T extends ((k: infer I) => void) ? I : never
                //   //                  ^ declares here    ^ resolves
                const target = if (parent != .null and self.tree.data(parent) == .ts_infer_type)
                    nearestConditionalScope(self.tree, scope)
                else
                    scope.current;
                self.pending = .{
                    .flags = .{ .type_parameter = true },
                    .excludes = Symbol.Excludes.type_parameter,
                    .scope = target,
                };
                self.export_state = .none;
            },

            // signature parameter names are labels, clear any enclosing
            // type-parameter context so they never declare
            //
            //   <T extends { [s: string]: number }>
            //   //          ^ label, must not become a symbol
            .ts_function_type,
            .ts_constructor_type,
            .ts_method_signature,
            .ts_call_signature_declaration,
            .ts_construct_signature_declaration,
            .ts_index_signature,
            => {
                try self.pushSavedContext();
                self.pending = .{
                    .flags = .{},
                    .excludes = .{},
                    .scope = scope.current,
                };
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
        /// The node is an identifier in assignment-target position.
        is_write: bool,
        /// The declaration space the identifier resolves in.
        space: Reference.Space = .value,
    };

    /// Declares the pending binding at a `binding_identifier` or records
    /// a reference. Called for every node after its enter hook, before its children.
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
                // type-position identifiers are labels unless they are type parameters
                if (ref_ctx.space.inTypePosition() and !self.pending.flags.type_parameter) return;

                const sym_id = try self.declare(id.name, index);

                // visible in every block it passes through so redeclarations see it
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
                    .write = ref_ctx.is_write,
                    .space = ref_ctx.space,
                });
            },
            // `v is T` parses `v` as an identifier_name but it references the
            // parameter binding, renamers need the link
            //
            //   function isStr(v: unknown): v is string {}
            //                  ^             ^ references the parameter
            //   type P = (x: unknown) => x is string;
            //             ^              ^ a label, no reference
            .ts_type_predicate => |pred| {
                if (pred.parameter_name == .null) return;
                const pname = self.tree.data(pred.parameter_name);
                if (pname != .identifier_name) return;
                const name = pname.identifier_name.name;
                const param = self.ownBinding(scope.current, self.tree.string(name)) orelse return;
                if (!self.symbol(param).flags.parameter) return;
                _ = try self.addReference(name, scope.current, pred.parameter_name, .{
                    .space = .typeof,
                });
            },
            // members resolve lexically inside the body like tsc, and their
            // names are not binding identifiers so they declare here
            //
            //   enum E { a, b = a }
            //   //              ^ resolves to the member, tsc-compatible
            .ts_enum_member => |member| {
                if (member.computed) return;
                const name = switch (self.tree.data(member.id)) {
                    .identifier_name => |id| id.name,
                    .string_literal => |str| str.value,
                    else => return,
                };
                const saved = self.pending;
                self.pending = .{
                    .flags = .{ .enum_member = true, .ambient = self.ambient },
                    .excludes = Symbol.Excludes.enum_member,
                    .scope = scope.current,
                };
                _ = try self.declare(name, member.id);
                self.pending = saved;
            },
            // a member tag references its leftmost object, a lone lowercase tag is intrinsic
            inline .jsx_opening_element, .jsx_closing_element => |el| {
                if (jsxTagRoot(self.tree, el.name)) |root_idx| {
                    const id = self.tree.data(root_idx).jsx_identifier;
                    const text = self.tree.string(id.name);
                    const member = switch (self.tree.data(el.name)) {
                        .jsx_member_expression => true,
                        else => false,
                    };
                    if (member or (text.len > 0 and text[0] >= 'A' and text[0] <= 'Z')) {
                        _ = try self.addReference(id.name, scope.current, root_idx, .{});
                    }
                }
            },
            else => {},
        }
    }

    /// Restores the context saved by the matching enter. Safe for any node.
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
            .ts_function_type,
            .ts_constructor_type,
            .ts_method_signature,
            .ts_call_signature_declaration,
            .ts_construct_signature_declaration,
            .ts_index_signature,
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

    /// Declares the pending binding for `name` at `node`, merging into a
    /// compatible existing symbol. A conflicting name leaves the existing
    /// flags unchanged but still records `node` as a declarator for error recovery.
    pub fn declare(self: *SymbolTracker, name: String, node: ast.NodeIndex) Allocator.Error!SymbolId {
        const target = self.pending.scope;
        std.debug.assert(target != .none);
        std.debug.assert(@intFromEnum(target) < self.scope_maps.items.len);
        std.debug.assert(node != .null);

        const name_str = self.tree.string(name);
        const target_idx = @intFromEnum(target);

        const id = if (self.binding(target, name_str)) |existing| sid: {
            const sym = &self.symbols.items[@intFromEnum(existing)];
            if (!sym.flags.intersects(self.pending.excludes)) {
                var merged = sym.flags.merge(self.pending.flags);
                merged.exported = merged.exported or self.export_state != .none;
                merged.is_default = merged.is_default or self.export_state == .default;
                // an overload implementation emits at runtime, so the merge is non-ambient
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

    /// The `binding_identifier` node of the earliest declaration of `id`.
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

    /// Finalizes the tracker into a complete `Semantic` that aliases the
    /// tracker's storage and stays valid for the lifetime of the tree.
    pub fn finalize(
        self: *SymbolTracker,
        scopes: sc.ScopeTree,
        node_scopes: []const sc.ScopeId,
        node_parents: []const ast.NodeIndex,
    ) Allocator.Error!Semantic {
        std.debug.assert(self.saved_stack.items.len == 0);
        std.debug.assert(self.export_state == .none);

        try self.syncScopeMaps(scopes.list.len);

        const allocator = self.allocator;
        const sym_count = self.symbols.items.len;

        // `decls.len` doubles as the write cursor during the fill
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
            // the implicit arguments object (10.2.11 argumentsObjectNeeded) shadows outer bindings,
            // an own parameter or var still wins
            const arguments_barrier = (ref.flags.space == .value or ref.flags.space == .typeof) and
                std.mem.eql(u8, name, "arguments");
            ref.symbol = blk: {
                var it = scopes.ancestors(ref.scope);
                while (it.next()) |ancestor| {
                    const idx = @intFromEnum(ancestor);
                    if (self.scope_maps.items[idx].getAdapted(name, pctx)) |id| {
                        // a binding outside the reference's space does not shadow
                        const sym = self.symbol(id);
                        if (sym.flags.visibleIn(ref.flags.space) and
                            typeParameterVisible(self.tree, sym, ref.node, scopes, node_parents))
                        {
                            break :blk id;
                        }
                    }
                    if (arguments_barrier and isArgumentsBarrier(self.tree, scopes.get(ancestor))) {
                        break :blk .none;
                    }
                }
                break :blk .none;
            };
        }

        // `len` doubles as the write cursor during the fill
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

// infer variables exist only in their conditional's true branch, class type
// parameters are hidden in static members (TS2302) and computed keys (TS2467)
fn typeParameterVisible(
    tree: *const ast.Tree,
    sym: Symbol,
    ref_node: ast.NodeIndex,
    scopes: sc.ScopeTree,
    node_parents: []const ast.NodeIndex,
) bool {
    if (!sym.flags.type_parameter) return true;
    const scope_node = scopes.get(sym.scope).node;
    return switch (tree.data(scope_node)) {
        .ts_conditional_type => |cond| inSubtree(node_parents, ref_node, scope_node, cond.true_type),
        .class => !classTypeParameterHidden(tree, node_parents, ref_node, scope_node),
        else => true,
    };
}

fn inSubtree(
    node_parents: []const ast.NodeIndex,
    node: ast.NodeIndex,
    root: ast.NodeIndex,
    subtree: ast.NodeIndex,
) bool {
    var child = node;
    var parent = node_parents[@intFromEnum(child)];
    while (parent != .null) : ({
        child = parent;
        parent = node_parents[@intFromEnum(parent)];
    }) {
        if (parent == root) return child == subtree;
    }
    return false;
}

fn classTypeParameterHidden(
    tree: *const ast.Tree,
    node_parents: []const ast.NodeIndex,
    ref_node: ast.NodeIndex,
    class_node: ast.NodeIndex,
) bool {
    var child = ref_node;
    var parent = node_parents[@intFromEnum(child)];
    while (parent != .null) : ({
        child = parent;
        parent = node_parents[@intFromEnum(parent)];
    }) {
        switch (tree.data(parent)) {
            .method_definition => |m| if (m.computed and m.key == child and
                memberOwner(node_parents, parent) == class_node) return true,
            .property_definition => |p| if (p.computed and p.key == child and
                memberOwner(node_parents, parent) == class_node) return true,
            .class_body => {
                if (node_parents[@intFromEnum(parent)] != class_node) continue;
                return switch (tree.data(child)) {
                    .method_definition => |m| m.static,
                    .property_definition => |p| p.static,
                    .ts_index_signature => |s| s.static,
                    .static_block => true,
                    else => false,
                };
            },
            else => {},
        }
    }
    return false;
}

fn memberOwner(node_parents: []const ast.NodeIndex, member: ast.NodeIndex) ast.NodeIndex {
    const body = node_parents[@intFromEnum(member)];
    return if (body == .null) .null else node_parents[@intFromEnum(body)];
}

fn nearestConditionalScope(tree: *const ast.Tree, scope: *const sc.ScopeTracker) sc.ScopeId {
    var it = scope.ancestors(scope.current);
    while (it.next()) |id| {
        if (tree.data(scope.get(id).node) == .ts_conditional_type) return id;
    }
    return scope.current;
}

// only non-arrow functions hold an arguments object, static blocks have none at all
fn isArgumentsBarrier(tree: *const ast.Tree, scope: sc.Scope) bool {
    return switch (scope.kind) {
        .function => tree.data(scope.node) == .function,
        .static_block => true,
        else => false,
    };
}

fn jsxTagRoot(tree: *const ast.Tree, name: ast.NodeIndex) ?ast.NodeIndex {
    var cur = name;
    while (true) switch (tree.data(cur)) {
        .jsx_identifier => return cur,
        .jsx_member_expression => |m| cur = m.object,
        .jsx_namespaced_name => return null,
        else => return null,
    };
}

fn exprNameScope(scope: *const sc.ScopeTracker) sc.ScopeId {
    const cur = scope.currentScope();
    return if (cur.kind == .function or cur.kind == .class) cur.parent else scope.current;
}

// at a body's top level a function declaration is var-scoped and hoists past
fn declNameScope(scope: *const sc.ScopeTracker) sc.ScopeId {
    const enclosing = scope.currentScope().parent;
    std.debug.assert(enclosing != .none);
    const enclosing_scope = scope.get(enclosing);
    if (enclosing_scope.kind == .function_body) return enclosing_scope.hoist_target;
    return enclosing;
}

// body-less ambient modules are instantiated by spec
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
