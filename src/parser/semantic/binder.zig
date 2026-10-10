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
            return @backingInt(a) & @backingInt(b) != 0;
        }

        /// The union of two flag sets.
        pub inline fn merge(a: Flags, b: Flags) Flags {
            return @fromBackingInt(@backingInt(a) | @backingInt(b));
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

    // mirrored into the JS decoder's BindingFlags

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

        /// A TypeScript function.
        pub const function: Flags = blk: {
            var f = value_space;
            f.function = false;
            f.value_module = false;
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
        /// True when the use is erased with the types.
        type_position: bool = false,
        _: u3 = 0,
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
        /// The qualifier of a dotted name, `ns` in `ns.T` and `import x = ns.T`.
        namespace,
        /// A value use inside a type, such as the entity of a `typeof` query.
        typeof,
        /// An alias position that accepts every space, such as `export { x }`.
        any,
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
    /// Per scope, the next body of the same namespace or enum, in a cycle, or `.none`.
    next_bodies: []const sc.ScopeId,
    node_scopes: []const sc.ScopeId,
    node_parents: []const ast.NodeIndex,
    node_symbols: []const SymbolId,
    node_references: []const ReferenceId,

    /// The symbol with the given id.
    pub inline fn symbol(self: Semantic, id: SymbolId) Symbol {
        std.debug.assert(id != .none);
        std.debug.assert(@backingInt(id) < self.symbols.len);
        return self.symbols[@backingInt(id)];
    }

    /// The reference with the given id.
    pub inline fn reference(self: Semantic, id: ReferenceId) Reference {
        std.debug.assert(id != .none);
        std.debug.assert(@backingInt(id) < self.references.len);
        return self.references[@backingInt(id)];
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
        std.debug.assert(@backingInt(node) < self.node_symbols.len);
        const declared = self.node_symbols[@backingInt(node)];
        if (declared != .none) return declared;
        const ref = self.node_references[@backingInt(node)];
        if (ref == .none) return null;
        const resolved = self.reference(ref).symbol;
        return if (resolved != .none) resolved else null;
    }

    /// The reference recorded at `node`, or `null` when the node is
    /// not a reference site.
    pub fn referenceOf(self: Semantic, node: ast.NodeIndex) ?ReferenceId {
        std.debug.assert(node != .null);
        std.debug.assert(@backingInt(node) < self.node_references.len);
        const id = self.node_references[@backingInt(node)];
        return if (id != .none) id else null;
    }

    /// The innermost scope containing `node`. A scope-creating node maps
    /// to the scope it creates.
    pub inline fn scopeOf(self: Semantic, node: ast.NodeIndex) sc.ScopeId {
        std.debug.assert(node != .null);
        std.debug.assert(@backingInt(node) < self.node_scopes.len);
        return self.node_scopes[@backingInt(node)];
    }

    /// The structural parent of `node`, or `null` at the root.
    pub fn parentOf(self: Semantic, node: ast.NodeIndex) ?ast.NodeIndex {
        std.debug.assert(node != .null);
        std.debug.assert(@backingInt(node) < self.node_parents.len);
        const parent = self.node_parents[@backingInt(node)];
        return if (parent != .null) parent else null;
    }

    /// Walks from `node` up to the root, yielding `node` first.
    pub fn ancestors(self: Semantic, node: ast.NodeIndex) AncestorIterator {
        std.debug.assert(node == .null or @backingInt(node) < self.node_parents.len);
        return .{ .node_parents = self.node_parents, .current = node };
    }

    /// The name node of every declaration of `id`, in
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
        std.debug.assert(@backingInt(id) < self.use_ranges.len);
        const range = self.use_ranges[@backingInt(id)];
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
        std.debug.assert(@backingInt(scope_id) < self.scope_maps.len);
        return self.scope_maps[@backingInt(scope_id)].get(name) orelse
            self.hoisting_variables[@backingInt(scope_id)].get(name);
    }

    /// Every symbol declared directly in `scope`. A hoisting `var` appears
    /// in its hoist target only.
    pub fn bindings(self: Semantic, scope_id: sc.ScopeId) BindingIterator {
        std.debug.assert(scope_id != .none);
        std.debug.assert(@backingInt(scope_id) < self.scope_maps.len);
        return .{ .inner = self.scope_maps[@backingInt(scope_id)].valueIterator() };
    }

    /// The nearest binding of `name` visible in `space` from `scope`, walking up the
    /// scope chain and the other blocks of each namespace or enum on it. A binding
    /// outside the space does not shadow.
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
        const bodies: Bodies = .{
            .scopes = self.scopes.list,
            .scope_maps = self.scope_maps,
            .symbols = self.symbols,
            .next = self.next_bodies,
        };
        var it = self.scopes.ancestors(scope_id);
        while (it.next()) |ancestor| {
            if (self.scope_maps[@backingInt(ancestor)].get(name)) |id| {
                if (self.symbol(id).flags.visibleIn(space)) return id;
            }
            if (bodies.sharedMember(ancestor, name, space)) |id| return id;
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
            return .{ .id = @fromBackingInt(i), .scope = self.list[i] };
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
            std.debug.assert(@backingInt(node) < self.node_parents.len);
            self.current = self.node_parents[@backingInt(node)];
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
            return .{ .id = @fromBackingInt(i), .symbol = self.symbols[i] };
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
            return .{ .id = @fromBackingInt(i), .reference = self.references[i] };
        }
    };
};

const Bodies = struct {
    scopes: []const sc.Scope,
    scope_maps: []const ScopeMap,
    symbols: []const Symbol,
    next: []const sc.ScopeId,

    fn sharedMember(
        self: Bodies,
        scope_id: sc.ScopeId,
        name: []const u8,
        space: Reference.Space,
    ) ?SymbolId {
        // the tracker sizes `next` on demand, past its end no scope is a body
        if (@backingInt(scope_id) >= self.next.len) return null;
        var body = self.next[@backingInt(scope_id)];
        if (body == .none) return null;
        const in_enum = self.scopes[@backingInt(scope_id)].kind != .ts_module;
        while (body != scope_id) : (body = self.next[@backingInt(body)]) {
            const id = self.scope_maps[@backingInt(body)].get(name) orelse continue;
            const flags = self.symbols[@backingInt(id)].flags;
            const shared = if (in_enum) flags.enum_member else flags.exported;
            if (shared and flags.visibleIn(space)) return id;
        }
        return null;
    }
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
    next_bodies: std.ArrayList(sc.ScopeId) = .empty,
    // per scope, whether declarations export implicitly
    export_contexts: std.ArrayList(bool) = .empty,
    last_bodies: std.AutoHashMapUnmanaged(SymbolId, sc.ScopeId) = .empty,
    last_module_bodies: std.StringHashMapUnmanaged(sc.ScopeId) = .empty,
    shares_members: bool = false,
    body_owner: SymbolId = .none,
    global_body: sc.ScopeId = .none,

    /// What the next `binding_identifier` declares, valid inside its enter hook.
    pending: PendingBinding = .{},
    /// Whether the next `binding_identifier` is the exported name of an
    /// `export` declaration.
    export_state: ExportState = .none,
    ambient: bool = false,
    excluded_imports: Symbol.Flags,

    saved_stack: std.ArrayList(SavedContext) = .empty,

    pub const ExportState = enum { none, named, default };

    /// The declaration context for the next `binding_identifier`.
    pub const PendingBinding = struct {
        flags: Symbol.Flags = .{},
        excludes: Symbol.Flags = .{},
        scope: sc.ScopeId = .root,
    };

    /// Per-node facts computed by the walker for `declareBindings`.
    pub const RefContext = struct {
        /// The node is an identifier in assignment-target position.
        is_write: bool,
        /// The declaration space the identifier resolves in.
        space: Reference.Space = .value,
        /// The identifier is erased with the types.
        type_position: bool = false,
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
            .excluded_imports = if (tree.isTs()) .{} else Symbol.any_import,
        };

        const nodes: u32 = @intCast(tree.nodes.len);
        try self.symbols.ensureTotalCapacity(alloc, @max(16, nodes / 12));
        try self.first_decls.ensureTotalCapacity(alloc, @max(16, nodes / 12));
        try self.references.ensureTotalCapacity(alloc, @max(16, nodes / 4));
        try self.decl_pairs.ensureTotalCapacity(alloc, @max(16, nodes / 12));
        try self.scope_maps.ensureTotalCapacity(alloc, @max(8, nodes / 16));
        try self.hoisting_variables.ensureTotalCapacity(alloc, @max(8, nodes / 16));
        try self.saved_stack.ensureTotalCapacity(alloc, 32);

        if (exportsImplicitly(tree)) try self.setExportContext(.module, true);
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
                const ambient = decl.declare or self.ambient;
                switch (decl.kind) {
                    .@"var" => {
                        const target = scope.hoistTarget();
                        // ts and module-level functions never merge with `var`
                        var excludes =
                            Symbol.Excludes.function_scoped_var.merge(self.excluded_imports);
                        if (self.tree.isTs() or scope.get(target).kind == .module) {
                            excludes.function = true;
                        }
                        self.pending = .{
                            .flags = .{ .function_scoped_var = true, .ambient = ambient },
                            .excludes = excludes,
                            .scope = target,
                        };
                    },
                    .let, .@"const", .using, .await_using => self.pending = .{
                        .flags = .{
                            .block_scoped_var = true,
                            .const_var = decl.kind != .let,
                            .ambient = ambient,
                        },
                        .excludes = Symbol.Excludes.block_scoped_var.merge(self.excluded_imports),
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

                const kind = scope.get(target).kind;
                const var_like = kind == .function or
                    kind == .function_body or
                    kind == .global or
                    kind == .static_block;

                self.pending = .{
                    .flags = .{ .function = true, .ambient = ambient },
                    .excludes = (if (self.tree.isTs())
                        Symbol.Excludes.function
                    else if (var_like)
                        Symbol.Excludes.function_scoped_var
                    else
                        Symbol.Excludes.block_scoped_var).merge(self.excluded_imports),
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
                    .excludes = Symbol.Excludes.class.merge(self.excluded_imports),
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
            .class_body => self.export_state = .none,
            .ts_module_block, .ts_enum_body => try self.enterBody(data, parent, scope),

            inline .import_declaration, .ts_import_equals_declaration => |decl| {
                try self.pushSavedContext();
                self.pending = .{
                    .flags = if (decl.import_kind == .type)
                        .{ .type_import = true }
                    else
                        .{ .import = true },
                    .excludes = if (self.tree.isTs())
                        Symbol.Excludes.import_binding
                    else
                        Symbol.Excludes.import_binding.merge(Symbol.value_space),
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
                self.pending = .{
                    .flags = .{
                        .regular_enum = !decl.is_const,
                        .const_enum = decl.is_const,
                        .ambient = decl.declare or self.ambient,
                    },
                    .excludes = if (decl.is_const)
                        Symbol.Excludes.const_enum
                    else
                        Symbol.Excludes.regular_enum,
                    .scope = scope.current,
                };
                if (decl.declare) self.ambient = true;
            },

            .ts_module_declaration => |decl| {
                try self.pushSavedContext();
                self.body_owner = .none;
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
                self.body_owner = .none;
                self.ambient = true;
            },

            .ts_namespace_export_declaration => |decl| {
                try self.pushSavedContext();
                try self.declareGlobalNamespace(decl, scope);
            },

            // mapped type keys `[K in T]` are bare binding identifiers, so they share this
            .ts_type_parameter, .ts_mapped_type => {
                try self.pushSavedContext();
                // infer declares in the enclosing conditional so the true branch sees it
                //   T extends ((k: infer I) => void) ? I : never
                //   //                   ^ declares    ^ resolves
                const target = if (parent != .null and self.tree.data(parent) == .ts_infer_type)
                    inferScope(self.tree, scope, parent)
                else
                    scope.current;
                self.pending = .{
                    .flags = .{ .type_parameter = true },
                    .excludes = Symbol.Excludes.type_parameter,
                    .scope = target,
                };
                self.export_state = .none;
            },

            // its parameters are not `formal_parameters`
            .ts_index_signature => {
                try self.pushSavedContext();
                self.pending = .{
                    .flags = .{ .function_scoped_var = true, .parameter = true },
                    .excludes = Symbol.Excludes.parameter,
                    .scope = scope.current,
                };
            },

            else => {},
        }
    }

    // `export as namespace N`
    noinline fn declareGlobalNamespace(
        self: *SymbolTracker,
        decl: ast.TSNamespaceExportDeclaration,
        scope: *const sc.ScopeTracker,
    ) Allocator.Error!void {
        self.pending = .{
            .flags = .{ .namespace_module = true, .ambient = true },
            .excludes = Symbol.Excludes.namespace_module,
            .scope = .root,
        };
        try self.syncScopeMaps(scope.scopes.items.len);
        const name = self.tree.data(decl.id).identifier_name.name;
        _ = try self.declare(name, decl.id, scope.scopes.items);
    }

    noinline fn enterBody(
        self: *SymbolTracker,
        data: ast.NodeData,
        parent: ast.NodeIndex,
        scope: *const sc.ScopeTracker,
    ) Allocator.Error!void {
        self.export_state = .none;
        try self.syncScopeMaps(scope.scopes.items.len);
        if (parent != .null and self.tree.data(parent) == .ts_global_declaration) {
            self.global_body = scope.current;
        } else if (self.body_owner != .none) {
            try self.addBody(try self.lastBodyOf(self.body_owner), scope.current);
            self.body_owner = .none;
        } else if (try self.lastModuleBody(parent)) |last| {
            try self.addBody(last, scope.current);
        }
        if (data == .ts_module_block and self.ambient) {
            const body = self.tree.extra(data.ts_module_block.body);
            try self.setExportContext(scope.current, !hasExportStatement(self.tree, body));
        }
    }

    inline fn pushSavedContext(self: *SymbolTracker) Allocator.Error!void {
        try self.saved_stack.append(self.allocator, .{
            .pending = self.pending,
            .export_state = self.export_state,
            .ambient = self.ambient,
        });
    }

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
                const flags = self.pending.flags;
                // in a type, only type parameters and parameters declare
                if (ref_ctx.type_position and !flags.type_parameter and !flags.parameter) {
                    return;
                }

                const sym_id = try self.declare(id.name, index, scope.scopes.items);
                if (flags.namespace_module or flags.regular_enum or flags.const_enum) {
                    self.body_owner = sym_id;
                }

                // visible in every block it passes through so redeclarations see it
                if (flags.isHoistingVar()) {
                    var iter = scope.ancestors(scope.current);
                    while (iter.next()) |s| {
                        if (s == self.pending.scope) break;
                        const table = &self.hoisting_variables.items[@backingInt(s)];
                        const gop = try table.getOrPut(self.allocator, self.tree.string(id.name));
                        if (!gop.found_existing) gop.value_ptr.* = sym_id;
                    }
                }
            },
            .ts_module_declaration => |decl| {
                if (self.tree.data(decl.id) == .ts_qualified_name) {
                    try self.declareQualifiedNamespace(decl, scope);
                }
            },
            .identifier_reference => |id| {
                _ = try self.addReference(id.name, scope.current, index, .{
                    .write = ref_ctx.is_write,
                    .space = ref_ctx.space,
                    .type_position = ref_ctx.type_position,
                });
            },
            // `v is T` parses `v` as an identifier_name but it references the
            // parameter binding, renamers need the link
            //
            //   function isStr(v: unknown): v is string {}
            //                  ^            ^ references the parameter
            .ts_type_predicate => |pred| {
                if (pred.parameter_name == .null) return;
                const pname = self.tree.data(pred.parameter_name);
                if (pname != .identifier_name) return;
                const name = pname.identifier_name.name;
                const param = self.ownBinding(scope.current, self.tree.string(name)) orelse return;
                if (!self.symbol(param).flags.parameter) return;
                _ = try self.addReference(name, scope.current, pred.parameter_name, .{
                    .space = .typeof,
                    .type_position = true,
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
                _ = try self.declare(name, member.id, scope.scopes.items);
                self.pending = saved;
            },
            // like the JSX transforms, a lone tag starting with a-z or holding a `-` is an
            // intrinsic string, and a member tag references its leftmost object unless `this`
            inline .jsx_opening_element, .jsx_closing_element => |el| {
                if (jsxTagRoot(self.tree, el.name)) |root_idx| {
                    const id = self.tree.data(root_idx).jsx_identifier;
                    const text = self.tree.string(id.name);
                    const is_value = if (self.tree.data(el.name) == .jsx_member_expression)
                        !std.mem.eql(u8, text, "this")
                    else
                        text.len > 0 and !std.ascii.isLower(text[0]);
                    if (is_value and std.mem.findScalar(u8, text, '-') == null) {
                        _ = try self.addReference(id.name, scope.current, root_idx, .{});
                    }
                }
            },
            else => {},
        }
    }

    // `namespace A.B {}` as `namespace A { export namespace B {} }`
    noinline fn declareQualifiedNamespace(
        self: *SymbolTracker,
        decl: ast.TSModuleDeclaration,
        scope: *const sc.ScopeTracker,
    ) Allocator.Error!void {
        std.debug.assert(self.tree.data(decl.id) == .ts_qualified_name);
        const export_state = self.export_state;
        defer self.export_state = export_state;
        var depth: u32 = 0;
        var head = decl.id;
        while (self.tree.data(head) == .ts_qualified_name) : (depth += 1) {
            head = self.tree.data(head).ts_qualified_name.left;
        }
        var owner: SymbolId = .none;
        var level: u32 = 0;
        while (level <= depth) : (level += 1) {
            var part = decl.id;
            var target = scope.current;
            var steps = depth - level;
            while (steps > 0) : (steps -= 1) {
                part = self.tree.data(part).ts_qualified_name.left;
                target = scope.get(target).parent;
            }
            const name, const node = switch (self.tree.data(part)) {
                .ts_qualified_name => |q| .{
                    self.tree.data(q.right).identifier_name.name,
                    q.right,
                },
                .binding_identifier => |id| .{ id.name, part },
                else => unreachable,
            };
            if (owner != .none) try self.addBody(try self.lastBodyOf(owner), target);
            self.export_state = if (owner == .none) export_state else .named;
            self.pending.scope = target;
            owner = try self.declare(name, node, scope.scopes.items);
        }
        self.body_owner = owner;
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
    pub fn declare(
        self: *SymbolTracker,
        name: String,
        node: ast.NodeIndex,
        scopes: []const sc.Scope,
    ) Allocator.Error!SymbolId {
        const target = self.pendingTarget();
        std.debug.assert(target != .none);
        std.debug.assert(@backingInt(target) < self.scope_maps.items.len);
        std.debug.assert(node != .null);

        const name_str = self.tree.string(name);
        const target_idx = @backingInt(target);

        const exported = self.pendingExported(target);
        // an export merges across the bodies of its namespace
        const own = self.binding(target, name_str);
        const shared = if (own == null) self.sharedExport(target, name_str, scopes) else null;
        if (shared) |existing| {
            try self.scope_maps.items[target_idx].put(self.allocator, name_str, existing);
        }

        const id = if (own orelse shared) |existing| sid: {
            const sym = &self.symbols.items[@backingInt(existing)];
            if (!conflicts(self.pending, sym.flags)) {
                var merged = sym.flags.merge(self.pending.flags);
                merged.exported = merged.exported or exported;
                merged.is_default = merged.is_default or self.export_state == .default;
                // an overload implementation emits at runtime, so the merge is non-ambient
                merged.ambient = sym.flags.ambient and self.pending.flags.ambient;
                sym.flags = merged;
            }
            break :sid existing;
        } else sid: {
            std.debug.assert(self.symbols.items.len < std.math.maxInt(u32));
            const new_id: SymbolId = @fromBackingInt(@intCast(self.symbols.items.len));
            var flags = self.pending.flags;
            flags.exported = exported;
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

    fn pendingTarget(self: *const SymbolTracker) sc.ScopeId {
        return if (self.pending.scope == self.global_body) .root else self.pending.scope;
    }

    fn pendingExported(self: *const SymbolTracker, target: sc.ScopeId) bool {
        const target_idx = @backingInt(target);
        const is_import = self.pending.flags.intersects(Symbol.any_import);
        const export_context = target_idx < self.export_contexts.items.len and
            self.export_contexts.items[target_idx];
        return self.export_state != .none or (export_context and !is_import);
    }

    fn sharedExport(
        self: *const SymbolTracker,
        target: sc.ScopeId,
        name: []const u8,
        scopes: []const sc.Scope,
    ) ?SymbolId {
        if (!self.shares_members or !self.pendingExported(target)) return null;
        return self.bodies(scopes).sharedMember(target, name, .any);
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

        const id: ReferenceId = @fromBackingInt(@intCast(self.references.items.len));
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
        std.debug.assert(@backingInt(id) < self.symbols.items.len);
        return self.symbols.items[@backingInt(id)];
    }

    /// The name node of the earliest declaration of `id`.
    pub fn firstDeclOf(self: *const SymbolTracker, id: SymbolId) ast.NodeIndex {
        std.debug.assert(id != .none);
        std.debug.assert(@backingInt(id) < self.first_decls.items.len);
        return self.first_decls.items[@backingInt(id)];
    }

    /// The binding of `name` declared directly in `scope`, excluding
    /// hoisting `var`s passing through.
    pub fn ownBinding(self: *const SymbolTracker, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        const idx = @backingInt(scope);
        if (idx >= self.scope_maps.items.len) return null;
        return self.scope_maps.items[idx].get(name);
    }

    /// The binding of `name` at `scope`, including a hoisting `var`
    /// passing through on its way to its hoist target.
    pub fn binding(self: *const SymbolTracker, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        if (self.ownBinding(scope, name)) |id| return id;
        const idx = @backingInt(scope);
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

    fn setExportContext(
        self: *SymbolTracker,
        scope_id: sc.ScopeId,
        value: bool,
    ) Allocator.Error!void {
        const count = @backingInt(scope_id) + 1;
        try padTo(bool, &self.export_contexts, self.allocator, count, false);
        self.export_contexts.items[@backingInt(scope_id)] = value;
    }

    // of a `declare module "m"`
    fn lastModuleBody(
        self: *SymbolTracker,
        declaration: ast.NodeIndex,
    ) Allocator.Error!?*sc.ScopeId {
        if (declaration == .null) return null;
        const id = switch (self.tree.data(declaration)) {
            .ts_module_declaration => |decl| decl.id,
            else => return null,
        };
        const name = switch (self.tree.data(id)) {
            .string_literal => |literal| self.tree.string(literal.value),
            else => return null,
        };
        const entry = try self.last_module_bodies.getOrPutValue(self.allocator, name, .none);
        return entry.value_ptr;
    }

    fn lastBodyOf(self: *SymbolTracker, owner: SymbolId) Allocator.Error!*sc.ScopeId {
        return (try self.last_bodies.getOrPutValue(self.allocator, owner, .none)).value_ptr;
    }

    fn addBody(self: *SymbolTracker, last: *sc.ScopeId, body: sc.ScopeId) Allocator.Error!void {
        std.debug.assert(last.* != body);
        const count = @backingInt(body) + 1;
        try padTo(sc.ScopeId, &self.next_bodies, self.allocator, count, .none);
        const next = self.next_bodies.items;
        std.debug.assert(next[@backingInt(body)] == .none);
        if (last.* == .none) {
            next[@backingInt(body)] = body;
        } else {
            next[@backingInt(body)] = next[@backingInt(last.*)];
            next[@backingInt(last.*)] = body;
            self.shares_members = true;
        }
        last.* = body;
    }

    fn bodies(self: *const SymbolTracker, scopes: []const sc.Scope) Bodies {
        return .{
            .scopes = scopes,
            .scope_maps = self.scope_maps.items,
            .symbols = self.symbols.items,
            .next = self.next_bodies.items,
        };
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
        try padTo(sc.ScopeId, &self.next_bodies, self.allocator, scopes.list.len, .none);

        const allocator = self.allocator;
        const sym_count = self.symbols.items.len;

        // `decls.len` doubles as the write cursor during the fill
        const decl_nodes = try allocator.alloc(ast.NodeIndex, self.decl_pairs.items.len);
        for (self.symbols.items) |*s| s.decls = .{ .start = 0, .len = 0 };
        for (self.decl_pairs.items) |pair| {
            self.symbols.items[@backingInt(pair.sid)].decls.len += 1;
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
            const s = &self.symbols.items[@backingInt(pair.sid)];
            decl_nodes[s.decls.start + s.decls.len] = pair.node;
            s.decls.len += 1;
        }

        const node_symbols = try allocator.alloc(SymbolId, self.tree.nodes.len);
        @memset(node_symbols, .none);
        for (self.decl_pairs.items) |pair| {
            node_symbols[@backingInt(pair.node)] = pair.sid;
        }
        const node_references = try allocator.alloc(ReferenceId, self.tree.nodes.len);
        @memset(node_references, .none);
        for (self.references.items, 0..) |ref, i| {
            node_references[@backingInt(ref.node)] = @fromBackingInt(@intCast(i));
        }

        const members = self.bodies(scopes.list);
        const shares_members = self.shares_members;
        for (self.references.items) |*ref| {
            const name = self.tree.string(ref.name);
            const space = ref.flags.space;
            const pctx = PrehashCtx{ .h = std.hash.Wyhash.hash(0, name) };
            // the implicit arguments object (10.2.11 argumentsObjectNeeded) shadows outer bindings,
            // an own parameter or var still wins
            const arguments_barrier = (space == .value or space == .typeof) and
                std.mem.eql(u8, name, "arguments");
            ref.symbol = blk: {
                var it = scopes.ancestors(ref.scope);
                while (it.next()) |ancestor| {
                    const idx = @backingInt(ancestor);
                    if (self.scope_maps.items[idx].getAdapted(name, pctx)) |id| {
                        // a binding outside the reference's space does not shadow
                        const sym = self.symbol(id);
                        if (sym.flags.visibleIn(space) and
                            visibleAt(self.tree, sym, ref, scopes, node_parents))
                        {
                            break :blk id;
                        }
                    }
                    if (shares_members) {
                        if (members.sharedMember(ancestor, name, space)) |id| break :blk id;
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
            if (ref.symbol != .none) use_ranges[@backingInt(ref.symbol)].len += 1;
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
            const r = &use_ranges[@backingInt(ref.symbol)];
            use_ids[r.start + r.len] = @fromBackingInt(@intCast(i));
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
            .next_bodies = self.next_bodies.items,
            .node_scopes = node_scopes,
            .node_parents = node_parents,
            .node_symbols = node_symbols,
            .node_references = node_references,
        };
    }
};

/// The symbol the pending declaration of `name` merges into, the scope's own binding or an
/// export another body of its namespace shares.
pub fn prior(tracker: *const SymbolTracker, name: []const u8, scopes: []const sc.Scope) ?SymbolId {
    const target = tracker.pendingTarget();
    return tracker.binding(target, name) orelse tracker.sharedExport(target, name, scopes);
}

/// Whether `pending` cannot merge into a symbol with `flags`.
pub fn conflicts(pending: SymbolTracker.PendingBinding, flags: Symbol.Flags) bool {
    var excludes = pending.excludes;
    // a class and a function merge only when the class is ambient
    if (pending.flags.class and pending.flags.ambient) excludes.function = false;
    if (pending.flags.function and flags.class and flags.ambient) excludes.class = false;
    return flags.intersects(excludes);
}

// infer variables exist only in their conditional's true branch, class type
// parameters are hidden in static members (TS2302) and computed keys (TS2467)
fn visibleAt(
    tree: *const ast.Tree,
    sym: Symbol,
    ref: *const Reference,
    scopes: sc.ScopeTree,
    node_parents: []const ast.NodeIndex,
) bool {
    if (sym.flags.type_parameter) {
        const scope_node = scopes.get(sym.scope).node;
        return switch (tree.data(scope_node)) {
            .ts_conditional_type => |cond| {
                return inSubtree(node_parents, ref.node, scope_node, cond.true_type);
            },
            .class, .ts_interface_declaration => {
                return !typeParameterHidden(tree, node_parents, ref.node, scope_node);
            },
            else => true,
        };
    }
    if (!ref.flags.type_position) return true;
    if (sym.flags.parameter) {
        const type_parameters = typeParametersOf(tree, scopes.get(sym.scope).node);
        if (type_parameters == .null) return true;
        const span = tree.span(type_parameters);
        const at = tree.span(ref.node).start;
        return at < span.start or at >= span.end;
    }
    if (sym.flags.isHoistingVar()) {
        const body = switch (tree.data(scopes.get(sym.scope).node)) {
            inline .function, .arrow_function_expression => |f| f.body,
            else => return true,
        };
        const span = tree.span(body);
        const at = tree.span(ref.node).start;
        return at >= span.start and at < span.end;
    }
    return true;
}

fn padTo(
    comptime T: type,
    list: *std.ArrayList(T),
    allocator: Allocator,
    count: usize,
    value: T,
) Allocator.Error!void {
    if (list.items.len < count) try list.appendNTimes(allocator, value, count - list.items.len);
}

fn typeParametersOf(tree: *const ast.Tree, node: ast.NodeIndex) ast.NodeIndex {
    return switch (tree.data(node)) {
        inline .function,
        .arrow_function_expression,
        .ts_function_type,
        .ts_constructor_type,
        .ts_method_signature,
        .ts_call_signature_declaration,
        .ts_construct_signature_declaration,
        => |signature| signature.type_parameters,
        else => .null,
    };
}

fn inSubtree(
    node_parents: []const ast.NodeIndex,
    node: ast.NodeIndex,
    root: ast.NodeIndex,
    subtree: ast.NodeIndex,
) bool {
    var child = node;
    var parent = node_parents[@backingInt(child)];
    while (parent != .null) : ({
        child = parent;
        parent = node_parents[@backingInt(parent)];
    }) {
        if (parent == root) return child == subtree;
    }
    return false;
}

fn typeParameterHidden(
    tree: *const ast.Tree,
    node_parents: []const ast.NodeIndex,
    ref_node: ast.NodeIndex,
    owner: ast.NodeIndex,
) bool {
    var child = ref_node;
    var parent = node_parents[@backingInt(child)];
    while (parent != .null) : ({
        child = parent;
        parent = node_parents[@backingInt(parent)];
    }) {
        switch (tree.data(parent)) {
            .class => |cls| if (parent == owner and child == cls.super_class) return true,
            inline .method_definition,
            .property_definition,
            .ts_property_signature,
            .ts_method_signature,
            => |member| {
                if (member.computed and member.key == child and
                    memberOwner(node_parents, parent) == owner) return true;
            },
            .class_body => {
                if (node_parents[@backingInt(parent)] != owner) continue;
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
    const body = node_parents[@backingInt(member)];
    return if (body == .null) .null else node_parents[@backingInt(body)];
}

// the conditional whose extends clause holds `infer_node`
fn inferScope(
    tree: *const ast.Tree,
    scope: *const sc.ScopeTracker,
    infer_node: ast.NodeIndex,
) sc.ScopeId {
    const at = tree.span(infer_node).start;
    var it = scope.ancestors(scope.current);
    while (it.next()) |id| {
        switch (tree.data(scope.get(id).node)) {
            .ts_conditional_type => |cond| {
                const extends = tree.span(cond.extends_type);
                if (at >= extends.start and at < extends.end) return id;
            },
            else => {},
        }
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
    var current = name;
    while (true) switch (tree.data(current)) {
        .jsx_identifier => return current,
        .jsx_member_expression => |m| current = m.object,
        else => return null,
    };
}

fn exprNameScope(scope: *const sc.ScopeTracker) sc.ScopeId {
    const current = scope.currentScope();
    if (current.kind == .function or current.kind == .class) return current.parent;
    return scope.current;
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
    const block = switch (tree.data(body_node)) {
        .ts_module_block => |b| b,
        else => return true,
    };
    const statements = tree.extra(block.body);
    for (statements) |stmt| {
        if (isInstantiatingStatement(tree, stmt, statements)) return true;
    }
    return false;
}

fn isInstantiatingStatement(
    tree: *const ast.Tree,
    idx: ast.NodeIndex,
    statements: []const ast.NodeIndex,
) bool {
    return switch (tree.data(idx)) {
        .ts_interface_declaration,
        .ts_type_alias_declaration,
        .ts_import_equals_declaration,
        => false,
        // const enums included, as in tsc
        .ts_enum_declaration => true,
        .ts_module_declaration => |m| isNamespaceInstantiated(tree, m.body),
        .export_named_declaration => |e| {
            // `export declare` is a type-only export of a value
            if (e.declaration != .null) {
                if (tree.data(e.declaration) == .ts_import_equals_declaration) return true;
                return isInstantiatingStatement(tree, e.declaration, statements);
            }
            if (e.export_kind == .type) return false;
            if (e.source != .null) return true;
            return exportsValue(tree, e.specifiers, statements);
        },
        else => true,
    };
}

// `export { x }` exports a value unless x is declared only as types
fn exportsValue(
    tree: *const ast.Tree,
    specifiers: ast.IndexRange,
    statements: []const ast.NodeIndex,
) bool {
    for (tree.extra(specifiers)) |specifier| {
        const spec = tree.data(specifier).export_specifier;
        if (spec.export_kind == .type) continue;
        const name = switch (tree.data(spec.local)) {
            .identifier_reference => |id| tree.string(id.name),
            else => return true,
        };
        var found = false;
        for (statements) |stmt| {
            if (!declaresName(tree, stmt, name)) continue;
            found = true;
            if (tree.data(stmt) == .ts_import_equals_declaration) return true;
            if (isInstantiatingStatement(tree, stmt, statements)) return true;
        }
        if (!found) return true;
    }
    return false;
}

/// Whether a declaration module without export statements exports all but its imports.
pub fn exportsImplicitly(tree: *const ast.Tree) bool {
    if (tree.lang != .dts or !tree.isModule()) return false;
    const body = tree.extra(tree.data(tree.root).program.body);
    return hasModuleSyntax(tree, body) and !hasExportStatement(tree, body);
}

pub fn hasModuleSyntax(tree: *const ast.Tree, statements: []const ast.NodeIndex) bool {
    for (statements) |stmt| {
        switch (tree.data(stmt)) {
            .import_declaration,
            .export_named_declaration,
            .export_default_declaration,
            .export_all_declaration,
            .ts_export_assignment,
            => return true,
            .ts_import_equals_declaration => |decl| {
                if (tree.data(decl.module_reference) == .ts_external_module_reference) return true;
            },
            else => {},
        }
    }
    return false;
}

// `export default function` is a declaration
fn hasExportStatement(tree: *const ast.Tree, statements: []const ast.NodeIndex) bool {
    for (statements) |stmt| {
        switch (tree.data(stmt)) {
            .export_named_declaration => |e| if (e.declaration == .null) return true,
            .export_all_declaration, .ts_export_assignment => return true,
            .export_default_declaration => |e| switch (tree.data(e.declaration)) {
                .function => |func| if (func.type == .function_expression) return true,
                .class => |cls| if (cls.type == .class_expression) return true,
                .ts_interface_declaration => {},
                else => return true,
            },
            else => {},
        }
    }
    return false;
}

fn declaresName(tree: *const ast.Tree, stmt: ast.NodeIndex, name: []const u8) bool {
    const id = switch (tree.data(stmt)) {
        inline .function,
        .class,
        .ts_interface_declaration,
        .ts_type_alias_declaration,
        .ts_enum_declaration,
        .ts_module_declaration,
        .ts_import_equals_declaration,
        => |decl| decl.id,
        .variable_declaration => |decl| {
            for (tree.extra(decl.declarators)) |declarator| {
                const target = tree.data(declarator).variable_declarator.id;
                if (namedIdentifier(tree, target, name)) return true;
            }
            return false;
        },
        .export_named_declaration => |e| {
            return e.declaration != .null and declaresName(tree, e.declaration, name);
        },
        else => return false,
    };
    return namedIdentifier(tree, id, name);
}

fn namedIdentifier(tree: *const ast.Tree, node: ast.NodeIndex, name: []const u8) bool {
    if (node == .null) return false;
    return switch (tree.data(node)) {
        .binding_identifier => |id| std.mem.eql(u8, tree.string(id.name), name),
        else => false,
    };
}
