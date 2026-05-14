const std = @import("std");
const ast = @import("ast.zig");
const sc = @import("scope.zig");
const String = ast.String;

const Allocator = std.mem.Allocator;

/// Identifier for a `Symbol` in a `SymbolTable`. `.none` means absent.
pub const SymbolId = enum(u32) { none = std.math.maxInt(u32), _ };

/// Identifier for a `Reference` in a `SymbolTable`. `.none` means absent.
pub const ReferenceId = enum(u32) { none = std.math.maxInt(u32), _ };

const ScopeMap = std.StringHashMapUnmanaged(SymbolId);

/// A `(start, len)` window into a backing slice.
pub const Range = struct { start: u32, len: u32 };

/// A declared binding. `flags` describes which spaces (value, type,
/// namespace) and modifiers it occupies. `decls` indexes
/// `SymbolTable.decl_nodes` and lists every declarator of the symbol.
pub const Symbol = struct {
    name: String,
    flags: Flags,
    scope: sc.ScopeId,
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

        /// Returns the union of two flag sets: every flag set in either
        /// `a` or `b` ends up set in the result. Used when merging two
        /// compatible declarations into a single symbol.
        pub inline fn merge(a: Flags, b: Flags) Flags {
            return @bitCast(@as(u32, @bitCast(a)) | @as(u32, @bitCast(b)));
        }

        /// True for a `var` that hoists past intermediate blocks.
        /// Parameters and catch variables are function-scoped but do
        /// not hoist, so they return false here.
        pub inline fn isHoistingVar(self: Flags) bool {
            return self.function_scoped_var and !self.parameter and !self.catch_var;
        }

        /// True when this is a JavaScript value-space binding (visible
        /// at runtime): `var`/`let`/`const`, function, class, enum, or
        /// an instantiated namespace.
        pub inline fn inValueSpace(self: Flags) bool {
            return self.intersects(value_space);
        }

        /// True when this is a TypeScript type-space binding: interface,
        /// type alias, type parameter, plus class and enum, which exist
        /// in both spaces.
        pub inline fn inTypeSpace(self: Flags) bool {
            return self.intersects(type_space);
        }

        /// True for declarations a hoisting `var` is forbidden to pass
        /// through: block-scoped bindings (`let`, `const`), classes,
        /// and functions.
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

    // Declarations that live in JS value space (visible at runtime)
    const value_space: Flags = .{
        .function_scoped_var = true,
        .block_scoped_var = true,
        .function = true,
        .class = true,
        .regular_enum = true,
        .const_enum = true,
        .value_module = true,
    };

    // Declarations that live in TS type space
    const type_space: Flags = .{
        .class = true,
        .regular_enum = true,
        .const_enum = true,
        .interface = true,
        .type_alias = true,
        .type_parameter = true,
    };

    // Names a hoisted `var` cannot pass through
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

        // type parameters merge silently with other type parameters
        // (multiple `infer T` in the same conditional unify per ts).
        // duplicate explicit `<T, T>` is caught structurally in the
        // semantic checker, not via excludes.
        pub const type_parameter: Flags = blk: {
            var f = type_space;
            f.type_parameter = false;
            break :blk f;
        };
    };
};

/// A use of a name. One entry per `identifier_reference` in the
/// source. Declaration sites live in `Symbol.decls`.
pub const Reference = struct {
    name: String,
    scope: sc.ScopeId,
    node: ast.NodeIndex,
    /// `.value` for runtime uses, `.type` for type-position uses
    /// (annotations, `extends`, `implements`, type arguments).
    kind: Kind = .value,

    pub const Kind = enum(u1) { value, type };
};

/// Immutable result of a semantic walk. Holds every symbol declared,
/// every reference recorded, and the per-scope binding maps. Call
/// `resolveAll` to build the cross-index between symbols and
/// references.
pub const SymbolTable = struct {
    symbols: []const Symbol,
    references: []const Reference,
    /// Flat array of declarator nodes indexed by `Symbol.decls`.
    decl_nodes: []const ast.NodeIndex,
    scope_maps: []const ScopeMap,
    hoisting_variables: []const ScopeMap,
    strings: *const ast.StringPool,
    allocator: Allocator,

    resolutions: []const SymbolId = &.{},
    symbol_refs: []const ReferenceId = &.{},
    symbol_ref_ranges: []const Range = &.{},
    unresolved_refs: []const ReferenceId = &.{},

    /// A `(id, symbol)` pair yielded by `iterSymbols`.
    pub const SymbolEntry = struct { id: SymbolId, symbol: Symbol };

    /// A `(id, reference)` pair yielded by `iterReferences` and `iterUnresolved`.
    pub const ReferenceEntry = struct { id: ReferenceId, reference: Reference };

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

    /// Yields every unresolved `(id, reference)` pair. Only valid after
    /// `resolveAll` has run.
    pub const UnresolvedIterator = struct {
        references: []const Reference,
        ids: []const ReferenceId,
        index: u32 = 0,

        pub fn next(self: *UnresolvedIterator) ?ReferenceEntry {
            if (self.index >= self.ids.len) return null;
            const id = self.ids[self.index];
            self.index += 1;
            return .{ .id = id, .reference = self.references[@intFromEnum(id)] };
        }
    };

    /// Yields the node index of every `identifier_reference` resolved
    /// to a symbol.
    pub const UseIterator = struct {
        references: []const Reference,
        ids: []const ReferenceId,
        index: u32 = 0,

        pub fn next(self: *UseIterator) ?ast.NodeIndex {
            if (self.index >= self.ids.len) return null;
            const ref = self.references[@intFromEnum(self.ids[self.index])];
            self.index += 1;
            return ref.node;
        }
    };

    /// Yields every node index bound to a symbol. Declaration sites
    /// come first in source order, then use sites.
    pub const SiteIterator = struct {
        decls: []const ast.NodeIndex,
        references: []const Reference,
        use_ids: []const ReferenceId,
        decl_index: u32 = 0,
        use_index: u32 = 0,

        pub fn next(self: *SiteIterator) ?ast.NodeIndex {
            if (self.decl_index < self.decls.len) {
                const node = self.decls[self.decl_index];
                self.decl_index += 1;
                return node;
            }
            if (self.use_index >= self.use_ids.len) return null;
            const node = self.references[@intFromEnum(self.use_ids[self.use_index])].node;
            self.use_index += 1;
            return node;
        }
    };

    /// Returns the symbol for the given id.
    pub inline fn getSymbol(self: SymbolTable, id: SymbolId) Symbol {
        return self.symbols[@intFromEnum(id)];
    }

    /// Returns the reference for the given id.
    pub inline fn getReference(self: SymbolTable, id: ReferenceId) Reference {
        return self.references[@intFromEnum(id)];
    }

    /// Iterator over every `(id, symbol)` pair in the table.
    pub fn iterSymbols(self: SymbolTable) SymbolIterator {
        return .{ .symbols = self.symbols };
    }

    /// Iterator over every `(id, reference)` pair in source order.
    pub fn iterReferences(self: SymbolTable) ReferenceIterator {
        return .{ .references = self.references };
    }

    /// Iterator over `(id, reference)` pairs that did not resolve to any
    /// symbol. Only valid after `resolveAll` has run.
    pub fn iterUnresolved(self: SymbolTable) UnresolvedIterator {
        return .{ .references = self.references, .ids = self.unresolved_refs };
    }

    /// Iterator over symbol ids declared directly in `scope` (excluding
    /// hoisted vars passing through).
    pub fn scopeSymbols(self: SymbolTable, scope: sc.ScopeId) ScopeMap.ValueIterator {
        return self.scope_maps[@intFromEnum(scope)].valueIterator();
    }

    /// Looks up `name` declared directly in `scope`. Returns `null` if
    /// not found.
    pub fn findInScope(self: SymbolTable, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        return self.scope_maps[@intFromEnum(scope)].get(name);
    }

    /// Like `findInScope`, but also matches a hoisting `var` passing
    /// through `scope` on its way to its target.
    pub fn findInScopeOrHoisted(self: SymbolTable, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        return self.findInScope(scope, name) orelse
            self.hoisting_variables[@intFromEnum(scope)].get(name);
    }

    /// Walks up the scope chain from `scope` to find the nearest binding
    /// of `name`, including hoisted vars in any visited scope.
    pub fn resolve(self: SymbolTable, scope_tree: sc.ScopeTree, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        var it = scope_tree.ancestors(scope);
        while (it.next()) |ancestor| {
            if (self.findInScopeOrHoisted(ancestor, name)) |id| return id;
        }
        return null;
    }

    /// Resolves every reference to its declaring symbol and builds the
    /// reverse index. After this returns:
    ///   - `referenceSymbol(ref_id)` gives the symbol a reference resolves to.
    ///   - `symbolUses(sym_id)` walks the use sites of a symbol.
    ///   - `symbolSites(sym_id)` walks declarations and uses together.
    ///   - `iterUnresolved()` walks references that did not resolve.
    /// `symbolDecls` is available without calling `resolveAll`.
    pub fn resolveAll(self: *SymbolTable, scope_tree: sc.ScopeTree) Allocator.Error!void {
        const allocator = self.allocator;
        const ref_count: u32 = @intCast(self.references.len);
        const sym_count: u32 = @intCast(self.symbols.len);

        if (ref_count == 0) return;

        const resolutions = try allocator.alloc(SymbolId, ref_count);

        for (self.references, resolutions) |ref, *out| {
            const name = self.strings.get(ref.name);
            const pctx = PrehashCtx{ .h = std.hash.Wyhash.hash(0, name) };
            out.* = blk: {
                var it = scope_tree.ancestors(ref.scope);
                while (it.next()) |ancestor| {
                    const idx = @intFromEnum(ancestor);
                    if (self.scope_maps[idx].getAdapted(name, pctx)) |id| break :blk id;
                }
                break :blk .none;
            };
        }

        const ranges = try allocator.alloc(Range, sym_count);
        for (ranges) |*r| r.* = .{ .start = 0, .len = 0 };
        for (resolutions) |sym_id| {
            if (sym_id != .none) ranges[@intFromEnum(sym_id)].len += 1;
        }

        var offset: u32 = 0;
        for (ranges) |*r| {
            r.start = offset;
            offset += r.len;
            r.len = 0;
        }

        const symbol_refs = try allocator.alloc(ReferenceId, offset);
        const unresolved = try allocator.alloc(ReferenceId, ref_count - offset);
        var unresolved_cursor: u32 = 0;

        for (resolutions, 0..) |resolved, i| {
            const ref_id: ReferenceId = @enumFromInt(@as(u32, @intCast(i)));
            if (resolved != .none) {
                const r = &ranges[@intFromEnum(resolved)];
                symbol_refs[r.start + r.len] = ref_id;
                r.len += 1;
            } else {
                unresolved[unresolved_cursor] = ref_id;
                unresolved_cursor += 1;
            }
        }

        self.resolutions = resolutions;
        self.symbol_refs = symbol_refs;
        self.symbol_ref_ranges = ranges;
        self.unresolved_refs = unresolved;
    }

    /// The symbol a reference resolves to, or `.none` if the reference
    /// is unresolved. Only valid after `resolveAll` has run.
    pub inline fn referenceSymbol(self: SymbolTable, id: ReferenceId) SymbolId {
        const idx = @intFromEnum(id);
        if (idx >= self.resolutions.len) return .none;
        return self.resolutions[idx];
    }

    /// Every declaration site of `id`, in declaration order.
    pub fn symbolDecls(self: SymbolTable, id: SymbolId) []const ast.NodeIndex {
        const range = self.symbols[@intFromEnum(id)].decls;
        return self.decl_nodes[range.start..][0..range.len];
    }

    /// Iterates the use sites of `id`.
    pub fn symbolUses(self: SymbolTable, id: SymbolId) UseIterator {
        return .{ .references = self.references, .ids = self.useIds(id) };
    }

    /// The number of use sites of `id`.
    pub fn symbolUsesCount(self: SymbolTable, id: SymbolId) u32 {
        return @intCast(self.useIds(id).len);
    }

    /// The number of declaration sites of `id`.
    pub fn symbolDeclsCount(self: SymbolTable, id: SymbolId) u32 {
        return @intCast(self.symbolDecls(id).len);
    }

    /// Iterates every site of `id`. Declarations come first in source
    /// order, then use sites.
    pub fn symbolSites(self: SymbolTable, id: SymbolId) SiteIterator {
        return .{
            .decls = self.symbolDecls(id),
            .references = self.references,
            .use_ids = self.useIds(id),
        };
    }

    /// True when `id` has at least one use.
    pub fn isReferenced(self: SymbolTable, id: SymbolId) bool {
        return self.useIds(id).len > 0;
    }

    inline fn useIds(self: SymbolTable, id: SymbolId) []const ReferenceId {
        const idx = @intFromEnum(id);
        if (idx >= self.symbol_ref_ranges.len) return &.{};
        const range = self.symbol_ref_ranges[idx];
        return self.symbol_refs[range.start..][0..range.len];
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

/// Collects symbols and references during the AST walk
pub const SymbolTracker = struct {
    tree: *const ast.Tree,
    allocator: Allocator,
    symbols: std.ArrayList(Symbol) = .empty,
    references: std.ArrayList(Reference) = .empty,
    decl_pairs: std.ArrayList(DeclPair) = .empty,
    scope_maps: std.ArrayList(ScopeMap) = .empty,
    hoisting_variables: std.ArrayList(ScopeMap) = .empty,

    binding_flags: Symbol.Flags = .{},
    binding_excludes: Symbol.Flags = .{},
    target: sc.ScopeId = .root,
    /// Whether the next `binding_identifier` is the directly-exported
    /// name of an `export` declaration.
    export_state: ExportState = .none,

    saved_stack: std.ArrayList(SavedContext) = .empty,

    pub const ExportState = enum { none, named, default };

    const DeclPair = struct { sid: SymbolId, node: ast.NodeIndex };

    /// Snapshot of the four context fields, taken before a mutator
    /// rewrites them so the mutator's exit can restore the prior state.
    pub const SavedContext = struct {
        flags: Symbol.Flags,
        excludes: Symbol.Flags,
        target: sc.ScopeId,
        export_state: ExportState,
    };

    pub fn init(tree: *ast.Tree) Allocator.Error!SymbolTracker {
        const alloc = tree.allocator();
        var self = SymbolTracker{ .tree = tree, .allocator = alloc };

        const estimated: u32 = @max(16, @as(u32, @intCast(tree.nodes.len / 32)));
        try self.symbols.ensureTotalCapacity(alloc, estimated);
        try self.references.ensureTotalCapacity(alloc, estimated);
        try self.decl_pairs.ensureTotalCapacity(alloc, estimated);
        try self.scope_maps.ensureTotalCapacity(alloc, estimated / 2);
        try self.hoisting_variables.ensureTotalCapacity(alloc, estimated / 2);
        try self.saved_stack.ensureTotalCapacity(alloc, 32);
        return self;
    }

    /// Records the binding context for the next `binding_identifier`.
    /// Called from `Ctx.enter` for every node so parent declaration
    /// nodes can configure flags, excludes, target scope, and export
    /// state before the child binding identifier fires.
    pub fn setBindingContext(self: *SymbolTracker, data: ast.NodeData, scope: *const sc.ScopeTracker) Allocator.Error!void {
        switch (data) {
            .export_named_declaration => |decl| {
                // Declaration form (`export type X = Y`, `export const x = 1`,
                // `export interface I {}`, ...) always exports the binding it
                // declares, even when `export_kind` is `.type` (which the
                // parser sets automatically for type-aliases and interfaces).
                // Pure re-export form (`export { Foo }` / `export type { Foo }`)
                // has `declaration == .null`, no inner binding, and stays
                // dependent on `export_kind` for the `.named` value-export
                // case so we don't tag the value-side as exported for a
                // type-only re-export.
                if (decl.declaration != .null or decl.export_kind != .type) {
                    self.export_state = .named;
                }
            },
            .export_default_declaration => self.export_state = .default,

            .variable_declaration => |decl| {
                try self.pushSavedContext();
                switch (decl.kind) {
                    .@"var" => {
                        self.binding_flags = .{ .function_scoped_var = true };
                        self.binding_excludes = Symbol.Excludes.function_var;
                        self.target = scope.currentHoistScopeId();
                    },
                    .@"const", .using, .await_using => {
                        self.binding_flags = .{ .block_scoped_var = true, .const_var = true };
                        self.binding_excludes = Symbol.Excludes.block_var;
                        self.target = scope.currentScopeId();
                    },
                    .let => {
                        self.binding_flags = .{ .block_scoped_var = true };
                        self.binding_excludes = Symbol.Excludes.block_var;
                        self.target = scope.currentScopeId();
                    },
                }
            },

            .function => |func| {
                try self.pushSavedContext();
                const ambient = func.declare or
                    func.type == .ts_declare_function or
                    func.type == .ts_empty_body_function_expression;
                self.binding_flags = .{ .function = true, .ambient = ambient };
                const is_decl = func.type == .function_declaration or func.type == .ts_declare_function;
                self.target = if (is_decl) scope.currentScope().parent else exprNameScope(scope);

                // ts overloads, sloppy js annex b 3.2, and global merge
                // with var. lexical scopes (block/module) are not.
                const k = scope.getScope(self.target).kind;
                const allow_overload = self.tree.isTs() or k == .function or k == .global or k == .static_block;
                self.binding_excludes = if (allow_overload) Symbol.Excludes.function else Symbol.Excludes.block_var;

                // expression names are local
                if (!is_decl) self.export_state = .none;
            },

            .class => |cls| {
                try self.pushSavedContext();
                self.binding_flags = .{ .class = true, .ambient = cls.declare };
                self.binding_excludes = Symbol.Excludes.class;
                const is_decl = cls.type == .class_declaration;
                self.target = if (is_decl) scope.currentScope().parent else exprNameScope(scope);
                // expression names are local
                if (!is_decl) self.export_state = .none;
            },

            .formal_parameters => {
                try self.pushSavedContext();
                self.binding_flags = .{
                    .function_scoped_var = true,
                    .parameter = true,
                    .ambient = self.binding_flags.ambient,
                };
                self.binding_excludes = Symbol.Excludes.parameter;
                self.target = scope.currentScopeId();
                self.export_state = .none;
            },

            // members are not the exported binding
            .class_body, .ts_module_block => self.export_state = .none,

            inline .import_declaration, .ts_import_equals_declaration => |decl| {
                try self.pushSavedContext();
                self.binding_flags = if (decl.import_kind == .type)
                    .{ .type_import = true }
                else
                    .{ .import = true };
                self.binding_excludes = Symbol.Excludes.import_binding;
                self.target = scope.currentScopeId();
            },

            .import_specifier => |spec| {
                try self.pushSavedContext();
                if (spec.import_kind == .type) {
                    self.binding_flags = .{ .type_import = true };
                    self.binding_excludes = Symbol.Excludes.import_binding;
                }
            },

            .catch_clause => {
                try self.pushSavedContext();
                self.binding_flags = .{ .function_scoped_var = true, .catch_var = true };
                self.binding_excludes = Symbol.Excludes.catch_param;
                self.target = scope.currentScopeId();
            },

            // the id binds in the surrounding scope. type-parameter
            // and body bindings live in a scope pushed by the tracker.
            .ts_interface_declaration => |decl| {
                try self.pushSavedContext();
                self.binding_flags = .{ .interface = true, .ambient = decl.declare };
                self.binding_excludes = Symbol.Excludes.interface;
                self.target = scope.currentScope().parent;
            },

            .ts_type_alias_declaration => |decl| {
                try self.pushSavedContext();
                self.binding_flags = .{ .type_alias = true, .ambient = decl.declare };
                self.binding_excludes = Symbol.Excludes.type_alias;
                self.target = scope.currentScope().parent;
            },

            .ts_enum_declaration => |decl| {
                try self.pushSavedContext();
                if (decl.is_const) {
                    self.binding_flags = .{ .const_enum = true, .ambient = decl.declare };
                    self.binding_excludes = Symbol.Excludes.const_enum;
                } else {
                    self.binding_flags = .{ .regular_enum = true, .ambient = decl.declare };
                    self.binding_excludes = Symbol.Excludes.regular_enum;
                }
                self.target = scope.currentScopeId();
            },

            .ts_module_declaration => |decl| {
                try self.pushSavedContext();
                // type-only bodies (interface/type alias/etc.) don't
                // occupy value space
                const instantiated = isNamespaceInstantiated(self.tree, decl.body);
                self.binding_flags = .{
                    .value_module = instantiated,
                    .namespace_module = true,
                    .ambient = decl.declare,
                };
                self.binding_excludes = if (instantiated)
                    Symbol.Excludes.value_module
                else
                    Symbol.Excludes.namespace_module;
                self.target = scope.currentScopeId();
            },

            .ts_namespace_export_declaration => {
                try self.pushSavedContext();
                self.binding_flags = .{ .namespace_module = true };
                self.binding_excludes = Symbol.Excludes.namespace_module;
                self.target = scope.currentScopeId();
            },

            // ts_type_parameter wraps `<T>` and `infer U`. mapped type
            // keys `[K in T]` are bare binding_identifiers under
            // ts_mapped_type, so the same context applies.
            .ts_type_parameter, .ts_mapped_type => {
                try self.pushSavedContext();
                self.binding_flags = .{ .type_parameter = true };
                self.binding_excludes = Symbol.Excludes.type_parameter;
                self.target = scope.currentScopeId();
                self.export_state = .none;
            },

            else => {},
        }
    }

    inline fn pushSavedContext(self: *SymbolTracker) Allocator.Error!void {
        try self.saved_stack.append(self.allocator, .{
            .flags = self.binding_flags,
            .excludes = self.binding_excludes,
            .target = self.target,
            .export_state = self.export_state,
        });
    }

    /// Materializes the pending binding context into a symbol (for
    /// `binding_identifier`) or records a reference (for
    /// `identifier_reference`). Called from `Ctx.post_enter` for every
    /// node.
    pub fn declareBindings(
        self: *SymbolTracker,
        index: ast.NodeIndex,
        data: ast.NodeData,
        scope: *const sc.ScopeTracker,
        in_type_position: bool,
    ) Allocator.Error!void {
        if (self.scope_maps.items.len < scope.scopes.items.len)
            try self.syncScopeMaps(scope);

        switch (data) {
            .binding_identifier => |id| {
                // type-position identifiers are parameter labels (function
                // type, index signature, etc.). only type parameters are
                // real declarations here.
                if (in_type_position and !self.binding_flags.type_parameter) return;

                const sym_id = try self.declare(id.name, self.binding_flags, self.binding_excludes, self.target, index);

                // register the hoisting var in each block it passes
                // through so block-scoped redeclarations see it
                if (self.binding_flags.isHoistingVar()) {
                    var iter = scope.ancestors(scope.currentScopeId());
                    while (iter.next()) |s| {
                        if (s == self.target) break;
                        const gop = try self.hoisting_variables.items[@intFromEnum(s)].getOrPut(self.allocator, self.tree.string(id.name));
                        if (!gop.found_existing) gop.value_ptr.* = sym_id;
                    }
                }
            },
            .identifier_reference => |id| {
                const kind: Reference.Kind = if (in_type_position) .type else .value;
                _ = try self.addReference(id.name, scope.currentScopeId(), index, kind);
            },
            // `param is T` / `asserts param is T`: the `param` is parsed as
            // an `identifier_name` (the AST shape downstream FFI consumers
            // expect) but it semantically references the surrounding
            // function's parameter binding. Synthesize the reference here so
            // renamers update both the parameter and the predicate together.
            .ts_type_predicate => |pred| {
                if (pred.parameter_name == .null) return;
                const pname = self.tree.data(pred.parameter_name);
                if (pname != .identifier_name) return;
                const kind: Reference.Kind = if (in_type_position) .type else .value;
                _ = try self.addReference(
                    pname.identifier_name.name,
                    scope.currentScopeId(),
                    pred.parameter_name,
                    kind,
                );
            },
            // JSX tag names: `<Foo>`, `<Foo.Bar>`, `</Foo>`. The leftmost
            // `jsx_identifier` is a JS binding reference (capitalized). The
            // rest of the chain (`.Bar`, `:path`) is property/namespace
            // syntax and not a binding. Lowercase tags (`<div>`) are
            // intrinsic HTML element names, not bindings; skip them.
            inline .jsx_opening_element, .jsx_closing_element => |el| {
                if (jsxTagRoot(self.tree, el.name)) |root_idx| {
                    const id = self.tree.data(root_idx).jsx_identifier;
                    const text = self.tree.string(id.name);
                    if (text.len > 0 and text[0] >= 'A' and text[0] <= 'Z') {
                        _ = try self.addReference(id.name, scope.currentScopeId(), root_idx, .value);
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
                    self.binding_flags = saved.flags;
                    self.binding_excludes = saved.excludes;
                    self.target = saved.target;
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
            .ts_namespace_export_declaration,
            .ts_type_parameter,
            .ts_mapped_type,
            => {
                if (self.saved_stack.pop()) |saved| {
                    self.binding_flags = saved.flags;
                    self.binding_excludes = saved.excludes;
                    self.target = saved.target;
                    self.export_state = saved.export_state;
                }
            },

            .export_named_declaration, .export_default_declaration => self.export_state = .none,

            else => {},
        }
    }

    /// Declares a binding in `target`. If the name is already bound
    /// there with non-conflicting flags, merges into the existing
    /// symbol. If the name conflicts, keeps the existing symbol
    /// unchanged (the caller emits a diagnostic). Otherwise creates
    /// a fresh symbol. `node` is always recorded as a declarator.
    pub fn declare(
        self: *SymbolTracker,
        name: String,
        flags: Symbol.Flags,
        excludes: Symbol.Flags,
        target: sc.ScopeId,
        node: ast.NodeIndex,
    ) Allocator.Error!SymbolId {
        const name_str = self.tree.string(name);
        const target_idx = @intFromEnum(target);

        const id = if (self.scope_maps.items[target_idx].get(name_str) orelse
            self.hoisting_variables.items[target_idx].get(name_str)) |existing|
        sid: {
            const sym = &self.symbols.items[@intFromEnum(existing)];
            if (!sym.flags.intersects(excludes)) {
                var merged = sym.flags.merge(flags);
                merged.exported = merged.exported or self.export_state != .none;
                merged.is_default = merged.is_default or self.export_state == .default;
                // `function f(): T; function f() {}` is non-ambient
                // because the implementation emits at runtime
                merged.ambient = sym.flags.ambient and flags.ambient;
                sym.flags = merged;
            }
            break :sid existing;
        } else sid: {
            const new_id = try self.append(name, flags, target);
            try self.scope_maps.items[target_idx].put(self.allocator, name_str, new_id);
            break :sid new_id;
        };

        try self.decl_pairs.append(self.allocator, .{ .sid = id, .node = node });
        return id;
    }

    /// Records an identifier reference in `scope`. The kind tags it as
    /// value-position or type-position for rename-aware tooling.
    pub fn addReference(
        self: *SymbolTracker,
        name: String,
        scope: sc.ScopeId,
        node: ast.NodeIndex,
        kind: Reference.Kind,
    ) Allocator.Error!ReferenceId {
        const id: ReferenceId = @enumFromInt(@as(u32, @intCast(self.references.items.len)));
        try self.references.append(self.allocator, .{
            .name = name,
            .scope = scope,
            .node = node,
            .kind = kind,
        });
        return id;
    }

    fn append(
        self: *SymbolTracker,
        name: String,
        flags: Symbol.Flags,
        target: sc.ScopeId,
    ) Allocator.Error!SymbolId {
        const id: SymbolId = @enumFromInt(@as(u32, @intCast(self.symbols.items.len)));
        var stored = flags;
        stored.exported = self.export_state != .none;
        stored.is_default = self.export_state == .default;
        try self.symbols.append(self.allocator, .{
            .name = name,
            .flags = stored,
            .scope = target,
            .decls = .{ .start = 0, .len = 0 },
        });
        return id;
    }

    /// Flags the next `binding_identifier` will be declared with.
    pub inline fn currentBindingFlags(self: *const SymbolTracker) Symbol.Flags {
        return self.binding_flags;
    }

    /// Excludes the next `binding_identifier` will be checked against.
    pub inline fn currentBindingExcludes(self: *const SymbolTracker) Symbol.Flags {
        return self.binding_excludes;
    }

    /// Scope the next `binding_identifier` will be bound into.
    pub inline fn currentTarget(self: *const SymbolTracker) sc.ScopeId {
        return self.target;
    }

    /// Returns the symbol for the given id.
    pub inline fn getSymbol(self: *const SymbolTracker, id: SymbolId) Symbol {
        return self.symbols.items[@intFromEnum(id)];
    }

    /// First declarator recorded for `id`, or `.null` if none.
    /// Slow path. Post-finalize callers use `symbolDecls(id)[0]`.
    pub fn firstDeclOf(self: *const SymbolTracker, id: SymbolId) ast.NodeIndex {
        for (self.decl_pairs.items) |pair| {
            if (pair.sid == id) return pair.node;
        }
        return .null;
    }

    /// Iterator over symbol ids declared directly in `scope`.
    pub fn scopeSymbols(self: *const SymbolTracker, scope: sc.ScopeId) ScopeMap.ValueIterator {
        return self.scope_maps.items[@intFromEnum(scope)].valueIterator();
    }

    /// Looks up `name` declared directly in `scope`. Returns `null` if
    /// not found, or if `scope` is past the scopes seen so far.
    pub fn findInScope(self: *const SymbolTracker, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        const idx = @intFromEnum(scope);
        if (idx >= self.scope_maps.items.len) return null;
        return self.scope_maps.items[idx].get(name);
    }

    /// Like `findInScope`, but also matches a hoisting `var` passing
    /// through `scope` on its way to its target.
    pub fn findInScopeOrHoisted(self: *const SymbolTracker, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        if (self.findInScope(scope, name)) |id| return id;
        const idx = @intFromEnum(scope);
        if (idx < self.hoisting_variables.items.len) return self.hoisting_variables.items[idx].get(name);
        return null;
    }

    fn syncScopeMaps(self: *SymbolTracker, scope: *const sc.ScopeTracker) Allocator.Error!void {
        const n = scope.scopes.items.len;
        try self.scope_maps.ensureTotalCapacity(self.allocator, n);
        while (self.scope_maps.items.len < n) self.scope_maps.appendAssumeCapacity(.empty);
        try self.hoisting_variables.ensureTotalCapacity(self.allocator, n);
        while (self.hoisting_variables.items.len < n) self.hoisting_variables.appendAssumeCapacity(.empty);
    }

    /// Finalizes the tracker into an immutable `SymbolTable`. The
    /// table aliases the tracker's storage and stays valid for the
    /// lifetime of the source tree.
    pub fn toSymbolTable(self: *SymbolTracker) Allocator.Error!SymbolTable {
        const decl_nodes = try self.allocator.alloc(ast.NodeIndex, self.decl_pairs.items.len);

        // count per symbol
        for (self.symbols.items) |*s| s.decls = .{ .start = 0, .len = 0 };
        for (self.decl_pairs.items) |pair| {
            self.symbols.items[@intFromEnum(pair.sid)].decls.len += 1;
        }

        // prefix sum, reusing len as a write cursor during fill
        var offset: u32 = 0;
        for (self.symbols.items) |*s| {
            const count = s.decls.len;
            s.decls.start = offset;
            s.decls.len = 0;
            offset += count;
        }

        // fill
        for (self.decl_pairs.items) |pair| {
            const s = &self.symbols.items[@intFromEnum(pair.sid)];
            decl_nodes[s.decls.start + s.decls.len] = pair.node;
            s.decls.len += 1;
        }

        return .{
            .symbols = self.symbols.items,
            .references = self.references.items,
            .decl_nodes = decl_nodes,
            .scope_maps = self.scope_maps.items,
            .hoisting_variables = self.hoisting_variables.items,
            .strings = &self.tree.strings,
            .allocator = self.allocator,
        };
    }
};

// Walks a JSX opening/closing tag name to its leftmost `jsx_identifier`.
// Returns null for `<svg:path>` (XML namespace, not a JS binding) and for
// anything that isn't a tag-shape expression. The returned NodeIndex always
// points at a `jsx_identifier`.
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
    return if (cur.kind == .function or cur.kind == .class) cur.parent else scope.currentScopeId();
}

// a namespace occupies value space if its body has any
// value-producing statement (var, function, class, non-const enum,
// instantiated nested namespace, value re-export). body-less ambient
// modules are instantiated by spec.
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
