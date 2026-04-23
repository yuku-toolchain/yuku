const std = @import("std");
const ast = @import("../../ast.zig");
const sc = @import("scope.zig");
const String = ast.String;

const Allocator = std.mem.Allocator;

/// ID for a symbol. `.none` means no symbol.
pub const SymbolId = enum(u32) { none = std.math.maxInt(u32), _ };

/// Per-scope hash map from name to symbol ID.
const ScopeMap = std.StringHashMapUnmanaged(SymbolId);

/// ID for a reference. `.none` means no reference.
pub const ReferenceId = enum(u32) { none = std.math.maxInt(u32), _ };

/// A declared binding (variable, function, class, import, or parameter).
pub const Symbol = struct {
    /// Name of the symbol.
    name: String,
    kind: Kind,
    flags: Flags,
    /// The scope this symbol is declared in.
    scope: sc.ScopeId,
    node: ast.NodeIndex,

    /// What kind of JavaScript declaration created this symbol.
    pub const Kind = enum(u8) {
        /// `let`, `const`, `using`, `await using`.
        lexical,
        /// `var` (hoists to nearest function/module/global scope).
        hoisted,
        /// Function declaration/expression name.
        function,
        /// Class declaration/expression name.
        class,
        /// Function parameter or catch clause parameter.
        parameter,
        /// Import binding.
        import,

        pub fn toString(self: Kind) []const u8 {
            return switch (self) {
                .lexical => "variable",
                .hoisted => "variable",
                .function => "function",
                .class => "class",
                .parameter => "parameter",
                .import => "import",
            };
        }

        /// Whether this kind is a LexicallyDeclaredName in the given scope.
        ///
        /// `function` declarations are lexical inside blocks and at module
        /// top level, but var-like at function/global scope
        /// (Section 14.2.1, 14.12.1, 16.2.1.6).
        pub fn isBlockScoped(kind: Kind, scope_kind: sc.Scope.Kind) bool {
            return switch (kind) {
                .lexical, .class, .import => true,
                .function => scope_kind == .block or scope_kind == .module,
                .hoisted, .parameter => false,
            };
        }
    };

    /// Per-symbol flags for export status and mutability.
    pub const Flags = packed struct(u8) {
        /// Whether this symbol is exported from the module.
        exported: bool = false,
        /// Whether this is the default export.
        is_default: bool = false,
        /// Whether this is a `const` or `using` binding.
        is_const: bool = false,
        /// Whether this is a TypeScript ambient declaration (`declare`).
        is_ambient: bool = false,
        _padding: u4 = 0,
    };
};

/// An identifier reference (a use of a name, not a declaration).
pub const Reference = struct {
    /// Name of the reference.
    name: String,
    /// The scope this reference appears in (used for resolution).
    scope: sc.ScopeId,
    /// The AST node for this reference.
    node: ast.NodeIndex,
};

/// The result of symbol collection. Contains all symbols and references from the walk.
pub const SymbolTable = struct {
    symbols: []const Symbol,
    references: []const Reference,
    /// Per-scope hash maps, indexed by scope ID.
    scope_maps: []const ScopeMap,
    /// Hoisted vars passing through intermediate block scopes (Section 14.2.1).
    hoisting_variables: []const ScopeMap,
    /// String pool for resolving `String` handles to text.
    strings: *const ast.StringPool,

    // populated by resolveAll()

    /// Forward index, for each reference, the symbol it resolves to (or `.none`).
    /// Parallel to `references`, indexed by `ReferenceId`.
    resolutions: []const SymbolId = &.{},
    /// Flat array of `ReferenceId`s grouped by symbol. Ranges into this
    /// array are stored in `symbol_ref_ranges`.
    symbol_refs: []const ReferenceId = &.{},
    /// Per-symbol range into `symbol_refs`. Indexed by `SymbolId`.
    symbol_ref_ranges: []const Range = &.{},
    /// References that could not be resolved to any symbol.
    unresolved_refs: []const ReferenceId = &.{},

    const Range = struct { start: u32, len: u32 };

    /// Returns the string content for a `String`.
    pub inline fn getString(self: SymbolTable, id: String) []const u8 {
        return self.strings.get(id);
    }

    /// Returns the symbol for the given ID.
    pub inline fn getSymbol(self: SymbolTable, id: SymbolId) Symbol {
        return self.symbols[@intFromEnum(id)];
    }

    /// Returns the reference for the given ID.
    pub inline fn getReference(self: SymbolTable, id: ReferenceId) Reference {
        return self.references[@intFromEnum(id)];
    }

    /// Returns the source name of a symbol as a string slice.
    pub inline fn getName(self: SymbolTable, sym: Symbol) []const u8 {
        return self.getString(sym.name);
    }

    /// Returns the source name of a reference as a string slice.
    pub inline fn getRefName(self: SymbolTable, ref: Reference) []const u8 {
        return self.getString(ref.name);
    }

    /// Returns an iterator over all symbol IDs declared in the given scope.
    pub fn scopeSymbols(self: SymbolTable, scope: sc.ScopeId) ScopeMap.ValueIterator {
        return self.scope_maps[@intFromEnum(scope)].valueIterator();
    }

    /// Searches for a symbol by name in a single scope.
    pub fn findInScope(self: SymbolTable, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        return self.scope_maps[@intFromEnum(scope)].get(name);
    }

    /// Searches for a declared symbol or a hoisted var passing through `scope`.
    pub fn findInScopeOrHoisted(self: SymbolTable, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        if (self.findInScope(scope, name)) |id| return id;
        const idx = @intFromEnum(scope);
        return self.hoisting_variables[idx].get(name);
    }

    /// Walks up the scope chain from `scope` looking for a symbol with `name`.
    pub fn resolve(self: SymbolTable, scope: sc.ScopeId, name: []const u8, scope_tree: sc.ScopeTree) ?SymbolId {
        var it = scope_tree.ancestors(scope);
        while (it.next()) |ancestor| {
            if (self.findInScopeOrHoisted(ancestor, name)) |id| return id;
        }
        return null;
    }

    /// Resolves all references.
    ///
    /// After this call:
    /// - `referenceSymbol(ref_id)` returns the symbol a reference resolves to.
    /// - `symbolReferences(sym_id)` returns all references to a symbol.
    /// - `unresolvedReferences()` returns references with no matching symbol.
    pub fn resolveAll(self: *SymbolTable, allocator: Allocator, scope_tree: sc.ScopeTree) Allocator.Error!void {
        const ref_count: u32 = @intCast(self.references.len);
        const sym_count: u32 = @intCast(self.symbols.len);

        if (ref_count == 0) return;

        // phase 1, resolve every reference
        const resolutions = try allocator.alloc(SymbolId, ref_count);

        for (self.references, 0..) |ref, i| {
            const name = self.getString(ref.name);
            const pctx = PrehashCtx{ .h = std.hash.Wyhash.hash(0, name) };
            resolutions[i] = blk: {
                var it = scope_tree.ancestors(ref.scope);
                while (it.next()) |ancestor| {
                    const idx = @intFromEnum(ancestor);
                    if (self.scope_maps[idx].getAdapted(name, pctx)) |id| break :blk id;
                    if (self.hoisting_variables[idx].getAdapted(name, pctx)) |id| break :blk id;
                }
                break :blk .none;
            };
        }

        // phase 2, build reverse index using Range.len as both
        // counter and scatter cursor
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

        for (0..ref_count) |i| {
            const ref_id: ReferenceId = @enumFromInt(@as(u32, @intCast(i)));
            const resolved = resolutions[i];
            if (resolved != .none) {
                const sym_idx = @intFromEnum(resolved);
                symbol_refs[ranges[sym_idx].start + ranges[sym_idx].len] = ref_id;
                ranges[sym_idx].len += 1;
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

    /// Returns the symbol that a reference resolves to, or `.none` if unresolved.
    pub inline fn referenceSymbol(self: SymbolTable, id: ReferenceId) SymbolId {
        const idx = @intFromEnum(id);
        if (idx >= self.resolutions.len) return .none;
        return self.resolutions[idx];
    }

    /// Returns all reference IDs that resolve to the given symbol.
    pub inline fn symbolReferences(self: SymbolTable, id: SymbolId) []const ReferenceId {
        const idx = @intFromEnum(id);
        if (idx >= self.symbol_ref_ranges.len) return &.{};
        const range = self.symbol_ref_ranges[idx];
        return self.symbol_refs[range.start..][0..range.len];
    }

    /// Returns true if at least one reference resolves to this symbol.
    pub inline fn isReferenced(self: SymbolTable, id: SymbolId) bool {
        const idx = @intFromEnum(id);
        if (idx >= self.symbol_ref_ranges.len) return false;
        return self.symbol_ref_ranges[idx].len > 0;
    }

    /// Returns all reference IDs that could not be resolved to any symbol.
    pub inline fn unresolvedReferences(self: SymbolTable) []const ReferenceId {
        return self.unresolved_refs;
    }
};

/// Pre-computed hash context for string hash map lookups.
const PrehashCtx = struct {
    h: u64,
    pub fn hash(self: @This(), _: []const u8) u64 { return self.h; }
    pub fn eql(_: @This(), a: []const u8, b: []const u8) bool {
        return std.mem.eql(u8, a, b);
    }
};

// where a binding should be declared. set by setBindingContext when
// entering a variable_declaration, function, class, etc.
const TargetScope = enum {
    // let, const, import, etc go in the current scope
    current,
    // var hoists to nearest function/module/global scope
    hoist,
    // function/class declarations bind in the enclosing scope
    parent,
    // function/class expression names go in the expression_name scope,
    // which is the parent of the current function/class scope.
    // Section 15.2.5 InstantiateOrdinaryFunctionExpression (step 2-3)
    // Section 15.7.14 ClassDefinitionEvaluation (step 5-6)
    name,
};

/// Collects symbols and references during an AST walk.
///
/// Split into two phases per node:
///   `setBindingContext`: called on enter, sets up what kind of binding we're
///                       looking at (let/var/function/etc.) and where it should go.
///                       This runs for parent nodes like `variable_declaration`.
///   `declareBindings`: called on post_enter, actually creates the symbol or
///                       reference. This runs for leaf nodes like `binding_identifier`.
///
/// The split lets user hooks see the world between these two phases.
pub const SymbolTracker = struct {
    tree: *const ast.Tree,
    allocator: Allocator,
    symbols: std.ArrayList(Symbol) = .empty,
    references: std.ArrayList(Reference) = .empty,
    /// Per-scope hash maps for name lookup.
    scope_maps: std.ArrayList(ScopeMap) = .empty,
    /// Hoisted vars passing through intermediate block scopes.
    /// A `var` inside a block hoists to the function scope, but the name
    /// is still "taken" in each block it passes through (Section 14.2.1).
    hoisting_variables: std.ArrayList(ScopeMap) = .empty,

    // binding context, set by parent nodes, consumed by binding_identifier
    binding_kind: Symbol.Kind = .lexical,
    binding_is_const: bool = false,
    binding_is_ambient: bool = false,
    target_scope: TargetScope = .current,
    is_export: bool = false,
    is_default_export: bool = false,
    //

    pub fn init(tree: *ast.Tree) Allocator.Error!SymbolTracker {
        const alloc = tree.allocator();
        var self = SymbolTracker{ .tree = tree, .allocator = alloc };

        const estimated_symbols: u32 = @max(16, @as(u32, @intCast(tree.nodes.len / 32)));

        try self.symbols.ensureTotalCapacity(alloc, estimated_symbols);
        try self.references.ensureTotalCapacity(alloc, estimated_symbols);
        try self.scope_maps.ensureTotalCapacity(alloc, estimated_symbols / 2);
        try self.hoisting_variables.ensureTotalCapacity(alloc, estimated_symbols / 2);

        return self;
    }

    /// Phase 1: sets up the binding context based on what kind of node we're entering.
    /// This doesn't create any symbols yet. It just records "the next
    /// `binding_identifier` we see should be a let/var/function/etc, in this scope".
    pub fn setBindingContext(self: *SymbolTracker, data: ast.NodeData) void {
        switch (data) {
            .export_named_declaration => {
                self.is_export = true;
            },
            .export_default_declaration => {
                self.is_export = true;
                self.is_default_export = true;
            },
            .variable_declaration => |decl| {
                // var declarations only appear inside non-ambient bodies
                // because ambient namespaces and declare-only forms are
                // skipped by the traverser before they reach this point.
                self.binding_is_ambient = false;
                switch (decl.kind) {
                    .@"var" => {
                        self.binding_kind = .hoisted;
                        self.binding_is_const = false;
                        self.target_scope = .hoist;
                    },
                    .@"const", .using, .await_using => {
                        self.binding_kind = .lexical;
                        self.binding_is_const = true;
                        self.target_scope = .current;
                    },
                    .let => {
                        self.binding_kind = .lexical;
                        self.binding_is_const = false;
                        self.target_scope = .current;
                    },
                }
            },
            .function => |func| {
                self.binding_kind = .function;
                self.binding_is_const = false;
                // typescript ambient context: `declare function`, top-level
                // overload signatures, and body-less class methods (overloads,
                // abstract members, ambient methods).
                self.binding_is_ambient = func.declare or
                    func.type == .ts_declare_function or
                    func.type == .ts_empty_body_function_expression;
                self.target_scope = switch (func.type) {
                    // declarations bind in the enclosing scope
                    .function_declaration, .ts_declare_function => .parent,
                    // expressions bind in the expression_name scope
                    else => .name,
                };
            },
            .class => |cls| {
                self.binding_kind = .class;
                self.binding_is_const = false;
                self.binding_is_ambient = cls.declare;
                self.target_scope = switch (cls.type) {
                    .class_declaration => .parent,
                    else => .name,
                };
            },
            .formal_parameters => {
                self.binding_kind = .parameter;
                self.binding_is_const = false;
                // ambient is inherited from the enclosing function, so leave
                // `binding_is_ambient` untouched here.
                self.target_scope = .current;
                // parameters are never exported
                self.is_export = false;
                self.is_default_export = false;
            },
            .class_body => {
                // class members are never exported (the class itself might be)
                self.is_export = false;
                self.is_default_export = false;
            },
            .import_declaration => {
                self.binding_kind = .import;
                self.binding_is_const = false;
                self.binding_is_ambient = false;
                self.target_scope = .current;
            },
            .catch_clause => {
                self.binding_kind = .parameter;
                self.binding_is_const = false;
                self.binding_is_ambient = false;
                self.target_scope = .current;
            },
            else => {},
        }
    }

    /// Phase 2: actually creates the symbol or reference.
    pub fn declareBindings(self: *SymbolTracker, index: ast.NodeIndex, data: ast.NodeData, scope: *const sc.ScopeTracker) Allocator.Error!void {
        // keep scope_maps in sync with scope count. only runs when
        // new scopes were created since the last call.
        if (self.scope_maps.items.len < scope.scopes.items.len)
            try self.syncScopeMaps(scope);

        switch (data) {
            .binding_identifier => |id| {
                const target = self.resolveTargetScope(scope);
                const sym_id = try self.declare(id.name, target, index);

                // register hoisted vars in each intermediate block scope's
                // hoisting_variables so that findInScopeOrHoisted can detect
                // conflicts with later lexical declarations.
                if (self.binding_kind == .hoisted) {
                    var iter = scope.ancestors(scope.currentScopeId());
                    while (iter.next()) |s| {
                        if (s == target) break;
                        const gop = try self.hoisting_variables.items[@intFromEnum(s)].getOrPut(self.allocator, self.tree.getString(id.name));
                        if (!gop.found_existing) {
                            gop.value_ptr.* = sym_id;
                        }
                    }
                }
            },
            .identifier_reference => |id| {
                _ = try self.addReference(id.name, scope.currentScopeId(), index);
            },
            else => {},
        }
    }

    // resets binding context when exiting a node.
    pub fn exit(self: *SymbolTracker, data: ast.NodeData) void {
        switch (data) {
            .export_named_declaration, .export_default_declaration => {
                self.is_export = false;
                self.is_default_export = false;
            },
            else => {},
        }
    }

    /// Figures out which scope a binding should land in, based on
    /// the target scope set by `setBindingContext`.
    pub fn resolveTargetScope(self: *const SymbolTracker, scope: *const sc.ScopeTracker) sc.ScopeId {
        return switch (self.target_scope) {
            .current => scope.currentScopeId(),
            .hoist => scope.currentHoistScopeId(),
            .parent => scope.currentScope().parent,
            // for `.name` (function/class expression names), the current scope
            // is the function/class body. The `expression_name` scope is its parent,
            // so we go up one level. If for some reason we're not inside a
            // function/class (shouldn't happen), falls back to current scope.
            .name => blk: {
                const current = scope.currentScope();
                break :blk if (current.kind == .function or current.kind == .class)
                    current.parent
                else
                    scope.currentScopeId();
            },
        };
    }

    /// Creates a new symbol in the given scope using the current binding context.
    /// Returns the ID of the newly created symbol.
    pub fn declare(self: *SymbolTracker, name: String, target_scope: sc.ScopeId, node: ast.NodeIndex) Allocator.Error!SymbolId {
        const id: SymbolId = @enumFromInt(@as(u32, @intCast(self.symbols.items.len)));

        try self.symbols.append(self.allocator, .{
            .name = name,
            .kind = self.binding_kind,
            .flags = .{
                .exported = self.is_export,
                .is_default = self.is_default_export,
                .is_const = self.binding_is_const,
                .is_ambient = self.binding_is_ambient,
            },
            .scope = target_scope,
            .node = node,
        });

        try self.scope_maps.items[@intFromEnum(target_scope)].put(self.allocator, self.tree.getString(name), id);

        return id;
    }

    /// Records a new identifier reference in the given scope.
    /// Returns the ID of the newly created reference.
    pub fn addReference(self: *SymbolTracker, name: String, scope: sc.ScopeId, node: ast.NodeIndex) Allocator.Error!ReferenceId {
        const id: ReferenceId = @enumFromInt(@as(u32, @intCast(self.references.items.len)));
        try self.references.append(self.allocator, .{
            .name = name,
            .scope = scope,
            .node = node,
        });
        return id;
    }

    /// Returns what kind of binding the current context is for.
    /// Useful in visitor hooks to know if a `binding_identifier`
    /// is a let, var, function name, import, etc.
    pub inline fn currentBindingKind(self: *const SymbolTracker) Symbol.Kind {
        return self.binding_kind;
    }

    /// True when the current binding context is a typescript ambient
    /// declaration. See `setBindingContext` for what counts as ambient.
    pub inline fn currentBindingIsAmbient(self: *const SymbolTracker) bool {
        return self.binding_is_ambient;
    }

    /// Returns the symbol for the given ID.
    pub inline fn getSymbol(self: *const SymbolTracker, id: SymbolId) Symbol {
        return self.symbols.items[@intFromEnum(id)];
    }

    /// Returns the source name of a symbol as a string slice.
    pub inline fn getName(self: *const SymbolTracker, sym: Symbol) []const u8 {
        return self.tree.getString(sym.name);
    }

    /// Returns an iterator over all symbol IDs declared in the given scope.
    pub fn scopeSymbols(self: *const SymbolTracker, scope: sc.ScopeId) ScopeMap.ValueIterator {
        return self.scope_maps.items[@intFromEnum(scope)].valueIterator();
    }

    /// Searches for a symbol with `name` in a single scope. Returns `null` if not found.
    pub fn findInScope(self: *const SymbolTracker, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        const idx = @intFromEnum(scope);
        if (idx >= self.scope_maps.items.len) return null;
        return self.scope_maps.items[idx].get(name);
    }

    /// Searches for a declared symbol or a hoisted var passing through `scope`.
    pub fn findInScopeOrHoisted(self: *const SymbolTracker, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        if (self.findInScope(scope, name)) |id| return id;
        const idx = @intFromEnum(scope);
        if (idx < self.hoisting_variables.items.len) return self.hoisting_variables.items[idx].get(name);
        return null;
    }

    fn syncScopeMaps(self: *SymbolTracker, scope: *const sc.ScopeTracker) Allocator.Error!void {
        const scope_count = scope.scopes.items.len;
        try self.scope_maps.ensureTotalCapacity(self.allocator, scope_count);
        while (self.scope_maps.items.len < scope_count) {
            self.scope_maps.appendAssumeCapacity(.empty);
        }
        try self.hoisting_variables.ensureTotalCapacity(self.allocator, scope_count);
        while (self.hoisting_variables.items.len < scope_count) {
            self.hoisting_variables.appendAssumeCapacity(.empty);
        }
    }

    /// Finalizes into an immutable `SymbolTable`.
    pub fn toSymbolTable(self: *SymbolTracker) SymbolTable {
        return .{
            .symbols = self.symbols.items,
            .references = self.references.items,
            .scope_maps = self.scope_maps.items,
            .hoisting_variables = self.hoisting_variables.items,
            .strings = &self.tree.strings,
        };
    }
};
