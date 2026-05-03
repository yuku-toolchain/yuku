const std = @import("std");
const ast = @import("../../ast.zig");
const sc = @import("scope.zig");
const String = ast.String;

const Allocator = std.mem.Allocator;

/// Identifier for a `Symbol` in a `SymbolTable`. `.none` means absent.
pub const SymbolId = enum(u32) { none = std.math.maxInt(u32), _ };

/// Identifier for a `Reference` in a `SymbolTable`. `.none` means absent.
pub const ReferenceId = enum(u32) { none = std.math.maxInt(u32), _ };

const ScopeMap = std.StringHashMapUnmanaged(SymbolId);

/// A declared binding. `flags` describes which spaces (value, type,
/// namespace) and modifiers it occupies.
pub const Symbol = struct {
    name: String,
    flags: Flags,
    scope: sc.ScopeId,
    node: ast.NodeIndex,

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

        /// True when `a` and `b` share at least one set bit.
        pub inline fn intersects(a: Flags, b: Flags) bool {
            return @as(u32, @bitCast(a)) & @as(u32, @bitCast(b)) != 0;
        }

        /// Bitwise OR of two flag sets.
        pub inline fn merge(a: Flags, b: Flags) Flags {
            return @bitCast(@as(u32, @bitCast(a)) | @as(u32, @bitCast(b)));
        }

        /// True for a `var` that hoists past intermediate blocks. False
        /// for parameters and catch variables, which are function-scoped
        /// but don't hoist.
        pub inline fn isHoistingVar(self: Flags) bool {
            return self.function_scoped_var and !self.parameter and !self.catch_var;
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

    /// Bits in JS value space. Things visible at runtime.
    pub const VALUE: Flags = .{
        .function_scoped_var = true,
        .block_scoped_var = true,
        .function = true,
        .class = true,
        .regular_enum = true,
        .const_enum = true,
        .value_module = true,
    };

    /// Bits in TS type space. `class` and `enum` appear in both
    /// `VALUE` and `TYPE`.
    pub const TYPE: Flags = .{
        .class = true,
        .regular_enum = true,
        .const_enum = true,
        .interface = true,
        .type_alias = true,
        .type_parameter = true,
    };

    /// Names a hoisted `var` cannot pass through (section 14.2.1).
    pub const BLOCK_SCOPED_LIKE: Flags = .{
        .block_scoped_var = true,
        .class = true,
        .function = true,
    };

    /// Per-declaration redeclaration excludes. A new declaration with
    /// `Excludes.X` conflicts with any existing flag also in
    /// `Excludes.X`. Otherwise both declarations merge into one symbol.
    pub const Excludes = struct {
        pub const block_var: Flags = VALUE;

        pub const function_var: Flags = blk: {
            var f = VALUE;
            f.function_scoped_var = false;
            f.function = false;
            break :blk f;
        };

        /// Function in a hoist scope (function/global/static_block). TS
        /// allows function overloads, sloppy JS allows merge with `var`
        /// (Annex B 3.2). The `block_var` excludes are used instead at
        /// lexical scopes (block/module).
        pub const function: Flags = blk: {
            var f = VALUE;
            f.function_scoped_var = false;
            f.function = false;
            f.value_module = false;
            f.class = false;
            break :blk f;
        };

        pub const class: Flags = blk: {
            var f = VALUE.merge(TYPE);
            f.value_module = false;
            f.interface = false;
            break :blk f;
        };

        pub const interface: Flags = blk: {
            var f = TYPE;
            f.interface = false;
            f.class = false;
            break :blk f;
        };

        pub const type_alias: Flags = TYPE;

        pub const regular_enum: Flags = blk: {
            var f = VALUE.merge(TYPE);
            f.regular_enum = false;
            f.value_module = false;
            break :blk f;
        };

        pub const const_enum: Flags = blk: {
            var f = VALUE.merge(TYPE);
            f.const_enum = false;
            break :blk f;
        };

        pub const value_module: Flags = blk: {
            var f = VALUE;
            f.function = false;
            f.class = false;
            f.regular_enum = false;
            f.value_module = false;
            break :blk f;
        };

        pub const namespace_module: Flags = .{};

        pub const import_binding: Flags = .{ .import = true, .type_import = true };

        pub const parameter: Flags = blk: {
            var f = VALUE;
            f.function_scoped_var = false;
            break :blk f;
        };

        pub const catch_param: Flags = VALUE;

        pub const type_parameter: Flags = blk: {
            var f = TYPE;
            f.type_parameter = false;
            break :blk f;
        };
    };
};

/// A use of a name (not a declaration). Each `identifier_reference` in
/// the source produces one entry.
pub const Reference = struct {
    name: String,
    scope: sc.ScopeId,
    node: ast.NodeIndex,
    /// `.value` for runtime uses, `.type` for type-position uses
    /// (annotations, `extends`, `implements`, type arguments)
    kind: Kind = .value,

    pub const Kind = enum(u1) { value, type };
};

/// Immutable result of a semantic walk. Holds every symbol declared,
/// every reference recorded, and the per-scope binding maps. Pass to
/// `resolveAll` to build the cross-index between symbols and
/// references.
pub const SymbolTable = struct {
    symbols: std.MultiArrayList(Symbol).Slice,
    references: std.MultiArrayList(Reference).Slice,
    scope_maps: []const ScopeMap,
    hoisting_variables: []const ScopeMap,
    strings: *const ast.StringPool,

    resolutions: []const SymbolId = &.{},
    symbol_refs: []const ReferenceId = &.{},
    symbol_ref_ranges: []const Range = &.{},
    unresolved_refs: []const ReferenceId = &.{},

    const Range = struct { start: u32, len: u32 };

    /// Returns the source text for a `String` handle.
    pub inline fn string(self: SymbolTable, id: String) []const u8 {
        return self.strings.get(id);
    }

    /// Returns the symbol for the given id, rebuilt from the SoA columns.
    pub inline fn getSymbol(self: SymbolTable, id: SymbolId) Symbol {
        return self.symbols.get(@intFromEnum(id));
    }

    /// Returns the reference for the given id, rebuilt from the SoA columns.
    pub inline fn getReference(self: SymbolTable, id: ReferenceId) Reference {
        return self.references.get(@intFromEnum(id));
    }

    /// Number of symbols in the table.
    pub inline fn symbolCount(self: SymbolTable) usize {
        return self.symbols.len;
    }

    /// Number of references in the table.
    pub inline fn referenceCount(self: SymbolTable) usize {
        return self.references.len;
    }

    /// Direct access to a single symbol field as a slice. For tools
    /// that scan one column (e.g. minifier filtering by `.exported`).
    pub inline fn symbolField(self: SymbolTable, comptime field: std.meta.FieldEnum(Symbol)) []const std.meta.FieldType(Symbol, field) {
        return self.symbols.items(field);
    }

    /// Direct access to a single reference field as a slice.
    pub inline fn referenceField(self: SymbolTable, comptime field: std.meta.FieldEnum(Reference)) []const std.meta.FieldType(Reference, field) {
        return self.references.items(field);
    }

    /// Returns a symbol's source name as a string slice.
    pub inline fn getName(self: SymbolTable, sym: Symbol) []const u8 {
        return self.string(sym.name);
    }

    /// Returns a reference's source name as a string slice.
    pub inline fn getRefName(self: SymbolTable, ref: Reference) []const u8 {
        return self.string(ref.name);
    }

    /// Iterator over symbol ids declared in `scope` (excluding hoisted).
    pub fn scopeSymbols(self: SymbolTable, scope: sc.ScopeId) ScopeMap.ValueIterator {
        return self.scope_maps[@intFromEnum(scope)].valueIterator();
    }

    /// Looks up `name` declared directly in `scope`. Returns `null` if
    /// not found.
    pub fn findInScope(self: SymbolTable, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        return self.scope_maps[@intFromEnum(scope)].get(name);
    }

    /// Like `findInScope`, but also matches a hoisting `var` that is
    /// passing through `scope` on its way to its target.
    pub fn findInScopeOrHoisted(self: SymbolTable, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        if (self.findInScope(scope, name)) |id| return id;
        return self.hoisting_variables[@intFromEnum(scope)].get(name);
    }

    /// Walks up the scope chain from `scope` to find the nearest binding
    /// of `name`, including hoisted vars in any visited scope.
    pub fn resolve(self: SymbolTable, scope: sc.ScopeId, name: []const u8, scope_tree: sc.ScopeTree) ?SymbolId {
        var it = scope_tree.ancestors(scope);
        while (it.next()) |ancestor| {
            if (self.findInScopeOrHoisted(ancestor, name)) |id| return id;
        }
        return null;
    }

    /// Resolves every reference to its declaring symbol and builds the
    /// reverse index. After this returns:
    ///   - `referenceSymbol(ref_id)` gives the symbol the reference resolves to.
    ///   - `symbolReferences(sym_id)` gives all references to that symbol.
    ///   - `unresolvedReferences()` gives references with no matching symbol
    ///     (free variables, globals, or undeclared names).
    pub fn resolveAll(self: *SymbolTable, allocator: Allocator, scope_tree: sc.ScopeTree) Allocator.Error!void {
        const ref_count: u32 = @intCast(self.references.len);
        const sym_count: u32 = @intCast(self.symbols.len);

        if (ref_count == 0) return;

        const resolutions = try allocator.alloc(SymbolId, ref_count);
        const ref_names = self.references.items(.name);
        const ref_scopes = self.references.items(.scope);

        for (0..ref_count) |i| {
            const name = self.string(ref_names[i]);
            const pctx = PrehashCtx{ .h = std.hash.Wyhash.hash(0, name) };
            resolutions[i] = blk: {
                var it = scope_tree.ancestors(ref_scopes[i]);
                while (it.next()) |ancestor| {
                    const idx = @intFromEnum(ancestor);
                    if (self.scope_maps[idx].getAdapted(name, pctx)) |id| break :blk id;
                    if (self.hoisting_variables[idx].getAdapted(name, pctx)) |id| break :blk id;
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

    /// Returns the symbol a reference resolves to, or `.none` if the
    /// reference is unresolved. Only valid after `resolveAll` has run.
    pub inline fn referenceSymbol(self: SymbolTable, id: ReferenceId) SymbolId {
        const idx = @intFromEnum(id);
        if (idx >= self.resolutions.len) return .none;
        return self.resolutions[idx];
    }

    /// Returns every reference resolved to `id`. Only valid after
    /// `resolveAll` has run. Returns an empty slice before then.
    pub inline fn symbolReferences(self: SymbolTable, id: SymbolId) []const ReferenceId {
        const idx = @intFromEnum(id);
        if (idx >= self.symbol_ref_ranges.len) return &.{};
        const range = self.symbol_ref_ranges[idx];
        return self.symbol_refs[range.start..][0..range.len];
    }

    /// True when at least one reference resolves to `id`.
    pub inline fn isReferenced(self: SymbolTable, id: SymbolId) bool {
        const idx = @intFromEnum(id);
        if (idx >= self.symbol_ref_ranges.len) return false;
        return self.symbol_ref_ranges[idx].len > 0;
    }

    /// Reference ids that did not resolve to any symbol in the table.
    pub inline fn unresolvedReferences(self: SymbolTable) []const ReferenceId {
        return self.unresolved_refs;
    }
};

const PrehashCtx = struct {
    h: u64,
    pub fn hash(self: @This(), _: []const u8) u64 { return self.h; }
    pub fn eql(_: @This(), a: []const u8, b: []const u8) bool {
        return std.mem.eql(u8, a, b);
    }
};

/// Collects symbols and references during the AST walk.
///
/// Parent declaration nodes set `binding_flags`, `binding_excludes`,
/// and `target` in `setBindingContext` (called on enter). The next
/// `binding_identifier` consumes them in `declareBindings` (called on
/// post_enter). The two-phase split keeps user visitor hooks observing
/// a consistent view, parent context is in place when child nodes fire.
pub const SymbolTracker = struct {
    tree: *const ast.Tree,
    allocator: Allocator,
    symbols: std.MultiArrayList(Symbol) = .empty,
    references: std.MultiArrayList(Reference) = .empty,
    scope_maps: std.ArrayList(ScopeMap) = .empty,
    hoisting_variables: std.ArrayList(ScopeMap) = .empty,

    binding_flags: Symbol.Flags = .{},
    binding_excludes: Symbol.Flags = .{},
    target: sc.ScopeId = .root,
    is_export: bool = false,
    is_default_export: bool = false,

    pub fn init(tree: *ast.Tree) Allocator.Error!SymbolTracker {
        const alloc = tree.allocator();
        var self = SymbolTracker{ .tree = tree, .allocator = alloc };

        const estimated: u32 = @max(16, @as(u32, @intCast(tree.nodes.len / 32)));
        try self.symbols.ensureTotalCapacity(alloc, estimated);
        try self.references.ensureTotalCapacity(alloc, estimated);
        try self.scope_maps.ensureTotalCapacity(alloc, estimated / 2);
        try self.hoisting_variables.ensureTotalCapacity(alloc, estimated / 2);
        return self;
    }

    /// Records the pending binding state for the next `binding_identifier`.
    /// Called from `Ctx.enter` for every node so parent declaration nodes
    /// can configure flags, excludes, and target scope before the child
    /// binding identifier fires.
    pub fn setBindingContext(self: *SymbolTracker, data: ast.NodeData, scope: *const sc.ScopeTracker) void {
        switch (data) {
            .export_named_declaration => |decl| {
                if (decl.export_kind != .type) self.is_export = true;
            },
            .export_default_declaration => {
                self.is_export = true;
                self.is_default_export = true;
            },

            .variable_declaration => |decl| switch (decl.kind) {
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
            },

            .function => |func| {
                const ambient = func.declare or
                    func.type == .ts_declare_function or
                    func.type == .ts_empty_body_function_expression;
                self.binding_flags = .{ .function = true, .ambient = ambient };
                self.target = switch (func.type) {
                    .function_declaration, .ts_declare_function => scope.currentScope().parent,
                    else => exprNameScope(scope),
                };
                // sloppy JS at function/global/static_block lets functions
                // merge with var (Annex B). Anywhere else (block, module,
                // and TS) functions are lexical, so use block_var excludes.
                const k = scope.getScope(self.target).kind;
                const hoisted = !self.tree.isTs() and (k == .function or k == .global or k == .static_block);
                self.binding_excludes = if (hoisted) Symbol.Excludes.function else Symbol.Excludes.block_var;
            },

            .class => |cls| {
                self.binding_flags = .{ .class = true, .ambient = cls.declare };
                self.binding_excludes = Symbol.Excludes.class;
                self.target = switch (cls.type) {
                    .class_declaration => scope.currentScope().parent,
                    else => exprNameScope(scope),
                };
            },

            .formal_parameters => {
                self.binding_flags = .{
                    .function_scoped_var = true,
                    .parameter = true,
                    .ambient = self.binding_flags.ambient,
                };
                self.binding_excludes = Symbol.Excludes.parameter;
                self.target = scope.currentScopeId();
                self.is_export = false;
                self.is_default_export = false;
            },

            .class_body, .ts_module_block => {
                self.is_export = false;
                self.is_default_export = false;
            },

            inline .import_declaration, .ts_import_equals_declaration => |decl| {
                self.binding_flags = if (decl.import_kind == .type)
                    .{ .type_import = true }
                else
                    .{ .import = true };
                self.binding_excludes = Symbol.Excludes.import_binding;
                self.target = scope.currentScopeId();
            },

            .import_specifier => |spec| {
                if (spec.import_kind == .type) {
                    self.binding_flags = .{ .type_import = true };
                    self.binding_excludes = Symbol.Excludes.import_binding;
                }
            },

            .catch_clause => {
                self.binding_flags = .{ .function_scoped_var = true, .catch_var = true };
                self.binding_excludes = Symbol.Excludes.catch_param;
                self.target = scope.currentScopeId();
            },

            .ts_interface_declaration => |decl| {
                self.binding_flags = .{ .interface = true, .ambient = decl.declare };
                self.binding_excludes = Symbol.Excludes.interface;
                self.target = scope.currentScopeId();
            },

            .ts_type_alias_declaration => |decl| {
                self.binding_flags = .{ .type_alias = true, .ambient = decl.declare };
                self.binding_excludes = Symbol.Excludes.type_alias;
                self.target = scope.currentScopeId();
            },

            .ts_enum_declaration => |decl| {
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
                self.binding_flags = .{
                    .value_module = true,
                    .namespace_module = true,
                    .ambient = decl.declare,
                };
                self.binding_excludes = Symbol.Excludes.value_module;
                self.target = scope.currentScopeId();
            },

            .ts_namespace_export_declaration => {
                self.binding_flags = .{ .namespace_module = true };
                self.binding_excludes = Symbol.Excludes.namespace_module;
                self.target = scope.currentScopeId();
            },

            .ts_type_parameter => {
                self.binding_flags = .{ .type_parameter = true };
                self.binding_excludes = Symbol.Excludes.type_parameter;
                self.target = scope.currentScopeId();
            },

            else => {},
        }
    }

    /// Materializes the pending binding context into a symbol (for
    /// `binding_identifier`) or records a reference (for
    /// `identifier_reference`). Called from `Ctx.post_enter` for every
    /// node. `in_type_position` tags identifier references inside TS
    /// type subtrees so consumers can rename value and type spaces
    /// independently.
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
                const sym_id = try self.declare(id.name, self.binding_flags, self.binding_excludes, self.target, index);

                // Register a hoisting `var` in each intermediate block
                // so later let/const/class declarations see the conflict.
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
            else => {},
        }
    }

    /// Resets the pending export state when an export declaration ends.
    /// Called from `Ctx.exit`.
    pub fn exit(self: *SymbolTracker, data: ast.NodeData) void {
        switch (data) {
            .export_named_declaration, .export_default_declaration => {
                self.is_export = false;
                self.is_default_export = false;
            },
            else => {},
        }
    }

    /// Declares a binding in `target`. If the name is already bound
    /// there with non-conflicting flags, merges into the existing
    /// symbol. If the name conflicts (`existing.flags ∩ excludes ≠ ∅`),
    /// keeps the existing symbol unchanged and returns its id (the
    /// caller already detected the conflict and emitted a diagnostic).
    /// Otherwise creates a fresh symbol.
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

        if (self.scope_maps.items[target_idx].get(name_str) orelse
            self.hoisting_variables.items[target_idx].get(name_str)) |existing|
        {
            const flags_col = self.symbols.items(.flags);
            const existing_idx = @intFromEnum(existing);
            if (flags_col[existing_idx].intersects(excludes)) return existing;

            var merged = flags_col[existing_idx].merge(flags);
            if (self.is_export) merged.exported = true;
            if (self.is_default_export) merged.is_default = true;
            flags_col[existing_idx] = merged;
            return existing;
        }

        const new_id = try self.append(name, flags, target, node);
        try self.scope_maps.items[target_idx].put(self.allocator, name_str, new_id);
        return new_id;
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
        const id: ReferenceId = @enumFromInt(@as(u32, @intCast(self.references.len)));
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
        node: ast.NodeIndex,
    ) Allocator.Error!SymbolId {
        const id: SymbolId = @enumFromInt(@as(u32, @intCast(self.symbols.len)));
        var stored = flags;
        stored.exported = self.is_export;
        stored.is_default = self.is_default_export;
        try self.symbols.append(self.allocator, .{
            .name = name,
            .flags = stored,
            .scope = target,
            .node = node,
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

    /// Returns the symbol for the given id, rebuilt from the SoA columns.
    pub inline fn getSymbol(self: *const SymbolTracker, id: SymbolId) Symbol {
        return self.symbols.get(@intFromEnum(id));
    }

    /// Returns a symbol's source name as a string slice.
    pub inline fn getName(self: *const SymbolTracker, sym: Symbol) []const u8 {
        return self.tree.string(sym.name);
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

    /// Finalizes into an immutable `SymbolTable`. The tracker's backing
    /// arrays are aliased into the table. Both share the tree's arena
    /// lifetime.
    pub fn toSymbolTable(self: *SymbolTracker) SymbolTable {
        return .{
            .symbols = self.symbols.slice(),
            .references = self.references.slice(),
            .scope_maps = self.scope_maps.items,
            .hoisting_variables = self.hoisting_variables.items,
            .strings = &self.tree.strings,
        };
    }
};

/// Function and class expressions bind their name in an `expression_name`
/// scope between the outer scope and the body. That scope is the parent
/// of the current function or class scope.
fn exprNameScope(scope: *const sc.ScopeTracker) sc.ScopeId {
    const cur = scope.currentScope();
    return if (cur.kind == .function or cur.kind == .class) cur.parent else scope.currentScopeId();
}
