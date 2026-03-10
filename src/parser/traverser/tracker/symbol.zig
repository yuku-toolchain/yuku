const std = @import("std");
const ast = @import("../../ast.zig");
const sc = @import("scope.zig");

const Allocator = std.mem.Allocator;

/// Typed index into the symbol array. `.none` is the sentinel for "no symbol".
pub const SymbolId = enum(u32) { none = std.math.maxInt(u32), _ };
/// Typed index into the reference array. `.none` is the sentinel for "no reference".
pub const ReferenceId = enum(u32) { none = std.math.maxInt(u32), _ };

/// A declared binding (variable, function, class, import, or parameter).
pub const Symbol = struct {
    /// Byte offset of the name in the source text.
    name_start: u32,
    /// Length of the name in bytes.
    name_len: u16,
    kind: Kind,
    flags: Flags,
    /// The scope this symbol is declared in.
    scope: sc.ScopeId,
    node: ast.NodeIndex,
    /// Symbols in the same scope form a linked list.
    /// Points to the next symbol, or `.none` if this is the last one.
    /// Used by `findInScope` to walk all symbols in a scope.
    next_in_scope: SymbolId,

    /// What kind of JavaScript declaration created this symbol.
    pub const Kind = enum(u8) {
        /// ECMA-262 14.3.1: `let`, `const`, `using`, `await using`.
        lexical,
        /// ECMA-262 14.3.2: `var` (hoists to nearest function/module/global scope).
        hoisted,
        /// ECMA-262 15.2: function declaration or expression name.
        function,
        /// ECMA-262 15.7: class declaration or expression name.
        class,
        /// ECMA-262 15.1: function parameter or catch clause parameter.
        parameter,
        /// ECMA-262 16.2.2: import binding.
        import,
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
    /// Byte offset of the name in the source text.
    name_start: u32,
    /// Length of the name in bytes.
    name_len: u16,
    /// The scope this reference appears in (used for resolution).
    scope: sc.ScopeId,
    /// The AST node for this reference.
    node: ast.NodeIndex,
    /// Set to the resolved symbol after calling `resolveAll`, or `.none` if unresolved.
    resolved: SymbolId,
};

/// Immutable result of symbol collection. Owns its backing memory.
pub const SymbolTable = struct {
    symbols: []const Symbol,
    references: []const Reference,
    /// Head of the per-scope symbol linked list, indexed by scope ID.
    /// Each entry points to the first symbol in that scope (or `.none`).
    /// Follow `next_in_scope` on each symbol to walk the rest.
    scope_symbols: []const SymbolId,
    source: []const u8,
    allocator: Allocator,

    /// Frees all backing arrays.
    pub fn deinit(self: *SymbolTable) void {
        self.allocator.free(self.symbols);
        self.allocator.free(self.references);
        self.allocator.free(self.scope_symbols);
        self.* = undefined;
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
        return self.source[sym.name_start..][0..sym.name_len];
    }

    /// Returns the source name of a reference as a string slice.
    pub inline fn getRefName(self: SymbolTable, ref: Reference) []const u8 {
        return self.source[ref.name_start..][0..ref.name_len];
    }

    /// Searches for a symbol with `name` in a single scope. Returns `null` if not found.
    pub fn findInScope(self: SymbolTable, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        var id = self.scope_symbols[@intFromEnum(scope)];
        while (id != .none) {
            const sym = self.symbols[@intFromEnum(id)];
            if (sym.name_len == name.len and
                std.mem.eql(u8, self.source[sym.name_start..][0..sym.name_len], name))
            {
                return id;
            }
            id = sym.next_in_scope;
        }
        return null;
    }

    /// Walks up the scope chain from `scope` looking for a symbol with `name`.
    pub fn resolve(self: SymbolTable, scope: sc.ScopeId, name: []const u8, scope_tree: sc.ScopeTree) ?SymbolId {
        var it = scope_tree.ancestors(scope);
        while (it.next()) |ancestor| {
            if (self.findInScope(ancestor, name)) |id| return id;
        }
        return null;
    }

    /// Resolves all unresolved references by walking up scope chains.
    pub fn resolveAll(self: *SymbolTable, scope_tree: sc.ScopeTree) void {
        for (self.asReferenceMut()) |*ref| {
            if (ref.resolved == .none) {
                ref.resolved = self.resolve(ref.scope, self.source[ref.name_start..][0..ref.name_len], scope_tree) orelse .none;
            }
        }
    }

    fn asReferenceMut(self: *SymbolTable) []Reference {
        return @constCast(self.references);
    }
};

// where a binding should be declared. set by setBindingContext when
// entering a variable_declaration, function, class, etc.
const TargetScope = enum {
    // let, const, import, etc go in the current scope
    current,
    // var hoists to nearest function/module/global scope
    // ecma262 10.2.11 FunctionDeclarationInstantiation
    hoist,
    // function/class declarations bind in the enclosing scope
    parent,
    // function/class expression names go in the expression_name scope,
    // which is the parent of the current function/class scope.
    // ecma262 15.2.5 InstantiateOrdinaryFunctionExpression (step 2-3)
    // ecma262 15.7.14 ClassDefinitionEvaluation (step 5-6)
    name,
};

/// Mutable builder that collects symbols and references during an AST walk.
///
/// Split into two phases per node:
///   `setBindingContext`: called on enter, sets up what kind of binding we're
///                       looking at (let/var/function/etc.) and where it should go.
///                       This runs for parent nodes like `variable_declaration`.
///   `declare_bindings`: called on post_enter, actually creates the symbol or
///                       reference. This runs for leaf nodes like `binding_identifier`.
///
/// The split lets user hooks see the world between these two phases.
pub const SymbolTracker = struct {
    tree: *const ast.ParseTree,
    allocator: Allocator,
    symbols: std.ArrayList(Symbol) = .{},
    references: std.ArrayList(Reference) = .{},
    /// Per-scope linked list heads, indexed by scope ID.
    /// Grows as new scopes are created by the scope tracker.
    scope_symbols: std.ArrayList(SymbolId) = .{},

    // binding context: set by parent nodes, consumed by binding_identifier
    binding_kind: Symbol.Kind = .lexical,
    binding_is_const: bool = false,
    target_scope: TargetScope = .current,
    is_export: bool = false,
    is_default_export: bool = false,

    /// Creates a new tracker with pre-allocated capacity based on tree size.
    pub fn init(tree: *const ast.ParseTree, allocator: Allocator) Allocator.Error!SymbolTracker {
        var self = SymbolTracker{ .tree = tree, .allocator = allocator };

        const estimated_symbols: u32 = @max(16, @as(u32, @intCast(tree.nodes.len / 32)));

        try self.symbols.ensureTotalCapacity(allocator, estimated_symbols);
        try self.references.ensureTotalCapacity(allocator, estimated_symbols);
        try self.scope_symbols.ensureTotalCapacity(allocator, estimated_symbols / 2);

        return self;
    }

    /// Phase 1: sets up the binding context based on what kind of node we're entering.
    /// This doesn't create any symbols yet. It just records "the next
    /// `binding_identifier` we see should be a let/var/function/etc. in this scope".
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
                self.target_scope = switch (cls.type) {
                    .class_declaration => .parent,
                    else => .name,
                };
            },
            .formal_parameters => {
                self.binding_kind = .parameter;
                self.binding_is_const = false;
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
                self.target_scope = .current;
            },
            .catch_clause => {
                self.binding_kind = .parameter;
                self.binding_is_const = false;
                self.target_scope = .current;
            },
            else => {},
        }
    }

    /// Phase 2: actually creates the symbol or reference.
    /// Only fires for `binding_identifier` and `identifier_reference` nodes.
    /// Runs after user hooks, so the user can inspect existing state first.
    pub fn declare_bindings(self: *SymbolTracker, index: ast.NodeIndex, data: ast.NodeData, scope: *const sc.ScopeTracker) Allocator.Error!void {
        // keep scope_symbols in sync with scope count. new scopes might
        // have been created since the last time we were called.
        const scope_count = scope.scopes.items.len;
        try self.scope_symbols.ensureTotalCapacity(self.allocator, scope_count);
        while (self.scope_symbols.items.len < scope_count) {
            self.scope_symbols.appendAssumeCapacity(.none);
        }

        switch (data) {
            .binding_identifier => |id| {
                const target = self.resolveTargetScope(scope);
                _ = try self.declare(id.name_start, id.name_len, target, index);
            },
            .identifier_reference => |id| {
                _ = try self.addReference(id.name_start, id.name_len, scope.currentScopeId(), index);
            },
            else => {},
        }
    }

    /// Cleans up binding context on exit (resets export flags after export nodes).
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
    ///
    /// For `.name` (function/class expression names): the current scope
    /// is the function/class body. The `expression_name` scope is its parent,
    /// so we go up one level. If for some reason we're not inside a
    /// function/class (shouldn't happen), falls back to current scope.
    pub fn resolveTargetScope(self: *const SymbolTracker, scope: *const sc.ScopeTracker) sc.ScopeId {
        return switch (self.target_scope) {
            .current => scope.currentScopeId(),
            .hoist => scope.currentHoistScopeId(),
            .parent => scope.currentScope().parent,
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
    pub fn declare(self: *SymbolTracker, name_start: u32, name_len: u16, target_scope: sc.ScopeId, node: ast.NodeIndex) Allocator.Error!SymbolId {
        const id: SymbolId = @enumFromInt(@as(u32, @intCast(self.symbols.items.len)));
        const scope_idx = @intFromEnum(target_scope);

        // prepend to the scope's linked list (new symbol becomes the head)
        try self.symbols.append(self.allocator, .{
            .name_start = name_start,
            .name_len = name_len,
            .kind = self.binding_kind,
            .flags = .{
                .exported = self.is_export,
                .is_default = self.is_default_export,
                .is_const = self.binding_is_const,
            },
            .scope = target_scope,
            .node = node,
            .next_in_scope = self.scope_symbols.items[scope_idx],
        });

        self.scope_symbols.items[scope_idx] = id;
        return id;
    }

    /// Records a new identifier reference in the given scope.
    /// Returns the ID of the newly created reference.
    pub fn addReference(self: *SymbolTracker, name_start: u32, name_len: u16, scope: sc.ScopeId, node: ast.NodeIndex) Allocator.Error!ReferenceId {
        const id: ReferenceId = @enumFromInt(@as(u32, @intCast(self.references.items.len)));
        try self.references.append(self.allocator, .{
            .name_start = name_start,
            .name_len = name_len,
            .scope = scope,
            .node = node,
            .resolved = .none,
        });
        return id;
    }

    /// Returns what kind of binding the current context is for.
    /// Useful in visitor hooks to know if a `binding_identifier`
    /// is a let, var, function name, import, etc.
    pub inline fn currentBindingKind(self: *const SymbolTracker) Symbol.Kind {
        return self.binding_kind;
    }

    /// Returns the symbol for the given ID.
    pub inline fn getSymbol(self: *const SymbolTracker, id: SymbolId) Symbol {
        return self.symbols.items[@intFromEnum(id)];
    }

    /// Returns the source name of a symbol as a string slice.
    pub inline fn getName(self: *const SymbolTracker, sym: Symbol) []const u8 {
        return self.tree.getSourceText(sym.name_start, sym.name_len);
    }

    /// Searches for a symbol with `name` in a single scope. Returns `null` if not found.
    pub fn findInScope(self: *const SymbolTracker, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        var id = self.scope_symbols.items[@intFromEnum(scope)];
        while (id != .none) {
            const sym = self.symbols.items[@intFromEnum(id)];
            if (sym.name_len == name.len) {
                if (std.mem.eql(u8, self.tree.getSourceText(sym.name_start, sym.name_len), name))
                    return id;
            }
            id = sym.next_in_scope;
        }
        return null;
    }

    /// Finalizes into an immutable `SymbolTable` by transferring ownership of the arrays.
    pub fn toSymbolTable(self: *SymbolTracker) Allocator.Error!SymbolTable {
        return .{
            .symbols = try self.symbols.toOwnedSlice(self.allocator),
            .references = try self.references.toOwnedSlice(self.allocator),
            .scope_symbols = try self.scope_symbols.toOwnedSlice(self.allocator),
            .source = self.tree.source,
            .allocator = self.allocator,
        };
    }

    /// Frees all resources. Only needed if the traversal is aborted early
    /// (normally, call `toSymbolTable` instead which takes ownership).
    pub fn deinit(self: *SymbolTracker) void {
        self.symbols.deinit(self.allocator);
        self.references.deinit(self.allocator);
        self.scope_symbols.deinit(self.allocator);
    }
};
