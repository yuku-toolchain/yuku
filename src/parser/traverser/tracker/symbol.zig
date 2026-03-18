const std = @import("std");
const ast = @import("../../ast.zig");
const sc = @import("scope.zig");

const Allocator = std.mem.Allocator;

/// ID for a symbol. `.none` means no symbol.
pub const SymbolId = enum(u32) { none = std.math.maxInt(u32), _ };

/// ID for a reference. `.none` means no reference.
pub const ReferenceId = enum(u32) { none = std.math.maxInt(u32), _ };

/// A declared binding (variable, function, class, import, or parameter).
pub const Symbol = struct {
    /// Name of the symbol.
    name: ast.StringId,
    kind: Kind,
    flags: Flags,
    /// The scope this symbol is declared in.
    scope: sc.ScopeId,
    node: ast.NodeIndex,
    /// Next symbol in the same scope, or `.none` if this is the last one.
    next_in_scope: SymbolId,

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

        pub fn isLexical(kind: Kind) bool {
            return switch (kind) {
                .lexical, .class, .import => true,
                else => false,
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
    /// Interned name of the reference.
    name: ast.StringId,
    /// The scope this reference appears in (used for resolution).
    scope: sc.ScopeId,
    /// The AST node for this reference.
    node: ast.NodeIndex,
    /// Set to the resolved symbol after calling `resolveAll`, or `.none` if unresolved.
    resolved: SymbolId,
};

/// The result of symbol collection. Contains all symbols and references from the walk.
pub const SymbolTable = struct {
    symbols: []const Symbol,
    references: []const Reference,
    /// First symbol in each scope, indexed by scope ID.
    /// `.none` if the scope has no symbols.
    scope_symbols: []const SymbolId,
    /// String pool for resolving symbol and reference names.
    strings: ast.StringPool,

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
        return self.strings.get(sym.name);
    }

    /// Returns the source name of a reference as a string slice.
    pub inline fn getRefName(self: SymbolTable, ref: Reference) []const u8 {
        return self.strings.get(ref.name);
    }

    /// Returns an iterator over all symbols declared in the given scope.
    pub fn scopeSymbols(self: SymbolTable, scope: sc.ScopeId) ScopeSymbolIterator {
        return .{ .symbols = self.symbols, .current = self.scope_symbols[@intFromEnum(scope)] };
    }

    /// Searches for a symbol with `name` in a single scope. Returns `null` if not found.
    pub fn findInScope(self: SymbolTable, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        var it = self.scopeSymbols(scope);
        while (it.next()) |id| {
            const sym = self.symbols[@intFromEnum(id)];
            if (std.mem.eql(u8, self.getName(sym), name))
            {
                return id;
            }
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
                ref.resolved = self.resolve(ref.scope, self.getRefName(ref), scope_tree) orelse .none;
            }
        }
    }

    fn asReferenceMut(self: *SymbolTable) []Reference {
        return @constCast(self.references);
    }

    /// Walks the symbols declared in a single scope, following `next_in_scope` links.
    pub const ScopeSymbolIterator = struct {
        symbols: []const Symbol,
        current: SymbolId,

        /// Returns the next symbol ID in this scope, or `null` when done.
        pub fn next(self: *ScopeSymbolIterator) ?SymbolId {
            const id = self.current;
            if (id == .none) return null;
            self.current = self.symbols[@intFromEnum(id)].next_in_scope;
            return id;
        }
    };
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
    symbols: std.ArrayList(Symbol) = .{},
    references: std.ArrayList(Reference) = .{},
    /// First symbol in each scope, indexed by scope ID.
    scope_symbols: std.ArrayList(SymbolId) = .{},

    // binding context, set by parent nodes, consumed by binding_identifier
    binding_kind: Symbol.Kind = .lexical,
    binding_is_const: bool = false,
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
        try self.scope_symbols.ensureTotalCapacity(alloc, estimated_symbols / 2);

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
    pub fn declareBindings(self: *SymbolTracker, index: ast.NodeIndex, data: ast.NodeData, scope: *const sc.ScopeTracker) Allocator.Error!void {
        // keep scope_symbols in sync with scope count. new scopes might
        // have been created since the last time we were called.
        const scope_count = scope.scopes.items.len;

        try self.scope_symbols.ensureTotalCapacity(self.allocator, scope_count);

        while (self.scope_symbols.items.len < scope_count) {
            self.scope_symbols.appendAssumeCapacity(.none);
        }
        //

        switch (data) {
            .binding_identifier => |id| {
                const target = self.resolveTargetScope(scope);
                _ = try self.declare(id.name, target, index);
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
    pub fn declare(self: *SymbolTracker, name: ast.StringId, target_scope: sc.ScopeId, node: ast.NodeIndex) Allocator.Error!SymbolId {
        const id: SymbolId = @enumFromInt(@as(u32, @intCast(self.symbols.items.len)));
        const scope_idx = @intFromEnum(target_scope);

        // prepend to the scope's linked list (new symbol becomes the head)
        try self.symbols.append(self.allocator, .{
            .name = name,
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
    pub fn addReference(self: *SymbolTracker, name: ast.StringId, scope: sc.ScopeId, node: ast.NodeIndex) Allocator.Error!ReferenceId {
        const id: ReferenceId = @enumFromInt(@as(u32, @intCast(self.references.items.len)));
        try self.references.append(self.allocator, .{
            .name = name,
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
        return self.tree.getString(sym.name);
    }

    /// Returns an iterator over all symbols declared in the given scope.
    pub fn scopeSymbols(self: *const SymbolTracker, scope: sc.ScopeId) SymbolTable.ScopeSymbolIterator {
        return .{ .symbols = self.symbols.items, .current = self.scope_symbols.items[@intFromEnum(scope)] };
    }

    /// Searches for a symbol with `name` in a single scope. Returns `null` if not found.
    pub fn findInScope(self: *const SymbolTracker, scope: sc.ScopeId, name: []const u8) ?SymbolId {
        var it = self.scopeSymbols(scope);
        while (it.next()) |id| {
            const sym = self.symbols.items[@intFromEnum(id)];
            if (std.mem.eql(u8, self.tree.getString(sym.name), name))
                return id;
        }
        return null;
    }

    /// Finalizes into an immutable `SymbolTable`.
    pub fn toSymbolTable(self: *SymbolTracker) SymbolTable {
        return .{
            .symbols = self.symbols.items,
            .references = self.references.items,
            .scope_symbols = self.scope_symbols.items,
            .strings = self.tree.strings.freeze(),
        };
    }
};
