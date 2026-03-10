const std = @import("std");
const ast = @import("../../ast.zig");
const sc = @import("scope.zig");

const Allocator = std.mem.Allocator;
const NodeTag = std.meta.Tag(ast.NodeData);

pub const SymbolId = enum(u32) { none = std.math.maxInt(u32), _ };
pub const ReferenceId = enum(u32) { none = std.math.maxInt(u32), _ };

/// A declared binding in the source.
pub const Symbol = struct {
    /// Offset into source text where the name starts.
    name_start: u32,
    /// Length of the name in bytes.
    name_len: u16,
    kind: Kind,
    flags: Flags,
    /// The scope this symbol is declared in.
    scope: sc.ScopeId,
    /// The AST node of the binding identifier.
    node: ast.NodeIndex,
    /// Next symbol in the same scope (intrusive linked list).
    next_in_scope: SymbolId,

    pub const Kind = enum(u8) {
        /// `let`, `const`, `using`, `await using`
        lexical,
        /// `var`
        hoisted,
        /// function declaration or expression name
        function,
        /// class declaration or expression name
        class,
        /// function parameter or catch parameter
        parameter,
        /// import binding
        import,
    };

    pub const Flags = packed struct(u8) {
        exported: bool = false,
        is_default: bool = false,
        is_const: bool = false,
        is_ambient: bool = false,
        _padding: u4 = 0,
    };
};

/// A use of an identifier in the source.
pub const Reference = struct {
    /// Offset into source text where the name starts.
    name_start: u32,
    /// Length of the name in bytes.
    name_len: u16,
    /// The scope where this reference appears.
    scope: sc.ScopeId,
    /// The AST node of the identifier reference.
    node: ast.NodeIndex,
    /// Resolved symbol, or `.none` if unresolved.
    resolved: SymbolId,
};

/// Immutable result of symbol collection. Owns its backing memory.
pub const SymbolTable = struct {
    symbols: []const Symbol,
    references: []const Reference,
    /// Head of the per-scope symbol linked list, indexed by ScopeId.
    scope_symbols: []const SymbolId,
    source: []const u8,
    allocator: Allocator,

    pub fn deinit(self: *SymbolTable) void {
        self.allocator.free(self.symbols);
        self.allocator.free(self.references);
        self.allocator.free(self.scope_symbols);
        self.* = undefined;
    }

    /// Returns the symbol for a given id.
    pub inline fn getSymbol(self: SymbolTable, id: SymbolId) Symbol {
        return self.symbols[@intFromEnum(id)];
    }

    /// Returns the reference for a given id.
    pub inline fn getReference(self: SymbolTable, id: ReferenceId) Reference {
        return self.references[@intFromEnum(id)];
    }

    /// Returns the name of a symbol as a slice of the source text.
    pub inline fn getName(self: SymbolTable, sym: Symbol) []const u8 {
        return self.source[sym.name_start..][0..sym.name_len];
    }

    /// Returns the name of a reference as a slice of the source text.
    pub inline fn getRefName(self: SymbolTable, ref: Reference) []const u8 {
        return self.source[ref.name_start..][0..ref.name_len];
    }

    /// Find a symbol by name in a specific scope.
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

    /// Resolve a name from a scope by walking up the scope chain.
    pub fn resolve(self: SymbolTable, scope: sc.ScopeId, name: []const u8, scope_tree: sc.ScopeTree) ?SymbolId {
        var it = scope_tree.ancestors(scope);
        while (it.next()) |ancestor| {
            if (self.findInScope(ancestor, name)) |id| return id;
        }
        return null;
    }

    /// Resolve all references against the scope tree.
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

const TargetScope = enum { current, hoist, parent };

pub const SymbolTracker = struct {
    tree: *const ast.ParseTree,
    allocator: Allocator,
    symbols: std.ArrayList(Symbol) = .{},
    references: std.ArrayList(Reference) = .{},
    /// Head of the per-scope symbol linked list, indexed by ScopeId.
    scope_symbols: std.ArrayList(SymbolId) = .{},

    // binding context state
    binding_kind: Symbol.Kind = .lexical,
    binding_is_const: bool = false,
    target_scope: TargetScope = .current,
    is_export: bool = false,
    is_default_export: bool = false,

    pub fn init(tree: *const ast.ParseTree, allocator: Allocator) Allocator.Error!SymbolTracker {
        var self = SymbolTracker{ .tree = tree, .allocator = allocator };

        const estimated_symbols: u32 = @max(16, @as(u32, @intCast(tree.nodes.len / 32)));

        try self.symbols.ensureTotalCapacity(allocator, estimated_symbols);
        try self.references.ensureTotalCapacity(allocator, estimated_symbols);
        try self.scope_symbols.ensureTotalCapacity(allocator, estimated_symbols / 2);

        return self;
    }

    /// Process a node enter. Call this from your context's `onEnter`, after `scope.enter`.
    pub fn enter(self: *SymbolTracker, index: ast.NodeIndex, tag: NodeTag, scope: *const sc.ScopeTracker) Allocator.Error!void {
        // sync scope_symbols with scope count
        const scope_count = scope.scopes.items.len;

        try self.scope_symbols.ensureTotalCapacity(self.allocator, scope_count);

        while (self.scope_symbols.items.len < scope_count) {
            self.scope_symbols.appendAssumeCapacity(.none);
        }

        switch (tag) {
            .export_named_declaration => {
                self.is_export = true;
            },
            .export_default_declaration => {
                self.is_export = true;
                self.is_default_export = true;
            },
            .variable_declaration => {
                const decl = self.tree.getData(index).variable_declaration;
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
            .function => {
                const func = self.tree.getData(index).function;
                self.binding_kind = .function;
                self.binding_is_const = false;
                self.target_scope = switch (func.type) {
                    .function_declaration, .ts_declare_function => .parent,
                    else => .current,
                };
            },
            .class => {
                const cls = self.tree.getData(index).class;
                self.binding_kind = .class;
                self.binding_is_const = false;
                self.target_scope = switch (cls.type) {
                    .class_declaration => .parent,
                    else => .current,
                };
            },
            .formal_parameters => {
                self.binding_kind = .parameter;
                self.binding_is_const = false;
                self.target_scope = .current;
                self.is_export = false;
                self.is_default_export = false;
            },
            .class_body => {
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
            .binding_identifier => {
                const id = self.tree.getData(index).binding_identifier;
                const target = switch (self.target_scope) {
                    .current => scope.currentScopeId(),
                    .hoist => scope.currentHoistScopeId(),
                    .parent => scope.currentScope().parent,
                };
                _ = try self.declare(id.name_start, id.name_len, target, index);
            },
            .identifier_reference => {
                const id = self.tree.getData(index).identifier_reference;
                _ = try self.addReference(id.name_start, id.name_len, scope.currentScopeId(), index);
            },
            else => {},
        }
    }

    /// Process a node exit. Call this from your context's `onExit`.
    pub fn exit(self: *SymbolTracker, tag: NodeTag) void {
        switch (tag) {
            .export_named_declaration, .export_default_declaration => {
                self.is_export = false;
                self.is_default_export = false;
            },
            else => {},
        }
    }

    /// Declare a symbol in the given scope. Returns the new SymbolId.
    pub fn declare(self: *SymbolTracker, name_start: u32, name_len: u16, target_scope: sc.ScopeId, node: ast.NodeIndex) Allocator.Error!SymbolId {
        const id: SymbolId = @enumFromInt(@as(u32, @intCast(self.symbols.items.len)));
        const scope_idx = @intFromEnum(target_scope);

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

    /// Record an unresolved reference.
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

    /// Returns the current binding kind (useful for visitors checking before declaration).
    pub inline fn currentBindingKind(self: *const SymbolTracker) Symbol.Kind {
        return self.binding_kind;
    }

    /// Returns the current binding target scope kind.
    pub inline fn currentTargetScopeKind(self: *const SymbolTracker) TargetScope {
        return self.target_scope;
    }

    /// Returns the symbol for a given id.
    pub inline fn getSymbol(self: *const SymbolTracker, id: SymbolId) Symbol {
        return self.symbols.items[@intFromEnum(id)];
    }

    /// Returns the name of a symbol as a slice of the source text.
    pub inline fn getName(self: *const SymbolTracker, sym: Symbol) []const u8 {
        return self.tree.getSourceText(sym.name_start, sym.name_len);
    }

    /// Find a symbol by name in a specific scope.
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

    /// Returns the completed symbol table and releases builder resources.
    pub fn toSymbolTable(self: *SymbolTracker) Allocator.Error!SymbolTable {
        return .{
            .symbols = try self.symbols.toOwnedSlice(self.allocator),
            .references = try self.references.toOwnedSlice(self.allocator),
            .scope_symbols = try self.scope_symbols.toOwnedSlice(self.allocator),
            .source = self.tree.source,
            .allocator = self.allocator,
        };
    }

    /// Release all owned memory without producing a symbol table.
    pub fn deinit(self: *SymbolTracker) void {
        self.symbols.deinit(self.allocator);
        self.references.deinit(self.allocator);
        self.scope_symbols.deinit(self.allocator);
    }
};
