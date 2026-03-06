const std = @import("std");
const ast = @import("../ast.zig");
const root = @import("root.zig");

const Allocator = std.mem.Allocator;
const NodeTag = root.NodeTag;

// ── Handles ──

pub const ScopeId = enum(u32) { root = 0, none = std.math.maxInt(u32), _ };
pub const SymbolId = enum(u32) { none = std.math.maxInt(u32), _ };

// ── Scope ──

pub const Scope = struct {
    node: ast.NodeIndex,
    parent: ScopeId,
    kind: Kind,
    flags: Flags,

    pub const Kind = enum(u8) {
        module,
        function,
        block,
        @"for",
        @"catch",
        class,
        static_block,
        @"switch",
    };

    pub const Flags = packed struct(u8) {
        strict: bool = false,
        arrow: bool = false,
        @"async": bool = false,
        generator: bool = false,
        _pad: u4 = 0,
    };
};

// ── Scope kind lookup table (O(1), branch-free) ──

const scope_kinds: [std.meta.fields(ast.NodeData).len]?Scope.Kind = blk: {
    var table: [std.meta.fields(ast.NodeData).len]?Scope.Kind = @splat(null);
    table[@intFromEnum(NodeTag.function)] = .function;
    table[@intFromEnum(NodeTag.arrow_function_expression)] = .function;
    table[@intFromEnum(NodeTag.block_statement)] = .block;
    table[@intFromEnum(NodeTag.for_statement)] = .@"for";
    table[@intFromEnum(NodeTag.for_in_statement)] = .@"for";
    table[@intFromEnum(NodeTag.for_of_statement)] = .@"for";
    table[@intFromEnum(NodeTag.catch_clause)] = .@"catch";
    table[@intFromEnum(NodeTag.class)] = .class;
    table[@intFromEnum(NodeTag.static_block)] = .static_block;
    table[@intFromEnum(NodeTag.switch_statement)] = .@"switch";
    break :blk table;
};

/// Returns the scope kind for a node tag, or null if it doesn't create a scope.
pub inline fn scopeKindOf(tag: NodeTag) ?Scope.Kind {
    return scope_kinds[@intFromEnum(tag)];
}

// ── ScopeTree: flat output that survives traversal ──

pub const ScopeTree = struct {
    scopes: []const Scope,

    pub inline fn get(self: ScopeTree, id: ScopeId) Scope {
        return self.scopes[@intFromEnum(id)];
    }

    pub inline fn parentOf(self: ScopeTree, id: ScopeId) ScopeId {
        return self.scopes[@intFromEnum(id)].parent;
    }

    pub inline fn len(self: ScopeTree) u32 {
        return @intCast(self.scopes.len);
    }

    /// Walk up the scope chain, calling `func` for each ancestor scope.
    /// Stops when func returns false or the root is reached.
    pub fn walkAncestors(self: ScopeTree, id: ScopeId, comptime func: fn (ScopeId, Scope) bool) void {
        var current = id;
        while (current != .none) {
            const s = self.get(current);
            if (!func(current, s)) return;
            current = s.parent;
        }
    }

    /// Find the nearest ancestor scope of a given kind.
    pub fn nearestOfKind(self: ScopeTree, from: ScopeId, kind: Scope.Kind) ?ScopeId {
        var current = self.get(from).parent;
        while (current != .none) {
            const s = self.get(current);
            if (s.kind == kind) return current;
            current = s.parent;
        }
        return null;
    }

    /// Find the nearest function or module scope (for var hoisting).
    pub fn nearestVarScope(self: ScopeTree, from: ScopeId) ScopeId {
        var current = from;
        while (current != .none) {
            const s = self.get(current);
            if (s.kind == .function or s.kind == .module) return current;
            current = s.parent;
        }
        return .root;
    }
};

// ── ScopedCtx: enhanced traversal context ──

pub const ScopedCtx = struct {
    tree: *const ast.ParseTree,
    parents: root.ParentStack = .{},
    allocator: Allocator,

    // scope internals
    scopes: std.ArrayList(Scope) = .{},
    scope_stack: std.ArrayList(ScopeId) = .{},

    pub fn init(tree: *const ast.ParseTree, allocator: Allocator) ScopedCtx {
        var self = ScopedCtx{ .tree = tree, .allocator = allocator };
        const node_count = tree.nodes.len;
        self.scopes.ensureTotalCapacity(allocator, @max(16, @as(u32, @intCast(node_count / 16)))) catch {};
        self.scope_stack.ensureTotalCapacity(allocator, 64) catch {};
        // Root module scope
        self.scopes.appendAssumeCapacity(.{
            .node = tree.program,
            .parent = .none,
            .kind = .module,
            .flags = .{},
        });
        self.scope_stack.appendAssumeCapacity(.root);
        return self;
    }

    // ── Context protocol (called by root.walk) ──

    pub fn onEnter(self: *ScopedCtx, index: ast.NodeIndex, tag: NodeTag) void {
        self.parents.push(index);

        const kind = scopeKindOf(tag) orelse return;
        var flags: Scope.Flags = .{};
        if (kind == .function) {
            const data = self.tree.getData(index);
            switch (data) {
                .arrow_function_expression => |f| {
                    flags.arrow = true;
                    flags.@"async" = f.@"async";
                },
                .function => |f| {
                    flags.@"async" = f.@"async";
                    flags.generator = f.generator;
                },
                else => {},
            }
        }
        const id: ScopeId = @enumFromInt(@as(u32, @intCast(self.scopes.items.len)));
        self.scopes.append(self.allocator, .{
            .node = index,
            .parent = self.currentScope(),
            .kind = kind,
            .flags = flags,
        }) catch unreachable;
        self.scope_stack.append(self.allocator, id) catch unreachable;
    }

    pub fn onExit(self: *ScopedCtx, _: ast.NodeIndex, tag: NodeTag) void {
        if (scopeKindOf(tag) != null) {
            _ = self.scope_stack.pop().?;
        }
        self.parents.pop();
    }

    // ── User-facing API ──

    /// The currently active scope.
    pub inline fn currentScope(self: *const ScopedCtx) ScopeId {
        return self.scope_stack.getLast();
    }

    /// Get the Scope data for a given ScopeId.
    pub inline fn scopeData(self: *const ScopedCtx, id: ScopeId) Scope {
        return self.scopes.items[@intFromEnum(id)];
    }

    /// Get the scope tree built so far (useful in exit hooks to find var-hoist target, etc).
    pub inline fn scopeTree(self: *const ScopedCtx) ScopeTree {
        return .{ .scopes = self.scopes.items };
    }

    /// The immediate parent node (null_node at root).
    pub inline fn parent(self: *const ScopedCtx) ast.NodeIndex {
        return self.parents.current();
    }

    /// Nth ancestor: 0 = parent, 1 = grandparent, ...
    pub inline fn ancestor(self: *const ScopedCtx, depth_offset: usize) ast.NodeIndex {
        return self.parents.get(depth_offset);
    }

    pub inline fn parentData(self: *const ScopedCtx) ?ast.NodeData {
        const p = self.parent();
        if (ast.isNull(p)) return null;
        return self.tree.getData(p);
    }

    pub inline fn nodeText(self: *const ScopedCtx, start: u32, len: u16) []const u8 {
        return self.tree.source[start..][0..len];
    }

    /// Finalize and return the complete scope tree.
    pub fn finalize(self: *const ScopedCtx) ScopeTree {
        return .{ .scopes = self.scopes.items };
    }
};

// ── Entry point ──

/// Scoped traversal. Automatically tracks scope enter/exit.
/// Returns the complete ScopeTree after traversal.
pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V, allocator: Allocator) ScopeTree {
    var ctx = ScopedCtx.init(tree, allocator);
    root.walk(ScopedCtx, V, visitor, &ctx);
    return ctx.finalize();
}

// ── SymbolTable: standalone, shadow-stack based, O(1) resolve ──

pub const Symbol = struct {
    name_start: u32,
    name_len: u16,
    node: ast.NodeIndex,
    scope: ScopeId,
    kind: Kind,
    flags: Flags,

    pub const Kind = enum(u8) { variable, function, parameter, import, class, type_alias };
    pub const Flags = packed struct(u8) {
        @"const": bool = false,
        exported: bool = false,
        hoisted: bool = false,
        _pad: u5 = 0,
    };

    pub inline fn name(self: Symbol, source: []const u8) []const u8 {
        return source[self.name_start..][0..self.name_len];
    }
};

pub const SymbolTable = struct {
    symbols: std.ArrayList(Symbol) = .{},
    names: std.StringHashMap(SymbolId),
    shadows: std.ArrayList(ShadowEntry) = .{},
    save_points: std.ArrayList(u32) = .{},
    allocator: Allocator,

    const ShadowEntry = struct {
        name: []const u8,
        prev: SymbolId,
    };

    pub fn init(allocator: Allocator, estimated_symbols: u32) SymbolTable {
        var self = SymbolTable{ .allocator = allocator, .names = .init(allocator) };
        self.symbols.ensureTotalCapacity(allocator, estimated_symbols) catch {};
        self.names.ensureTotalCapacity(estimated_symbols) catch {};
        self.shadows.ensureTotalCapacity(allocator, estimated_symbols) catch {};
        self.save_points.ensureTotalCapacity(allocator, 64) catch {};
        return self;
    }

    /// Mark scope entry. Must be paired with `popScope`.
    pub fn pushScope(self: *SymbolTable) void {
        self.save_points.append(self.allocator, @intCast(self.shadows.items.len)) catch unreachable;
    }

    /// Restore all names shadowed since the matching `pushScope`.
    pub fn popScope(self: *SymbolTable) void {
        const base = self.save_points.pop().?;
        while (self.shadows.items.len > base) {
            const entry = self.shadows.pop().?;
            if (entry.prev == .none) {
                _ = self.names.remove(entry.name);
            } else {
                self.names.putAssumeCapacity(entry.name, entry.prev);
            }
        }
    }

    /// Declare a symbol in the current scope. Returns its SymbolId.
    pub fn declare(self: *SymbolTable, sym: Symbol, name_slice: []const u8) SymbolId {
        const id: SymbolId = @enumFromInt(@as(u32, @intCast(self.symbols.items.len)));

        self.symbols.append(self.allocator, sym) catch unreachable;

        const prev = self.names.get(name_slice) orelse .none;
        self.shadows.append(self.allocator, .{ .name = name_slice, .prev = prev }) catch unreachable;
        self.names.put(name_slice, id) catch unreachable;
        return id;
    }

    /// O(1) name resolution. Returns the innermost visible symbol.
    pub inline fn resolve(self: *const SymbolTable, name_slice: []const u8) ?SymbolId {
        return self.names.get(name_slice);
    }

    /// Get symbol data by ID.
    pub inline fn get(self: *const SymbolTable, id: SymbolId) Symbol {
        return self.symbols.items[@intFromEnum(id)];
    }

    /// Get mutable pointer to symbol data by ID.
    pub inline fn getPtr(self: *SymbolTable, id: SymbolId) *Symbol {
        return &self.symbols.items[@intFromEnum(id)];
    }

    /// All declared symbols as a flat array.
    pub inline fn all(self: *const SymbolTable) []const Symbol {
        return self.symbols.items;
    }

    /// Number of declared symbols.
    pub inline fn count(self: *const SymbolTable) u32 {
        return @intCast(self.symbols.items.len);
    }
};
