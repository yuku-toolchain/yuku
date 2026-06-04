const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");
const sc = @import("../semantic/scope.zig");
const bi = @import("../semantic/binder.zig");

const Allocator = std.mem.Allocator;

pub const Scope = sc.Scope;
pub const ScopeId = sc.ScopeId;
pub const ScopeTree = sc.ScopeTree;
pub const ScopeTracker = sc.ScopeTracker;
pub const SymbolId = bi.SymbolId;
pub const ReferenceId = bi.ReferenceId;
pub const Symbol = bi.Symbol;
pub const Reference = bi.Reference;
pub const SymbolTable = bi.SymbolTable;
pub const SymbolTracker = bi.SymbolTracker;

/// Walk context combining the path stack, scope tracker, and symbol
/// tracker. Visitor hooks receive `*Ctx` and use it to inspect the
/// surrounding source structure (`scope`, `path`, `inTypePosition`) or
/// to query and contribute symbols (`symbols`).
pub const Ctx = struct {
    tree: *const ast.Tree,
    path: wk.NodePath = .{},
    scope: ScopeTracker,
    symbols: SymbolTracker,
    // depth of ts type-only context. inspect via `inTypePosition()`.
    type_position_depth: u32 = 0,
    // depth of ts namespace bodies. inspect via `inTsNamespace()`.
    ts_namespace_depth: u32 = 0,

    pub fn init(tree: *ast.Tree) Allocator.Error!Ctx {
        return .{
            .tree = tree,
            .scope = try ScopeTracker.init(tree),
            .symbols = try SymbolTracker.init(tree),
        };
    }

    /// True when the walker is currently inside a TS type-only subtree.
    pub inline fn inTypePosition(self: *const Ctx) bool {
        return self.type_position_depth > 0;
    }

    /// True when the walker is currently inside a TS namespace body.
    pub inline fn inTsNamespace(self: *const Ctx) bool {
        return self.ts_namespace_depth > 0;
    }

    pub inline fn enter(self: *Ctx, index: ast.NodeIndex, data: ast.NodeData) Allocator.Error!void {
        self.path.push(index);
        try self.scope.enter(index, data);

        // namespace bodies count as type position so their type-only
        // members bind correctly
        if (data == .ts_module_block) self.ts_namespace_depth += 1;
        if (data.isTypeContext() or data == .ts_module_block) self.type_position_depth += 1;

        try self.symbols.setBindingContext(data, &self.scope);
    }

    pub inline fn post_enter(
        self: *Ctx,
        index: ast.NodeIndex,
        data: ast.NodeData,
    ) Allocator.Error!void {
        try self.symbols.declareBindings(index, data, &self.scope, self.inTypePosition());
    }

    pub inline fn exit(self: *Ctx, data: ast.NodeData) void {
        self.symbols.exit(data);
        self.scope.exit(data);
        if (data == .ts_module_block) {
            std.debug.assert(self.ts_namespace_depth > 0);
            self.ts_namespace_depth -= 1;
        }
        if (data.isTypeContext() or data == .ts_module_block) {
            std.debug.assert(self.type_position_depth > 0);
            self.type_position_depth -= 1;
        }
        self.path.pop();
    }
};

/// Combined output of a semantic traversal. Holds the scope tree and
/// the symbol table.
pub const Result = struct {
    scope_tree: ScopeTree,
    symbol_table: SymbolTable,
};

/// Walks the tree with full path, scope, and symbol tracking. Returns
/// the scope tree and the unresolved symbol table. Call
/// `SymbolTable.resolveAll` to build the reference cross-index.
pub fn traverse(comptime V: type, tree: *ast.Tree, visitor: *V) Allocator.Error!Result {
    std.debug.assert(tree.root != .null);
    var ctx = try Ctx.init(tree);
    var layer = wk.Layer(Ctx, V){ .inner = visitor };
    try wk.walk(Ctx, wk.Layer(Ctx, V), &layer, &ctx);
    return .{
        .scope_tree = ctx.scope.toScopeTree(),
        .symbol_table = try ctx.symbols.toSymbolTable(),
    };
}
