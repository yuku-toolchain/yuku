const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");
const sc = @import("tracker/scope.zig");
const sy = @import("tracker/symbol.zig");

const Allocator = std.mem.Allocator;

pub const ScopeId = sc.ScopeId;
pub const Scope = sc.Scope;
pub const ScopeTree = sc.ScopeTree;
pub const ScopeTracker = sc.ScopeTracker;
pub const SymbolId = sy.SymbolId;
pub const ReferenceId = sy.ReferenceId;
pub const Symbol = sy.Symbol;
pub const Reference = sy.Reference;
pub const SymbolTable = sy.SymbolTable;
pub const SymbolTracker = sy.SymbolTracker;

pub const Ctx = struct {
    tree: *const ast.TreeBuilder,
    path: wk.NodePath = .{},
    scope: ScopeTracker,
    symbols: SymbolTracker,

    pub fn init(tree: *const ast.TreeBuilder, allocator: Allocator) Allocator.Error!Ctx {
        return .{
            .tree = tree,
            .scope = try ScopeTracker.init(tree, allocator),
            .symbols = try SymbolTracker.init(tree, allocator),
        };
    }

    pub fn enter(self: *Ctx, index: ast.NodeIndex, data: ast.NodeData) Allocator.Error!void {
        self.path.push(index);
        try self.scope.enter(index, data);
        self.symbols.setBindingContext(data);
    }

    pub fn post_enter(self: *Ctx, index: ast.NodeIndex, data: ast.NodeData) Allocator.Error!void {
        try self.symbols.declareBindings(index, data, &self.scope);
    }

    pub fn exit(self: *Ctx, data: ast.NodeData) void {
        self.symbols.exit(data);
        self.scope.exit(data);
        self.path.pop();
    }

    /// Finalizes into a `Result` containing the scope tree and symbol table.
    pub fn toResult(self: *Ctx) Allocator.Error!Result {
        return .{
            .scope_tree = try self.scope.toScopeTree(),
            .symbol_table = try self.symbols.toSymbolTable(),
        };
    }

    /// Frees all resources. Only needed if the traversal is aborted early.
    pub fn deinit(self: *Ctx) void {
        self.scope.deinit();
        self.symbols.deinit();
    }
};

/// Combined output of a semantic traversal.
pub const Result = struct {
    scope_tree: ScopeTree,
    symbol_table: SymbolTable,

    /// Frees both the scope tree and symbol table.
    pub fn deinit(self: *Result) void {
        self.scope_tree.deinit();
        self.symbol_table.deinit();
    }
};

/// Walks the tree with full path, scope, and symbol tracking.
/// Returns a `Result` containing the scope tree and symbol table.
pub fn traverse(comptime V: type, tree: *const ast.TreeBuilder, visitor: *V, allocator: Allocator) Allocator.Error!Result {
    var ctx = try Ctx.init(tree, allocator);
    errdefer ctx.deinit();

    var layer = wk.Layer(Ctx, V){ .inner = visitor };

    try wk.walk(Ctx, wk.Layer(Ctx, V), &layer, &ctx);

    return try ctx.toResult();
}
