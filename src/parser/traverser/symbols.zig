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

// full traverser context: path + scopes + symbols.
//
// the tracking is split into two phases around user hooks:
//
//   enter:       push path, push scopes, set binding context
//                (now user hooks fire, they can inspect existing symbols)
//   post_enter:  declare the new binding / record the reference
//                (symbol is added to the table after the user had a chance to check)
//   exit:        clean up symbols, scopes, and path in reverse order
//
// this split matters for things like redeclaration checking. the user's
// enter_binding_identifier hook runs between setBindingContext and
// declare_bindings, so it sees the scope state before the new symbol
// is added and can check for conflicts.
pub const Ctx = struct {
    tree: *const ast.ParseTree,
    path: wk.NodePath = .{},
    scope: ScopeTracker,
    symbols: SymbolTracker,

    pub fn init(tree: *const ast.ParseTree, allocator: Allocator) Allocator.Error!Ctx {
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
        try self.symbols.declare_bindings(index, data, &self.scope);
    }

    pub fn exit(self: *Ctx, data: ast.NodeData) void {
        self.symbols.exit(data);
        self.scope.exit(data);
        self.path.pop();
    }

    pub fn toResult(self: *Ctx) Allocator.Error!Result {
        return .{
            .scope_tree = try self.scope.toScopeTree(),
            .symbol_table = try self.symbols.toSymbolTable(),
        };
    }

    pub fn deinit(self: *Ctx) void {
        self.scope.deinit();
        self.symbols.deinit();
    }
};

pub const Result = struct {
    scope_tree: ScopeTree,
    symbol_table: SymbolTable,

    pub fn deinit(self: *Result) void {
        self.scope_tree.deinit();
        self.symbol_table.deinit();
    }
};

pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V, allocator: Allocator) Allocator.Error!Result {
    var ctx = try Ctx.init(tree, allocator);
    errdefer ctx.deinit();
    var layer = wk.Layer(Ctx, V){ .inner = visitor };
    try wk.walk(Ctx, wk.Layer(Ctx, V), &layer, &ctx);
    return try ctx.toResult();
}
