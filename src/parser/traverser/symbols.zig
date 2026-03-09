const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");
const sc = @import("tracker/scope.zig");
const sy = @import("tracker/symbol.zig");

const Allocator = std.mem.Allocator;
const NodeTag = std.meta.Tag(ast.NodeData);

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

    pub fn onEnter(self: *Ctx, index: ast.NodeIndex, tag: NodeTag) Allocator.Error!void {
        try self.scope.enter(index, tag);
        try self.symbols.enter(index, tag, &self.scope);
    }

    pub fn onExit(self: *Ctx, _: ast.NodeIndex, tag: NodeTag) void {
        self.scope.exit(tag);
        self.symbols.exit(tag);
    }

    /// Returns the completed result and releases builder resources.
    pub fn toResult(self: *Ctx) Allocator.Error!Result {
        return .{
            .scope_tree = try self.scope.toScopeTree(),
            .symbol_table = try self.symbols.toSymbolTable(),
        };
    }

    /// Release all owned memory without producing a result.
    pub fn deinit(self: *Ctx) void {
        self.scope.deinit();
        self.symbols.deinit();
    }
};

/// The immutable result of a symbol-tracking traversal.
pub const Result = struct {
    scope_tree: ScopeTree,
    symbol_table: SymbolTable,

    pub fn deinit(self: *Result) void {
        self.scope_tree.deinit();
        self.symbol_table.deinit();
    }
};

/// Run a symbol-tracking traversal over the parse tree.
///
/// Walks the AST while automatically tracking scopes and collecting symbols
/// and references. The visitor receives a `*symbols.Ctx` as its context,
/// giving access to scope and symbol information at every node via
/// `ctx.scope` and `ctx.symbols`. Returns the completed `Result`.
pub fn traverse(comptime V: type, tree: *const ast.ParseTree, visitor: *V, allocator: Allocator) Allocator.Error!Result {
    var ctx = try Ctx.init(tree, allocator);
    errdefer ctx.deinit();
    try wk.walk(Ctx, V, visitor, &ctx);
    return try ctx.toResult();
}
