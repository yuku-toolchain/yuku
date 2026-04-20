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
    tree: *const ast.Tree,
    path: wk.NodePath = .{},
    scope: ScopeTracker,
    symbols: SymbolTracker,

    pub fn init(tree: *ast.Tree) Allocator.Error!Ctx {
        return .{
            .tree = tree,
            .scope = try ScopeTracker.init(tree),
            .symbols = try SymbolTracker.init(tree),
        };
    }

    /// typescript types live in a separate namespace from javascript values
    /// and are erased at runtime. the semantic traverser skips their subtrees
    /// entirely so type-only identifiers never enter the js scope maps.
    pub fn shouldWalk(_: *const Ctx, data: ast.NodeData) bool {
        return switch (data) {
            .ts_export_assignment, .ts_parameter_property, .ts_as_expression, .ts_satisfies_expression => true,
            inline else => |_, tag| comptime !std.mem.startsWith(u8, @tagName(tag), "ts_"),
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
};

/// Combined output of a semantic traversal.
pub const Result = struct {
    scope_tree: ScopeTree,
    symbol_table: SymbolTable,
};

/// Walks the tree with full path, scope, and symbol tracking.
/// Returns a `Result` containing the scope tree and symbol table.
pub fn traverse(comptime V: type, tree: *ast.Tree, visitor: *V) Allocator.Error!Result {
    var ctx = try Ctx.init(tree);

    var layer = wk.Layer(Ctx, V){ .inner = visitor };

    try wk.walk(Ctx, wk.Layer(Ctx, V), &layer, &ctx);

    return .{
        .scope_tree = ctx.scope.toScopeTree(),
        .symbol_table = ctx.symbols.toSymbolTable(),
    };
}
