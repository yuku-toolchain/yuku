//! Corpus sweep. Semantic-model invariants must hold for every fixture
//! that parses cleanly, covering scope-tree shape, symbol and scope-map
//! agreement, reference resolution agreeing with `lookup`, and the decl
//! and use cross-indexes.

const std = @import("std");
const parser = @import("parser");
const helpers = @import("../helpers.zig");

const ast = parser.ast;
const semantic = parser.semantic;
const Semantic = semantic.Semantic;
const ScopeId = parser.traverser.semantic.ScopeId;

const InvariantChecker = struct {
    pub fn check(self: InvariantChecker, path: []const u8, tree: *ast.Tree) !void {
        _ = self;
        const sem = try semantic.analyze(tree);
        verify(tree, &sem) catch |err| {
            std.debug.print("{s}: semantic invariant violated: {t}\n", .{ path, err });
            return err;
        };
    }
};

fn verify(tree: *const ast.Tree, sem: *const Semantic) !void {
    try verifyScopes(tree, sem);
    try verifySymbols(tree, sem);
    try verifyReferences(tree, sem);
    try verifyNodeTables(tree, sem);
}

fn verifyScopes(tree: *const ast.Tree, sem: *const Semantic) !void {
    var it = sem.iterScopes();
    while (it.next()) |entry| {
        const scope = entry.scope;
        if (scope.node == .null) return error.ScopeWithoutNode;
        if (@intFromEnum(scope.node) >= tree.nodes.len) return error.ScopeNodeOutOfBounds;

        if (entry.id == .root) {
            if (scope.parent != .none) return error.RootScopeHasParent;
            if (scope.kind != .global) return error.RootScopeNotGlobal;
        } else {
            if (scope.parent == .none) return error.OrphanScope;
            if (@intFromEnum(scope.parent) >= @intFromEnum(entry.id))
                return error.ParentScopeCreatedAfterChild;
        }

        const target = sem.scope(scope.hoist_target);
        if (!target.kind.isHoistTarget()) return error.HoistTargetNotHoistable;
        if (scope.kind.isHoistTarget()) {
            if (scope.hoist_target != entry.id) return error.HoistTargetNotSelf;
        } else {
            if (scope.hoist_target != sem.scope(scope.parent).hoist_target)
                return error.HoistTargetSkipsParent;
        }

        var steps: usize = 0;
        var chain = sem.scopes.ancestors(entry.id);
        while (chain.next()) |_| : (steps += 1) {
            if (steps > sem.scopes.list.len) return error.ScopeChainCycle;
        }
    }
}

fn verifySymbols(tree: *const ast.Tree, sem: *const Semantic) !void {
    var it = sem.iterSymbols();
    while (it.next()) |entry| {
        const symbol = entry.symbol;
        if (symbol.scope == .none) return error.SymbolWithoutScope;

        const decls = sem.decls(entry.id);
        if (decls.len == 0) return error.SymbolWithoutDeclaration;
        for (decls) |decl| {
            if (decl == .null) return error.NullDeclarationNode;
            if (sem.symbolOf(decl) != entry.id) return error.DeclarationNodeMismatch;
        }

        const name = tree.string(symbol.name);
        const bound = sem.binding(symbol.scope, name) orelse return error.SymbolNotInScopeMap;
        if (bound != entry.id) return error.ScopeMapPointsElsewhere;

        for (sem.uses(entry.id)) |use| {
            if (sem.reference(use).symbol != entry.id) return error.UseIndexMismatch;
        }
    }
}

fn verifyReferences(tree: *const ast.Tree, sem: *const Semantic) !void {
    var resolved_count: usize = 0;
    var it = sem.iterReferences();
    while (it.next()) |entry| {
        const ref = entry.reference;
        if (ref.node == .null) return error.ReferenceWithoutNode;
        if (ref.scope == .none) return error.ReferenceWithoutScope;
        if (sem.referenceOf(ref.node) != entry.id) return error.ReferenceNodeMismatch;

        const name = tree.string(ref.name);
        const expected = sem.lookup(ref.scope, name);
        if (ref.symbol == .none) {
            if (expected != null) return error.UnresolvedButBound;
            continue;
        }
        resolved_count += 1;

        const symbol = sem.symbol(ref.symbol);
        if (!std.mem.eql(u8, tree.string(symbol.name), name))
            return error.ResolvedNameMismatch;
        if (expected != ref.symbol) return error.ResolvedToWrongBinding;

        var on_chain = false;
        var chain = sem.scopes.ancestors(ref.scope);
        while (chain.next()) |scope_id| {
            if (scope_id == symbol.scope) {
                on_chain = true;
                break;
            }
        }
        if (!on_chain) return error.SymbolScopeNotOnChain;
    }

    var use_total: usize = 0;
    var symbols = sem.iterSymbols();
    while (symbols.next()) |entry| use_total += sem.uses(entry.id).len;
    if (use_total != resolved_count) return error.UseIndexNotPartition;
}

fn verifyNodeTables(tree: *const ast.Tree, sem: *const Semantic) !void {
    if (sem.parentOf(tree.root) != null) return error.RootHasParent;

    const top: ScopeId = if (tree.isModule()) .module else .root;
    if (sem.scopeOf(tree.root) != top) return error.RootScopeMismatch;

    var i: u32 = 0;
    while (i < tree.nodes.len) : (i += 1) {
        const node: ast.NodeIndex = @enumFromInt(i);
        if (@intFromEnum(sem.scopeOf(node)) >= sem.scopes.list.len)
            return error.NodeScopeOutOfBounds;
        if (sem.parentOf(node)) |p| {
            if (@intFromEnum(p) >= tree.nodes.len) return error.NodeParentOutOfBounds;
        }
    }
}

test "semantic model invariants hold across the parser corpus" {
    try helpers.forEachCorpusTree(std.testing.allocator, InvariantChecker{});
}
