const std = @import("std");
const parser = @import("parser");
const helpers = @import("../helpers.zig");

const ast = parser.ast;
const semantic = parser.semantic;
const Semantic = semantic.Semantic;
const Scope = semantic.Scope;
const ScopeId = semantic.ScopeId;
const Symbol = semantic.Symbol;
const Reference = semantic.Reference;

fn checkInvariants(path: []const u8, tree: *ast.Tree) !void {
    const checked = try semantic.analyze(tree, .{ .check = true });
    verify(tree, &checked) catch |err| {
        std.debug.print("{s}: semantic invariant violated: {t}\n", .{ path, err });
        return err;
    };

    const unchecked = try semantic.analyze(tree, .{ .check = false });
    verifySameModel(&checked, &unchecked) catch |err| {
        std.debug.print("{s}: checking changed the semantic model: {t}\n", .{ path, err });
        return err;
    };

    verifyRename(tree, &unchecked) catch |err| {
        std.debug.print("{s}: renaming every symbol changed the program: {t}\n", .{ path, err });
        return err;
    };
}

// every declaration and use renamed through setName resolves as before
fn verifyRename(tree: *ast.Tree, sem: *const Semantic) !void {
    for (sem.symbols, 0..) |symbol, i| {
        const id: semantic.SymbolId = @fromBackingInt(@intCast(i));
        const old = tree.string(symbol.name);
        const text = try std.fmt.allocPrint(tree.allocator(), "ŷ_{s}", .{old});
        const name = try tree.addString(text);
        for (sem.decls(id)) |node| tree.setName(node, name);
        for (sem.uses(id)) |use| tree.setName(sem.reference(use).node, name);
    }

    const renamed = try semantic.analyze(tree, .{ .check = false });
    if (renamed.symbols.len != sem.symbols.len) return error.SymbolCountChanged;
    if (resolvedCount(&renamed) != resolvedCount(sem)) return error.ResolutionChanged;
    for (renamed.symbols) |symbol| {
        const name = tree.string(symbol.name);
        if (!std.mem.startsWith(u8, name, "ŷ_")) return error.DeclarationMissed;
    }
}

fn resolvedCount(sem: *const Semantic) usize {
    var count: usize = 0;
    for (sem.references) |reference| {
        if (reference.symbol != .none) count += 1;
    }
    return count;
}

fn verify(tree: *const ast.Tree, sem: *const Semantic) !void {
    try verifyScopes(tree, sem);
    try verifySymbols(tree, sem);
    try verifyReferences(tree, sem);
    try verifyNodeTables(tree, sem);
}

fn verifyScopes(tree: *const ast.Tree, sem: *const Semantic) !void {
    for (sem.scopes.list, 0..) |scope, i| {
        const id: ScopeId = @fromBackingInt(@intCast(i));
        if (scope.node == .null) return error.ScopeWithoutNode;
        if (@backingInt(scope.node) >= tree.nodes.len) return error.ScopeNodeOutOfBounds;

        if (id == .root) {
            if (scope.parent != .none) return error.RootScopeHasParent;
            if (scope.kind != .global) return error.RootScopeNotGlobal;
        } else {
            if (scope.parent == .none) return error.OrphanScope;
            if (@backingInt(scope.parent) >= @backingInt(id)) {
                return error.ParentScopeCreatedAfterChild;
            }
        }

        const target = sem.scopes.get(scope.hoist_target);
        if (!target.kind.isHoistTarget()) return error.HoistTargetNotHoistable;
        if (scope.kind == .function_body) {
            // its own var environment only with parameter expressions
            if (scope.hoist_target != id and
                scope.hoist_target != sem.scopes.get(scope.parent).hoist_target)
            {
                return error.FunctionBodyHoistTargetInvalid;
            }
        } else if (scope.kind.isHoistTarget()) {
            if (scope.hoist_target != id) return error.HoistTargetNotSelf;
        } else {
            if (scope.hoist_target != sem.scopes.get(scope.parent).hoist_target) {
                return error.HoistTargetSkipsParent;
            }
        }

        var steps: usize = 0;
        var chain = sem.scopes.ancestors(id);
        while (chain.next()) |_| : (steps += 1) {
            if (steps > sem.scopes.list.len) return error.ScopeChainCycle;
        }
    }
}

fn verifySymbols(tree: *const ast.Tree, sem: *const Semantic) !void {
    for (sem.symbols, 0..) |symbol, i| {
        const id: semantic.SymbolId = @fromBackingInt(@intCast(i));
        if (symbol.scope == .none) return error.SymbolWithoutScope;

        const decls = sem.decls(id);
        if (decls.len == 0) return error.SymbolWithoutDeclaration;
        for (decls) |decl| {
            if (decl == .null) return error.NullDeclarationNode;
            if (sem.symbolOf(decl) != id) return error.DeclarationNodeMismatch;
        }

        const name = tree.string(symbol.name);
        const bound = sem.binding(symbol.scope, name) orelse return error.SymbolNotInScopeMap;
        if (bound != id) return error.ScopeMapPointsElsewhere;

        for (sem.uses(id)) |use| {
            if (sem.reference(use).symbol != id) return error.UseIndexMismatch;
        }
    }
}

fn verifyReferences(tree: *const ast.Tree, sem: *const Semantic) !void {
    var resolved_count: usize = 0;
    for (sem.references, 0..) |ref, i| {
        const id: semantic.ReferenceId = @fromBackingInt(@intCast(i));
        if (ref.node == .null) return error.ReferenceWithoutNode;
        if (ref.scope == .none) return error.ReferenceWithoutScope;
        if (sem.referenceOf(ref.node) != id) return error.ReferenceNodeMismatch;

        const name = tree.string(ref.name);
        const expected = sem.lookup(ref.scope, name, ref.flags.space);
        // resolution knows the arguments barrier and type parameter visibility, lookup does not
        const rules_may_differ = std.mem.eql(u8, name, "arguments") or
            (expected != null and positionDependent(sem.symbol(expected.?).flags));
        if (ref.symbol == .none) {
            if (expected != null and !rules_may_differ) return error.UnresolvedButBound;
            continue;
        }
        resolved_count += 1;

        const symbol = sem.symbol(ref.symbol);
        if (!std.mem.eql(u8, tree.string(symbol.name), name)) return error.ResolvedNameMismatch;
        if (expected != ref.symbol and !rules_may_differ) return error.ResolvedToWrongBinding;
        if (!isOnChain(sem, &ref, name)) return error.SymbolScopeNotOnChain;
    }

    var use_total: usize = 0;
    for (0..sem.symbols.len) |i| use_total += sem.uses(@fromBackingInt(@intCast(i))).len;
    if (use_total != resolved_count) return error.UseIndexNotPartition;
}

fn positionDependent(flags: Symbol.Flags) bool {
    return flags.type_parameter or flags.parameter or flags.isHoistingVar();
}

fn isOnChain(sem: *const Semantic, ref: *const Reference, name: []const u8) bool {
    const scope = sem.symbol(ref.symbol).scope;
    var chain = sem.scopes.ancestors(ref.scope);
    while (chain.next()) |scope_id| {
        if (scope_id == scope) return true;
        if (sem.binding(scope_id, name) == ref.symbol) return true;
        if (isOtherBody(sem, scope_id, scope)) return true;
    }
    return false;
}

fn isOtherBody(sem: *const Semantic, body: ScopeId, other: ScopeId) bool {
    var next = sem.next_bodies[@backingInt(body)];
    if (next == .none) return false;
    while (next != body) : (next = sem.next_bodies[@backingInt(next)]) {
        if (next == other) return true;
    }
    return false;
}

fn verifySameModel(checked: *const Semantic, unchecked: *const Semantic) !void {
    if (!eqlElements(Scope, checked.scopes.list, unchecked.scopes.list)) {
        return error.ScopesDiffer;
    }
    if (!eqlElements(Symbol, checked.symbols, unchecked.symbols)) {
        return error.SymbolsDiffer;
    }
    if (!eqlElements(Reference, checked.references, unchecked.references)) {
        return error.ReferencesDiffer;
    }
    for (0..checked.symbols.len) |i| {
        const id: semantic.SymbolId = @fromBackingInt(@intCast(i));
        if (!std.mem.eql(ast.NodeIndex, checked.decls(id), unchecked.decls(id))) {
            return error.DeclarationsDiffer;
        }
    }
    if (!std.mem.eql(ScopeId, checked.node_scopes, unchecked.node_scopes)) {
        return error.NodeScopesDiffer;
    }
    if (!std.mem.eql(ast.NodeIndex, checked.node_parents, unchecked.node_parents)) {
        return error.NodeParentsDiffer;
    }
}

fn eqlElements(comptime T: type, a: []const T, b: []const T) bool {
    if (a.len != b.len) return false;
    for (a, b) |x, y| {
        if (!std.meta.eql(x, y)) return false;
    }
    return true;
}

fn verifyNodeTables(tree: *const ast.Tree, sem: *const Semantic) !void {
    if (sem.parentOf(tree.root) != null) return error.RootHasParent;

    const top: Scope.Kind = if (tree.isModule()) .module else .global;
    if (sem.scopes.get(sem.scopeOf(tree.root)).kind != top) return error.RootScopeMismatch;

    var i: u32 = 0;
    while (i < tree.nodes.len) : (i += 1) {
        const node: ast.NodeIndex = @fromBackingInt(i);
        if (@backingInt(sem.scopeOf(node)) >= sem.scopes.list.len) {
            return error.NodeScopeOutOfBounds;
        }
        if (sem.parentOf(node)) |p| {
            if (@backingInt(p) >= tree.nodes.len) return error.NodeParentOutOfBounds;
        }
    }
}

test "semantic model invariants hold across the parser corpus" {
    try helpers.forEachCorpusTree(checkInvariants);
}
