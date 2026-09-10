const std = @import("std");
const parser = @import("parser");
const helpers = @import("../helpers.zig");

const ast = parser.ast;
const semantic = parser.semantic;
const Semantic = semantic.Semantic;
const traverser = parser.traverser;
const SymbolId = traverser.semantic.SymbolId;
const ScopeId = traverser.semantic.ScopeId;

const testing = std.testing;

fn analyze(source: []const u8, opts: parser.Options) !helpers.Analyzed {
    return helpers.analyze(testing.allocator, source, opts);
}

fn analyzeAllowErrors(source: []const u8, opts: parser.Options) !helpers.Analyzed {
    return helpers.analyzeAllowErrors(testing.allocator, source, opts);
}

test "lookup agrees with resolution when a var passes through a catch scope" {
    var a = try helpers.analyzeAllowErrors(
        testing.allocator,
        "try {} catch (e) { { var e; e; } }",
        .{ .source_type = .script },
    );
    defer a.deinit();
    try testing.expect(!a.tree.hasErrors());

    var buf: [4]Semantic.SymbolEntry = undefined;
    const es = a.symbolsNamed("e", &buf);
    try testing.expectEqual(@as(usize, 2), es.len);
    const catch_param = if (es[0].symbol.flags.catch_var) es[0] else es[1];
    const hoisted_var = if (es[0].symbol.flags.catch_var) es[1] else es[0];
    try testing.expect(catch_param.symbol.flags.catch_var);
    try testing.expect(hoisted_var.symbol.flags.isHoistingVar());

    const ref = try a.onlyReferenceNamed("e");
    try testing.expectEqual(catch_param.id, ref.reference.symbol);
    try testing.expectEqual(catch_param.id, a.sem.lookup(ref.reference.scope, "e", .any).?);
    try testing.expectEqual(hoisted_var.id, a.sem.binding(ref.reference.scope, "e").?);
}

test "symbolOf answers for both declaration and reference nodes" {
    var a = try analyze("let q = 1; q;", .{});
    defer a.deinit();

    const q = try a.symbolNamed("q");
    const binding_node = try a.bindingNamed("q");
    const ref_node = try a.referenceNamed("q");

    try testing.expectEqual(q.id, a.sem.symbolOf(binding_node).?);
    try testing.expectEqual(q.id, a.sem.symbolOf(ref_node).?);

    try testing.expect(a.sem.referenceOf(ref_node) != null);
    try testing.expectEqual(
        @as(?traverser.semantic.ReferenceId, null),
        a.sem.referenceOf(binding_node),
    );

    try testing.expectEqual(@as(?SymbolId, null), a.sem.symbolOf(a.tree.root));
}

test "parentOf and ancestors walk the structural tree" {
    var a = try analyze("let q = 1;", .{});
    defer a.deinit();

    const binding_node = try a.bindingNamed("q");
    const declarator = a.sem.parentOf(binding_node).?;
    try testing.expect(a.tree.data(declarator) == .variable_declarator);
    const declaration = a.sem.parentOf(declarator).?;
    try testing.expect(a.tree.data(declaration) == .variable_declaration);
    try testing.expectEqual(a.tree.root, a.sem.parentOf(declaration).?);
    try testing.expectEqual(@as(?ast.NodeIndex, null), a.sem.parentOf(a.tree.root));

    var it = a.sem.ancestors(binding_node);
    try testing.expectEqual(binding_node, it.next().?);
    try testing.expectEqual(declarator, it.next().?);
    try testing.expectEqual(declaration, it.next().?);
    try testing.expectEqual(a.tree.root, it.next().?);
    try testing.expectEqual(@as(?ast.NodeIndex, null), it.next());
}

test "lookup walks the scope chain, binding does not" {
    var a = try analyze("let outer = 1; { let inner = 2; }", .{});
    defer a.deinit();

    const outer = try a.symbolNamed("outer");
    const inner = try a.symbolNamed("inner");
    const block_scope = inner.symbol.scope;

    try testing.expectEqual(inner.id, a.sem.lookup(block_scope, "inner", .any).?);
    try testing.expectEqual(outer.id, a.sem.lookup(block_scope, "outer", .any).?);
    try testing.expectEqual(@as(?SymbolId, null), a.sem.lookup(block_scope, "missing", .any));

    try testing.expectEqual(@as(?SymbolId, null), a.sem.binding(block_scope, "outer"));
    try testing.expectEqual(inner.id, a.sem.binding(block_scope, "inner").?);
}

test "iterators yield ids that round-trip through the accessors" {
    var a = try analyze("let x = 1; function f(y) { x; y; }", .{});
    defer a.deinit();

    var scope_count: usize = 0;
    var scope_it = a.sem.iterScopes();
    while (scope_it.next()) |entry| {
        scope_count += 1;
        const direct = a.sem.scope(entry.id);
        try testing.expectEqual(direct.kind, entry.scope.kind);
        try testing.expectEqual(direct.node, entry.scope.node);
    }
    try testing.expectEqual(a.sem.scopes.list.len, scope_count);

    var symbol_count: usize = 0;
    var sym_it = a.sem.iterSymbols();
    while (sym_it.next()) |entry| {
        symbol_count += 1;
        try testing.expectEqual(
            a.sem.symbol(entry.id).name,
            entry.symbol.name,
        );
    }
    try testing.expectEqual(a.sem.symbols.len, symbol_count);

    var ref_count: usize = 0;
    var ref_it = a.sem.iterReferences();
    while (ref_it.next()) |entry| {
        ref_count += 1;
        try testing.expectEqual(
            a.sem.reference(entry.id).node,
            entry.reference.node,
        );
        try testing.expectEqual(entry.id, a.sem.referenceOf(entry.reference.node).?);
    }
    try testing.expectEqual(a.sem.references.len, ref_count);
}

test "every declared symbol is reachable through its scope's bindings" {
    var a = try analyze("let x = 1; function f(y) { var z; }", .{});
    defer a.deinit();

    var sym_it = a.sem.iterSymbols();
    while (sym_it.next()) |entry| {
        const found = a.sem.binding(
            entry.symbol.scope,
            a.tree.string(entry.symbol.name),
        ) orelse return error.SymbolNotInScopeMap;
        try testing.expectEqual(entry.id, found);
    }
}

const CtxProbeVisitor = struct {
    saw_probe: bool = false,
    saw_type_ref: bool = false,
    failure: ?anyerror = null,

    pub fn enter_identifier_reference(
        self: *CtxProbeVisitor,
        id: ast.IdentifierReference,
        index: ast.NodeIndex,
        ctx: *traverser.semantic.Ctx,
    ) parser.traverser.Action {
        _ = index;
        const name = ctx.tree.string(id.name);
        if (std.mem.eql(u8, name, "probe")) {
            self.saw_probe = true;
            self.checkProbe(ctx) catch |err| {
                self.failure = err;
            };
        }
        return .proceed;
    }

    pub fn enter_ts_type_reference(
        self: *CtxProbeVisitor,
        ref: ast.TSTypeReference,
        index: ast.NodeIndex,
        ctx: *traverser.semantic.Ctx,
    ) parser.traverser.Action {
        _ = ref;
        _ = index;
        self.saw_type_ref = true;
        self.check(ctx.inTypePosition()) catch |err| {
            self.failure = err;
        };
        return .proceed;
    }

    fn check(self: *CtxProbeVisitor, ok: bool) !void {
        _ = self;
        try testing.expect(ok);
    }

    fn checkProbe(self: *CtxProbeVisitor, ctx: *traverser.semantic.Ctx) !void {
        _ = self;
        try testing.expect(!ctx.inTypePosition());
        try testing.expect(ctx.inTsNamespace());

        try testing.expectEqual(
            @as(?SymbolId, null),
            ctx.symbols.ownBinding(ctx.scope.current, "hv"),
        );
        const hv = ctx.symbols.binding(ctx.scope.current, "hv") orelse
            return error.HoistingVarInvisible;
        try testing.expect(ctx.symbols.symbol(hv).flags.function_scoped_var);
        const first = ctx.symbols.firstDeclOf(hv);
        try testing.expect(ctx.tree.data(first) == .binding_identifier);
    }
};

test "the semantic Ctx exposes type position, namespace state, and the tracker" {
    const source =
        \\namespace N {
        \\  export function f(): void {
        \\    { var hv; probe; }
        \\  }
        \\  export let ann: SomeType;
        \\}
    ;
    var tree = try parser.parse(testing.allocator, source, .{ .lang = .ts });
    defer tree.deinit();
    try testing.expect(!tree.hasErrors());

    var visitor = CtxProbeVisitor{};
    _ = try traverser.semantic.traverse(CtxProbeVisitor, &tree, &visitor);

    try testing.expect(visitor.saw_probe);
    try testing.expect(visitor.saw_type_ref);
    if (visitor.failure) |err| return err;
}
