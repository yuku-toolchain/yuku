//! Scope tree construction. Kinds, strict-mode propagation, hoist
//! targets, catch-body sharing, expression-name scopes, decorator
//! retargeting, and ancestor iteration.

const std = @import("std");
const parser = @import("parser");
const helpers = @import("../helpers.zig");

const ast = parser.ast;
const semantic = parser.semantic;
const sc = parser.traverser.scoped;
const ScopeId = sc.ScopeId;
const Scope = sc.Scope;

const testing = std.testing;

fn analyze(source: []const u8, opts: parser.Options) !helpers.Analyzed {
    return helpers.analyze(testing.allocator, source, opts);
}

fn declScope(a: *const helpers.Analyzed, name: []const u8) !Scope {
    const entry = try a.symbolNamed(name);
    return a.sem.scope(entry.symbol.scope);
}

fn countKind(a: *const helpers.Analyzed, kind: Scope.Kind) usize {
    var count: usize = 0;
    var it = a.sem.iterScopes();
    while (it.next()) |entry| {
        if (entry.scope.kind == kind) count += 1;
    }
    return count;
}

test "a script has a single non-strict global root scope" {
    var a = try analyze("var x;", .{ .source_type = .script });
    defer a.deinit();

    try testing.expectEqual(@as(usize, 1), a.sem.scopes.list.len);
    const root = a.sem.scope(.root);
    try testing.expectEqual(Scope.Kind.global, root.kind);
    try testing.expectEqual(ScopeId.none, root.parent);
    try testing.expectEqual(ScopeId.root, root.hoist_target);
    try testing.expect(!root.flags.strict);

    const x = try a.symbolNamed("x");
    try testing.expectEqual(ScopeId.root, x.symbol.scope);
}

test "a module wraps the global scope in a strict module scope" {
    var a = try analyze("var x;", .{ .source_type = .module });
    defer a.deinit();

    try testing.expectEqual(@as(usize, 2), a.sem.scopes.list.len);
    const module = a.sem.scope(.module);
    try testing.expectEqual(Scope.Kind.module, module.kind);
    try testing.expectEqual(ScopeId.root, module.parent);
    try testing.expectEqual(ScopeId.module, module.hoist_target);
    try testing.expect(module.flags.strict);
    try testing.expect(!a.sem.scope(.root).flags.strict);

    const x = try a.symbolNamed("x");
    try testing.expectEqual(ScopeId.module, x.symbol.scope);
}

test "a use strict directive makes a script's global scope strict" {
    var a = try analyze("\"use strict\"; var x;", .{ .source_type = .script });
    defer a.deinit();
    try testing.expect(a.sem.scope(.root).flags.strict);
}

test "function declarations create a function scope that hoists vars" {
    var a = try analyze("function f(p) { { var v; } }", .{ .source_type = .script });
    defer a.deinit();

    const v_scope = try declScope(&a, "v");
    try testing.expectEqual(Scope.Kind.function, v_scope.kind);
    const p_scope_id = (try a.symbolNamed("p")).symbol.scope;
    const v_scope_id = (try a.symbolNamed("v")).symbol.scope;
    try testing.expectEqual(p_scope_id, v_scope_id);

    try testing.expectEqual(v_scope_id, v_scope.hoist_target);
    try testing.expect(a.tree.data(v_scope.node) == .function);

    const block_node = try a.nthNode(.block_statement, 0);
    const block_scope_id = a.sem.scopeOf(block_node);
    try testing.expectEqual(Scope.Kind.block, a.sem.scope(block_scope_id).kind);
    try testing.expectEqual(v_scope_id, a.sem.scope(block_scope_id).hoist_target);

    try testing.expectEqual(ScopeId.root, (try a.symbolNamed("f")).symbol.scope);
    try testing.expectEqual(@as(usize, 0), countKind(&a, .expression_name));
}

test "sloppy function scopes stay non-strict, and use strict applies retroactively" {
    var a = try analyze(
        "function sloppy(p) {} function strict(q) { \"use strict\"; }",
        .{ .source_type = .script },
    );
    defer a.deinit();

    try testing.expect(!(try declScope(&a, "p")).flags.strict);
    try testing.expect((try declScope(&a, "q")).flags.strict);
}

test "functions inherit strictness from the enclosing scope" {
    var a = try analyze(
        "\"use strict\"; function f(p) {}",
        .{ .source_type = .script },
    );
    defer a.deinit();
    try testing.expect((try declScope(&a, "p")).flags.strict);
}

test "arrow functions create function scopes" {
    var a = try analyze("const g = (p) => p;", .{});
    defer a.deinit();

    const p_scope = try declScope(&a, "p");
    try testing.expectEqual(Scope.Kind.function, p_scope.kind);
    try testing.expect(a.tree.data(p_scope.node) == .arrow_function_expression);
    try testing.expectEqual(@as(usize, 0), countKind(&a, .expression_name));
}

test "named function expressions get an expression-name scope for their name" {
    var a = try analyze("const F = function G() { return G; };", .{});
    defer a.deinit();

    const g_scope = try declScope(&a, "G");
    try testing.expectEqual(Scope.Kind.expression_name, g_scope.kind);

    const g_scope_id = (try a.symbolNamed("G")).symbol.scope;
    const func_node = try a.nthNode(.function, 0);
    const func_scope = a.sem.scope(a.sem.scopeOf(func_node));
    try testing.expectEqual(Scope.Kind.function, func_scope.kind);
    try testing.expectEqual(g_scope_id, func_scope.parent);
    try testing.expectEqual(ScopeId.module, g_scope.parent);

    const ref = try a.onlyReferenceNamed("G");
    try testing.expectEqual((try a.symbolNamed("G")).id, ref.reference.symbol);
}

test "anonymous function expressions get no expression-name scope" {
    var a = try analyze("const F = function () {};", .{});
    defer a.deinit();
    try testing.expectEqual(@as(usize, 0), countKind(&a, .expression_name));
}

test "block statements create block scopes" {
    var a = try analyze("{ let x; }", .{});
    defer a.deinit();

    const x_scope = try declScope(&a, "x");
    try testing.expectEqual(Scope.Kind.block, x_scope.kind);
    try testing.expect(a.tree.data(x_scope.node) == .block_statement);
    try testing.expectEqual(ScopeId.module, x_scope.parent);
    try testing.expectEqual(ScopeId.module, x_scope.hoist_target);
}

test "for statements scope their declarations, the body block nests inside" {
    var a = try analyze("for (let i = 0;;) { let j; }", .{});
    defer a.deinit();

    const i_scope = try declScope(&a, "i");
    try testing.expectEqual(Scope.Kind.block, i_scope.kind);
    try testing.expect(a.tree.data(i_scope.node) == .for_statement);

    const j_scope = try declScope(&a, "j");
    try testing.expect(a.tree.data(j_scope.node) == .block_statement);
    const i_scope_id = (try a.symbolNamed("i")).symbol.scope;
    try testing.expectEqual(i_scope_id, j_scope.parent);
}

test "for-in and for-of statements create their own scopes" {
    var a = try analyze(
        "for (const k in o) {} for (const v of xs) {}",
        .{},
    );
    defer a.deinit();

    const k_scope = try declScope(&a, "k");
    try testing.expect(a.tree.data(k_scope.node) == .for_in_statement);
    const v_scope = try declScope(&a, "v");
    try testing.expect(a.tree.data(v_scope.node) == .for_of_statement);
}

test "catch parameter and catch body share one scope" {
    var a = try analyze("try { let t; } catch (e) { let x; }", .{});
    defer a.deinit();

    const e_scope_id = (try a.symbolNamed("e")).symbol.scope;
    const x_scope_id = (try a.symbolNamed("x")).symbol.scope;
    try testing.expectEqual(e_scope_id, x_scope_id);

    const e_scope = a.sem.scope(e_scope_id);
    try testing.expectEqual(Scope.Kind.block, e_scope.kind);
    try testing.expect(a.tree.data(e_scope.node) == .catch_clause);

    try testing.expectEqual(@as(usize, 2), countKind(&a, .block));
}

test "switch creates one block scope shared by all cases" {
    var a = try analyze(
        "switch (c) { case 1: let x1; break; default: let x2; }",
        .{},
    );
    defer a.deinit();

    const s1 = (try a.symbolNamed("x1")).symbol.scope;
    const s2 = (try a.symbolNamed("x2")).symbol.scope;
    try testing.expectEqual(s1, s2);
    try testing.expect(a.tree.data(a.sem.scope(s1).node) == .switch_statement);
    try testing.expectEqual(@as(usize, 1), countKind(&a, .block));
}

test "class declarations create an always-strict class scope" {
    var a = try analyze("class C { m(p) {} }", .{ .source_type = .script });
    defer a.deinit();

    const class_node = try a.nthNode(.class, 0);
    const class_scope = a.sem.scope(a.sem.scopeOf(class_node));
    try testing.expectEqual(Scope.Kind.class, class_scope.kind);
    try testing.expect(class_scope.flags.strict);
    try testing.expect((try declScope(&a, "p")).flags.strict);

    try testing.expectEqual(ScopeId.root, (try a.symbolNamed("C")).symbol.scope);
    try testing.expectEqual(@as(usize, 0), countKind(&a, .expression_name));
}

test "named class expressions bind their name in an expression-name scope" {
    var a = try analyze("const D = class E { m() { return E; } };", .{});
    defer a.deinit();

    const e_scope = try declScope(&a, "E");
    try testing.expectEqual(Scope.Kind.expression_name, e_scope.kind);
    try testing.expect(e_scope.flags.strict);

    const class_node = try a.nthNode(.class, 0);
    const class_scope = a.sem.scope(a.sem.scopeOf(class_node));
    try testing.expectEqual((try a.symbolNamed("E")).symbol.scope, class_scope.parent);

    const ref = try a.onlyReferenceNamed("E");
    try testing.expectEqual((try a.symbolNamed("E")).id, ref.reference.symbol);
}

test "static blocks are hoist targets for their vars" {
    var a = try analyze("class C { static { var v; } }", .{});
    defer a.deinit();

    const v_scope = try declScope(&a, "v");
    try testing.expectEqual(Scope.Kind.static_block, v_scope.kind);
    try testing.expect(a.tree.data(v_scope.node) == .static_block);
    const v_scope_id = (try a.symbolNamed("v")).symbol.scope;
    try testing.expectEqual(v_scope_id, v_scope.hoist_target);
}

test "namespace bodies are hoist targets so vars stay inside" {
    var a = try analyze("namespace N { var v; }", .{ .lang = .ts });
    defer a.deinit();

    const v_scope = try declScope(&a, "v");
    try testing.expectEqual(Scope.Kind.ts_module, v_scope.kind);
    try testing.expect(a.tree.data(v_scope.node) == .ts_module_block);
    const v_scope_id = (try a.symbolNamed("v")).symbol.scope;
    try testing.expectEqual(v_scope_id, v_scope.hoist_target);

    try testing.expectEqual(ScopeId.module, (try a.symbolNamed("N")).symbol.scope);
}

test "interfaces and type aliases scope their type parameters" {
    var a = try analyze(
        "interface I<Ti> { m(): Ti; } type Al<Ta> = Ta;",
        .{ .lang = .ts },
    );
    defer a.deinit();

    const ti_scope = try declScope(&a, "Ti");
    try testing.expectEqual(Scope.Kind.block, ti_scope.kind);
    try testing.expect(a.tree.data(ti_scope.node) == .ts_interface_declaration);

    const ta_scope = try declScope(&a, "Ta");
    try testing.expectEqual(Scope.Kind.block, ta_scope.kind);
    try testing.expect(a.tree.data(ta_scope.node) == .ts_type_alias_declaration);

    try testing.expectEqual(ScopeId.module, (try a.symbolNamed("I")).symbol.scope);
    try testing.expectEqual(ScopeId.module, (try a.symbolNamed("Al")).symbol.scope);
}

test "conditional types scope their infer bindings" {
    var a = try analyze(
        "type X<A> = A extends infer B ? B : never;",
        .{ .lang = .ts },
    );
    defer a.deinit();

    const b_scope = try declScope(&a, "B");
    try testing.expectEqual(Scope.Kind.block, b_scope.kind);
    try testing.expect(a.tree.data(b_scope.node) == .ts_conditional_type);
}

test "mapped types scope their key binding" {
    var a = try analyze(
        "type M<T> = { [K in keyof T]: T[K] };",
        .{ .lang = .ts },
    );
    defer a.deinit();

    const k_scope = try declScope(&a, "K");
    try testing.expectEqual(Scope.Kind.block, k_scope.kind);
    try testing.expect(a.tree.data(k_scope.node) == .ts_mapped_type);
}

test "decorators evaluate in the scope enclosing the class" {
    var a = try analyze(
        \\let dek = (x: unknown) => x;
        \\let T = 1;
        \\@dek(T)
        \\class C<T> {
        \\  @dek(T) m() {}
        \\}
    , .{ .lang = .ts });
    defer a.deinit();

    var buf: [4]semantic.Semantic.SymbolEntry = undefined;
    const t_symbols = a.symbolsNamed("T", &buf);
    try testing.expectEqual(@as(usize, 2), t_symbols.len);

    const outer_t = blk: {
        for (t_symbols) |entry| {
            if (entry.symbol.flags.block_scoped_var) break :blk entry;
        }
        return error.OuterTNotFound;
    };
    const param_t = blk: {
        for (t_symbols) |entry| {
            if (entry.symbol.flags.type_parameter) break :blk entry;
        }
        return error.TypeParamTNotFound;
    };

    var t_refs: usize = 0;
    var it = a.sem.iterReferences();
    while (it.next()) |entry| {
        if (!std.mem.eql(u8, a.tree.string(entry.reference.name), "T")) continue;
        t_refs += 1;
        try testing.expectEqual(outer_t.id, entry.reference.symbol);
    }
    try testing.expectEqual(@as(usize, 2), t_refs);
    try testing.expectEqual(@as(usize, 0), a.sem.uses(param_t.id).len);

    const dek = try a.symbolNamed("dek");
    try testing.expectEqual(@as(usize, 2), a.sem.uses(dek.id).len);
}

test "scope ancestors walk from a scope to the root" {
    var a = try analyze("function f() { { let x; } }", .{});
    defer a.deinit();

    const x_scope_id = (try a.symbolNamed("x")).symbol.scope;
    var it = a.sem.scopes.ancestors(x_scope_id);

    try testing.expectEqual(x_scope_id, it.next().?);
    try testing.expectEqual(Scope.Kind.function, a.sem.scope(it.next().?).kind);
    try testing.expectEqual(ScopeId.module, it.next().?);
    try testing.expectEqual(ScopeId.root, it.next().?);
    try testing.expectEqual(@as(?ScopeId, null), it.next());

    var none_it = a.sem.scopes.ancestors(.none);
    try testing.expectEqual(@as(?ScopeId, null), none_it.next());
}

test "scopeOf maps scope-creating nodes to the scope they create" {
    var a = try analyze("function f() {}", .{});
    defer a.deinit();

    const func_node = try a.nthNode(.function, 0);
    const func_scope = a.sem.scope(a.sem.scopeOf(func_node));
    try testing.expectEqual(Scope.Kind.function, func_scope.kind);
    try testing.expectEqual(func_node, func_scope.node);
    try testing.expectEqual(ScopeId.module, a.sem.scopeOf(a.tree.root));
}

test "Scope.Kind.isHoistTarget matches the spec hoist boundaries" {
    try testing.expect(Scope.Kind.isHoistTarget(.global));
    try testing.expect(Scope.Kind.isHoistTarget(.module));
    try testing.expect(Scope.Kind.isHoistTarget(.function));
    try testing.expect(Scope.Kind.isHoistTarget(.static_block));
    try testing.expect(Scope.Kind.isHoistTarget(.ts_module));
    try testing.expect(!Scope.Kind.isHoistTarget(.block));
    try testing.expect(!Scope.Kind.isHoistTarget(.class));
    try testing.expect(!Scope.Kind.isHoistTarget(.expression_name));
}
