//! Binder and semantic model. Symbol flags per declaration kind,
//! declaration merging, hoisting tables, reference resolution and
//! classification, synthesized references, and the Semantic queries.

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

fn flagsOf(a: *const helpers.Analyzed, name: []const u8) !traverser.semantic.Symbol.Flags {
    return (try a.symbolNamed(name)).symbol.flags;
}

fn referencesNamed(
    a: *const helpers.Analyzed,
    name: []const u8,
    buf: []Semantic.ReferenceEntry,
) []Semantic.ReferenceEntry {
    var len: usize = 0;
    var it = a.sem.iterReferences();
    while (it.next()) |entry| {
        if (std.mem.eql(u8, a.tree.string(entry.reference.name), name)) {
            buf[len] = entry;
            len += 1;
        }
    }
    return buf[0..len];
}

test "var, let, and const set the variable flags" {
    var a = try analyze("var v; let l; const c = 1;", .{});
    defer a.deinit();

    const v = try flagsOf(&a, "v");
    try testing.expect(v.function_scoped_var);
    try testing.expect(!v.block_scoped_var);
    try testing.expect(!v.const_var);

    const l = try flagsOf(&a, "l");
    try testing.expect(l.block_scoped_var);
    try testing.expect(!l.function_scoped_var);
    try testing.expect(!l.const_var);

    const c = try flagsOf(&a, "c");
    try testing.expect(c.block_scoped_var);
    try testing.expect(c.const_var);

    try testing.expect(v.inValueSpace());
    try testing.expect(!v.inTypeSpace());
}

test "parameters and catch variables are function-scoped but never hoist" {
    var a = try analyze("function f(p) { try {} catch (e) {} }", .{});
    defer a.deinit();

    const p = try flagsOf(&a, "p");
    try testing.expect(p.function_scoped_var);
    try testing.expect(p.parameter);
    try testing.expect(!p.isHoistingVar());

    const e = try flagsOf(&a, "e");
    try testing.expect(e.function_scoped_var);
    try testing.expect(e.catch_var);
    try testing.expect(!e.isHoistingVar());

    var a2 = try analyze("var h;", .{});
    defer a2.deinit();
    try testing.expect((try flagsOf(&a2, "h")).isHoistingVar());
}

test "functions and classes set their flags" {
    var a = try analyze("function fn() {} class Cl {}", .{});
    defer a.deinit();

    const fn_flags = try flagsOf(&a, "fn");
    try testing.expect(fn_flags.function);
    try testing.expect(fn_flags.inValueSpace());
    try testing.expect(!fn_flags.inTypeSpace());

    const cl = try flagsOf(&a, "Cl");
    try testing.expect(cl.class);
    try testing.expect(cl.inValueSpace());
    try testing.expect(cl.inTypeSpace());
}

test "import bindings distinguish value and type imports" {
    var a = try analyze(
        \\import d from "m";
        \\import type t from "m";
        \\import { type u, w } from "m";
        \\import * as ns from "m";
    , .{ .lang = .ts });
    defer a.deinit();

    try testing.expect((try flagsOf(&a, "d")).import);
    try testing.expect(!(try flagsOf(&a, "d")).type_import);
    try testing.expect((try flagsOf(&a, "t")).type_import);
    try testing.expect((try flagsOf(&a, "u")).type_import);
    try testing.expect((try flagsOf(&a, "w")).import);
    try testing.expect((try flagsOf(&a, "ns")).import);
}

test "TS type declarations set their flags" {
    var a = try analyze(
        \\interface I {}
        \\type Al = number;
        \\function g<Tp>(x: Tp): Tp { return x; }
        \\enum Er { A }
        \\const enum Ec { B }
    , .{ .lang = .ts });
    defer a.deinit();

    const i = try flagsOf(&a, "I");
    try testing.expect(i.interface);
    try testing.expect(i.inTypeSpace());
    try testing.expect(!i.inValueSpace());

    try testing.expect((try flagsOf(&a, "Al")).type_alias);
    try testing.expect((try flagsOf(&a, "Tp")).type_parameter);

    const er = try flagsOf(&a, "Er");
    try testing.expect(er.regular_enum);
    try testing.expect(er.inValueSpace());
    try testing.expect(er.inTypeSpace());

    const ec = try flagsOf(&a, "Ec");
    try testing.expect(ec.const_enum);
    try testing.expect(!ec.regular_enum);
}

test "namespaces occupy value space only when instantiated" {
    var a = try analyze(
        \\namespace Nv { export const x = 1; }
        \\namespace Nt { export type T = number; }
        \\namespace No { namespace Inner { export const y = 1; } }
    , .{ .lang = .ts });
    defer a.deinit();

    const nv = try flagsOf(&a, "Nv");
    try testing.expect(nv.value_module);
    try testing.expect(nv.namespace_module);

    const nt = try flagsOf(&a, "Nt");
    try testing.expect(!nt.value_module);
    try testing.expect(nt.namespace_module);

    const no = try flagsOf(&a, "No");
    try testing.expect(no.value_module);
}

test "declare marks declarations and whole namespace bodies ambient" {
    var a = try analyze(
        \\declare const dc: number;
        \\declare namespace DN { const inner: number; }
        \\const live = 1;
    , .{ .lang = .ts });
    defer a.deinit();

    try testing.expect((try flagsOf(&a, "dc")).ambient);
    try testing.expect((try flagsOf(&a, "DN")).ambient);
    try testing.expect((try flagsOf(&a, "inner")).ambient);
    try testing.expect(!(try flagsOf(&a, "live")).ambient);
}

test "declaration files are ambient in their entirety" {
    var a = try analyze("let x: number;", .{ .lang = .dts });
    defer a.deinit();
    try testing.expect((try flagsOf(&a, "x")).ambient);
}

test "export declarations mark their bindings exported" {
    var a = try analyze(
        \\export const ec = 1;
        \\export function ef() {}
        \\export default class Dc {}
        \\const local = 2;
    , .{});
    defer a.deinit();

    try testing.expect((try flagsOf(&a, "ec")).exported);
    try testing.expect((try flagsOf(&a, "ef")).exported);
    try testing.expect(!(try flagsOf(&a, "ec")).is_default);

    const dc = try flagsOf(&a, "Dc");
    try testing.expect(dc.exported);
    try testing.expect(dc.is_default);

    try testing.expect(!(try flagsOf(&a, "local")).exported);
}

test "expression names and parameters never inherit the exported flag" {
    var a = try analyze(
        \\export const fe = function inner() {};
        \\export function outer(param) {}
    , .{});
    defer a.deinit();

    try testing.expect((try flagsOf(&a, "fe")).exported);
    try testing.expect(!(try flagsOf(&a, "inner")).exported);
    try testing.expect(!(try flagsOf(&a, "param")).exported);
}

test "var redeclarations merge into one symbol with every declarator" {
    var a = try analyze("var m; var m;", .{});
    defer a.deinit();

    const m = try a.symbolNamed("m");
    const decls = a.sem.decls(m.id);
    try testing.expectEqual(@as(usize, 2), decls.len);

    const first = try a.nthBindingNamed("m", 0);
    const second = try a.nthBindingNamed("m", 1);
    try testing.expectEqual(m.id, a.sem.symbolOf(first).?);
    try testing.expectEqual(m.id, a.sem.symbolOf(second).?);
    try testing.expectEqual(first, decls[0]);
    try testing.expectEqual(second, decls[1]);
}

test "TS overloads merge, and an implementation clears ambient" {
    var a = try analyze(
        \\function ov(): void;
        \\function ov(x: number): void;
        \\function ov(x?: number) {}
    , .{ .lang = .ts });
    defer a.deinit();

    const ov = try a.symbolNamed("ov");
    try testing.expectEqual(@as(usize, 3), a.sem.decls(ov.id).len);
    try testing.expect(ov.symbol.flags.function);
    try testing.expect(!ov.symbol.flags.ambient);

    var a2 = try analyze("declare function amb(): void;", .{ .lang = .ts });
    defer a2.deinit();
    try testing.expect((try flagsOf(&a2, "amb")).ambient);
}

test "class and interface declarations merge" {
    var a = try analyze("class CI {} interface CI {}", .{ .lang = .ts });
    defer a.deinit();

    const ci = try a.symbolNamed("CI");
    try testing.expect(ci.symbol.flags.class);
    try testing.expect(ci.symbol.flags.interface);
    try testing.expectEqual(@as(usize, 2), a.sem.decls(ci.id).len);
}

test "namespace and function declarations merge" {
    var a = try analyze(
        "function nf() {} namespace nf { export const a = 1; }",
        .{ .lang = .ts },
    );
    defer a.deinit();

    const nf = try a.symbolNamed("nf");
    try testing.expect(nf.symbol.flags.function);
    try testing.expect(nf.symbol.flags.namespace_module);
    try testing.expectEqual(@as(usize, 2), a.sem.decls(nf.id).len);
}

test "enum declarations merge with each other" {
    var a = try analyze("enum Em { A } enum Em { B }", .{ .lang = .ts });
    defer a.deinit();
    try testing.expectEqual(@as(usize, 2), a.sem.decls((try a.symbolNamed("Em")).id).len);
}

test "conflicting redeclarations keep one symbol and report an error" {
    var a = try analyzeAllowErrors("let dup; let dup;", .{});
    defer a.deinit();

    try testing.expect(a.tree.hasErrors());
    const dup = try a.symbolNamed("dup");
    try testing.expectEqual(@as(usize, 2), a.sem.decls(dup.id).len);
}

test "sloppy scripts merge var with function declarations" {
    var a = try analyze("var dual; function dual() {}", .{ .source_type = .script });
    defer a.deinit();

    const dual = try a.symbolNamed("dual");
    try testing.expect(dual.symbol.flags.function);
    try testing.expect(dual.symbol.flags.function_scoped_var);
}

test "duplicate module-level function declarations conflict in plain JS" {
    var a = try analyzeAllowErrors("function fd() {} function fd() {}", .{});
    defer a.deinit();
    try testing.expect(a.tree.hasErrors());
    _ = try a.symbolNamed("fd");
}

test "hoisting vars are visible in pass-through blocks but owned by the target" {
    var a = try analyze("function h() { { var hv; } }", .{});
    defer a.deinit();

    const hv = try a.symbolNamed("hv");
    const func_scope = hv.symbol.scope;
    const block_node = try a.nthNode(.block_statement, 0);
    const block_scope = a.sem.scopeOf(block_node);
    try testing.expect(block_scope != func_scope);

    try testing.expectEqual(hv.id, a.sem.binding(func_scope, "hv").?);
    try testing.expectEqual(hv.id, a.sem.binding(block_scope, "hv").?);

    var block_it = a.sem.bindings(block_scope);
    try testing.expectEqual(@as(?SymbolId, null), block_it.next());

    var found = false;
    var func_it = a.sem.bindings(func_scope);
    while (func_it.next()) |id| {
        if (id == hv.id) found = true;
    }
    try testing.expect(found);
}

test "sloppy block-level functions bind in the block" {
    var a = try analyze("{ function bf() {} }", .{ .source_type = .script });
    defer a.deinit();

    const bf = try a.symbolNamed("bf");
    try testing.expect(bf.symbol.flags.function);
    const scope = a.sem.scope(bf.symbol.scope);
    try testing.expectEqual(traverser.semantic.Scope.Kind.block, scope.kind);
    try testing.expectEqual(@as(?SymbolId, null), a.sem.binding(.root, "bf"));
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

test "references resolve to the nearest binding on the scope chain" {
    var a = try analyze("let sh = 1; { let sh = 2; sh; } sh;", .{});
    defer a.deinit();

    var sym_buf: [4]Semantic.SymbolEntry = undefined;
    const shs = a.symbolsNamed("sh", &sym_buf);
    try testing.expectEqual(@as(usize, 2), shs.len);
    const outer = shs[0];
    const inner = shs[1];

    var ref_buf: [4]Semantic.ReferenceEntry = undefined;
    const refs = referencesNamed(&a, "sh", &ref_buf);
    try testing.expectEqual(@as(usize, 2), refs.len);
    try testing.expectEqual(inner.id, refs[0].reference.symbol);
    try testing.expectEqual(outer.id, refs[1].reference.symbol);

    try testing.expectEqual(@as(usize, 1), a.sem.uses(outer.id).len);
    try testing.expectEqual(@as(usize, 1), a.sem.uses(inner.id).len);
}

test "unresolved names keep symbol none" {
    var a = try analyze("undeclared;", .{});
    defer a.deinit();

    const ref = try a.onlyReferenceNamed("undeclared");
    try testing.expectEqual(SymbolId.none, ref.reference.symbol);

    const node = try a.referenceNamed("undeclared");
    try testing.expectEqual(@as(?SymbolId, null), a.sem.symbolOf(node));
    try testing.expectEqual(ref.id, a.sem.referenceOf(node).?);
}

test "references resolve regardless of source order" {
    var a = try analyze("hoisted(); function hoisted() {}", .{});
    defer a.deinit();

    const ref = try a.onlyReferenceNamed("hoisted");
    try testing.expectEqual((try a.symbolNamed("hoisted")).id, ref.reference.symbol);
}

test "write references cover every assignment-target shape" {
    var a = try analyze(
        \\let w = 1;
        \\w = 2;
        \\w++;
        \\[w] = [1];
        \\({ x: w } = { x: 1 });
        \\[w = 5] = [];
        \\(w) = 3;
        \\for (w in o) {}
        \\for (w of xs) {}
        \\f(w);
        \\w.p;
    , .{});
    defer a.deinit();

    var buf: [16]Semantic.ReferenceEntry = undefined;
    const refs = referencesNamed(&a, "w", &buf);
    const expected_writes = [_]bool{
        true,  true, true, true, true, true, true, true,
        false, false,
    };
    try testing.expectEqual(expected_writes.len, refs.len);
    for (expected_writes, refs, 0..) |want, ref, i| {
        if (ref.reference.flags.write != want) {
            std.debug.print("reference #{d} write flag mismatch\n", .{i});
            return error.WrongWriteFlag;
        }
        try testing.expectEqual((try a.symbolNamed("w")).id, ref.reference.symbol);
    }
}

test "TS cast and non-null wrappers stay write targets" {
    var a = try analyze(
        \\let t = 1;
        \\(t as number) = 2;
        \\t!++;
        \\(t satisfies number) = 3;
        \\t;
    , .{ .lang = .ts });
    defer a.deinit();

    var buf: [8]Semantic.ReferenceEntry = undefined;
    const refs = referencesNamed(&a, "t", &buf);
    const expected_writes = [_]bool{ true, true, true, false };
    try testing.expectEqual(expected_writes.len, refs.len);
    for (expected_writes, refs) |want, ref| {
        try testing.expectEqual(want, ref.reference.flags.write);
    }
}

test "type-position references are flagged, value references are not" {
    var a = try analyze(
        \\class Ck {}
        \\let ann: Ck = new Ck();
    , .{ .lang = .ts });
    defer a.deinit();

    var buf: [4]Semantic.ReferenceEntry = undefined;
    const refs = referencesNamed(&a, "Ck", &buf);
    try testing.expectEqual(@as(usize, 2), refs.len);
    try testing.expectEqual(.type, refs[0].reference.flags.space);
    try testing.expect(refs[0].reference.flags.space.inTypePosition());
    try testing.expectEqual(.value, refs[1].reference.flags.space);

    const ck = try a.symbolNamed("Ck");
    try testing.expectEqual(ck.id, refs[0].reference.symbol);
    try testing.expectEqual(ck.id, refs[1].reference.symbol);
}

test "initializers are reads, and declarations are not uses" {
    var a = try analyze("let src = 1; let dst = src; dst; dst = 2;", .{});
    defer a.deinit();

    const src = try a.symbolNamed("src");
    const src_uses = a.sem.uses(src.id);
    try testing.expectEqual(@as(usize, 1), src_uses.len);
    try testing.expect(!a.sem.reference(src_uses[0]).flags.write);

    const dst = try a.symbolNamed("dst");
    try testing.expectEqual(@as(usize, 1), a.sem.decls(dst.id).len);
    const dst_uses = a.sem.uses(dst.id);
    try testing.expectEqual(@as(usize, 2), dst_uses.len);
    try testing.expect(!a.sem.reference(dst_uses[0]).flags.write);
    try testing.expect(a.sem.reference(dst_uses[1]).flags.write);
}

test "JSX component tags reference their binding, intrinsic tags do not" {
    var a = try analyze(
        \\import Foo from "m";
        \\import * as NS from "m";
        \\const el = <Foo a={1}><div/><NS.Part/><svg:path/></Foo>;
    , .{ .lang = .jsx });
    defer a.deinit();

    try testing.expectEqual(@as(usize, 2), a.sem.uses((try a.symbolNamed("Foo")).id).len);
    try testing.expectEqual(@as(usize, 1), a.sem.uses((try a.symbolNamed("NS")).id).len);

    var it = a.sem.iterReferences();
    while (it.next()) |entry| {
        const name = a.tree.string(entry.reference.name);
        try testing.expect(!std.mem.eql(u8, name, "div"));
        try testing.expect(!std.mem.eql(u8, name, "svg"));
        try testing.expect(!std.mem.eql(u8, name, "path"));
        try testing.expect(!std.mem.eql(u8, name, "Part"));
    }
}

test "type predicates synthesize a reference to the parameter" {
    var a = try analyze(
        \\function isStr(v: unknown): v is string { return !!v; }
    , .{ .lang = .ts });
    defer a.deinit();

    const v = try a.symbolNamed("v");
    const uses = a.sem.uses(v.id);
    try testing.expectEqual(@as(usize, 2), uses.len);

    const pred_ref = a.sem.reference(uses[0]);
    const body_ref = a.sem.reference(uses[1]);
    try testing.expectEqual(.typeof, pred_ref.flags.space);
    try testing.expectEqual(.value, body_ref.flags.space);
}

test "a predicate in a type-only signature names no binding and records no reference" {
    var a = try analyze(
        \\const x = 1;
        \\type P = (x: unknown) => x is string;
        \\interface I { m(v: unknown): v is string }
    , .{ .lang = .ts });
    defer a.deinit();

    // the outer const must not capture the predicate names
    try testing.expectEqual(@as(usize, 0), a.sem.uses((try a.symbolNamed("x")).id).len);

    var buf: [4]Semantic.ReferenceEntry = undefined;
    try testing.expectEqual(@as(usize, 0), referencesNamed(&a, "x", &buf).len);
    try testing.expectEqual(@as(usize, 0), referencesNamed(&a, "v", &buf).len);
}

test "a conflicting redeclaration is aliased onto the existing symbol" {
    var a = try analyzeAllowErrors("let y; let y;", .{});
    defer a.deinit();

    try testing.expect(a.tree.hasErrors());

    const y = try a.symbolNamed("y");
    try testing.expectEqual(@as(usize, 2), a.sem.decls(y.id).len);

    const second = try a.nthBindingNamed("y", 1);
    try testing.expectEqual(y.id, a.sem.symbolOf(second).?);
}

test "a value binding does not shadow an outer type for type-position references" {
    var a = try analyze(
        \\type T = string;
        \\function f() {
        \\  const T = 1;
        \\  let x: T;
        \\  T;
        \\}
    , .{ .lang = .ts });
    defer a.deinit();

    var syms: [4]Semantic.SymbolEntry = undefined;
    const ts = a.symbolsNamed("T", &syms);
    try testing.expectEqual(@as(usize, 2), ts.len);
    const alias = ts[0]; // module scope `type T`
    const local = ts[1]; // function scope `const T`
    try testing.expect(alias.symbol.flags.type_alias);
    try testing.expect(local.symbol.flags.const_var);

    var buf: [4]Semantic.ReferenceEntry = undefined;
    const refs = referencesNamed(&a, "T", &buf);
    try testing.expectEqual(@as(usize, 2), refs.len);

    const type_ref = refs[0].reference; // `x: T`
    try testing.expectEqual(.type, type_ref.flags.space);
    try testing.expectEqual(alias.id, type_ref.symbol);

    const value_ref = refs[1].reference; // the bare statement use
    try testing.expectEqual(.value, value_ref.flags.space);
    try testing.expectEqual(local.id, value_ref.symbol);

    // the space-aware lookup mirrors resolution from the same scope
    const scope = a.sem.reference(refs[0].id).scope;
    try testing.expectEqual(alias.id, a.sem.lookup(scope, "T", .type).?);
    try testing.expectEqual(local.id, a.sem.lookup(scope, "T", .value).?);
}

test "a type binding does not shadow an outer value for value references" {
    var a = try analyze(
        \\const T = 1;
        \\function f() {
        \\  interface T { a: number }
        \\  T;
        \\  let x: T;
        \\}
    , .{ .lang = .ts });
    defer a.deinit();

    var syms: [4]Semantic.SymbolEntry = undefined;
    const ts = a.symbolsNamed("T", &syms);
    try testing.expectEqual(@as(usize, 2), ts.len);
    const outer_const = ts[0];
    const inner_iface = ts[1];
    try testing.expect(outer_const.symbol.flags.const_var);
    try testing.expect(inner_iface.symbol.flags.interface);

    var buf: [4]Semantic.ReferenceEntry = undefined;
    const refs = referencesNamed(&a, "T", &buf);
    try testing.expectEqual(@as(usize, 2), refs.len);
    try testing.expectEqual(outer_const.id, refs[0].reference.symbol); // the bare statement use
    try testing.expectEqual(inner_iface.id, refs[1].reference.symbol); // `x: T`
}

test "typeof resolves its entity in value space" {
    var a = try analyze(
        \\const v = 1;
        \\function f() {
        \\  type v = string;
        \\  let a: typeof v;
        \\  let b: v;
        \\}
    , .{ .lang = .ts });
    defer a.deinit();

    var syms: [4]Semantic.SymbolEntry = undefined;
    const vs = a.symbolsNamed("v", &syms);
    try testing.expectEqual(@as(usize, 2), vs.len);
    const outer_const = vs[0];
    const inner_alias = vs[1];

    var buf: [4]Semantic.ReferenceEntry = undefined;
    const refs = referencesNamed(&a, "v", &buf);
    try testing.expectEqual(@as(usize, 2), refs.len);

    const query_ref = refs[0].reference; // `typeof v`
    try testing.expectEqual(.typeof, query_ref.flags.space);
    try testing.expectEqual(outer_const.id, query_ref.symbol);

    const ann_ref = refs[1].reference; // `: v`
    try testing.expectEqual(.type, ann_ref.flags.space);
    try testing.expectEqual(inner_alias.id, ann_ref.symbol);
}

test "qualified type names start from a namespace or enum" {
    var a = try analyze(
        \\namespace N { export type T = number; }
        \\enum E { A }
        \\function f() {
        \\  const N = 1;
        \\  const E = 2;
        \\  let x: N.T;
        \\  let y: E.A;
        \\  N;
        \\}
    , .{ .lang = .ts });
    defer a.deinit();

    var syms: [4]Semantic.SymbolEntry = undefined;
    const ns = a.symbolsNamed("N", &syms);
    try testing.expectEqual(@as(usize, 2), ns.len);
    try testing.expect(ns[0].symbol.flags.namespace_module);

    var ebuf: [4]Semantic.SymbolEntry = undefined;
    const es = a.symbolsNamed("E", &ebuf);
    try testing.expectEqual(@as(usize, 2), es.len);
    try testing.expect(es[0].symbol.flags.regular_enum);

    var buf: [4]Semantic.ReferenceEntry = undefined;
    const n_refs = referencesNamed(&a, "N", &buf);
    try testing.expectEqual(@as(usize, 2), n_refs.len);

    const qual_ref = n_refs[0].reference; // `N.T`
    try testing.expectEqual(.namespace, qual_ref.flags.space);
    try testing.expectEqual(ns[0].id, qual_ref.symbol);
    try testing.expectEqual(ns[1].id, n_refs[1].reference.symbol); // the bare statement use

    const e_refs = referencesNamed(&a, "E", &buf);
    try testing.expectEqual(@as(usize, 1), e_refs.len);
    try testing.expectEqual(.namespace, e_refs[0].reference.flags.space);
    try testing.expectEqual(es[0].id, e_refs[0].reference.symbol); // `E.A`
}

test "export specifiers see every declaration space" {
    var a = try analyze(
        \\type T = 1;
        \\interface I { a: number }
        \\const v = 2;
        \\export { T, I, v };
    , .{ .lang = .ts });
    defer a.deinit();

    inline for (.{ "T", "I", "v" }) |name| {
        const entry = try a.onlyReferenceNamed(name);
        try testing.expectEqual(.any, entry.reference.flags.space);
        try testing.expectEqual((try a.symbolNamed(name)).id, entry.reference.symbol);
    }
}

test "export default, export =, and import = see type-only bindings" {
    {
        var a = try analyze(
            \\interface I { a: number }
            \\export default I;
        , .{ .lang = .ts });
        defer a.deinit();
        const ref = try a.onlyReferenceNamed("I");
        try testing.expectEqual(.any, ref.reference.flags.space);
        try testing.expectEqual((try a.symbolNamed("I")).id, ref.reference.symbol);
    }
    {
        var a = try analyze(
            \\interface I { a: number }
            \\export = I;
        , .{ .lang = .ts });
        defer a.deinit();
        const ref = try a.onlyReferenceNamed("I");
        try testing.expectEqual(.any, ref.reference.flags.space);
        try testing.expectEqual((try a.symbolNamed("I")).id, ref.reference.symbol);
    }
    {
        var a = try analyze(
            \\namespace X { export const Y = 1; }
            \\import a = X.Y;
        , .{ .lang = .ts });
        defer a.deinit();
        const ref = try a.onlyReferenceNamed("X");
        try testing.expectEqual(.any, ref.reference.flags.space);
        try testing.expectEqual((try a.symbolNamed("X")).id, ref.reference.symbol);
    }
}

test "a reference with no binding in its space anywhere is unresolved" {
    var a = try analyze(
        \\function f() {
        \\  const T = 1;
        \\  let x: T;
        \\  T;
        \\}
    , .{ .lang = .ts });
    defer a.deinit();

    var buf: [4]Semantic.ReferenceEntry = undefined;
    const refs = referencesNamed(&a, "T", &buf);
    try testing.expectEqual(@as(usize, 2), refs.len);
    try testing.expectEqual(.none, refs[0].reference.symbol); // `x: T`
    try testing.expectEqual((try a.symbolNamed("T")).id, refs[1].reference.symbol); // the bare statement use

    // no use is recorded for the unresolved type-position reference
    try testing.expectEqual(@as(usize, 1), a.sem.uses((try a.symbolNamed("T")).id).len);
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
