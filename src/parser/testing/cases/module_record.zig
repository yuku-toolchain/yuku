//! Module records. Every import and export form, resolved against the
//! semantic model.

const std = @import("std");
const parser = @import("parser");
const helpers = @import("../helpers.zig");

const ast = parser.ast;
const semantic = parser.semantic;
const module_record = semantic.module_record;
const SymbolId = parser.traverser.semantic.SymbolId;

const testing = std.testing;

const Collected = struct {
    analyzed: helpers.Analyzed,
    records: module_record.Records,

    fn deinit(self: *Collected) void {
        self.analyzed.deinit();
    }

    fn str(self: *const Collected, s: ast.String) []const u8 {
        return self.analyzed.tree.string(s);
    }

    fn symbolIdOf(self: *const Collected, name: []const u8) !SymbolId {
        return (try self.analyzed.symbolNamed(name)).id;
    }
};

fn collect(source: []const u8, opts: parser.Options) !Collected {
    var analyzed = try helpers.analyze(testing.allocator, source, opts);
    errdefer analyzed.deinit();
    const records = try module_record.collect(&analyzed.tree, &analyzed.sem);
    return .{ .analyzed = analyzed, .records = records };
}

test "import records cover side-effect, default, namespace, named, and type forms" {
    var c = try collect(
        \\import "side";
        \\import d from "mod-d";
        \\import * as ns from "mod-ns";
        \\import { a, b as renamed } from "mod-n";
        \\import type T from "mod-t";
        \\import { type U } from "mod-u";
    , .{ .lang = .ts });
    defer c.deinit();

    const imports = c.records.imports;
    try testing.expectEqual(@as(usize, 7), imports.len);

    try testing.expectEqual(module_record.Import.Kind.side_effect, imports[0].kind);
    try testing.expectEqual(SymbolId.none, imports[0].symbol);
    try testing.expectEqualStrings("side", c.str(imports[0].specifier));
    try testing.expect(!imports[0].type_only);
    try testing.expectEqual(@as(?ast.ImportPhase, null), imports[0].phase);

    try testing.expectEqual(module_record.Import.Kind.named, imports[1].kind);
    try testing.expectEqualStrings("default", c.str(imports[1].name));
    try testing.expectEqual(try c.symbolIdOf("d"), imports[1].symbol);
    try testing.expectEqualStrings("mod-d", c.str(imports[1].specifier));

    try testing.expectEqual(module_record.Import.Kind.namespace, imports[2].kind);
    try testing.expectEqualStrings("", c.str(imports[2].name));
    try testing.expectEqual(try c.symbolIdOf("ns"), imports[2].symbol);

    try testing.expectEqualStrings("a", c.str(imports[3].name));
    try testing.expectEqual(try c.symbolIdOf("a"), imports[3].symbol);
    try testing.expectEqualStrings("b", c.str(imports[4].name));
    try testing.expectEqual(try c.symbolIdOf("renamed"), imports[4].symbol);
    try testing.expectEqualStrings("mod-n", c.str(imports[4].specifier));

    try testing.expect(imports[5].type_only);
    try testing.expectEqualStrings("default", c.str(imports[5].name));

    try testing.expect(imports[6].type_only);
    try testing.expectEqualStrings("U", c.str(imports[6].name));

    for (imports) |record| {
        try testing.expect(record.node != .null);
    }
}

test "import equals records an edge, qualified names do not" {
    var c = try collect(
        \\import r = require("mod-r");
        \\namespace A { export namespace B {} }
        \\import q = A.B;
    , .{ .lang = .ts });
    defer c.deinit();

    try testing.expectEqual(@as(usize, 1), c.records.imports.len);
    const record = c.records.imports[0];
    try testing.expectEqual(module_record.Import.Kind.import_equals, record.kind);
    try testing.expectEqual(try c.symbolIdOf("r"), record.symbol);
    try testing.expectEqualStrings("mod-r", c.str(record.specifier));
}

test "dynamic imports are recorded at any depth, computed specifiers are not" {
    var c = try collect(
        \\import "side";
        \\import("dyn-a");
        \\async function f() { await import("dyn-b"); }
        \\import(someExpr);
    , .{});
    defer c.deinit();

    const imports = c.records.imports;
    try testing.expectEqual(@as(usize, 3), imports.len);
    try testing.expectEqual(module_record.Import.Kind.side_effect, imports[0].kind);

    for (imports[1..], [_][]const u8{ "dyn-a", "dyn-b" }) |record, specifier| {
        try testing.expectEqual(module_record.Import.Kind.dynamic, record.kind);
        try testing.expectEqual(SymbolId.none, record.symbol);
        try testing.expect(!record.type_only);
        try testing.expectEqual(@as(?ast.ImportPhase, null), record.phase);
        try testing.expectEqualStrings(specifier, c.str(record.specifier));
        try testing.expect(record.node != .null);
    }
}

test "phased dynamic imports carry the phase" {
    var c = try collect(
        \\import.source("dyn-s");
        \\import.defer("dyn-d");
    , .{});
    defer c.deinit();

    const imports = c.records.imports;
    try testing.expectEqual(@as(usize, 2), imports.len);
    try testing.expectEqual(@as(?ast.ImportPhase, .source), imports[0].phase);
    try testing.expectEqual(@as(?ast.ImportPhase, .@"defer"), imports[1].phase);
    for (imports) |record| {
        try testing.expectEqual(module_record.Import.Kind.dynamic, record.kind);
    }
}

test "free require calls are recorded, shadowed and computed ones are not" {
    var c = try collect(
        \\const a = require("cjs-a");
        \\function f() { require("cjs-b"); }
        \\function g(require) { require("shadowed-param"); }
        \\function h() { const require = () => {}; require("shadowed-local"); }
        \\require(someExpr);
        \\require("two", "args");
    , .{ .source_type = .script });
    defer c.deinit();

    const imports = c.records.imports;
    try testing.expectEqual(@as(usize, 2), imports.len);
    for (imports, [_][]const u8{ "cjs-a", "cjs-b" }) |record, specifier| {
        try testing.expectEqual(module_record.Import.Kind.require, record.kind);
        try testing.expectEqual(SymbolId.none, record.symbol);
        try testing.expectEqualStrings(specifier, c.str(record.specifier));
        try testing.expect(record.node != .null);
    }
    try testing.expect(c.records.flags.uses_require);
}

test "ts import equals does not double as a require record" {
    var c = try collect("import r = require(\"mod-r\");", .{ .lang = .ts });
    defer c.deinit();
    try testing.expectEqual(@as(usize, 1), c.records.imports.len);
    try testing.expectEqual(module_record.Import.Kind.import_equals, c.records.imports[0].kind);
}

test "cjs flags classify free module and exports references" {
    var c = try collect(
        \\module.exports = { a: 1 };
        \\exports.b = 2;
    , .{ .source_type = .script });
    defer c.deinit();
    try testing.expect(c.records.flags.uses_module);
    try testing.expect(c.records.flags.uses_exports);
    try testing.expect(!c.records.flags.uses_require);
    try testing.expect(!c.records.flags.uses_import_meta);

    var c2 = try collect(
        "const module = {}; module.exports = 1;",
        .{ .source_type = .script },
    );
    defer c2.deinit();
    try testing.expect(!c2.records.flags.uses_module);
}

test "import meta sets its flag" {
    var c = try collect("import.meta.url;", .{});
    defer c.deinit();
    try testing.expect(c.records.flags.uses_import_meta);
    try testing.expectEqual(@as(usize, 0), c.records.imports.len);

    var c2 = try collect("class C { static f() { new.target; } }", .{});
    defer c2.deinit();
    try testing.expect(!c2.records.flags.uses_import_meta);
}

test "type-position require references do not classify" {
    var c = try collect("declare const x: typeof require;", .{ .lang = .ts });
    defer c.deinit();
    try testing.expect(!c.records.flags.uses_require);
    try testing.expectEqual(@as(usize, 0), c.records.imports.len);
}

test "exported declarations expose every bound name" {
    var c = try collect(
        \\export var v1, v2 = 1;
        \\export function ef() {}
        \\export class Ec {}
        \\export type Et = number;
        \\export interface Ei {}
        \\export enum Ee { A }
        \\export namespace En { export const q = 1; }
    , .{ .lang = .ts });
    defer c.deinit();

    const exports = c.records.exports;
    try testing.expectEqual(@as(usize, 8), exports.len);

    const expected = [_]struct { name: []const u8, type_only: bool }{
        .{ .name = "v1", .type_only = false },
        .{ .name = "v2", .type_only = false },
        .{ .name = "ef", .type_only = false },
        .{ .name = "Ec", .type_only = false },
        .{ .name = "Et", .type_only = true },
        .{ .name = "Ei", .type_only = true },
        .{ .name = "Ee", .type_only = false },
        .{ .name = "En", .type_only = false },
    };
    for (expected, exports) |want, record| {
        try testing.expectEqual(module_record.Export.Kind.named, record.kind);
        try testing.expectEqualStrings(want.name, c.str(record.name));
        try testing.expectEqual(want.type_only, record.type_only);
        try testing.expectEqualStrings("", c.str(record.specifier));
        try testing.expectEqual(try c.symbolIdOf(want.name), record.symbol);
        // the record points at the binding identifier naming it
        try testing.expect(c.analyzed.tree.data(record.node) == .binding_identifier);
    }
}

test "exported destructuring declarations expose every pattern leaf" {
    var c = try collect(
        \\export const [p1, { deep: p2, ...restObj }, ...restArr] = data;
        \\const data = [];
    , .{});
    defer c.deinit();

    const exports = c.records.exports;
    try testing.expectEqual(@as(usize, 4), exports.len);
    const names = [_][]const u8{ "p1", "p2", "restObj", "restArr" };
    for (names, exports) |want, record| {
        try testing.expectEqualStrings(want, c.str(record.name));
        try testing.expectEqual(try c.symbolIdOf(want), record.symbol);
        try testing.expect(c.analyzed.tree.data(record.node) == .binding_identifier);
    }
}

test "defaulted pattern leaves are still exported" {
    var c = try collect("export const { k: aliased = 1 } = obj; const obj = {};", .{});
    defer c.deinit();

    try testing.expectEqual(@as(usize, 1), c.records.exports.len);
    try testing.expectEqualStrings("aliased", c.str(c.records.exports[0].name));
}

test "export specifiers resolve local bindings and honor aliases" {
    var c = try collect("const l1 = 1; export { l1, l1 as alias };", .{});
    defer c.deinit();

    const exports = c.records.exports;
    try testing.expectEqual(@as(usize, 2), exports.len);

    try testing.expectEqualStrings("l1", c.str(exports[0].name));
    try testing.expectEqualStrings("alias", c.str(exports[1].name));
    for (exports) |record| {
        try testing.expectEqual(module_record.Export.Kind.named, record.kind);
        try testing.expectEqual(try c.symbolIdOf("l1"), record.symbol);
    }
}

test "re-exports carry the source name and bind nothing locally" {
    var c = try collect(
        \\export { r1, r2 as R2 } from "mod-re";
        \\export type { rt } from "mod-rt";
    , .{ .lang = .ts });
    defer c.deinit();

    const exports = c.records.exports;
    try testing.expectEqual(@as(usize, 3), exports.len);

    try testing.expectEqualStrings("r1", c.str(exports[0].name));
    try testing.expectEqualStrings("r1", c.str(exports[0].from_name));
    try testing.expectEqualStrings("R2", c.str(exports[1].name));
    try testing.expectEqualStrings("r2", c.str(exports[1].from_name));
    try testing.expectEqualStrings("mod-re", c.str(exports[1].specifier));

    try testing.expect(exports[2].type_only);

    for (exports) |record| {
        try testing.expectEqual(module_record.Export.Kind.re_export, record.kind);
        try testing.expectEqual(SymbolId.none, record.symbol);
    }
}

test "export star forms produce star and namespace records" {
    var c = try collect(
        \\export * from "m1";
        \\export * as bundle from "m2";
    , .{});
    defer c.deinit();

    const exports = c.records.exports;
    try testing.expectEqual(@as(usize, 2), exports.len);

    try testing.expectEqual(module_record.Export.Kind.star, exports[0].kind);
    try testing.expectEqualStrings("", c.str(exports[0].name));
    try testing.expectEqualStrings("m1", c.str(exports[0].specifier));

    try testing.expectEqual(module_record.Export.Kind.namespace, exports[1].kind);
    try testing.expectEqualStrings("bundle", c.str(exports[1].name));
    try testing.expectEqualStrings("m2", c.str(exports[1].specifier));
}

test "default exports resolve named declarations to their symbol" {
    var c = try collect("export default function dfn() {}", .{});
    defer c.deinit();

    try testing.expectEqual(@as(usize, 1), c.records.exports.len);
    const record = c.records.exports[0];
    try testing.expectEqual(module_record.Export.Kind.named, record.kind);
    try testing.expectEqualStrings("default", c.str(record.name));
    try testing.expectEqual(try c.symbolIdOf("dfn"), record.symbol);
}

test "anonymous default exports bind no symbol" {
    var c = try collect("export default class {}", .{});
    defer c.deinit();
    try testing.expectEqual(SymbolId.none, c.records.exports[0].symbol);

    var c2 = try collect("export default 1 + 2;", .{});
    defer c2.deinit();
    try testing.expectEqual(SymbolId.none, c2.records.exports[0].symbol);
    try testing.expectEqualStrings("default", c2.str(c2.records.exports[0].name));
}

test "default-exported identifiers resolve to the module binding" {
    var c = try collect("const value = 1; export default value;", .{});
    defer c.deinit();
    try testing.expectEqual(try c.symbolIdOf("value"), c.records.exports[0].symbol);
}

test "export assignment records the equals kind" {
    var c = try collect("const eq = 1; export = eq;", .{ .lang = .ts });
    defer c.deinit();

    try testing.expectEqual(@as(usize, 1), c.records.exports.len);
    const record = c.records.exports[0];
    try testing.expectEqual(module_record.Export.Kind.equals, record.kind);
    try testing.expectEqualStrings("", c.str(record.name));
    try testing.expectEqual(try c.symbolIdOf("eq"), record.symbol);

    var c2 = try collect("export = 1 + 2;", .{ .lang = .ts });
    defer c2.deinit();
    try testing.expectEqual(SymbolId.none, c2.records.exports[0].symbol);
}

test "export as namespace records a global alias" {
    var c = try collect(
        "declare function f(): void; export as namespace MyLib;",
        .{ .lang = .dts },
    );
    defer c.deinit();

    try testing.expectEqual(@as(usize, 1), c.records.exports.len);
    const record = c.records.exports[0];
    try testing.expectEqual(module_record.Export.Kind.global, record.kind);
    try testing.expectEqualStrings("MyLib", c.str(record.name));
    try testing.expectEqual(SymbolId.none, record.symbol);
}

test "scripts without cjs syntax produce empty records and flags" {
    var c = try collect("var x = 1; x;", .{ .source_type = .script });
    defer c.deinit();
    try testing.expectEqual(@as(usize, 0), c.records.imports.len);
    try testing.expectEqual(@as(usize, 0), c.records.exports.len);
    try testing.expectEqual(module_record.Flags{}, c.records.flags);
}
