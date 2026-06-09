//! Semantic analysis tests: reference write classification and module
//! import/export records, asserted end to end through parse + analyze.

const std = @import("std");
const ast = @import("../ast.zig");
const parser = @import("../parser.zig");
const semantic = @import("root.zig");
const module_record = @import("module_record.zig");

const testing = std.testing;
const analyze = semantic.analyze;

const ExpectedRef = struct { name: []const u8, write: bool };

fn expectNoErrors(tree: *const ast.Tree) !void {
    if (tree.hasErrors()) {
        for (tree.diagnostics.items) |d| {
            std.debug.print("diagnostic: {s}: {s} ({d}..{d})\n", .{
                d.severity.toString(), d.message, d.span.start, d.span.end,
            });
        }
        return error.TestUnexpectedResult;
    }
}

// parses `source`, runs semantic analysis, and asserts that the recorded
// references match `expected` in source order, both names and is_write.
fn expectReferences(
    source: []const u8,
    lang: ast.Lang,
    expected: []const ExpectedRef,
) !void {
    var tree = try parser.parse(testing.allocator, source, .{
        .lang = lang,
        .source_type = .module,
    });
    defer tree.deinit();
    try expectNoErrors(&tree);

    const result = try analyze(&tree);
    try expectNoErrors(&tree);

    var iter = result.symbol_table.iterReferences();
    var i: usize = 0;
    while (iter.next()) |entry| : (i += 1) {
        try testing.expect(i < expected.len);
        const name = tree.string(entry.reference.name);
        try testing.expectEqualStrings(expected[i].name, name);
        try testing.expectEqual(expected[i].write, entry.reference.is_write);
    }
    try testing.expectEqual(expected.len, i);
}

// the positive space: every assignment-target form marks its reference
// as a write, while the surrounding reads stay reads.
test "is_write: assignment, update, and for-in/of targets" {
    try expectReferences(
        \\let a = 1;
        \\let b = 2;
        \\const arr = [];
        \\const obj = {};
        \\a = 10;
        \\b += 1;
        \\a++;
        \\--b;
        \\[a, b] = arr;
        \\[a = b] = arr;
        \\({ x: a } = obj);
        \\({ a } = obj);
        \\for (a of arr) {}
        \\for (b in obj) {}
    , .js, &.{
        .{ .name = "a", .write = true }, // a = 10
        .{ .name = "b", .write = true }, // b += 1
        .{ .name = "a", .write = true }, // a++
        .{ .name = "b", .write = true }, // --b
        .{ .name = "a", .write = true }, // [a, b] = arr
        .{ .name = "b", .write = true },
        .{ .name = "arr", .write = false },
        .{ .name = "a", .write = true }, // [a = b] = arr
        .{ .name = "b", .write = false }, // default value is a read
        .{ .name = "arr", .write = false },
        .{ .name = "a", .write = true }, // ({ x: a } = obj)
        .{ .name = "obj", .write = false },
        .{ .name = "a", .write = true }, // ({ a } = obj)
        .{ .name = "obj", .write = false },
        .{ .name = "a", .write = true }, // for (a of arr)
        .{ .name = "arr", .write = false },
        .{ .name = "b", .write = true }, // for (b in obj)
        .{ .name = "obj", .write = false },
    });
}

// pins the current namespace model: ts_module_block interiors count as
// type position, so interior references are kind .type (and unresolved,
// since interior bindings are not modeled yet) but still carry a
// truthful is_write.
test "is_write: ts namespace interiors keep truthful write flags" {
    try expectReferences(
        \\namespace N { let y = 2; y = y + 1; }
    , .ts, &.{
        .{ .name = "y", .write = true },
        .{ .name = "y", .write = false },
    });
}

// the negative space: member writes mutate the object, not the binding.
// the base object and the RHS are reads.
test "is_write: member assignment and reads stay reads" {
    try expectReferences(
        \\const obj = {};
        \\let a = 1;
        \\obj.x = a;
        \\obj[a] = a;
        \\f(a);
        \\function f(p) { return p; }
    , .js, &.{
        .{ .name = "obj", .write = false },
        .{ .name = "a", .write = false },
        .{ .name = "obj", .write = false },
        .{ .name = "a", .write = false }, // computed key
        .{ .name = "a", .write = false }, // value
        .{ .name = "f", .write = false },
        .{ .name = "a", .write = false },
        .{ .name = "p", .write = false },
    });
}

// ts wrappers are transparent for target classification, and
// type-position references never write.
test "is_write: typescript wrappers and type positions" {
    try expectReferences(
        \\let a: number = 1;
        \\let b = 2;
        \\(a as number) = b;
        \\a!++;
        \\(a)++;
        \\type T = typeof a;
        \\let c: T = a;
    , .ts, &.{
        .{ .name = "a", .write = true }, // (a as number) = b
        .{ .name = "b", .write = false },
        .{ .name = "a", .write = true }, // a!++
        .{ .name = "a", .write = true }, // (a)++
        .{ .name = "a", .write = false }, // typeof a (type query)
        .{ .name = "T", .write = false }, // type annotation
        .{ .name = "a", .write = false }, // initializer read
    });
}

const ExpectedImport = struct {
    name_kind: module_record.NameKind,
    name: []const u8,
    specifier: []const u8,
    kind: ast.ImportOrExportKind = .value,
    has_symbol: bool = true,
};

const ExpectedExport = struct {
    name_kind: module_record.NameKind,
    name: []const u8,
    has_symbol: bool,
    from_kind: module_record.NameKind = .none,
    from_name: []const u8 = "",
    specifier: []const u8 = "",
    kind: ast.ImportOrExportKind = .value,
};

// parses `source` as a module, analyzes, collects module records, and
// asserts them in source order.
fn expectRecords(
    source: []const u8,
    lang: ast.Lang,
    expected_imports: []const ExpectedImport,
    expected_exports: []const ExpectedExport,
) !void {
    var tree = try parser.parse(testing.allocator, source, .{
        .lang = lang,
        .source_type = .module,
    });
    defer tree.deinit();
    try expectNoErrors(&tree);

    const result = try analyze(&tree);
    try expectNoErrors(&tree);

    const records = try module_record.collect(&tree, &result.symbol_table);

    try testing.expectEqual(expected_imports.len, records.imports.len);
    for (expected_imports, records.imports) |want, got| {
        try testing.expectEqual(want.name_kind, got.name_kind);
        try testing.expectEqualStrings(want.name, tree.string(got.name));
        try testing.expectEqualStrings(want.specifier, tree.string(got.specifier));
        try testing.expectEqual(want.kind, got.kind);
        try testing.expectEqual(want.has_symbol, got.symbol != .none);
    }

    try testing.expectEqual(expected_exports.len, records.exports.len);
    for (expected_exports, records.exports) |want, got| {
        try testing.expectEqual(want.name_kind, got.name_kind);
        try testing.expectEqualStrings(want.name, tree.string(got.name));
        try testing.expectEqual(want.has_symbol, got.symbol != .none);
        try testing.expectEqual(want.from_kind, got.from_kind);
        try testing.expectEqualStrings(want.from_name, tree.string(got.from_name));
        try testing.expectEqualStrings(want.specifier, tree.string(got.specifier));
        try testing.expectEqual(want.kind, got.kind);
    }
}

test "module records: import forms" {
    try expectRecords(
        \\import def from "a";
        \\import { x, y as z, type T } from "b";
        \\import * as ns from "c";
        \\import "d";
        \\import fs = require("fs");
    , .ts, &.{
        .{ .name_kind = .named, .name = "default", .specifier = "a" },
        .{ .name_kind = .named, .name = "x", .specifier = "b" },
        .{ .name_kind = .named, .name = "y", .specifier = "b" },
        .{ .name_kind = .named, .name = "T", .specifier = "b", .kind = .type },
        .{ .name_kind = .star, .name = "", .specifier = "c" },
        .{ .name_kind = .none, .name = "", .specifier = "d", .has_symbol = false },
        .{ .name_kind = .star, .name = "", .specifier = "fs" },
    }, &.{});
}

test "module records: export forms" {
    try expectRecords(
        \\const internal = 1;
        \\export const a = 1, { b, c: d = 2 } = obj;
        \\export function f() {}
        \\export class C {}
        \\export type T = number;
        \\export { internal, internal as renamed };
        \\export { x as y } from "m";
        \\export * from "n";
        \\export * as ns from "o";
        \\export default f;
        \\declare const obj: { b: number; c: number };
    , .ts, &.{}, &.{
        .{ .name_kind = .named, .name = "a", .has_symbol = true },
        .{ .name_kind = .named, .name = "b", .has_symbol = true },
        .{ .name_kind = .named, .name = "d", .has_symbol = true },
        .{ .name_kind = .named, .name = "f", .has_symbol = true },
        .{ .name_kind = .named, .name = "C", .has_symbol = true },
        .{ .name_kind = .named, .name = "T", .has_symbol = true, .kind = .type },
        .{ .name_kind = .named, .name = "internal", .has_symbol = true },
        .{ .name_kind = .named, .name = "renamed", .has_symbol = true },
        .{
            .name_kind = .named,
            .name = "y",
            .has_symbol = false,
            .from_kind = .named,
            .from_name = "x",
            .specifier = "m",
        },
        .{
            .name_kind = .star,
            .name = "",
            .has_symbol = false,
            .from_kind = .star,
            .specifier = "n",
        },
        .{
            .name_kind = .named,
            .name = "ns",
            .has_symbol = false,
            .from_kind = .star,
            .specifier = "o",
        },
        .{ .name_kind = .named, .name = "default", .has_symbol = true },
    });
}

test "module records: anonymous default export binds nothing" {
    try expectRecords(
        \\export default function () {}
    , .js, &.{}, &.{
        .{ .name_kind = .named, .name = "default", .has_symbol = false },
    });
}
