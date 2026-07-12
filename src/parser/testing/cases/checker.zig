const std = @import("std");
const parser = @import("parser");
const helpers = @import("../helpers.zig");

const testing = std.testing;

fn expectClean(source: []const u8) !void {
    var a = try helpers.analyzeAllowErrors(testing.allocator, source, .{ .source_type = .script });
    defer a.deinit();
    if (a.tree.hasErrors()) {
        for (a.tree.diagnostics.items) |d| std.debug.print("unexpected: {s}\n", .{d.message});
        std.debug.print("source: {s}\n", .{source});
        return error.UnexpectedEarlyError;
    }
}

fn expectRedeclaration(source: []const u8, name: []const u8) !void {
    var a = try helpers.analyzeAllowErrors(testing.allocator, source, .{ .source_type = .script });
    defer a.deinit();
    var buf: [128]u8 = undefined;
    const expected = try std.fmt.bufPrint(&buf, "Identifier '{s}' has already been declared", .{name});
    for (a.tree.diagnostics.items) |d| {
        if (std.mem.eql(u8, d.message, expected)) return;
    }
    std.debug.print("missing \"{s}\" for: {s}\n", .{ expected, source });
    return error.MissingEarlyError;
}

test "a var may redeclare a simple catch parameter" {
    try expectClean("try {} catch (e) { var e; }");
    try expectClean("try {} catch (e) { { var e; } }");
    try expectClean("try {} catch (e) { for (var e;;) {} }");
    try expectClean("try {} catch (e) { for (var e in x) {} }");
    try expectClean("try {} catch (e) { for (var e of x) {} }");
    try expectClean("try {} catch (e) { for (var [e] of x) {} }");
    try expectClean("async function f() { try {} catch (e) { for await (var e of x) {} } }");
    try expectClean("\"use strict\"; try {} catch (e) { var e; }");
}

test "a var may not redeclare a destructured catch parameter" {
    try expectRedeclaration("try {} catch ([e]) { var e; }", "e");
    try expectRedeclaration("try {} catch ({ e }) { var e; }", "e");
    try expectRedeclaration("try {} catch ([e]) { for (var e in x) {} }", "e");
    try expectRedeclaration("try {} catch ([e]) { for (var e of x) {} }", "e");
    try expectRedeclaration("try {} catch ([a, [e]]) { var e; }", "e");
    try expectClean("try {} catch ([e]) { var other; }");
    try expectClean("try {} catch { var e; }");
}

test "duplicate names inside one catch parameter conflict" {
    try expectRedeclaration("try {} catch ([e, e]) {}", "e");
    try expectRedeclaration("try {} catch ({ a: e, b: e }) {}", "e");
}

test "lexical declarations always conflict with catch parameters" {
    try expectRedeclaration("try {} catch (e) { let e; }", "e");
    try expectRedeclaration("try {} catch (e) { const e = 1; }", "e");
    try expectRedeclaration("try {} catch ([e]) { let e; }", "e");
}

test "vars named like unrelated catch parameters stay legal" {
    try expectClean("try {} catch (e) {} var e;");
    try expectClean("function f() { try {} catch (e) {} } var e;");
}

test "duplicate parameters are legal only in sloppy simple-parameter functions" {
    try expectClean("function f(a, a) { return a; }");
    try expectClean("var f = function (a, a) {};");
    try expectClean("({ set s(a) {} });");
}

test "duplicate parameters conflict in strict code" {
    try expectRedeclaration("\"use strict\"; function f(a, a) {}", "a");
    try expectRedeclaration("function f(a, a) { \"use strict\"; }", "a");
}

test "duplicate parameters conflict in arrows, methods, and non-simple lists" {
    try expectRedeclaration("var g = (a, a) => a;", "a");
    try expectRedeclaration("({ m(a, a) {} });", "a");
    try expectRedeclaration("class C { m(a, a) {} }", "a");
    try expectRedeclaration("function f(a, a = 1) {}", "a");
    try expectRedeclaration("function f(a, ...a) {}", "a");
    try expectRedeclaration("function f(a, a, [b]) {}", "a");
    try expectRedeclaration("({ set s([a, a]) {} });", "a");
}
