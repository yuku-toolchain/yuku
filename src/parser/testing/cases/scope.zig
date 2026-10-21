const std = @import("std");
const parser = @import("parser");
const helpers = @import("../helpers.zig");

const Scope = parser.semantic.Scope;
const ScopeId = parser.semantic.ScopeId;

const testing = std.testing;

test "scope ancestors walk from a scope to the root" {
    var a = try helpers.analyze("function f() { { let x; } }", .{});
    defer a.deinit();

    const x_scope_id = (try a.symbolNamed("x")).symbol.scope;
    var it = a.sem.scopes.ancestors(x_scope_id);

    try testing.expectEqual(x_scope_id, it.next().?);
    try testing.expectEqual(Scope.Kind.function_body, a.sem.scopes.get(it.next().?).kind);
    try testing.expectEqual(Scope.Kind.function, a.sem.scopes.get(it.next().?).kind);
    try testing.expectEqual(a.sem.scopeOf(a.tree.root), it.next().?);
    try testing.expectEqual(ScopeId.root, it.next().?);
    try testing.expectEqual(@as(?ScopeId, null), it.next());

    var none_it = a.sem.scopes.ancestors(.none);
    try testing.expectEqual(@as(?ScopeId, null), none_it.next());
}

test "scopeOf maps scope-creating nodes to the scope they create" {
    var a = try helpers.analyze("function f() {}", .{});
    defer a.deinit();

    const func_node = try a.firstNode(.function);
    const func_scope = a.sem.scopes.get(a.sem.scopeOf(func_node));
    try testing.expectEqual(Scope.Kind.function, func_scope.kind);
    try testing.expectEqual(func_node, func_scope.node);
    try testing.expectEqual(Scope.Kind.module, a.sem.scopes.get(a.sem.scopeOf(a.tree.root)).kind);
}

test "Scope.Kind.isHoistTarget matches the spec hoist boundaries" {
    try testing.expect(Scope.Kind.isHoistTarget(.global));
    try testing.expect(Scope.Kind.isHoistTarget(.module));
    try testing.expect(Scope.Kind.isHoistTarget(.function));
    try testing.expect(Scope.Kind.isHoistTarget(.static_block));
    try testing.expect(Scope.Kind.isHoistTarget(.ts_module));
    // a body scope decides per instance, see its hoist_target
    try testing.expect(Scope.Kind.isHoistTarget(.function_body));
    try testing.expect(!Scope.Kind.isHoistTarget(.block));
    try testing.expect(!Scope.Kind.isHoistTarget(.class));
    try testing.expect(!Scope.Kind.isHoistTarget(.expression_name));
}

test "nesting deeper than the path capacity analyzes without a parent" {
    const source = helpers.repeat("typeof ", 300) ++ "function f() { switch (a) { case 1: b } }";
    var result = try helpers.analyzeAllowErrors(source, .{});
    defer result.deinit();
    try testing.expect(result.sem.scopes.list.len >= 2);
}
