const std = @import("std");
const ast = @import("../ast.zig");
const traverser = @import("../traverser/root.zig");
const checker = @import("checker.zig");
const binder = @import("binder.zig");
const scope = @import("scope.zig");

const Ctx = traverser.semantic.Ctx;
const Error = std.mem.Allocator.Error;

pub const module_record = @import("module_record.zig");
pub const Semantic = binder.Semantic;
pub const Symbol = binder.Symbol;
pub const SymbolId = binder.SymbolId;
pub const Reference = binder.Reference;
pub const ReferenceId = binder.ReferenceId;
pub const Scope = scope.Scope;
pub const ScopeId = scope.ScopeId;
pub const ScopeTree = scope.ScopeTree;

pub const AnalyzeOptions = struct {
    /// Whether to report the early errors of `check` in the same walk.
    check: bool,
};

/// Reports the scope-dependent early errors of a tree as diagnostics in its arena.
pub fn check(tree: *ast.Tree) Error!void {
    std.debug.assert(tree.root != .null);
    var ctx = try Ctx.init(tree, .check);
    try walk(tree, &ctx);
}

/// Returns the `Semantic` model, and with `options.check` reports the same early errors as
/// `check` in the same walk.
pub fn analyze(tree: *ast.Tree, options: AnalyzeOptions) Error!Semantic {
    std.debug.assert(tree.root != .null);
    var ctx = try Ctx.init(tree, .analyze);
    if (options.check) {
        try walk(tree, &ctx);
    } else {
        const diagnostics_count = tree.diagnostics.items.len;
        var visitor: struct {} = .{};
        try traverser.walk(Ctx, @TypeOf(visitor), &visitor, &ctx, .{ .whole_tree = false });
        std.debug.assert(tree.diagnostics.items.len == diagnostics_count);
    }
    return ctx.finalize();
}

fn walk(tree: *ast.Tree, ctx: *Ctx) Error!void {
    var visitor = checker.Checker{ .tree = tree, .allocator = tree.allocator() };
    try traverser.walk(Ctx, checker.Checker, &visitor, ctx, .{ .whole_tree = false });
    try visitor.checkExports(&ctx.symbols);
}
