const std = @import("std");
const ast = @import("../ast.zig");
const traverser = @import("../traverser/root.zig");
const checker = @import("checker.zig");

const semantic = traverser.semantic;

pub const module_record = @import("module_record.zig");
pub const Semantic = semantic.Semantic;
pub const AnalysisError = checker.AnalysisError;

/// Reports the scope-dependent early errors of a tree as diagnostics in its arena.
pub fn check(tree: *ast.Tree) AnalysisError!void {
    std.debug.assert(tree.root != .null);

    var ctx = try semantic.Ctx.init(tree, .check);
    try walk(tree, &ctx);
}

/// Reports the same early errors as `check` and returns the `Semantic` model.
pub fn analyze(tree: *ast.Tree) AnalysisError!Semantic {
    std.debug.assert(tree.root != .null);

    var ctx = try semantic.Ctx.init(tree, .analyze);
    try walk(tree, &ctx);
    return ctx.finalize();
}

fn walk(tree: *ast.Tree, ctx: *semantic.Ctx) AnalysisError!void {
    var visitor = checker.Checker{
        .tree = tree,
        .allocator = tree.allocator(),
    };
    var layer = traverser.Layer(semantic.Ctx, checker.Checker){ .inner = &visitor };
    try traverser.walk(semantic.Ctx, @TypeOf(layer), &layer, ctx);
    try visitor.checkExports(&ctx.symbols);
}
