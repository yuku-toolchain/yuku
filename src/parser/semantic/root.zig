const std = @import("std");
const ast = @import("../ast.zig");
const traverser = @import("../traverser/root.zig");
const checker = @import("checker.zig");

const semantic = traverser.semantic;

pub const module_record = @import("module_record.zig");
pub const Semantic = semantic.Semantic;
pub const AnalysisError = checker.AnalysisError;

pub const AnalyzeOptions = struct {
    /// Whether to report the early errors of `check` in the same walk.
    check: bool,
};

/// Reports the scope-dependent early errors of a tree as diagnostics in its arena.
pub fn check(tree: *ast.Tree) AnalysisError!void {
    std.debug.assert(tree.root != .null);

    var ctx = try semantic.Ctx.init(tree, .check);
    try walk(tree, &ctx);
}

/// Returns the `Semantic` model, and with `options.check` reports the same early errors as
/// `check` in the same walk.
pub fn analyze(tree: *ast.Tree, options: AnalyzeOptions) AnalysisError!Semantic {
    std.debug.assert(tree.root != .null);

    var ctx = try semantic.Ctx.init(tree, .analyze);
    if (options.check) {
        try walk(tree, &ctx);
    } else {
        const diagnostics_count = tree.diagnostics.items.len;
        var visitor: struct {} = .{};
        try traverser.walk(semantic.Ctx, @TypeOf(visitor), &visitor, &ctx, .{
            .whole_tree = false,
        });
        std.debug.assert(tree.diagnostics.items.len == diagnostics_count);
    }
    return ctx.finalize();
}

fn walk(tree: *ast.Tree, ctx: *semantic.Ctx) AnalysisError!void {
    var visitor = checker.Checker{
        .tree = tree,
        .allocator = tree.allocator(),
    };
    try traverser.walk(semantic.Ctx, checker.Checker, &visitor, ctx, .{ .whole_tree = false });
    try visitor.checkExports(&ctx.symbols);
}
