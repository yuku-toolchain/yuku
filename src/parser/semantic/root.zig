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

    if (options.check) {
        var ctx = try semantic.Ctx.init(tree, .analyze);
        try walk(tree, &ctx);
        return ctx.finalize();
    } else {
        const diagnostics_count = tree.diagnostics.items.len;
        var visitor: struct {} = .{};
        const model = try semantic.traverse(@TypeOf(visitor), tree, &visitor);
        std.debug.assert(tree.diagnostics.items.len == diagnostics_count);
        return model;
    }
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
