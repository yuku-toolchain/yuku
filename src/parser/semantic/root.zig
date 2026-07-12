const std = @import("std");
const ast = @import("../ast.zig");
const traverser = @import("../traverser/root.zig");
const checker = @import("checker.zig");

const semantic = traverser.semantic;

pub const module_record = @import("module_record.zig");
pub const Semantic = semantic.Semantic;
pub const AnalysisError = checker.AnalysisError;

/// Runs semantic analysis on a tree: builds the complete `Semantic`
/// model and reports the scope-dependent early errors.
///
/// Diagnostics are appended directly to the tree alongside parse
/// errors. All allocations use the tree's arena, so the returned
/// model is valid as long as the tree is alive.
pub fn analyze(tree: *ast.Tree) AnalysisError!Semantic {
    std.debug.assert(tree.root != .null);

    var visitor = checker.Checker{
        .tree = tree,
        .allocator = tree.allocator(),
    };

    const sem = try semantic.traverse(checker.Checker, tree, &visitor);
    try visitor.checkUnresolvedExports(sem);
    return sem;
}
