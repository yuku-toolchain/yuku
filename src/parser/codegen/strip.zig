const std = @import("std");
const ast = @import("../ast.zig");
const printer = @import("printer.zig");

const Allocator = std.mem.Allocator;

/// Strips TypeScript from `tree` and codegens JavaScript.
pub fn strip(allocator: Allocator, tree: *ast.Tree, options: printer.Options) printer.Error!printer.Result {
    return printer.printImpl(true, allocator, tree, options);
}
