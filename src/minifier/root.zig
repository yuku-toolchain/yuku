const std = @import("std");
const parser = @import("parser");

pub const options = @import("options.zig");
pub const mangle = @import("mangle.zig");

pub const Options = options.Options;
pub const MangleOptions = options.MangleOptions;
pub const FormatOptions = options.FormatOptions;

pub const Result = parser.codegen.Result;
pub const Diagnostic = parser.codegen.Diagnostic;
pub const Error = error{OutOfMemory};

/// Minifies `tree` into compact JavaScript source.
///
/// `tree` must already be parsed. Caller owns the returned `Result` and
/// frees it via `result.deinit(allocator)`.
pub fn minify(allocator: std.mem.Allocator, tree: *parser.ast.Tree, opts: Options) Error!Result {
    if (opts.mangle.enabled) {
        var noop = NoopVisitor{};
        var sem = try parser.traverser.semantic.traverse(NoopVisitor, tree, &noop);
        try sem.symbol_table.resolveAll(sem.scope_tree);
        try mangle.run(allocator, tree, sem.scope_tree, sem.symbol_table, opts.mangle);
    }

    return parser.codegen.minify(allocator, tree, .{
        .format = opts.format.format,
        .quotes = opts.format.quotes,
    });
}

const NoopVisitor = struct {};
