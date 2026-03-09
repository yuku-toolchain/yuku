const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const traverser = parser.traverser;
const symbols = traverser.symbols;

pub fn main(init: std.process.Init) !void {
    const Io = init.io;

    const allocator = init.arena.allocator();

    const file_path = "test.js";

    const contents = try std.Io.Dir.cwd().readFileAlloc(Io, file_path, allocator, std.Io.Limit.limited(10 * 1024 * 1024));

    const tree = try parser.parse(std.heap.page_allocator, contents, .{ .lang = .fromPath(file_path), .source_type = .fromPath(file_path) });
    defer tree.deinit();

    const start = std.Io.Clock.Timestamp.now(Io, .real);

    // Run symbol-tracking traversal with redeclaration checker
    var checker = RedeclChecker{};
    var result = try symbols.traverse(RedeclChecker, &tree, &checker, std.heap.page_allocator);
    defer result.deinit();

    const end = std.Io.Clock.Timestamp.now(Io, .real);
    const taken_ms = @as(f64, @floatFromInt(start.durationTo(end).raw.toNanoseconds())) / std.time.ns_per_ms;

    // Print results
    for (checker.errors.items) |err| {
        const loc = getLineAndColumn(contents, err.offset);
        std.debug.print("{s}:{d}:{d}: redeclaration of '{s}'\n", .{ file_path, loc.line, loc.col, err.name });
    }

    std.debug.print("\nsymbols: {d}, references: {d}, scopes: {d}, errors: {d}\n", .{
        result.symbol_table.symbols.len,
        result.symbol_table.references.len,
        result.scope_tree.scopes.len,
        checker.errors.items.len,
    });
    std.debug.print("took: {d:.2}ms\n", .{taken_ms});
}

const RedeclChecker = struct {
    errors: std.ArrayList(Error) = .{},

    const Error = struct {
        name: []const u8,
        offset: usize,
    };

    pub fn enter_binding_identifier(
        self: *RedeclChecker,
        id: ast.BindingIdentifier,
        _: ast.NodeIndex,
        ctx: *symbols.Ctx,
    ) traverser.Action {
        const name = ctx.tree.getSourceText(id.name_start, id.name_len);
        const current_kind = ctx.symbols.currentBindingKind();
        const target_scope = switch (current_kind) {
            .hoisted => ctx.scope.currentHoistScopeId(),
            .function => ctx.scope.currentScope().parent,
            .class => ctx.scope.currentScope().parent,
            else => ctx.scope.currentScopeId(),
        };

        if (ctx.symbols.findInScope(target_scope, name)) |existing_id| {
            const existing = ctx.symbols.getSymbol(existing_id);

            // var+var and function+function redeclarations are allowed
            const allowed = switch (existing.kind) {
                .hoisted, .function => current_kind == .hoisted or current_kind == .function,
                else => false,
            };

            if (!allowed) {
                self.errors.append(std.heap.page_allocator, .{
                    .name = name,
                    .offset = id.name_start,
                }) catch {};
            }
        }

        return .proceed;
    }
};

fn getLineAndColumn(contents: []const u8, offset: usize) struct { line: usize, col: usize } {
    var line: usize = 1;
    var col: usize = 1;

    for (contents[0..@min(offset, contents.len)]) |char| {
        if (char == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    return .{ .line = line, .col = col };
}
