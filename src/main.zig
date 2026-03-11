const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const traverser = parser.traverser;
const semantic = traverser.semantic;

pub fn main(init: std.process.Init) !void {
    const Io = init.io;

    const allocator = init.arena.allocator();

    const file_path = "test.js";

    const contents = try std.Io.Dir.cwd().readFileAlloc(Io, file_path, allocator, std.Io.Limit.limited(10 * 1024 * 1024));

    const tree = try parser.parse(std.heap.page_allocator, contents, .{ .lang = .fromPath(file_path), .source_type = .fromPath(file_path) });
    defer tree.deinit();

    const start = std.Io.Clock.Timestamp.now(Io, .real);

    var checker = RedeclChecker{};
    var result = try semantic.traverse(RedeclChecker, &tree, &checker, std.heap.page_allocator);
    defer result.deinit();

    const end = std.Io.Clock.Timestamp.now(Io, .real);
    const taken_ms = @as(f64, @floatFromInt(start.durationTo(end).raw.toNanoseconds())) / std.time.ns_per_ms;

    const json = try parser.estree.toJSON(&tree, allocator, .{});

    std.debug.print("{s}\n\n", .{json});

    for (checker.errors.items) |err| {
        const current_loc = getLineAndColumn(contents, err.offset);
        const existing_loc = getLineAndColumn(contents, err.existing_offset);
        std.debug.print("{s}:{d}:{d}: redeclaration of '{s}' which already declared at {s}:{d}:{d}\n", .{ file_path, current_loc.line, current_loc.col, err.name, file_path, existing_loc.line, existing_loc.col });
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
        existing_offset: usize,
    };

    pub fn enter_binding_identifier(
        self: *RedeclChecker,
        id: ast.BindingIdentifier,
        _: ast.NodeIndex,
        ctx: *semantic.Ctx,
    ) traverser.Action {
        const name = ctx.tree.getSourceText(id.name_start, id.name_len);

        const target = ctx.symbols.resolveTargetScope(&ctx.scope);

        if (ctx.symbols.findInScope(target, name)) |ex| {
            const existing = ctx.symbols.getSymbol(ex);

            self.errors.append(std.heap.page_allocator, .{
                .name = name,
                .offset = id.name_start,
                .existing_offset = existing.name_start,
            }) catch {};
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
