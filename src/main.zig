const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;

const traverser = parser.traverser;
const scoped = traverser.scoped;
const symbols = traverser.symbols;

pub fn main(init: std.process.Init) !void {
    const Io = init.io;
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file_path = "test.js";

    const contents = try std.Io.Dir.cwd().readFileAlloc(Io, file_path, allocator, std.Io.Limit.limited(10 * 1024 * 1024));
    defer allocator.free(contents);

    const tree = try parser.parse(std.heap.page_allocator, contents, .{ .lang = .fromPath(file_path), .source_type = .fromPath(file_path) });
    defer tree.deinit();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();

    const Linter = struct {
        symbols: symbols.SymbolTable,
        source: []const u8,

        pub fn enter_node(self: *@This(), data: ast.NodeData, _: ast.NodeIndex, _: *scoped.ScopedCtx) traverser.Action {
            if (scoped.scopeKindOf(std.meta.activeTag(data)) != null) {
                self.symbols.pushScope();
            }
            return .proceed;
        }

        pub fn enter_binding_identifier(self: *@This(), id: ast.VariableDeclarator, index: ast.NodeIndex, ctx: *scoped.ScopedCtx) traverser.Action {
            const name_slice = ctx.tree.getSourceText(id.name_start, id.name_len);

            if (self.symbols.resolve(name_slice)) |existing_id| {
                const existing = self.symbols.get(existing_id);

                if (existing.scope == ctx.currentScope()) {
                    const pos = getLineAndColumn(self.source, id.name_start);

                    std.debug.print("redeclaration of '{s}' at test.js:{d}:{d}\n", .{ name_slice, pos.line, pos.col });
                }
            }

            _ = self.symbols.declare(.{
                .name_start = id.name_start,
                .name_len = id.name_len,
                .node = index,
                .scope = ctx.currentScope(),
                .kind = .variable,
                .flags = .{},
            }, name_slice);

            return .proceed;
        }

        pub fn exit_node(self: *@This(), data: ast.NodeData, _: ast.NodeIndex, _: *scoped.ScopedCtx) void {
            if (scoped.scopeKindOf(std.meta.activeTag(data)) != null) {
                self.symbols.popScope();
            }
        }
    };

    var linter = Linter{
        .symbols = symbols.SymbolTable.init(arena_allocator),
        .source = contents,
    };

    const start = std.Io.Clock.Timestamp.now(Io, .real);

    const scope_tree = scoped.traverse(Linter, &tree, &linter, arena_allocator);

    const end = std.Io.Clock.Timestamp.now(Io, .real);
    const taken_ms = @as(f64, @floatFromInt(start.durationTo(end).raw.toNanoseconds())) / std.time.ns_per_ms;

    std.debug.print("\nscopes: {d}, symbols: {d}, took: {d:.2}ms\n", .{ scope_tree.len(), linter.symbols.count(), taken_ms });
}

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
