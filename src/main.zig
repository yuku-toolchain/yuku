const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const traverser = parser.traverser;
const scoped = traverser.scoped;

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

    // -- Redeclaration linter --

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    const Linter = struct {
        syms: scoped.SymbolTable,
        source: []const u8,

        pub fn enter_node(self: *@This(), data: ast.NodeData, _: *scoped.ScopedCtx) traverser.Action {
            if (scoped.scopeKindOf(std.meta.activeTag(data)) != null) {
                self.syms.pushScope();
            }
            return .proceed;
        }

        pub fn enter_variable_declarator(self: *@This(), decl: ast.VariableDeclarator, ctx: *scoped.ScopedCtx) traverser.Action {
            const id_data = ctx.tree.getData(decl.id);
            switch (id_data) {
                .binding_identifier => |binding| {
                    const name_slice = ctx.tree.getSourceText(binding.name_start, binding.name_len);

                    if (self.syms.resolve(name_slice)) |existing_id| {
                        const existing = self.syms.get(existing_id);
                        if (existing.scope == ctx.currentScope()) {
                            const pos = getLineAndColumn(self.source, binding.name_start);
                            std.debug.print("redeclaration of '{s}' at {d}:{d}\n", .{ name_slice, pos.line, pos.col });
                        }
                    }

                    _ = self.syms.declare(.{
                        .name_start = binding.name_start,
                        .name_len = binding.name_len,
                        .node = decl.id,
                        .scope = ctx.currentScope(),
                        .kind = .variable,
                        .flags = .{},
                    }, name_slice);
                },
                else => {},
            }
            return .proceed;
        }

        pub fn exit_node(self: *@This(), data: ast.NodeData, _: *scoped.ScopedCtx) void {
            if (scoped.scopeKindOf(std.meta.activeTag(data)) != null) {
                self.syms.popScope();
            }
        }
    };

    var linter = Linter{
        .syms = scoped.SymbolTable.init(arena_allocator, @intCast(tree.nodes.len / 8)),
        .source = contents,
    };

    const start = std.Io.Clock.Timestamp.now(Io, .real);

    const scope_tree = scoped.traverse(Linter, &tree, &linter, arena_allocator);

    const end = std.Io.Clock.Timestamp.now(Io, .real);
    const taken_ms = @as(f64, @floatFromInt(start.durationTo(end).raw.toNanoseconds())) / std.time.ns_per_ms;

    std.debug.print("\nscopes: {d}, symbols: {d}, took: {d:.2}ms\n", .{ scope_tree.len(), linter.syms.count(), taken_ms });
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
