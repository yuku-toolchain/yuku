const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const sc = parser.traverser.scoped;
const sem = parser.traverser.semantic;

const ScopeTree = sc.ScopeTree;
const Symbol = sem.Symbol;
const SymbolId = sem.SymbolId;
const SymbolTable = sem.SymbolTable;

const Allocator = std.mem.Allocator;

const MangleOptions = @import("options.zig").MangleOptions;

const NameSet = std.StringHashMapUnmanaged(void);

pub fn run(
    parent_allocator: Allocator,
    tree: *ast.Tree,
    scope_tree: ScopeTree,
    table: SymbolTable,
    opts: MangleOptions,
) Allocator.Error!void {
    if (table.symbols.len == 0) return;

    var arena = std.heap.ArenaAllocator.init(parent_allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const toplevel = opts.toplevel orelse tree.isModule();

    // mames the generator must never produce
    var fixed: NameSet = .empty;
    for (reserved_words) |kw| try fixed.put(a, kw, {});
    for (opts.reserved) |r| try fixed.put(a, r, {});
    var unresolved = table.iterUnresolved();
    while (unresolved.next()) |entry| try fixed.put(a, table.string(entry.reference.name), {});

    // count manglables per scope, reserve kept names along the way.
    const count_in_scope = try a.alloc(u32, scope_tree.scopes.len);
    @memset(count_in_scope, 0);
    var symbols = table.iterSymbols();
    while (symbols.next()) |entry| {
        const sym = entry.symbol;
        if (isManglable(sym, scope_tree.getScope(sym.scope), tree, toplevel, opts)) {
            count_in_scope[@intFromEnum(sym.scope)] += 1;
        } else {
            try fixed.put(a, table.string(sym.name), {});
        }
    }

    // ancestor_count[scope] = total manglable symbols in strict ancestors.
    // doubles as the running slot counter during the rename pass below.
    // scopes are pre-order, so every parent index is < the child's.
    const ancestor_count = try a.alloc(u32, scope_tree.scopes.len);
    ancestor_count[0] = 0;
    var name_count: u32 = count_in_scope[0];
    for (scope_tree.scopes[1..], 1..) |scope, i| {
        const pi = @intFromEnum(scope.parent);
        ancestor_count[i] = ancestor_count[pi] + count_in_scope[pi];
        const upper = ancestor_count[i] + count_in_scope[i];
        if (upper > name_count) name_count = upper;
    }

    // slot → ast.String
    const names = try a.alloc(ast.String, name_count);
    var gen = NameGen{};
    for (names) |*n| {
        while (true) {
            const cand = gen.next();
            if (fixed.contains(cand)) continue;
            n.* = try tree.addString(cand);
            break;
        }
    }

    // walk symbols, hand out slots, rewrite via sym.node and ref.node.
    symbols = table.iterSymbols();
    while (symbols.next()) |entry| {
        const sym = entry.symbol;
        if (!isManglable(sym, scope_tree.getScope(sym.scope), tree, toplevel, opts)) continue;

        const si = @intFromEnum(sym.scope);
        const slot = ancestor_count[si];
        ancestor_count[si] += 1;

        rename(tree, table, entry.id, sym, names[slot]);
    }
}

/// rewrites the binding and every reference of `sym` in place.
fn rename(tree: *ast.Tree, table: SymbolTable, sid: SymbolId, sym: Symbol, name: ast.String) void {
    if (sym.node != .null) {
        var bid = tree.data(sym.node).binding_identifier;
        bid.name = name;
        tree.setData(sym.node, .{ .binding_identifier = bid });
    }
    for (table.symbolReferences(sid)) |ref_id| {
        const ref = table.getReference(ref_id);
        tree.setData(ref.node, .{ .identifier_reference = .{ .name = name } });
    }
}

fn isManglable(sym: Symbol, scope: sc.Scope, tree: *const ast.Tree, toplevel: bool, opts: MangleOptions) bool {
    if (sym.flags.exported or sym.flags.is_default) return false;
    if (sym.flags.ambient) return false;

    switch (scope.kind) {
        .global => return false,
        .module => if (!toplevel) return false,
        else => {},
    }

    const name = tree.string(sym.name);
    if (std.mem.eql(u8, name, "arguments") or std.mem.eql(u8, name, "eval")) return false;

    if (sym.flags.function and opts.keep_fnames) return false;
    if (sym.flags.class and opts.keep_classnames) return false;

    return true;
}

const NameGen = struct {
    counter: u64 = 0,
    buf: [16]u8 = undefined,

    const head_alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$";
    const tail_alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$0123456789";

    fn next(self: *NameGen) []const u8 {
        var n = self.counter;
        self.counter += 1;

        const head_len: u64 = head_alpha.len;
        if (n < head_len) {
            self.buf[0] = head_alpha[n];
            return self.buf[0..1];
        }
        n -= head_len;

        const tail_len: u64 = tail_alpha.len;
        var len: usize = 1;
        var place: u64 = head_len * tail_len;
        while (n >= place) : (len += 1) {
            n -= place;
            place *= tail_len;
        }
        var rem = n;
        for (0..len) |i| {
            self.buf[len - i] = tail_alpha[rem % tail_len];
            rem /= tail_len;
        }
        self.buf[0] = head_alpha[rem % head_len];
        return self.buf[0 .. len + 1];
    }
};

const reserved_words = [_][]const u8{
    "as",         "async",  "await",      "break",      "case",      "catch",
    "class",      "const",  "continue",   "debugger",   "default",   "delete",
    "do",         "else",   "enum",       "export",     "extends",   "false",
    "finally",    "for",    "from",       "function",   "get",       "if",
    "implements", "import", "in",         "instanceof", "interface", "is",
    "let",        "new",    "null",       "of",         "package",   "private",
    "protected",  "public", "return",     "satisfies",  "set",       "static",
    "super",      "switch", "this",       "throw",      "true",      "try",
    "type",       "typeof", "var",        "void",       "while",     "with",
    "yield",
};
