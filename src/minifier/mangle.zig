const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const sc = parser.traverser.scoped;
const sem = parser.traverser.semantic;

const Allocator = std.mem.Allocator;
const MangleOptions = @import("options.zig").MangleOptions;
const NameSeq = @import("name_seq.zig").NameSeq;

const NameSet = std.StringHashMapUnmanaged(void);
const SlotCache = std.AutoArrayHashMapUnmanaged(u32, ast.String);
const OuterRefList = std.ArrayList(sem.SymbolId);

/// Sentinel for "no slot assigned" / "symbol left un-renamed".
const NO_SLOT: u32 = std.math.maxInt(u32);

/// Names a slot must avoid (reserved words, free vars, kept originals) all
/// sit within the first few thousand entries of the name sequence.
const FORBIDDEN_SCAN_LIMIT: u32 = 4096;

pub fn run(
    allocator: Allocator,
    tree: *ast.Tree,
    scope_tree: sc.ScopeTree,
    table: sem.SymbolTable,
    opts: MangleOptions,
) Allocator.Error!void {
    std.debug.assert(tree.root != .null);
    if (table.symbols.len == 0) return;
    std.debug.assert(scope_tree.scopes.len > 0);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var m: Mangler = .{
        .a = arena.allocator(),
        .tree = tree,
        .scope_tree = scope_tree,
        .table = table,
        .opts = opts,
        .toplevel = opts.toplevel orelse tree.isModule(),
    };
    try m.collectForbiddenNames();
    try m.collectOuterRefs();
    try m.assignAndRewrite();
}

const Mangler = struct {
    // inputs
    a: Allocator,
    tree: *ast.Tree,
    scope_tree: sc.ScopeTree,
    table: sem.SymbolTable,
    opts: MangleOptions,
    toplevel: bool,

    // built by `collectForbiddenNames`.
    forbidden: NameSet = .empty,

    // built by `collectOuterRefs`. `outer_refs[s]` lists symbol ids declared
    // in a strict ancestor of scope `s` and used somewhere in subtree(s).
    outer_refs: []OuterRefList = &.{},

    // built / consumed by `assignAndRewrite`.
    reserved: []u8 = &.{},
    slot_of: []u32 = &.{},
    slot_cache: SlotCache = .empty,
    scratch: std.ArrayList(u32) = .empty,
    locals: std.ArrayList(Local) = .empty,
    name_buf: [16]u8 = undefined,

    /// Names no slot may produce, keywords, the user reserved list,
    /// free-variable references, and the literal names of every un-manglable
    /// symbol (they keep their original name in the output).
    fn collectForbiddenNames(m: *Mangler) !void {
        for (reserved_words) |kw| try m.forbidden.put(m.a, kw, {});
        for (m.opts.reserved) |r| try m.forbidden.put(m.a, r, {});

        var unresolved = m.table.iterUnresolved();
        while (unresolved.next()) |e|
            try m.forbidden.put(m.a, m.tree.string(e.reference.name), {});

        var symbols = m.table.iterSymbols();
        while (symbols.next()) |e| {
            const scope = m.scope_tree.getScope(e.symbol.scope);
            if (!m.isManglable(e.symbol, scope))
                try m.forbidden.put(m.a, m.tree.string(e.symbol.name), {});
        }
    }

    /// For each reference, walk from its use scope up the ancestor chain to
    /// (but not including) the declaration's scope, appending the symbol id
    /// at every step. Duplicates are harmless, pass 2's bitmap is idempotent.
    fn collectOuterRefs(m: *Mangler) !void {
        m.outer_refs = try m.a.alloc(OuterRefList, m.scope_tree.scopes.len);
        for (m.outer_refs) |*r| r.* = .empty;

        for (m.table.references, 0..) |ref, i| {
            const sid = m.table.referenceSymbol(@enumFromInt(@as(u32, @intCast(i))));
            if (sid == .none) continue;
            const decl_scope = m.table.symbols[@intFromEnum(sid)].scope;

            var ancestors = m.scope_tree.ancestors(ref.scope);
            while (ancestors.next()) |scope_id| {
                if (scope_id == decl_scope) break;
                try m.outer_refs[@intFromEnum(scope_id)].append(m.a, sid);
            }
        }
    }

    /// Walk scopes pre-order (ids are issued pre-order, so iterating the scope
    /// array visits each parent before its children). Each scope:
    ///   1. Reserves slots held by outer-referenced symbols.
    ///   2. Hands out the lowest free slot to each local in use-count order.
    ///   3. Rolls back its scope-local reservations via `defer`.
    fn assignAndRewrite(m: *Mangler) !void {
        std.debug.assert(m.outer_refs.len == m.scope_tree.scopes.len);
        const sym_count: u32 = @intCast(m.table.symbols.len);
        std.debug.assert(sym_count > 0);

        // bitmap of forbidden + scope-local reservations. forbidden bits set
        // once and never cleared.
        m.reserved = try m.a.alloc(u8, @max(sym_count + 256, FORBIDDEN_SCAN_LIMIT));
        @memset(m.reserved, 0);
        for (0..FORBIDDEN_SCAN_LIMIT) |k| {
            const name = NameSeq.nameAt(@intCast(k), &m.name_buf);
            if (m.forbidden.contains(name)) m.reserved[k] = 1;
        }

        try m.tree.ensureUnusedStringCapacity(sym_count *| 4, sym_count);

        m.slot_of = try m.a.alloc(u32, sym_count);
        @memset(m.slot_of, NO_SLOT);

        for (m.scope_tree.scopes, 0..) |scope, idx| {
            const scratch_base = m.scratch.items.len;
            defer {
                for (m.scratch.items[scratch_base..]) |s| m.reserved[s] = 0;
                m.scratch.shrinkRetainingCapacity(scratch_base);
            }

            for (m.outer_refs[idx].items) |sid| {
                const s = m.slot_of[@intFromEnum(sid)];
                if (s != NO_SLOT and m.reserved[s] == 0) {
                    m.reserved[s] = 1;
                    try m.scratch.append(m.a, s);
                }
            }

            try m.collectLocals(@enumFromInt(@as(u32, @intCast(idx))), scope);
            if (m.locals.items.len == 0) continue;
            std.mem.sort(Local, m.locals.items, {}, moreUsedFirst);

            var slot: u32 = 0;
            for (m.locals.items) |local| {
                while (m.reserved[slot] == 1) slot += 1;
                m.reserved[slot] = 1;
                try m.scratch.append(m.a, slot);
                m.slot_of[@intFromEnum(local.sid)] = slot;
                try m.rewriteSites(local.sid, slot);
                slot += 1;
            }
        }
    }

    fn collectLocals(m: *Mangler, scope_id: sc.ScopeId, scope: sc.Scope) !void {
        m.locals.clearRetainingCapacity();
        var syms = m.table.scopeSymbols(scope_id);
        while (syms.next()) |sid_ptr| {
            const sid = sid_ptr.*;
            const sym = m.table.symbols[@intFromEnum(sid)];
            if (m.isManglable(sym, scope))
                try m.locals.append(m.a, .{ .sid = sid, .uses = m.table.symbolUsesCount(sid) });
        }
    }

    fn rewriteSites(m: *Mangler, sid: sem.SymbolId, slot: u32) !void {
        std.debug.assert(sid != .none);
        std.debug.assert(slot != NO_SLOT);
        const gop = try m.slot_cache.getOrPut(m.a, slot);
        if (!gop.found_existing)
            gop.value_ptr.* = try m.tree.addString(NameSeq.nameAt(slot, &m.name_buf));
        var sites = m.table.symbolSites(sid);
        while (sites.next()) |node| m.tree.setIdentifierName(node, gop.value_ptr.*);
    }

    fn isManglable(m: *const Mangler, sym: sem.Symbol, scope: sc.Scope) bool {
        if (sym.flags.exported or sym.flags.is_default) return false;
        if (sym.flags.ambient) return false;

        switch (scope.kind) {
            .global => return false,
            .module => if (!m.toplevel) return false,
            else => {},
        }

        const name = m.tree.string(sym.name);
        if (std.mem.eql(u8, name, "arguments") or std.mem.eql(u8, name, "eval")) return false;

        if (sym.flags.function and m.opts.keep_fnames) return false;
        if (sym.flags.class and m.opts.keep_classnames) return false;

        return true;
    }
};

const Local = struct {
    sid: sem.SymbolId,
    uses: u32,
};

fn moreUsedFirst(_: void, x: Local, y: Local) bool {
    if (x.uses != y.uses) return x.uses > y.uses;
    // stable tie-break by id keeps output deterministic.
    return @intFromEnum(x.sid) < @intFromEnum(y.sid);
}

const reserved_words = [_][]const u8{
    "as",         "async",  "await",    "break",      "case",      "catch",
    "class",      "const",  "continue", "debugger",   "default",   "delete",
    "do",         "else",   "enum",     "export",     "extends",   "false",
    "finally",    "for",    "from",     "function",   "get",       "if",
    "implements", "import", "in",       "instanceof", "interface", "is",
    "let",        "new",    "null",     "of",         "package",   "private",
    "protected",  "public", "return",   "satisfies",  "set",       "static",
    "super",      "switch", "this",     "throw",      "true",      "try",
    "type",       "typeof", "var",      "void",       "while",     "with",
    "yield",
};
