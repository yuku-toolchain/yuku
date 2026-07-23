const std = @import("std");
const parser = @import("parser");
const ast = parser.ast;
const codegen = parser.codegen;

const Allocator = std.mem.Allocator;

pub const Mode = struct { lang: ast.Lang, source_type: ast.SourceType };

pub const modes = [_]Mode{
    .{ .lang = .js, .source_type = .module },
    .{ .lang = .js, .source_type = .script },
    .{ .lang = .js, .source_type = .commonjs },
    .{ .lang = .ts, .source_type = .module },
    .{ .lang = .ts, .source_type = .script },
    .{ .lang = .tsx, .source_type = .module },
    .{ .lang = .jsx, .source_type = .module },
    .{ .lang = .dts, .source_type = .module },
};

// adversarial fragments. surrogate and overlong escapes, unterminated literals,
// numeric and regex corners, raw wtf-8 bytes, and nesting openers.
pub const fragments = [_][]const u8{
    "'\\uD800",              "\"\\uDBFF",              "`\\uD800",
    "'\\uD800\\uDC00'",      "'\\u{",                  "'\\u{110000}'",
    "'\\u{}'",               "'\\u{0}'",               "'\\x",
    "'\\xGG'",               "'\\",                    "\"\\",
    "`\\",                   "`${",                    "`${`",
    "`${}`",                 "`${`${`x`}`}`",          "`\\u",
    "\\u{61}",               "a\\u0",                  "\\u",
    "0x",                    "0b",                     "0o",
    "1e",                    "1e+",                    "1_",
    "_1",                    "1n",                     "0.",
    ".5",                    ".5e3",                   "1__2",
    "12345678901234567890n", "0xFFFFFFFFFFFFFFFFFFFF", "/a",
    "/a/giuy",               "/[",                     "/\\",
    "/(?<",                  "a / b / c",              "let x:",
    "function f<T",          "type A=A&A&",            "x as ",
    "interface I{",          "enum E{",                "<a",
    "<a>",                   "</a>",                   "<a b=",
    "<a {...x}",             "<>",                     "</>",
    "class C{#x",            "async ",                 "yield ",
    "await ",                "...",                    "?.",
    "??=",                   "#!",                     "\x00",
    "\xEF\xBB\xBF",          "\xC3",                   "\xE2\x82",
    "\xF0\x9F\x98",          "\xED\xA0\x80",           "\xED\xB0\x80",
    "\xFF\xFE",
};

// structurally complete programs, so one mutation lands in deep parser state.
pub const seeds = [_][]const u8{
    "const x = 1 + 2 * 3;",
    "function f(a, b = 1, ...rest) { return a ? b : rest; }",
    "class C extends B { #p = 1; get x() { return this.#p; } }",
    "const s = `a${b}c${`d${e}f`}g`;",
    "const re = /ab+c/giu; const n = 1_000.5e-3;",
    "import a, { b as c } from 'm'; export default a;",
    "let x: Array<number> = [1, 2, 3] as const;",
    "type T<U> = U extends string ? A & B : C | D;",
    "const el = <div a={1} {...p}>{x}<C />text</div>;",
    "for (const [k, v] of Object.entries(o)) console.log(k, v);",
    "try { await f(); } catch (e) { throw e; } finally { g(); }",
    "const o = { a, b: 1, [c]: 2, ...d, m() {}, get g() { return 1; } };",
    "switch (x) { case 1: break; default: ; }",
    "outer: for (;;) { if (a) continue outer; else break outer; }",
    "const f = async function* () { yield* gen(); };",
    "'use strict';\nvar a = 0b1010, b = 0o17, c = 0xff, d = 100n;",
    "enum E { A, B = 2, C }\nnamespace N { export const x = 1; }",
    "abstract class A { abstract m(): void; private readonly x = 1; }",
    "const { a = 1, b: { c } = {}, ...rest } = obj;",
    "const u = '\\u{1F600}\\uD83D\\uDE00\\n\\t\\x41';",
};

// caps a runaway allocation as an attributable panic instead of a silent oom
// kill, so a non-advancing loop or overflowed size shows up as a finding.
pub const memory_cap = 512 * 1024 * 1024;

const Guard = struct {
    backing: Allocator,
    live: usize = 0,

    fn allocator(self: *Guard) Allocator {
        return .{ .ptr = self, .vtable = &.{
            .alloc = alloc,
            .resize = resize,
            .remap = remap,
            .free = free,
        } };
    }

    fn trip(self: *Guard, add: usize) void {
        if (self.live + add > memory_cap) std.debug.panic(
            "pathological allocation: {d} bytes live, +{d} exceeds the {d} byte cap",
            .{ self.live, add, memory_cap },
        );
    }

    fn alloc(ctx: *anyopaque, len: usize, a: std.mem.Alignment, ra: usize) ?[*]u8 {
        const self: *Guard = @ptrCast(@alignCast(ctx));
        self.trip(len);
        const p = self.backing.rawAlloc(len, a, ra) orelse return null;
        self.live += len;
        return p;
    }

    fn resize(ctx: *anyopaque, mem: []u8, a: std.mem.Alignment, new_len: usize, ra: usize) bool {
        const self: *Guard = @ptrCast(@alignCast(ctx));
        if (new_len > mem.len) self.trip(new_len - mem.len);
        if (!self.backing.rawResize(mem, a, new_len, ra)) return false;
        self.live = self.live - mem.len + new_len;
        return true;
    }

    fn remap(ctx: *anyopaque, mem: []u8, a: std.mem.Alignment, new_len: usize, ra: usize) ?[*]u8 {
        const self: *Guard = @ptrCast(@alignCast(ctx));
        if (new_len > mem.len) self.trip(new_len - mem.len);
        const p = self.backing.rawRemap(mem, a, new_len, ra) orelse return null;
        self.live = self.live - mem.len + new_len;
        return p;
    }

    fn free(ctx: *anyopaque, mem: []u8, a: std.mem.Alignment, ra: usize) void {
        const self: *Guard = @ptrCast(@alignCast(ctx));
        self.backing.rawFree(mem, a, ra);
        self.live -= mem.len;
    }
};

// inputs the fuzzer has caught, kept as fixed regressions.
pub const regressions = [_][]const u8{
    "'\\uD800", // high-surrogate escape at eof caused an oob read in the lexer
    "switch (x) { case \xa01: break; default", // lex error spun parseSwitchCases
    "'\\uD83D\\u{1F600}'", // astral \u{} after a high surrogate overflowed a u16
    "const x = 1 .awai", // member `.` on a bare int reprinted as a fraction dot
    "const el = <di<a>v\\u{61} a={1}/>", // jsx rewind inverted a lex-error span
    "let x = [1, 2, 3] as c\n< st", // (x as T) < y reprinted with < as type args
    "type T<U> = U extends string\r ? & B : C | D", // leading-& cond-type true arm
    "type T = import(a).X", // import-type with a non-string arg hit unreachable
    "import x = require(1)", // require() with a non-string arg hit unreachable
    "x as T\r[1, 2]", // member access on a cast reprinted as a type index T[1,2]
    "class C{async get x(){await 0}}", // async getter whose async the printer dropped
};

// parse one (input, mode) and assert the invariants. violations panic so the
// driver can dump the reproducer.
pub fn check(gpa: Allocator, src: []const u8, mode: Mode) void {
    var guard = Guard{ .backing = gpa };
    const a = guard.allocator();

    var tree = parser.parse(a, src, .{
        .lang = mode.lang,
        .source_type = mode.source_type,
    }) catch |e| switch (e) {
        error.OutOfMemory => return,
    };
    defer tree.deinit();

    checkSpans(&tree, src);

    // round-trip only the syntactically valid programs, captured before
    // semantic analysis folds its early-error diagnostics into the tree.
    const parse_clean = !tree.hasErrors();

    // exercise semantic analysis on every parse. it must never panic, and the
    // diagnostics it appends must stay within the source bounds.
    _ = parser.semantic.analyze(&tree) catch |e| switch (e) {
        error.OutOfMemory => return,
    };
    checkSpans(&tree, src);

    if (parse_clean) checkRoundTrip(a, &tree, mode, src);
}

fn checkSpans(tree: *const ast.Tree, src: []const u8) void {
    var i: usize = 0;
    while (i < tree.nodes.len) : (i += 1) {
        const sp = tree.span(@enumFromInt(@as(u32, @intCast(i))));
        if (sp.start > sp.end or sp.end > src.len) std.debug.panic(
            "node span out of bounds: node {d}/{d} is {d}..{d}, src.len {d}",
            .{ i, tree.nodes.len, sp.start, sp.end, src.len },
        );
    }
    for (tree.diagnostics.items) |d| {
        if (d.span.start > d.span.end or d.span.end > src.len) std.debug.panic(
            "diagnostic span out of bounds: {d}..{d}, src.len {d}",
            .{ d.span.start, d.span.end, src.len },
        );
    }
}

fn checkRoundTrip(gpa: Allocator, tree: *ast.Tree, mode: Mode, src: []const u8) void {
    var res = codegen.generate(gpa, tree, .{}) catch |e| switch (e) {
        error.OutOfMemory => return,
    };
    defer res.deinit(gpa);

    var reparsed = parser.parse(gpa, res.code, .{
        .lang = mode.lang,
        .source_type = mode.source_type,
    }) catch |e| switch (e) {
        error.OutOfMemory => return,
    };
    defer reparsed.deinit();

    if (reparsed.hasErrors()) std.debug.panic(
        "round trip: a clean parse printed to output that fails to reparse\n--- src ---\n{s}\n--- printed ---\n{s}",
        .{ src, res.code },
    );
}

// fail at each successive allocation. every point must yield error.OutOfMemory
// or success, never a panic. the arena absorbs any error-path leak, while the
// safety-checked build still traps a bad free.
pub fn oomSweep(src: []const u8, mode: Mode) void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var idx: usize = 0;
    while (idx < 1 << 18) : (idx += 1) {
        defer _ = arena.reset(.retain_capacity);

        var failing = std.testing.FailingAllocator.init(arena.allocator(), .{ .fail_index = idx });
        if (parser.parse(failing.allocator(), src, .{
            .lang = mode.lang,
            .source_type = mode.source_type,
        })) |tree| {
            var t = tree;
            defer t.deinit();
            // semantic analysis allocates too, and it must also yield OOM or
            // success at every failure point, never a panic.
            if (parser.semantic.analyze(&t)) |_| {
                // neither parse nor analysis induced a failure, so the fail
                // index is past the last allocation, so every point is covered.
                if (!failing.has_induced_failure) break;
            } else |e| switch (e) {
                error.OutOfMemory => {},
            }
        } else |e| switch (e) {
            error.OutOfMemory => {},
        }
    }
}

// rewrites buf from a random seed, weighted toward eof truncation and fragment
// injection (the two operators that find the most).
pub const Mutator = struct {
    rng: std.Random,
    max_len: usize = 64 * 1024,

    pub fn produce(self: *Mutator, buf: *std.ArrayList(u8), gpa: Allocator) Allocator.Error![]const u8 {
        buf.clearRetainingCapacity();
        try buf.appendSlice(gpa, seeds[self.rng.uintLessThan(usize, seeds.len)]);

        const rounds = self.rng.intRangeAtMost(usize, 1, 5);
        var r: usize = 0;
        while (r < rounds) : (r += 1) {
            try self.applyOne(buf, gpa);
            if (buf.items.len > self.max_len) buf.shrinkRetainingCapacity(self.max_len);
        }
        return buf.items;
    }

    fn applyOne(self: *Mutator, buf: *std.ArrayList(u8), gpa: Allocator) Allocator.Error!void {
        if (buf.items.len == 0) try buf.append(gpa, 'x');
        const len = buf.items.len;
        switch (self.rng.uintLessThan(usize, 13)) {
            // eof truncation, double-weighted, since most past bugs were token-at-eof.
            0, 1 => buf.shrinkRetainingCapacity(self.rng.intRangeAtMost(usize, 0, len)),
            // cut just past an escape backslash to strand an incomplete escape.
            2 => {
                if (std.mem.lastIndexOfScalar(u8, buf.items, '\\')) |bs| {
                    const keep = @min(buf.items.len, bs + 1 + self.rng.uintLessThan(usize, 5));
                    buf.shrinkRetainingCapacity(keep);
                }
            },
            // inject an adversarial fragment.
            3, 4 => try buf.insertSlice(gpa, self.rng.intRangeAtMost(usize, 0, len), self.fragment()),
            // append a fragment then truncate inside it (escape/template at eof).
            5 => {
                const f = self.fragment();
                try buf.appendSlice(gpa, f);
                buf.shrinkRetainingCapacity(buf.items.len - self.rng.intRangeAtMost(usize, 0, f.len));
            },
            // overwrite a byte with an interesting one.
            6 => buf.items[self.rng.uintLessThan(usize, len)] = self.interesting(),
            // bit flip.
            7 => buf.items[self.rng.uintLessThan(usize, len)] ^= @as(u8, 1) << self.rng.int(u3),
            // insert an interesting byte.
            8 => try buf.insert(gpa, self.rng.intRangeAtMost(usize, 0, len), self.interesting()),
            // delete a span.
            9 => {
                const a = self.rng.uintLessThan(usize, len);
                const b = self.rng.intRangeAtMost(usize, a, len);
                buf.replaceRangeAssumeCapacity(a, b - a, &.{});
            },
            // duplicate a span to stress repetition and growth.
            10 => {
                const a = self.rng.uintLessThan(usize, len);
                const b = self.rng.intRangeAtMost(usize, a, len);
                const dup = try gpa.dupe(u8, buf.items[a..b]);
                defer gpa.free(dup);
                try buf.insertSlice(gpa, self.rng.intRangeAtMost(usize, 0, buf.items.len), dup);
            },
            // nesting bomb. a parser must reject depth, never overflow the stack.
            11, 12 => {
                const openers = [_][]const u8{ "(", "[", "{", "`${", "/", "a&", "a?b:", "typeof " };
                const op = openers[self.rng.uintLessThan(usize, openers.len)];
                const count = self.rng.intRangeAtMost(usize, 1, 400);
                var prefix: std.ArrayList(u8) = .empty;
                defer prefix.deinit(gpa);
                var k: usize = 0;
                while (k < count) : (k += 1) try prefix.appendSlice(gpa, op);
                try buf.insertSlice(gpa, 0, prefix.items);
            },
            else => unreachable,
        }
    }

    fn fragment(self: *Mutator) []const u8 {
        return fragments[self.rng.uintLessThan(usize, fragments.len)];
    }

    fn interesting(self: *Mutator) u8 {
        const bytes = [_]u8{
            0x00, 0x09, 0x0a, 0x0d, ' ',  '\\', '\'', '"', '`', '$', '{',  '}',
            '(',  ')',  '[',  ']',  '<',  '>',  '/',  '*', '+', '-', '=',  '.',
            ',',  ';',  ':',  '!',  '?',  'u',  'x',  'n', 'e', '0', 0x80, 0xc3,
            0xe2, 0xf0, 0xff, 0xed, 0xa0, 0xbf,
        };
        return bytes[self.rng.uintLessThan(usize, bytes.len)];
    }
};
