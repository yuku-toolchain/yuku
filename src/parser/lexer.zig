const std = @import("std");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const Span = @import("token.zig").Span;
const TokenFlag = @import("token.zig").TokenFlag;
const flagMask = @import("token.zig").flagMask;
const keywordTag = @import("token.zig").keywordTag;
const keywordTagAt = @import("token.zig").keywordTagAt;
const keyword_read_len = @import("token.zig").keyword_read_len;
const keyword_length_max = @import("token.zig").keyword_length_max;
const ast = @import("ast.zig");
const lexical = @import("lexical.zig");
const util = @import("util");
const spec = @import("spec");
const Simd = util.Simd;

pub const LexicalError = error{
    UnterminatedString,
    NonTerminatedTemplateLiteral,
    UnterminatedRegexLiteral,
    InvalidRegexLineTerminator,
    InvalidIdentifierStart,
    InvalidIdentifierContinue,
    UnterminatedMultiLineComment,
    InvalidUnicodeEscape,
    InvalidHexEscape,
    InvalidOctalEscape,
    InvalidOctalLiteralDigit,
    InvalidBinaryLiteral,
    InvalidHexLiteral,
    InvalidExponentPart,
    NumericSeparatorMisuse,
    ConsecutiveNumericSeparators,
    ExponentialBigInt,
    NonIntegerBigInt,
    IdentifierAfterNumericLiteral,
    InvalidUtf8,
    OutOfMemory,
    JsxIdentifierCannotContainEscapes,
    JsxIdentifierCannotStartWithBackslash,
};

pub const LexerMode = enum {
    normal,
    /// Inside a JSX tag, where identifiers may contain hyphens, escapes are disabled,
    /// strings may span lines, and `>` is always a single token.
    jsx_tag,
};

inline fn isIdentifierChunk(chunk: Simd.Chunk) @Vector(16, bool) {
    if (Simd.ByteSet.is_fast and !@inComptime()) return identifier_bytes.contains(chunk);
    const lower = chunk | Simd.splat(0x20);
    const alpha = (lower -% Simd.splat('a')) < Simd.splat(26);
    const digit = (chunk -% Simd.splat('0')) < Simd.splat(10);
    const underscore = chunk == Simd.splat('_');
    const dollar = chunk == Simd.splat('$');
    return alpha | digit | underscore | dollar;
}

const identifier_start_ascii = asciiTable(lexical.isIdentifierStart);
const identifier_char_ascii = asciiTable(lexical.isIdentifierChar);
const identifier_bytes: Simd.ByteSet = .init(identifier_char_ascii);
const identifier_char_jsx_ascii = asciiTable(lexical.isJSXIdentifierChar);

fn asciiTable(comptime predicate: fn (u21) bool) [256]bool {
    @setEvalBranchQuota(10_000);
    var table: [256]bool = @splat(false);
    for (0..0x80) |c| table[c] = predicate(c);
    return table;
}

comptime {
    @setEvalBranchQuota(20_000);
    for (0..256) |c| {
        const lanes = isIdentifierChunk(Simd.splat(c));
        std.debug.assert(@reduce(.And, lanes) == identifier_char_ascii[c]);
        std.debug.assert(@reduce(.Or, lanes) == identifier_char_ascii[c]);
    }
}

fn findAnyPos(comptime chars: []const u8, src: []const u8, from: u32) u32 {
    var i: usize = from;
    while (i + 16 <= src.len) : (i += 16) {
        const chunk = Simd.loadChunk(src, i);
        var hit: @Vector(16, bool) = @splat(false);
        inline for (chars) |ch| hit = hit | (chunk == Simd.splat(ch));
        const first = Simd.firstTrueLane(hit);
        if (first < 16) return @intCast(i + first);
    }
    while (i < src.len) : (i += 1) {
        inline for (chars) |ch| {
            if (src[i] == ch) return @intCast(i);
        }
    }
    return @intCast(src.len);
}

pub const LexerState = struct {
    /// Flags attached to the next emitted token.
    token_flags: u8 = 0,
};

const IdentifierBlocks = struct {
    start: u32 = std.math.maxInt(u32),
    first: u64 = 0,
    second: u64 = 0,

    const start_mask = ~@as(u32, 63);
};

pub const Lexer = struct {
    /// Comments in source order, populated only when `collect_comments` is set.
    comments: std.ArrayList(ast.Comment),
    collect_comments: bool,
    allocator: std.mem.Allocator,
    state: LexerState,
    mode: LexerMode = .normal,

    source: []const u8,
    cursor: u32,
    fast_limit: u32,
    fast_end: u32,
    fast_open: bool = true,
    blocks: IdentifierBlocks,

    source_type: ast.SourceType,
    hashbang: ?struct { start: u32, len: u32 } = null,

    const fast_margin = 80;

    pub fn init(
        source: []const u8,
        allocator: std.mem.Allocator,
        source_type: ast.SourceType,
        collect_comments: bool,
    ) Lexer {
        std.debug.assert(source.len <= std.math.maxInt(u32));
        var self: Lexer = .{
            .source = source,
            .state = .{},
            .cursor = 0,
            .fast_limit = @intCast(source.len -| fast_margin),
            .fast_end = @intCast(source.len -| fast_margin),
            .blocks = .{},
            .comments = .empty,
            .collect_comments = collect_comments,
            .allocator = allocator,
            .source_type = source_type,
        };
        self.skipHashbang();
        std.debug.assert(self.cursor <= self.source.len);
        return self;
    }

    pub inline fn setMode(self: *Lexer, mode: LexerMode) void {
        self.mode = mode;
        self.fast_limit = if (mode == .normal) @intCast(self.source.len -| fast_margin) else 0;
        self.fast_end = if (self.fast_open) self.fast_limit else 0;
    }

    pub inline fn peekByte(self: *const Lexer) u8 {
        const src = self.source;
        var position: usize = self.cursor;
        while (position < src.len and src[position] == ' ') position += 1;
        if (position >= src.len) return 0;
        const byte = src[position];
        return if (starts_token[byte]) byte else 0;
    }

    const starts_token: [256]bool = blk: {
        var t: [256]bool = @splat(false);
        for (0x21..0x7F) |c| t[c] = true;
        for ("/<-\\") |c| t[c] = false;
        break :blk t;
    };

    pub fn setFastOpen(self: *Lexer, open: bool) void {
        self.fast_open = open;
        self.fast_end = if (open) self.fast_limit else 0;
    }

    fn skipHashbang(self: *Lexer) void {
        std.debug.assert(self.cursor == 0);
        if (self.source.len >= 2 and self.source[0] == '#' and self.source[1] == '!') {
            const end = findAnyPos("\n\r", self.source, 2);
            std.debug.assert(end >= 2);
            std.debug.assert(end <= self.source.len);
            self.hashbang = .{ .start = 2, .len = end - 2 };
            self.cursor = end;
        }
    }

    /// `nextToken`, written into `out`.
    pub fn nextTokenInto(self: *Lexer, out: *Token) LexicalError!void {
        // avoids a stack round trip
        out.* = try @call(.always_inline, nextToken, .{self});
    }

    pub fn nextToken(self: *Lexer) LexicalError!Token {
        std.debug.assert(self.cursor <= self.source.len);
        self.clearTokenFlags();
        try self.skipTrivia();
        std.debug.assert(self.cursor <= self.source.len);

        if (self.cursor >= self.source.len) {
            return self.createToken(.eof, self.cursor, self.cursor);
        }

        const current_char = self.source[self.cursor];

        if (identifier_start_ascii[current_char] and self.mode == .normal) {
            var token: Token = undefined;
            if (self.scanAsciiIdentifier(&token)) return token;
            return self.scanIdentifierOrKeyword();
        }

        return switch (current_char) {
            '+',
            '*',
            '-',
            '!',
            '<',
            '>',
            '=',
            '|',
            '&',
            '^',
            '%',
            '/',
            '?',
            => self.scanPunctuation(),
            '.' => self.scanDot(),
            '0'...'9' => try self.scanNumber(),
            '"', '\'' => self.scanString(),
            '`' => self.scanTemplateLiteral(),
            '~', '(', ')', '{', '}', '[', ']', ';', ',', ':', '@' => self.scanSimplePunctuation(),
            else => self.scanIdentifierOrKeyword(),
        };
    }

    pub inline fn tryNextToken(self: *Lexer, out: *Token) bool {
        if (self.cursor >= self.fast_end) return false;
        return self.scanFast(out);
    }

    pub inline fn tryNextTokenAlways(self: *Lexer, out: *Token) bool {
        if (self.cursor >= self.fast_limit) return false;
        return self.scanFast(out);
    }

    inline fn scanFast(self: *Lexer, out: *Token) bool {
        var start = self.cursor;
        std.debug.assert(start < self.fast_limit);
        std.debug.assert(self.mode == .normal);
        std.debug.assert(start + fast_margin <= self.source.len);

        const src = self.source.ptr;
        var c = src[start];
        if (c == ' ') {
            start += 1;
            c = src[start];
        }

        return self.scanTokenAt(start, 0, c, out);
    }

    inline fn scanTokenAt(self: *Lexer, at: u32, flags_before: u8, first: u8, out: *Token) bool {
        const src = self.source.ptr;
        var start = at;
        var flags = flags_before;
        var c = first;
        while (true) {
            const info = &byte_info[c];
            const class = info.class;
            if (class & Class.identifier != 0) return self.scanIdentifierFast(start, flags, out);
            if (class & (Class.punctuation | Class.operator) != 0) {
                const end = start + 1;
                if (class & Class.operator != 0 and
                    byte_info[src[end]].group & info.continued_by != 0)
                {
                    return self.scanOperatorFast(start, flags, out);
                }
                self.cursor = end;
                writeToken(out, start, end, info.tag, flags);
                return true;
            }
            if (class & Class.digit != 0) return self.scanDecimalFast(start, flags, out);
            if (class & Class.quote != 0) return self.scanStringFast(start, flags, out);
            if (class & Class.line_break == 0) return false;

            start = self.skipLineBreaks(start);
            if (start == 0) return false;
            flags = flagMask(.line_terminator_before);
            c = src[start];
        }
    }

    noinline fn skipLineBreaks(self: *const Lexer, from: u32) u32 {
        const src = self.source.ptr;
        std.debug.assert(src[from] == '\n' or src[from] == '\r');
        var position = from;
        while (true) {
            position = self.skipIndentation(position + 1) orelse return 0;
            if (byte_info[src[position]].class & Class.line_break == 0) return position;
        }
    }

    inline fn writeToken(out: *Token, start: u32, end: u32, tag: TokenTag, flags: u8) void {
        comptime {
            std.debug.assert(@offsetOf(Token, "span") == 0 and @offsetOf(Token, "tag") == 8);
            std.debug.assert(@offsetOf(Token, "flags") == 12 and @sizeOf(Token) == 16);
            std.debug.assert(@import("builtin").target.cpu.arch.endian() == .little);
        }
        const span: u64 = @as(u64, start) | @as(u64, end) << 32;
        const rest: u64 = @as(u64, @backingInt(tag)) | @as(u64, flags) << 32;
        const words: *align(@alignOf(Token)) u128 = @ptrCast(out);
        words.* = @as(u128, span) | @as(u128, rest) << 64;
    }

    inline fn skipIndentation(self: *const Lexer, from: u32) ?u32 {
        const src = self.source.ptr;
        var position = from;
        while (position < self.fast_limit) : (position += 8) {
            const word = std.mem.readInt(u64, src[position..][0..8], .little);
            const other = nonZeroBytes(word ^ repeated(' ')) & nonZeroBytes(word ^ repeated('\t'));
            if (other != 0) return position + (@ctz(other) >> 3);
        }
        return null;
    }

    inline fn repeated(byte: u8) u64 {
        return @bitCast(@as([8]u8, @splat(byte)));
    }

    inline fn nonZeroBytes(word: u64) u64 {
        const low = repeated(0x7F);
        return (((word & low) +% low) | word) & repeated(0x80);
    }

    inline fn scanIdentifierFast(self: *Lexer, start: u32, flags: u8, out: *Token) bool {
        if (start & IdentifierBlocks.start_mask != self.blocks.start) return self.scanIdentifierReloaded(start, flags, out);
        return self.scanIdentifierInBlocks(start, flags, out);
    }

    noinline fn scanIdentifierReloaded(self: *Lexer, start: u32, flags: u8, out: *Token) bool {
        @call(.always_inline, loadBlocks, .{ self, start });
        return self.scanIdentifierInBlocks(start, flags, out);
    }

    inline fn scanIdentifierInBlocks(self: *Lexer, start: u32, flags: u8, out: *Token) bool {
        const src = self.source.ptr;
        std.debug.assert(identifier_start_ascii[src[start]]);
        std.debug.assert(start & IdentifierBlocks.start_mask == self.blocks.start);

        const len = self.identifierRunInBlocks(start);
        const end = start + len;
        std.debug.assert(len > 0);
        std.debug.assert(end < self.source.len);
        const after = src[end];
        if (byte_info[after].class & Class.identifier_part != 0) return false;

        self.cursor = end;
        writeToken(out, start, end, keywordTagAt(src + start, len), flags);
        return true;
    }

    const identifier_run_max = 64;

    inline fn identifierRun(self: *Lexer, position: u32) u32 {
        if (position & IdentifierBlocks.start_mask != self.blocks.start) self.loadBlocks(position);
        return self.identifierRunInBlocks(position);
    }

    inline fn identifierRunInBlocks(self: *const Lexer, position: u32) u32 {
        const shift: u6 = @truncate(position);
        const stops = ~self.blocks.first >> shift;
        if (stops == 0) {
            @branchHint(.unlikely);
            const len = 64 - @as(u32, shift) + @as(u32, @ctz(~self.blocks.second));
            return @min(len, identifier_run_max);
        }
        return @ctz(stops);
    }

    fn loadBlocks(self: *Lexer, position: u32) void {
        const start = position & IdentifierBlocks.start_mask;
        const first = if (start == @as(u64, self.blocks.start) + 64)
            self.blocks.second
        else
            self.identifierBlock(start);
        self.blocks = .{
            .start = start,
            .first = first,
            .second = self.identifierBlock(@as(u64, start) + 64),
        };
    }

    inline fn identifierBlock(self: *const Lexer, start: u64) u64 {
        const src = self.source;
        if (start + 64 > src.len) {
            @branchHint(.unlikely);
            var tail: [64]u8 = @splat(0);
            if (start < src.len) {
                const rest = src[@intCast(start)..];
                @memcpy(tail[0..rest.len], rest);
            }
            return Simd.mask64(&tail, isIdentifierChunk);
        }
        return Simd.mask64(src[@intCast(start)..][0..64], isIdentifierChunk);
    }

    fn scanAsciiIdentifier(self: *Lexer, out: *Token) bool {
        const src = self.source;
        const start = self.cursor;
        std.debug.assert(identifier_start_ascii[src[start]]);

        const len = self.identifierRun(start);
        const end = start + len;
        std.debug.assert(len > 0);
        std.debug.assert(end <= src.len);
        if (len == identifier_run_max) return false;
        if (end < src.len and (src[end] == '\\' or src[end] >= 0x80)) return false;

        const tag = if (@as(usize, start) + keyword_read_len <= src.len)
            keywordTagAt(src.ptr + start, len)
        else
            keywordTag(src[start..end]);
        self.cursor = end;
        out.* = self.createToken(tag, start, end);
        return true;
    }

    noinline fn scanOperatorFast(self: *Lexer, start: u32, flags: u8, out: *Token) bool {
        const src = self.source.ptr;
        const next = src[start + 1];
        switch (src[start]) {
            '/' => if (next == '/' or next == '*') return self.scanAfterComments(start, flags, out),
            '<' => if (next == '!') return false,
            '-' => if (next == '-') return false,
            '.' => return false,
            else => {},
        }
        self.cursor = start;
        self.state.token_flags = flags;
        out.* = self.scanPunctuation();
        return true;
    }

    fn scanAfterComments(self: *Lexer, start: u32, flags_before: u8, out: *Token) bool {
        const src = self.source.ptr;
        const entry = self.cursor;
        const comments_len = self.comments.items.len;
        var flags = flags_before;
        var position = start;

        const reached_token = comments: while (true) {
            std.debug.assert(src[position] == '/');
            self.cursor = position;
            self.clearTokenFlags();
            if (src[position + 1] == '/') {
                self.scanLineComment() catch break :comments false;
            } else {
                self.scanBlockComment() catch break :comments false;
                if (self.hasTokenFlag(.line_terminator_before)) {
                    flags = flagMask(.line_terminator_before);
                }
            }
            position = self.cursor;

            while (true) {
                if (position >= self.fast_limit) break :comments false;
                const c = src[position];
                if (byte_info[c].class & Class.line_break != 0) {
                    position = self.skipLineBreaks(position);
                    if (position == 0) break :comments false;
                    flags = flagMask(.line_terminator_before);
                } else if (c == ' ' or c == '\t') {
                    position = self.skipIndentation(position) orelse break :comments false;
                } else break;
            }
            const next = src[position + 1];
            if (src[position] != '/' or (next != '/' and next != '*')) break :comments true;
        };

        self.clearTokenFlags();
        if (reached_token and self.scanTokenAt(position, flags, src[position], out)) return true;

        self.cursor = entry;
        self.clearTokenFlags();
        self.comments.shrinkRetainingCapacity(comments_len);
        return false;
    }

    noinline fn scanDecimalFast(self: *Lexer, start: u32, flags: u8, out: *Token) bool {
        const src = self.source.ptr;
        std.debug.assert(std.ascii.isDigit(src[start]));

        const limit = start + decimal_fast_len_max;
        var end = start + 1;
        while (end < limit and std.ascii.isDigit(src[end])) end += 1;
        std.debug.assert(end < self.source.len);
        const after = src[end];
        if (after == '.' or byte_info[after].class & Class.identifier_part != 0) return false;
        if (src[start] == '0' and end - start > 1) return false;

        self.cursor = end;
        writeToken(out, start, end, .numeric_literal, flags);
        return true;
    }

    const decimal_fast_len_max = 32;

    noinline fn scanStringFast(self: *Lexer, start: u32, flags: u8, out: *Token) bool {
        const entry = self.cursor;
        self.cursor = start;
        self.state.token_flags = flags;
        out.* = self.scanString() catch {
            self.cursor = entry;
            self.clearTokenFlags();
            return false;
        };
        return true;
    }

    const Class = struct {
        const identifier: u8 = 1 << 0;
        const punctuation: u8 = 1 << 1;
        const operator: u8 = 1 << 2;
        const digit: u8 = 1 << 3;
        const quote: u8 = 1 << 4;
        const line_break: u8 = 1 << 6;
        const identifier_part: u8 = 1 << 5;
    };

    const Group = struct {
        const equals: u8 = 1 << 0;
        const operator: u8 = 1 << 1;
        const dot: u8 = 1 << 2;
        const digit: u8 = 1 << 3;
    };

    const ByteInfo = extern struct {
        tag: TokenTag = .eof,
        class: u8 = 0,
        group: u8 = 0,
        continued_by: u8 = 0,
        _pad: u8 = 0,
    };

    const byte_info: [256]ByteInfo = blk: {
        @setEvalBranchQuota(10_000);
        var t: [256]ByteInfo = @splat(.{});
        for (0..256) |c| {
            if (identifier_start_ascii[c]) t[c].class |= Class.identifier;
            if (identifier_char_ascii[c] or c == '\\' or c >= 0x80) {
                t[c].class |= Class.identifier_part;
            }
            if (std.ascii.isDigit(c)) {
                t[c].class |= Class.digit;
                t[c].group = Group.digit;
            }
        }
        t['\n'].class = Class.line_break;
        t['\r'].class = Class.line_break;
        t['"'].class = Class.quote;
        t['\''].class = Class.quote;

        const punctuation = [_]struct { u8, TokenTag }{
            .{ '~', .bitwise_not },   .{ '(', .left_paren },  .{ ')', .right_paren },
            .{ '{', .left_brace },    .{ '}', .right_brace }, .{ '[', .left_bracket },
            .{ ']', .right_bracket }, .{ ';', .semicolon },   .{ ',', .comma },
            .{ ':', .colon },         .{ '@', .at },
        };
        for (punctuation) |entry| {
            t[entry[0]].class = Class.punctuation;
            t[entry[0]].tag = entry[1];
        }

        const repeats = Group.equals | Group.operator;
        const operators = [_]struct { u8, TokenTag, u8 }{
            .{ '=', .assign, repeats },                      .{ '+', .plus, repeats },
            .{ '-', .minus, repeats },                       .{ '*', .star, repeats },
            .{ '/', .slash, repeats },                       .{ '&', .bitwise_and, repeats },
            .{ '|', .bitwise_or, repeats },                  .{ '<', .less_than, repeats },
            .{ '>', .greater_than, repeats },                .{ '!', .logical_not, Group.equals },
            .{ '%', .percent, Group.equals },                .{ '^', .bitwise_xor, Group.equals },
            .{ '?', .question, Group.operator | Group.dot }, .{ '.', .dot, Group.dot | Group.digit },
        };
        for (operators) |entry| {
            t[entry[0]].class = Class.operator;
            t[entry[0]].tag = entry[1];
            t[entry[0]].continued_by = entry[2];
        }
        t['='].group = Group.equals;
        t['.'].group = Group.dot;
        for ("<>&|?+-*/!") |c| t[c].group = Group.operator;
        break :blk t;
    };

    comptime {
        std.debug.assert(@sizeOf(ByteInfo) == 8);
        for ([_][]const u8{
            "++", "+=", "--", "-=", "**", "*=", "/=", "%=", "<<", "<=", ">>", ">=", "==",
            "=>", "!=", "&&", "&=", "||", "|=", "^=", "??", "?.", "..", "//", "/*", "<!",
            ".5",
        }) |operator| {
            const first = byte_info[operator[0]];
            std.debug.assert(first.class == Class.operator);
            std.debug.assert(byte_info[operator[1]].group & first.continued_by != 0);
        }
    }

    inline fn scanSimplePunctuation(self: *Lexer) Token {
        std.debug.assert(self.cursor < self.source.len);

        const start = self.cursor;
        const info = byte_info[self.source[start]];
        std.debug.assert(info.class == Class.punctuation);
        self.cursor = start + 1;

        return self.createToken(info.tag, start, start + 1);
    }

    inline fn puncToken(self: *Lexer, len: u32, tag: TokenTag, start: u32) Token {
        std.debug.assert(len > 0);
        std.debug.assert(start == self.cursor);
        std.debug.assert(start + len <= self.source.len);

        self.cursor += len;
        return self.createToken(tag, start, self.cursor);
    }

    fn scanPunctuation(self: *Lexer) Token {
        std.debug.assert(self.cursor < self.source.len);

        const start = self.cursor;
        const c0 = self.source[self.cursor];
        const c1 = self.peek(1);
        const c2 = self.peek(2);
        const c3 = self.peek(3);

        std.debug.assert(switch (c0) {
            '+', '-', '*', '/', '%', '<', '>', '=', '!', '&', '|', '^', '?' => true,
            else => false,
        });

        return switch (c0) {
            '+' => switch (c1) {
                '+' => self.puncToken(2, .increment, start),
                '=' => self.puncToken(2, .plus_assign, start),
                else => self.puncToken(1, .plus, start),
            },
            '-' => switch (c1) {
                '-' => self.puncToken(2, .decrement, start),
                '=' => self.puncToken(2, .minus_assign, start),
                else => self.puncToken(1, .minus, start),
            },
            '*' => if (c1 == '*' and c2 == '=')
                self.puncToken(3, .exponent_assign, start)
            else switch (c1) {
                '*' => self.puncToken(2, .exponent, start),
                '=' => self.puncToken(2, .star_assign, start),
                else => self.puncToken(1, .star, start),
            },
            '/' => if (c1 == '=')
                self.puncToken(2, .slash_assign, start)
            else
                self.puncToken(1, .slash, start),
            '%' => switch (c1) {
                '=' => self.puncToken(2, .percent_assign, start),
                else => self.puncToken(1, .percent, start),
            },
            '<' => if (c1 == '<' and c2 == '=')
                self.puncToken(3, .left_shift_assign, start)
            else switch (c1) {
                '<' => self.puncToken(2, .left_shift, start),
                '=' => self.puncToken(2, .less_than_equal, start),
                else => self.puncToken(1, .less_than, start),
            },
            // in a jsx tag `>` stays alone, as in `<a>=</a>` and `<a b=<e></e>></a>`
            '>' => if (self.mode == .jsx_tag)
                self.puncToken(1, .greater_than, start)
            else if (c1 == '>' and c2 == '=')
                self.puncToken(3, .right_shift_assign, start)
            else if (c1 == '>' and c2 == '>')
                if (c3 == '=')
                    self.puncToken(4, .unsigned_right_shift_assign, start)
                else
                    self.puncToken(3, .unsigned_right_shift, start)
            else switch (c1) {
                '>' => self.puncToken(2, .right_shift, start),
                '=' => self.puncToken(2, .greater_than_equal, start),
                else => self.puncToken(1, .greater_than, start),
            },
            '=' => if (c1 == '=' and c2 == '=')
                self.puncToken(3, .strict_equal, start)
            else switch (c1) {
                '=' => self.puncToken(2, .equal, start),
                '>' => self.puncToken(2, .arrow, start),
                else => self.puncToken(1, .assign, start),
            },
            '!' => if (c1 == '=' and c2 == '=')
                self.puncToken(3, .strict_not_equal, start)
            else switch (c1) {
                '=' => self.puncToken(2, .not_equal, start),
                else => self.puncToken(1, .logical_not, start),
            },
            '&' => if (c1 == '&' and c2 == '=')
                self.puncToken(3, .logical_and_assign, start)
            else switch (c1) {
                '&' => self.puncToken(2, .logical_and, start),
                '=' => self.puncToken(2, .bitwise_and_assign, start),
                else => self.puncToken(1, .bitwise_and, start),
            },
            '|' => if (c1 == '|' and c2 == '=')
                self.puncToken(3, .logical_or_assign, start)
            else switch (c1) {
                '|' => self.puncToken(2, .logical_or, start),
                '=' => self.puncToken(2, .bitwise_or_assign, start),
                else => self.puncToken(1, .bitwise_or, start),
            },
            '^' => switch (c1) {
                '=' => self.puncToken(2, .bitwise_xor_assign, start),
                else => self.puncToken(1, .bitwise_xor, start),
            },
            '?' => if (c1 == '?' and c2 == '=')
                self.puncToken(3, .nullish_assign, start)
            else switch (c1) {
                '?' => self.puncToken(2, .nullish_coalescing, start),
                '.' => if (std.ascii.isDigit(c2))
                    self.puncToken(1, .question, start)
                else
                    self.puncToken(2, .optional_chaining, start),
                else => self.puncToken(1, .question, start),
            },
            else => unreachable,
        };
    }

    inline fn peek(self: *const Lexer, offset: u32) u8 {
        const idx = @as(usize, self.cursor) + offset;
        if (idx >= self.source.len) return 0;
        return self.source[idx];
    }

    inline fn setTokenFlag(self: *Lexer, comptime flag: TokenFlag) void {
        self.state.token_flags |= flagMask(flag);
    }

    inline fn hasTokenFlag(self: *const Lexer, comptime flag: TokenFlag) bool {
        return (self.state.token_flags & flagMask(flag)) != 0;
    }

    inline fn clearTokenFlags(self: *Lexer) void {
        self.state.token_flags = 0;
    }

    inline fn consumeTokenFlags(self: *Lexer) u8 {
        const flags = self.state.token_flags;
        self.state.token_flags = 0;
        return flags;
    }

    pub inline fn rewindTo(self: *Lexer, position: u32) void {
        std.debug.assert(position <= self.source.len);
        self.cursor = position;
        self.clearTokenFlags();
    }

    inline fn isLineTerminator(self: *const Lexer) bool {
        return spec.line_terminators.lineBreakLen(self.source, self.cursor) > 0;
    }

    /// Scans the `template_middle` or `template_tail` that starts at the `}` closing
    /// a substitution.
    pub fn reScanTemplateContinuation(self: *Lexer, right_brace_start: u32) LexicalError!Token {
        std.debug.assert(right_brace_start < self.source.len);
        std.debug.assert(self.source[right_brace_start] == '}');
        self.rewindTo(right_brace_start + 1);
        return try self.scanTemplateSpan(right_brace_start, .template_tail, .template_middle) orelse
            return error.NonTerminatedTemplateLiteral;
    }

    /// Splits a `>`-led token into a leading `>` so nested type argument lists like
    /// `Foo<Bar<T>>` can close.
    pub fn reScanGreaterThan(self: *Lexer, token_start: u32) Token {
        std.debug.assert(token_start < self.source.len);
        std.debug.assert(self.source[token_start] == '>');
        self.rewindTo(token_start + 1);
        return self.createToken(.greater_than, token_start, token_start + 1);
    }

    /// Splits `<<` into a leading `<` so nested generics like `Foo<<T>(x: T) => R>` can open.
    pub fn reScanLessThan(self: *Lexer, token_start: u32) Token {
        std.debug.assert(token_start < self.source.len);
        std.debug.assert(self.source[token_start] == '<');
        self.rewindTo(token_start + 1);
        return self.createToken(.less_than, token_start, token_start + 1);
    }

    /// Scans a JSX text run.
    pub fn reScanJsxText(self: *Lexer, start: u32) Token {
        std.debug.assert(start <= self.source.len);
        self.rewindTo(start);
        while (self.cursor < self.source.len) {
            switch (self.source[self.cursor]) {
                '<', '{', '>', '}' => break,
                else => self.cursor += 1,
            }
        }

        std.debug.assert(self.cursor >= start);
        return self.createToken(.jsx_text, start, self.cursor);
    }

    const RegexResult = struct {
        span: Span,
        pattern: []const u8,
        flags: []const u8,
    };

    /// Re-scans a `/` token as a regex literal once the parser knows it is not division.
    pub fn reScanAsRegex(self: *Lexer, start: u32) LexicalError!RegexResult {
        std.debug.assert(start < self.source.len);
        std.debug.assert(self.source[start] == '/');

        self.rewindTo(start + 1);
        var in_class = false;

        while (self.cursor < self.source.len) {
            if (self.isLineTerminator()) return error.InvalidRegexLineTerminator;
            const c = self.source[self.cursor];
            if (c == '\\') {
                self.cursor += 1;
                if (self.cursor >= self.source.len) return error.UnterminatedRegexLiteral;
                if (self.isLineTerminator()) return error.InvalidRegexLineTerminator;
                self.cursor += 1;
                continue;
            }
            if (c == '[') {
                in_class = true;
                self.cursor += 1;
                continue;
            }
            if (c == ']' and in_class) {
                in_class = false;
                self.cursor += 1;
                continue;
            }
            if (c == '/' and !in_class) {
                self.cursor += 1;
                const flags_start = self.cursor;
                try self.reScanAsRegexFlags();

                return .{
                    .span = .{ .start = start, .end = self.cursor },
                    .pattern = self.source[start + 1 .. flags_start - 1],
                    .flags = self.source[flags_start..self.cursor],
                };
            }

            self.cursor += 1;
        }
        return error.UnterminatedRegexLiteral;
    }

    fn reScanAsRegexFlags(self: *Lexer) LexicalError!void {
        const src = self.source;
        const flags_start = self.cursor;
        while (self.cursor < src.len) {
            const c = src[self.cursor];
            if (identifier_char_ascii[c]) {
                self.cursor += 1;
                continue;
            }
            if (c < 0x80) break;
            const cp = try util.Utf.codePointAt(src, self.cursor);
            if (!lexical.isIdentifierChar(cp.value)) break;
            self.cursor += cp.len;
        }
        std.debug.assert(self.cursor >= flags_start);
        std.debug.assert(self.cursor <= src.len);
    }

    fn scanString(self: *Lexer) LexicalError!Token {
        std.debug.assert(self.cursor < self.source.len);

        const start = self.cursor;
        const src = self.source;
        const quote = src[start];
        std.debug.assert(quote == '"' or quote == '\'');
        var pos = self.cursor + 1;

        if (self.mode == .normal) {
            while (pos < src.len) {
                const hit = if (quote == '"')
                    findAnyPos("\"\\\n\r", src, pos)
                else
                    findAnyPos("'\\\n\r", src, pos);
                if (hit >= src.len) {
                    pos = hit;
                    break;
                }
                if (src[hit] == quote) {
                    pos = hit + 1;
                    self.cursor = pos;
                    return self.createToken(.string_literal, start, pos);
                }
                if (src[hit] == '\\') {
                    self.cursor = hit;
                    try self.consumeEscape(.string);
                    pos = self.cursor;
                    continue;
                }
                std.debug.assert(src[hit] == '\n' or src[hit] == '\r');
                self.cursor = hit;
                return error.UnterminatedString;
            }
        } else {
            // jsx attribute values have no escapes and may span lines
            while (pos < src.len) {
                if (src[pos] == quote) {
                    pos += 1;
                    self.cursor = pos;
                    return self.createToken(.string_literal, start, pos);
                }
                pos += 1;
            }
        }

        self.cursor = pos;
        return error.UnterminatedString;
    }

    fn scanTemplateLiteral(self: *Lexer) LexicalError!Token {
        std.debug.assert(self.cursor < self.source.len);
        std.debug.assert(self.source[self.cursor] == '`');

        const start = self.cursor;
        self.cursor += 1;
        return try self.scanTemplateSpan(start, .no_substitution_template, .template_head) orelse {
            self.cursor = @intCast(self.source.len);
            return error.NonTerminatedTemplateLiteral;
        };
    }

    inline fn scanTemplateSpan(
        self: *Lexer,
        start: u32,
        comptime backtick_tag: TokenTag,
        comptime substitution_tag: TokenTag,
    ) LexicalError!?Token {
        const src = self.source;
        var pos = self.cursor;
        while (true) {
            pos = findAnyPos("`\\$\r", src, pos);
            if (pos >= src.len) return null;
            switch (src[pos]) {
                '`' => {
                    self.cursor = pos + 1;
                    return self.createToken(backtick_tag, start, self.cursor);
                },
                '\\' => {
                    self.cursor = pos;
                    try self.consumeEscape(.template);
                    pos = self.cursor;
                },
                '$' => {
                    if (pos + 1 < src.len and src[pos + 1] == '{') {
                        self.cursor = pos + 2;
                        return self.createToken(substitution_tag, start, self.cursor);
                    }
                    pos += 1;
                },
                // a raw CR must be normalized in the cooked value, so it counts as escaped
                '\r' => {
                    self.setTokenFlag(.escaped);
                    pos += 1;
                },
                else => unreachable,
            }
        }
    }

    const EscapeContext = enum {
        string,
        template,
    };

    // a bad template escape is not fatal, a tagged template cooks it to undefined
    fn consumeEscape(self: *Lexer, comptime context: EscapeContext) LexicalError!void {
        if (context == .template) {
            self.consumeEscapeImpl(context) catch |err| {
                if (err == error.OutOfMemory) return error.OutOfMemory;
                self.setTokenFlag(.invalid_escape);
                if (self.cursor < self.source.len) self.cursor += 1;
            };
        } else {
            try self.consumeEscapeImpl(context);
        }
    }

    fn consumeEscapeImpl(self: *Lexer, comptime context: EscapeContext) LexicalError!void {
        std.debug.assert(self.cursor < self.source.len);
        std.debug.assert(self.source[self.cursor] == '\\');

        self.cursor += 1;
        self.setTokenFlag(.escaped);
        if (self.cursor >= self.source.len) return error.UnterminatedString;

        switch (self.source[self.cursor]) {
            '0' => {
                // `\0` is a null escape only when no digit follows
                if (!std.ascii.isDigit(self.peek(1))) {
                    self.cursor += 1;
                    return;
                }
                if (context == .template) return error.InvalidOctalEscape;
                try self.consumeOctal();
            },
            'x' => try self.consumeHex(),
            'u' => {
                const cp = try self.consumeUnicodeEscape(.normal);
                if (std.unicode.isSurrogateCodepoint(cp)) {
                    const is_high_paired = std.unicode.utf16IsHighSurrogate(@intCast(cp)) and
                        self.peek(0) == '\\' and
                        self.peek(1) == 'u';
                    if (is_high_paired) {
                        self.cursor += 1;
                        const next_cp = try self.consumeUnicodeEscape(.normal);
                        if (next_cp < 0xDC00 or next_cp > 0xDFFF) {
                            self.setTokenFlag(.lone_surrogates);
                        }
                    } else self.setTokenFlag(.lone_surrogates);
                }
            },
            '1'...'7' => {
                if (context == .template) return error.InvalidOctalEscape;
                try self.consumeOctal();
            },
            '8'...'9' => {
                if (context == .template) return error.InvalidOctalEscape;
                self.cursor += 1;
            },
            '\n' => self.cursor += 1,
            '\r' => {
                self.cursor += 1;
                if (self.cursor < self.source.len and self.source[self.cursor] == '\n') {
                    self.cursor += 1;
                }
            },
            else => self.cursor += @max(1, spec.line_terminators.unicodeSeparatorLen(
                self.source,
                self.cursor,
            )),
        }
    }

    fn consumeOctal(self: *Lexer) LexicalError!void {
        std.debug.assert(self.cursor < self.source.len);
        const result = spec.escapes.parseOctal(self.source, self.cursor);
        std.debug.assert(result.end > self.cursor);
        self.cursor = @intCast(result.end);
    }

    fn consumeHex(self: *Lexer) LexicalError!void {
        std.debug.assert(self.cursor < self.source.len);
        const r = spec.escapes.parseHex2(self.source, self.cursor + 1) orelse
            return error.InvalidHexEscape;
        std.debug.assert(r.end >= self.cursor + 1);
        self.cursor = @intCast(r.end);
    }

    const ConsumeUnicodeContext = enum {
        identifier_start,
        identifier_continue,
        normal,
    };

    fn consumeUnicodeEscape(
        self: *Lexer,
        comptime context: ConsumeUnicodeContext,
    ) LexicalError!u21 {
        if (self.cursor >= self.source.len or self.source[self.cursor] != 'u') {
            return error.InvalidUnicodeEscape;
        }
        self.setTokenFlag(.escaped);

        const parsed = spec.escapes.parseUnicodeEscape(self.source, self.cursor + 1) orelse
            return error.InvalidUnicodeEscape;
        switch (context) {
            .identifier_start => {
                if (!lexical.isIdentifierStart(parsed.value)) return error.InvalidIdentifierStart;
            },
            .identifier_continue => {
                if (!lexical.isIdentifierChar(parsed.value)) {
                    return error.InvalidIdentifierContinue;
                }
            },
            .normal => {},
        }
        self.cursor = @intCast(parsed.end);
        return parsed.value;
    }

    fn scanDot(self: *Lexer) LexicalError!Token {
        std.debug.assert(self.cursor < self.source.len);
        std.debug.assert(self.source[self.cursor] == '.');

        const start = self.cursor;
        const c1 = self.peek(1);
        if (std.ascii.isDigit(c1)) return self.scanNumber();
        if (c1 == '.' and self.peek(2) == '.') return self.puncToken(3, .spread, start);
        return self.puncToken(1, .dot, start);
    }

    fn scanIdentifierBody(self: *Lexer, is_jsx_tag: bool) !bool {
        std.debug.assert(self.cursor <= self.source.len);
        const table = if (is_jsx_tag) &identifier_char_jsx_ascii else &identifier_char_ascii;
        const src = self.source;
        const entry_pos = self.cursor;
        var pos = entry_pos;
        var has_escape = false;

        while (true) {
            while (pos < src.len and table[src[pos]]) pos += 1;
            if (pos >= src.len) break;

            const c = src[pos];
            if (c == '\\') {
                if (is_jsx_tag) {
                    self.cursor = pos;
                    return error.JsxIdentifierCannotContainEscapes;
                }
                has_escape = true;
                self.cursor = pos + 1;
                _ = try self.consumeUnicodeEscape(.identifier_continue);
                pos = self.cursor;
                continue;
            }
            if (c >= 0x80) {
                @branchHint(.cold);
                self.cursor = pos;
                const cp = try util.Utf.codePointAt(src, pos);
                if (lexical.isIdentifierChar(cp.value)) {
                    pos += cp.len;
                    continue;
                }
            }
            break;
        }

        std.debug.assert(pos >= entry_pos);
        self.cursor = pos;
        return has_escape;
    }

    fn scanIdentifierOrKeyword(self: *Lexer) !Token {
        std.debug.assert(self.cursor < self.source.len);
        const start = self.cursor;
        const is_jsx_tag = self.mode == .jsx_tag;
        const is_private = self.peek(0) == '#';
        if (is_private) self.cursor += 1;

        var has_escape = false;
        const first_char = self.peek(0);
        if (std.ascii.isAscii(first_char)) {
            @branchHint(.likely);
            if (first_char == '\\') {
                if (is_jsx_tag) return error.JsxIdentifierCannotStartWithBackslash;
                has_escape = true;
                self.cursor += 1;
                _ = try self.consumeUnicodeEscape(.identifier_start);
            } else {
                if (!identifier_start_ascii[first_char]) {
                    @branchHint(.cold);
                    return error.InvalidIdentifierStart;
                }
                self.cursor += 1;
            }
            const body_has_escape = try self.scanIdentifierBody(is_jsx_tag);
            has_escape = has_escape or body_has_escape;
        } else {
            @branchHint(.cold);
            const cp = try util.Utf.codePointAt(self.source, self.cursor);
            if (!lexical.isIdentifierStart(cp.value)) return error.InvalidIdentifierStart;
            self.cursor += cp.len;
            has_escape = try self.scanIdentifierBody(is_jsx_tag);
        }

        std.debug.assert(self.cursor > start);
        const lexeme = self.source[start..self.cursor];
        const tag: TokenTag = if (is_jsx_tag)
            .jsx_identifier
        else if (is_private)
            .private_identifier
        else if (has_escape)
            getEscapedKeywordType(lexeme)
        else
            keywordTag(lexeme);

        return self.createToken(tag, start, self.cursor);
    }

    fn getEscapedKeywordType(lexeme: []const u8) TokenTag {
        @branchHint(.cold);
        std.debug.assert(lexeme.len > 0);
        var buf: [keyword_length_max]u8 = undefined;
        var out: usize = 0;
        var i: usize = 0;
        while (i < lexeme.len) {
            if (out == buf.len) return .identifier;
            if (lexeme[i] == '\\' and i + 1 < lexeme.len and lexeme[i + 1] == 'u') {
                const parsed = spec.escapes.parseUnicodeEscape(lexeme, i + 2) orelse
                    return .identifier;
                if (parsed.value >= 0x80) return .identifier;
                buf[out] = @intCast(parsed.value);
                out += 1;
                i = parsed.end;
            } else {
                if (!std.ascii.isAscii(lexeme[i])) return .identifier;
                buf[out] = lexeme[i];
                out += 1;
                i += 1;
            }
        }
        return keywordTag(buf[0..out]);
    }

    fn scanNumber(self: *Lexer) LexicalError!Token {
        std.debug.assert(self.cursor < self.source.len);
        const lead = self.source[self.cursor];
        std.debug.assert(lead == '.' or std.ascii.isDigit(lead));

        const start = self.cursor;
        var tag: TokenTag = .numeric_literal;
        var has_decimal = false;
        var has_exponent = false;
        var is_leading_zero = false;

        if (self.peek(0) == '0') {
            self.cursor += 1;
            switch (self.peek(0)) {
                'x', 'X' => {
                    tag = .hex_literal;
                    try self.consumeRadixDigits(std.ascii.isHex, error.InvalidHexLiteral);
                },
                'o', 'O' => {
                    tag = .octal_literal;
                    try self.consumeRadixDigits(
                        isOctalDigit,
                        error.InvalidOctalLiteralDigit,
                    );
                },
                'b', 'B' => {
                    tag = .binary_literal;
                    try self.consumeRadixDigits(isBinaryDigit, error.InvalidBinaryLiteral);
                },
                '0'...'9' => {
                    is_leading_zero = true;
                    // legacy octal `077` or decimal `089`, neither allows separators
                    var is_legacy_octal = true;
                    while (self.cursor < self.source.len) {
                        const c = self.source[self.cursor];
                        if (std.ascii.isDigit(c)) {
                            if (c >= '8') is_legacy_octal = false;
                            self.cursor += 1;
                        } else break;
                    }
                    tag = if (is_legacy_octal) .octal_literal else .numeric_literal;
                },
                else => {},
            }
        } else {
            try self.consumeDecimalDigits();
        }

        if (tag == .numeric_literal and self.peek(0) == '.') {
            self.cursor += 1;
            has_decimal = true;
            try self.consumeDecimalDigits();
        }

        if (tag == .numeric_literal) {
            const exp = self.peek(0);
            if (exp == 'e' or exp == 'E') {
                has_exponent = true;
                try self.consumeExponent();
            }
        }

        if (!is_leading_zero and self.peek(0) == 'n') {
            if (has_exponent) return error.ExponentialBigInt;
            if (has_decimal) return error.NonIntegerBigInt;
            self.cursor += 1;
            tag = .bigint_literal;
        }

        const c = self.peek(0);
        if (identifier_start_ascii[c] or c == '\\') return error.IdentifierAfterNumericLiteral;
        std.debug.assert(self.cursor > start);
        return self.createToken(tag, start, self.cursor);
    }

    inline fn consumeDigits(self: *Lexer, comptime isValidDigit: fn (u8) bool) LexicalError!void {
        std.debug.assert(self.cursor <= self.source.len);
        if (self.cursor < self.source.len and self.source[self.cursor] == '_') {
            return error.NumericSeparatorMisuse;
        }

        while (self.cursor < self.source.len) {
            const c = self.source[self.cursor];
            if (isValidDigit(c)) {
                self.cursor += 1;
            } else if (c == '_') {
                const next = self.peek(1);
                if (isValidDigit(next)) {
                    self.cursor += 1;
                } else if (next == '_') {
                    self.cursor += 1;
                    return error.ConsecutiveNumericSeparators;
                } else {
                    return error.NumericSeparatorMisuse;
                }
            } else {
                break;
            }
        }
        std.debug.assert(self.cursor == self.source.len or self.source[self.cursor] != '_');
    }

    inline fn consumeDecimalDigits(self: *Lexer) LexicalError!void {
        return self.consumeDigits(std.ascii.isDigit);
    }

    inline fn consumeRadixDigits(
        self: *Lexer,
        comptime isValidDigit: fn (u8) bool,
        comptime empty_error: LexicalError,
    ) LexicalError!void {
        self.cursor += 1;
        const digits_start = self.cursor;
        try self.consumeDigits(isValidDigit);
        if (self.cursor == digits_start) return empty_error;
    }

    fn isBinaryDigit(c: u8) bool {
        return c == '0' or c == '1';
    }

    fn isOctalDigit(c: u8) bool {
        return c >= '0' and c <= '7';
    }

    fn consumeExponent(self: *Lexer) LexicalError!void {
        std.debug.assert(self.cursor < self.source.len);
        std.debug.assert(self.source[self.cursor] == 'e' or self.source[self.cursor] == 'E');
        self.cursor += 1;
        const sign = self.peek(0);
        if (sign == '+' or sign == '-') self.cursor += 1;
        const digits_start = self.cursor;
        try self.consumeDecimalDigits();
        if (self.cursor == digits_start) return error.InvalidExponentPart;
    }

    const TriviaClass = enum(u8) { none, space, line_break, slash, less_than, minus, non_ascii };

    const trivia_class: [256]TriviaClass = blk: {
        var t: [256]TriviaClass = @splat(.none);
        t[' '] = .space;
        t['\t'] = .space;
        t[0x0B] = .space;
        t[0x0C] = .space;
        t['\n'] = .line_break;
        t['\r'] = .line_break;
        t['/'] = .slash;
        t['<'] = .less_than;
        t['-'] = .minus;
        for (0x80..256) |i| t[i] = .non_ascii;
        break :blk t;
    };

    fn isMultiByteSpace(cp: u21) bool {
        return switch (cp) {
            '\u{FEFF}',
            '\u{00A0}',
            // not WhiteSpace in the spec, but tsc scans NEL as a space and not a line break
            '\u{0085}',
            '\u{2000}'...'\u{200A}',
            '\u{202F}',
            '\u{205F}',
            '\u{3000}',
            '\u{1680}',
            => true,
            else => false,
        };
    }

    inline fn skipTrivia(self: *Lexer) LexicalError!void {
        std.debug.assert(self.cursor <= self.source.len);

        if (self.cursor < self.source.len and trivia_class[self.source[self.cursor]] == .none) {
            return;
        }

        var can_be_html_close_comment =
            self.cursor == 0 or self.hasTokenFlag(.line_terminator_before);

        const src = self.source;
        var pos = self.cursor;
        const entry_pos = pos;

        while (pos < src.len) {
            if (src[pos] == ' ') {
                pos += 1;
                continue;
            }
            switch (trivia_class[src[pos]]) {
                .space => pos += 1,
                .line_break => {
                    self.setTokenFlag(.line_terminator_before);
                    can_be_html_close_comment = true;
                    pos += 1;
                },
                .slash => {
                    self.cursor = pos;
                    switch (self.peek(1)) {
                        '/' => try self.scanLineComment(),
                        '*' => {
                            try self.scanBlockComment();
                            if (self.hasTokenFlag(.line_terminator_before)) {
                                can_be_html_close_comment = true;
                            }
                        },
                        else => break,
                    }
                    pos = self.cursor;
                },
                .less_than => {
                    // annex B html comments are script-only
                    if (self.source_type == .module or pos + 3 >= src.len or
                        src[pos + 1] != '!' or src[pos + 2] != '-' or src[pos + 3] != '-') break;
                    self.cursor = pos;
                    try self.scanHtmlComment();
                    pos = self.cursor;
                },
                .minus => {
                    // `-->` closes only at line start
                    if (self.source_type == .module or
                        !can_be_html_close_comment or
                        pos + 2 >= src.len or
                        src[pos + 1] != '-' or
                        src[pos + 2] != '>') break;
                    self.cursor = pos;
                    try self.scanHtmlCloseComment();
                    pos = self.cursor;
                },
                .non_ascii => {
                    @branchHint(.unlikely);
                    const us_len = spec.line_terminators.unicodeSeparatorLen(src, pos);
                    if (us_len > 0) {
                        self.setTokenFlag(.line_terminator_before);
                        can_be_html_close_comment = true;
                        pos += us_len;
                        continue;
                    }
                    self.cursor = pos;
                    const cp = try util.Utf.codePointAt(src, pos);
                    if (!isMultiByteSpace(cp.value)) break;
                    pos += cp.len;
                },
                .none => break,
            }
        }

        std.debug.assert(pos >= entry_pos);
        std.debug.assert(pos <= src.len);
        self.cursor = pos;
    }

    inline fn recordComment(
        self: *Lexer,
        @"type": ast.Comment.Type,
        start: u32,
        end: u32,
    ) error{OutOfMemory}!void {
        std.debug.assert(start < end);
        std.debug.assert(end <= self.source.len);
        if (!self.collect_comments) return;
        // `<!--` is 4 wide, `-->` is 3 with no tail, `//` and `/*` are 2
        const head: u32 = switch (self.source[start]) {
            '<' => 4,
            '-' => 3,
            else => 2,
        };
        const tail: u32 = if (@"type" == .block) 2 else 0;
        try self.comments.append(self.allocator, .{
            .type = @"type",
            .value = .{ .start = start + head, .end = end - tail },
            .span = .{ .start = start, .end = end },
        });
    }

    fn scanLineComment(self: *Lexer) LexicalError!void {
        std.debug.assert(self.cursor + 1 < self.source.len);
        std.debug.assert(self.source[self.cursor] == '/');
        std.debug.assert(self.source[self.cursor + 1] == '/');
        const start = self.cursor;
        const src = self.source;
        var pos = start + 2;
        while (true) {
            pos = findAnyPos("\r\n\xe2", src, pos);
            if (pos >= src.len) break;
            const c = src[pos];
            if (c == '\n' or c == '\r') break;
            if (c == 0xE2 and spec.line_terminators.unicodeSeparatorLen(src, pos) > 0) break;
            pos += 1;
        }
        self.cursor = pos;
        try self.recordComment(.line, start, pos);
    }

    fn scanBlockComment(self: *Lexer) LexicalError!void {
        std.debug.assert(self.cursor + 1 < self.source.len);
        std.debug.assert(self.source[self.cursor] == '/');
        std.debug.assert(self.source[self.cursor + 1] == '*');
        const start = self.cursor;
        const src = self.source;
        var pos = start + 2;
        if (pos + 1 < src.len and src[pos] == '*' and src[pos + 1] != '/') pos += 1;
        while (true) {
            pos = findAnyPos("*\n\r\xe2", src, pos);
            if (pos >= src.len) break;
            switch (src[pos]) {
                '*' => {
                    if (pos + 1 < src.len and src[pos + 1] == '/') {
                        pos += 2;
                        self.cursor = pos;
                        try self.recordComment(.block, start, pos);
                        return;
                    }
                    pos += 1;
                },
                '\n', '\r' => {
                    self.setTokenFlag(.line_terminator_before);
                    pos += 1;
                    break;
                },
                else => {
                    const lt_len = spec.line_terminators.unicodeSeparatorLen(src, pos);
                    if (lt_len > 0) {
                        self.setTokenFlag(.line_terminator_before);
                        pos += lt_len;
                        break;
                    } else pos += 1;
                },
            }
        }
        // the offset load catches `*/` across chunks
        var w = pos - 1;
        while (w + 17 <= src.len) {
            const stars = Simd.loadChunk(src, w) == Simd.splat('*');
            const slashes = Simd.loadChunk(src, w + 1) == Simd.splat('/');
            const first = Simd.firstTrueLane(stars & slashes);
            if (first < 16) {
                self.cursor = w + first + 2;
                try self.recordComment(.block, start, self.cursor);
                return;
            }
            w += 16;
        }
        while (w + 1 < src.len) : (w += 1) {
            if (src[w] == '*' and src[w + 1] == '/') {
                self.cursor = @intCast(w + 2);
                try self.recordComment(.block, start, self.cursor);
                return;
            }
        }
        self.cursor = @intCast(src.len);
        return error.UnterminatedMultiLineComment;
    }

    fn scanHtmlComment(self: *Lexer) LexicalError!void {
        std.debug.assert(self.source_type != .module);
        std.debug.assert(self.cursor + 3 < self.source.len);
        std.debug.assert(self.source[self.cursor] == '<');
        std.debug.assert(self.source[self.cursor + 1] == '!');
        const start = self.cursor;
        const src = self.source;
        self.cursor += 4;
        while (self.cursor < src.len) {
            const c = src[self.cursor];
            if (c == '-' and self.peek(1) == '-' and self.peek(2) == '>') {
                self.cursor += 3;
                return self.recordComment(.line, start, self.cursor);
            }
            if (self.isLineTerminator()) break;
            self.cursor += 1;
        }
        try self.recordComment(.line, start, self.cursor);
    }

    fn scanHtmlCloseComment(self: *Lexer) LexicalError!void {
        std.debug.assert(self.source_type != .module);
        std.debug.assert(self.cursor + 2 < self.source.len);
        std.debug.assert(self.source[self.cursor] == '-');
        std.debug.assert(self.source[self.cursor + 1] == '-');
        std.debug.assert(self.source[self.cursor + 2] == '>');
        const start = self.cursor;
        self.cursor += 3;
        while (self.cursor < self.source.len) : (self.cursor += 1) {
            if (self.isLineTerminator()) break;
        }
        try self.recordComment(.line, start, self.cursor);
    }

    pub inline fn createToken(self: *Lexer, tag: TokenTag, start: u32, end: u32) Token {
        std.debug.assert(start <= end);
        std.debug.assert(end <= self.source.len);
        return .{
            .tag = tag,
            .span = .{ .start = start, .end = end },
            .flags = self.consumeTokenFlags(),
        };
    }
};

pub fn getLexicalErrorMessage(error_type: LexicalError) []const u8 {
    return switch (error_type) {
        error.InvalidHexEscape => "Invalid hexadecimal escape sequence",
        error.UnterminatedString => "Unterminated string literal",
        error.NonTerminatedTemplateLiteral => "Unterminated template literal",
        error.UnterminatedRegexLiteral => "Unterminated regular expression literal",
        error.InvalidRegexLineTerminator => "Line terminators are not allowed in regular" ++
            " expression literals",
        error.InvalidIdentifierStart => "Invalid character at the start of an identifier",
        error.InvalidIdentifierContinue => "Invalid character in an identifier",
        error.UnterminatedMultiLineComment => "Unterminated multi-line comment",
        error.InvalidUnicodeEscape => "Invalid Unicode escape sequence",
        error.InvalidOctalEscape => "Octal escape sequences are not allowed in template literals",
        error.InvalidOctalLiteralDigit => "An octal literal must contain at least one octal digit",
        error.InvalidBinaryLiteral => "A binary literal must contain at least one binary digit",
        error.InvalidHexLiteral => "A hexadecimal literal must contain at least one hexadecimal" ++
            " digit",
        error.InvalidExponentPart => "An exponent must contain at least one digit",
        error.NumericSeparatorMisuse => "Numeric separators are only allowed between two digits",
        error.ConsecutiveNumericSeparators => "Multiple consecutive numeric separators are" ++
            " not permitted",
        error.ExponentialBigInt => "A bigint literal cannot use exponential notation",
        error.NonIntegerBigInt => "A bigint literal must be an integer",
        error.IdentifierAfterNumericLiteral => "An identifier or keyword cannot immediately" ++
            " follow a numeric literal",
        error.InvalidUtf8 => "Invalid UTF-8 byte sequence",
        error.OutOfMemory => "Out of memory",
        error.JsxIdentifierCannotContainEscapes => "JSX tag names cannot contain escape" ++
            " sequences",
        error.JsxIdentifierCannotStartWithBackslash => "JSX tag names cannot start with a" ++
            " backslash",
    };
}

pub fn getLexicalErrorHelp(error_type: LexicalError) []const u8 {
    return switch (error_type) {
        error.InvalidHexEscape => "Write two hexadecimal digits after '\\x', such as '\\x41'",
        error.UnterminatedString => "Add a closing quote to end the string",
        error.NonTerminatedTemplateLiteral => "Add a closing backtick to end the template",
        error.UnterminatedRegexLiteral => "Add a closing '/' to end the regular expression," ++
            " followed by any flags",
        error.InvalidRegexLineTerminator => "Remove the line break, or match it with '\\n'",
        error.InvalidIdentifierStart => "Start the identifier with a letter, '_', or '$'",
        error.InvalidIdentifierContinue => "Use only letters, digits, '_', and '$' in an" ++
            " identifier",
        error.UnterminatedMultiLineComment => "Add '*/' to close the comment",
        error.InvalidUnicodeEscape => "Write four hexadecimal digits, such as '\\u00e9', or a" ++
            " code point of one to six digits in braces, such as '\\u{1f600}'",
        error.InvalidOctalEscape => "Use a hexadecimal escape instead, such as '\\x07'",
        error.InvalidOctalLiteralDigit => "Add an octal digit (0-7) after '0o', such as '0o17'",
        error.InvalidBinaryLiteral => "Add a binary digit (0 or 1) after '0b', such as '0b101'",
        error.InvalidHexLiteral => "Add a hexadecimal digit (0-9, a-f) after '0x', such as" ++
            " '0xff'",
        error.InvalidExponentPart => "Add digits after the exponent, such as '1e10' or '1e-5'",
        error.NumericSeparatorMisuse => "Place the separator between two digits, or remove it",
        error.ConsecutiveNumericSeparators => "Remove the extra separator",
        error.ExponentialBigInt => "Write the value without an exponent, or remove the 'n'" ++
            " suffix",
        error.NonIntegerBigInt => "Remove the fractional part, or remove the 'n' suffix",
        error.IdentifierAfterNumericLiteral => "Add a space between the number and the" ++
            " identifier",
        error.InvalidUtf8 => "Save the file with UTF-8 encoding",
        error.OutOfMemory => "The system ran out of memory while parsing",
        error.JsxIdentifierCannotContainEscapes => "Write the character itself instead of the" ++
            " escape",
        error.JsxIdentifierCannotStartWithBackslash => "Write the tag name without escape" ++
            " sequences",
    };
}
