const std = @import("std");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const Span = @import("token.zig").Span;
const TokenFlag = @import("token.zig").TokenFlag;
const flagMask = @import("token.zig").flagMask;
const keywordTag = @import("token.zig").keywordTag;
const keywordTagAt = @import("token.zig").keywordTagAt;
const keyword_read_len = @import("token.zig").keyword_read_len;
const ast = @import("ast.zig");
const lexical = @import("lexical.zig");
const util = @import("util");
const spec = @import("spec");
const Simd = util.Simd;
const line_terminators = spec.line_terminators;

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
    token_flags: u8 = 0,
};

const IdentifierBlocks = struct {
    start: u32 = std.math.maxInt(u32),
    first: u64 = 0,
    second: u64 = 0,

    const start_mask = ~@as(u32, 63);
};

pub const Lexer = struct {
    comments: std.ArrayList(ast.Comment) = .empty,
    collect_comments: bool,
    allocator: std.mem.Allocator,
    state: LexerState = .{},
    mode: LexerMode = .normal,

    source: []const u8,
    cursor: u32 = 0,
    fast_limit: u32,
    fast_end: u32,
    fast_open: bool = true,
    blocks: IdentifierBlocks = .{},

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
        const fast_limit: u32 = @intCast(source.len -| fast_margin);
        var self: Lexer = .{
            .source = source,
            .fast_limit = fast_limit,
            .fast_end = fast_limit,
            .collect_comments = collect_comments,
            .allocator = allocator,
            .source_type = source_type,
        };
        if (std.mem.startsWith(u8, source, "#!")) {
            self.cursor = findAnyPos("\n\r", source, 2);
            self.hashbang = .{ .start = 2, .len = self.cursor - 2 };
        }
        return self;
    }

    pub inline fn setMode(self: *Lexer, mode: LexerMode) void {
        self.mode = mode;
        self.fast_limit = if (mode == .normal) @intCast(self.source.len -| fast_margin) else 0;
        self.fast_end = if (self.fast_open) self.fast_limit else 0;
    }

    pub fn setFastOpen(self: *Lexer, open: bool) void {
        self.fast_open = open;
        self.fast_end = if (open) self.fast_limit else 0;
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

    pub fn nextTokenInto(self: *Lexer, out: *Token) LexicalError!void {
        // avoids a stack round trip
        out.* = try @call(.always_inline, nextToken, .{self});
    }

    pub fn nextToken(self: *Lexer) LexicalError!Token {
        self.state.token_flags = 0;
        try self.skipTrivia();
        const start = self.cursor;
        if (start >= self.source.len) return self.createToken(.eof, start, start);

        const c = self.source[start];
        if (identifier_start_ascii[c] and self.mode == .normal) {
            if (self.scanAsciiIdentifier()) |token| return token;
        }

        const info = byte_info[c];
        if (info.class == Class.punctuation) return self.puncToken(info.tag, 1);
        if (info.class == Class.operator) {
            const next = self.peek(1);
            if (byte_info[next].group & info.continued_by == 0) return self.puncToken(info.tag, 1);
            if (c != '.' or !std.ascii.isDigit(next)) return self.scanPunctuation();
        }
        return switch (c) {
            '0'...'9', '.' => self.scanNumber(),
            '"', '\'' => self.scanString(),
            '`' => self.scanTemplateLiteral(),
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
        if (start & IdentifierBlocks.start_mask != self.blocks.start) {
            return self.scanIdentifierReloaded(start, flags, out);
        }
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
        if (byte_info[src[end]].class & Class.identifier_part != 0) return false;

        self.cursor = end;
        writeToken(out, start, end, keywordTagAt(src + start, len), flags);
        return true;
    }

    const identifier_run_max = 64;

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

    fn scanAsciiIdentifier(self: *Lexer) ?Token {
        const src = self.source;
        const start = self.cursor;
        if (start & IdentifierBlocks.start_mask != self.blocks.start) self.loadBlocks(start);
        const end = start + self.identifierRunInBlocks(start);
        if (end - start == identifier_run_max) return null;
        if (end < src.len and (src[end] == '\\' or src[end] >= 0x80)) return null;

        const tag = if (@as(usize, start) + keyword_read_len <= src.len)
            keywordTagAt(src.ptr + start, end - start)
        else
            keywordTag(src[start..end]);
        self.cursor = end;
        return self.createToken(tag, start, end);
    }

    noinline fn scanOperatorFast(self: *Lexer, start: u32, flags: u8, out: *Token) bool {
        const src = self.source.ptr;
        const next = src[start + 1];
        switch (src[start]) {
            '/' => if (next == '/' or next == '*') return self.scanAfterComments(start, flags, out),
            '<' => if (next == '!') return false,
            '-' => if (next == '-') return false,
            '.' => if (next != '.') return false,
            else => {},
        }
        self.cursor = start;
        self.state.token_flags = flags;
        out.* = self.scanPunctuation();
        return true;
    }

    fn scanAfterComments(self: *Lexer, start: u32, flags: u8, out: *Token) bool {
        const src = self.source.ptr;
        const entry = self.cursor;
        const comments_len = self.comments.items.len;
        self.state.token_flags = flags;
        var position = start;

        const reached_token = comments: while (true) {
            std.debug.assert(src[position] == '/');
            position = (if (src[position + 1] == '/')
                self.scanLineComment(position, 2)
            else
                self.scanBlockComment(position)) catch break :comments false;

            while (true) {
                if (position >= self.fast_limit) break :comments false;
                const c = src[position];
                if (byte_info[c].class & Class.line_break != 0) {
                    position = self.skipLineBreaks(position);
                    if (position == 0) break :comments false;
                    self.setTokenFlag(.line_terminator_before);
                } else if (c == ' ' or c == '\t') {
                    position = self.skipIndentation(position) orelse break :comments false;
                } else break;
            }
            const next = src[position + 1];
            if (src[position] != '/' or (next != '/' and next != '*')) break :comments true;
        };

        const seen = self.state.token_flags;
        self.state.token_flags = 0;
        if (reached_token and self.scanTokenAt(position, seen, src[position], out)) return true;

        self.cursor = entry;
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
            self.state.token_flags = 0;
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
        const identifier_part: u8 = 1 << 5;
        const line_break: u8 = 1 << 6;
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
        t['='].group = Group.equals;
        t['.'].group = Group.dot;
        for ("<>&|?+-*/!") |c| t[c].group = Group.operator;

        for (std.enums.values(TokenTag)) |tag| {
            if (tag.isIdentifierLike()) continue;
            const text = tag.toString() orelse continue;
            const info = &t[text[0]];
            if (text.len > 1) {
                info.continued_by |= t[text[1]].group;
            } else {
                info.tag = tag;
                info.class = Class.punctuation;
            }
        }
        for ([_][]const u8{ "//", "/*", "<!", ".5" }) |text| {
            t[text[0]].continued_by |= t[text[1]].group;
        }
        for (&t) |*info| {
            if (info.continued_by != 0) info.class = Class.operator;
        }
        break :blk t;
    };

    comptime {
        std.debug.assert(@sizeOf(ByteInfo) == 8);
    }

    inline fn puncToken(self: *Lexer, tag: TokenTag, len: u32) Token {
        const start = self.cursor;
        self.cursor = start + len;
        return self.createToken(tag, start, start + len);
    }

    fn scanPunctuation(self: *Lexer) Token {
        const first = self.source[self.cursor];
        const after = [3]u8{ self.peek(1), self.peek(2), self.peek(3) };
        const word = std.mem.readInt(u32, &.{ first, after[0], after[1], after[2] }, .little);
        switch (first) {
            inline '+', '-', '*', '/', '%', '<', '>', '=', '!', '&', '|', '^', '?', '.' => |byte| {
                // in a jsx tag `>` stays alone, as in `<a>=</a>` and `<a b=<e></e>></a>`
                if (byte == '>' and self.mode == .jsx_tag) return self.puncToken(.greater_than, 1);
                inline for (comptime punctuatorsFrom(byte)) |tag| {
                    const text = comptime tag.toString().?;
                    const mask = comptime std.math.maxInt(u32) >> @intCast(8 * (4 - text.len));
                    const spelled = comptime std.mem.readVarInt(u32, text, .little);
                    const is_split = tag == .optional_chaining and std.ascii.isDigit(after[1]);
                    if (word & mask == spelled and !is_split) return self.puncToken(tag, text.len);
                }
                unreachable;
            },
            else => unreachable,
        }
    }

    fn punctuatorsFrom(comptime first: u8) []const TokenTag {
        comptime {
            @setEvalBranchQuota(50_000);
            var tags: []const TokenTag = &.{};
            var len: usize = 4;
            while (len > 0) : (len -= 1) {
                for (std.enums.values(TokenTag)) |tag| {
                    if (tag.isIdentifierLike()) continue;
                    const text = tag.toString() orelse continue;
                    if (text.len == len and text[0] == first) tags = tags ++ .{tag};
                }
            }
            return tags;
        }
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

    pub inline fn rewindTo(self: *Lexer, position: u32) void {
        std.debug.assert(position <= self.source.len);
        self.cursor = position;
        self.state.token_flags = 0;
    }

    inline fn atLineBreak(self: *const Lexer) bool {
        return line_terminators.lineBreakLen(self.source, self.cursor) > 0;
    }

    pub fn reScanTemplateContinuation(self: *Lexer, right_brace_start: u32) LexicalError!Token {
        std.debug.assert(self.source[right_brace_start] == '}');
        self.rewindTo(right_brace_start + 1);
        return self.scanTemplateSpan(right_brace_start, .template_tail, .template_middle) orelse
            error.NonTerminatedTemplateLiteral;
    }

    pub fn reScanGreaterThan(self: *Lexer, token_start: u32) Token {
        std.debug.assert(self.source[token_start] == '>');
        self.rewindTo(token_start);
        return self.puncToken(.greater_than, 1);
    }

    pub fn reScanLessThan(self: *Lexer, token_start: u32) Token {
        std.debug.assert(self.source[token_start] == '<');
        self.rewindTo(token_start);
        return self.puncToken(.less_than, 1);
    }

    pub fn reScanJsxText(self: *Lexer, start: u32) Token {
        self.state.token_flags = 0;
        self.cursor = findAnyPos("<{>}", self.source, start);
        return self.createToken(.jsx_text, start, self.cursor);
    }

    const RegexResult = struct {
        span: Span,
        pattern: []const u8,
        flags: []const u8,
    };

    pub fn reScanAsRegex(self: *Lexer, start: u32) LexicalError!RegexResult {
        const src = self.source;
        std.debug.assert(src[start] == '/');
        self.rewindTo(start + 1);

        var in_class = false;
        while (self.cursor < src.len) : (self.cursor += 1) {
            if (self.atLineBreak()) return error.InvalidRegexLineTerminator;
            switch (src[self.cursor]) {
                '\\' => {
                    self.cursor += 1;
                    if (self.cursor == src.len) break;
                    if (self.atLineBreak()) return error.InvalidRegexLineTerminator;
                },
                '[' => in_class = true,
                ']' => in_class = false,
                '/' => if (!in_class) {
                    const pattern_end = self.cursor;
                    self.cursor += 1;
                    while (self.cursor < src.len) {
                        const cp = try util.Utf.codePointAt(src, self.cursor);
                        if (!lexical.isIdentifierChar(cp.value)) break;
                        self.cursor += cp.len;
                    }
                    return .{
                        .span = .{ .start = start, .end = self.cursor },
                        .pattern = src[start + 1 .. pattern_end],
                        .flags = src[pattern_end + 1 .. self.cursor],
                    };
                },
                else => {},
            }
        }
        return error.UnterminatedRegexLiteral;
    }

    fn scanString(self: *Lexer) LexicalError!Token {
        const src = self.source;
        const start = self.cursor;
        const quote = src[start];
        std.debug.assert(quote == '"' or quote == '\'');

        var pos = start + 1;
        while (true) {
            pos = if (quote == '"')
                findAnyPos("\"\\\n\r", src, pos)
            else
                findAnyPos("'\\\n\r", src, pos);
            if (pos < src.len and src[pos] == quote) {
                self.cursor = pos + 1;
                return self.createToken(.string_literal, start, pos + 1);
            }
            self.cursor = pos;
            if (pos == src.len) return error.UnterminatedString;
            if (self.mode == .jsx_tag) {
                // a jsx attribute value has no escapes and may span lines
                pos += 1;
            } else if (src[pos] == '\\') {
                try self.consumeEscape(.string);
                pos = self.cursor;
            } else return error.UnterminatedString;
        }
    }

    fn scanTemplateLiteral(self: *Lexer) LexicalError!Token {
        const start = self.cursor;
        self.cursor += 1;
        return self.scanTemplateSpan(start, .no_substitution_template, .template_head) orelse {
            self.cursor = @intCast(self.source.len);
            return error.NonTerminatedTemplateLiteral;
        };
    }

    inline fn scanTemplateSpan(
        self: *Lexer,
        start: u32,
        comptime backtick_tag: TokenTag,
        comptime substitution_tag: TokenTag,
    ) ?Token {
        const src = self.source;
        var pos = self.cursor;
        while (true) {
            pos = findAnyPos("`\\$\r", src, pos);
            if (pos >= src.len) return null;
            switch (src[pos]) {
                '`' => {
                    self.cursor = pos + 1;
                    return self.createToken(backtick_tag, start, pos + 1);
                },
                '\\' => {
                    self.cursor = pos;
                    // a bad escape is not fatal, a tagged template cooks it to undefined
                    self.consumeEscape(.template) catch {
                        self.setTokenFlag(.invalid_escape);
                        if (self.cursor < src.len) self.cursor += 1;
                    };
                    pos = self.cursor;
                },
                '$' => {
                    if (pos + 1 < src.len and src[pos + 1] == '{') {
                        self.cursor = pos + 2;
                        return self.createToken(substitution_tag, start, pos + 2);
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

    fn consumeEscape(self: *Lexer, comptime context: enum { string, template }) !void {
        std.debug.assert(self.source[self.cursor] == '\\');
        self.cursor += 1;
        self.setTokenFlag(.escaped);
        if (self.cursor >= self.source.len) return error.UnterminatedString;

        switch (self.source[self.cursor]) {
            '0'...'7' => |digit| {
                // `\0` is a null escape only when no digit follows
                const is_null = digit == '0' and !std.ascii.isDigit(self.peek(1));
                if (context == .template and !is_null) return error.InvalidOctalEscape;
                self.cursor = @intCast(spec.escapes.parseOctal(self.source, self.cursor).end);
            },
            '8', '9' => {
                if (context == .template) return error.InvalidOctalEscape;
                self.cursor += 1;
            },
            'x' => {
                const hex = spec.escapes.parseHex(self.source, self.cursor + 1, 2) orelse
                    return error.InvalidHexEscape;
                self.cursor = @intCast(hex.end);
            },
            'u' => {
                const unit = try self.consumeUnicodeEscape(.any);
                if (!std.unicode.isSurrogateCodepoint(unit)) return;
                var is_paired = false;
                if (unit < 0xDC00 and self.peek(0) == '\\' and self.peek(1) == 'u') {
                    self.cursor += 1;
                    const next = try self.consumeUnicodeEscape(.any);
                    is_paired = next >= 0xDC00 and next <= 0xDFFF;
                }
                if (!is_paired) self.setTokenFlag(.lone_surrogates);
            },
            else => self.cursor += @max(1, line_terminators.lineBreakLen(self.source, self.cursor)),
        }
    }

    fn consumeUnicodeEscape(
        self: *Lexer,
        position: enum { identifier_start, identifier_part, any },
    ) !u21 {
        if (self.peek(0) != 'u') return error.InvalidUnicodeEscape;
        self.setTokenFlag(.escaped);
        const parsed = spec.escapes.parseUnicodeEscape(self.source, self.cursor + 1) orelse
            return error.InvalidUnicodeEscape;
        switch (position) {
            .identifier_start => if (!lexical.isIdentifierStart(parsed.value)) {
                return error.InvalidIdentifierStart;
            },
            .identifier_part => if (!lexical.isIdentifierChar(parsed.value)) {
                return error.InvalidIdentifierContinue;
            },
            .any => {},
        }
        self.cursor = @intCast(parsed.end);
        return parsed.value;
    }

    fn scanIdentifierOrKeyword(self: *Lexer) LexicalError!Token {
        const src = self.source;
        const start = self.cursor;
        const is_jsx_tag = self.mode == .jsx_tag;
        const is_private = src[start] == '#';
        const name_start = start + @intFromBool(is_private);
        const part_ascii = if (is_jsx_tag) &identifier_char_jsx_ascii else &identifier_char_ascii;

        var has_escape = false;
        var pos = name_start;
        while (pos < src.len) {
            const at_start = pos == name_start;
            const c = src[pos];
            self.cursor = pos;
            if (c == '\\') {
                if (is_jsx_tag) return if (at_start)
                    error.JsxIdentifierCannotStartWithBackslash
                else
                    error.JsxIdentifierCannotContainEscapes;
                has_escape = true;
                self.cursor = pos + 1;
                _ = try self.consumeUnicodeEscape(
                    if (at_start) .identifier_start else .identifier_part,
                );
                pos = self.cursor;
            } else if (c >= 0x80) {
                @branchHint(.cold);
                const cp = try util.Utf.codePointAt(src, pos);
                const is_valid = if (at_start)
                    lexical.isIdentifierStart(cp.value)
                else
                    lexical.isIdentifierChar(cp.value);
                if (!is_valid) break;
                pos += cp.len;
            } else if (if (at_start) identifier_start_ascii[c] else part_ascii[c]) {
                pos += 1;
                while (pos < src.len and part_ascii[src[pos]]) pos += 1;
            } else break;
        }
        self.cursor = pos;
        if (pos == name_start) return error.InvalidIdentifierStart;

        const lexeme = src[start..pos];
        const tag: TokenTag = if (is_jsx_tag)
            .jsx_identifier
        else if (is_private)
            .private_identifier
        else if (has_escape)
            escapedKeywordTag(lexeme)
        else
            keywordTag(lexeme);
        return self.createToken(tag, start, pos);
    }

    fn escapedKeywordTag(lexeme: []const u8) TokenTag {
        @branchHint(.cold);
        var buf: [256]u8 = undefined;
        return keywordTag(spec.escapes.decodeIdentifierEscapes(lexeme, &buf));
    }

    fn scanNumber(self: *Lexer) LexicalError!Token {
        const src = self.source;
        const start = self.cursor;
        var tag: TokenTag = .numeric_literal;
        var is_legacy = false;
        if (src[start] == '0') {
            self.cursor += 1;
            switch (self.peek(0)) {
                'x', 'X' => tag = try self.scanRadixDigits(16),
                'o', 'O' => tag = try self.scanRadixDigits(8),
                'b', 'B' => tag = try self.scanRadixDigits(2),
                '0'...'9' => {
                    // legacy octal `077` or decimal `089`, neither allows separators nor `n`
                    is_legacy = true;
                    tag = .octal_literal;
                    while (std.ascii.isDigit(self.peek(0))) : (self.cursor += 1) {
                        if (self.peek(0) >= '8') tag = .numeric_literal;
                    }
                },
                else => {},
            }
        } else try self.consumeDigits(10);

        var has_fraction = false;
        var has_exponent = false;
        if (tag == .numeric_literal) {
            has_fraction = self.peek(0) == '.';
            if (has_fraction) {
                self.cursor += 1;
                try self.consumeDigits(10);
            }
            has_exponent = self.peek(0) | 0x20 == 'e';
            if (has_exponent) {
                self.cursor += 1;
                if (self.peek(0) == '+' or self.peek(0) == '-') self.cursor += 1;
                const digits = self.cursor;
                try self.consumeDigits(10);
                if (self.cursor == digits) return error.InvalidExponentPart;
            }
        }

        if (!is_legacy and self.peek(0) == 'n') {
            if (has_exponent) return error.ExponentialBigInt;
            if (has_fraction) return error.NonIntegerBigInt;
            self.cursor += 1;
            tag = .bigint_literal;
        }

        const after = self.peek(0);
        if (identifier_start_ascii[after] or after == '\\') {
            return error.IdentifierAfterNumericLiteral;
        }
        return self.createToken(tag, start, self.cursor);
    }

    fn scanRadixDigits(self: *Lexer, comptime radix: u8) LexicalError!TokenTag {
        self.cursor += 1;
        const digits = self.cursor;
        try self.consumeDigits(radix);
        const is_empty = self.cursor == digits;
        return switch (radix) {
            16 => if (is_empty) error.InvalidHexLiteral else .hex_literal,
            8 => if (is_empty) error.InvalidOctalLiteralDigit else .octal_literal,
            else => if (is_empty) error.InvalidBinaryLiteral else .binary_literal,
        };
    }

    inline fn consumeDigits(self: *Lexer, comptime radix: u8) LexicalError!void {
        if (self.peek(0) == '_') return error.NumericSeparatorMisuse;
        while (true) : (self.cursor += 1) {
            const c = self.peek(0);
            if (isDigit(c, radix)) continue;
            if (c != '_') break;
            const next = self.peek(1);
            if (next == '_') {
                self.cursor += 1;
                return error.ConsecutiveNumericSeparators;
            }
            if (!isDigit(next, radix)) return error.NumericSeparatorMisuse;
        }
    }

    inline fn isDigit(c: u8, comptime radix: u8) bool {
        return if (radix == 16) std.ascii.isHex(c) else c -% '0' < radix;
    }

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

    const TriviaClass = enum(u8) { none, space, line_break, slash, less_than, minus, non_ascii };

    const trivia_class: [256]TriviaClass = blk: {
        var t: [256]TriviaClass = @splat(.none);
        for (" \t\x0B\x0C") |c| t[c] = .space;
        t['\n'] = .line_break;
        t['\r'] = .line_break;
        t['/'] = .slash;
        t['<'] = .less_than;
        t['-'] = .minus;
        for (0x80..256) |c| t[c] = .non_ascii;
        break :blk t;
    };

    inline fn skipTrivia(self: *Lexer) LexicalError!void {
        const src = self.source;
        const entry = self.cursor;
        if (entry < src.len and trivia_class[src[entry]] == .none) return;

        var pos = entry;
        while (pos < src.len) {
            if (src[pos] == ' ') {
                pos += 1;
                continue;
            }
            switch (trivia_class[src[pos]]) {
                .none => break,
                .space => pos += 1,
                .line_break => {
                    self.setTokenFlag(.line_terminator_before);
                    pos += 1;
                },
                .slash => {
                    self.cursor = pos;
                    pos = switch (self.peek(1)) {
                        '/' => try self.scanLineComment(pos, 2),
                        '*' => try self.scanBlockComment(pos),
                        else => break,
                    };
                },
                // annex B html comments are script-only
                .less_than => {
                    if (self.source_type == .module or pos + 3 >= src.len or
                        src[pos + 1] != '!' or src[pos + 2] != '-' or src[pos + 3] != '-') break;
                    pos = try self.scanLineComment(pos, 4);
                },
                .minus => {
                    // `-->` closes only at the start of a line
                    const at_line_start = entry == 0 or self.hasTokenFlag(.line_terminator_before);
                    if (self.source_type == .module or !at_line_start or pos + 2 >= src.len or
                        src[pos + 1] != '-' or src[pos + 2] != '>') break;
                    pos = try self.scanLineComment(pos, 3);
                },
                .non_ascii => {
                    @branchHint(.unlikely);
                    self.cursor = pos;
                    const cp = try util.Utf.codePointAt(src, pos);
                    switch (cp.value) {
                        '\u{2028}', '\u{2029}' => self.setTokenFlag(.line_terminator_before),
                        else => if (!isMultiByteSpace(cp.value)) break,
                    }
                    pos += cp.len;
                },
            }
        }
        self.cursor = pos;
    }

    inline fn recordComment(
        self: *Lexer,
        kind: ast.Comment.Type,
        start: u32,
        end: u32,
        head: u32,
    ) error{OutOfMemory}!void {
        if (!self.collect_comments) return;
        const tail: u32 = if (kind == .block) 2 else 0;
        try self.comments.append(self.allocator, .{
            .type = kind,
            .value = .{ .start = start + head, .end = end - tail },
            .span = .{ .start = start, .end = end },
        });
    }

    noinline fn scanLineComment(self: *Lexer, start: u32, comptime head: u32) !u32 {
        const src = self.source;
        var end = start + head;
        while (true) : (end += 1) {
            end = findAnyPos("\r\n\xe2", src, end);
            if (end == src.len or src[end] != 0xE2) break;
            if (line_terminators.unicodeSeparatorLen(src, end) > 0) break;
        }
        if (head == 4) {
            if (std.mem.findPosLinear(u8, src[0..end], start + head, "-->")) |close| {
                end = @intCast(close + 3);
            }
        }
        try self.recordComment(.line, start, end, head);
        return end;
    }

    fn scanBlockComment(self: *Lexer, start: u32) LexicalError!u32 {
        const src = self.source;
        var pos = start + 2;
        while (true) : (pos += 1) {
            pos = if (self.hasTokenFlag(.line_terminator_before))
                findAnyPos("*", src, pos)
            else
                findAnyPos("*\n\r\xe2", src, pos);
            if (pos + 1 >= src.len) break;
            if (src[pos] != '*') {
                if (line_terminators.lineBreakLen(src, pos) > 0) {
                    self.setTokenFlag(.line_terminator_before);
                }
            } else if (src[pos + 1] == '/') {
                try self.recordComment(.block, start, pos + 2, 2);
                return pos + 2;
            }
        }
        self.cursor = @intCast(src.len);
        return error.UnterminatedMultiLineComment;
    }

    pub inline fn createToken(self: *Lexer, tag: TokenTag, start: u32, end: u32) Token {
        std.debug.assert(start <= end);
        std.debug.assert(end <= self.source.len);
        defer self.state.token_flags = 0;
        return .{
            .tag = tag,
            .span = .{ .start = start, .end = end },
            .flags = self.state.token_flags,
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
