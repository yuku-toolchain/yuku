const std = @import("std");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const Span = @import("token.zig").Span;
const TokenFlag = @import("token.zig").TokenFlag;
const flagMask = @import("token.zig").flagMask;
const ast = @import("ast.zig");
const util = @import("util");
const extension = @import("extension.zig");

pub const LexicalError = error{
    UnterminatedString,
    UnterminatedRegex,
    NonTerminatedTemplateLiteral,
    UnterminatedRegexLiteral,
    InvalidRegexLineTerminator,
    InvalidRegex,
    InvalidRegexFlag,
    DuplicateRegexFlag,
    IncompatibleRegexFlags,
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
    MultipleDecimalPoints,
    InvalidBigIntSuffix,
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

/// First offset >= `from` at which `src` holds one of the comptime
/// `chars`, searched 16 bytes at a time; `src.len` on a miss.
fn findAnyPos(comptime chars: []const u8, src: []const u8, from: u32) u32 {
    const Vec = @Vector(16, u8);

    var i: usize = from;
    while (i + 16 <= src.len) : (i += 16) {
        const v: Vec = src[i..][0..16].*;
        var hit: @Vector(16, bool) = @splat(false);
        inline for (chars) |ch| {
            hit = hit | (v == @as(Vec, @splat(ch)));
        }
        const mask: u16 = @bitCast(hit);
        if (mask != 0) return @intCast(i + @ctz(mask));
    }
    if (i + 8 <= src.len) {
        const v: @Vector(8, u8) = src[i..][0..8].*;
        var hit: @Vector(8, bool) = @splat(false);
        inline for (chars) |ch| {
            hit = hit | (v == @as(@Vector(8, u8), @splat(ch)));
        }
        const mask: u8 = @bitCast(hit);
        if (mask != 0) return @intCast(i + @ctz(mask));
        i += 8;
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

pub const Lexer = struct {
    /// Comments in source order, populated only when `collect_comments` is set.
    comments: std.ArrayList(ast.Comment),
    collect_comments: bool,
    allocator: std.mem.Allocator,
    state: LexerState,
    mode: LexerMode = .normal,

    source: []const u8,
    cursor: u32,

    source_type: ast.SourceType,
    hashbang: ?struct { start: u32, len: u16 } = null,

    pub fn init(
        source: []const u8,
        allocator: std.mem.Allocator,
        source_type: ast.SourceType,
        collect_comments: bool,
    ) error{OutOfMemory}!Lexer {
        std.debug.assert(source.len <= std.math.maxInt(u32));

        var self: Lexer = .{
            .source = source,
            .state = .{},
            .cursor = 0,
            .comments = .empty,
            .collect_comments = collect_comments,
            .allocator = allocator,
            .source_type = source_type,
        };

        self.skipHashbang();

        std.debug.assert(self.cursor <= self.source.len);
        return self;
    }

    fn skipHashbang(self: *Lexer) void {
        std.debug.assert(self.cursor == 0);

        if (self.source.len >= 2 and self.source[0] == '#' and self.source[1] == '!') {
            var end: u32 = 2;
            while (end < self.source.len and
                self.source[end] != '\n' and
                self.source[end] != '\r')
            {
                end += 1;
            }
            std.debug.assert(end >= 2);
            std.debug.assert(end <= self.source.len);
            self.hashbang = .{ .start = 2, .len = @intCast(end - 2) };
            self.cursor = end;
        }
    }

    pub fn nextToken(self: *Lexer) LexicalError!Token {
        std.debug.assert(self.cursor <= self.source.len);

        self.clearTokenFlags();

        try self.skipWsAndComments();

        std.debug.assert(self.cursor <= self.source.len);

        if (self.cursor >= self.source.len) {
            return self.createToken(.eof, self.cursor, self.cursor);
        }

        const current_char = self.source[self.cursor];

        if (ident_start_table_ascii[current_char] and self.mode == .normal) {
            return self.scanAsciiIdentifier() orelse try self.scanIdentifierOrKeyword();
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

    pub inline fn tryNextToken(self: *Lexer) ?Token {
        std.debug.assert(self.cursor <= self.source.len);

        if (self.mode != .normal) return null;

        const src = self.source;
        if (self.cursor >= src.len) return null;

        if (src[self.cursor] == ' ') {
            self.cursor += 1;
            if (self.cursor >= src.len) return null;
        }

        const c0 = src[self.cursor];
        if (ws_class[c0] != 0) return null;

        if (ident_start_table_ascii[c0]) return self.scanAsciiIdentifier();
        if (simple_punct_tag[c0] != .eof) return self.scanSimplePunctuation();

        if (c0 == '.') {
            const c1 = self.peek(1);
            if (c1 != '.' and !std.ascii.isDigit(c1)) {
                return self.puncToken(1, .dot, self.cursor);
            }
        }

        return null;
    }

    inline fn scanAsciiIdentifier(self: *Lexer) ?Token {
        const src = self.source;
        const start = self.cursor;

        var pos = start + 1;
        while (pos < src.len and ident_continue_table_ascii[src[pos]]) {
            pos += 1;
        }
        if (pos < src.len and (src[pos] == '\\' or src[pos] >= 0x80)) return null;

        self.cursor = pos;
        const first = src[start];
        const len = pos - start;
        const tag: TokenTag = if (first >= 'a' and first <= 'z' and len >= 2 and len <= 11)
            self.getKeywordType(src[start..pos])
        else
            .identifier;
        return self.createToken(tag, start, pos);
    }

    const simple_punct_tag: [256]TokenTag = blk: {
        var t: [256]TokenTag = @splat(.eof);
        t['~'] = .bitwise_not;
        t['('] = .left_paren;
        t[')'] = .right_paren;
        t['{'] = .left_brace;
        t['}'] = .right_brace;
        t['['] = .left_bracket;
        t[']'] = .right_bracket;
        t[';'] = .semicolon;
        t[','] = .comma;
        t[':'] = .colon;
        t['@'] = .at;
        break :blk t;
    };

    inline fn scanSimplePunctuation(self: *Lexer) Token {
        std.debug.assert(self.cursor < self.source.len);

        const start = self.cursor;
        const tag = simple_punct_tag[self.source[start]];
        self.cursor = start + 1;

        std.debug.assert(tag != .eof);
        return self.createToken(tag, start, start + 1);
    }

    inline fn puncToken(self: *Lexer, len: u32, tag: TokenTag, start: u32) Token {
        std.debug.assert(len > 0);
        std.debug.assert(start == self.cursor);
        std.debug.assert(start + len <= self.source.len);

        self.cursor += len;
        return self.createToken(tag, start, self.cursor);
    }

    fn scanPunctuation(self: *Lexer) LexicalError!Token {
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
            // inside a jsx tag `>` only closes the tag, so `<div>=</div>` and the
            // trailing `>>` of `<div attr=<e></e>></div>` must not merge
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
        if (idx >= self.source.len) {
            return 0;
        }
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
        return util.Utf.lineBreakLen(self.source, self.cursor) > 0;
    }

    /// Scans the `template_middle` or `template_tail` that starts at the `}` closing
    /// a substitution.
    pub fn reScanTemplateContinuation(self: *Lexer, right_brace_start: u32) LexicalError!Token {
        std.debug.assert(right_brace_start < self.source.len);
        std.debug.assert(self.source[right_brace_start] == '}');

        self.rewindTo(right_brace_start);

        const start = self.cursor;

        self.cursor += 1;

        while (self.cursor < self.source.len) {
            const c = self.source[self.cursor];
            if (c == '\\') {
                try self.consumeEscape(.template);
                continue;
            }
            if (c == '`') {
                self.cursor += 1;
                return self.createToken(.template_tail, start, self.cursor);
            }
            if (c == '$' and self.peek(1) == '{') {
                self.cursor += 2;
                return self.createToken(.template_middle, start, self.cursor);
            }
            // a raw CR must be normalized in the cooked value, so it counts as escaped
            if (c == '\r') self.setTokenFlag(.escaped);
            self.cursor += 1;
        }
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
    pub fn reScanJsxText(self: *Lexer, initial_cursor: u32) Token {
        std.debug.assert(initial_cursor <= self.source.len);
        self.rewindTo(initial_cursor);

        const start = self.cursor;

        while (self.cursor < self.source.len) {
            const c = self.source[self.cursor];
            if (extension.at(.jsx_text_boundary, .{ self.source, self.cursor })) |ends| {
                if (ends) break;
            }

            switch (c) {
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
    pub fn reScanAsRegex(self: *Lexer, slash_token_start: u32) LexicalError!RegexResult {
        std.debug.assert(slash_token_start < self.source.len);
        std.debug.assert(self.source[slash_token_start] == '/');

        self.rewindTo(slash_token_start);

        const start = self.cursor;
        var closing_delimeter_pos: u32 = 0;
        self.cursor += 1;
        var in_class = false;

        while (self.cursor < self.source.len) {
            const c = self.source[self.cursor];

            if (self.isLineTerminator()) {
                return error.InvalidRegexLineTerminator;
            }

            if (c == '\\') {
                self.cursor += 1;

                if (self.cursor >= self.source.len) {
                    return error.UnterminatedRegexLiteral;
                }

                if (self.isLineTerminator()) {
                    return error.InvalidRegexLineTerminator;
                }

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

                closing_delimeter_pos = self.cursor;

                var flags_seen: u32 = 0;

                while (true) {
                    const flag = self.peek(0);
                    if (!std.ascii.isAlphabetic(flag)) break;

                    const is_valid_flag = switch (flag) {
                        'g', 'i', 'm', 's', 'u', 'y', 'd', 'v' => true,
                        else => false,
                    };

                    if (!is_valid_flag) {
                        return error.InvalidRegexFlag;
                    }

                    const bit: u5 = @intCast(flag - 'a');

                    if ((flags_seen & (@as(u32, 1) << bit)) != 0) {
                        return error.DuplicateRegexFlag;
                    }

                    flags_seen |= (@as(u32, 1) << bit);

                    self.cursor += 1;
                }

                const u_bit = @as(u32, 1) << ('u' - 'a');
                const v_bit = @as(u32, 1) << ('v' - 'a');

                if (flags_seen & u_bit != 0 and flags_seen & v_bit != 0) {
                    return error.IncompatibleRegexFlags;
                }

                const end = self.cursor;

                const pattern = self.source[start + 1 .. closing_delimeter_pos - 1];

                const flags = self.source[closing_delimeter_pos..end];

                return .{
                    .span = .{ .start = start, .end = end },
                    .pattern = pattern,
                    .flags = flags,
                };
            }

            self.cursor += 1;
        }
        return error.UnterminatedRegexLiteral;
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
                const c = src[pos];

                if (c == quote) {
                    pos += 1;
                    self.cursor = pos;
                    return self.createToken(.string_literal, start, pos);
                }

                if (c == '\\') {
                    self.cursor = pos;
                    try self.consumeEscape(.string);
                    pos = self.cursor;
                    continue;
                }

                if (c == '\n' or c == '\r') {
                    self.cursor = pos;
                    return error.UnterminatedString;
                }

                pos += 1;
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

        while (self.cursor < self.source.len) {
            const c = self.source[self.cursor];

            if (c == '\\') {
                try self.consumeEscape(.template);
                continue;
            }

            if (c == '`') {
                self.cursor += 1;
                return self.createToken(.no_substitution_template, start, self.cursor);
            }

            if (c == '$' and self.peek(1) == '{') {
                self.cursor += 2;
                return self.createToken(.template_head, start, self.cursor);
            }

            // a raw CR must be normalized in the cooked value, so it counts as escaped
            if (c == '\r') self.setTokenFlag(.escaped);

            self.cursor += 1;
        }

        return error.NonTerminatedTemplateLiteral;
    }

    const EscapeContext = enum {
        string,
        template,
    };

    // a bad template escape is not fatal, tagged templates still get a token with an
    // undefined cooked value
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

        if (self.cursor >= self.source.len) {
            return error.UnterminatedString;
        }

        const c = self.source[self.cursor];

        brk: switch (c) {
            '0' => {
                const c1 = self.peek(1);

                // `\0` is a null escape only when no digit follows
                if (!std.ascii.isDigit(c1)) {
                    self.cursor += 1;
                    break :brk;
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
            else => {
                const us_len = util.Utf.unicodeSeparatorLen(self.source, self.cursor);

                if (us_len > 0) {
                    self.cursor += us_len;
                    break :brk;
                }

                self.cursor += 1;
            },
        }
    }

    fn consumeOctal(self: *Lexer) LexicalError!void {
        std.debug.assert(self.cursor < self.source.len);
        const result = util.Utf.parseOctal(self.source, self.cursor);
        if (result.end == self.cursor) {
            return error.InvalidOctalEscape;
        }
        std.debug.assert(result.end > self.cursor);
        self.cursor = @intCast(result.end);
    }

    fn consumeHex(self: *Lexer) LexicalError!void {
        std.debug.assert(self.cursor < self.source.len);
        if (util.Utf.parseHex2(self.source, self.cursor + 1)) |r| {
            std.debug.assert(r.end >= self.cursor + 1);
            self.cursor = @intCast(r.end);
        } else {
            return error.InvalidHexEscape;
        }
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

        const parsed = util.Utf.parseUnicodeEscape(self.source, self.cursor + 1) orelse
            return error.InvalidUnicodeEscape;

        switch (context) {
            .identifier_start => {
                if (!util.UnicodeId.canStartId(parsed.value)) {
                    return error.InvalidIdentifierStart;
                }
            },
            .identifier_continue => {
                if (!util.UnicodeId.canContinueId(parsed.value)) {
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
        const c2 = self.peek(2);

        if (c1 >= '0' and c1 <= '9') {
            return self.scanNumber();
        }
        if (c1 == '.' and c2 == '.') {
            return self.puncToken(3, .spread, start);
        }
        return self.puncToken(1, .dot, start);
    }

    const ident_start_table_ascii: [256]bool = blk: {
        var t = [_]bool{false} ** 256;
        for ('a'..('z' + 1)) |c| t[c] = true;
        for ('A'..('Z' + 1)) |c| t[c] = true;
        t['_'] = true;
        t['$'] = true;
        break :blk t;
    };

    const ident_continue_table_ascii: [256]bool = blk: {
        var t = [_]bool{false} ** 256;
        for ('a'..('z' + 1)) |c| t[c] = true;
        for ('A'..('Z' + 1)) |c| t[c] = true;
        for ('0'..('9' + 1)) |c| t[c] = true;
        t['_'] = true;
        t['$'] = true;
        break :blk t;
    };

    const ident_continue_jsx_table_ascii: [256]bool = blk: {
        var t = ident_continue_table_ascii;
        t['-'] = true;
        break :blk t;
    };

    fn scanIdentifierBody(self: *Lexer, is_jsx_tag: bool) !bool {
        std.debug.assert(self.cursor <= self.source.len);

        var has_escape = false;

        const table = if (is_jsx_tag)
            &ident_continue_jsx_table_ascii
        else
            &ident_continue_table_ascii;

        const src = self.source;
        var pos = self.cursor;
        const entry_pos = pos;

        while (true) {
            while (pos < src.len and table[src[pos]]) {
                pos += 1;
            }

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

                if (util.UnicodeId.canContinueId(cp.value)) {
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

        var has_escape = false;

        const is_private = self.peek(0) == '#';

        if (is_private) {
            self.cursor += 1;
        }

        const first_char = self.peek(0);

        if (std.ascii.isAscii(first_char)) {
            @branchHint(.likely);

            if (first_char == '\\') {
                if (is_jsx_tag) {
                    return error.JsxIdentifierCannotStartWithBackslash;
                }

                has_escape = true;

                self.cursor += 1;

                _ = try self.consumeUnicodeEscape(.identifier_start);
            } else {
                if (!ident_start_table_ascii[first_char]) {
                    @branchHint(.cold);
                    return error.InvalidIdentifierStart;
                }
                self.cursor += 1;
            }

            const body_has_escape = try self.scanIdentifierBody(is_jsx_tag);
            has_escape = has_escape or body_has_escape;
        } else {
            @branchHint(.cold);

            const c_cp = try util.Utf.codePointAt(self.source, self.cursor);

            if (!util.UnicodeId.canStartId(c_cp.value)) {
                return error.InvalidIdentifierStart;
            }

            self.cursor += c_cp.len;

            has_escape = try self.scanIdentifierBody(is_jsx_tag);
        }

        std.debug.assert(self.cursor > start);
        const lexeme = self.source[start..self.cursor];

        const tag: TokenTag = if (is_jsx_tag)
            .jsx_identifier
        else if (is_private)
            .private_identifier
        else if (has_escape)
            self.getEscapedKeywordType(lexeme)
        else
            self.getKeywordType(lexeme);

        return self.createToken(tag, start, self.cursor);
    }

    fn getEscapedKeywordType(self: *Lexer, lexeme: []const u8) TokenTag {
        @branchHint(.cold);
        std.debug.assert(lexeme.len > 0);
        var buf: [11]u8 = undefined; // keyword_length_max
        var out: usize = 0;
        var i: usize = 0;
        while (i < lexeme.len) {
            if (out == buf.len) return .identifier;
            if (lexeme[i] == '\\' and i + 1 < lexeme.len and lexeme[i + 1] == 'u') {
                const parsed = util.Utf.parseUnicodeEscape(lexeme, i + 2) orelse return .identifier;
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
        return self.getKeywordType(buf[0..out]);
    }

    const keyword_list = [_]struct { []const u8, TokenTag }{
        .{ "if", .@"if" },                .{ "of", .of },                 .{ "in", .in },
        .{ "do", .do },                   .{ "as", .as },                 .{ "is", .is },
        .{ "any", .any },                 .{ "for", .@"for" },            .{ "get", .get },
        .{ "let", .let },                 .{ "new", .new },               .{ "out", .out },
        .{ "set", .set },                 .{ "try", .@"try" },            .{ "var", .@"var" },
        .{ "case", .case },               .{ "this", .this },             .{ "else", .@"else" },
        .{ "enum", .@"enum" },            .{ "void", .void },             .{ "with", .with },
        .{ "null", .null_literal },       .{ "type", .type },             .{ "true", .true },
        .{ "from", .from },               .{ "await", .await },           .{ "async", .async },
        .{ "break", .@"break" },          .{ "const", .@"const" },        .{ "class", .class },
        .{ "catch", .@"catch" },          .{ "defer", .@"defer" },        .{ "false", .false },
        .{ "infer", .infer },             .{ "keyof", .keyof },           .{ "never", .never },
        .{ "super", .super },             .{ "throw", .throw },           .{ "using", .using },
        .{ "while", .@"while" },          .{ "yield", .yield },           .{ "assert", .assert },
        .{ "bigint", .bigint },           .{ "delete", .delete },         .{ "export", .@"export" },
        .{ "global", .global },           .{ "import", .import },         .{ "module", .module },
        .{ "number", .number },           .{ "object", .object },         .{ "public", .public },
        .{ "return", .@"return" },        .{ "string", .string },         .{ "symbol", .symbol },
        .{ "switch", .@"switch" },        .{ "static", .static },         .{ "source", .source },
        .{ "typeof", .typeof },           .{ "unique", .unique },         .{ "asserts", .asserts },
        .{ "boolean", .boolean },         .{ "default", .default },       .{ "declare", .declare },
        .{ "extends", .extends },         .{ "finally", .finally },       .{ "private", .private },
        .{ "package", .package },         .{ "require", .require },       .{ "unknown", .unknown },
        .{ "accessor", .accessor },       .{ "abstract", .abstract },     .{ "continue", .@"continue" },
        .{ "debugger", .debugger },       .{ "function", .function },     .{ "override", .override },
        .{ "readonly", .readonly },       .{ "interface", .interface },   .{ "intrinsic", .intrinsic },
        .{ "namespace", .namespace },     .{ "protected", .protected },   .{ "satisfies", .satisfies },
        .{ "undefined", .undefined },     .{ "instanceof", .instanceof }, .{ "implements", .implements },
        .{ "constructor", .constructor },
    };

    const keyword_length_min = 2;
    const keyword_length_max = 11;

    const KeywordEntry = struct {
        name: [keyword_length_max]u8,
        len: u8,
        tag: TokenTag,
    };

    // perfect hash over (first, second, last, length). the multipliers were found by
    // offline search so every keyword lands in a distinct slot of the 512-entry table
    inline fn keywordHash(c0: u8, c1: u8, c_last: u8, length: usize) u32 {
        const h = @as(u32, c0) * 56 + @as(u32, c1) * 97 +
            @as(u32, c_last) * 108 + @as(u32, @intCast(length)) * 117;
        return h & 511;
    }

    const keyword_table: [512]KeywordEntry = blk: {
        @setEvalBranchQuota(20_000);
        var t: [512]KeywordEntry = @splat(.{ .name = @splat(0), .len = 0, .tag = .identifier });
        for (keyword_list) |kv| {
            const name, const keyword_tag = kv;
            std.debug.assert(name.len >= keyword_length_min);
            std.debug.assert(name.len <= keyword_length_max);
            const h = keywordHash(name[0], name[1], name[name.len - 1], name.len);
            if (t[h].len != 0) @compileError("keyword perfect hash collision: " ++ name);
            var entry: KeywordEntry = .{ .name = @splat(0), .len = name.len, .tag = keyword_tag };
            @memcpy(entry.name[0..name.len], name);
            t[h] = entry;
        }
        break :blk t;
    };

    fn getKeywordType(_: *Lexer, lexeme: []const u8) TokenTag {
        if (lexeme.len < keyword_length_min or lexeme.len > keyword_length_max) {
            return .identifier;
        }
        const h = keywordHash(lexeme[0], lexeme[1], lexeme[lexeme.len - 1], lexeme.len);
        const entry = &keyword_table[h];
        if (entry.len != lexeme.len) return .identifier;
        if (!std.mem.eql(u8, entry.name[0..lexeme.len], lexeme)) return .identifier;
        return entry.tag;
    }

    fn scanNumber(self: *Lexer) LexicalError!Token {
        std.debug.assert(self.cursor < self.source.len);
        const lead = self.source[self.cursor];
        std.debug.assert(lead == '.' or std.ascii.isDigit(lead));

        const start = self.cursor;
        var tag: TokenTag = .numeric_literal;
        var has_decimal_or_exponent = false;
        var is_leading_zero = false;

        if (self.peek(0) == '0') {
            self.cursor += 1;

            switch (self.peek(0)) {
                'x', 'X' => {
                    tag = .hex_literal;
                    self.cursor += 1;
                    const digits_start = self.cursor;
                    try self.consumeHexDigits();
                    if (self.cursor == digits_start) return error.InvalidHexLiteral;
                },
                'o', 'O' => {
                    tag = .octal_literal;
                    self.cursor += 1;
                    const digits_start = self.cursor;
                    try self.consumeOctalDigits();
                    if (self.cursor == digits_start) return error.InvalidOctalLiteralDigit;
                },
                'b', 'B' => {
                    tag = .binary_literal;
                    self.cursor += 1;
                    const digits_start = self.cursor;
                    try self.consumeBinaryDigits();
                    if (self.cursor == digits_start) return error.InvalidBinaryLiteral;
                },
                '0'...'9' => {
                    is_leading_zero = true;
                    // legacy octal `077` or decimal `089`, neither allows separators
                    var is_legacy_octal = true;
                    while (self.cursor < self.source.len) {
                        const c = self.source[self.cursor];
                        if (c >= '0' and c <= '9') {
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
            const next = self.peek(1);
            if (next == '_') return error.NumericSeparatorMisuse;
            self.cursor += 1;
            has_decimal_or_exponent = true;
            if (next >= '0' and next <= '9') try self.consumeDecimalDigits();
        }

        if (tag == .numeric_literal) {
            const exp = self.peek(0);
            if (exp == 'e' or exp == 'E') {
                has_decimal_or_exponent = true;
                try self.consumeExponent();
            }
        }

        if (!is_leading_zero and self.peek(0) == 'n') {
            if (has_decimal_or_exponent) return error.InvalidBigIntSuffix;
            self.cursor += 1;
            tag = .bigint_literal;
        }

        const c = self.peek(0);
        if (ident_start_table_ascii[c] or c == '\\') return error.IdentifierAfterNumericLiteral;
        std.debug.assert(self.cursor > start);
        return self.createToken(tag, start, self.cursor);
    }

    inline fn consumeDigits(self: *Lexer, comptime isValidDigit: fn (u8) bool) LexicalError!void {
        std.debug.assert(self.cursor <= self.source.len);

        if (self.cursor < self.source.len and self.source[self.cursor] == '_') {
            return error.NumericSeparatorMisuse;
        }

        var last_was_separator = false;

        while (self.cursor < self.source.len) {
            const c = self.source[self.cursor];
            if (isValidDigit(c)) {
                self.cursor += 1;
                last_was_separator = false;
            } else if (c == '_') {
                if (last_was_separator) {
                    return error.ConsecutiveNumericSeparators;
                }
                self.cursor += 1;
                last_was_separator = true;
            } else {
                break;
            }
        }

        if (last_was_separator) {
            return error.NumericSeparatorMisuse;
        }
    }

    inline fn consumeDecimalDigits(self: *Lexer) LexicalError!void {
        return self.consumeDigits(std.ascii.isDigit);
    }

    inline fn consumeHexDigits(self: *Lexer) LexicalError!void {
        return self.consumeDigits(std.ascii.isHex);
    }

    inline fn consumeOctalDigits(self: *Lexer) LexicalError!void {
        return self.consumeDigits(util.Utf.isOctalDigit);
    }

    inline fn consumeBinaryDigits(self: *Lexer) LexicalError!void {
        const isBinary = comptime struct {
            fn check(c: u8) bool {
                return c == '0' or c == '1';
            }
        }.check;

        return self.consumeDigits(isBinary);
    }

    fn consumeExponent(self: *Lexer) LexicalError!void {
        std.debug.assert(self.cursor < self.source.len);
        std.debug.assert(self.source[self.cursor] == 'e' or self.source[self.cursor] == 'E');

        self.cursor += 1;

        const c = self.peek(0);

        if (c == '+' or c == '-') {
            self.cursor += 1;
        }

        const exp_start = self.cursor;
        try self.consumeDecimalDigits();

        if (self.cursor == exp_start) {
            return error.InvalidExponentPart;
        }
    }

    const ws_class: [256]u8 = blk: {
        var t: [256]u8 = @splat(0);
        t[' '] = 1;
        t['\t'] = 1;
        t[0x0B] = 1;
        t[0x0C] = 1;
        t['\n'] = 2;
        t['\r'] = 2;
        t['/'] = 3;
        t['<'] = 4;
        t['-'] = 5;
        for (0x80..256) |i| t[i] = 6;
        break :blk t;
    };

    fn isMultiByteSpace(cp: u21) bool {
        return switch (cp) {
            '\u{FEFF}',
            '\u{00A0}',
            // U+0085 NEXT LINE is not WhiteSpace in the spec, but tsc scans it as a space (not a line break, so no ASI)
            '\u{0085}',
            '\u{2000}',
            '\u{2001}'...'\u{200A}',
            '\u{202F}',
            '\u{205F}',
            '\u{3000}',
            '\u{1680}',
            => true,
            else => false,
        };
    }

    inline fn skipWsAndComments(self: *Lexer) LexicalError!void {
        std.debug.assert(self.cursor <= self.source.len);

        if (self.cursor < self.source.len and ws_class[self.source[self.cursor]] == 0) {
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
            switch (ws_class[src[pos]]) {
                1 => pos += 1,
                2 => {
                    self.setTokenFlag(.line_terminator_before);
                    can_be_html_close_comment = true;
                    pos += 1;
                },
                3 => {
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
                4 => {
                    // annex B html comments are script-only
                    if (self.source_type == .module or pos + 3 >= src.len or
                        src[pos + 1] != '!' or src[pos + 2] != '-' or src[pos + 3] != '-') break;
                    self.cursor = pos;
                    try self.scanHtmlComment();
                    pos = self.cursor;
                },
                5 => {
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
                6 => {
                    @branchHint(.unlikely);
                    const us_len = util.Utf.unicodeSeparatorLen(src, pos);
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
                else => break,
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
    ) LexicalError!void {
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
        self.comments.append(self.allocator, .{
            .type = @"type",
            .value = .{ .start = start + head, .end = end - tail },
            .span = .{ .start = start, .end = end },
        }) catch return error.OutOfMemory;
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
            if (c == 0xE2 and util.Utf.unicodeSeparatorLen(src, pos) > 0) break;
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
        while (pos < src.len) {
            const c = src[pos];
            switch (c) {
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
                0x80...0xFF => {
                    const lt_len = util.Utf.unicodeSeparatorLen(src, pos);
                    if (lt_len > 0) {
                        self.setTokenFlag(.line_terminator_before);
                        pos += lt_len;
                        break;
                    } else pos += 1;
                },
                else => pos += 1,
            }
        }
        // multi-line body: vectorized search for the two-byte '*/'
        // sequence (star and slash masks combined per lane). line leads
        // are " * " - star without a slash - so they never restart the
        // scan. windows overlap by one byte to catch a straddling '*/'.
        var w = pos - 1;
        while (w + 16 <= src.len) {
            const v: @Vector(16, u8) = src[w..][0..16].*;
            var stars: @Vector(16, bool) = @splat(false);
            var slashes: @Vector(16, bool) = @splat(false);
            stars = stars | (v == @as(@Vector(16, u8), @splat('*')));
            slashes = slashes | (v == @as(@Vector(16, u8), @splat('/')));
            const ends: u16 = @as(u16, @bitCast(stars)) & (@as(u16, @bitCast(slashes)) >> 1);
            if (ends != 0) {
                self.cursor = @intCast(w + @ctz(ends) + 2);
                try self.recordComment(.block, start, self.cursor);
                return;
            }
            w += 15;
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
        error.UnterminatedRegex => "Unterminated regular expression",
        error.NonTerminatedTemplateLiteral => "Unterminated template literal",
        error.UnterminatedRegexLiteral => "Unterminated regular expression literal",
        error.InvalidRegexLineTerminator => "Line terminator not allowed in regular expression" ++
            " literal",
        error.InvalidRegex => "Invalid regular expression",
        error.InvalidRegexFlag => "Invalid regular expression flag",
        error.DuplicateRegexFlag => "Duplicate regular expression flag",
        error.IncompatibleRegexFlags => "The 'u' and 'v' regular expression flags cannot be" ++
            " used together",
        error.InvalidIdentifierStart => "Invalid character at start of identifier",
        error.InvalidIdentifierContinue => "Invalid character in identifier",
        error.UnterminatedMultiLineComment => "Unterminated multi-line comment",
        error.InvalidUnicodeEscape => "Invalid Unicode escape sequence",
        error.InvalidOctalEscape => "Invalid octal escape sequence",
        error.InvalidOctalLiteralDigit => "Octal literal must contain at least one octal digit",
        error.InvalidBinaryLiteral => "Binary literal must contain at least one binary digit",
        error.InvalidHexLiteral => "Hexadecimal literal must contain at least one hex digit",
        error.InvalidExponentPart => "Exponent part is missing a number",
        error.NumericSeparatorMisuse => "Numeric separator is only allowed between two digits",
        error.ConsecutiveNumericSeparators => "Numeric literal cannot contain consecutive" ++
            " separators",
        error.MultipleDecimalPoints => "Numeric literal cannot contain multiple decimal points",
        error.InvalidBigIntSuffix => "BigInt literal cannot contain decimal point or exponent",
        error.IdentifierAfterNumericLiteral => "Identifier cannot immediately follow a" ++
            " numeric literal",
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
        error.InvalidHexEscape => "Try adding two hexadecimal digits here (e.g., \\x41 for 'A')",
        error.UnterminatedString => "Try adding a closing quote here to complete the string",
        error.UnterminatedRegex => "Try adding a closing slash (/) here to complete the regex",
        error.NonTerminatedTemplateLiteral => "Try adding a closing backtick (`) here to" ++
            " complete the template",
        error.UnterminatedRegexLiteral => "Try adding a closing slash (/) here, optionally" ++
            " followed by flags (g, i, m, etc.)",
        error.InvalidRegexLineTerminator => "Try removing the line break here or escaping" ++
            " it within the regex pattern",
        error.InvalidRegex => "Try checking the regex syntax here for unclosed groups," ++
            " invalid escapes, or malformed patterns",
        error.InvalidRegexFlag => "Valid regex flags are: `g` (global), `i` (ignoreCase)," ++
            " `m` (multiline), `s` (dotAll), `u` (unicode), `y` (sticky), `d` (hasIndices)," ++
            " `v` (setNotation)",
        error.DuplicateRegexFlag => "Remove the duplicate flag; each flag can only appear once",
        error.IncompatibleRegexFlags => "The 'u' (unicode) and 'v' (unicodeSets) flags are" ++
            " mutually exclusive; use one or the other",
        error.InvalidIdentifierStart => "Try starting the identifier here with a letter" ++
            " (a-z, A-Z), underscore (_), or dollar sign ($)",
        error.InvalidIdentifierContinue => "Try using a valid identifier character here" ++
            " (letters, digits, underscore, or dollar sign)",
        error.UnterminatedMultiLineComment => "Try adding the closing delimiter (*/) here" ++
            " to complete the comment",
        error.InvalidUnicodeEscape => "Try using \\uHHHH (4 hex digits) or \\u{HHHHHH}" ++
            " (1-6 hex digits) here",
        error.InvalidOctalEscape => "Try using a valid octal sequence here (\\0-7, \\00-77," ++
            " or \\000-377)",
        error.InvalidOctalLiteralDigit => "Try adding at least one octal digit (0-7) here" ++
            " after '0o'",
        error.InvalidBinaryLiteral => "Try adding at least one binary digit (0 or 1) here" ++
            " after '0b'",
        error.InvalidHexLiteral => "Try adding at least one hex digit (0-9, a-f, A-F) here" ++
            " after '0x'",
        error.InvalidExponentPart => "Try adding digits here after the exponent" ++
            " (e.g., e10, e-5, E+2)",
        error.NumericSeparatorMisuse => "Try placing the separator between two digits," ++
            " or removing it",
        error.ConsecutiveNumericSeparators => "Try removing one of the consecutive underscores" ++
            " here",
        error.MultipleDecimalPoints => "Try removing the extra decimal point here",
        error.InvalidBigIntSuffix => "Try removing the 'n' suffix here, or remove the decimal" ++
            " point/exponent from the number",
        error.IdentifierAfterNumericLiteral => "Try adding whitespace here between the number" ++
            " and identifier",
        error.InvalidUtf8 => "The source file contains invalid UTF-8 encoding. Ensure the" ++
            " file is saved with valid UTF-8 encoding",
        error.OutOfMemory => "The system ran out of memory while parsing",
        error.JsxIdentifierCannotContainEscapes => "Remove the escape sequence and use the" ++
            " literal character instead",
        error.JsxIdentifierCannotStartWithBackslash => "JSX tag names must be plain" ++
            " identifiers without escape sequences",
    };
}
