const std = @import("std");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const Span = @import("token.zig").Span;
const TokenFlag = @import("token.zig").TokenFlag;
const flagMask = @import("token.zig").flagMask;
const ast = @import("ast.zig");
const util = @import("util");

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
    // jsx-specific errors
    JsxIdentifierCannotContainEscapes,
    JsxIdentifierCannotStartWithBackslash,
};

pub const LexerMode = enum {
    /// normal javascript mode
    normal,
    /// jsx tag context: allows hyphens in identifiers, emits jsx_identifier tokens,
    /// disables escape sequences in both identifiers and string literals,
    /// allows newlines in string literals, and treats '>>' as two separate '>' tokens
    jsx_tag,
};

pub const LexerState = struct {
    /// metadata that should be attached to the next emitted token.
    token_flags: u8 = 0,
};

pub const Lexer = struct {
    /// comments in source order, populated only when `collect_comments` is true
    comments: std.ArrayList(ast.Comment),
    collect_comments: bool,
    allocator: std.mem.Allocator,
    state: LexerState,
    mode: LexerMode = .normal,

    source: []const u8,
    /// current byte index being scanned in the source
    cursor: u32,

    source_type: ast.SourceType,
    hashbang: ?struct { start: u32, len: u16 } = null,

    pub fn init(
        source: []const u8,
        allocator: std.mem.Allocator,
        source_type: ast.SourceType,
        collect_comments: bool,
    ) error{OutOfMemory}!Lexer {
        // span positions are u32
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
        // hashbang only legal at byte 0
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

        // fast path, plain ASCII identifier in normal mode
        if (ident_start_table_ascii[current_char] and self.mode == .normal) {
            const start = self.cursor;
            const src = self.source;
            var pos = start + 1;
            while (pos < src.len and ident_continue_table_ascii[src[pos]]) {
                pos += 1;
            }
            if (pos >= src.len or (src[pos] != '\\' and src[pos] < 0x80)) {
                self.cursor = pos;
                return self.createToken(self.getKeywordType(src[start..pos]), start, pos);
            }
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
            '~', '(', ')', '{', '[', ']', ';', ',', ':', '@' => self.scanSimplePunctuation(),
            '}' => self.handleRightBrace(),
            else => self.scanIdentifierOrKeyword(),
        };
    }

    inline fn scanSimplePunctuation(self: *Lexer) Token {
        std.debug.assert(self.cursor < self.source.len);

        const start = self.cursor;
        const c = self.source[self.cursor];
        self.cursor += 1;

        const tag: TokenTag = switch (c) {
            '~' => .bitwise_not,
            '(' => .left_paren,
            ')' => .right_paren,
            '{' => .left_brace,
            '[' => .left_bracket,
            ']' => .right_bracket,
            ';' => .semicolon,
            ',' => .comma,
            ':' => .colon,
            '@' => .at,
            else => unreachable,
        };

        std.debug.assert(self.cursor == start + 1);
        return self.createToken(tag, start, self.cursor);
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

        // dispatched from nextToken's punctuation lead-char set
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
            '>' => if (c1 == '>' and c2 == '=')
                self.puncToken(3, .right_shift_assign, start)
            else if (c1 == '>' and c2 == '>')
                if (c3 == '=')
                    self.puncToken(4, .unsigned_right_shift_assign, start)
                else
                    self.puncToken(3, .unsigned_right_shift, start)
            else switch (c1) {
                '>' => {
                    // in jsx, the trailing `>>` of `<div attr=<elem></elem>></div>` must lex
                    // as two separate `>` tokens so the parser can balance the inner element
                    if (self.mode == .jsx_tag) {
                        return self.puncToken(1, .greater_than, start);
                    } else {
                        return self.puncToken(2, .right_shift, start);
                    }
                },
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

    inline fn isLineTerminator(self: *const Lexer, c: u8) bool {
        std.debug.assert(self.cursor <= self.source.len);
        return c == '\n' or c == '\r' or
            (c == 0xE2 and util.Utf.unicodeSeparatorLen(self.source, self.cursor) > 0);
    }

    // functions exclusively called by the parser for context-specific lexing

    /// scans template_middle or template_tail.
    /// called by the parser when it expects a template continuation after parsing
    /// an expression inside ${}.
    pub fn reScanTemplateContinuation(self: *Lexer, right_brace_start: u32) LexicalError!Token {
        std.debug.assert(right_brace_start < self.source.len);
        std.debug.assert(self.source[right_brace_start] == '}');

        self.rewindTo(right_brace_start);

        const start = self.cursor;

        // consume '}' of the expression
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
            // raw CR requires cooked value normalization
            if (c == '\r') self.setTokenFlag(.escaped);
            self.cursor += 1;
        }
        return error.NonTerminatedTemplateLiteral;
    }

    /// splits `>>`, `>>>`, `>=`, `>>=`, `>>>=` into a leading `>` so
    /// nested type argument lists like `Foo<Bar<T>>` can close.
    pub fn reScanGreaterThan(self: *Lexer, token_start: u32) Token {
        std.debug.assert(token_start < self.source.len);
        std.debug.assert(self.source[token_start] == '>');
        self.rewindTo(token_start + 1);
        return self.createToken(.greater_than, token_start, token_start + 1);
    }

    /// splits `<<` into a leading `<` so nested generics like
    /// `Foo<<T>(x: T) => R>` can open.
    pub fn reScanLessThan(self: *Lexer, token_start: u32) Token {
        std.debug.assert(token_start < self.source.len);
        std.debug.assert(self.source[token_start] == '<');
        self.rewindTo(token_start + 1);
        return self.createToken(.less_than, token_start, token_start + 1);
    }

    /// scans JSX text content between '<' and '{' in JSX children.
    /// called by the parser when parsing JSX element children.
    pub fn reScanJsxText(self: *Lexer, initial_cursor: u32) Token {
        std.debug.assert(initial_cursor <= self.source.len);
        self.rewindTo(initial_cursor);

        const start = self.cursor;

        while (self.cursor < self.source.len) {
            const c = self.source[self.cursor];

            switch (c) {
                '<', '{' => break,
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

    /// re-scans a slash token as a regex literal
    /// called by the parser when context determines that a '/' token should be interpreted
    /// as the start of a regular expression rather than a division operator
    pub fn reScanAsRegex(self: *Lexer, slash_token_start: u32) LexicalError!RegexResult {
        std.debug.assert(slash_token_start < self.source.len);
        std.debug.assert(self.source[slash_token_start] == '/');

        self.rewindTo(slash_token_start);

        const start = self.cursor;
        var closing_delimeter_pos: u32 = 0;
        self.cursor += 1; // consume '/'
        var in_class = false;

        while (self.cursor < self.source.len) {
            const c = self.source[self.cursor];

            if (self.isLineTerminator(c)) {
                return error.InvalidRegexLineTerminator;
            }

            if (c == '\\') {
                self.cursor += 1; // consume '\'

                if (self.cursor >= self.source.len) {
                    return error.UnterminatedRegexLiteral;
                }

                if (self.isLineTerminator(self.source[self.cursor])) {
                    return error.InvalidRegexLineTerminator;
                }

                self.cursor += 1; // consume escaped char
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

                // 26 bits enough, 'a' to 'z'
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

                // u and v flags are mutually exclusive (ES2024)
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

    //

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
            // jsx tag mode, no escapes, newlines allowed in attribute values
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

            // raw CR requires cooked value normalization
            if (c == '\r') self.setTokenFlag(.escaped);

            self.cursor += 1;
        }

        return error.NonTerminatedTemplateLiteral;
    }

    const EscapeContext = enum {
        string,
        template,
    };

    // string escapes are fatal; template escapes set `.invalid_escape` and keep lexing
    // so tagged templates can still produce a token (cooked becomes null/undefined,
    // untagged templates get a diagnostic later)
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

        self.cursor += 1; // skip backslash
        self.setTokenFlag(.escaped);

        if (self.cursor >= self.source.len) {
            return error.UnterminatedString;
        }

        const c = self.source[self.cursor];

        brk: switch (c) {
            '0' => {
                const c1 = self.peek(1);

                // null escape \0 [lookahead ∉ DecimalDigit]
                if (!std.ascii.isDigit(c1)) {
                    self.cursor += 1;
                    break :brk;
                }

                // octal escape: \0 followed by a digit
                if (context == .template) return error.InvalidOctalEscape;
                try self.consumeOctal();
            },
            'x' => try self.consumeHex(),
            'u' => {
                const cp = try self.consumeUnicodeEscape(.normal);

                // check for lone surrogates
                if (std.unicode.isSurrogateCodepoint(cp)) {
                    const is_high_paired = std.unicode.utf16IsHighSurrogate(@intCast(cp)) and
                        self.peek(0) == '\\' and
                        self.peek(1) == 'u';
                    if (is_high_paired) {
                        self.cursor += 1; // skip backslash
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

        // marks the token so the parser can reject escaped reserved keywords
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

    fn handleRightBrace(self: *Lexer) Token {
        std.debug.assert(self.cursor < self.source.len);
        std.debug.assert(self.source[self.cursor] == '}');
        const start = self.cursor;
        self.cursor += 1;
        return self.createToken(.right_brace, start, self.cursor);
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
                // jsx tag names don't support escape sequences
                if (is_jsx_tag) {
                    self.cursor = pos;
                    return error.JsxIdentifierCannotContainEscapes;
                }

                has_escape = true;

                self.cursor = pos + 1; // consume backslash to get to 'u'

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
                // jsx tag names don't support escape sequences
                if (is_jsx_tag) {
                    return error.JsxIdentifierCannotStartWithBackslash;
                }

                has_escape = true;

                self.cursor += 1; // consume backslash to get to 'u'

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

    /// determines keyword type for an identifier that contains unicode escapes.
    fn getEscapedKeywordType(self: *Lexer, lexeme: []const u8) TokenTag {
        @branchHint(.cold);
        std.debug.assert(lexeme.len > 0);
        var buf: [11]u8 = undefined; // max keyword length
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

    // keyword lookup dispatched first by length and then by the most discriminating byte.
    // each candidate matches its full lexeme via `std.mem.eql`; the compiler folds the
    // length check (the outer switch already pinned `lexeme.len`). the switch shape is
    // preserved so dispatch stays a jump table
    fn getKeywordType(_: *Lexer, lexeme: []const u8) TokenTag {
        const eql = std.mem.eql;
        switch (lexeme.len) {
            2 => switch (lexeme[1]) {
                'f' => return switch (lexeme[0]) {
                    'i' => .@"if",
                    'o' => .of,
                    else => .identifier,
                },
                'n' => if (lexeme[0] == 'i') return .in,
                'o' => if (lexeme[0] == 'd') return .do,
                's' => {
                    if (lexeme[0] == 'a') return .as;
                    if (lexeme[0] == 'i') return .is;
                },
                else => {},
            },
            3 => switch (lexeme[0]) {
                'a' => if (eql(u8, lexeme, "any")) return .any,
                'f' => if (eql(u8, lexeme, "for")) return .@"for",
                'g' => if (eql(u8, lexeme, "get")) return .get,
                'l' => if (eql(u8, lexeme, "let")) return .let,
                'n' => if (eql(u8, lexeme, "new")) return .new,
                'o' => if (eql(u8, lexeme, "out")) return .out,
                's' => if (eql(u8, lexeme, "set")) return .set,
                't' => if (eql(u8, lexeme, "try")) return .@"try",
                'v' => if (eql(u8, lexeme, "var")) return .@"var",
                else => {},
            },
            4 => switch (lexeme[1]) {
                'a' => if (eql(u8, lexeme, "case")) return .case,
                'h' => if (eql(u8, lexeme, "this")) return .this,
                'l' => if (eql(u8, lexeme, "else")) return .@"else",
                'n' => if (eql(u8, lexeme, "enum")) return .@"enum",
                'o' => if (eql(u8, lexeme, "void")) return .void,
                'i' => if (eql(u8, lexeme, "with")) return .with,
                'u' => if (eql(u8, lexeme, "null")) return .null_literal,
                'y' => if (eql(u8, lexeme, "type")) return .type,
                'r' => {
                    if (eql(u8, lexeme, "true")) return .true;
                    if (eql(u8, lexeme, "from")) return .from;
                },
                else => {},
            },
            5 => switch (lexeme[0]) {
                'a' => {
                    if (eql(u8, lexeme, "await")) return .await;
                    if (eql(u8, lexeme, "async")) return .async;
                },
                'b' => if (eql(u8, lexeme, "break")) return .@"break",
                'c' => {
                    if (eql(u8, lexeme, "const")) return .@"const";
                    if (eql(u8, lexeme, "class")) return .class;
                    if (eql(u8, lexeme, "catch")) return .@"catch";
                },
                'd' => if (eql(u8, lexeme, "defer")) return .@"defer",
                'f' => if (eql(u8, lexeme, "false")) return .false,
                'i' => if (eql(u8, lexeme, "infer")) return .infer,
                'k' => if (eql(u8, lexeme, "keyof")) return .keyof,
                'n' => if (eql(u8, lexeme, "never")) return .never,
                's' => if (eql(u8, lexeme, "super")) return .super,
                't' => if (eql(u8, lexeme, "throw")) return .throw,
                'u' => if (eql(u8, lexeme, "using")) return .using,
                'w' => if (eql(u8, lexeme, "while")) return .@"while",
                'y' => if (eql(u8, lexeme, "yield")) return .yield,
                else => {},
            },
            6 => switch (lexeme[0]) {
                'a' => if (eql(u8, lexeme, "assert")) return .assert,
                'b' => if (eql(u8, lexeme, "bigint")) return .bigint,
                'd' => if (eql(u8, lexeme, "delete")) return .delete,
                'e' => if (eql(u8, lexeme, "export")) return .@"export",
                'g' => if (eql(u8, lexeme, "global")) return .global,
                'i' => if (eql(u8, lexeme, "import")) return .import,
                'm' => if (eql(u8, lexeme, "module")) return .module,
                'n' => if (eql(u8, lexeme, "number")) return .number,
                'o' => if (eql(u8, lexeme, "object")) return .object,
                'p' => if (eql(u8, lexeme, "public")) return .public,
                'r' => if (eql(u8, lexeme, "return")) return .@"return",
                's' => {
                    if (eql(u8, lexeme, "string")) return .string;
                    if (eql(u8, lexeme, "symbol")) return .symbol;
                    if (eql(u8, lexeme, "switch")) return .@"switch";
                    if (eql(u8, lexeme, "static")) return .static;
                    if (eql(u8, lexeme, "source")) return .source;
                },
                't' => if (eql(u8, lexeme, "typeof")) return .typeof,
                'u' => if (eql(u8, lexeme, "unique")) return .unique,
                else => {},
            },
            7 => switch (lexeme[0]) {
                'a' => if (eql(u8, lexeme, "asserts")) return .asserts,
                'b' => if (eql(u8, lexeme, "boolean")) return .boolean,
                'd' => {
                    if (eql(u8, lexeme, "default")) return .default;
                    if (eql(u8, lexeme, "declare")) return .declare;
                },
                'e' => if (eql(u8, lexeme, "extends")) return .extends,
                'f' => if (eql(u8, lexeme, "finally")) return .finally,
                'p' => {
                    if (eql(u8, lexeme, "private")) return .private;
                    if (eql(u8, lexeme, "package")) return .package;
                },
                'r' => if (eql(u8, lexeme, "require")) return .require,
                'u' => if (eql(u8, lexeme, "unknown")) return .unknown,
                else => {},
            },
            8 => switch (lexeme[0]) {
                'a' => {
                    if (eql(u8, lexeme, "accessor")) return .accessor;
                    if (eql(u8, lexeme, "abstract")) return .abstract;
                },
                'c' => if (eql(u8, lexeme, "continue")) return .@"continue",
                'd' => if (eql(u8, lexeme, "debugger")) return .debugger,
                'f' => if (eql(u8, lexeme, "function")) return .function,
                'o' => if (eql(u8, lexeme, "override")) return .override,
                'r' => if (eql(u8, lexeme, "readonly")) return .readonly,
                else => {},
            },
            9 => switch (lexeme[0]) {
                'i' => {
                    if (eql(u8, lexeme, "interface")) return .interface;
                    if (eql(u8, lexeme, "intrinsic")) return .intrinsic;
                },
                'n' => if (eql(u8, lexeme, "namespace")) return .namespace,
                'p' => if (eql(u8, lexeme, "protected")) return .protected,
                's' => if (eql(u8, lexeme, "satisfies")) return .satisfies,
                'u' => if (eql(u8, lexeme, "undefined")) return .undefined,
                else => {},
            },
            10 => switch (lexeme[0]) {
                'i' => {
                    if (eql(u8, lexeme, "instanceof")) return .instanceof;
                    if (eql(u8, lexeme, "implements")) return .implements;
                },
                else => {},
            },
            11 => {
                if (eql(u8, lexeme, "constructor")) return .constructor;
            },
            else => {},
        }
        return .identifier;
    }

    fn scanNumber(self: *Lexer) LexicalError!Token {
        std.debug.assert(self.cursor < self.source.len);
        // dispatched from nextToken or scanDot, both gate on a digit (or `.<digit>`)
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
                    // legacy octal (077) or decimal with leading zero (089)
                    // no separators allowed in legacy syntax
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

        // a separator cannot start a digit sequence
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

        self.cursor += 1; // skip 'e' or 'E'

        // handle optional sign: + or -
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

    // byte classification for whitespace/comment scanning
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

    inline fn skipWsAndComments(self: *Lexer) LexicalError!void {
        std.debug.assert(self.cursor <= self.source.len);

        var can_be_html_close_comment =
            self.cursor == 0 or self.hasTokenFlag(.line_terminator_before);

        const src = self.source;
        var pos = self.cursor;
        const entry_pos = pos;

        while (pos < src.len) {
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
                    // html-style `<!--` only valid in script mode
                    if (self.source_type != .script or pos + 3 >= src.len or
                        src[pos + 1] != '!' or src[pos + 2] != '-' or src[pos + 3] != '-') break;
                    self.cursor = pos;
                    try self.scanHtmlComment();
                    pos = self.cursor;
                },
                5 => {
                    // html-style `-->` only valid at line start in script mode
                    if (self.source_type != .script or
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
                    if (!util.Utf.isMultiByteSpace(cp.value)) break;
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
        // delimiter widths: `//` `/*` are 2, `<!--` is 4, `-->` is 3 (no tail)
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
        while (pos < src.len) {
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
                },
                0x80...0xFF => {
                    const lt_len = util.Utf.unicodeSeparatorLen(src, pos);
                    if (lt_len > 0) {
                        self.setTokenFlag(.line_terminator_before);
                        pos += lt_len;
                    } else pos += 1;
                },
                else => pos += 1,
            }
        }
        self.cursor = pos;
        return error.UnterminatedMultiLineComment;
    }

    fn scanHtmlComment(self: *Lexer) LexicalError!void {
        std.debug.assert(self.source_type == .script);
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
            if (self.isLineTerminator(c)) break;
            self.cursor += 1;
        }
        try self.recordComment(.line, start, self.cursor);
    }

    fn scanHtmlCloseComment(self: *Lexer) LexicalError!void {
        std.debug.assert(self.source_type == .script);
        std.debug.assert(self.cursor + 2 < self.source.len);
        std.debug.assert(self.source[self.cursor] == '-');
        std.debug.assert(self.source[self.cursor + 1] == '-');
        std.debug.assert(self.source[self.cursor + 2] == '>');
        const start = self.cursor;
        self.cursor += 3;
        while (self.cursor < self.source.len) : (self.cursor += 1) {
            if (self.isLineTerminator(self.source[self.cursor])) break;
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
        error.NumericSeparatorMisuse => "Numeric separator cannot appear at the end of a" ++
            " numeric literal",
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
        error.NumericSeparatorMisuse => "Try removing the trailing underscore here or" ++
            " adding more digits after it",
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
