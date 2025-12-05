const std = @import("std");
const token = @import("token.zig");
const util = @import("util");

pub const LexicalError = error{
    UnterminatedString,
    UnterminatedRegex,
    NonTerminatedTemplateLiteral,
    UnterminatedRegexLiteral,
    InvalidRegexLineTerminator,
    InvalidRegex,
    InvalidIdentifierStart,
    UnterminatedMultiLineComment,
    InvalidUnicodeEscape,
    InvalidHexEscape,
    InvalidOctalEscape,
    OctalEscapeInStrict,
    InvalidBinaryLiteral,
    InvalidOctalLiteralDigit,
    InvalidHexLiteral,
    InvalidExponentPart,
    NumericSeparatorMisuse,
    ConsecutiveNumericSeparators,
    MultipleDecimalPoints,
    InvalidBigIntSuffix,
    InvalidUtf8,
};

// TODO:
// [ ] some simd optimizations

pub const Lexer = struct {
    strict_mode: bool,
    source: []const u8,
    source_len: u32,
    /// token start position, retained for lexical error recovery if scan fails
    token_start: u32,
    /// current byte index being scanned in the source
    cursor: u32,
    template_depth: u32,
    arena: std.heap.ArenaAllocator,
    has_line_terminator_before: bool,

    pub fn init(backing_allocator: std.mem.Allocator, source: []const u8) Lexer {
        return .{
            .strict_mode = false,
            .source = source,
            .source_len = @intCast(source.len),
            .token_start = 0,
            .cursor = 0,
            .template_depth = 0,
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
            .has_line_terminator_before = false,
        };
    }

    pub fn deinit(self: *Lexer) void {
        self.arena.deinit();
    }

    inline fn peek(self: *const Lexer, offset: u32) u8 {
        if (offset > self.source_len or self.cursor >= self.source_len - offset) {
            return 0;
        }
        return self.source[self.cursor + offset];
    }

    pub fn nextToken(self: *Lexer) LexicalError!token.Token {
        try self.skipSkippable();

        if (self.cursor >= self.source_len) {
            return self.createToken(.EOF, "", self.cursor, self.cursor);
        }

        self.token_start = self.cursor;
        const current_char = self.source[self.cursor];

        return switch (current_char) {
            '+', '*', '-', '!', '<', '>', '=', '|', '&', '^', '%', '/', '?' => self.scanPunctuation(),
            '.' => self.scanDot(),
            '0'...'9' => try self.scanNumber(),
            '"', '\'' => self.scanString(),
            '`' => self.scanTemplateLiteral(),
            '~', '(', ')', '{', '[', ']', ';', ',', ':' => self.scanSimplePunctuation(),
            '}' => self.handleRightBrace(),
            else => try self.scanIdentifierOrKeyword(),
        };
    }

    inline fn scanSimplePunctuation(self: *Lexer) token.Token {
        const start = self.cursor;
        const c = self.source[self.cursor];
        self.cursor += 1;

        const token_type: token.TokenType = switch (c) {
            '~' => .BitwiseNot,
            '(' => .LeftParen,
            ')' => .RightParen,
            '{' => .LeftBrace,
            '[' => .LeftBracket,
            ']' => .RightBracket,
            ';' => .Semicolon,
            ',' => .Comma,
            ':' => .Colon,
            else => unreachable,
        };

        return self.createToken(token_type, self.source[start..self.cursor], start, self.cursor);
    }

    fn scanPunctuation(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        const c0 = self.source[self.cursor];
        const c1 = self.peek(1);
        const c2 = self.peek(2);
        const c3 = self.peek(3);

        return switch (c0) {
            '+' => switch (c1) {
                '+' => blk: {
                    self.cursor += 2;
                    break :blk self.createToken(.Increment, self.source[start..self.cursor], start, self.cursor);
                },
                '=' => blk: {
                    self.cursor += 2;
                    break :blk self.createToken(.PlusAssign, self.source[start..self.cursor], start, self.cursor);
                },
                else => blk: {
                    self.cursor += 1;
                    break :blk self.createToken(.Plus, self.source[start..self.cursor], start, self.cursor);
                },
            },
            '-' => switch (c1) {
                '-' => blk: {
                    self.cursor += 2;
                    break :blk self.createToken(.Decrement, self.source[start..self.cursor], start, self.cursor);
                },
                '=' => blk: {
                    self.cursor += 2;
                    break :blk self.createToken(.MinusAssign, self.source[start..self.cursor], start, self.cursor);
                },
                else => blk: {
                    self.cursor += 1;
                    break :blk self.createToken(.Minus, self.source[start..self.cursor], start, self.cursor);
                },
            },
            '*' => blk: {
                if (c1 == '*' and c2 == '=') {
                    self.cursor += 3;
                    break :blk self.createToken(.ExponentAssign, self.source[start..self.cursor], start, self.cursor);
                }
                break :blk switch (c1) {
                    '*' => blk2: {
                        self.cursor += 2;
                        break :blk2 self.createToken(.Exponent, self.source[start..self.cursor], start, self.cursor);
                    },
                    '=' => blk2: {
                        self.cursor += 2;
                        break :blk2 self.createToken(.StarAssign, self.source[start..self.cursor], start, self.cursor);
                    },
                    else => blk2: {
                        self.cursor += 1;
                        break :blk2 self.createToken(.Star, self.source[start..self.cursor], start, self.cursor);
                    },
                };
            },
            '/' => blk: {
                if (c1 == '=') {
                    self.cursor += 2;
                    break :blk self.createToken(.SlashAssign, self.source[start..self.cursor], start, self.cursor);
                }

                self.cursor += 1;

                break :blk self.createToken(.Slash, self.source[start..self.cursor], start, self.cursor);
            },
            '%' => switch (c1) {
                '=' => blk: {
                    self.cursor += 2;
                    break :blk self.createToken(.PercentAssign, self.source[start..self.cursor], start, self.cursor);
                },
                else => blk: {
                    self.cursor += 1;
                    break :blk self.createToken(.Percent, self.source[start..self.cursor], start, self.cursor);
                },
            },
            '<' => blk: {
                if (c1 == '<' and c2 == '=') {
                    self.cursor += 3;
                    break :blk self.createToken(.LeftShiftAssign, self.source[start..self.cursor], start, self.cursor);
                }
                break :blk switch (c1) {
                    '<' => blk2: {
                        self.cursor += 2;
                        break :blk2 self.createToken(.LeftShift, self.source[start..self.cursor], start, self.cursor);
                    },
                    '=' => blk2: {
                        self.cursor += 2;
                        break :blk2 self.createToken(.LessThanEqual, self.source[start..self.cursor], start, self.cursor);
                    },
                    else => blk2: {
                        self.cursor += 1;
                        break :blk2 self.createToken(.LessThan, self.source[start..self.cursor], start, self.cursor);
                    },
                };
            },
            '>' => blk: {
                if (c1 == '>' and c2 == '=') {
                    self.cursor += 3;
                    break :blk self.createToken(.RightShiftAssign, self.source[start..self.cursor], start, self.cursor);
                }
                if (c1 == '>' and c2 == '>') {
                    if (c3 == '=') {
                        self.cursor += 4;
                        break :blk self.createToken(.UnsignedRightShiftAssign, self.source[start..self.cursor], start, self.cursor);
                    } else {
                        self.cursor += 3;
                        break :blk self.createToken(.UnsignedRightShift, self.source[start..self.cursor], start, self.cursor);
                    }
                }
                break :blk switch (c1) {
                    '>' => blk2: {
                        self.cursor += 2;
                        break :blk2 self.createToken(.RightShift, self.source[start..self.cursor], start, self.cursor);
                    },
                    '=' => blk2: {
                        self.cursor += 2;
                        break :blk2 self.createToken(.GreaterThanEqual, self.source[start..self.cursor], start, self.cursor);
                    },
                    else => blk2: {
                        self.cursor += 1;
                        break :blk2 self.createToken(.GreaterThan, self.source[start..self.cursor], start, self.cursor);
                    },
                };
            },
            '=' => blk: {
                if (c1 == '=' and c2 == '=') {
                    self.cursor += 3;
                    break :blk self.createToken(.StrictEqual, self.source[start..self.cursor], start, self.cursor);
                }
                break :blk switch (c1) {
                    '=' => blk2: {
                        self.cursor += 2;
                        break :blk2 self.createToken(.Equal, self.source[start..self.cursor], start, self.cursor);
                    },
                    '>' => blk2: {
                        self.cursor += 2;
                        break :blk2 self.createToken(.Arrow, self.source[start..self.cursor], start, self.cursor);
                    },
                    else => blk2: {
                        self.cursor += 1;
                        break :blk2 self.createToken(.Assign, self.source[start..self.cursor], start, self.cursor);
                    },
                };
            },
            '!' => blk: {
                if (c1 == '=' and c2 == '=') {
                    self.cursor += 3;
                    break :blk self.createToken(.StrictNotEqual, self.source[start..self.cursor], start, self.cursor);
                }
                break :blk switch (c1) {
                    '=' => blk2: {
                        self.cursor += 2;
                        break :blk2 self.createToken(.NotEqual, self.source[start..self.cursor], start, self.cursor);
                    },
                    else => blk2: {
                        self.cursor += 1;
                        break :blk2 self.createToken(.LogicalNot, self.source[start..self.cursor], start, self.cursor);
                    },
                };
            },
            '&' => blk: {
                if (c1 == '&' and c2 == '=') {
                    self.cursor += 3;
                    break :blk self.createToken(.LogicalAndAssign, self.source[start..self.cursor], start, self.cursor);
                }
                break :blk switch (c1) {
                    '&' => blk2: {
                        self.cursor += 2;
                        break :blk2 self.createToken(.LogicalAnd, self.source[start..self.cursor], start, self.cursor);
                    },
                    '=' => blk2: {
                        self.cursor += 2;
                        break :blk2 self.createToken(.BitwiseAndAssign, self.source[start..self.cursor], start, self.cursor);
                    },
                    else => blk2: {
                        self.cursor += 1;
                        break :blk2 self.createToken(.BitwiseAnd, self.source[start..self.cursor], start, self.cursor);
                    },
                };
            },
            '|' => blk: {
                if (c1 == '|' and c2 == '=') {
                    self.cursor += 3;
                    break :blk self.createToken(.LogicalOrAssign, self.source[start..self.cursor], start, self.cursor);
                }
                break :blk switch (c1) {
                    '|' => blk2: {
                        self.cursor += 2;
                        break :blk2 self.createToken(.LogicalOr, self.source[start..self.cursor], start, self.cursor);
                    },
                    '=' => blk2: {
                        self.cursor += 2;
                        break :blk2 self.createToken(.BitwiseOrAssign, self.source[start..self.cursor], start, self.cursor);
                    },
                    else => blk2: {
                        self.cursor += 1;
                        break :blk2 self.createToken(.BitwiseOr, self.source[start..self.cursor], start, self.cursor);
                    },
                };
            },
            '^' => switch (c1) {
                '=' => blk: {
                    self.cursor += 2;
                    break :blk self.createToken(.BitwiseXorAssign, self.source[start..self.cursor], start, self.cursor);
                },
                else => blk: {
                    self.cursor += 1;
                    break :blk self.createToken(.BitwiseXor, self.source[start..self.cursor], start, self.cursor);
                },
            },
            '?' => blk: {
                if (c1 == '?' and c2 == '=') {
                    self.cursor += 3;
                    break :blk self.createToken(.NullishAssign, self.source[start..self.cursor], start, self.cursor);
                }
                break :blk switch (c1) {
                    '?' => blk2: {
                        self.cursor += 2;
                        break :blk2 self.createToken(.NullishCoalescing, self.source[start..self.cursor], start, self.cursor);
                    },
                    '.' => blk2: {
                        self.cursor += 2;
                        break :blk2 self.createToken(.OptionalChaining, self.source[start..self.cursor], start, self.cursor);
                    },
                    else => blk2: {
                        self.cursor += 1;
                        break :blk2 self.createToken(.Question, self.source[start..self.cursor], start, self.cursor);
                    },
                };
            },
            else => unreachable,
        };
    }

    fn scanString(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        const quote = self.source[start];
        self.cursor += 1;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];

            if (c == '\\') {
                try self.consumeEscape();
                continue;
            }

            if (c == quote) {
                self.cursor += 1;
                return self.createToken(.StringLiteral, self.source[start..self.cursor], start, self.cursor);
            }

            if (c == '\n' or c == '\r' or c == '\u{2028}' or c == '\u{2029}') {
                return error.UnterminatedString;
            }

            self.cursor += 1;
        }

        return error.UnterminatedString;
    }

    fn scanTemplateLiteral(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        self.cursor += 1;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];

            if (c == '\\') {
                try self.consumeEscape();
                continue;
            }

            if (c == '`') {
                self.cursor += 1;
                const end = self.cursor;
                return self.createToken(.NoSubstitutionTemplate, self.source[start..end], start, end);
            }

            if (c == '$' and self.peek(1) == '{') {
                self.cursor += 2;
                const end = self.cursor;
                self.template_depth += 1;
                return self.createToken(.TemplateHead, self.source[start..end], start, end);
            }

            self.cursor += 1;
        }

        return error.NonTerminatedTemplateLiteral;
    }

    fn scanTemplateMiddleOrTail(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        self.cursor += 1;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];
            if (c == '\\') {
                try self.consumeEscape();
                continue;
            }
            if (c == '`') {
                self.cursor += 1;
                const end = self.cursor;
                if (self.template_depth > 0) {
                    self.template_depth -= 1;
                }
                return self.createToken(.TemplateTail, self.source[start..end], start, end);
            }
            if (c == '$' and self.peek(1) == '{') {
                self.cursor += 2;
                const end = self.cursor;
                return self.createToken(.TemplateMiddle, self.source[start..end], start, end);
            }
            self.cursor += 1;
        }
        return error.NonTerminatedTemplateLiteral;
    }

    fn consumeEscape(self: *Lexer) LexicalError!void {
        self.cursor += 1; // skip backslash

        if (self.cursor >= self.source_len) {
            return error.UnterminatedString;
        }

        brk: switch (self.source[self.cursor]) {
            '0' => {
                const c1 = self.peek(1);

                if (!util.isOctalDigit(c1)) {
                    self.cursor += 1; // null escape
                    break :brk;
                }

                if (self.strict_mode) return error.OctalEscapeInStrict;
                try self.consumeOctal();
            },
            'x' => {
                try self.consumeHex();
            },
            'u' => {
                try self.consumeUnicodeEscape();
            },
            '1'...'7' => {
                if (self.strict_mode) return error.OctalEscapeInStrict;
                try self.consumeOctal();
            },
            else => {
                self.cursor += 1;
            },
        }
    }

    fn consumeOctal(self: *Lexer) LexicalError!void {
        var count: u8 = 0;

        while (self.cursor < self.source_len and count < 3) {
            const c = self.source[self.cursor];

            if (util.isOctalDigit(c)) {
                self.cursor += 1;
                count += 1;
            } else {
                break;
            }
        }

        if (count == 0) return error.InvalidOctalEscape;
    }

    fn consumeHex(self: *Lexer) LexicalError!void {
        const c1 = self.peek(1);
        const c2 = self.peek(2);

        if (!std.ascii.isHex(c1) or !std.ascii.isHex(c2)) {
            return error.InvalidHexEscape;
        }

        self.cursor += 3;
    }

    fn consumeUnicodeEscape(self: *Lexer) LexicalError!void {
        self.cursor += 1; // skip 'u'

        if (self.cursor < self.source_len and self.source[self.cursor] == '{') {
            // \u{XXXXX} format
            self.cursor += 1;
            const start = self.cursor;
            const end = std.mem.indexOfScalarPos(u8, self.source, self.cursor, '}') orelse
                return error.InvalidUnicodeEscape;

            const hex_len = end - start;
            if (hex_len == 0 or hex_len > 6) {
                return error.InvalidUnicodeEscape;
            }

            for (self.source[start..end]) |c| {
                if (!std.ascii.isHex(c)) {
                    return error.InvalidUnicodeEscape;
                }
            }

            self.cursor = @intCast(end + 1);
        } else {
            // \uXXXX format
            const end = self.cursor + 4;
            if (end > self.source_len) {
                return error.InvalidUnicodeEscape;
            }

            for (self.source[self.cursor..end]) |c| {
                if (!std.ascii.isHex(c)) {
                    return error.InvalidUnicodeEscape;
                }
            }

            self.cursor = end;
        }
    }

    fn handleRightBrace(self: *Lexer) LexicalError!token.Token {
        // inside template, scan for continuation
        if (self.template_depth > 0) {
            return self.scanTemplateMiddleOrTail();
        }

        const start = self.cursor;
        self.cursor += 1;
        return self.createToken(.RightBrace, self.source[start..self.cursor], start, self.cursor);
    }

    pub fn reScanAsRegex(self: *Lexer, slash_token: token.Token) LexicalError!struct { span: token.Span, pattern: []const u8, flags: []const u8, lexeme: []const u8 } {
        self.cursor = slash_token.span.start;

        const start = self.cursor;
        var closing_delimeter_pos: u32 = 0;
        self.cursor += 1; // consume '/'
        var in_class = false;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];

            if (c == '\n' or c == '\r') {
                return error.InvalidRegexLineTerminator;
            }

            if (c == '\\') {
                self.cursor += 1; // skip backslash
                if (self.cursor < self.source_len) {
                    self.cursor += 1; // skip escaped char
                }
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

                while (self.cursor < self.source_len and
                    std.ascii.isAlphabetic(self.source[self.cursor]))
                {
                    self.cursor += 1;
                }

                const end = self.cursor;

                const pattern = self.source[start + 1 .. closing_delimeter_pos - 1];
                const flags = self.source[closing_delimeter_pos..end];

                return .{ .span = .{ .start = start, .end = end }, .lexeme = self.source[start..end], .pattern = pattern, .flags = flags };
            }

            self.cursor += 1;
        }
        return error.UnterminatedRegexLiteral;
    }

    fn scanDot(self: *Lexer) LexicalError!token.Token {
        const c1 = self.peek(1);
        const c2 = self.peek(2);

        if (std.ascii.isDigit(c1)) {
            return self.scanNumber();
        }
        if (c1 == '.' and c2 == '.') {
            const start = self.cursor;
            self.cursor += 3;
            return self.createToken(.Spread, self.source[start..self.cursor], start, self.cursor);
        }
        const start = self.cursor;
        self.cursor += 1;
        return self.createToken(.Dot, self.source[start..self.cursor], start, self.cursor);
    }

    inline fn scanIdentifierBody(self: *Lexer) !void {
        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];
            if (std.ascii.isAscii(c)) {
                @branchHint(.likely);
                if (c == '\\') {
                    @branchHint(.cold);
                    if (self.peek(1) != 'u') {
                        return error.InvalidUnicodeEscape;
                    }
                    self.cursor += 1; // consume backslash to get to 'u'
                    try self.consumeUnicodeEscape();
                } else {
                    if ((c >= 'a' and c <= 'z') or
                        (c >= 'A' and c <= 'Z') or
                        (c >= '0' and c <= '9') or
                        c == '_' or c == '$')
                    {
                        self.cursor += 1;
                    } else {
                        break;
                    }
                }
            } else {
                @branchHint(.cold);
                const cp = try util.codePointAt(self.source, self.cursor);
                if (util.UnicodeId.canContinueIdentifier(cp.value)) {
                    self.cursor += cp.len;
                } else {
                    break;
                }
            }
        }
    }

    fn scanIdentifierOrKeyword(self: *Lexer) !token.Token {
        const start = self.cursor;

        const is_private = self.source[self.cursor] == '#';
        if (is_private) {
            self.cursor += 1;
        }

        const first_char = self.source[self.cursor];
        if (std.ascii.isAscii(first_char)) {
            @branchHint(.likely);
            if (first_char == '\\') {
                if (self.peek(1) != 'u') {
                    return error.InvalidUnicodeEscape;
                }
                self.cursor += 1; // consume backslash to get to 'u'
                try self.consumeUnicodeEscape();
            } else {
                if (!((first_char >= 'a' and first_char <= 'z') or
                    (first_char >= 'A' and first_char <= 'Z') or
                    first_char == '_' or first_char == '$'))
                {
                    @branchHint(.cold);
                    return error.InvalidIdentifierStart;
                }
                self.cursor += 1;
            }
            try self.scanIdentifierBody();
        } else {
            @branchHint(.cold);
            const c_cp = try util.codePointAt(self.source, self.cursor);
            if (!util.UnicodeId.canStartIdentifier(c_cp.value)) {
                return error.InvalidIdentifierStart;
            }
            self.cursor += c_cp.len;
            try self.scanIdentifierBody();
        }

        const lexeme = self.source[start..self.cursor];
        const token_type: token.TokenType = if (is_private) .PrivateIdentifier else self.getKeywordType(lexeme);
        return self.createToken(token_type, lexeme, start, self.cursor);
    }

    fn getKeywordType(self: *Lexer, lexeme: []const u8) token.TokenType {
        _ = self;
        switch (lexeme.len) {
            2 => {
                switch (lexeme[1]) {
                    'f' => {
                        return switch (lexeme[0]) {
                            'i' => .If,
                            'o' => .Of,
                            else => .Identifier,
                        };
                    },
                    'n' => if (lexeme[0] == 'i') return .In,
                    'o' => if (lexeme[0] == 'd') return .Do,
                    's' => if (lexeme[0] == 'a') return .As,
                    else => {},
                }
            },
            3 => {
                switch (lexeme[0]) {
                    'f' => if (lexeme[1] == 'o' and lexeme[2] == 'r') return .For,
                    'l' => if (lexeme[1] == 'e' and lexeme[2] == 't') return .Let,
                    'n' => if (lexeme[1] == 'e' and lexeme[2] == 'w') return .New,
                    't' => if (lexeme[1] == 'r' and lexeme[2] == 'y') return .Try,
                    'v' => if (lexeme[1] == 'a' and lexeme[2] == 'r') return .Var,
                    else => {},
                }
            },
            4 => {
                switch (lexeme[1]) {
                    'a' => if (lexeme[0] == 'c' and lexeme[2] == 's' and lexeme[3] == 'e') return .Case,
                    'h' => if (lexeme[0] == 't' and lexeme[2] == 'i' and lexeme[3] == 's') return .This,
                    'l' => if (lexeme[0] == 'e' and lexeme[2] == 's' and lexeme[3] == 'e') return .Else,
                    'n' => if (lexeme[0] == 'e' and lexeme[2] == 'u' and lexeme[3] == 'm') return .Enum,
                    'o' => if (lexeme[0] == 'v' and lexeme[2] == 'i' and lexeme[3] == 'd') return .Void,
                    'i' => if (lexeme[0] == 'w' and lexeme[2] == 't' and lexeme[3] == 'h') return .With,
                    'u' => if (lexeme[0] == 'n' and lexeme[2] == 'l' and lexeme[3] == 'l') return .NullLiteral,
                    'r' => {
                        if (lexeme[0] == 't' and lexeme[2] == 'u' and lexeme[3] == 'e') return .True;
                        if (lexeme[0] == 'f' and lexeme[2] == 'o' and lexeme[3] == 'm') return .From;
                    },
                    else => {},
                }
            },
            5 => {
                switch (lexeme[0]) {
                    'a' => {
                        if (lexeme[1] == 'w' and lexeme[2] == 'a' and lexeme[3] == 'i' and lexeme[4] == 't')
                            return .Await;
                        if (lexeme[1] == 's' and lexeme[2] == 'y' and lexeme[3] == 'n' and lexeme[4] == 'c')
                            return .Async;
                    },
                    'b' => if (lexeme[1] == 'r' and lexeme[2] == 'e' and lexeme[3] == 'a' and lexeme[4] == 'k')
                        return .Break,
                    'c' => {
                        if (lexeme[1] == 'o' and lexeme[2] == 'n' and lexeme[3] == 's' and lexeme[4] == 't')
                            return .Const;
                        if (lexeme[1] == 'l' and lexeme[2] == 'a' and lexeme[3] == 's' and lexeme[4] == 's')
                            return .Class;
                        if (lexeme[1] == 'a' and lexeme[2] == 't' and lexeme[3] == 'c' and lexeme[4] == 'h')
                            return .Catch;
                    },
                    'f' => if (lexeme[1] == 'a' and lexeme[2] == 'l' and lexeme[3] == 's' and lexeme[4] == 'e')
                        return .False,
                    's' => if (lexeme[1] == 'u' and lexeme[2] == 'p' and lexeme[3] == 'e' and lexeme[4] == 'r')
                        return .Super,
                    't' => if (lexeme[1] == 'h' and lexeme[2] == 'r' and lexeme[3] == 'o' and lexeme[4] == 'w')
                        return .Throw,
                    'u' => if (lexeme[1] == 's' and lexeme[2] == 'i' and lexeme[3] == 'n' and lexeme[4] == 'g')
                        return .Using,
                    'w' => if (lexeme[1] == 'h' and lexeme[2] == 'i' and lexeme[3] == 'l' and lexeme[4] == 'e')
                        return .While,
                    'y' => if (lexeme[1] == 'i' and lexeme[2] == 'e' and lexeme[3] == 'l' and lexeme[4] == 'd')
                        return .Yield,
                    else => {},
                }
            },
            6 => {
                switch (lexeme[0]) {
                    'd' => if (lexeme[1] == 'e' and lexeme[2] == 'l' and lexeme[3] == 'e' and lexeme[4] == 't' and lexeme[5] == 'e')
                        return .Delete,
                    'e' => if (lexeme[1] == 'x' and lexeme[2] == 'p' and lexeme[3] == 'o' and lexeme[4] == 'r' and lexeme[5] == 't')
                        return .Export,
                    'i' => if (lexeme[1] == 'm' and lexeme[2] == 'p' and lexeme[3] == 'o' and lexeme[4] == 'r' and lexeme[5] == 't')
                        return .Import,
                    'p' => if (lexeme[1] == 'u' and lexeme[2] == 'b' and lexeme[3] == 'l' and lexeme[4] == 'i' and lexeme[5] == 'c')
                        return .Public,
                    'r' => if (lexeme[1] == 'e' and lexeme[2] == 't' and lexeme[3] == 'u' and lexeme[4] == 'r' and lexeme[5] == 'n')
                        return .Return,
                    's' => {
                        if (lexeme[1] == 'w' and lexeme[2] == 'i' and lexeme[3] == 't' and lexeme[4] == 'c' and lexeme[5] == 'h')
                            return .Switch;
                        if (lexeme[1] == 't' and lexeme[2] == 'a' and lexeme[3] == 't' and lexeme[4] == 'i' and lexeme[5] == 'c')
                            return .Static;
                    },
                    't' => if (lexeme[1] == 'y' and lexeme[2] == 'p' and lexeme[3] == 'e' and lexeme[4] == 'o' and lexeme[5] == 'f')
                        return .Typeof,
                    else => {},
                }
            },
            7 => {
                switch (lexeme[0]) {
                    'd' => {
                        if (std.mem.eql(u8, lexeme, "default")) return .Default;
                        if (std.mem.eql(u8, lexeme, "declare")) return .Declare;
                    },
                    'e' => if (std.mem.eql(u8, lexeme, "extends")) return .Extends,
                    'f' => if (std.mem.eql(u8, lexeme, "finally")) return .Finally,
                    'p' => if (std.mem.eql(u8, lexeme, "private")) return .Private,
                    else => {},
                }
            },
            8 => {
                switch (lexeme[0]) {
                    'c' => if (std.mem.eql(u8, lexeme, "continue")) return .Continue,
                    'd' => if (std.mem.eql(u8, lexeme, "debugger")) return .Debugger,
                    'f' => if (std.mem.eql(u8, lexeme, "function")) return .Function,
                    else => {},
                }
            },
            9 => {
                switch (lexeme[0]) {
                    'i' => if (std.mem.eql(u8, lexeme, "interface")) return .Interface,
                    'p' => if (std.mem.eql(u8, lexeme, "protected")) return .Protected,
                    else => {},
                }
            },
            10 => {
                switch (lexeme[0]) {
                    'i' => {
                        if (std.mem.eql(u8, lexeme, "instanceof")) return .Instanceof;
                        if (std.mem.eql(u8, lexeme, "implements")) return .Implements;
                    },
                    else => {},
                }
            },
            else => {},
        }
        return .Identifier;
    }

    fn scanNumber(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        var token_type: token.TokenType = .NumericLiteral;

        // handle prefixes: 0x, 0o, 0b
        if (self.source[self.cursor] == '0') {
            const prefix = std.ascii.toLower(self.peek(1));

            switch (prefix) {
                'x' => {
                    token_type = .HexLiteral;
                    self.cursor += 2;
                    const hex_start = self.cursor;
                    try self.consumeHexDigits();
                    if (self.cursor == hex_start) return error.InvalidHexLiteral;
                },
                'o' => {
                    token_type = .OctalLiteral;
                    self.cursor += 2;
                    const oct_start = self.cursor;
                    try self.consumeOctalDigits();
                    if (self.cursor == oct_start) return error.InvalidOctalLiteralDigit;
                },
                'b' => {
                    token_type = .BinaryLiteral;
                    self.cursor += 2;
                    const bin_start = self.cursor;
                    try self.consumeBinaryDigits();
                    if (self.cursor == bin_start) return error.InvalidBinaryLiteral;
                },
                else => {
                    try self.consumeDecimalDigits();
                },
            }
        } else {
            try self.consumeDecimalDigits();
        }

        // handle decimal point (only for regular numbers)
        if (token_type == .NumericLiteral and
            self.cursor < self.source_len and self.source[self.cursor] == '.')
        {
            const next = self.peek(1);

            if (std.ascii.isDigit(next)) {
                self.cursor += 1;
                try self.consumeDecimalDigits();
            }
        }

        // handle exponent (only for regular numbers)
        if (token_type == .NumericLiteral and self.cursor < self.source_len) {
            const exp_char = std.ascii.toLower(self.source[self.cursor]);
            if (exp_char == 'e') {
                try self.consumeExponent();
            }
        }

        // handle bigint suffix 'n'
        if (self.cursor < self.source_len and self.source[self.cursor] == 'n') {
            // bigint cannot have decimal point or exponent
            if (token_type == .NumericLiteral) {
                const lexeme = self.source[start..self.cursor];
                for (lexeme) |c| {
                    if (c == '.' or std.ascii.toLower(c) == 'e') {
                        return error.InvalidBigIntSuffix;
                    }
                }
            }

            self.cursor += 1;
            token_type = .BigIntLiteral;
        }

        return self.createToken(token_type, self.source[start..self.cursor], start, self.cursor);
    }

    inline fn consumeDecimalDigits(self: *Lexer) LexicalError!void {
        var last_was_separator = false;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];
            if (std.ascii.isDigit(c)) {
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

    inline fn consumeHexDigits(self: *Lexer) LexicalError!void {
        var last_was_separator = false;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];
            if (std.ascii.isHex(c)) {
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

    inline fn consumeOctalDigits(self: *Lexer) LexicalError!void {
        var last_was_separator = false;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];
            if (util.isOctalDigit(c)) {
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

    inline fn consumeBinaryDigits(self: *Lexer) LexicalError!void {
        var last_was_separator = false;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];
            if (c == '0' or c == '1') {
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

    fn consumeExponent(self: *Lexer) LexicalError!void {
        self.cursor += 1; // skip 'e' or 'E'

        if (self.cursor >= self.source_len) {
            return error.InvalidExponentPart;
        }

        // handle optional sign: + or -
        const c = self.source[self.cursor];
        if (c == '+' or c == '-') {
            self.cursor += 1;
        }

        const exp_start = self.cursor;
        try self.consumeDecimalDigits();

        if (self.cursor == exp_start) {
            return error.InvalidExponentPart;
        }
    }

    inline fn skipSkippable(self: *Lexer) LexicalError!void {
        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];

            if (std.ascii.isAscii(c)) {
                @branchHint(.likely);
                switch (c) {
                    ' ', '\t', '\u{000B}', '\u{000C}' => {
                        self.cursor += 1;
                        continue;
                    },
                    '\n', '\r' => {
                        self.has_line_terminator_before = true;
                        self.cursor += 1;
                        continue;
                    },
                    else => break,
                }
            } else {
                @branchHint(.unlikely);
                // multi-byte space character
                const cp = try util.codePointAt(self.source, self.cursor);
                if (util.isMultiByteSpace(cp.value)) {
                    self.cursor += cp.len;
                    continue;
                }
            }

            break;
        }
    }

    pub inline fn createToken(self: *Lexer, token_type: token.TokenType, lexeme: []const u8, start: u32, end: u32) token.Token {
        const tok = token.Token{
            .type = token_type,
            .lexeme = lexeme,
            .span = .{ .start = start, .end = end },
            .has_line_terminator_before = self.has_line_terminator_before,
        };

        self.has_line_terminator_before = false;
        return tok;
    }
};

pub fn getLexicalErrorMessage(error_type: LexicalError) []const u8 {
    return switch (error_type) {
        error.InvalidHexEscape => "Invalid hexadecimal escape sequence",
        error.UnterminatedString => "Unterminated string literal",
        error.UnterminatedRegex => "Unterminated regular expression",
        error.NonTerminatedTemplateLiteral => "Unterminated template literal",
        error.UnterminatedRegexLiteral => "Unterminated regular expression literal",
        error.InvalidRegexLineTerminator => "Line terminator not allowed in regular expression literal",
        error.InvalidRegex => "Invalid regular expression",
        error.InvalidIdentifierStart => "Invalid character at start of identifier",
        error.UnterminatedMultiLineComment => "Unterminated multi-line comment",
        error.InvalidUnicodeEscape => "Invalid Unicode escape sequence",
        error.InvalidOctalEscape => "Invalid octal escape sequence",
        error.OctalEscapeInStrict => "Octal escape sequences are not allowed in strict mode",
        error.InvalidBinaryLiteral => "Binary literal must contain at least one binary digit",
        error.InvalidOctalLiteralDigit => "Octal literal must contain at least one octal digit",
        error.InvalidHexLiteral => "Hexadecimal literal must contain at least one hex digit",
        error.InvalidExponentPart => "Exponent part is missing a number",
        error.NumericSeparatorMisuse => "Numeric separator cannot appear at the end of a numeric literal",
        error.ConsecutiveNumericSeparators => "Numeric literal cannot contain consecutive separators",
        error.MultipleDecimalPoints => "Numeric literal cannot contain multiple decimal points",
        error.InvalidBigIntSuffix => "BigInt literal cannot contain decimal point or exponent",
        error.InvalidUtf8 => "Invalid UTF-8 byte sequence",
    };
}

pub fn getLexicalErrorHelp(error_type: LexicalError) []const u8 {
    return switch (error_type) {
        error.InvalidHexEscape => "Try adding two hexadecimal digits here (e.g., \\x41 for 'A')",
        error.UnterminatedString => "Try adding a closing quote here to complete the string",
        error.UnterminatedRegex => "Try adding a closing slash (/) here to complete the regex",
        error.NonTerminatedTemplateLiteral => "Try adding a closing backtick (`) here to complete the template",
        error.UnterminatedRegexLiteral => "Try adding a closing slash (/) here, optionally followed by flags (g, i, m, etc.)",
        error.InvalidRegexLineTerminator => "Try removing the line break here or escaping it within the regex pattern",
        error.InvalidRegex => "Try checking the regex syntax here for unclosed groups, invalid escapes, or malformed patterns",
        error.InvalidIdentifierStart => "Try starting the identifier here with a letter (a-z, A-Z), underscore (_), or dollar sign ($)",
        error.UnterminatedMultiLineComment => "Try adding the closing delimiter (*/) here to complete the comment",
        error.InvalidUnicodeEscape => "Try using \\uHHHH (4 hex digits) or \\u{HHHHHH} (1-6 hex digits) here",
        error.InvalidOctalEscape => "Try using a valid octal sequence here (\\0-7, \\00-77, or \\000-377)",
        error.OctalEscapeInStrict => "Try replacing this octal escape with \\xHH (hex) or \\uHHHH (unicode) instead",
        error.InvalidBinaryLiteral => "Try adding at least one binary digit (0 or 1) here after '0b'",
        error.InvalidOctalLiteralDigit => "Try adding at least one octal digit (0-7) here after '0o'",
        error.InvalidHexLiteral => "Try adding at least one hex digit (0-9, a-f, A-F) here after '0x'",
        error.InvalidExponentPart => "Try adding digits here after the exponent (e.g., e10, e-5, E+2)",
        error.NumericSeparatorMisuse => "Try removing the trailing underscore here or adding more digits after it",
        error.ConsecutiveNumericSeparators => "Try removing one of the consecutive underscores here",
        error.MultipleDecimalPoints => "Try removing the extra decimal point here",
        error.InvalidBigIntSuffix => "Try removing the 'n' suffix here, or remove the decimal point/exponent from the number",
        error.InvalidUtf8 => "The source file contains invalid UTF-8 encoding. Ensure the file is saved with valid UTF-8 encoding",
    };
}
