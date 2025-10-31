const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Comment = @import("token.zig").Comment;
const CommentType = @import("token.zig").CommentType;
const unicodeJsHelpers = @import("unicode/js-helpers.zig");
const util = @import("util.zig");

const LexicalError = error{
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
};

// TODO:
// [ ] some simd optimizations
const padding_size = 4; // four safe lookaheads

pub const Lexer = struct {
    strict_mode: bool,
    source: []u8,
    source_len: usize,
    position: usize,
    template_depth: usize,
    allocator: std.mem.Allocator,
    comments: std.ArrayList(Comment),

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Lexer {
        const padded_buffer = try allocator.alloc(u8, source.len + padding_size);
        @memcpy(padded_buffer[0..source.len], source);
        @memset(padded_buffer[source.len..], 0);
        return .{
            .strict_mode = false,
            .source = padded_buffer,
            .source_len = source.len,
            .position = 0,
            .template_depth = 0,
            .allocator = allocator,
            .comments = .empty,
        };
    }

    pub fn nextToken(self: *Lexer) LexicalError!Token {
        try self.skipSkippable();
        if (self.position >= self.source_len) {
            return self.createToken(.EOF, "", self.position, self.position);
        }
        const current_char = self.source[self.position];
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

    inline fn scanSimplePunctuation(self: *Lexer) Token {
        const start = self.position;
        const c = self.source[self.position];
        self.position += 1;
        const token_type: TokenType = switch (c) {
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
        return self.createToken(token_type, self.source[start..self.position], start, self.position);
    }

    fn scanPunctuation(self: *Lexer) LexicalError!Token {
        const start = self.position;
        const c0 = self.source[self.position];
        const c1 = self.source[self.position + 1];
        const c2 = self.source[self.position + 2];
        const c3 = self.source[self.position + 3];

        return switch (c0) {
            '+' => switch (c1) {
                '+' => blk: {
                    self.position += 2;
                    break :blk self.createToken(.Increment, self.source[start..self.position], start, self.position);
                },
                '=' => blk: {
                    self.position += 2;
                    break :blk self.createToken(.PlusAssign, self.source[start..self.position], start, self.position);
                },
                else => blk: {
                    self.position += 1;
                    break :blk self.createToken(.Plus, self.source[start..self.position], start, self.position);
                },
            },
            '-' => switch (c1) {
                '-' => blk: {
                    self.position += 2;
                    break :blk self.createToken(.Decrement, self.source[start..self.position], start, self.position);
                },
                '=' => blk: {
                    self.position += 2;
                    break :blk self.createToken(.MinusAssign, self.source[start..self.position], start, self.position);
                },
                else => blk: {
                    self.position += 1;
                    break :blk self.createToken(.Minus, self.source[start..self.position], start, self.position);
                },
            },
            '*' => blk: {
                if (c1 == '*' and c2 == '=') {
                    self.position += 3;
                    break :blk self.createToken(.ExponentAssign, self.source[start..self.position], start, self.position);
                }
                break :blk switch (c1) {
                    '*' => blk2: {
                        self.position += 2;
                        break :blk2 self.createToken(.Exponent, self.source[start..self.position], start, self.position);
                    },
                    '=' => blk2: {
                        self.position += 2;
                        break :blk2 self.createToken(.StarAssign, self.source[start..self.position], start, self.position);
                    },
                    else => blk2: {
                        self.position += 1;
                        break :blk2 self.createToken(.Star, self.source[start..self.position], start, self.position);
                    },
                };
            },
            '/' => blk: {
                if (c1 == '=') {
                    self.position += 2;
                    break :blk self.createToken(.SlashAssign, self.source[start..self.position], start, self.position);
                }

                self.position += 1;

                break :blk self.createToken(.Slash, self.source[start..self.position], start, self.position);
            },
            '%' => switch (c1) {
                '=' => blk: {
                    self.position += 2;
                    break :blk self.createToken(.PercentAssign, self.source[start..self.position], start, self.position);
                },
                else => blk: {
                    self.position += 1;
                    break :blk self.createToken(.Percent, self.source[start..self.position], start, self.position);
                },
            },
            '<' => blk: {
                if (c1 == '<' and c2 == '=') {
                    self.position += 3;
                    break :blk self.createToken(.LeftShiftAssign, self.source[start..self.position], start, self.position);
                }
                break :blk switch (c1) {
                    '<' => blk2: {
                        self.position += 2;
                        break :blk2 self.createToken(.LeftShift, self.source[start..self.position], start, self.position);
                    },
                    '=' => blk2: {
                        self.position += 2;
                        break :blk2 self.createToken(.LessThanEqual, self.source[start..self.position], start, self.position);
                    },
                    else => blk2: {
                        self.position += 1;
                        break :blk2 self.createToken(.LessThan, self.source[start..self.position], start, self.position);
                    },
                };
            },
            '>' => blk: {
                if (c1 == '>' and c2 == '=') {
                    self.position += 3;
                    break :blk self.createToken(.RightShiftAssign, self.source[start..self.position], start, self.position);
                }
                if (c1 == '>' and c2 == '>') {
                    if (c3 == '=') {
                        self.position += 4;
                        break :blk self.createToken(.UnsignedRightShiftAssign, self.source[start..self.position], start, self.position);
                    } else {
                        self.position += 3;
                        break :blk self.createToken(.UnsignedRightShift, self.source[start..self.position], start, self.position);
                    }
                }
                break :blk switch (c1) {
                    '>' => blk2: {
                        self.position += 2;
                        break :blk2 self.createToken(.RightShift, self.source[start..self.position], start, self.position);
                    },
                    '=' => blk2: {
                        self.position += 2;
                        break :blk2 self.createToken(.GreaterThanEqual, self.source[start..self.position], start, self.position);
                    },
                    else => blk2: {
                        self.position += 1;
                        break :blk2 self.createToken(.GreaterThan, self.source[start..self.position], start, self.position);
                    },
                };
            },
            '=' => blk: {
                if (c1 == '=' and c2 == '=') {
                    self.position += 3;
                    break :blk self.createToken(.StrictEqual, self.source[start..self.position], start, self.position);
                }
                break :blk switch (c1) {
                    '=' => blk2: {
                        self.position += 2;
                        break :blk2 self.createToken(.Equal, self.source[start..self.position], start, self.position);
                    },
                    '>' => blk2: {
                        self.position += 2;
                        break :blk2 self.createToken(.Arrow, self.source[start..self.position], start, self.position);
                    },
                    else => blk2: {
                        self.position += 1;
                        break :blk2 self.createToken(.Assign, self.source[start..self.position], start, self.position);
                    },
                };
            },
            '!' => blk: {
                if (c1 == '=' and c2 == '=') {
                    self.position += 3;
                    break :blk self.createToken(.StrictNotEqual, self.source[start..self.position], start, self.position);
                }
                break :blk switch (c1) {
                    '=' => blk2: {
                        self.position += 2;
                        break :blk2 self.createToken(.NotEqual, self.source[start..self.position], start, self.position);
                    },
                    else => blk2: {
                        self.position += 1;
                        break :blk2 self.createToken(.LogicalNot, self.source[start..self.position], start, self.position);
                    },
                };
            },
            '&' => blk: {
                if (c1 == '&' and c2 == '=') {
                    self.position += 3;
                    break :blk self.createToken(.LogicalAndAssign, self.source[start..self.position], start, self.position);
                }
                break :blk switch (c1) {
                    '&' => blk2: {
                        self.position += 2;
                        break :blk2 self.createToken(.LogicalAnd, self.source[start..self.position], start, self.position);
                    },
                    '=' => blk2: {
                        self.position += 2;
                        break :blk2 self.createToken(.BitwiseAndAssign, self.source[start..self.position], start, self.position);
                    },
                    else => blk2: {
                        self.position += 1;
                        break :blk2 self.createToken(.BitwiseAnd, self.source[start..self.position], start, self.position);
                    },
                };
            },
            '|' => blk: {
                if (c1 == '|' and c2 == '=') {
                    self.position += 3;
                    break :blk self.createToken(.LogicalOrAssign, self.source[start..self.position], start, self.position);
                }
                break :blk switch (c1) {
                    '|' => blk2: {
                        self.position += 2;
                        break :blk2 self.createToken(.LogicalOr, self.source[start..self.position], start, self.position);
                    },
                    '=' => blk2: {
                        self.position += 2;
                        break :blk2 self.createToken(.BitwiseOrAssign, self.source[start..self.position], start, self.position);
                    },
                    else => blk2: {
                        self.position += 1;
                        break :blk2 self.createToken(.BitwiseOr, self.source[start..self.position], start, self.position);
                    },
                };
            },
            '^' => switch (c1) {
                '=' => blk: {
                    self.position += 2;
                    break :blk self.createToken(.BitwiseXorAssign, self.source[start..self.position], start, self.position);
                },
                else => blk: {
                    self.position += 1;
                    break :blk self.createToken(.BitwiseXor, self.source[start..self.position], start, self.position);
                },
            },
            '?' => blk: {
                if (c1 == '?' and c2 == '=') {
                    self.position += 3;
                    break :blk self.createToken(.NullishAssign, self.source[start..self.position], start, self.position);
                }
                break :blk switch (c1) {
                    '?' => blk2: {
                        self.position += 2;
                        break :blk2 self.createToken(.NullishCoalescing, self.source[start..self.position], start, self.position);
                    },
                    '.' => blk2: {
                        self.position += 2;
                        break :blk2 self.createToken(.OptionalChaining, self.source[start..self.position], start, self.position);
                    },
                    else => blk2: {
                        self.position += 1;
                        break :blk2 self.createToken(.Question, self.source[start..self.position], start, self.position);
                    },
                };
            },
            else => unreachable,
        };
    }

    fn scanString(self: *Lexer) LexicalError!Token {
        const start = self.position;
        const quote = self.source[start];
        var i = start + 1;

        while (i < self.source_len) {
            const c = self.source[i];
            if (c == '\\') {
                i = try self.consumeEscape(i);
                continue;
            }
            if (c == quote) {
                i += 1;
                self.position = i;
                return self.createToken(.StringLiteral, self.source[start..i], start, i);
            }
            if (c == '\n' or c == '\r' or c == '\u{2028}' or c == '\u{2029}') {
                return error.UnterminatedString;
            }
            i += 1;
        }
        return error.UnterminatedString;
    }

    fn scanTemplateLiteral(self: *Lexer) LexicalError!Token {
        const start = self.position;
        var i = start + 1;

        while (i < self.source_len) {
            const c = self.source[i];
            if (c == '\\') {
                i = try self.consumeEscape(i);
                continue;
            }
            if (c == '`') {
                i += 1;
                self.position = i;
                return self.createToken(.NoSubstitutionTemplate, self.source[start..i], start, i);
            }
            if (c == '$' and self.source[i + 1] == '{') {
                i += 2;
                self.template_depth += 1;
                self.position = i;
                return self.createToken(.TemplateHead, self.source[start..i], start, i);
            }
            i += 1;
        }
        return error.NonTerminatedTemplateLiteral;
    }

    fn scanTemplateMiddleOrTail(self: *Lexer) LexicalError!Token {
        const start = self.position;
        var i = start + 1;

        while (i < self.source_len) {
            const c = self.source[i];
            if (c == '\\') {
                i = try self.consumeEscape(i);
                continue;
            }
            if (c == '`') {
                i += 1;
                if (self.template_depth > 0) {
                    self.template_depth -= 1;
                }
                self.position = i;
                return self.createToken(.TemplateTail, self.source[start..i], start, i);
            }
            if (c == '$' and self.source[i + 1] == '{') {
                i += 2;
                self.position = i;
                return self.createToken(.TemplateMiddle, self.source[start..i], start, i);
            }
            i += 1;
        }
        return error.NonTerminatedTemplateLiteral;
    }

    fn consumeEscape(self: *Lexer, pos: usize) LexicalError!usize {
        var i = pos + 1;
        brk: switch (self.source[i]) {
            '0' => {
                const c1 = self.source[i + 1];
                // null
                if (!util.isOctalDigit(c1)) {
                    i += 1;
                    break :brk;
                }
                if (self.strict_mode) return error.OctalEscapeInStrict;
                i = try self.consumeOctal(i);
            },
            // hex
            'x' => {
                i = try self.consumeHex(i);
            },
            // unicode
            'u' => {
                i = try self.consumeUnicodeEscape(i);
            },
            // octal
            '1'...'7' => {
                if (self.strict_mode) return error.OctalEscapeInStrict;
                i = try self.consumeOctal(i);
            },
            else => {
                i += 1;
            },
        }
        return i;
    }

    fn consumeOctal(self: *Lexer, pos: usize) LexicalError!usize {
        var i = pos;
        var count: u8 = 0;
        while (i < self.source_len and count < 3) {
            const c = self.source[i];
            if (util.isOctalDigit(c)) {
                i += 1;
                count += 1;
            } else {
                break;
            }
        }
        return if (count > 0) i else error.InvalidOctalEscape;
    }

    fn consumeHex(self: *Lexer, pos: usize) LexicalError!usize {
        const c1 = self.source[pos + 1];
        const c2 = self.source[pos + 2];
        if (!std.ascii.isHex(c1) or !std.ascii.isHex(c2)) {
            return error.InvalidHexEscape;
        }
        return pos + 3;
    }

    fn consumeUnicodeEscape(self: *Lexer, pos: usize) LexicalError!usize {
        var i = pos + 1;
        if (i < self.source_len and self.source[i] == '{') {
            // \u{XXXXX} format
            i += 1;
            const start = i;
            const end = std.mem.indexOfScalarPos(u8, self.source, i, '}') orelse
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
            return end + 1;
        } else {
            // \uXXXX format
            const end = i + 4;
            if (end > self.source_len) {
                return error.InvalidUnicodeEscape;
            }
            for (self.source[i..end]) |c| {
                if (!std.ascii.isHex(c)) {
                    return error.InvalidUnicodeEscape;
                }
            }
            return end;
        }
    }

    fn handleRightBrace(self: *Lexer) LexicalError!Token {
        if (self.template_depth > 0) {
            return self.scanTemplateMiddleOrTail();
        }
        const start = self.position;
        self.position += 1;
        return self.createToken(.RightBrace, self.source[start..self.position], start, self.position);
    }

    pub fn reScanAsRegex(self: *Lexer, slash_token: Token) LexicalError!Token {
        self.position = slash_token.span.start;
        return self.scanRegex();
    }

    fn scanRegex(self: *Lexer) LexicalError!Token {
        const start = self.position;
        self.position += 1; // consume '/'
        var in_class = false;

        while (self.position < self.source_len) {
            const c = self.source[self.position];
            if (c == '\\') {
                self.position += 1; // skip backslash
                if (self.position < self.source_len) {
                    self.position += 1; // skip escaped char
                }
                continue;
            }
            if (c == '[') {
                in_class = true;
                self.position += 1;
                continue;
            }
            if (c == ']' and in_class) {
                in_class = false;
                self.position += 1;
                continue;
            }
            if (c == '/' and !in_class) {
                self.position += 1;
                while (self.position < self.source_len and
                    std.ascii.isAlphabetic(self.source[self.position]))
                {
                    self.position += 1;
                }
                return self.createToken(.RegexLiteral, self.source[start..self.position], start, self.position);
            }
            if (c == '\n' or c == '\r') {
                return error.InvalidRegexLineTerminator;
            }
            self.position += 1;
        }
        return error.UnterminatedRegexLiteral;
    }

    fn scanDot(self: *Lexer) LexicalError!Token {
        const c1 = self.source[self.position + 1];
        const c2 = self.source[self.position + 2];

        if (std.ascii.isDigit(c1)) {
            return self.scanNumber();
        }
        if (c1 == '.' and c2 == '.') {
            const start = self.position;
            self.position += 3;
            return self.createToken(.Spread, self.source[start..self.position], start, self.position);
        }
        const start = self.position;
        self.position += 1;
        return self.createToken(.Dot, self.source[start..self.position], start, self.position);
    }

    inline fn scanIdentifierBody(self: *Lexer, i: usize) !usize {
        var pos = i;
        while (pos < self.source_len) {
            const c = self.source[pos];
            if (std.ascii.isAscii(c)) {
                @branchHint(.likely);
                if (c == '\\') {
                    @branchHint(.cold);
                    if (self.source[pos + 1] != 'u') {
                        return error.InvalidUnicodeEscape;
                    }
                    pos += 1; // consume u
                    pos = try self.consumeUnicodeEscape(pos);
                } else {
                    if ((c >= 'a' and c <= 'z') or
                        (c >= 'A' and c <= 'Z') or
                        (c >= '0' and c <= '9') or
                        c == '_' or c == '$')
                    {
                        pos += 1;
                    } else {
                        break;
                    }
                }
            } else {
                @branchHint(.cold);
                const cp = util.codePointAt(self.source, pos);
                if (unicodeJsHelpers.canContinueJsIdentifier(cp.value)) {
                    pos += cp.len;
                } else {
                    break;
                }
            }
        }
        return pos;
    }

    fn scanIdentifierOrKeyword(self: *Lexer) !Token {
        const start = self.position;
        var i = start;

        const is_private = self.source[i] == '#';
        if (is_private) {
            i += 1;
        }

        const first_char = self.source[i];
        if (std.ascii.isAscii(first_char)) {
            @branchHint(.likely);
            if (first_char == '\\') {
                if (self.source[i + 1] != 'u') {
                    return error.InvalidUnicodeEscape;
                }
                i += 1; // consume u
                i = try self.consumeUnicodeEscape(i);
            } else {
                if (!((first_char >= 'a' and first_char <= 'z') or
                    (first_char >= 'A' and first_char <= 'Z') or
                    first_char == '_' or first_char == '$'))
                {
                    @branchHint(.cold);
                    return error.InvalidIdentifierStart;
                }
                i += 1;
            }
            i = try self.scanIdentifierBody(i);
        } else {
            @branchHint(.cold);
            const c_cp = util.codePointAt(self.source, i);
            if (!unicodeJsHelpers.canStartJsIdentifier(c_cp.value)) {
                return error.InvalidIdentifierStart;
            }
            i += c_cp.len;
            i = try self.scanIdentifierBody(i);
        }

        self.position = i;
        const lexeme = self.source[start..i];
        const token_type: TokenType = if (is_private) .PrivateIdentifier else self.getKeywordType(lexeme);
        return self.createToken(token_type, lexeme, start, i);
    }

    fn getKeywordType(self: *Lexer, lexeme: []const u8) TokenType {
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
                    'd' => if (std.mem.eql(u8, lexeme, "default")) return .Default,
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

    fn scanNumber(self: *Lexer) LexicalError!Token {
        const start = self.position;
        var token_type: TokenType = .NumericLiteral;
        var i = self.position;

        // prefixes 0x, 0o, 0b
        if (self.source[i] == '0' and i + 1 < self.source_len) {
            const prefix = std.ascii.toLower(self.source[i + 1]);
            switch (prefix) {
                'x' => {
                    token_type = .HexLiteral;
                    i += 2;
                    const hex_start = i;
                    i = try self.consumeHexDigits(i);
                    if (i == hex_start) return error.InvalidHexLiteral;
                },
                'o' => {
                    token_type = .OctalLiteral;
                    i += 2;
                    const oct_start = i;
                    i = try self.consumeOctalDigits(i);
                    if (i == oct_start) return error.InvalidOctalLiteralDigit;
                },
                'b' => {
                    token_type = .BinaryLiteral;
                    i += 2;
                    const bin_start = i;
                    i = try self.consumeBinaryDigits(i);
                    if (i == bin_start) return error.InvalidBinaryLiteral;
                },
                else => {
                    // regular or octal
                    i = try self.consumeDecimalDigits(i);
                },
            }
        } else {
            // regular
            i = try self.consumeDecimalDigits(i);
        }

        // decimal point (only for regular numbers)
        if (token_type == .NumericLiteral and
            i < self.source_len and self.source[i] == '.')
        {
            const next = if (i + 1 < self.source_len) self.source[i + 1] else 0;
            if (std.ascii.isDigit(next)) {
                i += 1; // consume '.'
                i = try self.consumeDecimalDigits(i);
            }
        }

        // exponent (only for regular numbers)
        if (token_type == .NumericLiteral and i < self.source_len) {
            const exp_char = std.ascii.toLower(self.source[i]);
            if (exp_char == 'e') {
                i = try self.consumeExponent(i);
            }
        }

        // bigint suffix
        if (i < self.source_len and self.source[i] == 'n') {
            // bigint cannot have decimal point or exponent
            if (token_type == .NumericLiteral) {
                const lexeme = self.source[start..i];
                for (lexeme) |c| {
                    if (c == '.' or std.ascii.toLower(c) == 'e') {
                        return error.InvalidBigIntSuffix;
                    }
                }
            }
            i += 1;
            token_type = .BigIntLiteral;
        }

        self.position = i;
        return self.createToken(token_type, self.source[start..i], start, i);
    }

    inline fn consumeDecimalDigits(self: *Lexer, start: usize) LexicalError!usize {
        var i = start;
        var last_was_separator = false;

        while (i < self.source_len) {
            const c = self.source[i];
            if (std.ascii.isDigit(c)) {
                i += 1;
                last_was_separator = false;
            } else if (c == '_') {
                if (last_was_separator) {
                    return error.ConsecutiveNumericSeparators;
                }
                i += 1;
                last_was_separator = true;
            } else {
                break;
            }
        }

        if (last_was_separator) {
            return error.NumericSeparatorMisuse;
        }

        return i;
    }

    inline fn consumeHexDigits(self: *Lexer, start: usize) LexicalError!usize {
        var i = start;
        var last_was_separator = false;

        while (i < self.source_len) {
            const c = self.source[i];
            if (std.ascii.isHex(c)) {
                i += 1;
                last_was_separator = false;
            } else if (c == '_') {
                if (last_was_separator) {
                    return error.ConsecutiveNumericSeparators;
                }
                i += 1;
                last_was_separator = true;
            } else {
                break;
            }
        }

        if (last_was_separator) {
            return error.NumericSeparatorMisuse;
        }

        return i;
    }

    inline fn consumeOctalDigits(self: *Lexer, start: usize) LexicalError!usize {
        var i = start;
        var last_was_separator = false;

        while (i < self.source_len) {
            const c = self.source[i];
            if (util.isOctalDigit(c)) {
                i += 1;
                last_was_separator = false;
            } else if (c == '_') {
                if (last_was_separator) {
                    return error.ConsecutiveNumericSeparators;
                }
                i += 1;
                last_was_separator = true;
            } else {
                break;
            }
        }

        if (last_was_separator) {
            return error.NumericSeparatorMisuse;
        }

        return i;
    }

    inline fn consumeBinaryDigits(self: *Lexer, start: usize) LexicalError!usize {
        var i = start;
        var last_was_separator = false;

        while (i < self.source_len) {
            const c = self.source[i];
            if (c == '0' or c == '1') {
                i += 1;
                last_was_separator = false;
            } else if (c == '_') {
                if (last_was_separator) {
                    return error.ConsecutiveNumericSeparators;
                }
                i += 1;
                last_was_separator = true;
            } else {
                break;
            }
        }

        if (last_was_separator) {
            return error.NumericSeparatorMisuse;
        }

        return i;
    }

    fn consumeExponent(self: *Lexer, start: usize) LexicalError!usize {
        var i = start + 1; // skip 'e/E'

        if (i >= self.source_len) {
            return error.InvalidExponentPart;
        }

        // handle optional sign
        const c = self.source[i];
        if (c == '+' or c == '-') {
            i += 1;
        }

        const exp_start = i;
        i = try self.consumeDecimalDigits(i);

        if (i == exp_start) {
            return error.InvalidExponentPart;
        }

        return i;
    }

    inline fn skipSkippable(self: *Lexer) LexicalError!void {
        var i = self.position;
        while (i < self.source_len) {
            const c = self.source[i];
            if (std.ascii.isAscii(c)) {
                switch (c) {
                    // simple spaces
                    ' ', '\t', '\n', '\r', '\u{000B}', '\u{000C}' => {
                        i += 1;
                        continue;
                    },
                    // handle comments, consume and push to the comments array
                    '/' => {
                        const c1 = self.source[i + 1];
                        if (c1 == 0) break;
                        if (c1 == '/') {
                            i = try self.skipSingleLineComment(i);
                            continue;
                        } else if (c1 == '*') {
                            i = try self.skipMultiLineComment(i);
                            continue;
                        } else {
                            break;
                        }
                    },
                    else => break,
                }
            } else {
                // okay, it maybe a multi-byte space
                const cp = util.codePointAt(self.source, i);
                if (util.isMultiByteSpace(cp.value)) {
                    i += cp.len;
                    continue;
                }
            }
            break;
        }
        self.position = i;
    }

    inline fn skipSingleLineComment(self: *Lexer, pos: usize) LexicalError!usize {
        const start = pos;
        var i = start + 2;
        while (i < self.source_len) {
            const c = self.source[i];
            if (c == '\n' or c == '\r') {
                break;
            }
            i += 1;
        }
        self.comments.append(self.allocator, Comment{
            .content = self.source[start..i],
            .span = .{ .start = start, .end = i },
            .type = .SingleLine,
        }) catch unreachable;
        return i;
    }

    inline fn skipMultiLineComment(self: *Lexer, pos: usize) LexicalError!usize {
        const start = pos;
        var i = start + 2;
        while (i < self.source_len) {
            const c = self.source[i];
            if (c == '*' and self.source[i + 1] == '/') {
                i += 2;
                self.comments.append(self.allocator, Comment{
                    .content = self.source[start..i],
                    .span = .{ .start = start, .end = i },
                    .type = .MultiLine,
                }) catch unreachable;
                return i;
            }
            i += 1;
        }
        return error.UnterminatedMultiLineComment;
    }

    inline fn createToken(self: *Lexer, token_type: TokenType, lexeme: []const u8, start: usize, end: usize) Token {
        _ = self;
        return Token{ .type = token_type, .lexeme = lexeme, .span = .{ .start = start, .end = end } };
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
    };
}

pub fn getLexicalErrorHelp(error_type: LexicalError) []const u8 {
    return switch (error_type) {
        error.InvalidHexEscape => "Use format \\xHH where HH are valid hexadecimal digits (0-9, A-F)",
        error.UnterminatedString => "Add closing quote (' or \") to complete the string literal",
        error.UnterminatedRegex => "Add closing delimiter (/) to complete the regular expression",
        error.NonTerminatedTemplateLiteral => "Add closing backtick (`) to complete the template literal",
        error.UnterminatedRegexLiteral => "Add closing slash (/) and optional flags to complete the regex",
        error.InvalidRegexLineTerminator => "Remove line breaks or use a different regex pattern",
        error.InvalidRegex => "Check regex syntax for invalid patterns, unclosed groups, or invalid modifiers",
        error.InvalidIdentifierStart => "Identifiers must start with a letter (a-z, A-Z), underscore (_), or dollar sign ($)",
        error.UnterminatedMultiLineComment => "Add closing delimiter (*/) to complete the comment",
        error.InvalidUnicodeEscape => "Use format \\uHHHH (4 hex digits) or \\u{H+} (1-6 hex digits in braces)",
        error.InvalidOctalEscape => "Use format \\0-7, \\00-77, or \\000-377 for octal escapes",
        error.OctalEscapeInStrict => "Use hexadecimal (\\xHH) or Unicode (\\uHHHH) escape sequences instead",
        error.InvalidBinaryLiteral => "Binary literals (0b) must be followed by at least one binary digit (0 or 1)",
        error.InvalidOctalLiteralDigit => "Octal literals (0o) must be followed by at least one octal digit (0-7)",
        error.InvalidHexLiteral => "Hexadecimal literals (0x) must be followed by at least one hex digit (0-9, A-F)",
        error.InvalidExponentPart => "Exponent notation (e or E) must be followed by an integer",
        error.NumericSeparatorMisuse => "Remove the trailing underscore from the numeric literal",
        error.ConsecutiveNumericSeparators => "Separate digits with only a single underscore (_), not multiple consecutive underscores",
        error.MultipleDecimalPoints => "Use only one decimal point in a numeric literal",
        error.InvalidBigIntSuffix => "BigInt literals (suffix 'n') cannot contain fractional or exponential parts",
    };
}
