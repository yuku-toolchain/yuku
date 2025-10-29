const std = @import("std");

const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Comment = @import("token.zig").Comment;
const CommentType = @import("token.zig").CommentType;
const unicodeJsHelpers = @import("unicode/js-helpers.zig");
const util = @import("util.zig");

const LexError = error{
    UnterminatedString,
    UnterminatedRegex,
    NonTerminatedTemplateLiteral,
    UnterminatedRegexLiteral,
    InvalidRegexLineTerminator,
    InvalidRegex,
    InvalidIdentifierStart,
    UnexpectedCharacter,
    UnterminatedMultiLineComment,
    InvalidUnicodeEscape,
    InvalidHexEscape,
    InvalidOctalEscape,
    OctalInStrict,
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

    pub fn deinit(self: *Lexer) void {
        self.comments.deinit(self.allocator);
        self.allocator.free(self.source);
    }

    pub fn nextToken(self: *Lexer) LexError!Token {
        self.skipSkippable();

        if (self.position >= self.source_len) {
            return self.createToken(.EOF, "", self.position, self.position);
        }

        const current_char = self.source[self.position];

        return switch (current_char) {
            '+', '*', '-', '!', '<', '>', '=', '|', '&', '^', '%', '/', '?' => self.scanPunctuation(),
            '.' => self.scanDot(),
            '0'...'9' => self.scanNumber(),
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

    fn scanPunctuation(self: *Lexer) LexError!Token {
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

                const slash = self.createToken(.Slash, self.source[start..self.position], start, self.position);

                // TODO: remove this, this is added now just for testing
                // when starting parser, should remove this handle scanning regex from parser.
                const token = self.reScanAsRegex(slash);
                if (@TypeOf(token) == Token) {
                    break :blk token;
                }
                //

                break :blk slash;
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

    fn scanString(self: *Lexer) LexError!Token {
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

    fn scanTemplateLiteral(self: *Lexer) LexError!Token {
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

    fn scanTemplateMiddleOrTail(self: *Lexer) LexError!Token {
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

    fn consumeEscape(self: *Lexer, pos: usize) LexError!usize {
        var i = pos + 1;

        brk: switch (self.source[i]) {
            '0' => {
                const c1 = self.source[i + 1];
                // null
                if (!util.isOctalDigit(c1)) {
                    i += 1;
                    break :brk;
                }

                if (self.strict_mode) return error.OctalInStrict;

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
                if (self.strict_mode) return error.OctalInStrict;

                i = try self.consumeOctal(i);
            },

            else => {
                i += 1;
            },
        }

        return i;
    }

    fn consumeOctal(self: *Lexer, pos: usize) LexError!usize {
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

    fn consumeHex(self: *Lexer, pos: usize) LexError!usize {
        const c1 = self.source[pos + 1];
        const c2 = self.source[pos + 2];

        if (!std.ascii.isHex(c1) or !std.ascii.isHex(c2)) {
            return error.InvalidHexEscape;
        }

        return pos + 3;
    }

    fn consumeUnicodeEscape(self: *Lexer, pos: usize) LexError!usize {
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

    fn handleRightBrace(self: *Lexer) LexError!Token {
        if (self.template_depth > 0) {
            return self.scanTemplateMiddleOrTail();
        }

        const start = self.position;
        self.position += 1;
        return self.createToken(.RightBrace, self.source[start..self.position], start, self.position);
    }

    pub fn reScanAsRegex(self: *Lexer, slash_token: Token) LexError!Token {
        self.position = slash_token.span.start;

        return self.scanRegex();
    }

    fn scanRegex(self: *Lexer) LexError!Token {
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

    fn scanDot(self: *Lexer) Token {
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

    fn scanNumber(self: *Lexer) Token {
        const start = self.position;
        var token_type: TokenType = .NumericLiteral;
        var i = self.position;

        if (self.source[i] == '0') {
            const c1 = std.ascii.toLower(self.source[i + 1]);
            if (c1 == 'x') {
                token_type = .HexLiteral;
                i += 2;
                while (i < self.source_len and std.ascii.isHex(self.source[i])) {
                    i += 1;
                }
            } else if (c1 == 'o') {
                token_type = .OctalLiteral;
                i += 2;
                while (i < self.source_len and util.isOctalDigit(self.source[i])) {
                    i += 1;
                }
            } else if (c1 == 'b') {
                token_type = .BinaryLiteral;
                i += 2;
                while (i < self.source_len and (self.source[i] == '0' or self.source[i] == '1')) {
                    i += 1;
                }
            } else {
                while (i < self.source_len and std.ascii.isDigit(self.source[i])) {
                    i += 1;
                }
            }
        } else {
            while (i < self.source_len and std.ascii.isDigit(self.source[i])) {
                i += 1;
            }
        }

        if (token_type == .NumericLiteral and
            i < self.source_len and self.source[i] == '.' and
            std.ascii.isDigit(self.source[i + 1]))
        {
            i += 1;
            while (i < self.source_len and std.ascii.isDigit(self.source[i])) {
                i += 1;
            }
        }

        if (token_type == .NumericLiteral and i < self.source_len) {
            const c0 = std.ascii.toLower(self.source[i]);
            if (c0 == 'e') {
                const c1 = self.source[i + 1];
                if (std.ascii.isDigit(c1) or
                    ((c1 == '+' or c1 == '-') and
                        std.ascii.isDigit(self.source[i + 2])))
                {
                    i += 1;
                    if (self.source[i] == '+' or self.source[i] == '-') {
                        i += 1;
                    }
                    while (i < self.source_len and std.ascii.isDigit(self.source[i])) {
                        i += 1;
                    }
                }
            }
        }

        if (i < self.source_len and self.source[i] == '_') {
            while (i < self.source_len) {
                const c0 = self.source[i];
                const c1 = self.source[i + 1];

                const char_to_check = if (c0 == '_') c1 else c0;

                if ((std.ascii.isDigit(char_to_check) or (token_type == .HexLiteral and std.ascii.isAlphabetic(char_to_check))) and char_to_check != 'n') {
                    i += 1;
                } else {
                    break;
                }
            }
        }

        if (i < self.source_len and self.source[i] == 'n') {
            i += 1;
            token_type = .BigIntLiteral;
        }

        self.position = i;
        return self.createToken(token_type, self.source[start..i], start, i);
    }

    inline fn skipSkippable(self: *Lexer) void {
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
                            self.position = i;
                            self.skipSingleLineComment() catch return;
                            i = self.position;
                            continue;
                        } else if (c1 == '*') {
                            self.position = i;
                            self.skipMultiLineComment() catch return;
                            i = self.position;
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

    inline fn skipSingleLineComment(self: *Lexer) !void {
        const start = self.position;
        var i = start + 2;

        while (i < self.source_len) {
            const c = self.source[i];
            if (c == '\n' or c == '\r') {
                break;
            }
            i += 1;
        }

        self.position = i;
        try self.comments.append(self.allocator, Comment{
            .content = self.source[start..i],
            .span = .{ .start = start, .end = i },
            .type = .SingleLine,
        });
    }

    inline fn skipMultiLineComment(self: *Lexer) !void {
        const start = self.position;
        var i = start + 2;

        while (i < self.source_len) {
            const c = self.source[i];
            if (c == '*' and self.source[i + 1] == '/') {
                i += 2;
                self.position = i;
                try self.comments.append(self.allocator, Comment{
                    .content = self.source[start..i],
                    .span = .{ .start = start, .end = i },
                    .type = .MultiLine,
                });
                return;
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
