const std = @import("std");

const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Comment = @import("token.zig").Comment;
const CommentType = @import("token.zig").CommentType;

const LexError = error{
    UnterminatedString,
    UnterminatedRegex,
    NonTerminatedTemplateLiteral,
    UnterminatedRegexLiteral,
    InvalidRegexLineTerminator,
    InvalidRegex,
    IncompletePrivateIdentifier,
    InvalidPrivateIdentifierStart,
    UnexpectedCharacter,
    UnterminatedMultiLineComment,
};

pub const Lexer = struct {
    source: []const u8,
    position: usize,
    template_depth: usize,
    comments: std.array_list.Managed(Comment),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Lexer {
        return .{
            .source = source,
            .position = 0,
            .template_depth = 0,
            .comments = std.array_list.Managed(Comment).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Lexer) void {
        self.comments.deinit();
    }

    pub fn nextToken(self: *Lexer) LexError!Token {
        self.skipSkippable();

        if (self.position >= self.source.len) {
            return self.createToken(.EOF, "", self.position, self.position);
        }

        const current_char = self.source[self.position];

        return switch (current_char) {
            '+' => self.scanPlus(),
            '*' => self.scanStar(),
            '-' => self.scanMinus(),
            '.' => self.scanDot(),
            '!' => self.scanExclamation(),
            '<' => self.scanLessThan(),
            '>' => self.scanGreaterThan(),
            '=' => self.scanAssignOrEqualOrArrow(),
            '|' => self.scanOr(),
            '&' => self.scanAnd(),
            '^' => self.scanCaret(),
            '%' => self.scanPercent(),
            '0'...'9' => self.scanNumber(),
            '"', '\'' => self.scanString(),
            '/' => self.scanSlash(),
            '?' => self.scanQuestionMark(),
            '`' => self.scanTemplateLiteral(),
            '~' => self.consumeSingleCharToken(TokenType.BitwiseNot),
            '(' => self.consumeSingleCharToken(TokenType.LeftParen),
            ')' => self.consumeSingleCharToken(TokenType.RightParen),
            '{' => self.consumeSingleCharToken(TokenType.LeftBrace),
            '}' => self.handleRightBrace(),
            '[' => self.consumeSingleCharToken(TokenType.LeftBracket),
            ']' => self.consumeSingleCharToken(TokenType.RightBracket),
            ';' => self.consumeSingleCharToken(TokenType.Semicolon),
            ',' => self.consumeSingleCharToken(TokenType.Comma),
            ':' => self.consumeSingleCharToken(TokenType.Colon),
            '#' => self.scanPrivateIdentifier(),
            'a'...'z', 'A'...'Z', '_', '$' => self.scanIdentifierOrKeyword(),
            else => error.UnexpectedCharacter,
        };
    }

    fn scanQuestionMark(self: *Lexer) Token {
        const next_1 = if (self.position + 1 < self.source.len) self.source[self.position + 1] else 0;
        const next_2 = if (self.position + 2 < self.source.len) self.source[self.position + 2] else 0;

        if (next_1 == '?' and next_2 == '=') {
            return self.consumeMultiCharToken(.NullishAssign, 3);
        }

        return switch (next_1) {
            '?' => self.consumeMultiCharToken(.NullishCoalescing, 2),
            '.' => self.consumeMultiCharToken(.OptionalChaining, 2),
            else => self.consumeSingleCharToken(.Question),
        };
    }

    fn scanCaret(self: *Lexer) Token {
        const next_char = if (self.position + 1 < self.source.len) self.source[self.position + 1] else 0;

        return switch (next_char) {
            '=' => self.consumeMultiCharToken(.BitwiseXorAssign, 2),
            else => self.consumeSingleCharToken(.BitwiseXor),
        };
    }

    fn scanOr(self: *Lexer) Token {
        const next_1 = if (self.position + 1 < self.source.len) self.source[self.position + 1] else 0;
        const next_2 = if (self.position + 2 < self.source.len) self.source[self.position + 2] else 0;

        if (next_1 == '|' and next_2 == '=') {
            return self.consumeMultiCharToken(.LogicalOrAssign, 3);
        }

        return switch (next_1) {
            '|' => self.consumeMultiCharToken(.LogicalOr, 2),
            '=' => self.consumeMultiCharToken(.BitwiseOrAssign, 2),
            else => self.consumeSingleCharToken(.BitwiseOr),
        };
    }

    fn scanAnd(self: *Lexer) Token {
        const next_1 = if (self.position + 1 < self.source.len) self.source[self.position + 1] else 0;
        const next_2 = if (self.position + 2 < self.source.len) self.source[self.position + 2] else 0;

        if (next_1 == '&' and next_2 == '=') {
            return self.consumeMultiCharToken(.LogicalAndAssign, 3);
        }

        return switch (next_1) {
            '&' => self.consumeMultiCharToken(.LogicalAnd, 2),
            '=' => self.consumeMultiCharToken(.BitwiseAndAssign, 2),
            else => self.consumeSingleCharToken(.BitwiseAnd),
        };
    }

    fn scanGreaterThan(self: *Lexer) Token {
        const next_1 = if (self.position + 1 < self.source.len) self.source[self.position + 1] else 0;
        const next_2 = if (self.position + 2 < self.source.len) self.source[self.position + 2] else 0;
        const next_3 = if (self.position + 3 < self.source.len) self.source[self.position + 3] else 0;

        if (next_1 == '>' and next_2 == '=') {
            return self.consumeMultiCharToken(.RightShiftAssign, 3);
        }

        if (next_1 == '>' and next_2 == '>') {
            if (next_3 == '=') {
                return self.consumeMultiCharToken(.UnsignedRightShiftAssign, 4);
            } else {
                return self.consumeMultiCharToken(.UnsignedRightShift, 3);
            }
        }

        return switch (next_1) {
            '>' => self.consumeMultiCharToken(.RightShift, 2),
            '=' => self.consumeMultiCharToken(.GreaterThanEqual, 2),
            else => self.consumeSingleCharToken(.GreaterThan),
        };
    }

    fn scanLessThan(self: *Lexer) Token {
        const next_1 = if (self.position + 1 < self.source.len) self.source[self.position + 1] else 0;
        const next_2 = if (self.position + 2 < self.source.len) self.source[self.position + 2] else 0;

        if (next_1 == '<' and next_2 == '=') {
            return self.consumeMultiCharToken(.LeftShiftAssign, 3);
        }

        return switch (next_1) {
            '<' => self.consumeMultiCharToken(.LeftShift, 2),
            '=' => self.consumeMultiCharToken(.LessThanEqual, 2),
            else => self.consumeSingleCharToken(.LessThan),
        };
    }

    fn scanExclamation(self: *Lexer) Token {
        const next_1 = if (self.position + 1 < self.source.len) self.source[self.position + 1] else 0;
        const next_2 = if (self.position + 2 < self.source.len) self.source[self.position + 2] else 0;

        if (next_1 == '=' and next_2 == '=') {
            return self.consumeMultiCharToken(.StrictNotEqual, 3);
        }

        return switch (next_1) {
            '=' => self.consumeMultiCharToken(.NotEqual, 2),
            else => self.consumeSingleCharToken(.LogicalNot),
        };
    }

    fn scanAssignOrEqualOrArrow(self: *Lexer) Token {
        const next_1 = if (self.position + 1 < self.source.len) self.source[self.position + 1] else 0;
        const next_2 = if (self.position + 2 < self.source.len) self.source[self.position + 2] else 0;

        if (next_1 == '=' and next_2 == '=') {
            return self.consumeMultiCharToken(.StrictEqual, 3);
        }

        return switch (next_1) {
            '=' => self.consumeMultiCharToken(.Equal, 2),
            '>' => self.consumeMultiCharToken(.Arrow, 2),
            else => self.consumeSingleCharToken(.Assign),
        };
    }

    fn scanMinus(self: *Lexer) Token {
        const next_char = if (self.position + 1 < self.source.len) self.source[self.position + 1] else 0;

        return switch (next_char) {
            '-' => self.consumeMultiCharToken(.Decrement, 2),
            '=' => self.consumeMultiCharToken(.MinusAssign, 2),
            else => self.consumeSingleCharToken(.Minus),
        };
    }

    fn scanPercent(self: *Lexer) Token {
        const next_char = if (self.position + 1 < self.source.len) self.source[self.position + 1] else 0;

        return switch (next_char) {
            '=' => self.consumeMultiCharToken(.PercentAssign, 2),
            else => self.consumeSingleCharToken(.Percent),
        };
    }

    fn scanStar(self: *Lexer) Token {
        const next_1 = if (self.position + 1 < self.source.len) self.source[self.position + 1] else 0;
        const next_2 = if (self.position + 2 < self.source.len) self.source[self.position + 2] else 0;

        if (next_1 == '*' and next_2 == '=') {
            return self.consumeMultiCharToken(.ExponentAssign, 3);
        }

        return switch (next_1) {
            '*' => self.consumeMultiCharToken(.Exponent, 2),
            '=' => self.consumeMultiCharToken(.StarAssign, 2),
            else => self.consumeSingleCharToken(.Star),
        };
    }

    fn scanString(self: *Lexer) LexError!Token {
        const start = self.position;
        const quote = self.source[start];
        var i = start + 1;

        while (i < self.source.len) {
            const c = self.source[i];

            if (c < 128) {
                @branchHint(.likely);

                if (c == quote) {
                    i += 1;
                    self.position = i;
                    return self.createToken(.StringLiteral, self.source[start..i], start, i);
                }

                if (c == '\\') {
                    i += 1;
                    if (i >= self.source.len) break;

                    const next = self.source[i];
                    if (next == '\n') {
                        i += 1;
                    } else if (next == '\r') {
                        i += 1;
                        if (i < self.source.len and self.source[i] == '\n') {
                            i += 1;
                        }
                    } else {
                        i += 1;
                    }
                    continue;
                }

                if (c == '\r' or c == '\n') {
                    break;
                }

                i += 1;
            } else {
                @branchHint(.cold);
                i += 1;
            }
        }

        return error.UnterminatedString;
    }

    fn scanTemplateLiteral(self: *Lexer) LexError!Token {
        const start = self.position;
        var i = start + 1;

        while (i < self.source.len) {
            const c = self.source[i];

            if (c == '`') {
                i += 1;
                self.position = i;
                return self.createToken(.NoSubstitutionTemplate, self.source[start..i], start, i);
            }

            if (c == '$' and i + 1 < self.source.len and self.source[i + 1] == '{') {
                i += 2;
                self.template_depth += 1;
                self.position = i;
                return self.createToken(.TemplateHead, self.source[start..i], start, i);
            }

            if (c == '\\') {
                i += 1;
                if (i < self.source.len) {
                    i += 1;
                }
                continue;
            }

            i += 1;
        }

        return error.NonTerminatedTemplateLiteral;
    }

    fn scanTemplateMiddleOrTail(self: *Lexer) LexError!Token {
        const start = self.position;
        var i = start + 1;

        while (i < self.source.len) {
            const c = self.source[i];

            if (c == '`') {
                i += 1;
                if (self.template_depth > 0) {
                    self.template_depth -= 1;
                }
                self.position = i;
                return self.createToken(.TemplateTail, self.source[start..i], start, i);
            }

            if (c == '$' and i + 1 < self.source.len and self.source[i + 1] == '{') {
                i += 2;
                self.position = i;
                return self.createToken(.TemplateMiddle, self.source[start..i], start, i);
            }

            if (c == '\\') {
                i += 1;
                if (i < self.source.len) {
                    i += 1;
                }
                continue;
            }

            i += 1;
        }

        return error.NonTerminatedTemplateLiteral;
    }

    fn handleRightBrace(self: *Lexer) LexError!Token {
        // if we're inside a template substitution, scan template middle/tail
        if (self.template_depth > 0) {
            return self.scanTemplateMiddleOrTail();
        }

        return self.consumeSingleCharToken(TokenType.RightBrace);
    }

    fn scanSlash(self: *Lexer) Token {
        const next = if (self.position + 1 < self.source.len) self.source[self.position + 1] else 0;

        if (next == '=') {
            return self.consumeMultiCharToken(.SlashAssign, 2);
        }

        const slash = self.consumeSingleCharToken(.Slash);
        const token = self.reScanAsRegex(slash);

        if(@TypeOf(token) == Token){
            return token;
        }

        return slash;
    }

    pub fn reScanAsRegex(self: *Lexer, slash_token: Token) LexError!Token {
        self.position = slash_token.span.start;

        return self.scanRegex();
    }

    fn scanRegex(self: *Lexer) LexError!Token {
        const start = self.position;
        self.position += 1; // consume '/'

        var in_class = false;

        while (self.position < self.source.len) {
            const c = self.source[self.position];

            if (c == '\\') {
                self.position += 1; // skip backslash
                if (self.position < self.source.len) {
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

                while (self.position < self.source.len and
                       std.ascii.isAlphabetic(self.source[self.position])) {
                    self.position += 1;
                }

                return self.createToken(
                    .RegexLiteral,
                    self.source[start..self.position],
                    start,
                    self.position
                );
            }

            if (c == '\n' or c == '\r') {
                return error.InvalidRegexLineTerminator;
            }

            self.position += 1;
        }

        return error.UnterminatedRegexLiteral;
    }

    fn scanDot(self: *Lexer) Token {
        const next1 = if (self.position + 1 < self.source.len) self.source[self.position + 1] else 0;
        const next2 = if (self.position + 2 < self.source.len) self.source[self.position + 2] else 0;

        if (std.ascii.isDigit(next1)) {
            return self.scanNumber();
        }

        if (next1 == '.' and next2 == '.') {
            return self.consumeMultiCharToken(TokenType.Spread, 3);
        }

        return self.consumeSingleCharToken(.Dot);
    }

    fn scanPlus(self: *Lexer) Token {
        const next_char = if (self.position + 1 < self.source.len) self.source[self.position + 1] else 0;

        return switch (next_char) {
            '+' => self.consumeMultiCharToken(.Increment, 2),
            '=' => self.consumeMultiCharToken(.PlusAssign, 2),
            else => self.consumeSingleCharToken(.Plus),
        };
    }

    fn scanIdentifierOrKeyword(self: *Lexer) Token {
        const start = self.position;
        var i = start + 1;

        while (i < self.source.len) {
            const c = self.source[i];
            if (c < 128) {
                @branchHint(.likely);
                if (std.ascii.isAlphanumeric(c) or c == '_' or c == '$') {
                    i += 1;
                } else {
                    break;
                }
            } else {
                @branchHint(.cold);
                break;
            }
        }

        self.position = i;
        const lexeme = self.source[start..i];
        const token_type: TokenType = self.getKeywordType(lexeme);

        return self.createToken(token_type, lexeme, start, i);
    }

    fn scanPrivateIdentifier(self: *Lexer) LexError!Token {
        const start = self.position;
        var i = start + 1;

        if (i >= self.source.len) {
            return error.IncompletePrivateIdentifier;
        }

        const first = self.source[i];
        if (!std.ascii.isAlphabetic(first) and first != '_' and first != '$') {
            return error.InvalidPrivateIdentifierStart;
        }

        i += 1;

        while (i < self.source.len) {
            const c = self.source[i];
            if (std.ascii.isAlphanumeric(c) or c == '_' or c == '$') {
                i += 1;
            } else {
                break;
            }
        }

        self.position = i;
        return self.createToken(.PrivateIdentifier, self.source[start..i], start, i);
    }

    fn getKeywordType(self: *Lexer, lexeme: []const u8) TokenType {
        _ = self;
        switch (lexeme.len) {
            2 => {
                switch (lexeme[1]) {
                    'f' => {
                        return switch (lexeme[0]) {
                            'i' => .If, // "if"
                            'o' => .Of, // "of"
                            else => .Identifier,
                        };
                    },
                    'n' => if (lexeme[0] == 'i') return .In, // "in"
                    'o' => if (lexeme[0] == 'd') return .Do, // "do"
                    's' => if (lexeme[0] == 'a') return .As, // "as"
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

        if (self.source[i] == '0' and i + 1 < self.source.len) {
            const next = std.ascii.toLower(self.source[i + 1]);
            if (next == 'x') {
                token_type = .HexLiteral;
                i += 2;
                while (i < self.source.len and std.ascii.isHex(self.source[i])) {
                    i += 1;
                }
            } else if (next == 'o') {
                token_type = .OctalLiteral;
                i += 2;
                while (i < self.source.len and self.source[i] >= '0' and self.source[i] <= '7') {
                    i += 1;
                }
            } else if (next == 'b') {
                token_type = .BinaryLiteral;
                i += 2;
                while (i < self.source.len and (self.source[i] == '0' or self.source[i] == '1')) {
                    i += 1;
                }
            } else {
                while (i < self.source.len and std.ascii.isDigit(self.source[i])) {
                    i += 1;
                }
            }
        } else {
            while (i < self.source.len and std.ascii.isDigit(self.source[i])) {
                i += 1;
            }
        }

        if (token_type == .NumericLiteral and
            i < self.source.len and self.source[i] == '.' and
            i + 1 < self.source.len and std.ascii.isDigit(self.source[i + 1]))
        {
            i += 1;
            while (i < self.source.len and std.ascii.isDigit(self.source[i])) {
                i += 1;
            }
        }

        if (token_type == .NumericLiteral and i < self.source.len) {
            const cur = std.ascii.toLower(self.source[i]);
            if (cur == 'e' and i + 1 < self.source.len) {
                const next = self.source[i + 1];
                if (std.ascii.isDigit(next) or
                    ((next == '+' or next == '-') and
                        i + 2 < self.source.len and
                        std.ascii.isDigit(self.source[i + 2])))
                {
                    i += 1;
                    if (self.source[i] == '+' or self.source[i] == '-') {
                        i += 1;
                    }
                    while (i < self.source.len and std.ascii.isDigit(self.source[i])) {
                        i += 1;
                    }
                }
            }
        }

        if (i < self.source.len and self.source[i] == '_') {
            while (i < self.source.len) {
                const current_char = self.source[i];
                const next_char = if (i + 1 < self.source.len) self.source[i + 1] else 0;

                const char_to_check = if (current_char == '_') next_char else current_char;

                if ((std.ascii.isDigit(char_to_check) or (token_type == .HexLiteral and std.ascii.isAlphabetic(char_to_check))) and char_to_check != 'n') {
                    i += 1;
                } else {
                    break;
                }
            }
        }

        if (i < self.source.len and self.source[i] == 'n') {
            i += 1;
            token_type = .BigIntLiteral;
        }

        self.position = i;
        return self.createToken(token_type, self.source[start..i], start, i);
    }

    fn skipSkippable(self: *Lexer) void {
        var i = self.position;

        while (i < self.source.len) {
            const c = self.source[i];
            switch (c) {
                ' ', '\t', '\r', '\n' => {
                    @branchHint(.likely);
                    i += 1;
                },
                '/' => {
                    @branchHint(.likely);
                    if (i + 1 >= self.source.len) break;
                    const next = self.source[i + 1];
                    if (next == '/') {
                        self.position = i;
                        self.skipSingleLineComment() catch return;
                        i = self.position;
                    } else if (next == '*') {
                        self.position = i;
                        self.skipMultiLineComment() catch return;
                        i = self.position;
                    } else {
                        break;
                    }
                },
                else => {
                    @branchHint(.likely);
                    break;
                },
            }
        }

        self.position = i;
    }

    fn skipSingleLineComment(self: *Lexer) !void {
        const start = self.position;
        var i = start + 2;

        while (i < self.source.len) {
            const c = self.source[i];
            if (c == '\n' or c == '\r') {
                break;
            }
            i += 1;
        }

        self.position = i;
        try self.comments.append(Comment{
            .type = .SingleLine,
            .content = self.source[start..i],
            .span = .{ .start = start, .end = i },
        });
    }

    fn skipMultiLineComment(self: *Lexer) !void {
        const start = self.position;
        var i = start + 2;

        while (i < self.source.len) {
            const c = self.source[i];
            if (c == '*' and i + 1 < self.source.len and self.source[i + 1] == '/') {
                i += 2;
                self.position = i;
                try self.comments.append(Comment{
                    .type = .MultiLine,
                    .content = self.source[start..i],
                    .span = .{ .start = start, .end = i },
                });
                return;
            }
            i += 1;
        }

        return error.UnterminatedMultiLineComment;
    }

    fn consumeSingleCharToken(self: *Lexer, comptime token_type: TokenType) Token {
        const start = self.position;
        self.position += 1;
        return self.createToken(token_type, self.source[start..self.position], start, self.position);
    }

    fn consumeMultiCharToken(self: *Lexer, comptime token_type: TokenType, comptime length: u8) Token {
        const start = self.position;
        self.position += length;
        const end = self.position;
        return self.createToken(token_type, self.source[start..end], start, end);
    }

    fn createToken(self: *Lexer, token_type: TokenType, lexeme: []const u8, start: usize, end: usize) Token {
        _ = self;
        return Token{
            .type = token_type,
            .lexeme = lexeme,
            .span = .{ .start = start, .end = end }
        };
    }
};
