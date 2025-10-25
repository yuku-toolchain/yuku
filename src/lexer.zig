const std = @import("std");

const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;

const LexError = error{
    UnterminatedString,
    UnterminatedRegex,
    NonTerminatedTemplateLiteral,
    UnterminatedRegexLiteral,
    InvalidRegexLineTerminator,
    InvalidRegex,
    IncompletePrivateIdentifier,
    InvalidPrivateIdentifierStart,
    UnexpectedCharacter
};

pub const Lexer = struct {
    source: []const u8,
    position: usize,
    template_depth: usize,

    pub fn init(source: []const u8) Lexer {
        return .{
            .source = source,
            .position = 0,
            .template_depth = 0,
        };
    }

    pub fn nextToken(self: *Lexer) LexError!Token {
        self.skipWhitespace();

        if (self.isAtEnd()) {
            return self.createToken(.EOF, "", self.position, self.position);
        }

        const current_char = self.currentChar();

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
        const next_1 = self.peekAhead(1);
        const next_2 = self.peekAhead(2);

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
        const next_char = self.peekAhead(1);

        return switch (next_char) {
            '=' => self.consumeMultiCharToken(.BitwiseXorAssign, 2),
            else => self.consumeSingleCharToken(.BitwiseXor),
        };
    }

    fn scanOr(self: *Lexer) Token {
        const next_1 = self.peekAhead(1);
        const next_2 = self.peekAhead(2);

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
        const next_1 = self.peekAhead(1);
        const next_2 = self.peekAhead(2);

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
        const next_1 = self.peekAhead(1);
        const next_2 = self.peekAhead(2);
        const next_3 = self.peekAhead(3);

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
        const next_1 = self.peekAhead(1);
        const next_2 = self.peekAhead(2);

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
        const next_1 = self.peekAhead(1);
        const next_2 = self.peekAhead(2);

        if (next_1 == '=' and next_2 == '=') {
            return self.consumeMultiCharToken(.StrictNotEqual, 3);
        }

        return switch (next_1) {
            '=' => self.consumeMultiCharToken(.NotEqual, 2),
            else => self.consumeSingleCharToken(.LogicalNot),
        };
    }

    fn scanAssignOrEqualOrArrow(self: *Lexer) Token {
        const next_1 = self.peekAhead(1);
        const next_2 = self.peekAhead(2);

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
        const next_char = self.peekAhead(1);

        return switch (next_char) {
            '-' => self.consumeMultiCharToken(.Decrement, 2),
            '=' => self.consumeMultiCharToken(.MinusAssign, 2),
            else => self.consumeSingleCharToken(.Minus),
        };
    }

    fn scanPercent(self: *Lexer) Token {
        const next_char = self.peekAhead(1);

        return switch (next_char) {
            '=' => self.consumeMultiCharToken(.PercentAssign, 2),
            else => self.consumeSingleCharToken(.Percent),
        };
    }

    fn scanStar(self: *Lexer) Token {
        const next_1 = self.peekAhead(1);
        const next_2 = self.peekAhead(2);

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
        const quote = self.currentChar();
        self.advanceBy(1);

        while (!self.isAtEnd()) {
            const c = self.currentChar();

            // found closing quote
            if (c == quote) {
                self.advanceBy(1);
                const end = self.position;
                return self.createToken(.StringLiteral, self.source[start..end], start, end);
            }

            if (c == '\\') {
                self.advanceBy(1);

                if (self.isAtEnd()) {
                    break;
                }


                const next = self.currentChar();

                if (next == '\n') {
                    self.advanceBy(1);
                } else if (next == '\r') {
                    self.advanceBy(1);
                    if (!self.isAtEnd() and self.currentChar() == '\n') {
                        self.advanceBy(1);
                    }
                } else {
                    self.advanceBy(1);
                }
                continue;
            }

            if (c == '\r' or c == '\n') {
                break;
            }

            self.advanceBy(1);
        }

        return error.UnterminatedString;
    }

    fn scanTemplateLiteral(self: *Lexer) LexError!Token {
        const start = self.position;
        self.advanceBy(1);

        while (!self.isAtEnd()) {
            const c = self.currentChar();

            if (c == '`') {
                self.advanceBy(1);
                const end = self.position;
                return self.createToken(.NoSubstitutionTemplate, self.source[start..end], start, end);
            }

            if (c == '$' and self.peekAhead(1) == '{') {
                self.advanceBy(2); // consume ${
                self.template_depth += 1;
                const end = self.position;
                return self.createToken(.TemplateHead, self.source[start..end], start, end);
            }

            if (c == '\\') {
                self.advanceBy(1);
                if (!self.isAtEnd()) {
                    self.advanceBy(1);
                }
                continue;
            }

            self.advanceBy(1);
        }

        return error.NonTerminatedTemplateLiteral;
    }

    fn scanTemplateMiddleOrTail(self: *Lexer) LexError!Token {
        const start = self.position;
        self.advanceBy(1); // consume closing brace

        while (!self.isAtEnd()) {
            const c = self.currentChar();

            if (c == '`') {
                self.advanceBy(1); // consume closing backtick
                if (self.template_depth > 0) {
                    self.template_depth -= 1;
                }
                const end = self.position;
                return self.createToken(.TemplateTail, self.source[start..end], start, end);
            }

            if (c == '$' and self.peekAhead(1) == '{') {
                self.advanceBy(2); // consume ${
                const end = self.position;
                return self.createToken(.TemplateMiddle, self.source[start..end], start, end);
            }

            if (c == '\\') {
                self.advanceBy(1);
                if (!self.isAtEnd()) {
                    self.advanceBy(1);
                }
                continue;
            }

            self.advanceBy(1);
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
        const next = self.peekAhead(1);

        if (next == '=') {
            return self.consumeMultiCharToken(.SlashAssign, 2);
        }

        return self.consumeSingleCharToken(.Slash);
    }

    pub fn reScanAsRegex(self: *Lexer, slash_token: Token) Token {
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
        const next1 = self.peekAhead(1);
        const next2 = self.peekAhead(2);

        if (std.ascii.isDigit(next1)) {
            return self.scanNumber();
        }

        if (next1 == '.' and next2 == '.') {
            return self.consumeMultiCharToken(TokenType.Spread, 3);
        }

        return self.consumeSingleCharToken(.Dot);
    }

    fn scanPlus(self: *Lexer) Token {
        const next_char = self.peekAhead(1);

        return switch (next_char) {
            '+' => self.consumeMultiCharToken(.Increment, 2),
            '=' => self.consumeMultiCharToken(.PlusAssign, 2),
            else => self.consumeSingleCharToken(.Plus),
        };
    }

    fn scanIdentifierOrKeyword(self: *Lexer) Token {
        const start = self.position;

        self.advanceBy(1);

        while (!self.isAtEnd()) {
            const c = self.currentChar();
            if (std.ascii.isAlphanumeric(c) or c == '_' or c == '$') {
                self.advanceBy(1);
            } else {
                break;
            }
        }

        const end = self.position;
        const lexeme = self.source[start..end];

        const token_type: TokenType = self.getKeywordType(lexeme);

        return self.createToken(token_type, lexeme, start, end);
    }

    fn scanPrivateIdentifier(self: *Lexer) LexError!Token {
        const start = self.position;

        self.advanceBy(1);

        // must have at least one valid identifier character after #
        if (self.isAtEnd()) {
            return error.IncompletePrivateIdentifier;
        }

        const first = self.currentChar();
        if (!std.ascii.isAlphabetic(first) and first != '_' and first != '$') {
            return error.InvalidPrivateIdentifierStart;
        }

        while (!self.isAtEnd()) {
            const c = self.currentChar();
            if (std.ascii.isAlphanumeric(c) or c == '_' or c == '$') {
                self.advanceBy(1);
            } else {
                break;
            }
        }

        const end = self.position;
        return self.createToken(.PrivateIdentifier, self.source[start..end], start, end);
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

        if (self.currentChar() == '0' and !self.isAtEndWithOffset(1)) {
            const next = std.ascii.toLower(self.peekAhead(1));
            if (next == 'x') {
                token_type = .HexLiteral;
                self.advanceBy(2);
                self.consumeWhile(std.ascii.isHex);
            } else if (next == 'o') {
                token_type = .OctalLiteral;
                self.advanceBy(2);
                self.consumeWhileOctal();
            } else if (next == 'b') {
                token_type = .BinaryLiteral;
                self.advanceBy(2);
                self.consumeWhileBinary();
            } else {
                self.consumeWhile(std.ascii.isDigit);
            }
        } else {
            self.consumeWhile(std.ascii.isDigit);
        }

        if (token_type == .NumericLiteral and
            self.currentChar() == '.' and
            !self.isAtEndWithOffset(1) and
            std.ascii.isDigit(self.peekAhead(1)))
        {
            self.advanceBy(1);
            self.consumeWhile(std.ascii.isDigit);
        }

        if (token_type == .NumericLiteral) {
            const cur = std.ascii.toLower(self.currentChar());
            if (cur == 'e') {
                const next = self.peekAhead(1);
                if (std.ascii.isDigit(next) or
                    ((next == '+' or next == '-') and
                        !self.isAtEndWithOffset(2) and
                        std.ascii.isDigit(self.peekAhead(2))))
                {
                    self.advanceBy(1);
                    if (self.currentChar() == '+' or self.currentChar() == '-') {
                        self.advanceBy(1);
                    }
                    self.consumeWhile(std.ascii.isDigit);
                }
            }
        }

        if (self.currentChar() == '_') {
            while (!self.isAtEnd()) {
                const current_char = self.currentChar();
                const next_char = self.peekAhead(1);

                const char_to_check = if (current_char == '_') next_char else current_char;

                if ((std.ascii.isDigit(char_to_check) or (token_type == .HexLiteral and std.ascii.isAlphabetic(char_to_check))) and char_to_check != 'n') {
                    self.advanceBy(1);
                } else {
                    break;
                }
            }
        }

        if (self.currentChar() == 'n') {
            self.advanceBy(1);
            token_type = .BigIntLiteral;
        }

        const end = self.position;
        return self.createToken(token_type, self.source[start..end], start, end);
    }

    fn consumeWhileOctal(self: *Lexer) void {
        while (!self.isAtEnd()) {
            const c = self.currentChar();
            if (c >= '0' and c <= '7') {
                self.advanceBy(1);
            } else {
                break;
            }
        }
    }

    fn consumeWhileBinary(self: *Lexer) void {
        while (!self.isAtEnd()) {
            const c = self.currentChar();
            if (c == '0' or c == '1') {
                self.advanceBy(1);
            } else {
                break;
            }
        }
    }

    fn skipWhitespace(self: *Lexer) void {
        while (!self.isAtEnd()) {
            const current_char = self.currentChar();
            switch (current_char) {
                ' ', '\t', '\r', '\n' => self.advanceBy(1),
                else => break,
            }
        }
    }

    fn consumeSingleCharToken(self: *Lexer, comptime token_type: TokenType) Token {
        const start = self.position;
        self.advanceBy(1);
        const lexeme = self.source[start..self.position];
        return self.createToken(token_type, lexeme, start, self.position);
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

    fn advanceBy(self: *Lexer, comptime offset: u8) void {
        self.position += offset;
    }

    fn currentChar(self: *Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.position];
    }

    fn peekAhead(self: *Lexer, offset: u8) u8 {
        if ((self.position + offset) >= self.source.len) return 0;
        return self.source[self.position + offset];
    }

    fn consumeWhile(self: *Lexer, comptime predicate: fn (u8) bool) void {
        while (!self.isAtEnd() and predicate(self.currentChar())) {
            self.advanceBy(1);
        }
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.position >= self.source.len;
    }

    fn isAtEndWithOffset(self: *Lexer, comptime offset: u8) bool {
        return (self.position + offset) >= self.source.len;
    }
};
