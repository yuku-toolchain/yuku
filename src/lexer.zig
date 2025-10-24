const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;

pub const Lexer = struct {
    source: []const u8,
    position: usize,

    pub fn init(source: []const u8) Lexer {
        return .{
            .source = source,
            .position = 0,
        };
    }

    pub fn nextToken(self: *Lexer) !Token {
        self.skipWhitespace();

        if (self.isAtEnd()) {
            return self.createEmptyToken(TokenType.EOF);
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
            '%' => self.scanPercent(),
            '0'...'9' => self.scanNumber(),
            '"', '\'' => self.scanString(),
            '/' => self.scanSlashOrRegex(),
            else => self.consumeSingleCharToken(TokenType.Invalid),
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
            if(next_3 == '='){
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

    fn scanString(self: *Lexer) Token {
        const start = self.position;
        const quote = self.currentChar();
        self.advanceBy(1);

        while (!self.isAtEnd()) {
            const c = self.currentChar();

            if (c == quote) {
                self.advanceBy(1);
                const end = self.position;
                return self.createToken(.StringLiteral, self.source[start..end], start, end);
            }

            if (c == '\\') {
                self.advanceBy(1);

                if (!self.isAtEnd()) {
                    const next = self.currentChar();
                    if (next == '\r' and self.peekAhead(1) == '\n') {
                        self.advanceBy(2);
                    } else {
                        self.advanceBy(1);
                    }
                }

                continue;
            }

            if (c == '\r' or c == '\n') {
                break;
            }

            self.advanceBy(1);
        }

        const end = self.position;
        return self.createToken(.Invalid, self.source[start..end], start, end);
    }

    fn scanSlashOrRegex(self: *Lexer) Token {
        const start = self.position;
        const next = self.peekAhead(1);

        // /=
        if (next == '=') {
            return self.consumeMultiCharToken(.SlashAssign, 2);
        }

        self.advanceBy(1);
        var in_class = false;

        // try regex first
        while (!self.isAtEnd()) {
            const c = self.currentChar();

            if (c == '\\') {
                self.advanceBy(1);
                if (!self.isAtEnd()) {
                    self.advanceBy(1);
                }
                continue;
            }

            if (c == '[') {
                in_class = true;
                self.advanceBy(1);
                continue;
            }

            if (c == ']') {
                in_class = false;
                self.advanceBy(1);
                continue;
            }

            if (c == '/' and !in_class) {
                self.advanceBy(1);

                while (!self.isAtEnd()) {
                    const flag = self.currentChar();
                    if (std.ascii.isAlphabetic(flag)) {
                        self.advanceBy(1);
                    } else {
                        break;
                    }
                }

                const end = self.position;
                return self.createToken(.RegexLiteral, self.source[start..end], start, end);
            }

            self.advanceBy(1);
        }

        // if it's not a valid regex, reset the position, it's a pure slash
        self.position = start;

        return self.consumeSingleCharToken(.Slash);
    }

    fn scanDot(self: *Lexer) Token {
        const next1 = self.peekAhead(1);
        const next2 = self.peekAhead(2);

        if(std.ascii.isDigit(next1)){
            return self.scanNumber();
        }

        if(next1 == '.' and next2 == '.'){
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
            self.advanceBy(1);
            while (!self.isAtEnd()) {
                if (std.ascii.isAlphanumeric(self.currentChar())) {
                    self.advanceBy(1);
                } else if (self.currentChar() == '_') {
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

    fn consumeSingleCharToken(self: *Lexer, token_type: TokenType) Token {
        const start = self.position;
        self.advanceBy(1);
        const lexeme = self.source[start..self.position];
        return self.createToken(token_type, lexeme, start, self.position);
    }

    fn consumeMultiCharToken(self: *Lexer, token_type: TokenType, length: u8) Token {
        const start = self.position;
        self.advanceBy(length);
        const end = self.position;
        return self.createToken(token_type, self.source[start..end], start, end);
    }

    fn createEmptyToken(self: *Lexer, token_type: TokenType) Token {
        return self.createToken(token_type, "", self.position, self.position);
    }

    fn createToken(self: *Lexer, token_type: TokenType, lexeme: []const u8, start: usize, end: usize) Token {
        _ = self;
        return Token{
            .type = token_type,
            .lexeme = lexeme,
            .span = .{ .start = start, .end = end },
        };
    }

    fn advanceBy(self: *Lexer, offset: u8) void {
        self.position += offset;
    }

    fn currentChar(self: *Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.position];
    }

    fn peekAhead(self: *Lexer, offset: u8) u8 {
        if (self.isAtEndWithOffset(offset)) return 0;
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

    fn isAtEndWithOffset(self: *Lexer, offset: u8) bool {
        return (self.position + offset) >= self.source.len;
    }
};
