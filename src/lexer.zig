const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;

pub const Lexer = struct {
    source: []const u8,
    position: usize,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Lexer {
        return .{
            .source = source,
            .position = 0,
            .allocator = allocator,
        };
    }

    pub fn nextToken(self: *Lexer) !Token {
        self.skipWhitespace();

        if (self.isAtEnd()) {
            return self.emptyToken(TokenType.EOF);
        }

        const c = self.peek();

        return switch (c) {
            '+' => self.singleCharToken(TokenType.Plus),
            ')' => self.singleCharToken(TokenType.RightParen),
            '{' => self.singleCharToken(TokenType.LeftBrace),
            '}' => self.singleCharToken(TokenType.RightBrace),
            ';' => self.singleCharToken(TokenType.Semicolon),
            ',' => self.singleCharToken(TokenType.Comma),
            '-' => self.singleCharToken(TokenType.Minus),
            '*' => self.singleCharToken(TokenType.Star),
            '/' => self.singleCharToken(TokenType.Slash),
            '<' => self.singleCharToken(TokenType.LessThan),
            '>' => self.singleCharToken(TokenType.GreaterThan),
             '0'...'9' => self.scanNumber(),
            else => self.emptyToken(TokenType.Invalid)
        };
    }

    fn scanNumber(self: *Lexer) Token {
        const start = self.position;

        while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        while (!self.isAtEndOffset(1) and self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            _ = self.advance();

            while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        const end = self.position;

        const number = self.source[start..end];
        return Lexer.makeToken(.NumericLiteral, number, start, end);
    }

    fn skipWhitespace(self: *Lexer) void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            switch (c) {
                ' ', '\t', '\r' => _ = self.advance(),
                else => break,
            }
        }
    }

    fn singleCharToken(self: *Lexer, token_type: TokenType) Token {
        const start = self.position;
        _ = self.advance();
        const c = self.source[start..self.position];
        return Lexer.makeToken(token_type, c, self.position, self.position);
    }

    fn emptyToken(self: *Lexer, token_type: TokenType) Token {
        return Lexer.makeToken(token_type, "", self.position, self.position);
    }

    fn advance(self: *Lexer) u8 {
        const c = self.source[self.position];
        self.position += 1;
        return c;
    }

    fn peek(self: *Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.position];
    }

    fn peekNext(self: *Lexer) u8 {
        if(self.position + 1 >= self.source.len) return 0;
        return self.source[self.position + 1];
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.position >= self.source.len;
    }

    fn isAtEndOffset(self: *Lexer, offset: u8) bool {
        return (self.position + offset) >= self.source.len;
    }

    fn makeToken(toke_type: TokenType, lexeme: []const u8, start: usize, end: usize) Token {
        return Token{ .type = toke_type, .lexeme = lexeme, .span = .{
            .start = start,
            .end = end
        }};
    }
};
