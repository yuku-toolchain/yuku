const std = @import("std");

const Token = @import("token.zig").Token;
const Span = @import("token.zig").Span;
const TokenType = @import("token.zig").TokenType;
const Lexer = @import("lexer.zig").Lexer;
const Node = @import("ast.zig").Node;

const ParseErrorType = enum {
    LexicalError,
    UnexpectedToken,
    ExpectedToken,
    UnexpectedEof,
    InvalidSyntax,
    DuplicateDeclaration,
    InvalidAssignmentTarget,
};

const ParseError = struct {
    type: ParseErrorType,
    message: []const u8,
    span: Span,
    help: ?[][]const u8,
    severity: Severity,

    const Severity = enum { @"error", warning, info };
};

pub const Parser = struct {
    panic_mode: bool,
    source: []const u8,
    lexer: Lexer,
    current_token: Token,
    /// expects arena allocator
    allocator: std.mem.Allocator,
    errors: std.ArrayList(ParseError),

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Parser {
        var lexer = try Lexer.init(allocator, source);

        return Parser{ .lexer = lexer, .current_token = try lexer.nextToken(), .source = source, .allocator = allocator, .panic_mode = false, .errors = .empty };
    }

    pub fn parse(self: *Parser) !*Node {
        var body: std.ArrayList(*Node) = .empty;

        while (self.current_token.type != .EOF) {
            const stmt = try self.parseStatement();
            try body.append(self.allocator, stmt);
        }

        return self.createNode(.{ .program = .{
            .body = try body.toOwnedSlice(self.allocator)
        }});
    }

    fn parseStatement(self: *Parser) !*Node {
        return switch (self.current_token.type) {
            .Var, .Const, .Let => try self.parseVariableDeclaration(),
            else => {
                return error.InvalidSyntax;
            },
        };
    }

    fn parseExpression(self: *Parser) !*Node {
        return switch (self.current_token.type) {
            .Identifier => {
                const name = self.current_token.lexeme;

                _ = try self.advance();

                return self.createNode(.{ .identifier = .{ .name = name } });
            },
            else => {
                return error.InvalidSyntax;
            },
        };
    }

    fn parseVariableDeclaration(self: *Parser) !*Node {
        _ = try self.advance();
       return self.createNode(.{
           .identifier = .{
               .name = "cool"
           }
       });
    }

    inline fn advance(self: *Parser) !Token {
        const token = self.current_token;
        self.current_token = try self.lexer.nextToken();
        return token;
    }

    inline fn expect(self: *Parser, expected: TokenType) !Token {
        if (self.current_token.type == expected) {
            return try self.advance();
        }

        return error.ExpectedToken;
    }

    inline fn createNode(self: *Parser, node: Node) !*Node {
        const ptr = try self.allocator.create(Node);
        ptr.* = node;
        return ptr;
    }
};
