const std = @import("std");
const Token = @import("token.zig").Token;
const Span = @import("token.zig").Span;
const TokenType = @import("token.zig").TokenType;
const Lexer = @import("lexer.zig").Lexer;
const AstNode = @import("ast.zig").AstNode;
const Program = @import("ast.zig").Program;

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

        return Parser{
            .lexer = lexer,
            .current_token = try lexer.nextToken(),
            .source = source,
            .allocator = allocator,
            .panic_mode = false,
            .errors = .empty
        };
    }

    pub fn parse(self: *Parser) !AstNode {
        var body: std.ArrayList(AstNode) = .empty;

        while (self.current_token.type != .EOF) {
            const stmt = try self.parseStatement();
            try body.append(self.allocator, stmt);
        }

        return AstNode{ .program = Program{
            .body = try body.toOwnedSlice(self.allocator),
        } };
    }

    fn parseStatement(self: *Parser) !AstNode {
        return switch (self.current_token.type) {
            else => unreachable,
        };
    }
};
