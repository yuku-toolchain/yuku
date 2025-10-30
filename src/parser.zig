const std = @import("std");

const token = @import("token.zig");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");

const ParseError = error{
    LexicalError,
    UnexpectedToken,
    ExpectedToken,
    UnexpectedEof,
    InvalidSyntax,
    DuplicateDeclaration,
    InvalidAssignmentTarget,
    OutOfMemory,
};

const ParseErrorData = struct {
    message: []const u8,
    span: token.Span,
    help: []const u8,
    severity: Severity,

    const Severity = enum { @"error", warning, info };
};

pub const Parser = struct {
    panic_mode: bool,
    source: []const u8,
    lexer: lexer.Lexer,
    current_token: token.Token,
    /// expects arena allocator
    allocator: std.mem.Allocator,
    errors: std.ArrayList(ParseErrorData),

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Parser {
        var lexer_instance = try lexer.Lexer.init(allocator, source);

        return Parser{ .lexer = lexer_instance, .current_token = try lexer_instance.nextToken(), .source = source, .allocator = allocator, .panic_mode = false, .errors = .empty };
    }

    pub fn parse(self: *Parser) !*ast.Node {
        var body: std.ArrayList(*ast.Node) = .empty;

        while (self.current_token.type != .EOF) {
            const stmt = try self.parseStatement();
            try body.append(self.allocator, stmt);
        }

        return self.createNode(.{ .program = .{ .body = try body.toOwnedSlice(self.allocator) } });
    }

    fn parseStatement(self: *Parser) !*ast.Node {
        return switch (self.current_token.type) {
            .Var, .Const, .Let => try self.parseVariableDeclaration(),
            else => {
                return error.InvalidSyntax;
            },
        };
    }

    fn parseExpression(self: *Parser) !*ast.Node {
        return switch (self.current_token.type) {
            .Identifier => {
                const name = self.current_token.lexeme;

                try self.advance();

                return self.createNode(.{ .identifier = .{ .name = name } });
            },
            else => {
                return error.InvalidSyntax;
            },
        };
    }

    fn parseVariableDeclaration(self: *Parser) !*ast.Node {
        try self.advance();
        return self.createNode(.{ .identifier = .{ .name = "cool" } });
    }

    inline fn advance(self: *Parser) ParseError!void {
        self.current_token = self.lexer.nextToken() catch |err| {
            try self.errors.append(self.allocator, .{ .message = lexer.getLexicalErrorMessage(err), .help = lexer.getLexicalErrorHelp(err), .severity = .@"error", .span = .{
                .start = self.current_token.span.end,
                .end = self.lexer.position,
            } });

            return error.LexicalError;
        };
    }

    inline fn createNode(self: *Parser, node: ast.Node) !*ast.Node {
        const ptr = try self.allocator.create(ast.Node);
        ptr.* = node;
        return ptr;
    }
};
