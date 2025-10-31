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
    InvalidAssignmentTarget,
    OutOfMemory,
};

const ErrorInfo = struct {
    message: []const u8,
    span: token.Span,
    help: []const u8,
};

pub const Parser = struct {
    source: []const u8,
    lexer: lexer.Lexer,
    current: token.Token,
    peek: token.Token,
    allocator: std.mem.Allocator,
    errors: std.ArrayList(ErrorInfo),

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Parser {
        var lex = try lexer.Lexer.init(allocator, source);
        const current = try lex.nextToken();
        const peek = try lex.nextToken();

        return .{
            .source = source,
            .lexer = lex,
            .current = current,
            .peek = peek,
            .allocator = allocator,
            .errors = .empty,
        };
    }

    pub fn parse(self: *Parser) !*ast.Node {
        const start = self.current.span.start;
        var body: std.ArrayList(*ast.Node) = .empty;

        while (self.current.type != .EOF) {
            const stmt = try self.parseStatement();
            try body.append(self.allocator, stmt);
        }

        const end = self.current.span.end;
        return self.node(.{ .program = .{
            .body = try body.toOwnedSlice(self.allocator),
            .span = .{ .start = start, .end = end },
        }});
    }

    fn parseStatement(self: *Parser) ParseError!*ast.Node {
        return switch (self.current.type) {
            .Var, .Const, .Let, .Using => self.parseVariableDeclaration(),
            .Await => if (self.peek.type == .Using)
                self.parseVariableDeclaration()
            else
                error.InvalidSyntax,
            else => error.InvalidSyntax,
        };
    }

    fn parseVariableDeclaration(self: *Parser) ParseError!*ast.Node {
        const start = self.current.span.start;

        const kind: ast.VariableDeclaration.VariableKind = blk: {
            if (self.current.type == .Await) {
                try self.advance();
                if (self.current.type != .Using) return error.ExpectedToken;
                try self.advance();
                break :blk .@"await using";
            }

            const k = switch (self.current.type) {
                .Var => ast.VariableDeclaration.VariableKind.@"var",
                .Let => .let,
                .Const => .@"const",
                .Using => .using,
                else => return error.UnexpectedToken,
            };
            try self.advance();
            break :blk k;
        };

        const is_using = kind == .using or kind == .@"await using";
        var declarators: std.ArrayList(*ast.Node) = .empty;

        try declarators.append(self.allocator, try self.parseDeclarator(is_using));

        while (self.current.type == .Comma) {
            try self.advance();
            try declarators.append(self.allocator, try self.parseDeclarator(is_using));
        }

        if (self.current.type != .Semicolon) return error.ExpectedToken;
        const end = self.current.span.end;
        try self.advance();

        return self.node(.{ .variable_declaration = .{
            .kind = kind,
            .declarations = try declarators.toOwnedSlice(self.allocator),
            .span = .{ .start = start, .end = end },
        }});
    }

    fn parseDeclarator(self: *Parser, is_using: bool) ParseError!*ast.Node {
        const start = self.current.span.start;

        if (self.current.type != .Identifier) return error.ExpectedToken;

        const id_name = self.current.lexeme;
        const id_span = self.current.span;
        try self.advance();

        const id = try self.node(.{ .identifier = .{
            .name = id_name,
            .span = id_span,
        }});

        var init_: ?*ast.Node = null;
        if (self.current.type == .Assign) {
            try self.advance();
            init_ = try self.parseExpression();
        }

        if (is_using and init_ == null) return error.ExpectedToken;

        const end = if (init_) |i| i.getSpan().end else id_span.end;

        return self.node(.{ .variable_declarator = .{
            .id = id,
            .init = init_,
            .span = .{ .start = start, .end = end },
        }});
    }

    fn parseExpression(self: *Parser) ParseError!*ast.Node {
        return self.parsePrimaryExpression();
    }

    fn parsePrimaryExpression(self: *Parser) ParseError!*ast.Node {
        return switch (self.current.type) {
            .Identifier => self.parseIdentifier(),
            .NumericLiteral, .HexLiteral, .OctalLiteral, .BinaryLiteral, .BigIntLiteral => self.parseNumericLiteral(),
            .StringLiteral => self.parseStringLiteral(),
            .True, .False => self.parseBooleanLiteral(),
            .NullLiteral => self.parseNullLiteral(),
            .LeftParen => self.parseParenthesizedExpression(),
            .LeftBracket => self.parseArrayExpression(),
            .LeftBrace => self.parseObjectExpression(),
            else => error.InvalidSyntax,
        };
    }

    fn parseIdentifier(self: *Parser) ParseError!*ast.Node {
        const name = self.current.lexeme;
        const span = self.current.span;
        try self.advance();
        return self.node(.{ .identifier = .{ .name = name, .span = span } });
    }

    fn parseNumericLiteral(self: *Parser) ParseError!*ast.Node {
        _ = self;
        return error.InvalidSyntax;
    }

    fn parseStringLiteral(self: *Parser) ParseError!*ast.Node {
        _ = self;
        return error.InvalidSyntax;
    }

    fn parseBooleanLiteral(self: *Parser) ParseError!*ast.Node {
        _ = self;
        return error.InvalidSyntax;
    }

    fn parseNullLiteral(self: *Parser) ParseError!*ast.Node {
        _ = self;
        return error.InvalidSyntax;
    }

    fn parseParenthesizedExpression(self: *Parser) ParseError!*ast.Node {
        _ = self;
        return error.InvalidSyntax;
    }

    fn parseArrayExpression(self: *Parser) ParseError!*ast.Node {
        _ = self;
        return error.InvalidSyntax;
    }

    fn parseObjectExpression(self: *Parser) ParseError!*ast.Node {
        _ = self;
        return error.InvalidSyntax;
    }

    inline fn advance(self: *Parser) ParseError!void {
        self.current = self.peek;
        self.peek = self.lexer.nextToken() catch |err| {
            try self.errors.append(self.allocator, .{
                .message = lexer.getLexicalErrorMessage(err),
                .help = lexer.getLexicalErrorHelp(err),
                .span = .{
                    .start = self.current.span.end,
                    .end = self.lexer.position,
                },
            });
            return error.LexicalError;
        };
    }

    inline fn node(self: *Parser, n: ast.Node) !*ast.Node {
        const ptr = try self.allocator.create(ast.Node);
        ptr.* = n;
        return ptr;
    }
};
