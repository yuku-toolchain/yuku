const std = @import("std");
const token = @import("token.zig");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");

pub const Error = struct {
    message: []const u8,
    span: token.Span,
    help: ?[]const u8 = null,
};

pub const ParseResult = struct {
    program: ?ast.Program,
    errors: []Error,

    pub inline fn hasErrors(self: ParseResult) bool {
        return self.errors.len > 0;
    }
};

pub const Parser = struct {
    source: []const u8,
    lexer: lexer.Lexer,
    current: token.Token,
    peek: token.Token,
    /// expects arena allocator
    allocator: std.mem.Allocator,
    errors: std.ArrayList(Error),

    scratch_body: std.ArrayList(*ast.Body),
    scratch_declarators: std.ArrayList(*ast.VariableDeclarator),
    scratch_expressions: std.ArrayList(*ast.Expression),

    panic_mode: bool = false,

    const estimated_nodes_per_line = 2;
    const avg_chars_per_line = 40;
    const initial_error_capacity = 8;

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Parser {
        var lex = try lexer.Lexer.init(allocator, source);

        const current = lex.nextToken() catch token.Token.eof(0);
        const peek = lex.nextToken() catch token.Token.eof(0);

        return .{
            .source = source,
            .lexer = lex,
            .current = current,
            .peek = peek,
            .allocator = allocator,
            .errors = .empty,

            .scratch_body = std.ArrayList(*ast.Body).empty,
            .scratch_declarators = std.ArrayList(*ast.VariableDeclarator).empty,
            .scratch_expressions = std.ArrayList(*ast.Expression).empty,

            .panic_mode = false,
        };
    }

    pub fn parse(self: *Parser) !ParseResult {
        const start = self.current.span.start;

        const estimated_lines = self.source.len / avg_chars_per_line;
        const estimated_statements = @max(estimated_lines / 2, 16);
        const estimated_errors = @min(@max(estimated_lines / 50, 2), initial_error_capacity);

        self.scratch_body.clearRetainingCapacity();
        try self.scratch_body.ensureTotalCapacity(self.allocator, estimated_statements);

        try self.errors.ensureTotalCapacity(self.allocator, estimated_errors);

        while (self.current.type != .EOF) {
            const stmt = self.parseStatement() orelse {
                if (!self.panic_mode) self.panic_mode = true;
                self.synchronize();
                continue;
            };

            const body_item = try self.createNode(ast.Body, .{ .statement = stmt });
            self.scratch_body.appendAssumeCapacity(body_item);
            self.panic_mode = false;
        }

        const end = self.current.span.end;

        const body = try self.allocator.dupe(*ast.Body, self.scratch_body.items);

        const program = ast.Program{
            .body = body,
            .span = .{ .start = start, .end = end },
        };

        return ParseResult{
            .program = program,
            .errors = try self.errors.toOwnedSlice(self.allocator),
        };
    }

    fn parseStatement(self: *Parser) ?*ast.Statement {
        return switch (self.current.type) {
            .Var, .Const, .Let, .Using => self.parseVariableDeclaration(),
            .Await => {
                if (self.peek.type == .Using)
                    return self.parseVariableDeclaration()
                else {
                    self.recordError("Invalid await usage", "Expected 'using' after 'await'");
                    return null;
                }
            },
            else => {
                self.recordError("Unexpected token", "Expected a statement");
                return null;
            },
        };
    }

    fn parseVariableDeclaration(self: *Parser) ?*ast.Statement {
        const start = self.current.span.start;
        const kind = self.parseVariableDeclarationKind() orelse return null;

        self.scratch_declarators.clearRetainingCapacity();
        self.scratch_declarators.ensureTotalCapacity(self.allocator, 4) catch {};

        // first declarator
        const first_decl = self.parseVariableDeclarator(kind) orelse return null;
        self.scratch_declarators.appendAssumeCapacity(first_decl);

        // additional declarators
        while (self.current.type == .Comma) {
            self.advance();
            const decl = self.parseVariableDeclarator(kind) orelse return null;
            self.scratch_declarators.append(self.allocator, decl) catch unreachable;
        }

        self.eatSemi();

        const end = self.current.span.end;

        const declarations = self.allocator.dupe(*ast.VariableDeclarator, self.scratch_declarators.items) catch unreachable;

        const var_decl = ast.VariableDeclaration{
            .kind = kind,
            .declarations = declarations,
            .span = .{ .start = start, .end = end },
        };

        return self.createNode(ast.Statement, .{ .variable_declaration = var_decl }) catch null;
    }

    fn parseVariableDeclarationKind(self: *Parser) ?ast.VariableDeclaration.VariableDeclarationKind {
        return switch (self.current.type) {
            .Await => blk: {
                self.advance();
                if (self.current.type != .Using) {
                    self.recordError("Expected 'using' after 'await'", null);
                    return null;
                }
                self.advance();
                break :blk .@"await using";
            },
            .Var => blk: {
                self.advance();
                break :blk .@"var";
            },
            .Let => blk: {
                @branchHint(.likely);
                self.advance();
                break :blk .let;
            },
            .Const => blk: {
                @branchHint(.likely);
                self.advance();
                break :blk .@"const";
            },
            .Using => blk: {
                self.advance();
                break :blk .using;
            },
            else => {
                self.recordError("Expected variable declaration", "Expected 'var', 'let', 'const', or 'using'");
                return null;
            },
        };
    }

    fn parseVariableDeclarator(
        self: *Parser,
        kind: ast.VariableDeclaration.VariableDeclarationKind,
    ) ?*ast.VariableDeclarator {
        const start = self.current.span.start;
        const id = self.parseBindingPattern() orelse return null;

        var init_expr: ?*ast.Expression = null;

        if (self.current.type == .Assign) {
            self.advance();
            init_expr = self.parseExpression();
        }

        const requires_init = kind == .@"const" or
            kind == .using or
            kind == .@"await using";

        if (init_expr == null and requires_init) {
            self.recordError(
                "Variable declaration missing required initializer",
                "Add '= value' after the variable name to complete the declaration",
            );
            return null;
        }

        const end = if (init_expr) |expr| expr.getSpan().end else id.getSpan().end;

        return self.createNode(ast.VariableDeclarator, .{
            .id = id,
            .init = init_expr,
            .span = .{ .start = start, .end = end },
        }) catch null;
    }

    fn parseExpression(self: *Parser) ?*ast.Expression {
        return self.parsePrimaryExpression();
    }

    fn parsePrimaryExpression(self: *Parser) ?*ast.Expression {
        return switch (self.current.type) {
            .Identifier => self.parseIdentifierReference(),
            .StringLiteral => self.parseStringLiteral(),
            else => {
                self.recordError("Unexpected token", "Expected expression");
                return null;
            },
        };
    }

    fn parseIdentifierReference(self: *Parser) ?*ast.Expression {
        const name = self.current.lexeme;
        const span = self.current.span;
        self.advance();

        const identifier = ast.IdentifierReference{
            .name = name,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .identifier_reference = identifier }) catch null;
    }

    fn parseStringLiteral(self: *Parser) ?*ast.Expression {
        const value = self.current.lexeme;
        const span = self.current.span;
        self.advance();

        const literal = ast.StringLiteral{
            .value = value, // TODO: handle escape sequences
            .raw = value,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .string_literal = literal }) catch null;
    }

    fn parseBindingPattern(self: *Parser) ?*ast.BindingPattern {
        return switch (self.current.type) {
            .Identifier => self.parseBindingIdentifierPattern(),
            else => {
                self.recordError("Expected binding pattern", "Expected identifier or pattern");
                return null;
            },
        };
    }

    fn parseBindingIdentifierPattern(self: *Parser) ?*ast.BindingPattern {
        if (self.current.type != .Identifier) {
            self.recordError("Expected identifier", "Variable name required");
            return null;
        }

        const name = self.current.lexeme;
        const span = self.current.span;
        self.advance();

        const binding_id = ast.BindingIdentifier{
            .name = name,
            .span = span,
        };

        return self.createNode(ast.BindingPattern, .{ .binding_identifier = binding_id }) catch null;
    }

    inline fn advance(self: *Parser) void {
        self.current = self.peek;
        self.peek = self.lexer.nextToken() catch |err| blk: {
            self.recordError(
                lexer.getLexicalErrorMessage(err),
                lexer.getLexicalErrorHelp(err),
            );
            break :blk token.Token.eof(0);
        };
    }

    inline fn expect(
        self: *Parser,
        token_type: token.TokenType,
        message: []const u8,
        help: ?[]const u8,
    ) bool {
        if (self.current.type == token_type) {
            self.advance();
            return true;
        }
        self.recordError(message, help);
        return false;
    }

    inline fn eatSemi(self: *Parser) void {
        if (self.current.type == .Semicolon) {
            self.advance();
        }
    }

    inline fn recordError(self: *Parser, message: []const u8, help: ?[]const u8) void {
        self.errors.append(self.allocator, Error{
            .message = message,
            .span = self.current.span,
            .help = help,
        }) catch {};
    }

    fn synchronize(self: *Parser) void {
        self.panic_mode = false;

        while (self.current.type != .EOF) {
            if (self.current.type == .Semicolon) {
                self.advance();
                return;
            }

            switch (self.current.type) {
                .Class,
                .Function,
                .Var,
                .For,
                .If,
                .While,
                .Return,
                .Let,
                .Const,
                .Using,
                => return,
                else => {},
            }

            self.advance();
        }
    }

    inline fn createNode(self: *Parser, comptime T: type, value: T) !*T {
        const ptr = try self.allocator.create(T);
        ptr.* = value;
        return ptr;
    }
};
