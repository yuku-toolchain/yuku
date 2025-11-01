const std = @import("std");
const token = @import("token.zig");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");

const Error = struct {
    message: []const u8,
    span: token.Span,
    help: ?[]const u8 = null,
};

pub const ParseResult = struct {
    program: ?ast.Program,
    errors: []Error,

    pub fn hasErrors(self: ParseResult) bool {
        return self.errors.len > 0;
    }
};

pub const Parser = struct {
    source: []const u8,
    lexer: lexer.Lexer,
    current: token.Token,
    peek: token.Token,
    allocator: std.mem.Allocator,
    errors: std.ArrayList(Error),
    panic_mode: bool = false,

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
            .errors = std.ArrayList(Error).empty,
            .panic_mode = false,
        };
    }

    pub fn parse(self: *Parser) !ParseResult {
        const start = self.current.span.start;
        var body = std.ArrayList(*ast.Body).empty;

        const assumed_capacity = self.source.len / 30;

        try self.errors.ensureTotalCapacity(self.allocator, 3);
        try body.ensureTotalCapacity(self.allocator, assumed_capacity);

        while (self.current.type != .EOF) {
            if (self.parseStatement()) |stmt| {
                const body_item = try self.alloc(ast.Body, .{ .statement = stmt });
                try body.append(self.allocator, body_item);
                self.panic_mode = false;
            } else {
                if (!self.panic_mode) {
                    self.panic_mode = true;
                }
                self.synchronize();
            }
        }

        const program = if (self.errors.items.len == 0) blk: {
            const end = if (body.items.len > 0)
                body.items[body.items.len - 1].getSpan().end
            else
                start;

            break :blk ast.Program{
                .body = try body.toOwnedSlice(self.allocator),
                .span = .{ .start = start, .end = end },
            };
        } else null;

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
                    self.err("Invalid await usage", "Expected 'using' after 'await'");
                    return null;
                }
            },
            else => {
                self.err("Unexpected token", "Expected a statement");
                return null;
            },
        };
    }

    fn parseVariableDeclaration(self: *Parser) ?*ast.Statement {
        const start = self.current.span.start;
        const kind = self.parseVariableDeclarationKind() orelse return null;

        var declarators = std.ArrayList(*ast.VariableDeclarator).empty;

        // parse declarators
        if (self.parseVariableDeclarator(&kind)) |decl| {
            declarators.append(self.allocator, decl) catch return null;
        } else {
            return null;
        }

        while (self.current.type == .Comma) {
            self.advance();
            if (self.parseVariableDeclarator(&kind)) |decl| {
                declarators.append(self.allocator, decl) catch return null;
            } else {
                return null;
            }
        }

        if (!self.consume(.Semicolon, "Expected ';'", "Variable declarations must end with semicolon")) {
            return null;
        }

        const end = self.current.span.end;
        const var_decl = ast.VariableDeclaration{
            .kind = kind,
            .declarations = declarators.toOwnedSlice(self.allocator) catch return null,
            .span = .{ .start = start, .end = end },
        };

        return self.alloc(ast.Statement, .{ .variable_declaration = var_decl }) catch null;
    }

    fn parseVariableDeclarationKind(self: *Parser) ?ast.VariableDeclaration.VariableDeclarationKind {
        return switch (self.current.type) {
            .Await => blk: {
                self.advance();
                if (self.current.type == .Using) {
                    self.advance();
                    break :blk .@"await using";
                } else {
                    self.err("Expected 'using' after 'await'", null);
                    return null;
                }
            },
            .Var => blk: { self.advance(); break :blk .@"var"; },
            .Let => blk: { self.advance(); break :blk .let; },
            .Const => blk: { self.advance(); break :blk .@"const"; },
            .Using => blk: { self.advance(); break :blk .using; },
            else => {
                self.err("Expected variable declaration", "Expected 'var', 'let', 'const', or 'using'");
                return null;
            },
        };
    }

    fn parseVariableDeclarator(self: *Parser, kind: *const ast.VariableDeclaration.VariableDeclarationKind) ?*ast.VariableDeclarator {
        const start = self.current.span.start;

        const id = self.parseBindingPattern() orelse return null;

        var init_: ?*ast.Expression = null;
        if (self.current.type == .Assign) {
            self.advance();
            init_ = self.parseExpression();
        }

        if (init_ == null and (kind.* == .@"const" or kind.* == .using or kind.* == .@"await using")) {
            self.err("Variable declaration missing required initializer", "Add '= value' after the variable name to complete the declaration");
            return null;
        }

        const end = if (init_) |i| i.getSpan().end else id.getSpan().end;

        return self.alloc(ast.VariableDeclarator, .{
            .id = id,
            .init = init_,
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
            // TODO: add more expression types
            else => {
                self.err("Unexpected token", "Expected expression");
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

        return self.alloc(ast.Expression, .{ .identifier_reference = identifier }) catch null;
    }

    fn parseStringLiteral(self: *Parser) ?*ast.Expression {
        const value = self.current.lexeme;
        const span = self.current.span;
        self.advance();

        const literal = ast.StringLiteral{
            .value = value,
            .raw = value, // TODO: handle escape sequences
            .span = span,
        };

        return self.alloc(ast.Expression, .{ .string_literal = literal }) catch null;
    }

    fn parseBindingPattern(self: *Parser) ?*ast.BindingPattern {
        return switch (self.current.type) {
            .Identifier => self.parseBindingIdentifierPattern(),
            // TODO: add object patterns, array patterns
            else => {
                self.err("Expected binding pattern", "Expected identifier or pattern");
                return null;
            },
        };
    }

    fn parseBindingIdentifierPattern(self: *Parser) ?*ast.BindingPattern {
        const name = self.current.lexeme;
        const span = self.current.span;

        if (self.current.type != .Identifier) {
            self.err("Expected identifier", "Variable name required");
            return null;
        }

        self.advance();

        const binding_id = ast.BindingIdentifier{
            .name = name,
            .span = span,
        };

        return self.alloc(ast.BindingPattern, .{ .binding_identifier = binding_id }) catch null;
    }
    inline fn advance(self: *Parser) void {
        self.current = self.peek;
        self.peek = self.lexer.nextToken() catch |error_type| blk: {
            self.err(lexer.getLexicalErrorMessage(error_type), lexer.getLexicalErrorHelp(error_type));
            break :blk token.Token.eof(0);
        };
    }

    inline fn consume(self: *Parser, token_type: token.TokenType, message: []const u8, help: ?[]const u8) bool {
        if (self.current.type == token_type) {
            self.advance();
            return true;
        }

        self.err(message, help);
        return false;
    }

    inline fn err(self: *Parser, message: []const u8, help: ?[]const u8) void {
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
                .Class, .Function, .Var, .For, .If, .While, .Return, .Let, .Const, .Using => return,
                else => {},
            }

            self.advance();
        }
    }

    inline fn alloc(self: *Parser, comptime T: type, value: T) !*T {
        const ptr = try self.allocator.create(T);
        ptr.* = value;
        return ptr;
    }
};
