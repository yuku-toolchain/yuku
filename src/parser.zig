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
    arena: *std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    errors: std.ArrayList(Error),

    scratch_body: std.ArrayList(*ast.Body),
    scratch_declarators: std.ArrayList(*ast.VariableDeclarator),
    scratch_expressions: std.ArrayList(*ast.Expression),

    panic_mode: bool = false,

    const estimated_nodes_per_line = 2;
    const avg_chars_per_line = 40;
    const initial_error_capacity = 8;

    pub fn init(child_allocator: std.mem.Allocator, source: []const u8) !Parser {
        var arena = std.heap.ArenaAllocator.init(child_allocator);

        const allocator = arena.allocator();

        var lex = try lexer.Lexer.init(allocator, source);

        const current = lex.nextToken() catch token.Token.eof(0);
        const peek = lex.nextToken() catch token.Token.eof(0);

        return .{
            .source = source,
            .lexer = lex,
            .current = current,
            .peek = peek,
            .arena = &arena,
            .allocator = allocator,
            .errors = .empty,

            .scratch_body = std.ArrayList(*ast.Body).empty,
            .scratch_declarators = std.ArrayList(*ast.VariableDeclarator).empty,
            .scratch_expressions = std.ArrayList(*ast.Expression).empty,

            .panic_mode = false,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.arena.deinit();
    }

    pub fn parse(self: *Parser) !ParseResult {
        const start = self.current.span.start;

        const estimated_lines = self.source.len / avg_chars_per_line;
        const estimated_statements = @max(estimated_lines / 2, 16);
        const estimated_errors = @min(@max(estimated_lines / 50, 2), initial_error_capacity);

        self.clearRetainingCapacity(&self.scratch_body);
        self.ensureCapacity(&self.scratch_body, estimated_statements);
        self.ensureCapacity(&self.errors, estimated_errors);

        while (self.current.type != .EOF) {
            const stmt = self.parseStatement() orelse {
                if (!self.panic_mode) self.panic_mode = true;
                self.synchronize();
                continue;
            };

            const body_item = self.createNode(ast.Body, .{ .statement = stmt });
            self.appendAssumeCapacity(&self.scratch_body, body_item);
            self.panic_mode = false;
        }

        const end = self.current.span.end;

        const body = self.dupeSlice(*ast.Body, self.scratch_body.items);

        const program = ast.Program{
            .body = body,
            .span = .{ .start = start, .end = end },
        };

        return ParseResult{
            .program = program,
            .errors = self.toOwnedSlice(&self.errors),
        };
    }

    fn parseStatement(self: *Parser) ?*ast.Statement {
        return switch (self.current.type) {
            .Var, .Const, .Let, .Using => self.parseVariableDeclaration(),
            .Await => {
                if (self.peek.type == .Using) {
                    return self.parseVariableDeclaration();
                }

                return null;
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

        self.clearRetainingCapacity(&self.scratch_declarators);
        self.ensureCapacity(&self.scratch_declarators, 4);

        // first declarator
        const first_decl = self.parseVariableDeclarator(kind) orelse return null;
        self.appendAssumeCapacity(&self.scratch_declarators, first_decl);

        // additional declarators
        while (self.current.type == .Comma) {
            self.advance();
            const decl = self.parseVariableDeclarator(kind) orelse return null;
            self.appendItem(&self.scratch_declarators, decl);
        }

        self.eatSemi();

        const end = self.current.span.end;

        const declarations = self.dupeSlice(*ast.VariableDeclarator, self.scratch_declarators.items);

        const var_decl = ast.VariableDeclaration{
            .kind = kind,
            .declarations = declarations,
            .span = .{ .start = start, .end = end },
        };

        return self.createNode(ast.Statement, .{ .variable_declaration = var_decl });
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
        });
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

        return self.createNode(ast.Expression, .{ .identifier_reference = identifier });
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

        return self.createNode(ast.Expression, .{ .string_literal = literal });
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

        return self.createNode(ast.BindingPattern, .{ .binding_identifier = binding_id });
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
        self.appendItem(&self.errors, Error{
            .message = message,
            .span = self.current.span,
            .help = help,
        });
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

    inline fn createNode(self: *Parser, comptime T: type, value: T) *T {
        const ptr = self.allocator.create(T) catch unreachable;
        ptr.* = value;
        return ptr;
    }

    inline fn ensureCapacity(self: *Parser, list: anytype, capacity: usize) void {
        list.ensureTotalCapacity(self.allocator, capacity) catch unreachable; // it's oom, we failed
    }

    inline fn appendItem(self: *Parser, list: anytype, item: anytype) void {
        list.append(self.allocator, item) catch unreachable; // it's oom, we failed
    }

    inline fn appendAssumeCapacity(self: *Parser, list: anytype, item: anytype) void {
        _ = self;
        list.appendAssumeCapacity(item);
    }

    inline fn clearRetainingCapacity(self: *Parser, list: anytype) void {
        _ = self;
        list.clearRetainingCapacity();
    }

    inline fn dupeSlice(self: *Parser, comptime T: type, items: []const T) []T {
        return self.allocator.dupe(T, items) catch unreachable; // it's oom, we failed
    }

    inline fn toOwnedSlice(self: *Parser, list: anytype) @TypeOf(list.*.toOwnedSlice(self.allocator) catch unreachable) {
        return list.toOwnedSlice(self.allocator) catch unreachable; // it's oom, we failed
    }
};
