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
    allocator: std.mem.Allocator,
    errors: std.ArrayList(Error),

    lookahead: [4]token.Token,
    lookahead_start: u3,
    lookahead_count: u3, // how many tokens buffered

    scratch_body: std.ArrayList(*ast.Body),
    scratch_declarators: std.ArrayList(*ast.VariableDeclarator),
    scratch_expressions: std.ArrayList(*ast.Expression),

    const estimated_nodes_per_line = 2;
    const avg_chars_per_line = 40;
    const initial_error_capacity = 8;

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Parser {
        var lex = try lexer.Lexer.init(allocator, source);

        var lookahead_buf: [4]token.Token = undefined;
        lookahead_buf[0] = lex.nextToken() catch token.Token.eof(0);

        return .{ .source = source, .lexer = lex, .allocator = allocator, .lookahead = lookahead_buf,
                    .lookahead_start = 0,
                    .lookahead_count = 1, .errors = .empty, .scratch_body = .empty, .scratch_declarators = .empty, .scratch_expressions = .empty };
    }

    pub fn parse(self: *Parser) !ParseResult {
        const start = self.current().span.start;

        const estimated_lines = self.source.len / avg_chars_per_line;
        const estimated_statements = @max(estimated_lines / 2, 16);
        const estimated_errors = @min(@max(estimated_lines / 50, 2), initial_error_capacity);

        self.clearRetainingCapacity(&self.scratch_body);
        self.ensureCapacity(&self.scratch_body, estimated_statements);
        self.ensureCapacity(&self.errors, estimated_errors);

        while (self.current().type != .EOF) {
            const stmt = self.parseStatement() orelse {
                self.synchronize();
                continue;
            };

            const body_item = self.createNode(ast.Body, .{ .statement = stmt });
            self.appendItem(&self.scratch_body, body_item);
        }

        const end = self.current().span.end;

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
        return switch (self.current().type) {
            .Var, .Const, .Let, .Using => self.parseVariableDeclaration(),
            .Await => {
                const next = self.peek(1) orelse return null;

                if (next.type == .Using) {
                    return self.parseVariableDeclaration();
                }

                self.recordError("Expected 'using' after 'await'", "Try adding 'using' here to complete the 'await using' declaration");
                return null;
            },
            else => {
                self.recordError("Unexpected token in statement position", "Try starting a statement here with 'var', 'let', 'const', 'if', 'for', 'while', or a function/class declaration");
                return null;
            },
        };
    }

    fn parseVariableDeclaration(self: *Parser) ?*ast.Statement {
        const start = self.current().span.start;
        const kind = self.parseVariableDeclarationKind() orelse return null;

        self.clearRetainingCapacity(&self.scratch_declarators);
        self.ensureCapacity(&self.scratch_declarators, 4);

        // first declarator
        const first_decl = self.parseVariableDeclarator(kind) orelse return null;
        self.appendItem(&self.scratch_declarators, first_decl);

        // additional declarators
        while (self.current().type == .Comma) {
            self.advance();
            const decl = self.parseVariableDeclarator(kind) orelse return null;
            self.appendItem(&self.scratch_declarators, decl);
        }

        self.eatSemi();

        const end = self.current().span.end;

        const declarations = self.dupeSlice(*ast.VariableDeclarator, self.scratch_declarators.items);

        const var_decl = ast.VariableDeclaration{
            .kind = kind,
            .declarations = declarations,
            .span = .{ .start = start, .end = end },
        };

        return self.createNode(ast.Statement, .{ .variable_declaration = var_decl });
    }

    inline fn parseVariableDeclarationKind(self: *Parser) ?ast.VariableDeclaration.VariableDeclarationKind {
        return switch (self.current().type) {
            .Await => blk: {
                self.advance();
                if (self.current().type != .Using) {
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
                self.recordError("Expected variable declaration keyword", "Try using 'var', 'let', 'const', or 'using' here to start a variable declaration");
                return null;
            },
        };
    }

    fn parseVariableDeclarator(
        self: *Parser,
        kind: ast.VariableDeclaration.VariableDeclarationKind,
    ) ?*ast.VariableDeclarator {
        const start = self.current().span.start;
        const id = self.parseBindingPattern() orelse return null;

        const requires_init = kind == .@"const" or
            kind == .using or
            kind == .@"await using";

        var init_expr: ?*ast.Expression = null;

        if (self.current().type == .Assign) {
            self.advance();
            init_expr = self.parseExpression();
        } else if (requires_init) {
            self.recordError(
                "Variable declaration missing required initializer",
                "Try adding '= value' here to initialize this variable",
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
        return switch (self.current().type) {
            .Identifier => self.parseIdentifierReference(),
            .StringLiteral => self.parseStringLiteral(),
            .True, .False => self.parseBooleanLiteral(),
            .NullLiteral => self.parseNullLiteral(),
            .NumericLiteral, .HexLiteral, .OctalLiteral, .BinaryLiteral => self.parseNumericLiteral(),
            .BigIntLiteral => self.parseBigIntLiteral(),
            .Slash => self.parseRegExpLiteral(),
            else => {
                self.recordError("Unexpected token in expression position", "Try using a valid expression here such as a variable name, literal value, operator, etc.");
                return null;
            },
        };
    }

    fn parseIdentifierReference(self: *Parser) ?*ast.Expression {
        const name = self.current().lexeme;
        const span = self.current().span;
        self.advance();

        const identifier = ast.IdentifierReference{
            .name = name,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .identifier_reference = identifier });
    }

    fn parseStringLiteral(self: *Parser) ?*ast.Expression {
        const value = self.current().lexeme;
        const span = self.current().span;
        self.advance();

        const literal = ast.StringLiteral{
            .value = value, // TODO: handle escape sequences and remove quotes
            .raw = value,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .string_literal = literal });
    }

    fn parseBooleanLiteral(self: *Parser) ?*ast.Expression {
        const value = self.current().type == .True;
        const raw = self.current().lexeme;
        const span = self.current().span;
        self.advance();

        const literal = ast.BooleanLiteral{
            .value = value,
            .raw = raw,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .boolean_literal = literal });
    }

    fn parseNullLiteral(self: *Parser) ?*ast.Expression {
        const raw = self.current().lexeme;
        const span = self.current().span;
        self.advance();

        const literal = ast.NullLiteral{
            .value = null,
            .raw = raw,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .null_literal = literal });
    }

    fn parseNumericLiteral(self: *Parser) ?*ast.Expression {
        const value = self.current().lexeme;
        const span = self.current().span;
        self.advance();

        const literal = ast.NumericLiteral{
            .value = std.fmt.parseFloat(f64, value) catch unreachable,
            .raw = value,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .numeric_literal = literal });
    }

    fn parseBigIntLiteral(self: *Parser) ?*ast.Expression {
        const raw = self.current().lexeme;
        const span = self.current().span;
        self.advance();

        const bigint = raw[0..(raw.len - 1)];

        const literal = ast.BigIntLiteral{
            .value = raw,
            .raw = raw,
            .bigint = bigint,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .bigint_literal = literal });
    }

    fn parseRegExpLiteral(self: *Parser) ?*ast.Expression {
        // we are handling regex as a special case, lexer won't scan regex as a standalone regex token
        // parser decides when to scan regex, in this case, we are scanning it in the parseExpression
        // when a slash is encountered, because an expression can be a regex.
        // so in this parseRegExpLiteral, we need to handle creating the regex token and advancing it manually
        // unlike other tokens or nodes.
        const regex = self.lexer.reScanAsRegex(self.current()) catch |err| {
            self.recordError(
                lexer.getLexicalErrorMessage(err),
                lexer.getLexicalErrorHelp(err),
            );

            return null;
        };

        // TODO: add a method to add a token to lexer manually
        // const start = regex.span.start;
        // const end = regex.span.end;

        // const regex_token = self.lexer.createToken(.RegexLiteral, self.source[start..end], start, end);

        // self.position += 1;
        // self.tokens.append(self.allocator, regex_token) catch {};

        self.advance(); // consume the regex

        const literal = ast.RegExpLiteral{
            .value = regex.lexeme,
            .raw = regex.lexeme,
            .regex = .{
                .pattern = regex.pattern,
                .flags = regex.flags,
            },
            .span = regex.span,
        };

        return self.createNode(ast.Expression, .{ .regex_literal = literal });
    }

    fn parseBindingPattern(self: *Parser) ?*ast.BindingPattern {
        return switch (self.current().type) {
            .Identifier => self.parseBindingIdentifierPattern(),
            else => {
                self.recordError("Expected binding pattern", "Try using an identifier name here");
                return null;
            },
        };
    }

    fn parseBindingIdentifierPattern(self: *Parser) ?*ast.BindingPattern {
        if (self.current().type != .Identifier) {
            self.recordError("Expected identifier for variable name", "Try using a valid identifier here (letters, digits, _, or $ - must start with letter, _ or $)");
            return null;
        }

        const name = self.current().lexeme;
        const span = self.current().span;
        self.advance();

        const binding_id = ast.BindingIdentifier{
            .name = name,
            .span = span,
        };

        return self.createNode(ast.BindingPattern, .{ .binding_identifier = binding_id });
    }

    inline fn current(self: *Parser) token.Token {
        return self.lookahead[self.lookahead_start];
    }

    fn peek(self: *Parser, n: u3) ?token.Token {
        while (self.lookahead_count <= n) {
            const tok = self.lexer.nextToken() catch return null;
            const write_pos = (self.lookahead_start + self.lookahead_count) & 3;
            self.lookahead[write_pos] = tok;
            self.lookahead_count += 1;
            if (self.lookahead_count > 4) return null; // buffer full
        }

        const read_pos = (self.lookahead_start + n) & 3;
        return self.lookahead[read_pos];
    }

    inline fn advance(self: *Parser) void {
        if (self.lookahead_count > 1) {
            self.lookahead_start = (self.lookahead_start + 1) & 3;
            self.lookahead_count -= 1;
        } else {
            // refill
            const tok = self.lexer.nextToken() catch token.Token.eof(0);
            self.lookahead[self.lookahead_start] = tok;
        }
    }

    inline fn expect(
        self: *Parser,
        token_type: token.TokenType,
        message: []const u8,
        help: ?[]const u8,
    ) bool {
        if (self.current().type == token_type) {
            self.advance();
            return true;
        }
        self.recordError(message, help);
        return false;
    }

    inline fn eatSemi(self: *Parser) void {
        if (self.current().type == .Semicolon) {
            self.advance();
        }
    }

    inline fn recordError(self: *Parser, message: []const u8, help: ?[]const u8) void {
        self.appendItem(&self.errors, Error{
            .message = message,
            .span = .{ .start = self.lexer.token_start, .end = self.lexer.cursor },
            .help = help,
        });
    }

    // TODO: this is much simple now, this can improve later
    fn synchronize(self: *Parser) void {
        while (self.current().type != .EOF) {
            if (self.current().type == .Semicolon) {
                self.advance();
                return;
            }

            switch (self.current().type) {
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
