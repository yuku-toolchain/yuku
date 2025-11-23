const std = @import("std");
const token = @import("token.zig");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");

const expressions = @import("syntax/expressions.zig");
const literals = @import("syntax/literals.zig");
const patterns = @import("syntax/patterns.zig");
const variables = @import("syntax/variables.zig");

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

pub const SourceType = enum {
    Script,
    Module,
};

pub const Parser = struct {
    source: []const u8,
    lexer: lexer.Lexer,
    allocator: std.mem.Allocator,
    errors: std.ArrayList(Error),

    current_token: token.Token,

    scratch_declarators: std.ArrayList(*ast.VariableDeclarator),
    scratch_expressions: std.ArrayList(*ast.Expression),
    scratch_template_elements: std.ArrayList(*ast.TemplateElement),
    scratch_array_pattern_elements: std.ArrayList(?*ast.ArrayPatternElement),
    scratch_object_pattern_properties: std.ArrayList(*ast.ObjectPatternProperty),

    in_async: bool = false,
    in_generator: bool = false,
    in_function: bool = false,

    // TODO: add logic to detect it, likely in directive parsing
    strict_mode: bool = true,
    source_type: SourceType = .Module,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Parser {
        const lex = try lexer.Lexer.init(allocator, source);

        return .{
            .source = source,
            .lexer = lex,
            .allocator = allocator,

            .current_token = undefined,

            .errors = std.ArrayList(Error).empty,

            .scratch_declarators = .empty,
            .scratch_expressions = .empty,
            .scratch_template_elements = .empty,
            .scratch_array_pattern_elements = .empty,
            .scratch_object_pattern_properties = .empty,
        };
    }

    pub fn parse(self: *Parser) !ParseResult {
        // let's gooo
        self.advance();

        const start = self.current_token.span.start;

        var body_list = std.ArrayList(*ast.Body).empty;

        while (self.current_token.type != .EOF) {
            const stmt = self.parseStatement() orelse {
                self.synchronize();
                continue;
            };

            const body_item = self.createNode(ast.Body, .{ .statement = stmt });
            self.append(&body_list, body_item);
        }

        const end = self.current_token.span.start;

        const program = ast.Program{
            .body = self.dupe(*ast.Body, body_list.items),
            .span = .{ .start = start, .end = end },
        };

        return ParseResult{
            .program = program,
            .errors = self.toOwnedSlice(&self.errors),
        };
    }

    pub fn parseStatement(self: *Parser) ?*ast.Statement {
        return switch (self.current_token.type) {
            .Var, .Const, .Let, .Using => variables.parseVariableDeclaration(self),
            .Await => {
                const await_token = self.current_token;
                const next = self.lookAhead() orelse return null;

                if (next.type == .Using) {
                    return variables.parseVariableDeclaration(self);
                }

                const span_start = await_token.span.start;
                const span_end = await_token.span.end;
                self.err(
                    span_start,
                    span_end,
                    "Expected 'using' after 'await'",
                    "Add 'using' after 'await' to create an 'await using' declaration",
                );
                return null;
            },
            else => self.parseExpressionStatement(),
        };
    }

    pub fn parseExpressionStatement(self: *Parser) ?*ast.Statement {
        const expr = expressions.parseExpression(self, 0) orelse return null;
        const end = self.eatSemi(expr.getSpan().end);

        const expr_stmt = ast.ExpressionStatement{
            .expression = expr,
            .span = .{ .start = expr.getSpan().start, .end = end },
        };

        return self.createNode(ast.Statement, .{ .expression_statement = expr_stmt });
    }

    pub inline fn ensureValidIdentifier(self: *Parser, tok: token.Token, comptime as_what: []const u8, help: []const u8) bool {
        if (self.strict_mode and (tok.type == .Identifier)) {
            if (std.mem.eql(u8, tok.lexeme, "eval") or std.mem.eql(u8, tok.lexeme, "arguments")) {
                self.err(tok.span.start, tok.span.end, self.formatMessage("'{s}' cannot be used {s} in strict mode", .{ tok.lexeme, as_what }), help);
                return false;
            }
        }

        if (self.strict_mode and tok.type.isStrictModeReserved()) {
            self.err(tok.span.start, tok.span.end, self.formatMessage("'{s}' is reserved in strict mode and cannot be used {s}", .{ tok.lexeme, as_what }), help);
            return false;
        }

        if (tok.type == .Await and (self.in_async or self.source_type == .Module)) {
            self.err(tok.span.start, tok.span.end, self.formatMessage("'await' is reserved {s} and cannot be used {s}", .{ if (self.in_async) "in async functions" else "at the top level of modules", as_what }), help);
            return false;
        }

        if (tok.type == .Yield and (self.in_generator or self.source_type == .Module)) {
            self.err(tok.span.start, tok.span.end, self.formatMessage("'yield' is reserved {s} and cannot be used {s}", .{ if (self.in_generator) "in generator functions" else "at the top level of modules", as_what }), help);
            return false;
        }

        if (tok.type.isStrictReserved()) {
            self.err(tok.span.start, tok.span.end, self.formatMessage("'{s}' is a reserved word and cannot be used {s}", .{ tok.lexeme, as_what }), help);
            return false;
        }

        return true;
    }

    pub fn formatMessage(self: *Parser, comptime fmt: []const u8, args: anytype) []u8 {
        return std.fmt.allocPrint(self.allocator, fmt, args) catch unreachable;
    }

    pub fn lookAhead(self: *Parser) ?token.Token {
        // TODO: add lookahead support, rewind etc.
        return self.current_token;
    }

    pub inline fn advance(self: *Parser) void {
        self.current_token = self.lexer.nextToken() catch |err_| blk: {
            self.append(&self.errors, Error{
                .message = lexer.getLexicalErrorMessage(err_),
                .span = .{ .start = self.lexer.token_start, .end = self.lexer.cursor },
                .help = lexer.getLexicalErrorHelp(err_),
            });
            break :blk token.Token.eof(0);
        };
    }

    pub inline fn replaceTokenAndAdvance(self: *Parser, tok: token.Token) void {
        self.current_token = tok;
        self.advance();
    }

    pub inline fn expect(
        self: *Parser,
        token_type: token.TokenType,
        message: []const u8,
        help: ?[]const u8,
    ) bool {
        if (self.current_token.type == token_type) {
            self.advance();
            return true;
        }

        const tok = self.current_token;
        self.err(tok.span.start, tok.span.end, message, help);
        return false;
    }

    pub inline fn eatSemi(self: *Parser, current_end: u32) u32 {
        if (self.current_token.type == .Semicolon) {
            self.advance();
            return current_end + 1;
        }

        return current_end;
    }

    pub inline fn err(
        self: *Parser,
        start: u32,
        end: u32,
        message: []const u8,
        help: ?[]const u8,
    ) void {
        self.append(&self.errors, Error{
            .message = message,
            .span = .{ .start = start, .end = end },
            .help = help,
        });
    }

    // TODO(arshad): this is too basic now, make it much better later
    fn synchronize(self: *Parser) void {
        self.advance();

        while (self.current_token.type != .EOF) {
            if (self.current_token.type == .Semicolon) {
                self.advance();
                return;
            }

            switch (self.current_token.type) {
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

    pub inline fn createNode(self: *Parser, comptime T: type, value: T) *T {
        const ptr = self.allocator.create(T) catch unreachable;
        ptr.* = value;
        return ptr;
    }

    pub inline fn ensureCapacity(self: *Parser, list: anytype, capacity: u32) void {
        list.ensureUnusedCapacity(self.allocator, capacity) catch unreachable;
    }

    pub inline fn append(self: *Parser, list: anytype, item: anytype) void {
        list.append(self.allocator, item) catch unreachable;
    }

    pub inline fn clear(self: *Parser, list: anytype) void {
        _ = self;
        list.items.len = 0;
    }

    pub inline fn dupe(self: *Parser, comptime T: type, items: []const T) []T {
        return self.allocator.dupe(T, items) catch unreachable;
    }

    pub inline fn toOwnedSlice(self: *Parser, list: anytype) @TypeOf(list.*.toOwnedSlice(self.allocator) catch unreachable) {
        return list.toOwnedSlice(self.allocator) catch unreachable;
    }
};
