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
    span: ast.Span,
    help: ?[]const u8 = null,
};

pub const ParseResult = struct {
    program: ast.NodeIndex,
    nodes: *ast.NodeList,
    source: []const u8,
    errors: []Error,

    pub inline fn hasErrors(self: ParseResult) bool {
        return self.errors.len > 0;
    }
};

pub const SourceType = enum { Script, Module };

pub const Parser = struct {
    source: []const u8,
    lexer: lexer.Lexer,
    allocator: std.mem.Allocator,
    errors: std.ArrayList(Error) = .empty,
    nodes: ast.NodeList,
    current_token: token.Token = undefined,

    in_async: bool = false,
    in_generator: bool = false,
    in_function: bool = false,

    strict_mode: bool = true,
    source_type: SourceType = .Module,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Parser {
        return .{
            .source = source,
            .lexer = try lexer.Lexer.init(allocator, source),
            .allocator = allocator,
            .nodes = ast.NodeList.init(allocator, @intCast(source.len)),
        };
    }

    pub fn parse(self: *Parser) !ParseResult {
        self.advance();

        const start = self.current_token.span.start;
        var statements: std.ArrayList(ast.NodeIndex) = .empty;

        while (self.current_token.type != .EOF) {
            if (self.parseStatement()) |statement| {
                statements.append(self.allocator, statement) catch unreachable;
            } else {
                self.synchronize();
            }
        }

        const body = self.nodes.addExtra(self.allocator, statements.items);

        const program = self.addNode(.{
            .program = .{
                .body = body,
                .source_type = if (self.source_type == .Module) .Module else .Script,
            },
        }, .{ .start = start, .end = self.current_token.span.start });

        return .{
            .program = program,
            .nodes = &self.nodes,
            .source = self.source,
            .errors = self.errors.toOwnedSlice(self.allocator) catch unreachable,
        };
    }

    pub fn parseStatement(self: *Parser) ?ast.NodeIndex {
        return switch (self.current_token.type) {
            .Var, .Const, .Let, .Using => variables.parseVariableDeclaration(self),
            .Await => blk: {
                const await_token = self.current_token;
                if ((self.lookAhead() orelse break :blk null).type == .Using) {
                    break :blk variables.parseVariableDeclaration(self);
                }
                self.err(await_token.span.start, await_token.span.end, "Expected 'using' after 'await'", null);
                break :blk null;
            },
            else => self.parseExpressionStatement(),
        };
    }

    pub fn parseExpressionStatement(self: *Parser) ?ast.NodeIndex {
        const expression = expressions.parseExpression(self, 0) orelse return null;
        const span = self.nodes.getSpan(expression);
        return self.addNode(.{ .expression_statement = .{ .expression = expression } }, .{
            .start = span.start,
            .end = self.eatSemicolon(span.end),
        });
    }

    pub inline fn addNode(self: *Parser, data: ast.NodeData, span: ast.Span) ast.NodeIndex {
        return self.nodes.add(self.allocator, data, span);
    }

    pub inline fn addExtra(self: *Parser, indices: []const ast.NodeIndex) ast.IndexRange {
        return self.nodes.addExtra(self.allocator, indices);
    }

    pub inline fn getSpan(self: *Parser, index: ast.NodeIndex) ast.Span {
        return self.nodes.getSpan(index);
    }

    pub inline fn getData(self: *Parser, index: ast.NodeIndex) ast.NodeData {
        return self.nodes.getData(index);
    }

    pub fn lookAhead(self: *Parser) ?token.Token {
        return self.current_token;
    }

    pub inline fn advance(self: *Parser) void {
        self.current_token = self.lexer.nextToken() catch |e| blk: {
            self.errors.append(self.allocator, .{
                .message = lexer.getLexicalErrorMessage(e),
                .span = .{ .start = self.lexer.token_start, .end = self.lexer.cursor },
                .help = lexer.getLexicalErrorHelp(e),
            }) catch unreachable;
            break :blk token.Token.eof(0);
        };
    }

    pub inline fn replaceTokenAndAdvance(self: *Parser, tok: token.Token) void {
        self.current_token = tok;
        self.advance();
    }

    pub inline fn expect(self: *Parser, token_type: token.TokenType, message: []const u8, help: ?[]const u8) bool {
        if (self.current_token.type == token_type) {
            self.advance();
            return true;
        }
        self.err(self.current_token.span.start, self.current_token.span.end, message, help);
        return false;
    }

    pub inline fn eatSemicolon(self: *Parser, end: u32) u32 {
        if (self.current_token.type == .Semicolon) {
            self.advance();
            return end + 1;
        }
        return end;
    }

    pub inline fn ensureValidIdentifier(
        self: *Parser,
        tok: token.Token,
        comptime as_what: []const u8,
        comptime help: []const u8,
        help_args: anytype,
    ) bool {
        if (self.strict_mode and tok.type == .Identifier) {
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
            self.err(tok.span.start, tok.span.end, self.formatMessage("'{s}' is a reserved word and cannot be used {s}", .{ tok.lexeme, as_what }), self.formatMessage(help, help_args));
            return false;
        }
        return true;
    }

    pub inline fn err(self: *Parser, start: u32, end: u32, message: []const u8, help: ?[]const u8) void {
        self.errors.append(self.allocator, .{
            .message = message,
            .span = .{ .start = start, .end = end },
            .help = help,
        }) catch unreachable;
    }

    pub fn formatMessage(self: *Parser, comptime format: []const u8, args: anytype) []u8 {
        return std.fmt.allocPrint(self.allocator, format, args) catch unreachable;
    }

    fn synchronize(self: *Parser) void {
        self.advance();
        while (self.current_token.type != .EOF) {
            if (self.current_token.type == .Semicolon) {
                self.advance();
                return;
            }
            switch (self.current_token.type) {
                .Class, .Function, .Var, .For, .If, .While, .Return, .Let, .Const, .Using => return,
                else => {},
            }
            self.advance();
        }
    }
};
