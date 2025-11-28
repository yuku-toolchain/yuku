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
    parser: *Parser,
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
    errors: std.ArrayList(Error),
    nodes: std.MultiArrayList(ast.Node),
    extra: std.ArrayList(ast.NodeIndex),
    current_token: token.Token = undefined,

    // multiple scratches to handle multiple extras at the same time
    scratch_a: ScratchBuffer,
    scratch_b: ScratchBuffer,

    in_async: bool = false,
    in_generator: bool = false,
    in_function: bool = false,

    strict_mode: bool = true,
    source_type: SourceType = .Module,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Parser {
        const estimated_nodes = @max(1024, (source.len * 3) / 4);
        const estimated_extra = estimated_nodes / 2;

        var nodes: std.MultiArrayList(ast.Node) = .empty;
        try nodes.ensureTotalCapacity(allocator, estimated_nodes);

        const extra = try std.ArrayList(ast.NodeIndex).initCapacity(allocator, estimated_extra);
        const errors = try std.ArrayList(Error).initCapacity(allocator, 32);

        return .{
            .source = source,
            .lexer = try lexer.Lexer.init(allocator, source),
            .allocator = allocator,
            .errors = errors,
            .nodes = nodes,
            .extra = extra,
            .scratch_a = ScratchBuffer.init(allocator),
            .scratch_b = ScratchBuffer.init(allocator),
        };
    }

    pub fn parse(self: *Parser) !ParseResult {
        self.advance();

        const start = self.current_token.span.start;
        const checkpoint = self.scratch_a.begin();

        while (self.current_token.type != .EOF) {
            if (self.parseStatement()) |statement| {
                self.scratch_a.append(statement);
            } else {
                self.synchronize();
            }
        }

        const body = self.addExtra(self.scratch_a.take(checkpoint));

        const program = self.addNode(
            .{
                .program = .{
                    .source_type = if (self.source_type == .Module) .Module else .Script,
                    .body = body,
                    .directives = .empty, // TODO: parse directives
                },
            },
            .{ .start = start, .end = self.current_token.span.start },
        );

        return .{
            .program = program,
            .parser = self,
            .source = self.source,
            .errors = try self.errors.toOwnedSlice(self.allocator),
        };
    }

    pub fn parseStatement(self: *Parser) ?ast.NodeIndex {
        return switch (self.current_token.type) {
            .Var, .Const, .Let, .Using => variables.parseVariableDeclaration(self),

            // handle 'await using' declarations
            .Await => blk: {
                const await_token = self.current_token;

                if ((self.lookAhead() orelse break :blk null).type == .Using) {
                    break :blk variables.parseVariableDeclaration(self);
                }

                self.err(
                    await_token.span.start,
                    await_token.span.end,
                    "'await' is only valid at the start of an 'await using' declaration or inside async functions",
                    "If you intended to declare a disposable resource, use 'await using'. Otherwise, 'await' can only appear inside an async function.",
                );
                break :blk null;
            },

            else => self.parseExpressionStatement(),
        };
    }

    pub fn parseExpressionStatement(self: *Parser) ?ast.NodeIndex {
        const expression = expressions.parseExpression(self, 0) orelse return null;
        const span = self.getSpan(expression);

        return self.addNode(
            .{ .expression_statement = .{ .expression = expression } },
            .{ .start = span.start, .end = self.eatSemicolon(span.end) },
        );
    }

    pub fn lookAhead(self: *Parser) ?token.Token {
        return self.current_token;
    }

    pub inline fn addNode(self: *Parser, data: ast.NodeData, span: ast.Span) ast.NodeIndex {
        const index: ast.NodeIndex = @intCast(self.nodes.len);
        self.nodes.append(self.allocator, .{ .data = data, .span = span }) catch unreachable;
        return index;
    }

    pub inline fn addExtra(self: *Parser, indices: []const ast.NodeIndex) ast.IndexRange {
        const start: u32 = @intCast(self.extra.items.len);
        const len: u32 = @intCast(indices.len);
        self.extra.appendSlice(self.allocator, indices) catch unreachable;
        return .{ .start = start, .len = len };
    }

    pub inline fn getSpan(self: *const Parser, index: ast.NodeIndex) ast.Span {
        return self.nodes.items(.span)[index];
    }

    pub inline fn getData(self: *const Parser, index: ast.NodeIndex) ast.NodeData {
        return self.nodes.items(.data)[index];
    }

    pub inline fn getExtra(self: *const Parser, range: ast.IndexRange) []const ast.NodeIndex {
        return self.extra.items[range.start..][0..range.len];
    }

    pub inline fn advance(self: *Parser) void {
        self.current_token = self.lexer.nextToken() catch |e| blk: {
            // handle lexical errors by recording and continuing with EOF
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
        // consume optional semicolon and adjust span
        if (self.current_token.type == .Semicolon) {
            self.advance();
            return end + 1;
        }

        return end;
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

const ScratchBuffer = struct {
    items: std.ArrayList(ast.NodeIndex),
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) ScratchBuffer {
        const list = std.ArrayList(ast.NodeIndex).initCapacity(allocator, 512) catch unreachable;
        return .{ .items = list, .allocator = allocator };
    }

    pub inline fn begin(self: *ScratchBuffer) usize {
        return self.items.items.len;
    }

    pub inline fn append(self: *ScratchBuffer, index: ast.NodeIndex) void {
        self.items.append(self.allocator, index) catch unreachable;
    }

    pub inline fn take(self: *ScratchBuffer, checkpoint: usize) []const ast.NodeIndex {
        const slice = self.items.items[checkpoint..];
        self.items.shrinkRetainingCapacity(checkpoint);
        return slice;
    }

    pub inline fn reset(self: *ScratchBuffer, checkpoint: usize) void {
        self.items.shrinkRetainingCapacity(checkpoint);
    }
};
