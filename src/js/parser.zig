const std = @import("std");
const token = @import("token.zig");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");

const expressions = @import("syntax/expressions.zig");
const literals = @import("syntax/literals.zig");
const patterns = @import("syntax/patterns.zig");
const variables = @import("syntax/variables.zig");
const functions = @import("syntax/functions.zig");

pub const Error = struct {
    message: []const u8,
    span: ast.Span,
    help: ?[]const u8 = null,
};

pub const SourceType = enum { Script, Module };
pub const Lang = enum { Js, Ts, Jsx, Tsx, Dts };

pub const Options = struct {
    source_type: SourceType = .Module,
    lang: Lang = .Js,
    is_strict: bool = true,
};

/// Must be deinitialized to free the arena-allocated memory.
pub const ParseTree = struct {
    /// Root node of the AST (always a Program node)
    program: ast.NodeIndex,
    /// Source code that was parsed
    source: []const u8,
    /// All nodes in the AST
    nodes: std.MultiArrayList(ast.Node),
    /// Extra data storage for variadic node children
    extra: std.ArrayList(ast.NodeIndex),
    /// Parse errors encountered
    errors: std.ArrayList(Error),
    /// Arena allocator owning all the memory
    arena: std.heap.ArenaAllocator,

    pub inline fn hasErrors(self: ParseTree) bool {
        return self.errors.items.len > 0;
    }

    pub fn deinit(self: *const ParseTree) void {
        self.arena.deinit();
    }
};

pub const Parser = struct {
    source: []const u8,
    lexer: lexer.Lexer,
    arena: std.heap.ArenaAllocator,
    errors: std.ArrayList(Error) = .empty,
    nodes: std.MultiArrayList(ast.Node) = .empty,
    extra: std.ArrayList(ast.NodeIndex) = .empty,
    current_token: token.Token = undefined,

    scratch_statements: ScratchBuffer = .{},
    scratch_directives: ScratchBuffer = .{},

    // multiple scratches to handle multiple extras at the same time
    scratch_a: ScratchBuffer = .{},
    scratch_b: ScratchBuffer = .{},
    //

    in_async: bool = false,
    in_generator: bool = false,
    in_function: bool = false,

    strict_mode: bool = true,
    source_type: SourceType = .Module,
    lang: Lang = .Js,

    pub fn init(backing_allocator: std.mem.Allocator, source: []const u8, options: Options) Parser {
        return .{
            .source = source,
            .lexer = lexer.Lexer.init(backing_allocator, source),
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
            .source_type = options.source_type,
            .lang = options.lang,
            .strict_mode = options.is_strict,
        };
    }

    pub inline fn allocator(self: *Parser) std.mem.Allocator {
        return self.arena.allocator();
    }

    /// Parse the source code and return a ParseTree.
    /// The Parser is consumed and should not be used after calling this method.
    /// The caller owns the returned ParseTree and must call deinit() on it.
    pub fn parse(self: *Parser) !ParseTree {
        try self.ensureCapacity();

        self.advance();

        const start = self.current_token.span.start;

        const program_data = self.parseBody();

        const end = self.current_token.span.start;

        const program = self.addNode(
            .{
                .program = .{
                    .source_type = if (self.source_type == .Module) .Module else .Script,
                    .body = program_data.statements,
                    .directives = program_data.directives,
                },
            },
            .{ .start = start, .end = end },
        );

        const tree = ParseTree{
            .program = program,
            .source = self.source,
            .nodes = self.nodes,
            .extra = self.extra,
            .errors = self.errors,
            .arena = self.arena,
        };

        self.lexer.deinit();

        return tree;
    }

    pub fn parseBody(self: *Parser) struct { statements: ast.IndexRange, directives: ast.IndexRange } {
        const statements_checkpoint = self.scratch_statements.begin();
        const directives_checkpoint = self.scratch_directives.begin();

        // directive
        // can only appear at top
        if (self.current_token.type == .StringLiteral) {
            if (self.parseDirective()) |directive| {
                self.scratch_directives.append(self.allocator(), directive);
            }
        }

        while (self.current_token.type != .EOF) {
            // block end
            if (self.current_token.type == .RightBrace) break;

            if (self.parseStatement()) |statement| {
                self.scratch_statements.append(self.allocator(), statement);
            } else {
                self.synchronize();
            }
        }

        return .{
            .statements = self.addExtra(self.scratch_statements.take(statements_checkpoint)),
            .directives = self.addExtra(self.scratch_directives.take(directives_checkpoint)),
        };
    }

    pub fn parseStatement(self: *Parser) ?ast.NodeIndex {
        return switch (self.current_token.type) {
            .Var, .Const, .Let, .Using => variables.parseVariableDeclaration(self),
            .Function => functions.parseFunction(self, .{}),
            .Async => functions.parseFunction(self, .{ .is_async = true }),
            .Declare => blk: {
                if (!self.isTs()) {
                    break :blk self.parseExpressionStatement();
                }
                break :blk functions.parseFunction(self, .{ .is_declare = true });
            },

            .Await => blk: {
                const await_token = self.current_token;

                // TODO: remove lookahead method, and use a way without lookahead, like when we implement
                // top level awaits
                // leave this as is for now
                if ((self.lookAhead() orelse break :blk null).type == .Using) {
                    break :blk variables.parseVariableDeclaration(self);
                }

                self.err(
                    await_token.span.start,
                    await_token.span.end,
                    // TODO: message is not always right, there is top level awaits
                    // fix it when implement that
                    "'await' is only valid at the start of an 'await using' declaration or inside async functions",
                    "If you intended to declare a disposable resource, use 'await using'. Otherwise, 'await' can only appear inside an async function.",
                );
                break :blk null;
            },

            else => self.parseExpressionStatement(),
        };
    }

    pub fn parseDirective(self: *Parser) ?ast.NodeIndex {
        const current_token = self.current_token;

        const expression = literals.parseStringLiteral(self) orelse return null;

        return self.addNode(.{
            .directive = .{
                .expression = expression,
                // without quotes
                .value_start = current_token.span.start + 1,
                .value_len = @intCast(current_token.lexeme.len - 2),
            },
        }, .{ .start = current_token.span.start, .end = self.eatSemicolon(current_token.span.end) });
    }

    pub fn parseExpressionStatement(self: *Parser) ?ast.NodeIndex {
        const expression = expressions.parseExpression(self, 0) orelse return null;
        const span = self.getSpan(expression);

        return self.addNode(
            .{ .expression_statement = .{ .expression = expression } },
            .{ .start = span.start, .end = self.eatSemicolon(span.end) },
        );
    }

    pub inline fn isTs(self: *Parser) bool {
        return self.lang == .Ts or self.lang == .Tsx or self.lang == .Dts;
    }

    // utils

    pub fn lookAhead(self: *Parser) ?token.Token {
        return self.current_token;
    }

    pub inline fn addNode(self: *Parser, data: ast.NodeData, span: ast.Span) ast.NodeIndex {
        const index: ast.NodeIndex = @intCast(self.nodes.len);
        self.nodes.append(self.allocator(), .{ .data = data, .span = span }) catch unreachable;
        return index;
    }

    pub fn addExtra(self: *Parser, indices: []const ast.NodeIndex) ast.IndexRange {
        const start: u32 = @intCast(self.extra.items.len);
        const len: u32 = @intCast(indices.len);
        self.extra.appendSlice(self.allocator(), indices) catch unreachable;
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

    pub fn advance(self: *Parser) void {
        self.current_token = self.lexer.nextToken() catch |e| blk: {
            if (e == error.OutOfMemory) @panic("Out of memory");

            const lex_err: lexer.LexicalError = @errorCast(e);

            self.errors.append(self.allocator(), .{
                .message = lexer.getLexicalErrorMessage(lex_err),
                .span = .{ .start = self.lexer.token_start, .end = self.lexer.cursor },
                .help = lexer.getLexicalErrorHelp(lex_err),
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
        self.errors.append(self.allocator(), .{
            .message = message,
            .span = .{ .start = start, .end = end },
            .help = help,
        }) catch unreachable;
    }

    pub fn formatMessage(self: *Parser, comptime format: []const u8, args: anytype) []u8 {
        return std.fmt.allocPrint(self.allocator(), format, args) catch unreachable;
    }

    // this is very basic now
    fn synchronize(self: *Parser) void {
        self.advance();
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

    fn ensureCapacity(self: *Parser) !void {
        if (self.nodes.capacity > 0) return;

        const alloc = self.allocator();
        const estimated_nodes = @max(1024, (self.source.len * 3) / 4);
        const estimated_extra = estimated_nodes / 2;

        try self.nodes.ensureTotalCapacity(alloc, estimated_nodes);
        try self.extra.ensureTotalCapacity(alloc, estimated_extra);
        try self.errors.ensureTotalCapacity(alloc, 32);
        try self.scratch_statements.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_directives.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_a.items.ensureTotalCapacity(alloc, 128);
        try self.scratch_b.items.ensureTotalCapacity(alloc, 128);
    }
};

const ScratchBuffer = struct {
    items: std.ArrayList(ast.NodeIndex) = .empty,

    pub inline fn begin(self: *ScratchBuffer) usize {
        return self.items.items.len;
    }

    pub inline fn append(self: *ScratchBuffer, alloc: std.mem.Allocator, index: ast.NodeIndex) void {
        self.items.append(alloc, index) catch unreachable;
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
