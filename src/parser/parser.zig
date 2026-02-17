const std = @import("std");
const token = @import("token.zig");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");

const statements = @import("syntax/statements.zig");

pub const Options = struct {
    /// Source type determines how the code is parsed and evaluated.
    /// Defaults to `.module` (ES module semantics, strict mode enabled).
    source_type: ast.SourceType = .module,
    /// Language variant determines which syntax features are enabled.
    /// Defaults to `.js` (plain JavaScript).
    lang: ast.Lang = .js,
};

const ParserContext = struct {
    in_async: bool = false,
    /// When true, `yield` is a keyword (allowed as an expression, disallowed as an identifier).
    yield_is_keyword: bool = false,
    allow_in: bool = true,
    in_function: bool = false,
    /// Whether we're parsing a single statement.
    /// example:
    /// if(test) 30;
    ///          ~~
    ///           ^ this is in a single statement context
    in_single_statement_context: bool = false,
    // https://tc39.es/ecma262/#directive-prologue
    in_directive_prologue: bool = false,
};

const ParserState = struct {
    /// tracks if the cover (array or object) we are parsing has a trailing comma
    /// value is the start index of the cover
    cover_has_trailing_comma: ?u32 = null,
    /// tracks if CoverInitializedName ({a = 1}) was parsed in current cover context.
    cover_has_init_name: bool = false,
};

pub const Error = error{OutOfMemory};

pub const Parser = struct {
    source: []const u8,
    lexer: lexer.Lexer,
    arena: std.heap.ArenaAllocator,
    diagnostics: std.ArrayList(ast.Diagnostic) = .empty,
    nodes: ast.NodeList = .empty,
    extra: std.ArrayList(ast.NodeIndex) = .empty,
    current_token: token.Token,

    scratch_statements: ScratchBuffer = .{},
    scratch_cover: ScratchBuffer = .{},
    scratch_decorators: ScratchBuffer = .{},

    // multiple scratches to handle multiple extras at the same time
    scratch_a: ScratchBuffer = .{},
    scratch_b: ScratchBuffer = .{},
    //

    context: ParserContext = .{},
    state: ParserState = .{},

    source_type: ast.SourceType,
    lang: ast.Lang,

    pub fn init(child_allocator: std.mem.Allocator, source: []const u8, options: Options) Parser {
        return .{
            .source = source,
            .arena = std.heap.ArenaAllocator.init(child_allocator),
            .source_type = options.source_type,
            .lang = options.lang,
            .lexer = undefined,
            .current_token = undefined,
        };
    }

    pub inline fn allocator(self: *Parser) std.mem.Allocator {
        return self.arena.allocator();
    }

    /// Parse the source code and return a ParseTree.
    /// The Parser is consumed and should not be used after calling this method.
    /// The caller owns the returned ParseTree and must call deinit() on it.
    pub fn parse(self: *Parser) Error!ast.ParseTree {
        const alloc = self.allocator();

        self.lexer = try lexer.Lexer.init(self.source, alloc, self.source_type);

        // let's begin
        try self.advance() orelse {
            self.current_token = token.Token.eof(0);
        };

        errdefer self.arena.deinit();

        try self.ensureCapacity();

        const body = try self.parseBody(null);

        const end = self.current_token.span.end;

        const program = try self.addNode(
            .{
                .program = .{
                    .source_type = if (self.source_type == .module) .module else .script,
                    .body = body,
                    .hashbang = self.lexer.hashbang,
                },
            },
            .{ .start = 0, .end = end },
        );

        const tree = ast.ParseTree{
            .program = program,
            .source = self.source,
            .nodes = self.nodes.toOwnedSlice(),
            .extra = try self.extra.toOwnedSlice(alloc),
            .diagnostics = try self.diagnostics.toOwnedSlice(alloc),
            .comments = try self.lexer.comments.toOwnedSlice(alloc),
            .arena = self.arena,
            .source_type = self.source_type,
            .lang = self.lang,
        };

        return tree;
    }

    pub fn parseBody(self: *Parser, terminator: ?token.TokenType) Error!ast.IndexRange {
        // it's a directive prologue if it's a function body or if we are at the program level
        // terminator null means, we are at program level
        self.context.in_directive_prologue = self.context.in_function or terminator == null;
        defer self.context.in_directive_prologue = false;

        const statements_checkpoint = self.scratch_statements.begin();
        defer self.scratch_statements.reset(statements_checkpoint);

        while (!self.isAtBodyEnd(terminator)) {
            if (try statements.parseStatement(self, .{})) |statement| {
                try self.scratch_statements.append(self.allocator(), statement);
            } else {
                try self.synchronize(terminator) orelse break;
            }
        }

        return self.addExtraFromScratch(&self.scratch_statements, statements_checkpoint);
    }

    inline fn isAtBodyEnd(self: *Parser, terminator: ?token.TokenType) bool {
        return self.current_token.type == .eof or
            (terminator != null and self.current_token.type == terminator.?);
    }

    pub inline fn isTs(self: *Parser) bool {
        return self.lang == .ts or self.lang == .tsx or self.lang == .dts;
    }

    pub inline fn isJsx(self: *Parser) bool {
        return self.lang == .tsx or self.lang == .jsx;
    }

    pub inline fn isModule(self: *Parser) bool {
        return self.source_type == .module;
    }

    // utils

    pub inline fn setLexerMode(self: *Parser, mode: lexer.LexerMode) void {
        self.lexer.state.mode = mode;
    }

    pub inline fn addNode(self: *Parser, data: ast.NodeData, span: ast.Span) Error!ast.NodeIndex {
        const index: ast.NodeIndex = @intCast(self.nodes.len);
        if (self.nodes.len < self.nodes.capacity) {
            self.nodes.appendAssumeCapacity(.{ .data = data, .span = span });
        } else {
            try self.nodes.append(self.allocator(), .{ .data = data, .span = span });
        }
        return index;
    }

    pub fn addExtra(self: *Parser, indices: []const ast.NodeIndex) Error!ast.IndexRange {
        const start: u32 = @intCast(self.extra.items.len);
        const len: u32 = @intCast(indices.len);
        if (self.extra.items.len + indices.len <= self.extra.capacity) {
            self.extra.appendSliceAssumeCapacity(indices);
        } else {
            try self.extra.appendSlice(self.allocator(), indices);
        }
        return .{ .start = start, .len = len };
    }

    pub fn addExtraFromScratch(self: *Parser, scratch: *ScratchBuffer, checkpoint: usize) Error!ast.IndexRange {
        const start: u32 = @intCast(self.extra.items.len);
        const slice = scratch.items.items[checkpoint..scratch.items.items.len];
        const len: u32 = @intCast(slice.len);

        if (slice.len > 0) {
            if (self.extra.items.len + slice.len <= self.extra.capacity) {
                self.extra.appendSliceAssumeCapacity(slice);
            } else {
                try self.extra.appendSlice(self.allocator(), slice);
            }
        }

        return .{ .start = start, .len = len };
    }

    pub inline fn getSpan(self: *const Parser, index: ast.NodeIndex) ast.Span {
        return self.nodes.items(.span)[index];
    }

    pub inline fn getData(self: *const Parser, index: ast.NodeIndex) ast.NodeData {
        return self.nodes.items(.data)[index];
    }

    pub inline fn setData(self: *Parser, index: ast.NodeIndex, data: ast.NodeData) void {
        self.nodes.items(.data)[index] = data;
    }

    pub inline fn setSpan(self: *Parser, index: ast.NodeIndex, span: ast.Span) void {
        self.nodes.items(.span)[index] = span;
    }

    pub inline fn getExtra(self: *const Parser, range: ast.IndexRange) []const ast.NodeIndex {
        return self.extra.items[range.start..][0..range.len];
    }

    pub inline fn getSourceText(self: *const Parser, start: u32, len: u16) []const u8 {
        return self.source[start..][0..len];
    }

    pub inline fn getTokenText(self: *const Parser, tok: token.Token) []const u8 {
        return tok.text(self.source);
    }

    inline fn nextToken(self: *Parser) Error!?token.Token {
        return self.lexer.nextToken() catch |e| blk: {
            if (e == error.OutOfMemory) return error.OutOfMemory;

            const lex_err: lexer.LexicalError = @errorCast(e);

            try self.diagnostics.append(self.allocator(), .{
                .message = lexer.getLexicalErrorMessage(lex_err),
                .span = .{ .start = self.current_token.span.end, .end = self.lexer.cursor },
                .help = lexer.getLexicalErrorHelp(lex_err),
            });

            break :blk null;
        };
    }

    pub inline fn advance(self: *Parser) Error!?void {
        self.current_token = try self.nextToken() orelse return null;
    }

    pub fn lookAhead(self: *Parser) Error!?token.Token {
        const prev_state = self.lexer.state;
        const prev_cursor = self.lexer.cursor;
        const prev_comments_len = self.lexer.comments.items.len;

        defer {
            self.lexer.state = prev_state;
            self.lexer.cursor = prev_cursor;
            self.lexer.comments.shrinkRetainingCapacity(prev_comments_len);
        }

        return try self.nextToken();
    }

    /// sets current token from a re-scanned token and advances to the next token.
    /// use after lexer re-scan functions (reScanJsxText, reScanTemplateContinuation, etc.)
    pub inline fn advanceWithRescannedToken(self: *Parser, tok: token.Token) Error!?void {
        self.current_token = tok;
        return self.advance();
    }

    pub fn expect(self: *Parser, token_type: token.TokenType, message: []const u8, help: ?[]const u8) Error!bool {
        if (self.current_token.type == token_type) {
            try self.advance() orelse return false;
            return true;
        }

        try self.reportExpected(self.current_token.span, message, .{ .help = help });

        return false;
    }

    pub fn eatSemicolon(self: *Parser, end: u32) Error!?u32 {
        if (self.current_token.type == .semicolon) {
            const semicolon_end = self.current_token.span.end;
            try self.advance() orelse return null;
            return semicolon_end;
        } else {
            if (!self.canInsertSemicolon(self.current_token)) {
                try self.reportExpected(
                    self.current_token.span,
                    "Expected a semicolon or an implicit semicolon after a statement",
                    .{ .help = "Try inserting a semicolon here" },
                );
                return null;
            }
        }

        return end;
    }

    /// lenient semicolon consumption for statements with special ASI exceptions.
    ///
    /// ES2015+ ASI: "The previous token is ) and the inserted semicolon would
    /// then be parsed as the terminating semicolon of a do-while statement (14.7.2)"
    ///
    /// allows `do {} while (false) foo()`, semicolon optional after the `)`.
    pub fn eatSemicolonLenient(self: *Parser, end: u32) Error!?u32 {
        if (self.current_token.type == .semicolon) {
            const semicolon_end = self.current_token.span.end;
            try self.advance() orelse return null;
            return semicolon_end;
        }
        return end;
    }

    /// https://tc39.es/ecma262/#sec-rules-of-automatic-semicolon-insertion
    pub inline fn canInsertSemicolon(_: *Parser, tok: token.Token) bool {
        return tok.type == .eof or tok.has_line_terminator_before or tok.type == .right_brace;
    }

    pub inline fn describeToken(self: *Parser, tok: token.Token) []const u8 {
        if (tok.type == .eof) return "end of file";
        return tok.type.toString() orelse tok.text(self.source);
    }

    pub const ReportOptions = struct {
        severity: ast.Severity = .@"error",
        help: ?[]const u8 = null,
        labels: []const ast.Label = &.{},
    };

    pub fn report(self: *Parser, span: ast.Span, message: []const u8, opts: ReportOptions) Error!void {
        try self.diagnostics.append(self.allocator(), .{
            .severity = opts.severity,
            .message = message,
            .span = span,
            .help = opts.help,
            .labels = opts.labels,
        });
    }

    pub fn reportExpected(self: *Parser, span: ast.Span, message: []const u8, opts: ReportOptions) Error!void {
        const expected_message = try std.fmt.allocPrint(self.allocator(), "{s}, but found '{s}'", .{
            message,
            self.describeToken(self.current_token),
        });

        try self.report(span, expected_message, opts);
    }

    pub fn reportFmt(self: *Parser, span: ast.Span, comptime format: []const u8, args: anytype, opts: ReportOptions) Error!void {
        const message = try std.fmt.allocPrint(self.allocator(), format, args);
        try self.report(span, message, opts);
    }

    pub fn label(_: *Parser, span: ast.Span, message: []const u8) ast.Label {
        return .{ .span = span, .message = message };
    }

    pub fn makeLabels(self: *Parser, labels: []const ast.Label) Error![]const ast.Label {
        return try self.allocator().dupe(ast.Label, labels);
    }

    pub fn formatMessage(self: *Parser, comptime format: []const u8, args: anytype) Error![]u8 {
        return try std.fmt.allocPrint(self.allocator(), format, args);
    }

    // returning null here means, break the top level statement parsing loop
    // otherwise continue
    // TODO: make it better
    fn synchronize(self: *Parser, terminator: ?token.TokenType) Error!?void {
        while (self.current_token.type != .eof) {
            // stop at the block terminator to avoid consuming the closing brace
            if (terminator) |t| {
                if (self.current_token.type == t) return;
            }

            if (self.current_token.type == .semicolon or self.current_token.type == .right_brace) {
                try self.advance() orelse return null;
                return;
            }

            if (self.current_token.has_line_terminator_before) {
                const can_start_statement = switch (self.current_token.type) {
                    .at, .class, .function, .@"var", .@"for", .@"if", .@"while", .@"return", .let, .@"const", .@"try", .throw, .debugger, .@"break", .@"continue", .@"switch", .do, .with, .async, .@"export", .import, .left_brace => true,
                    else => false,
                };

                if (can_start_statement) {
                    return;
                }
            }

            try self.advance() orelse return null;
        }
    }

    fn ensureCapacity(self: *Parser) Error!void {
        if (self.nodes.capacity > 0) return;

        const alloc = self.allocator();

        const estimated_nodes = if (self.source.len < 10_000)
            @max(512, self.source.len / 2)
        else if (self.source.len < 100_000)
            self.source.len / 5
        else
            self.source.len / 8;

        const estimated_extra = if (self.source.len < 5_000_000)
            estimated_nodes / 4
        else
            estimated_nodes / 3;

        try self.nodes.ensureTotalCapacity(alloc, estimated_nodes);
        try self.extra.ensureTotalCapacity(alloc, estimated_extra);
        try self.diagnostics.ensureTotalCapacity(alloc, 32);
        try self.scratch_cover.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_statements.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_a.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_b.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_decorators.items.ensureTotalCapacity(alloc, 128);
    }
};

const ScratchBuffer = struct {
    items: std.ArrayList(ast.NodeIndex) = .empty,

    pub inline fn begin(self: *ScratchBuffer) usize {
        return self.items.items.len;
    }

    pub inline fn append(self: *ScratchBuffer, alloc: std.mem.Allocator, index: ast.NodeIndex) Error!void {
        if (self.items.items.len < self.items.capacity) {
            self.items.appendAssumeCapacity(index);
        } else {
            try self.items.append(alloc, index);
        }
    }

    pub inline fn reset(self: *ScratchBuffer, checkpoint: usize) void {
        self.items.shrinkRetainingCapacity(checkpoint);
    }
};

/// Parse JavaScript/TypeScript source code into an AST.
/// Returns a ParseTree that must be freed by calling `tree.deinit()` when you're done using it.
pub fn parse(child_allocator: std.mem.Allocator, source: []const u8, options: Options) Error!ast.ParseTree {
    var parser = Parser.init(child_allocator, source, options);
    return parser.parse();
}
