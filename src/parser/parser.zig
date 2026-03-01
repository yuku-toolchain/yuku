const std = @import("std");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
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
    /// When true, `await` is a keyword (allowed as an expression, disallowed as an identifier).
    await_is_keyword: bool = false,
    /// When true, `yield` is a keyword (allowed as an expression, disallowed as an identifier).
    yield_is_keyword: bool = false,
    allow_in: bool = true,
    /// Whether `return` statements are allowed in the current statement list.
    allow_return_statement: bool = false,
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
    /// Whether the parser is currently in strict mode.
    strict_mode: bool = false,
    /// Tracks if the cover (array or object) we are parsing has a trailing comma
    /// value is the start index of the cover
    cover_has_trailing_comma: ?u32 = null,
    /// Tracks if CoverInitializedName ({a = 1}) was parsed in current cover context.
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
    current_token: Token,

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

        if (self.isModule()) _ = self.enterStrictMode();

        // ScriptBody: StatementList[~Yield, ~Await, ~Return]
        // ModuleItemList: ModuleItem[~Yield, +Await, ~Return]
        self.context.yield_is_keyword = false;
        self.context.await_is_keyword = self.isModule();
        self.context.allow_return_statement = false;

        // let's begin
        try self.advance() orelse {
            self.current_token = Token.eof(0);
        };

        errdefer self.arena.deinit();

        try self.ensureCapacity();

        const body = try self.parseBody(null, .program);

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

    const BodyKind = enum {
        program,
        function,
        other,
    };

    pub fn parseBody(self: *Parser, terminator: ?TokenTag, kind: BodyKind) Error!ast.IndexRange {
        // save and restore strict mode, directives like "use strict" only apply within this scope
        const prev_strict = self.isStrictMode();
        defer self.restoreStrictMode(prev_strict);

        self.context.in_directive_prologue = kind == .program or kind == .function;

        defer self.context.in_directive_prologue = false;

        const statements_checkpoint = self.scratch_statements.begin();
        defer self.scratch_statements.reset(statements_checkpoint);

        while (!self.isAtBodyEnd(terminator)) {
            if (try statements.parseStatement(self, .{})) |statement| {
                try self.scratch_statements.append(self.allocator(), statement);
            } else {
                try self.recover(terminator) orelse break;
            }
        }

        return self.addExtraFromScratch(&self.scratch_statements, statements_checkpoint);
    }

    inline fn isAtBodyEnd(self: *Parser, terminator: ?TokenTag) bool {
        return self.current_token.tag == .eof or
            (terminator != null and self.current_token.tag == terminator.?);
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

    pub inline fn isStrictMode(self: *Parser) bool {
        return self.state.strict_mode;
    }

    pub inline fn enterStrictMode(self: *Parser) bool {
        const prev = self.state.strict_mode;
        self.state.strict_mode = true;
        self.lexer.state.strict_mode = true;
        return prev;
    }

    pub inline fn restoreStrictMode(self: *Parser, prev: bool) void {
        self.state.strict_mode = prev;
        self.lexer.state.strict_mode = prev;
    }

    // utils

    pub inline fn setLexerMode(self: *Parser, mode: lexer.LexerMode) void {
        self.lexer.mode = mode;
    }

    pub inline fn addNode(self: *Parser, data: ast.NodeData, span: ast.Span) Error!ast.NodeIndex {
        const index: ast.NodeIndex = @intCast(self.nodes.len);
        try self.nodes.append(self.allocator(), .{ .data = data, .span = span });
        return index;
    }

    pub fn addExtra(self: *Parser, indices: []const ast.NodeIndex) Error!ast.IndexRange {
        const start: u32 = @intCast(self.extra.items.len);
        const len: u32 = @intCast(indices.len);
        try self.extra.appendSlice(self.allocator(), indices);
        return .{ .start = start, .len = len };
    }

    pub fn addExtraFromScratch(self: *Parser, scratch: *ScratchBuffer, checkpoint: usize) Error!ast.IndexRange {
        const start: u32 = @intCast(self.extra.items.len);
        const slice = scratch.items.items[checkpoint..scratch.items.items.len];
        const len: u32 = @intCast(slice.len);

        if (slice.len > 0) {
            try self.extra.appendSlice(self.allocator(), slice);
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

    pub inline fn getTokenText(self: *const Parser, token: Token) []const u8 {
        return token.text(self.source);
    }

    inline fn nextToken(self: *Parser) Error!?Token {
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

    /// advance to the next token. reports an error if the current token
    /// is an escaped keyword being consumed in a keyword position.
    pub inline fn advance(self: *Parser) Error!?void {
        try self.checkEscapedKeyword();
        self.current_token = try self.nextToken() orelse return null;
    }

    /// advance without the escaped-keyword check.
    pub inline fn advanceWithoutEscapeCheck(self: *Parser) Error!?void {
        self.current_token = try self.nextToken() orelse return null;
    }

    pub inline fn checkEscapedKeyword(self: *Parser) Error!void {
        const current_token = self.current_token;

        if (!current_token.isEscaped()) return;

        if (current_token.tag.isKeyword()) {
            try self.reportEscapedKeyword(current_token.span);
        }
    }

    pub fn reportEscapedKeyword(self: *Parser, span: ast.Span) Error!void {
        try self.diagnostics.append(self.allocator(), .{
            .message = "Keywords cannot contain escape characters",
            .span = span,
            .help = "Remove the escape characters",
        });
    }

    /// reports an escaped-keyword error if the given token has the escaped flag.
    /// use for deferred checks where the token was consumed before its role was known.
    pub inline fn reportIfEscapedKeyword(self: *Parser, token: Token) Error!void {
        if (token.isEscaped()) try self.reportEscapedKeyword(token.span);
    }

    pub fn lookAhead(self: *Parser) Error!?Token {
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
    pub inline fn advanceWithRescannedToken(self: *Parser, token: Token) Error!?void {
        self.current_token = token;
        return self.advance();
    }

    pub fn expect(self: *Parser, comptime tag: TokenTag, message: []const u8, help: ?[]const u8) Error!bool {
        if (self.current_token.tag == tag) {
            try self.advance() orelse return false;
            return true;
        }

        try self.reportExpected(self.current_token.span, message, .{ .help = help });

        return false;
    }

    pub fn eatSemicolon(self: *Parser, end: u32) Error!?u32 {
        if (self.current_token.tag == .semicolon) {
            const semicolon_end = self.current_token.span.end;
            try self.advance() orelse return null;
            return semicolon_end;
        } else {
            if (!self.canInsertImplicitSemicolon(self.current_token)) {
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
        if (self.current_token.tag == .semicolon) {
            const semicolon_end = self.current_token.span.end;
            try self.advance() orelse return null;
            return semicolon_end;
        }
        return end;
    }

    /// https://tc39.es/ecma262/#sec-rules-of-automatic-semicolon-insertion
    pub inline fn canInsertImplicitSemicolon(_: *Parser, token: Token) bool {
        return token.tag == .eof or token.hasLineTerminatorBefore() or token.tag == .right_brace;
    }

    pub inline fn describeToken(self: *Parser, token: Token) []const u8 {
        if (token.tag == .eof) return "end of file";
        return token.tag.toString() orelse token.text(self.source);
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

    fn recover(self: *Parser, terminator: ?TokenTag) Error!?void {
        var depth: i32 = 0;
        var depth_escaped = false;

        while (self.current_token.tag != .eof) {
            switch (self.current_token.tag) {
                .left_brace => {
                    depth += 1;
                    self.current_token = try self.recoverNextToken();
                    depth_escaped = true;
                    continue;
                },
                .right_brace => {
                    depth -= 1;
                    self.current_token = try self.recoverNextToken();
                    depth_escaped = true;
                    continue;
                },
                else => {},
            }

            const debt_check = if (depth_escaped) depth <= 0 else depth < 0;

            if (terminator) |t| {
                if (self.current_token.tag == t and debt_check) {
                    break;
                }
            }

            if (
                self.current_token.hasLineTerminatorBefore() and
                self.current_token.tag.isKeyword() and
                debt_check
            )
            {
                break;
            }

            depth_escaped = false;
            self.current_token = try self.recoverNextToken();
        }
    }

    /// advances to the next token during error recovery, skipping characters that cause lexical errors.
    fn recoverNextToken(self: *Parser) Error!Token {
        while (true) {
            return self.lexer.nextToken() catch |e| {
                if (e == error.OutOfMemory) return error.OutOfMemory;

                if (self.lexer.cursor < self.source.len) {
                    self.lexer.cursor += 1;
                } else {
                    return Token.eof(@intCast(self.source.len));
                }

                continue;
            };
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
        try self.items.append(alloc, index);
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
