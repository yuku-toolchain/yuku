const std = @import("std");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const flagMask = @import("token.zig").flagMask;
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");
const util = @import("util");

const statements = @import("syntax/statements.zig");
const comments = @import("comments.zig");

/// How comments are collected. Not at all, as a flat list, attached to host nodes, or both.
pub const CommentMode = enum {
    none,
    flat,
    attached,
    both,

    pub inline fn collects(self: CommentMode) bool {
        return self != .none;
    }
    pub inline fn attaches(self: CommentMode) bool {
        return self == .attached or self == .both;
    }
    pub inline fn isFlat(self: CommentMode) bool {
        return self == .flat or self == .both;
    }
};

pub const Options = struct {
    /// How the source is parsed. `.commonjs` treats the top level as a function body.
    source_type: ast.SourceType = .module,
    /// Language variant that decides which syntax features are enabled.
    lang: ast.Lang = .js,
    /// Whether parenthesized expressions are kept as `ParenthesizedExpression` nodes.
    preserve_parens: bool = true,
    /// Whether and how comments are collected.
    comments: CommentMode = .flat,
    /// Whether to keep every consumed token in `Tree.tokens`.
    tokens: bool = false,
};

pub const Context = packed struct {
    /// `[In]`
    in: bool = true,
    /// `[Yield]`
    yield: bool = false,
    /// `[Await]`
    await: bool = false,
    /// `[Return]`
    @"return": bool = false,
    /// Body of `if`, `while`, `for`, `with`, or a labelled statement, where lexical
    /// declarations need a block.
    single_statement: bool = false,
    /// Inside a directive prologue.
    directive_prologue: bool = false,
};

pub const TsContext = packed struct {
    /// Inside a `declare`-prefixed declaration, where nested declarations inherit ambient rules.
    ambient: bool = false,
    /// A trailing `?` after a type does not start a new conditional.
    disallow_conditional_types: bool = false,
    /// Whether a speculatively parsed arrow may carry a return type. Cleared only while a
    /// conditional expression re-parses its consequent after a `(x): T => ...` arrow ate
    /// the ternary `:`.
    allow_arrow_return_type: bool = true,
};

const ParserState = struct {
    // start index of the cover that had a trailing comma
    cover_has_trailing_comma: ?u32 = null,
    cover_has_init_name: bool = false,
    // `(a) = b` is legal, but the same node is a syntax error in binding position
    stripped_paren: ?ast.NodeIndex = null,
};

pub const Error = error{OutOfMemory};

pub const Parser = struct {
    tree: ast.Tree,
    source: []const u8,
    source_type: ast.SourceType,
    lang: ast.Lang,
    preserve_parens: bool,
    comment_mode: CommentMode,
    collect_tokens: bool,
    lexer: lexer.Lexer,
    diagnostics: std.ArrayList(ast.Diagnostic) = .empty,
    // the tokens the committed parse advanced past, `current_token` is never in it
    tokens: std.ArrayList(Token) = .empty,

    current_token: Token,
    // spans must stop at a consumed delimiter, not at the next token past trivia
    prev_token_end: u32 = 0,

    scratch_statements: ScratchBuffer = .{},
    scratch_cover: ScratchBuffer = .{},
    scratch_decorators: ScratchBuffer = .{},

    scratch_a: ScratchBuffer = .{},
    scratch_b: ScratchBuffer = .{},

    context: Context = .{},
    ts_context: TsContext = .{},
    state: ParserState = .{},

    pub fn init(child_allocator: std.mem.Allocator, source: []const u8, options: Options) Parser {
        var b = ast.Tree.init(child_allocator, source);
        b.source_type = options.source_type;
        b.lang = options.lang;
        return .{
            .tree = b,
            .source = source,
            .source_type = options.source_type,
            .lang = options.lang,
            .preserve_parens = options.preserve_parens,
            .comment_mode = options.comments,
            .collect_tokens = options.tokens,
            .lexer = undefined,
            .current_token = Token.eof(0),
        };
    }

    pub inline fn allocator(self: *Parser) std.mem.Allocator {
        return self.tree.allocator();
    }

    pub fn parse(self: *Parser) Error!ast.Tree {
        try self.parseInner();
        return self.tree;
    }

    fn parseInner(self: *Parser) Error!void {
        const alloc = self.allocator();

        self.lexer = try lexer.Lexer.init(
            self.source,
            alloc,
            self.source_type,
            self.comment_mode.collects(),
        );

        // ScriptBody is [~Yield, ~Await, ~Return], ModuleItemList is [+Await], and a
        // commonjs body is [+Return]
        self.context.yield = false;
        self.context.await = self.tree.isModule();
        self.context.@"return" = self.source_type == .commonjs;

        self.ts_context.ambient = self.lang == .dts;

        try self.advance() orelse {
            self.current_token = try self.recoverNextToken();
        };

        errdefer self.tree.arena.deinit();

        try self.ensureCapacity();

        const body = try self.parseBody(null, .program);

        std.debug.assert(self.current_token.tag == .eof);
        try self.commitEof();

        const end = self.current_token.span.end;

        self.tree.root = try self.tree.addNode(
            .{
                .program = .{
                    .source_type = if (self.source_type == .module) .module else .script,
                    .body = body,
                    .hashbang = if (self.lexer.hashbang) |h| .{
                        .value = self.tree.sourceSlice(h.start, h.start + h.len),
                    } else null,
                },
            },
            .{ .start = 0, .end = end },
        );

        self.tree.diagnostics = self.diagnostics;

        if (!self.preserve_parens) {
            stripParenthesizedNodes(&self.tree);
        }

        if (self.comment_mode.attaches()) {
            try comments.attach(&self.tree, self.lexer.comments.items);
        }
        if (self.comment_mode.isFlat()) {
            self.tree.comments = self.lexer.comments.items;
        }
        if (self.collect_tokens) {
            self.tree.tokens = self.tokens.items;
        }
    }

    inline fn commitToken(self: *Parser, token: Token) Error!void {
        if (!self.collect_tokens) return;
        // the initial placeholder or an empty jsx text run
        if (token.span.start == token.span.end) return;
        if (self.tokens.items.len < self.tokens.capacity) {
            self.tokens.appendAssumeCapacity(token);
        } else {
            try self.tokens.append(self.allocator(), token);
        }
    }

    fn commitEof(self: *Parser) Error!void {
        if (!self.collect_tokens) return;
        std.debug.assert(self.current_token.tag == .eof);
        try self.tokens.append(self.allocator(), self.current_token);
    }

    const BodyKind = enum {
        program,
        function,
        module_block,
        other,
    };

    pub fn parseBody(self: *Parser, terminator: ?TokenTag, kind: BodyKind) Error!ast.IndexRange {
        self.context.directive_prologue =
            kind == .program or kind == .function or kind == .module_block;

        defer self.context.directive_prologue = false;

        const statements_checkpoint = self.scratch_statements.begin();
        defer self.scratch_statements.reset(statements_checkpoint);

        while (!self.isAtBodyEnd(terminator)) {
            if (try statements.parseStatement(self, .{})) |statement| {
                try self.scratch_statements.append(self.allocator(), statement);
            } else {
                try self.recover(terminator);
            }
        }

        return self.flushToExtras(&self.scratch_statements, statements_checkpoint);
    }

    inline fn isAtBodyEnd(self: *Parser, terminator: ?TokenTag) bool {
        return self.current_token.tag == .eof or
            (terminator != null and self.current_token.tag == terminator.?);
    }

    /// Returns the resolved name of an identifier-like token, without a private `#` and
    /// with escapes decoded.
    pub inline fn identifierName(self: *Parser, token: Token) Error!ast.String {
        const is_private = token.tag == .private_identifier;
        const start = token.span.start + @as(u32, @intFromBool(is_private));
        if (token.isEscaped()) return self.decodeEscapedIdentifier(start, token.span.end);
        return self.tree.sourceSlice(start, token.span.end);
    }

    /// Returns the decoded string value without surrounding quotes.
    pub inline fn stringValue(self: *Parser, token: Token) Error!ast.String {
        if (!token.isEscaped()) {
            return self.tree.sourceSlice(token.span.start + 1, token.span.end - 1);
        }
        return self.decodeEscapedString(token.span.start + 1, token.span.end - 1);
    }

    /// Returns the decoded content of a template quasi span.
    pub inline fn templateElementValue(
        self: *Parser,
        token: Token,
        span: ast.Span,
    ) Error!ast.String {
        if (!token.isEscaped()) {
            return self.tree.sourceSlice(span.start, span.end);
        }
        return self.decodeEscapedString(span.start, span.end);
    }

    fn decodeEscapedIdentifier(self: *Parser, start: u32, end: u32) Error!ast.String {
        @branchHint(.cold);
        var buf: [256]u8 = undefined;
        return try self.tree.addString(
            util.Utf.decodeIdentifierEscapes(self.source[start..end], &buf),
        );
    }

    fn decodeEscapedString(self: *Parser, start: u32, end: u32) Error!ast.String {
        @branchHint(.cold);
        const alloc = self.allocator();
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(alloc);
        try util.Utf.decodeStringEscapes(self.source[start..end], &buf, alloc);
        return try self.tree.addString(buf.items);
    }

    pub inline fn describeToken(self: *Parser, token: Token) []const u8 {
        if (token.tag == .eof) return "end of file";
        return token.tag.toString() orelse token.text(self.source);
    }

    pub inline fn setLexerMode(self: *Parser, mode: lexer.LexerMode) void {
        self.lexer.mode = mode;
    }

    pub fn flushToExtras(
        self: *Parser,
        scratch: *ScratchBuffer,
        scratch_checkpoint: usize,
    ) Error!ast.IndexRange {
        const start: u32 = @intCast(self.tree.extras.items.len);
        const slice = scratch.items.items[scratch_checkpoint..scratch.items.items.len];
        const len: u32 = @intCast(slice.len);

        if (slice.len > 0) {
            if (self.tree.extras.items.len + slice.len <= self.tree.extras.capacity) {
                self.tree.extras.appendSliceAssumeCapacity(slice);
            } else {
                try self.tree.extras.appendSlice(self.allocator(), slice);
            }
        }

        return .{ .start = start, .len = len };
    }

    pub inline fn spanText(self: *const Parser, span: ast.Span) []const u8 {
        return self.source[span.start..span.end];
    }

    inline fn nextToken(self: *Parser) Error!?Token {
        return self.lexer.nextToken() catch |e| {
            if (e == error.OutOfMemory) return error.OutOfMemory;
            try self.reportLexicalError(@errorCast(e));
            return null;
        };
    }

    pub noinline fn reportLexicalError(self: *Parser, lex_err: lexer.LexicalError) Error!void {
        @branchHint(.cold);
        const cursor = self.lexer.cursor;
        try self.report(
            .{ .start = cursor, .end = @min(cursor + 1, self.source.len) },
            lexer.getLexicalErrorMessage(lex_err),
            .{ .help = lexer.getLexicalErrorHelp(lex_err) },
        );
    }

    /// Advances to the next token, reporting an escaped keyword consumed in keyword position.
    pub inline fn advance(self: *Parser) Error!?void {
        try self.checkEscapedKeyword();
        return self.advanceWithoutEscapeCheck();
    }

    /// Advances without the escaped-keyword check.
    pub inline fn advanceWithoutEscapeCheck(self: *Parser) Error!?void {
        const leaving = self.current_token;
        self.prev_token_end = leaving.span.end;
        if (self.lexer.tryNextToken()) |token| {
            self.current_token = token;
        } else {
            self.current_token = try self.nextToken() orelse return null;
        }
        try self.commitToken(leaving);
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

    /// Reports an escaped-keyword error for a token consumed before its role was known.
    pub inline fn reportIfEscapedKeyword(self: *Parser, token: Token) Error!void {
        if (token.isEscaped()) try self.reportEscapedKeyword(token.span);
    }

    /// Peeks the token after `current_token` without advancing. Use `beginPeek` for
    /// multi-token lookahead.
    pub inline fn peekAhead(self: *Parser) Token {
        var peek = self.beginPeek();
        defer peek.end();
        return peek.next();
    }

    /// Captures a snapshot of parser state for `rewind`.
    pub fn checkpoint(self: *const Parser) Checkpoint {
        return .{
            .lexer_cursor = self.lexer.cursor,
            .lexer_state = self.lexer.state,
            .lexer_mode = self.lexer.mode,
            .lexer_comments_len = self.lexer.comments.items.len,
            .current_token = self.current_token,
            .prev_token_end = self.prev_token_end,
            .nodes_len = self.tree.nodes.len,
            .extra_len = self.tree.extras.items.len,
            .diagnostics_len = self.diagnostics.items.len,
            .tokens_len = self.tokens.items.len,
            .context = self.context,
            .ts_context = self.ts_context,
            .state = self.state,
        };
    }

    /// Restores parser state captured by `checkpoint`.
    pub fn rewind(self: *Parser, cp: Checkpoint) void {
        self.lexer.cursor = cp.lexer_cursor;
        self.lexer.state = cp.lexer_state;
        self.lexer.mode = cp.lexer_mode;
        self.lexer.comments.shrinkRetainingCapacity(cp.lexer_comments_len);
        self.current_token = cp.current_token;
        self.prev_token_end = cp.prev_token_end;
        self.tree.nodes.shrinkRetainingCapacity(cp.nodes_len);
        self.tree.extras.shrinkRetainingCapacity(cp.extra_len);
        self.diagnostics.shrinkRetainingCapacity(cp.diagnostics_len);
        self.tokens.shrinkRetainingCapacity(cp.tokens_len);
        self.context = cp.context;
        self.ts_context = cp.ts_context;
        self.state = cp.state;
    }

    pub const Peek = struct {
        parser: *Parser,
        state: lexer.LexerState,
        cursor: u32,
        comments_len: usize,

        pub inline fn next(self: *Peek) Token {
            return self.parser.lexer.nextToken() catch
                Token.invalid(self.parser.lexer.cursor);
        }

        pub inline fn end(self: Peek) void {
            self.parser.lexer.state = self.state;
            self.parser.lexer.cursor = self.cursor;
            self.parser.lexer.comments.shrinkRetainingCapacity(self.comments_len);
        }
    };

    pub inline fn beginPeek(self: *Parser) Peek {
        return .{
            .parser = self,
            .state = self.lexer.state,
            .cursor = self.lexer.cursor,
            .comments_len = self.lexer.comments.items.len,
        };
    }

    /// Replaces the current token with a re-scanned one and advances past it.
    pub inline fn advanceWithRescannedToken(self: *Parser, token: Token) Error!?void {
        var rescanned = token;
        // a rescan past the current token completes it, one inside it supersedes it
        if (token.span.start >= self.current_token.span.end) {
            try self.commitToken(self.current_token);
        } else {
            rescanned.flags |= newlineFlag(self.current_token);
        }
        self.current_token = rescanned;
        return self.advance();
    }

    /// Re-tokenizes the current token in the current lexer mode.
    pub inline fn reScanCurrent(self: *Parser) Error!?void {
        const replaced = self.current_token;
        self.lexer.rewindTo(replaced.span.start);
        self.current_token = try self.nextToken() orelse return null;
        self.current_token.flags |= newlineFlag(replaced);
    }

    // a rescan starts at the token it replaces, past the trivia that set the flag
    inline fn newlineFlag(token: Token) u8 {
        return token.flags & flagMask(.line_terminator_before);
    }

    pub fn expect(
        self: *Parser,
        comptime tag: TokenTag,
        message: []const u8,
        help: ?[]const u8,
    ) Error!bool {
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

    /// Consumes an optional semicolon. ASI always inserts one after the `)` of `do-while`.
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

    fn stripParenthesizedNodes(tree: *ast.Tree) void {
        const datas = tree.nodes.items(.data);
        const spans = tree.nodes.items(.span);
        for (0..datas.len) |i| {
            var inner: u32 = switch (datas[i]) {
                .parenthesized_expression => |p| @intFromEnum(p.expression),
                .ts_parenthesized_type => |p| @intFromEnum(p.type_annotation),
                else => continue,
            };

            while (true) {
                switch (datas[inner]) {
                    .parenthesized_expression => |p| inner = @intFromEnum(p.expression),
                    .ts_parenthesized_type => |p| inner = @intFromEnum(p.type_annotation),
                    else => break,
                }
            }

            datas[i] = datas[inner];
            spans[i] = spans[inner];
        }
    }

    pub const ReportOptions = struct {
        severity: ast.Severity = .@"error",
        help: ?[]const u8 = null,
        labels: []const ast.Label = &.{},
    };

    pub fn report(
        self: *Parser,
        span: ast.Span,
        message: []const u8,
        opts: ReportOptions,
    ) Error!void {
        try self.diagnostics.append(self.allocator(), .{
            .severity = opts.severity,
            .message = message,
            .span = span,
            .help = opts.help,
            .labels = opts.labels,
        });
    }

    pub fn reportExpected(
        self: *Parser,
        span: ast.Span,
        message: []const u8,
        opts: ReportOptions,
    ) Error!void {
        const expected_message = try std.fmt.allocPrint(self.allocator(), "{s}, but found '{s}'", .{
            message,
            self.describeToken(self.current_token),
        });

        try self.report(span, expected_message, opts);
    }

    pub fn label(_: *Parser, span: ast.Span, message: []const u8) ast.Label {
        return .{ .span = span, .message = message };
    }

    pub fn labels(self: *Parser, items: []const ast.Label) Error![]const ast.Label {
        return try self.allocator().dupe(ast.Label, items);
    }

    pub fn fmt(self: *Parser, comptime format: []const u8, args: anytype) Error![]u8 {
        return try std.fmt.allocPrint(self.allocator(), format, args);
    }

    pub fn recover(self: *Parser, terminator: ?TokenTag) Error!void {
        while (self.current_token.tag != .eof) {
            // a failed rescan leaves the lexer inside the current token, which is then stale
            if (self.current_token.span.end <= self.lexer.cursor) {
                try self.commitToken(self.current_token);
            }
            self.current_token = try self.recoverNextToken();

            if (self.current_token.tag == .eof) break;

            if (terminator) |t| {
                if (self.current_token.tag == t) break;
            }

            if (self.current_token.hasLineTerminatorBefore() and
                self.current_token.tag.isKeyword()) break;
        }
    }

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
        if (self.tree.nodes.capacity > 0) return;

        const alloc = self.allocator();
        const source_len = self.source.len;

        const estimated_nodes = if (source_len < 512_000)
            @max(256, source_len / 2)
        else
            source_len / 4;

        const estimated_extra = if (source_len < 512_000)
            source_len / 6
        else
            source_len / 12;

        try self.tree.nodes.ensureTotalCapacity(alloc, estimated_nodes);
        try self.tree.extras.ensureTotalCapacity(alloc, estimated_extra);
        try self.scratch_cover.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_statements.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_a.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_b.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_decorators.items.ensureTotalCapacity(alloc, 128);
        if (self.collect_tokens) {
            try self.tokens.ensureTotalCapacity(alloc, @max(64, source_len / 3));
        }
    }
};

pub const Checkpoint = struct {
    lexer_cursor: u32,
    lexer_state: lexer.LexerState,
    lexer_mode: lexer.LexerMode,
    lexer_comments_len: usize,

    current_token: Token,
    prev_token_end: u32,

    nodes_len: usize,
    extra_len: usize,
    diagnostics_len: usize,
    tokens_len: usize,

    context: Context,
    ts_context: TsContext,
    state: ParserState,
};

const ScratchBuffer = struct {
    items: std.ArrayList(ast.NodeIndex) = .empty,

    pub inline fn begin(self: *ScratchBuffer) usize {
        return self.items.items.len;
    }

    pub inline fn append(
        self: *ScratchBuffer,
        alloc: std.mem.Allocator,
        index: ast.NodeIndex,
    ) Error!void {
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

/// Parses JavaScript or TypeScript source into a `Tree`. Call `deinit()` to free it.
pub fn parse(
    child_allocator: std.mem.Allocator,
    source: []const u8,
    options: Options,
) Error!ast.Tree {
    var p = Parser.init(child_allocator, source, options);
    return p.parse();
}
