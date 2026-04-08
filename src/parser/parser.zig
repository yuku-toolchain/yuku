const std = @import("std");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");
const util = @import("util");

const statements = @import("syntax/statements.zig");

pub const Options = struct {
    /// Source type determines how the code is parsed and evaluated.
    /// Defaults to `.module` (ES module semantics, strict mode enabled).
    source_type: ast.SourceType = .module,
    /// Language variant determines which syntax features are enabled.
    /// Defaults to `.js` (plain JavaScript).
    lang: ast.Lang = .js,
    /// When true, parenthesized expressions are represented as
    /// `ParenthesizedExpression` nodes in the AST. When false (default),
    /// parentheses are stripped and only the inner expression is kept.
    preserve_parens: bool = false,
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
    /// Tracks if the cover (array or object) we are parsing has a trailing comma
    /// value is the start index of the cover
    cover_has_trailing_comma: ?u32 = null,
    /// Tracks if CoverInitializedName ({a = 1}) was parsed in current cover context.
    cover_has_init_name: bool = false,
};

pub const Error = error{OutOfMemory};

pub const Parser = struct {
    tree: ast.Tree,
    source: []const u8,
    source_type: ast.SourceType,
    lang: ast.Lang,
    preserve_parens: bool,
    lexer: lexer.Lexer,
    diagnostics: std.ArrayList(ast.Diagnostic) = .empty,
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

        self.lexer = try lexer.Lexer.init(self.source, alloc, self.source_type);

        // ScriptBody: StatementList[~Yield, ~Await, ~Return]
        // ModuleItemList: ModuleItem[~Yield, +Await, ~Return]
        self.context.yield_is_keyword = false;
        self.context.await_is_keyword = self.tree.isModule();
        self.context.allow_return_statement = false;

        // let's begin
        try self.advance() orelse {
            self.current_token = Token.eof(0);
        };

        errdefer self.tree.arena.deinit();

        try self.ensureCapacity();

        const body = try self.parseBody(null, .program);

        const end = self.current_token.span.end;

        self.tree.program = try self.tree.createNode(
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

        for (self.lexer.comments.items) |*comment| {
            // strip delimiters: '//' or '/*' from start, '*/' from block end
            const content_start = comment.start + 2;
            const content_end = if (comment.type == .block) comment.end - 2 else comment.end;
            comment.value = self.tree.sourceSlice(content_start, content_end);
        }
        self.tree.comments = try self.lexer.comments.toOwnedSlice(alloc);
    }

    const BodyKind = enum {
        program,
        function,
        other,
    };

    pub fn parseBody(self: *Parser, terminator: ?TokenTag, kind: BodyKind) Error!ast.IndexRange {
        self.context.in_directive_prologue = kind == .program or kind == .function;

        defer self.context.in_directive_prologue = false;

        const statements_checkpoint = self.scratch_statements.begin();
        defer self.scratch_statements.reset(statements_checkpoint);

        while (!self.isAtBodyEnd(terminator)) {
            if (try statements.parseStatement(self, .{})) |statement| {
                try self.scratch_statements.append(self.allocator(), statement);
            } else {
                try self.recover(terminator);
            }
        }

        return self.createExtraFromScratch(&self.scratch_statements, statements_checkpoint);
    }

    inline fn isAtBodyEnd(self: *Parser, terminator: ?TokenTag) bool {
        return self.current_token.tag == .eof or
            (terminator != null and self.current_token.tag == terminator.?);
    }

    /// returns the resolved name for any identifier-like token.
    /// strips '#' for private identifiers, decodes unicode escapes if present.
    pub inline fn identifierName(self: *Parser, token: Token) Error!ast.String {
        const is_private = token.tag == .private_identifier;
        const start = token.span.start + @as(u32, @intFromBool(is_private));
        if (token.isEscaped()) return self.decodeEscapedIdentifier(start, token.span.end);
        return self.tree.sourceSlice(start, token.span.end);
    }

    /// returns the decoded string value without surrounding quotes.
    pub inline fn stringValue(self: *Parser, token: Token) Error!ast.String {
        if (!token.isEscaped()) {
            return self.tree.sourceSlice(token.span.start + 1, token.span.end - 1);
        }
        return self.decodeEscapedString(token.span.start + 1, token.span.end - 1);
    }

    /// returns the decoded content of a template quasi span.
    pub inline fn templateElementValue(self: *Parser, token: Token, span: ast.Span) Error!ast.String {
        if (!token.isEscaped()) {
            return self.tree.sourceSlice(span.start, span.end);
        }
        return self.decodeEscapedString(span.start, span.end);
    }

    fn decodeEscapedIdentifier(self: *Parser, start: u32, end: u32) Error!ast.String {
        @branchHint(.cold);
        var buf: [256]u8 = undefined;
        return try self.tree.addString(util.Utf.decodeIdentifierEscapes(self.source[start..end], &buf));
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

    // utils

    pub inline fn setLexerMode(self: *Parser, mode: lexer.LexerMode) void {
        self.lexer.mode = mode;
    }

    pub fn createExtraFromScratch(self: *Parser, scratch: *ScratchBuffer, checkpoint: usize) Error!ast.IndexRange {
        const start: u32 = @intCast(self.tree.extra.items.len);
        const slice = scratch.items.items[checkpoint..scratch.items.items.len];
        const len: u32 = @intCast(slice.len);

        if (slice.len > 0) {
            if (self.tree.extra.items.len + slice.len <= self.tree.extra.capacity) {
                self.tree.extra.appendSliceAssumeCapacity(slice);
            } else {
                try self.tree.extra.appendSlice(self.allocator(), slice);
            }
        }

        return .{ .start = start, .len = len };
    }


    pub inline fn getSpanText(self: *const Parser, span: ast.Span) []const u8 {
        return self.source[span.start..span.end];
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

    pub fn label(_: *Parser, span: ast.Span, message: []const u8) ast.Label {
        return .{ .span = span, .message = message };
    }

    pub fn labels(self: *Parser, items: []const ast.Label) Error![]const ast.Label {
        return try self.allocator().dupe(ast.Label, items);
    }

    pub fn fmt(self: *Parser, comptime format: []const u8, args: anytype) Error![]u8 {
        return try std.fmt.allocPrint(self.allocator(), format, args);
    }

    pub fn withTsCode(_: *Parser, comptime code: []const u8, comptime message: []const u8) []const u8 {
        return "TS(" ++ code ++ "): " ++ message;
    }

    fn recover(self: *Parser, terminator: ?TokenTag) Error!void {
        while (self.current_token.tag != .eof) {
            self.current_token = try self.recoverNextToken();

            if (self.current_token.tag == .eof) break;

            if (terminator) |t| {
                if (self.current_token.tag == t) break;
            }

            if (
                self.current_token.hasLineTerminatorBefore() and
                self.current_token.tag.isKeyword()
            ) break;
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
        if (self.tree.nodes.capacity > 0) return;

        const alloc = self.allocator();
        const source_len = self.source.len;

        const estimated_nodes = if (source_len < 10_000)
            @max(512, source_len / 2)
        else if (source_len < 100_000)
            source_len / 5
        else
            source_len / 8;

        const estimated_extra = if (source_len < 5_000_000)
            estimated_nodes / 4
        else
            estimated_nodes / 3;

        try self.tree.nodes.ensureTotalCapacity(alloc, estimated_nodes);
        try self.tree.extra.ensureTotalCapacity(alloc, estimated_extra);
        try self.diagnostics.ensureTotalCapacity(alloc, 32);
        try self.scratch_cover.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_statements.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_a.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_b.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_decorators.items.ensureTotalCapacity(alloc, 128);
        try self.tree.strings.ensureCapacity(alloc, 256, 16);
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

/// Parses JavaScript/TypeScript source into a `Tree`.
/// Call `deinit()` when done to free all memory.
pub fn parse(child_allocator: std.mem.Allocator, source: []const u8, options: Options) Error!ast.Tree {
    var p = Parser.init(child_allocator, source, options);
    return p.parse();
}
