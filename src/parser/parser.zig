const std = @import("std");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const flagMask = @import("token.zig").flagMask;
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");
const spec = @import("spec");
const util = @import("util");

const statements = @import("syntax/statements.zig");
const Speculation = @import("syntax/ts/expressions.zig").Speculation;
const grammar = @import("grammar.zig");
const comments = @import("comments.zig");

/// How comments are collected and attached to their nodes.
pub const CommentMode = enum { none, collect, attach };

pub const Options = struct {
    /// How the source is parsed. `.commonjs` treats the top level as a function body.
    source_type: ast.SourceType = .module,
    /// Language variant that decides which syntax features are enabled.
    lang: ast.Lang = .js,
    /// Whether parenthesized expressions are kept as `ParenthesizedExpression` nodes.
    preserve_parens: bool = true,
    /// Whether comments are collected, and attached to their nodes.
    comments: CommentMode = .collect,
    /// Whether to keep every consumed token in `Tree.tokens`.
    tokens: bool = false,
    max_depth: u32 = 256,
};

pub const Context = packed struct {
    /// `[Yield]`
    yield: bool = false,
    /// `[Await]`
    await: bool = false,
    /// `[Return]`
    @"return": bool = false,
    single_statement: bool = false,
    directive_prologue: bool = false,
};

pub const TsContext = packed struct {
    ambient: bool = false,
    disallow_conditional_types: bool = false,
};

const ParserState = struct {
    // `{ a = 1 }` defaults not yet part of a pattern, any left at the end are errors
    cover_init_names: u32 = 0,
    // `(a) = b` is legal, but the same node is a syntax error in binding position
    stripped_paren: ?ast.NodeIndex = null,
};

pub const Error = std.mem.Allocator.Error || error{NestedTooDeeply};

pub const Parser = struct {
    tree: ast.Tree,
    source: []const u8,
    options: Options,
    depth: u32 = 0,
    lexer: lexer.Lexer,
    diagnostics: std.ArrayList(ast.Diagnostic) = .empty,
    tokens: std.ArrayList(Token) = .empty,

    current_token: Token,
    // spans must stop at a consumed delimiter, not at the next token past trivia
    prev_token_end: u32 = 0,

    scratch: std.ArrayList(ast.NodeIndex) = .empty,
    scratch_side: std.ArrayList(ast.NodeIndex) = .empty,
    scratch_string: std.ArrayList(u8) = .empty,

    context: Context = .{},
    ts_context: TsContext = .{},
    state: ParserState = .{},

    ts_rejected_speculations: std.AutoHashMapUnmanaged(Speculation, void) = .empty,

    pub inline fn allocator(self: *Parser) std.mem.Allocator {
        return self.tree.allocator();
    }

    fn parse(self: *Parser) Error!ast.Tree {
        errdefer self.tree.arena.deinit();
        const collect_tokens = self.options.tokens;

        self.lexer = lexer.Lexer.init(
            self.source,
            self.allocator(),
            self.tree.source_type,
            self.options.comments != .none,
        );
        self.lexer.setFastOpen(!collect_tokens);

        // ScriptBody is [~Yield, ~Await, ~Return], ModuleItemList is [+Await], and a
        // commonjs body is [+Return]
        self.context.await = self.tree.isModule();
        self.context.@"return" = self.tree.source_type == .commonjs;
        self.ts_context.ambient = self.tree.lang == .dts;

        try self.advance() orelse {
            self.current_token = try self.recoverNextToken();
        };

        try self.ensureCapacity();

        const body = try self.parseBody(null, .program);
        if (self.state.cover_init_names > 0) try grammar.reportCoverInitializedNames(self);

        std.debug.assert(self.at(.eof));
        if (collect_tokens) try self.tokens.append(self.allocator(), self.current_token);

        self.tree.root = try self.tree.addNode(.{ .program = .{
            .source_type = if (self.tree.source_type == .module) .module else .script,
            .body = body,
            .hashbang = if (self.lexer.hashbang) |h| .{
                .value = self.tree.sourceSlice(h.start, h.start + h.len),
            } else null,
        } }, .{ .start = 0, .end = self.current_token.span.end });

        self.tree.diagnostics = self.diagnostics;
        if (!self.options.preserve_parens) stripParenthesizedNodes(&self.tree);
        const collected = self.lexer.comments.items;
        if (self.options.comments == .attach) try comments.attach(&self.tree, collected);
        self.tree.comments = collected;
        if (collect_tokens) self.tree.tokens = self.tokens.items;
        return self.tree;
    }

    inline fn commitToken(self: *Parser, token: Token) Error!void {
        if (!self.options.tokens) return;
        // the initial placeholder or an empty jsx text run
        if (token.span.start == token.span.end) return;
        if (self.tokens.items.len < self.tokens.capacity) {
            self.tokens.appendAssumeCapacity(token);
        } else {
            try self.tokens.append(self.allocator(), token);
        }
    }

    pub const BodyKind = enum { program, function, module_block, other };

    pub fn parseBody(self: *Parser, terminator: ?TokenTag, kind: BodyKind) Error!ast.IndexRange {
        self.context.directive_prologue = kind != .other;
        defer self.context.directive_prologue = false;

        const body = self.list();
        defer body.deinit();

        while (!self.at(.eof) and self.current_token.tag != terminator) {
            if (try statements.parseStatement(self, .{ .body = kind })) |statement| {
                try body.append(statement);
            } else {
                try self.recover(terminator);
            }
        }

        return body.finish();
    }

    pub inline fn identifierName(self: *Parser, token: Token) Error!ast.String {
        const is_private = token.tag == .private_identifier;
        const start = token.span.start + @as(u32, @intFromBool(is_private));
        if (token.isEscaped()) return self.decodeEscapedIdentifier(start, token.span.end);
        return self.tree.sourceSlice(start, token.span.end);
    }

    pub inline fn stringValue(self: *Parser, token: Token, span: ast.Span) Error!ast.String {
        if (!token.isEscaped()) return self.tree.sourceSlice(span.start, span.end);
        return self.decode(spec.escapes.decodeStringEscapes, span.start, span.end);
    }

    pub inline fn jsxValue(self: *Parser, start: u32, end: u32) Error!ast.String {
        if (std.mem.findScalar(u8, self.source[start..end], '&') == null) {
            return self.tree.sourceSlice(start, end);
        }
        return self.decode(spec.jsx_entities.decodeJSXEntities, start, end);
    }

    fn decodeEscapedIdentifier(self: *Parser, start: u32, end: u32) Error!ast.String {
        @branchHint(.cold);
        var buf: [256]u8 = undefined;
        return try self.tree.addString(
            spec.escapes.decodeIdentifierEscapes(self.source[start..end], &buf),
        );
    }

    fn decode(self: *Parser, comptime decoder: anytype, start: u32, end: u32) Error!ast.String {
        @branchHint(.cold);
        self.scratch_string.clearRetainingCapacity();
        try decoder(self.allocator(), self.source[start..end], &self.scratch_string);
        return try self.tree.addString(self.scratch_string.items);
    }

    pub inline fn spanText(self: *const Parser, span: ast.Span) []const u8 {
        return self.source[span.start..span.end];
    }

    pub inline fn descend(self: *Parser) Error!void {
        if (self.depth == self.options.max_depth) return error.NestedTooDeeply;
        self.depth += 1;
    }

    pub inline fn ascend(self: *Parser) void {
        std.debug.assert(self.depth > 0);
        self.depth -= 1;
    }

    /// Sets `[Await]` and `[Yield]` for a nested function and returns the context around it,
    pub inline fn enterFunction(self: *Parser, is_async: bool, is_generator: bool) Context {
        const outer = self.context;
        self.context.await = is_async;
        self.context.yield = is_generator;
        return outer;
    }

    /// Restores `[Await]`, `[Yield]`, and `[Return]`. The other flags follow statements.
    pub inline fn leaveFunction(self: *Parser, outer: Context) void {
        self.context.await = outer.await;
        self.context.yield = outer.yield;
        self.context.@"return" = outer.@"return";
    }

    pub inline fn at(self: *const Parser, tag: TokenTag) bool {
        return self.current_token.tag == tag;
    }

    pub inline fn advance(self: *Parser) Error!?void {
        const token = self.current_token;
        if (token.isEscaped()) {
            if (token.tag.isKeyword()) try self.reportEscapedKeyword(token.span);
        }
        return self.advanceWithoutEscapeCheck();
    }

    pub inline fn advanceWithoutEscapeCheck(self: *Parser) Error!?void {
        self.prev_token_end = self.current_token.span.end;
        var advanced: ?void = {};
        if (!self.lexer.tryNextToken(&self.current_token)) {
            if (!try self.advanceSlow()) advanced = null;
        }
        return advanced;
    }

    noinline fn advanceSlow(self: *Parser) Error!bool {
        const leaving = self.current_token;
        const fast = self.options.tokens and self.lexer.tryNextTokenAlways(&self.current_token);
        if (!fast) {
            const scanned = @call(.always_inline, nextTokenInto, .{ self, &self.current_token });
            if (!try scanned) return false;
        }
        try self.commitToken(leaving);
        return true;
    }

    fn nextTokenInto(self: *Parser, out: *Token) Error!bool {
        self.lexer.nextTokenInto(out) catch |e| {
            if (e == error.OutOfMemory) return error.OutOfMemory;
            try self.reportLexicalError(@errorCast(e));
            return false;
        };
        return true;
    }

    pub inline fn take(self: *Parser, comptime tag: TokenTag) Error!?u32 {
        std.debug.assert(self.current_token.tag == tag);
        const start = self.current_token.span.start;
        try self.advance() orelse return null;
        return start;
    }

    pub inline fn eat(self: *Parser, comptime tag: TokenTag) Error!?bool {
        var eaten: ?bool = self.current_token.tag == tag;
        if (eaten.?) {
            if (try self.advance() == null) eaten = null;
        }
        return eaten;
    }

    pub inline fn expect(
        self: *Parser,
        comptime tag: TokenTag,
        message: []const u8,
        help: ?[]const u8,
    ) Error!bool {
        var matched = self.current_token.tag == tag;
        if (matched) {
            if (try self.advance() == null) matched = false;
        } else {
            try self.reportExpected(message, .{ .help = help });
        }
        return matched;
    }

    pub inline fn eatSemicolon(self: *Parser, end: u32) Error!?u32 {
        var statement_end: ?u32 = end;
        if (self.current_token.tag == .semicolon) {
            statement_end = self.current_token.span.end;
            if (try self.advance() == null) statement_end = null;
        } else if (!self.canInsertSemicolon()) {
            try self.reportMissingSemicolon();
            statement_end = null;
        }
        return statement_end;
    }

    fn reportMissingSemicolon(self: *Parser) Error!void {
        @branchHint(.cold);
        try self.reportExpected("Expected ';' after the statement", .{
            .help = "Add ';' or a line break before it",
        });
    }

    /// https://tc39.es/ecma262/#sec-rules-of-automatic-semicolon-insertion
    pub inline fn canInsertSemicolon(self: *const Parser) bool {
        const token = self.current_token;
        return token.tag == .eof or token.hasLineTerminatorBefore() or token.tag == .right_brace;
    }

    pub noinline fn reportEscapedKeyword(self: *Parser, span: ast.Span) Error!void {
        @branchHint(.cold);
        try self.report(span, "Keywords cannot contain escape characters", .{
            .help = "Write the keyword without escapes",
        });
    }

    pub inline fn reportIfEscapedKeyword(self: *Parser, token: Token) Error!void {
        if (token.isEscaped()) try self.reportEscapedKeyword(token.span);
    }

    pub inline fn nextMayStartWith(self: *const Parser, byte: u8) bool {
        const next = self.lexer.peekByte();
        return next == byte or next == 0;
    }

    pub inline fn peekAhead(self: *Parser) Token {
        var peek = self.beginPeek();
        defer peek.end();
        return peek.next();
    }

    pub fn peekSecond(self: *Parser) Token {
        var peek = self.beginPeek();
        defer peek.end();
        _ = peek.next();
        return peek.next();
    }

    pub const Peek = struct {
        parser: *Parser,
        state: lexer.LexerState,
        cursor: u32,
        comments_len: usize,

        pub inline fn next(self: *Peek) Token {
            var token: Token = undefined;
            if (self.parser.lexer.tryNextTokenAlways(&token)) return token;
            return self.parser.lexer.nextToken() catch Token.invalid(self.parser.lexer.cursor);
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

    pub fn checkpoint(self: *const Parser) Checkpoint {
        return .{
            .lexer_cursor = self.lexer.cursor,
            .lexer_state = self.lexer.state,
            .lexer_mode = self.lexer.mode,
            .lexer_comments_len = self.lexer.comments.items.len,
            .current_token = self.current_token,
            .prev_token_end = self.prev_token_end,
            .nodes_len = self.tree.nodes.len,
            .extras_len = self.tree.extras.items.len,
            .diagnostics_len = self.diagnostics.items.len,
            .tokens_len = self.tokens.items.len,
            .context = self.context,
            .ts_context = self.ts_context,
            .state = self.state,
        };
    }

    pub fn rewind(self: *Parser, cp: Checkpoint) void {
        self.lexer.cursor = cp.lexer_cursor;
        self.lexer.state = cp.lexer_state;
        self.lexer.setMode(cp.lexer_mode);
        self.lexer.comments.shrinkRetainingCapacity(cp.lexer_comments_len);
        self.current_token = cp.current_token;
        self.prev_token_end = cp.prev_token_end;
        self.tree.nodes.shrinkRetainingCapacity(cp.nodes_len);
        self.tree.extras.shrinkRetainingCapacity(cp.extras_len);
        self.diagnostics.shrinkRetainingCapacity(cp.diagnostics_len);
        self.tokens.shrinkRetainingCapacity(cp.tokens_len);
        self.context = cp.context;
        self.ts_context = cp.ts_context;
        self.state = cp.state;
    }

    pub fn advanceWithRescannedToken(self: *Parser, token: Token) Error!?void {
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

    pub fn reScanCurrent(self: *Parser) Error!?void {
        const replaced = self.current_token;
        self.lexer.rewindTo(replaced.span.start);
        if (!try self.nextTokenInto(&self.current_token)) return null;
        self.current_token.flags |= newlineFlag(replaced);
    }

    // a rescan starts at the token it replaces, past the trivia that set the flag
    inline fn newlineFlag(token: Token) u8 {
        return token.flags & flagMask(.line_terminator_before);
    }

    pub inline fn finish(self: *Parser, start: u32, data: ast.NodeData) Error!ast.NodeIndex {
        return self.tree.addNode(data, .{ .start = start, .end = self.prev_token_end });
    }

    pub inline fn finishAt(
        self: *Parser,
        start: u32,
        last: ast.NodeIndex,
        data: ast.NodeData,
    ) Error!ast.NodeIndex {
        return self.tree.addNode(data, .{ .start = start, .end = self.tree.span(last).end });
    }

    pub fn finishStatement(self: *Parser, start: u32, data: ast.NodeData) Error!?ast.NodeIndex {
        _ = try self.eatSemicolon(self.prev_token_end) orelse return null;
        return try self.finish(start, data);
    }

    pub inline fn leaf(self: *Parser, data: ast.NodeData) Error!?ast.NodeIndex {
        const span = self.current_token.span;
        try self.advance() orelse return null;
        return try self.tree.addNode(data, span);
    }

    pub const List = struct {
        parser: *Parser,
        buffer: *std.ArrayList(ast.NodeIndex),
        start: usize,

        pub inline fn append(self: List, node: ast.NodeIndex) Error!void {
            if (self.buffer.items.len < self.buffer.capacity) {
                self.buffer.appendAssumeCapacity(node);
            } else {
                try self.grow(node);
            }
        }

        noinline fn grow(self: List, node: ast.NodeIndex) Error!void {
            @branchHint(.cold);
            try self.buffer.append(self.parser.allocator(), node);
        }

        pub inline fn parseSeparated(
            self: List,
            comptime close: ?TokenTag,
            comptime parseElement: anytype,
            args: anytype,
        ) Error!?void {
            const parser = self.parser;
            while (close == null or (parser.current_token.tag != close.? and !parser.at(.eof))) {
                const element = try @call(.auto, parseElement, .{parser} ++ args);
                try self.append(element orelse return null);
                if (!(try parser.eat(.comma) orelse return null)) break;
            }
        }

        pub inline fn items(self: List) []const ast.NodeIndex {
            return self.buffer.items[self.start..];
        }

        pub inline fn finish(self: List) Error!ast.IndexRange {
            return self.parser.tree.addExtra(self.items());
        }

        pub inline fn deinit(self: List) void {
            self.buffer.shrinkRetainingCapacity(self.start);
        }
    };

    pub inline fn list(self: *Parser) List {
        return .{ .parser = self, .buffer = &self.scratch, .start = self.scratch.items.len };
    }

    pub inline fn sideList(self: *Parser) List {
        return .{
            .parser = self,
            .buffer = &self.scratch_side,
            .start = self.scratch_side.items.len,
        };
    }

    pub const ReportOptions = struct {
        help: ?[]const u8 = null,
        label: ?ast.Label = null,
    };

    pub fn report(
        self: *Parser,
        span: ast.Span,
        message: []const u8,
        opts: ReportOptions,
    ) Error!void {
        @branchHint(.cold);
        const alloc = self.allocator();
        try self.diagnostics.append(alloc, .{
            .message = message,
            .span = span,
            .help = opts.help,
            .labels = if (opts.label) |label| try alloc.dupe(ast.Label, &.{label}) else &.{},
        });
    }

    pub fn reportExpected(self: *Parser, message: []const u8, opts: ReportOptions) Error!void {
        @branchHint(.cold);
        const expected_message = try self.fmt("{s}, but found {s}", .{
            message,
            try self.describeToken(self.current_token),
        });
        try self.report(self.current_token.span, expected_message, opts);
    }

    pub fn fail(
        self: *Parser,
        span: ast.Span,
        message: []const u8,
        opts: ReportOptions,
    ) Error!?ast.NodeIndex {
        @branchHint(.cold);
        try self.report(span, message, opts);
        return null;
    }

    pub fn failExpected(
        self: *Parser,
        message: []const u8,
        opts: ReportOptions,
    ) Error!?ast.NodeIndex {
        @branchHint(.cold);
        try self.reportExpected(message, opts);
        return null;
    }

    pub noinline fn reportLexicalError(self: *Parser, lex_err: lexer.LexicalError) Error!void {
        @branchHint(.cold);
        const cursor = self.lexer.cursor;
        std.debug.assert(cursor <= self.source.len);
        const width: u32 = if (cursor == self.source.len)
            0
        else if (util.Utf.codePointAt(self.source, cursor)) |code_point|
            code_point.len
        else |_|
            1;
        try self.report(
            .{ .start = cursor, .end = cursor + width },
            lexer.getLexicalErrorMessage(lex_err),
            .{ .help = lexer.getLexicalErrorHelp(lex_err) },
        );
    }

    pub fn describeToken(self: *Parser, token: Token) Error![]const u8 {
        @branchHint(.cold);
        if (token.tag == .eof) return "end of file";
        const text = token.tag.toString() orelse token.text(self.source);
        if (text.len > 0 and (text[0] == '\'' or text[0] == '"' or text[0] == '`')) return text;
        return self.fmt("'{s}'", .{text});
    }

    pub fn fmt(self: *Parser, comptime format: []const u8, args: anytype) Error![]u8 {
        @branchHint(.cold);
        return self.allocator().print(format, args);
    }

    pub fn recover(self: *Parser, terminator: ?TokenTag) Error!void {
        while (!self.at(.eof)) {
            // a failed rescan leaves the lexer inside the current token, which is then stale
            if (self.current_token.span.end <= self.lexer.cursor) {
                try self.commitToken(self.current_token);
            }
            self.current_token = try self.recoverNextToken();
            if (self.current_token.tag == terminator) break;
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

    fn stripParenthesizedNodes(tree: *ast.Tree) void {
        const datas = tree.nodes.items(.data);
        const spans = tree.nodes.items(.span);
        for (0..datas.len) |i| {
            var inner: u32 = @intCast(i);
            while (true) {
                inner = switch (datas[inner]) {
                    .parenthesized_expression => |p| @backingInt(p.expression),
                    .ts_parenthesized_type => |p| @backingInt(p.type_annotation),
                    else => break,
                };
            }
            if (inner == i) continue;
            datas[i] = datas[inner];
            spans[i] = spans[inner];
        }
    }

    fn ensureCapacity(self: *Parser) Error!void {
        std.debug.assert(self.tree.nodes.capacity == 0);
        const alloc = self.allocator();
        const source_len = self.source.len;
        const is_small = source_len < 512_000;
        const estimated_nodes = if (is_small) @max(256, source_len / 2) else source_len / 4;
        const estimated_extra = if (is_small) source_len / 6 else source_len / 12;

        try self.tree.nodes.ensureTotalCapacity(alloc, estimated_nodes);
        try self.tree.extras.ensureTotalCapacity(alloc, estimated_extra);
        try self.scratch.ensureTotalCapacity(alloc, 1024);
        try self.scratch_side.ensureTotalCapacity(alloc, 128);
        if (self.options.tokens) {
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
    extras_len: usize,
    diagnostics_len: usize,
    tokens_len: usize,

    context: Context,
    ts_context: TsContext,
    state: ParserState,
};

/// Parses JavaScript or TypeScript source into a `Tree`. Call `deinit()` to free it.
pub fn parse(
    child_allocator: std.mem.Allocator,
    source: []const u8,
    options: Options,
) std.mem.Allocator.Error!ast.Tree {
    if (source.len >= std.math.maxInt(u32)) return error.OutOfMemory;
    var parser: Parser = .{
        .tree = ast.Tree.init(child_allocator, source),
        .source = source,
        .options = options,
        .lexer = undefined,
        .current_token = Token.eof(0),
    };
    parser.tree.source_type = options.source_type;
    parser.tree.lang = options.lang;
    return parser.parse() catch |err| switch (err) {
        error.OutOfMemory => error.OutOfMemory,
        error.NestedTooDeeply => nestedTooDeeply(
            child_allocator,
            source,
            options,
            parser.current_token.span,
        ),
    };
}

fn nestedTooDeeply(
    child_allocator: std.mem.Allocator,
    source: []const u8,
    options: Options,
    span: ast.Span,
) std.mem.Allocator.Error!ast.Tree {
    const end: u32 = @intCast(source.len);
    var tree: ast.Tree = .init(child_allocator, source);
    errdefer tree.deinit();
    tree.source_type = options.source_type;
    tree.lang = options.lang;
    try tree.addDiagnostic(.{
        .message = "Code is nested too deeply to parse",
        .span = span,
        .help = "Split the code into smaller functions or expressions",
    });
    tree.root = try tree.addNode(.{ .program = .{
        .source_type = if (tree.source_type == .module) .module else .script,
        .body = .empty,
    } }, .{ .start = 0, .end = end });
    if (options.tokens) tree.tokens = try tree.allocator().dupe(Token, &.{.eof(end)});
    return tree;
}
