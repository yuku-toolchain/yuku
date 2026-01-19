const std = @import("std");
const token = @import("token.zig");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");

const statements = @import("syntax/statements.zig");

pub const Severity = enum {
    @"error",
    warning,
    hint,
    info,

    pub fn toString(self: Severity) []const u8 {
        return switch (self) {
            .@"error" => "error",
            .warning => "warning",
            .hint => "hint",
            .info => "info",
        };
    }
};

pub const Label = struct {
    span: ast.Span,
    message: []const u8,
};

pub const Diagnostic = struct {
    severity: Severity = .@"error",
    message: []const u8,
    span: ast.Span,
    help: ?[]const u8 = null,
    labels: []const Label = &.{},
};

pub const SourceType = enum { script, module };
pub const Lang = enum { js, ts, jsx, tsx, dts };

pub const Options = struct {
    source_type: SourceType = .module,
    lang: Lang = .js,
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
    /// Diagnostics (errors, warnings, etc.) encountered during parsing
    diagnostics: std.ArrayList(Diagnostic),
    /// Comments collected in source code
    comments: std.ArrayList(ast.Comment),
    /// Arena allocator owning all the memory
    arena: std.heap.ArenaAllocator,
    /// Source type (script or module)
    source_type: SourceType,
    /// Language variant (js, ts, jsx, tsx, dts)
    lang: Lang,

    /// Returns true if the parse tree contains any errors.
    pub inline fn hasErrors(self: ParseTree) bool {
        for (self.diagnostics.items) |d| {
            if (d.severity == .@"error") return true;
        }
        return false;
    }

    /// Returns true if the parse tree contains any diagnostics (errors, warnings, etc.).
    pub inline fn hasDiagnostics(self: ParseTree) bool {
        return self.diagnostics.items.len > 0;
    }

    /// Frees all memory allocated by this parse tree.
    pub fn deinit(self: *const ParseTree) void {
        self.arena.deinit();
    }

    /// Gets the data for the node at the given index.
    pub inline fn getData(self: *const ParseTree, index: ast.NodeIndex) ast.NodeData {
        return self.nodes.items(.data)[index];
    }

    /// Gets the span for the node at the given index.
    pub inline fn getSpan(self: *const ParseTree, index: ast.NodeIndex) ast.Span {
        return self.nodes.items(.span)[index];
    }

    /// Gets the extra node indices for the given range.
    pub inline fn getExtra(self: *const ParseTree, range: ast.IndexRange) []const ast.NodeIndex {
        return self.extra.items[range.start..][0..range.len];
    }

    /// Gets a slice of the source text at the given position.
    pub inline fn getSourceText(self: *const ParseTree, start: u32, len: u16) []const u8 {
        return self.source[start..][0..len];
    }
};

const ParserContext = struct {
    in_async: bool = false,
    in_generator: bool = false,
    allow_in: bool = true,
    in_function: bool = false,
    /// Whether we're parsing a single statement.
    /// example:
    /// if(test) 30;
    ///          ~~
    ///           ^ this is in a single statement context
    in_single_statement_context: bool = false,
};

const ParserState = struct {
    /// tracks if the cover (array or object) we are parsing has a trailing comma
    /// value is the start index of the cover
    cover_has_trailing_comma: ?u32 = null,
    /// tracks if CoverInitializedName ({a = 1}) was parsed in current cover context.
    cover_has_init_name: bool = false,
    /// tracks if we're still in the directive prologue of a function/script body.
    in_directive_prologue: bool = true,
    /// current scope identified by depth
    current_scope_id: u32 = 0,
};

pub const Error = error{OutOfMemory};

pub const Parser = struct {
    source: []const u8,
    lexer: lexer.Lexer,
    arena: std.heap.ArenaAllocator,
    diagnostics: std.ArrayList(Diagnostic) = .empty,
    nodes: std.MultiArrayList(ast.Node) = .empty,
    extra: std.ArrayList(ast.NodeIndex) = .empty,
    current_token: token.Token,
    next_token: ?token.Token = null,

    scratch_statements: ScratchBuffer = .{},
    scratch_cover: ScratchBuffer = .{},

    // multiple scratches to handle multiple extras at the same time
    scratch_a: ScratchBuffer = .{},
    scratch_b: ScratchBuffer = .{},
    //

    context: ParserContext = .{},
    state: ParserState = .{},

    strict_mode: bool,
    source_type: SourceType,
    lang: Lang,

    pub fn init(backing_allocator: std.mem.Allocator, source: []const u8, options: Options) Parser {
        return .{
            .source = source,
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
            .source_type = options.source_type,
            .lang = options.lang,
            .strict_mode = options.source_type == .module,
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
    pub fn parse(self: *Parser) Error!ParseTree {
        // init lexer
        self.lexer = try lexer.Lexer.init(self.source, self.allocator(), self.source_type, self.strict_mode);

        // let's begin
        try self.advance() orelse {
            self.current_token = token.Token.eof(0);
        };

        errdefer {
            self.arena.deinit();
        }

        try self.ensureCapacity();

        const body = try self.parseBody(null);

        const end = self.current_token.span.end;

        const program = try self.addNode(
            .{
                .program = .{
                    .source_type = if (self.source_type == .module) .module else .script,
                    .body = body,
                },
            },
            .{ .start = 0, .end = end },
        );

        const tree = ParseTree{
            .program = program,
            .source = self.source,
            .nodes = self.nodes,
            .extra = self.extra,
            .diagnostics = self.diagnostics,
            .comments = self.lexer.comments,
            .arena = self.arena,
            .source_type = self.source_type,
            .lang = self.lang,
        };

        return tree;
    }

    pub fn parseBody(self: *Parser, terminator: ?token.TokenType) Error!ast.IndexRange {
        const statements_checkpoint = self.scratch_statements.begin();
        defer self.scratch_statements.reset(statements_checkpoint);

        self.state.in_directive_prologue = true;
        self.state.current_scope_id += 1;

        while (!self.isAtBodyEnd(terminator)) {
            if (try statements.parseStatement(self, .{})) |statement| {
                try self.scratch_statements.append(self.allocator(), statement);
            } else {
                self.state.in_directive_prologue = false;
                try self.synchronize(terminator) orelse break;
            }
        }

        self.state.current_scope_id -= 1;

        return self.addExtra(self.scratch_statements.take(statements_checkpoint));
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
        // clear lookahead, so parser will fetch fresh next token with new mode
        self.clearLookAhead();
        self.lexer.state.mode = mode;
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

    pub fn advance(self: *Parser) Error!?void {
        const new_token = if (self.next_token) |tok| blk: {
            self.next_token = null;
            break :blk tok;
        } else try self.nextToken() orelse return null;

        self.current_token = new_token;
    }

    // lazy next token prefetching for lookahead.
    // if lookahead has already cached the next token, `advance` will use it,
    // so there's no extra cost for lookAhead.
    pub fn lookAhead(self: *Parser) Error!?token.Token {
        if (self.next_token) |tok| {
            return tok;
        }

        self.next_token = try self.nextToken() orelse return null;

        return self.next_token;
    }

    pub inline fn clearLookAhead(self: *Parser) void {
        self.next_token = null;
    }

    pub inline fn replaceTokenAndAdvance(self: *Parser, tok: token.Token) Error!?void {
        self.current_token = tok;
        try self.advance() orelse return null;
    }

    pub inline fn expect(self: *Parser, token_type: token.TokenType, message: []const u8, help: ?[]const u8) Error!bool {
        if (self.current_token.type == token_type) {
            try self.advance() orelse return false;
            return true;
        }

        try self.report(self.current_token.span, message, .{ .help = help });

        return false;
    }

    pub inline fn eatSemicolon(self: *Parser, end: u32) Error!?u32 {
        if (self.current_token.type == .semicolon) {
            const semicolon_end = self.current_token.span.end;
            try self.advance() orelse return null;
            return semicolon_end;
        } else {
            if (!self.canInsertSemicolon()) {
                try self.reportFmt(self.current_token.span, "Expected a semicolon or an implicit semicolon after a statement, but found '{s}'", .{self.describeToken(self.current_token)}, .{ .help = "Try inserting a semicolon here" });
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
    pub inline fn eatSemicolonLenient(self: *Parser, end: u32) Error!?u32 {
        if (self.current_token.type == .semicolon) {
            const semicolon_end = self.current_token.span.end;
            try self.advance() orelse return null;
            return semicolon_end;
        }
        return end;
    }

    /// https://tc39.es/ecma262/#sec-rules-of-automatic-semicolon-insertion
    pub inline fn canInsertSemicolon(self: *Parser) bool {
        const current_token = self.current_token;
        return current_token.type == .eof or current_token.has_line_terminator_before or current_token.type == .right_brace;
    }

    pub inline fn describeToken(_: *Parser, tok: token.Token) []const u8 {
        return if (tok.type == .eof) "end of file" else tok.lexeme;
    }

    pub const ReportOptions = struct {
        severity: Severity = .@"error",
        help: ?[]const u8 = null,
        labels: []const Label = &.{},
    };

    pub inline fn report(self: *Parser, span: ast.Span, message: []const u8, opts: ReportOptions) Error!void {
        try self.diagnostics.append(self.allocator(), .{
            .severity = opts.severity,
            .message = message,
            .span = span,
            .help = opts.help,
            .labels = opts.labels,
        });
    }

    pub inline fn reportFmt(self: *Parser, span: ast.Span, comptime format: []const u8, args: anytype, opts: ReportOptions) Error!void {
        const message = try std.fmt.allocPrint(self.allocator(), format, args);
        try self.report(span, message, opts);
    }

    pub inline fn label(_: *Parser, span: ast.Span, message: []const u8) Label {
        return .{ .span = span, .message = message };
    }

    pub fn makeLabels(self: *Parser, labels: []const Label) Error![]const Label {
        return try self.allocator().dupe(Label, labels);
    }

    pub fn formatMessage(self: *Parser, comptime format: []const u8, args: anytype) Error![]u8 {
        return try std.fmt.allocPrint(self.allocator(), format, args);
    }

    // returning null here means, break the top level statement parsing loop
    // otherwise continue
    fn synchronize(self: *Parser, terminator: ?token.TokenType) Error!?void {
        const errored_scope_id = self.state.current_scope_id;

        while (self.current_token.type != .eof) {
            // stop at the block terminator to avoid consuming the closing brace
            if (terminator) |t| {
                if (self.current_token.type == t) return;
            }

            if (self.state.current_scope_id <= errored_scope_id) {
                if (self.current_token.type == .semicolon or self.current_token.type == .right_brace) {
                    try self.advance() orelse return null;
                    return;
                }

                if (self.current_token.has_line_terminator_before) {
                    const can_start_statement = switch (self.current_token.type) {
                        .class, .function, .@"var", .@"for", .@"if", .@"while", .@"return", .let, .@"const", .@"try", .throw, .debugger, .@"break", .@"continue", .@"switch", .do, .with, .async, .@"export", .import, .left_brace => true,
                        else => false,
                    };

                    if (can_start_statement) {
                        return;
                    }
                }
            }

            try self.advance() orelse return null;
        }
    }

    fn ensureCapacity(self: *Parser) Error!void {
        if (self.nodes.capacity > 0) return;

        const alloc = self.allocator();

        const estimated_nodes = if (self.source.len < 10_000)
            @max(1024, (self.source.len * 3) / 4)
        else if (self.source.len < 100_000)
            self.source.len / 2
        else
            self.source.len / 3;

        const estimated_extra = estimated_nodes / 2;

        try self.nodes.ensureTotalCapacity(alloc, estimated_nodes);
        try self.extra.ensureTotalCapacity(alloc, estimated_extra);
        try self.diagnostics.ensureTotalCapacity(alloc, 32);
        try self.scratch_cover.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_statements.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_a.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_b.items.ensureTotalCapacity(alloc, 256);
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

    pub inline fn take(self: *ScratchBuffer, checkpoint: usize) []const ast.NodeIndex {
        const slice = self.items.items[checkpoint..];
        self.items.shrinkRetainingCapacity(checkpoint);
        return slice;
    }

    pub inline fn reset(self: *ScratchBuffer, checkpoint: usize) void {
        self.items.shrinkRetainingCapacity(checkpoint);
    }
};

/// Parse JavaScript/TypeScript source code into an AST.
/// Returns a ParseTree that must be freed with deinit().
pub fn parse(backing_allocator: std.mem.Allocator, source: []const u8, options: Options) Error!ParseTree {
    var parser = Parser.init(backing_allocator, source, options);
    return parser.parse();
}
