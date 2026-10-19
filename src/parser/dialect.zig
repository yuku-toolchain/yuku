const std = @import("std");
const ast = @import("ast.zig");
const core = @import("parser.zig");
const codegen = @import("codegen/printer.zig");
const statements = @import("syntax/statements.zig");
const expressions = @import("syntax/expressions.zig");
const patterns = @import("syntax/patterns.zig");
const ts = @import("syntax/ts/types.zig");
const jsx = @import("syntax/jsx/root.zig");

const Bound = @import("dialect");

pub const Error = core.Error;

pub const Position = enum {
    statement,
    expression,
    operator,
    function_body,
    jsx_child,
    jsx_attribute,
    jsx_element_name,
    module_specifier,
};

pub const Claim = union(enum) {
    /// Not the dialect's syntax, with nothing consumed.
    decline,
    /// The parsed node, with the current token past it.
    node: ast.NodeIndex,
    /// A diagnostic was reported.
    failed,
    /// At `operator`, the expression ends before the current token.
    end,
};

/// What a dialect node kind is, to JavaScript and to semantic analysis.
pub const Shape = struct {
    /// The `type` the node decodes to in JavaScript.
    type: []const u8,
    /// Names of its children in order, one ending in `[]` taking those left over.
    fields: []const []const u8 = &.{},
    /// Properties every node of the kind decodes with, as a JSON object.
    properties: []const u8 = "{}",
    block_scope: bool = false,
    /// A binding identifier inside the node declares a `let`-like binding.
    lexical_bindings: bool = false,
    /// Bindings inside declare catch parameters, which a `var` inside may redeclare while
    /// every parameter is an identifier, as in `catch (e) { var e }`.
    catch_bindings: bool = false,
    /// The node is a loop, which `break` and `continue` inside end or skip.
    iteration: bool = false,
};

/// A kind of file the dialect parses, listed in its `files`.
pub const File = struct {
    /// The extension of the file's path, such as ".tsrx".
    extension: []const u8,
    /// The standard language the dialect extends in these files.
    lang: ast.Lang,
};

/// The dialect's `Node` enum.
pub const Kind = if (@hasDecl(Bound, "Node")) Bound.Node else enum(u16) { _ };

pub const bound = @typeInfo(Bound).@"struct".decl_names.len > 0;
pub const prints = @hasDecl(Bound, "print");

comptime {
    if (bound and (!@hasDecl(Bound, "files") or Bound.files.len == 0)) {
        @compileError("a dialect lists the `files` it parses");
    }
}

pub inline fn has(comptime position: Position) bool {
    return @hasDecl(Bound, @tagName(position));
}

// null when the dialect declines, a null node after a diagnostic
pub inline fn at(
    comptime position: Position,
    inner: *core.Parser,
    args: anytype,
) Error!??ast.NodeIndex {
    comptime std.debug.assert(position != .operator);
    return switch (try claim(position, inner, args)) {
        .decline => null,
        .node => |node| node,
        .failed => @as(?ast.NodeIndex, null),
        .end => unreachable,
    };
}

pub inline fn claim(comptime position: Position, inner: *core.Parser, args: anytype) Error!Claim {
    comptime std.debug.assert(has(position));
    if (!inner.dialect) return .decline;

    const token_start = inner.current_token.span.start;
    const nodes_len = inner.tree.nodes.len;
    var parser: Parser = .{ .inner = inner };
    const result = try @call(.auto, @field(Bound, @tagName(position)), .{&parser} ++ args);
    switch (result) {
        .decline => {
            std.debug.assert(inner.current_token.span.start == token_start);
            std.debug.assert(inner.tree.nodes.len == nodes_len);
        },
        .node => |node| std.debug.assert(@backingInt(node) >= nodes_len),
        .failed => std.debug.assert(inner.diagnostics.items.len > 0),
        .end => std.debug.assert(position == .operator),
    }
    return result;
}

pub fn shape(kind: Kind) Shape {
    if (comptime !@hasDecl(Bound, "Node")) return .{ .type = "" };
    return kind.shape();
}

pub const description: []const u8 = if (bound) descriptionJson() else "";

fn descriptionJson() []const u8 {
    var files: []const u8 = "";
    for (Bound.files, 0..) |file, i| {
        files = files ++ (if (i == 0) "" else ",") ++ std.fmt.comptimePrint(
            "{{\"extension\":\"{s}\",\"lang\":\"{t}\"}}",
            .{ file.extension, file.lang },
        );
    }
    const shapes = if (@hasDecl(Bound, "Node")) shapesJson() else "[]";
    return "{\"files\":[" ++ files ++ "],\"shapes\":" ++ shapes ++ "}";
}

fn shapesJson() []const u8 {
    var json: []const u8 = "[";
    for (std.enums.values(Bound.Node), 0..) |node, i| {
        if (@backingInt(node) != i) @compileError("a dialect's Node kinds must count up from 0");
        const node_shape = Bound.Node.shape(node);
        const properties = node_shape.properties;
        if (!std.mem.startsWith(u8, properties, "{") or !std.mem.endsWith(u8, properties, "}")) {
            @compileError("a dialect node shape's properties must be a JSON object");
        }
        const list = listField(node_shape.fields);
        var names: []const u8 = "";
        for (node_shape.fields, 0..) |field, j| {
            if (list != null and j > list.? and std.mem.endsWith(u8, field, "[]")) {
                @compileError("a dialect node shape takes one list field");
            }
            const separator = if (j == 0) "" else ",";
            names = names ++ separator ++ "\"" ++ std.mem.trimEnd(u8, field, "[]") ++ "\"";
        }
        json = json ++ (if (i == 0) "" else ",") ++ std.fmt.comptimePrint(
            "{{\"type\":\"{s}\",\"fields\":[{s}],\"list\":{d},\"properties\":{s}}}",
            .{ node_shape.type, names, if (list) |index| @as(i32, index) else -1, properties },
        );
    }
    return json ++ "]";
}

fn listField(fields: []const []const u8) ?usize {
    for (fields, 0..) |field, i| if (std.mem.endsWith(u8, field, "[]")) return i;
    return null;
}

pub fn print(printer: *Printer, kind: Kind, children: []const ast.NodeIndex) Error!void {
    comptime std.debug.assert(prints);
    return Bound.print(printer, kind, children);
}

/// The parser as a dialect drives it, with positions applying inside every production.
pub const Parser = struct {
    inner: *core.Parser,

    pub inline fn tree(self: *const Parser) *ast.Tree {
        return &self.inner.tree;
    }

    pub inline fn source(self: *const Parser) []const u8 {
        return self.inner.source;
    }

    pub inline fn token(self: *const Parser) ast.Token {
        return self.inner.current_token;
    }

    /// Lexes the token at or after `offset` in the current mode, without moving or reporting.
    pub fn tokenAt(self: *const Parser, offset: u32) ast.Token {
        std.debug.assert(offset <= self.inner.source.len);
        var peek = self.inner.beginPeek();
        defer peek.end();
        self.inner.lexer.rewindTo(offset);
        const next = peek.next();
        std.debug.assert(next.span.start >= offset);
        return next;
    }

    /// False after a lexical error was reported.
    pub inline fn advance(self: *const Parser) Error!bool {
        return try self.inner.advance() != null;
    }

    pub inline fn expect(
        self: *const Parser,
        comptime tag: ast.TokenTag,
        message: []const u8,
    ) Error!bool {
        return self.inner.expect(tag, message, null);
    }

    /// Makes the token at `offset` current, as a JSX child that parses tokens needs.
    pub fn seek(self: *const Parser, offset: u32) Error!bool {
        std.debug.assert(offset <= self.inner.source.len);
        std.debug.assert(offset >= self.inner.current_token.span.start);
        self.inner.lexer.rewindTo(offset);
        const empty: ast.Token = .{ .span = .{ .start = offset, .end = offset }, .tag = .jsx_text };
        return try self.inner.advanceWithRescannedToken(empty) != null;
    }

    /// Ends a statement at `end`, with a `;` or where automatic semicolon insertion puts one.
    pub inline fn semicolon(self: *const Parser, end: u32) Error!?u32 {
        return self.inner.eatSemicolon(end);
    }

    pub inline fn checkpoint(self: *const Parser) core.Checkpoint {
        return self.inner.checkpoint();
    }

    pub inline fn rewind(self: *const Parser, to: core.Checkpoint) void {
        self.inner.rewind(to);
    }

    pub inline fn report(self: *const Parser, span: ast.Span, message: []const u8) Error!void {
        try self.inner.report(span, message, .{});
    }

    pub inline fn parseStatement(self: *const Parser) Error!?ast.NodeIndex {
        return statements.parseStatement(self.inner, .{});
    }

    /// Parses an expression whose operators bind at `precedence` or tighter.
    pub inline fn parseExpression(self: *const Parser, precedence: u8) Error!?ast.NodeIndex {
        return expressions.parseExpression(self.inner, precedence, .{});
    }

    /// Parses an identifier, array, or object pattern, with its type annotation in TypeScript.
    pub fn parseBindingPattern(self: *const Parser) Error!?ast.NodeIndex {
        const pattern = try patterns.parseBindingPattern(self.inner) orelse return null;
        if (!self.inner.tree.isTs() or self.inner.current_token.tag != .colon) return pattern;
        return ts.parsePatternTypeAnnotation(self.inner, pattern);
    }

    /// Parses the `{ expression }` at the current `{` inside a tag.
    pub inline fn parseJsxExpressionContainer(self: *const Parser) Error!?ast.NodeIndex {
        return jsx.parseJsxExpressionContainer(self.inner);
    }

    /// Parses the element or fragment at the current `<`, leaving its last `>` current.
    pub inline fn parseJsxElement(self: *const Parser) Error!?ast.NodeIndex {
        std.debug.assert(self.inner.current_token.tag == .less_than);
        return jsx.parseJsxElement(self.inner, .child);
    }

    pub fn addNode(
        self: *const Parser,
        kind: Kind,
        children: []const ast.NodeIndex,
        span: ast.Span,
    ) Error!ast.NodeIndex {
        const fields = kind.shape().fields;
        if (listField(fields) == null) {
            std.debug.assert(children.len == fields.len);
        } else {
            std.debug.assert(children.len + 1 >= fields.len);
        }
        for (children) |child| {
            if (child == .null) continue;
            std.debug.assert(self.tree().span(child).start >= span.start);
            std.debug.assert(self.tree().span(child).end <= span.end);
        }
        const data: ast.Dialect = .{ .kind = kind, .children = try self.tree().addExtra(children) };
        return self.tree().addNode(.{ .dialect = data }, span);
    }

    /// Records a comment the dialect reads where the standard grammar reads none, as in text.
    pub fn addComment(self: *const Parser, @"type": ast.Comment.Type, span: ast.Span) Error!void {
        std.debug.assert(self.inner.source[span.start] == '/');
        const comments = self.inner.lexer.comments.items;
        if (comments.len > 0) std.debug.assert(comments[comments.len - 1].span.end <= span.start);
        try self.inner.lexer.recordComment(@"type", span.start, span.end);
    }

    /// Adds JSX text with its character references decoded.
    pub fn addJsxText(self: *const Parser, span: ast.Span) Error!ast.NodeIndex {
        std.debug.assert(span.start < span.end);
        return self.tree().addNode(.{ .jsx_text = .{
            .value = try self.inner.jsxValue(span.start, span.end),
            .raw = self.tree().sourceSlice(span.start, span.end),
        } }, span);
    }

    /// Starts a child list on scratch space, to reset before an enclosing list continues.
    pub inline fn list(self: *const Parser) List {
        return .{ .inner = self.inner, .start = self.inner.scratch_statements.begin() };
    }
};

pub const List = struct {
    inner: *core.Parser,
    start: usize,

    pub inline fn append(self: List, node: ast.NodeIndex) Error!void {
        std.debug.assert(self.inner.scratch_statements.items.items.len >= self.start);
        try self.inner.scratch_statements.append(self.inner.allocator(), node);
    }

    pub inline fn items(self: List) []const ast.NodeIndex {
        return self.inner.scratch_statements.items.items[self.start..];
    }

    pub inline fn reset(self: List) void {
        self.inner.scratch_statements.reset(self.start);
    }
};

pub const Printer = codegen.DialectPrinter;

fn Hook(comptime position: Position) type {
    return switch (position) {
        .operator => fn (*Parser, ast.NodeIndex, u8) Error!Claim,
        .jsx_child => fn (*Parser, u32, ast.NodeIndex) Error!Claim,
        else => fn (*Parser) Error!Claim,
    };
}

comptime {
    for (@typeInfo(Position).@"enum".field_names) |name| {
        const Expected = Hook(@field(Position, name));
        if (@hasDecl(Bound, name) and @TypeOf(@field(Bound, name)) != Expected) {
            @compileError("dialect position '" ++ name ++ "' must be a " ++ @typeName(Expected));
        }
    }

    // a misspelled position must not compile to one that never runs
    for (@typeInfo(Bound).@"struct".decl_names) |name| {
        const info = @typeInfo(@TypeOf(@field(Bound, name)));
        if (info != .@"fn" or info.@"fn".param_types.len == 0) continue;
        if (info.@"fn".param_types[0] != *Parser) continue;
        if (std.meta.stringToEnum(Position, name) == null) {
            @compileError("dialect function '" ++ name ++ "' takes a *Parser but is no position");
        }
    }

    if (@hasDecl(Bound, "Node")) {
        if (!@hasDecl(Bound.Node, "shape") or @TypeOf(Bound.Node.shape) != fn (Bound.Node) Shape) {
            @compileError("a dialect's Node must declare fn shape(Node) Shape");
        }
    }

    if (prints and @TypeOf(Bound.print) != fn (*Printer, Kind, []const ast.NodeIndex) Error!void) {
        @compileError("a dialect's print must be a fn (*Printer, Node, []const ast.NodeIndex)");
    }
}
