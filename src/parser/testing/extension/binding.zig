//! Reference `parser_extension` binding that implements every extension point.

const std = @import("std");

/// Points reached so far, deduplicated.
pub var visited: [64][]const u8 = undefined;
pub var visited_len: u32 = 0;

pub fn at_expression(comptime R: type, parser: anytype) R {
    visit("at_expression");
    return decline(R, parser);
}

/// `@typed x: T;` declares `let x: T`, typed by the parser's own annotation.
pub fn at_statement(comptime R: type, parser: anytype) R {
    visit("at_statement");
    const start = parser.current_token.span.start;
    if (!std.mem.startsWith(u8, parser.source[parser.current_token.span.end..], "typed ")) return null;
    try parser.advance() orelse return .failed;
    try parser.advance() orelse return .failed;

    const name = parser.current_token.span;
    const id = try parser.tree.addNode(.{ .binding_identifier = .{
        .name = parser.tree.sourceSlice(name.start, name.end),
    } }, name);
    try parser.advance() orelse return .failed;
    _ = try parser.parseTypeAnnotation(id) orelse return .failed;

    const declarator = try parser.tree.addNode(
        .{ .variable_declarator = .{ .id = id, .init = .null } },
        parser.tree.span(id),
    );
    const end = try parser.eatSemicolon(parser.tree.span(id).end) orelse return .failed;
    return .of(try parser.tree.addNode(.{ .variable_declaration = .{
        .kind = .let,
        .declarators = try parser.tree.addExtra(&.{declarator}),
    } }, .{ .start = start, .end = end }));
}

pub fn binding_pattern(comptime R: type, parser: anytype) R {
    visit("binding_pattern");
    return decline(R, parser);
}

/// `%` reports, then fails.
pub fn expression_prefix(comptime R: type, parser: anytype) R {
    visit("expression_prefix");
    if (parser.current_token.tag != .percent) return null;

    try parser.report(parser.current_token.span, "'%' is not a prefix operator", .{});
    return .failed;
}

/// Eats an `@tail` clause and declines, which only this point may do.
pub fn for_of_tail(comptime R: type, parser: anytype, head: anytype) R {
    visit("for_of_tail");
    _ = head;
    if (parser.current_token.tag != .at) return null;

    try parser.advance() orelse return .failed;
    try parser.advance() orelse return .failed;
    return null;
}

/// A `%` body reports, then fails.
pub fn function_body(comptime R: type, parser: anytype) R {
    visit("function_body");
    if (parser.current_token.tag != .percent) return null;

    try parser.report(parser.current_token.span, "'%' is not a body", .{});
    return .failed;
}

/// `{}` reports, then fails.
pub fn jsx_attribute(comptime R: type, parser: anytype) R {
    visit("jsx_attribute");
    std.debug.assert(parser.current_token.tag == .left_brace);
    const close = parser.current_token.span.end;
    if (close >= parser.source.len or parser.source[close] != '}') return null;

    try parser.report(parser.current_token.span, "An attribute cannot be empty", .{});
    return .failed;
}

pub fn jsx_child(comptime R: type, parser: anytype) R {
    visit("jsx_child");
    return decline(R, parser);
}

pub fn jsx_element_name(comptime R: type, parser: anytype) R {
    visit("jsx_element_name");
    return decline(R, parser);
}

pub fn jsx_element_tail(comptime R: type, parser: anytype, opening: anytype, context: anytype) R {
    visit("jsx_element_tail");
    return decline(R, .{ parser, opening, context });
}

pub fn jsx_fragment_tail(comptime R: type, parser: anytype, opening: anytype) R {
    visit("jsx_fragment_tail");
    return decline(R, .{ parser, opening });
}

pub fn jsx_statement(comptime R: type, parser: anytype) R {
    visit("jsx_statement");
    std.debug.assert(parser.current_token.tag == .less_than);
    return null;
}

/// `#c#` in text is an empty `{}` child.
pub fn jsx_text_child(comptime R: type, parser: anytype, span: anytype) R {
    visit("jsx_text_child");
    const text = parser.source[span.start..span.end];
    const open = std.mem.indexOfScalar(u8, text, '#') orelse return null;
    const close = std.mem.indexOfScalarPos(u8, text, open + 1, '#') orelse return null;

    const child: @TypeOf(span) = .{
        .start = span.start + @as(u32, @intCast(open)),
        .end = span.start + @as(u32, @intCast(close)) + 1,
    };
    const empty = try parser.tree.addNode(.{ .jsx_empty_expression = .{} }, child);
    return .of(try parser.tree.addNode(
        .{ .jsx_expression_container = .{ .expression = empty } },
        child,
    ));
}

/// Accepts a bare identifier as a specifier.
pub fn module_specifier(comptime R: type, parser: anytype) R {
    visit("module_specifier");
    if (parser.current_token.tag != .identifier) return null;

    const span = parser.current_token.span;
    try parser.advance() orelse return .failed;

    const text = parser.tree.sourceSlice(span.start, span.end);
    return .of(try parser.tree.addNode(
        .{ .string_literal = .{ .value = text, .raw = text } },
        span,
    ));
}

/// `!!text` interns without the sigil.
pub fn jsx_text_value(comptime R: type, parser: anytype, span: anytype) R {
    visit("jsx_text_value");
    const text = parser.source[span.start..span.end];
    if (!std.mem.startsWith(u8, text, "!!")) return null;

    return try parser.tree.addString(text[2..]);
}

pub fn binding_start(tag: anytype) ?bool {
    visit("binding_start");
    return decline(?bool, tag);
}

/// A `%` starts a body, which `function_body` reads.
pub fn function_has_body(parser: anytype) ?bool {
    visit("function_has_body");
    if (parser.current_token.tag != .percent) return null;

    return true;
}

/// A line-leading `<p` starts a statement.
pub fn jsx_starts_statement(parser: anytype) ?bool {
    visit("jsx_starts_statement");
    std.debug.assert(parser.current_token.tag == .less_than);
    const after = parser.current_token.span.end;
    if (after >= parser.source.len or parser.source[after] != 'p') return null;

    return true;
}

/// `</_>` closes any element.
pub fn jsx_names_match(parser: anytype, opening: anytype, closing: anytype) ?bool {
    visit("jsx_names_match");
    _ = opening;
    const span = parser.tree.span(closing);
    if (!std.mem.eql(u8, parser.source[span.start..span.end], "_")) return null;

    return true;
}

pub fn jsx_text_boundary(source: []const u8, cursor: u32) ?bool {
    visit("jsx_text_boundary");
    if (source[cursor] != '<') return null;

    return true;
}

pub fn jsx_element_name_check(comptime R: type, parser: anytype, name: anytype) R {
    visit("jsx_element_name_check");
    const span = parser.tree.span(name);
    if (std.mem.eql(u8, parser.source[span.start..span.end], "Deprecated")) {
        try parser.report(span, "<Deprecated> is deprecated", .{ .severity = .warning });
    }
}

fn decline(comptime R: type, unused: anytype) R {
    _ = unused;
    return null;
}

// hooks name themselves since `@src().fn_name` mangles generic instantiations
fn visit(comptime point: []const u8) void {
    comptime std.debug.assert(@hasDecl(@This(), point));

    for (visited[0..visited_len]) |seen| {
        if (std.mem.eql(u8, seen, point)) return;
    }
    std.debug.assert(visited_len < visited.len);
    visited[visited_len] = point;
    visited_len += 1;
}
