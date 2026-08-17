//! reference `parser_extension` binding: implements all 20 extension points, counts every
//! call, and takes a real handled path on marker syntax stock JS/TS/JSX rejects.

const std = @import("std");

pub const Hook = enum {
    binding_pattern,
    can_start_binding,
    expression_at_code_block,
    expression_at_control_flow,
    for_of_tail,
    function_body,
    function_body_starts,
    jsx_child_at_code_block,
    jsx_child_at_control_flow,
    jsx_element_after_open,
    jsx_element_name,
    jsx_fragment_after_open,
    jsx_names_match,
    jsx_text_boundary,
    jsx_text_value,
    lazy_assignment_pattern,
    module_specifier,
    statement_at_code_block,
    statement_at_control_flow,
    validate_jsx_element_name,
};

pub var counts = [_]u32{0} ** @typeInfo(Hook).@"enum".fields.len;

fn hit(comptime hook: Hook) void {
    counts[@intFromEnum(hook)] += 1;
}

fn decline(comptime R: type, unused: anytype, comptime hook: Hook) R {
    _ = unused;
    hit(hook);
    return null;
}

/// the inner optional of a call site's `Error!??ast.NodeIndex`; a bare `null` is the outer one.
fn Node(comptime R: type) type {
    return @typeInfo(@typeInfo(R).error_union.payload).optional.child;
}

pub fn binding_pattern(comptime R: type, parser: anytype) R {
    return decline(R, parser, .binding_pattern);
}
pub fn can_start_binding(tag: anytype) ?bool {
    return decline(?bool, tag, .can_start_binding);
}
pub fn expression_at_code_block(comptime R: type, parser: anytype) R {
    return decline(R, parser, .expression_at_code_block);
}
pub fn expression_at_control_flow(comptime R: type, parser: anytype) R {
    return decline(R, parser, .expression_at_control_flow);
}

/// eats `@tail` and hands the statement back with `)` current: the one point where
/// consuming and then declining is legal.
pub fn for_of_tail(comptime R: type, parser: anytype, args: anytype) R {
    hit(.for_of_tail);
    _ = args;
    if (parser.current_token.tag != .at) return null;
    try parser.advance() orelse return null;
    try parser.advance() orelse return null;
    return null;
}

pub fn function_body(comptime R: type, parser: anytype) R {
    return decline(R, parser, .function_body);
}
pub fn function_body_starts(parser: anytype) ?bool {
    return decline(?bool, parser, .function_body_starts);
}
pub fn jsx_child_at_code_block(comptime R: type, parser: anytype) R {
    return decline(R, parser, .jsx_child_at_code_block);
}
pub fn jsx_child_at_control_flow(comptime R: type, parser: anytype) R {
    return decline(R, parser, .jsx_child_at_control_flow);
}
pub fn jsx_element_after_open(comptime R: type, parser: anytype, opening: anytype, context: anytype) R {
    return decline(R, .{ parser, opening, context }, .jsx_element_after_open);
}
pub fn jsx_element_name(comptime R: type, parser: anytype) R {
    return decline(R, parser, .jsx_element_name);
}
pub fn jsx_fragment_after_open(comptime R: type, parser: anytype, opening: anytype) R {
    return decline(R, .{ parser, opening }, .jsx_fragment_after_open);
}

/// `</_>` closes any element.
pub fn jsx_names_match(parser: anytype, a: anytype, b: anytype) ?bool {
    hit(.jsx_names_match);
    _ = a;
    const name = parser.tree.span(b);
    if (!std.mem.eql(u8, parser.source[name.start..name.end], "_")) return null;
    return true;
}

pub fn jsx_text_boundary(source: []const u8, cursor: u32) ?bool {
    hit(.jsx_text_boundary);
    if (source[cursor] != '<') return null;
    return true;
}

/// `!!text` is re-interned without the sigil.
pub fn jsx_text_value(comptime R: type, parser: anytype, span: anytype) R {
    hit(.jsx_text_value);
    const text = parser.source[span.start..span.end];
    if (!std.mem.startsWith(u8, text, "!!")) return null;
    return try parser.tree.addString(text[2..]);
}

/// handled-and-failed: reports, then returns `some(null)`.
pub fn lazy_assignment_pattern(comptime R: type, parser: anytype) R {
    hit(.lazy_assignment_pattern);
    if (parser.current_token.tag != .percent) return null;
    try parser.report(parser.current_token.span, "'%' is not a prefix operator", .{});
    return @as(Node(R), null);
}

/// accepts a bare identifier where the grammar wanted a string specifier.
pub fn module_specifier(comptime R: type, parser: anytype) R {
    hit(.module_specifier);
    if (parser.current_token.tag != .identifier) return null;
    const span = parser.current_token.span;
    try parser.advance() orelse return null;
    const text = parser.tree.sourceSlice(span.start, span.end);
    return @as(Node(R), try parser.tree.addNode(.{ .string_literal = .{ .value = text, .raw = text } }, span));
}

pub fn statement_at_code_block(comptime R: type, parser: anytype) R {
    return decline(R, parser, .statement_at_code_block);
}
pub fn statement_at_control_flow(comptime R: type, parser: anytype) R {
    return decline(R, parser, .statement_at_control_flow);
}

/// advisory: `R` is `Error!void`, so this can report but cannot reject.
pub fn validate_jsx_element_name(comptime R: type, parser: anytype, name: anytype) R {
    hit(.validate_jsx_element_name);
    const span = parser.tree.span(name);
    if (std.mem.eql(u8, parser.source[span.start..span.end], "Deprecated"))
        try parser.report(span, "<Deprecated> is deprecated", .{ .severity = .warning });
}
