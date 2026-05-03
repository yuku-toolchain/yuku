const std = @import("std");
const ast = @import("../../../ast.zig");
const Parser = @import("../../../parser.zig").Parser;
const Error = @import("../../../parser.zig").Error;
const TokenTag = @import("../../../token.zig").TokenTag;

const literals = @import("../../literals.zig");
const core = @import("core.zig");

// Foo<T, U, V>
//    ^^^^^^^^^
pub inline fn parseTypeArguments(parser: *Parser) Error!ast.NodeIndex {
    return parseAngleList(parser, .arguments);
}

// `<>` after name in ref or typeof. newline before `<` keeps `typeof a` away from a generic fn type that follows
pub inline fn parseTypeArgumentsAfterEntityName(parser: *Parser) Error!ast.NodeIndex {
    if (!isAngleOpen(parser.current_token.tag) or parser.current_token.hasLineTerminatorBefore()) return .null;
    return parseAngleList(parser, .arguments);
}

// function f<T, U extends V>() {}
//           ^^^^^^^^^^^^^^^^
pub fn parseTypeParameters(parser: *Parser) Error!ast.NodeIndex {
    return parseAngleList(parser, .parameters);
}

const AngleListKind = enum {
    // `Foo<T, U>` at call or instantiation site
    arguments,
    // `<T, U extends V>` at declaration
    parameters,
};

// null when no opening `<`
fn parseAngleList(parser: *Parser, comptime kind: AngleListKind) Error!ast.NodeIndex {
    const start = try consumeAngleOpen(parser) orelse return .null;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag != .greater_than and parser.current_token.tag != .eof) {
        const elem = switch (kind) {
            .arguments => try core.parseType(parser) orelse return .null,
            .parameters => try parseTypeParameter(parser) orelse return .null,
        };
        try parser.scratch_a.append(parser.allocator(), elem);
        if (parser.current_token.tag != .comma) break;
        try parser.advance() orelse return .null;
    }

    const end = try consumeAngleClose(parser, kind) orelse return .null;
    const params = try parser.addExtraFromScratch(&parser.scratch_a, checkpoint);

    const data: ast.NodeData = switch (kind) {
        .arguments => .{ .ts_type_parameter_instantiation = .{ .params = params } },
        .parameters => .{ .ts_type_parameter_declaration = .{ .params = params } },
    };
    return try parser.tree.addNode(data, .{ .start = start, .end = end });
}

// `<` or fused `<<` for nested instantiations eg `Foo<<T>(x: T) => R>`
pub inline fn isAngleOpen(tag: TokenTag) bool {
    return tag == .less_than or tag == .left_shift;
}

// `<` pos or null. peel fused `<<`
fn consumeAngleOpen(parser: *Parser) Error!?u32 {
    const start = parser.current_token.span.start;
    switch (parser.current_token.tag) {
        .less_than => try parser.advance() orelse return null,
        .left_shift => {
            const lt = parser.lexer.reScanLessThan(start);
            try parser.advanceWithRescannedToken(lt) orelse return null;
        },
        else => return null,
    }
    return start;
}

// `>` end or error report. peel one `>` from `>>` `>>=` style
fn consumeAngleClose(parser: *Parser, comptime kind: AngleListKind) Error!?u32 {
    switch (parser.current_token.tag) {
        .greater_than => {
            const end = parser.current_token.span.end;
            try parser.advance() orelse return null;
            return end;
        },
        .right_shift, .unsigned_right_shift, .greater_than_equal, .right_shift_assign, .unsigned_right_shift_assign => {
            const gt = parser.lexer.reScanGreaterThan(parser.current_token.span.start);
            try parser.advanceWithRescannedToken(gt) orelse return null;
            return gt.span.end;
        },
        else => {
            try parser.reportExpected(
                parser.current_token.span,
                switch (kind) {
                    .arguments => "Expected '>' to close a type argument list",
                    .parameters => "Expected '>' to close a type parameter list",
                },
                .{ .help = "Each '<' in a type must be matched by a '>'" },
            );
            return null;
        },
    }
}

// const in out T extends U = V
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
fn parseTypeParameter(parser: *Parser) Error!?ast.NodeIndex {
    // want `const` then `in` then `out`, step tracks that
    var flags: struct { @"const": bool = false, in: bool = false, out: bool = false } = .{};
    var step: u8 = 0;
    var start: u32 = parser.current_token.span.start;
    var start_set = false;

    // word is a modifier only when an actual name follows, else `<out>` is a type param named out
    while (true) {
        const token = parser.current_token;
        const this_step: u8, const seen_ptr: *bool = switch (token.tag) {
            .@"const" => .{ 1, &flags.@"const" },
            .in => .{ 2, &flags.in },
            .out => .{ 3, &flags.out },
            else => break,
        };

        const next = parser.peekAhead() orelse break;
        if (!next.tag.isIdentifierLike()) break;

        if (seen_ptr.*) {
            try parser.report(
                token.span,
                try parser.fmt("Duplicate '{s}' modifier on type parameter", .{token.tag.toString().?}),
                .{},
            );
        } else if (this_step < step) {
            try parser.report(
                token.span,
                "Type parameter modifiers must appear in the order 'const in out'",
                .{},
            );
        }

        seen_ptr.* = true;
        step = @max(step, this_step);

        if (!start_set) {
            start = token.span.start;
            start_set = true;
        }

        try parser.advance() orelse return null;
    }

    const name_token = parser.current_token;
    const name = try literals.parseBindingIdentifier(parser) orelse return null;

    if (!start_set) start = name_token.span.start;
    var end = name_token.span.end;

    var constraint: ast.NodeIndex = .null;
    if (parser.current_token.tag == .extends) {
        try parser.advance() orelse return null;
        constraint = try core.parseType(parser) orelse return null;
        end = parser.tree.span(constraint).end;
    }

    var default: ast.NodeIndex = .null;
    if (parser.current_token.tag == .assign) {
        try parser.advance() orelse return null;
        default = try core.parseType(parser) orelse return null;
        end = parser.tree.span(default).end;
    }

    return try parser.tree.addNode(
        .{ .ts_type_parameter = .{
            .name = name,
            .constraint = constraint,
            .default = default,
            .in = flags.in,
            .out = flags.out,
            .@"const" = flags.@"const",
        } },
        .{ .start = start, .end = end },
    );
}
