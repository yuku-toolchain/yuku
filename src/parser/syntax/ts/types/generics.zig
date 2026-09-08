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
    return parseAngleList(parser, .arguments, .peel);
}

// a fused `>=` or `>>` fails so `f<T>= x` stays relational like tsc
pub inline fn parseTypeArgumentsInExpression(parser: *Parser) Error!ast.NodeIndex {
    return parseAngleList(parser, .arguments, .exact);
}

// a newline before `<` keeps `typeof a` apart from a generic function type after it
pub inline fn parseTypeArgumentsAfterEntityName(parser: *Parser) Error!ast.NodeIndex {
    if (!isAngleOpen(parser.current_token.tag) or
        parser.current_token.hasLineTerminatorBefore()) return .null;
    return parseAngleList(parser, .arguments, .peel);
}

// function f<T, U extends V>() {}
//           ^^^^^^^^^^^^^^^^
pub fn parseTypeParameters(parser: *Parser) Error!ast.NodeIndex {
    return parseAngleList(parser, .parameters, .peel);
}

const AngleListKind = enum {
    arguments,
    parameters,
};

const AngleClose = enum {
    // one `>` peels off a fused `>>` or `>=`
    peel,
    // a fused closer fails the speculation
    exact,
};

fn parseAngleList(
    parser: *Parser,
    comptime kind: AngleListKind,
    comptime close: AngleClose,
) Error!ast.NodeIndex {
    const start = try consumeAngleOpen(parser) orelse return .null;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    var parsed_any = false;
    while (!isAngleClose(parser.current_token.tag) and parser.current_token.tag != .eof) {
        const elem = switch (kind) {
            .arguments => try core.parseType(parser) orelse return .null,
            .parameters => try parseTypeParameter(parser) orelse return .null,
        };
        try parser.scratch_a.append(parser.allocator(), elem);
        parsed_any = true;
        if (parser.current_token.tag != .comma) break;
        try parser.advance() orelse return .null;
    }

    if (!parsed_any) try parser.report(parser.current_token.span, switch (kind) {
        .arguments => "A type argument list cannot be empty",
        .parameters => "A type parameter list cannot be empty",
    }, .{});

    const end = try consumeAngleClose(parser, kind, close) orelse return .null;
    const params = try parser.flushToExtras(&parser.scratch_a, checkpoint);

    const data: ast.NodeData = switch (kind) {
        .arguments => .{ .ts_type_parameter_instantiation = .{ .params = params } },
        .parameters => .{ .ts_type_parameter_declaration = .{ .params = params } },
    };
    return try parser.tree.addNode(data, .{ .start = start, .end = end });
}

// fused `<<` opens nested instantiations like `Foo<<T>(x: T) => R>`
pub inline fn isAngleOpen(tag: TokenTag) bool {
    return tag == .less_than or tag == .left_shift;
}

// nested closers fuse into one token that consumeAngleClose peels apart
inline fn isAngleClose(tag: TokenTag) bool {
    return switch (tag) {
        .greater_than,
        .right_shift,
        .unsigned_right_shift,
        .greater_than_equal,
        .right_shift_assign,
        .unsigned_right_shift_assign,
        => true,
        else => false,
    };
}

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

fn consumeAngleClose(
    parser: *Parser,
    comptime kind: AngleListKind,
    comptime close: AngleClose,
) Error!?u32 {
    switch (parser.current_token.tag) {
        .greater_than => {
            const end = parser.current_token.span.end;
            try parser.advance() orelse return null;
            return end;
        },
        .right_shift,
        .unsigned_right_shift,
        .greater_than_equal,
        .right_shift_assign,
        .unsigned_right_shift_assign,
        => {
            if (close == .exact) return null;
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
    var flags: struct { @"const": bool = false, in: bool = false, out: bool = false } = .{};
    var step: u8 = 0;
    var start: u32 = parser.current_token.span.start;
    var start_set = false;

    // a modifier only when a name follows, else `<out>` is a parameter named out
    while (true) {
        const token = parser.current_token;
        const this_step: u8, const seen_ptr: *bool = switch (token.tag) {
            .@"const" => .{ 1, &flags.@"const" },
            .in => .{ 2, &flags.in },
            .out => .{ 3, &flags.out },
            else => break,
        };

        const next = parser.peekAhead();
        if (!next.tag.isIdentifierLike()) break;

        if (seen_ptr.*) {
            try parser.report(
                token.span,
                try parser.fmt(
                    "Duplicate '{s}' modifier on type parameter",
                    .{token.tag.toString().?},
                ),
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
