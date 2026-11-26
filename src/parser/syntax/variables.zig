const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const Precedence = @import("../token.zig").Precedence;
const TokenTag = @import("../token.zig").TokenTag;

const expressions = @import("expressions.zig");
const patterns = @import("patterns.zig");
const ts = @import("ts/types.zig");

pub const ParseVariableDeclarationOptions = struct {
    await_using: bool = false,
    is_declare: bool = false,
};

pub const DeclaratorContext = enum {
    normal,
    declare,
    for_loop,
};

pub fn parseVariableDeclaration(
    parser: *Parser,
    options: ParseVariableDeclarationOptions,
    start_from_param: ?u32,
) Error!?ast.NodeIndex {
    const start = start_from_param orelse parser.current_token.span.start;
    const kind: ast.VariableKind = switch (parser.current_token.tag) {
        inline .let, .@"const", .@"var" => |tag| @field(ast.VariableKind, @tagName(tag)),
        .using => if (options.await_using) .await_using else .using,
        else => unreachable,
    };
    try parser.advance() orelse return null;

    const declarators = parser.list();
    defer declarators.deinit();

    const context: DeclaratorContext = if (parser.ts_context.ambient) .declare else .normal;

    var end: u32 = undefined;
    while (true) {
        const declarator = try @call(
            .always_inline,
            parseVariableDeclarator,
            .{ parser, kind, context },
        ) orelse return null;
        try declarators.append(declarator);
        end = parser.tree.span(declarator).end;

        if (!(try parser.eat(.comma) orelse return null)) break;
    }

    const span: ast.Span = .{
        .start = start,
        .end = try parser.eatSemicolon(end) orelse return null,
    };

    if (parser.context.single_statement and kind != .@"var") {
        @branchHint(.unlikely);

        try parser.report(
            span,
            try parser.fmt(
                "'{s}' declarations can only be declared inside a block",
                .{kind.toString()},
            ),
            .{ .help = "Wrap the declaration in a block" },
        );
    }

    return try parser.tree.addNode(.{ .variable_declaration = .{
        .declarators = try declarators.finish(),
        .kind = kind,
        .declare = options.is_declare,
    } }, span);
}

pub fn parseVariableDeclarator(
    parser: *Parser,
    kind: ast.VariableKind,
    context: DeclaratorContext,
) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var id = try patterns.parseBindingPattern(parser) orelse return null;
    const id_span = parser.tree.span(id);

    var definite = false;
    var annotated = false;
    var end = id_span.end;

    if (parser.tree.isTs()) {
        if (parser.at(.logical_not) and !parser.current_token.hasLineTerminatorBefore()) {
            definite = true;
            end = parser.current_token.span.end;
            try parser.advance() orelse return null;
        }

        if (parser.at(.colon)) {
            id = try ts.parsePatternTypeAnnotation(parser, id) orelse return null;
            annotated = true;
            end = parser.tree.span(id).end;
        }
    }

    var init: ast.NodeIndex = .null;

    const is_using = kind == .using or kind == .await_using;
    const is_destructuring = patterns.isDestructuringPattern(parser, id);

    if (is_using and is_destructuring) try parser.report(
        id_span,
        try parser.fmt("'{s}' declarations may not have binding patterns", .{kind.toString()}),
        .{},
    );

    if (try parser.eat(.assign) orelse return null) {
        init = try expressions.parseExpression(
            parser,
            Precedence.Assignment,
            .{ .allow_in = context != .for_loop },
        ) orelse return null;
        end = parser.tree.span(init).end;
    } else if (context == .normal) {
        if (is_destructuring) {
            try parser.report(
                id_span,
                "A destructuring declaration must have an initializer",
                .{ .help = "Add an initializer with the value to destructure" },
            );
        } else if (kind == .@"const") {
            try parser.report(
                id_span,
                "'const' declarations must be initialized",
                .{ .help = "Add an initializer, or use 'let' to assign the value later" },
            );
        } else if (is_using) {
            try parser.report(
                id_span,
                try parser.fmt("'{s}' declarations must be initialized", .{kind.toString()}),
                .{ .help = "Add an initializer with the resource to dispose" },
            );
        }
    }

    if (definite) try ts.checkDefiniteAssignment(parser, id_span, annotated, init != .null);

    return try parser.tree.addNode(
        .{ .variable_declarator = .{ .id = id, .init = init, .definite = definite } },
        .{ .start = start, .end = end },
    );
}

pub fn canStartBindingIdentifier(tag: TokenTag) bool {
    return tag.isIdentifierLike() and !tag.isUnconditionallyReserved();
}

pub fn isLetIdentifier(parser: *Parser) bool {
    std.debug.assert(parser.at(.let));
    const next = parser.peekAhead();
    if (next.tag == .left_bracket or next.tag == .left_brace) return false;
    return !canStartBindingIdentifier(next.tag);
}

pub fn isUsingIdentifier(parser: *Parser) bool {
    std.debug.assert(parser.at(.using));
    const next = parser.peekAhead();
    // [+Using] using [no LineTerminator here] BindingList
    return next.hasLineTerminatorBefore() or !canStartBindingIdentifier(next.tag);
}

/// Returns whether `await using x` on one line heads an `AwaitUsingDeclaration` under [+Await].
pub fn isAwaitUsingDeclarationAhead(parser: *Parser) bool {
    std.debug.assert(parser.at(.await));
    if (!parser.context.await) return false;

    var peek = parser.beginPeek();
    defer peek.end();

    const using_token = peek.next();
    if (using_token.tag != .using or using_token.hasLineTerminatorBefore()) return false;
    const binding = peek.next();
    return !binding.hasLineTerminatorBefore() and canStartBindingIdentifier(binding.tag);
}
