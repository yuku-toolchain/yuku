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

/// Syntactic context of a `VariableDeclarator`, which selects the initializer rules that apply.
pub const DeclaratorContext = enum {
    /// A regular declaration statement.
    normal,
    /// An ambient `declare` declaration, where initializers may be omitted.
    declare,
    /// A for-loop head, where the loop parser reports initializer errors itself.
    for_loop,
};

pub fn parseVariableDeclaration(
    parser: *Parser,
    options: ParseVariableDeclarationOptions,
    start_from_param: ?u32,
) Error!?ast.NodeIndex {
    std.debug.assert(start_from_param != null or switch (parser.current_token.tag) {
        .@"var", .let, .@"const", .using => true,
        else => false,
    });
    const start = start_from_param orelse parser.current_token.span.start;
    const tag = parser.current_token.tag;
    try parser.advance() orelse return null;
    const kind: ast.VariableKind = switch (tag) {
        .let => .let,
        .@"const" => .@"const",
        .@"var" => .@"var",
        .using => if (options.await_using) .await_using else .using,
        else => return null,
    };

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    const context: DeclaratorContext = if (parser.ts_context.ambient) .declare else .normal;

    var end: u32 = undefined;
    while (true) {
        const declarator = try parseVariableDeclarator(parser, kind, context) orelse return null;
        try parser.scratch_a.append(parser.allocator(), declarator);
        end = parser.tree.span(declarator).end;

        if (parser.current_token.tag != .comma) break;
        try parser.advance() orelse return null;
    }

    const semi_end = try parser.eatSemicolon(end) orelse return null;
    const span: ast.Span = .{ .start = start, .end = semi_end };

    if (parser.context.single_statement and kind != .@"var") {
        @branchHint(.unlikely);

        try parser.report(
            span,
            "Lexical declaration cannot appear in a single-statement context",
            .{ .help = "Wrap this declaration in a block statement" },
        );
    }

    return try parser.tree.addNode(.{ .variable_declaration = .{
        .declarators = try parser.flushToExtras(&parser.scratch_a, checkpoint),
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
        if (parser.current_token.tag == .logical_not and
            !parser.current_token.hasLineTerminatorBefore())
        {
            definite = true;
            end = parser.current_token.span.end;
            try parser.advance() orelse return null;
        }

        if (parser.current_token.tag == .colon) {
            id = try ts.parsePatternTypeAnnotation(parser, id) orelse return null;
            annotated = true;
            end = parser.tree.span(id).end;
        }
    }

    var init: ast.NodeIndex = .null;

    const is_using = kind == .using or kind == .await_using;
    const is_destructuring = patterns.isDestructuringPattern(parser, id);

    if (is_using and is_destructuring) {
        try parser.report(id_span, "Using declaration cannot have destructuring patterns.", .{});
    }

    if (parser.current_token.tag == .assign) {
        try parser.advance() orelse return null;

        init = try expressions.parseExpression(
            parser,
            Precedence.Assignment,
            .{ .allow_in = context != .for_loop },
        ) orelse return null;

        end = parser.tree.span(init).end;
    } else switch (context) {
        .for_loop, .declare => {},
        .normal => {
            if (is_destructuring) {
                try parser.report(
                    id_span,
                    "Destructuring declaration must have an initializer",
                    .{ .help = "Add '= value' to provide the object or array to" ++
                        " destructure from." },
                );
            } else if (kind == .@"const") {
                try parser.report(
                    id_span,
                    "'const' declarations must be initialized",
                    .{ .help = "Add '= value' to initialize the constant, or use 'let' if" ++
                        " you need to assign it later." },
                );
            } else if (is_using) {
                try parser.report(
                    id_span,
                    try parser.fmt("'{s}' declarations must be initialized", .{kind.toString()}),
                    .{ .help = "Disposable resources require an initial value that" ++
                        " implements the dispose protocol." },
                );
            }
        },
    }

    if (definite) try ts.checkDefiniteAssignment(parser, id_span, annotated, init != .null);

    return try parser.tree.addNode(
        .{ .variable_declarator = .{ .id = id, .init = init, .definite = definite } },
        .{ .start = start, .end = end },
    );
}

/// Returns whether `tag` can begin a `BindingIdentifier`.
pub fn canStartBindingIdentifier(tag: TokenTag) bool {
    return tag.isIdentifierLike() and !tag.isUnconditionallyReserved();
}

/// Returns whether `let` begins an expression statement rather than a declaration. Only `let [`
/// is a lookahead restriction, so sloppy-mode `let = 1` and `let in obj` are expressions.
pub fn isLetIdentifier(parser: *Parser) bool {
    std.debug.assert(parser.current_token.tag == .let);
    const next = parser.peekAhead();
    if (next.tag == .left_bracket or next.tag == .left_brace) return false;
    return !canStartBindingIdentifier(next.tag);
}

/// Returns whether the current `using` is an `IdentifierReference`, not a declaration keyword.
pub fn isUsingIdentifier(parser: *Parser) bool {
    std.debug.assert(parser.current_token.tag == .using);
    const next = parser.peekAhead();
    // [+Using] using [no LineTerminator here] BindingList
    return next.hasLineTerminatorBefore() or !canStartBindingIdentifier(next.tag);
}

/// Returns whether `await using x` on one line heads an `AwaitUsingDeclaration` under [+Await].
/// `await [no LineTerminator here] using [no LineTerminator here] Binding`
pub fn isAwaitUsingDeclarationAhead(parser: *Parser) bool {
    std.debug.assert(parser.current_token.tag == .await);
    if (!parser.context.await) return false;

    var peek = parser.beginPeek();
    defer peek.end();

    const using_token = peek.next();
    if (using_token.tag != .using or using_token.hasLineTerminatorBefore()) return false;
    const binding = peek.next();
    return !binding.hasLineTerminatorBefore() and canStartBindingIdentifier(binding.tag);
}
