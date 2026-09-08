const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const Precedence = @import("../token.zig").Precedence;
const TokenTag = @import("../token.zig").TokenTag;
const expressions = @import("expressions.zig");
const patterns = @import("patterns.zig");
const ts = @import("ts/types.zig");
const std = @import("std");
const extension = @import("../extension.zig");

pub const ParseVariableDeclarationOpts = struct {
    await_using: bool = false,
    /// Sets the `declare` flag on the resulting `VariableDeclaration`.
    is_declare: bool = false,
};

/// Syntactic context of a `VariableDeclarator`, which selects the initializer rules that apply.
pub const DeclaratorCtx = enum {
    /// A regular declaration statement.
    normal,
    /// An ambient `declare` declaration, where initializers may be omitted.
    declare,
    /// A for-loop head, where the loop parser reports initializer errors itself.
    for_loop,
};

pub fn parseVariableDeclaration(
    parser: *Parser,
    opts: ParseVariableDeclarationOpts,
    start_from_param: ?u32,
) Error!?ast.NodeIndex {
    std.debug.assert(start_from_param != null or switch (parser.current_token.tag) {
        .@"var", .let, .@"const", .using => true,
        else => false,
    });
    const start = start_from_param orelse parser.current_token.span.start;
    const kind = try parseVariableKind(parser, opts.await_using) orelse return null;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    const ctx: DeclaratorCtx = if (parser.ts_context.ambient) .declare else .normal;

    const first_declarator = try parseVariableDeclarator(parser, kind, ctx) orelse return null;

    try parser.scratch_a.append(parser.allocator(), first_declarator);

    var end = parser.tree.span(first_declarator).end;

    while (parser.current_token.tag == .comma) {
        try parser.advance() orelse return null;
        const declarator = try parseVariableDeclarator(parser, kind, ctx) orelse return null;
        try parser.scratch_a.append(parser.allocator(), declarator);
        end = parser.tree.span(declarator).end;
    }

    const semi_end = try parser.eatSemicolon(end) orelse return null;
    const span: ast.Span = .{ .start = start, .end = semi_end };

    if (parser.context.single_statement and
        (kind == .let or kind == .@"const" or kind == .using or kind == .await_using))
    {
        @branchHint(.unlikely);

        try parser.report(
            span,
            "Lexical declaration cannot appear in a single-statement context",
            .{ .help = "Wrap this declaration in a block statement" },
        );
    }

    return try parser.tree.addNode(
        .{
            .variable_declaration = .{
                .declarators = try parser.flushToExtras(&parser.scratch_a, checkpoint),
                .kind = kind,
                .declare = opts.is_declare,
            },
        },
        span,
    );
}

fn parseVariableKind(parser: *Parser, await_using: bool) Error!?ast.VariableKind {
    const tag = parser.current_token.tag;
    try parser.advance() orelse return null;

    return switch (tag) {
        .let => .let,
        .@"const" => .@"const",
        .@"var" => .@"var",
        .using => blk: {
            if (await_using) {
                break :blk .await_using;
            } else {
                break :blk .using;
            }
        },
        else => null,
    };
}

pub fn parseVariableDeclarator(
    parser: *Parser,
    kind: ast.VariableKind,
    ctx: DeclaratorCtx,
) Error!?ast.NodeIndex {
    const is_ts = parser.tree.isTs();
    const start = parser.current_token.span.start;
    const id = try patterns.parseBindingPattern(parser) orelse return null;
    const id_span = parser.tree.span(id);

    var definite = false;
    var annotated = false;
    var end = id_span.end;

    if (is_ts) {
        if (parser.current_token.tag == .logical_not and
            !parser.current_token.hasLineTerminatorBefore())
        {
            definite = true;
            end = parser.current_token.span.end;
            try parser.advance() orelse return null;
        }

        if (parser.current_token.tag == .colon) {
            const annotation = try ts.parseTypeAnnotation(parser) orelse return null;
            ts.applyTypeAnnotationToPattern(parser, id, annotation);
            annotated = true;
            end = parser.tree.span(annotation).end;
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

        // Annex B 3.5 `for (var x = 0 in obj)` must stop at `in`
        init = try expressions.parseExpression(
            parser,
            Precedence.Assignment,
            .{ .respect_allow_in = true },
        ) orelse return null;

        end = parser.tree.span(init).end;
    } else switch (ctx) {
        .for_loop => {},
        .declare => {},
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

/// Returns whether `tag` can begin a `BindingIdentifier` or a destructuring pattern.
pub fn canStartBinding(tag: TokenTag) bool {
    if (extension.at(.binding_start, .{tag})) |answer| return answer;
    return tag.isIdentifierLike() or tag == .left_bracket or tag == .left_brace;
}

/// Returns whether `tag` can begin a `BindingIdentifier`.
pub fn canStartBindingIdentifier(tag: TokenTag) bool {
    return tag.isIdentifierLike() and !tag.isUnconditionallyReserved();
}

/// Like `canStartBinding`, but rejects reserved words so that `let in obj` keeps `let` as an identifier.
pub fn canStartLetBinding(tag: TokenTag) bool {
    return canStartBinding(tag) and !tag.isUnconditionallyReserved();
}

test "canStartLetBinding matches the longhand it replaced" {
    inline for (@typeInfo(TokenTag).@"enum".fields) |field| {
        const tag = @field(TokenTag, field.name);
        try std.testing.expectEqual(
            tag == .left_bracket or tag == .left_brace or canStartBindingIdentifier(tag),
            canStartLetBinding(tag),
        );
    }
}

/// Returns whether `let` begins an expression statement rather than a declaration.
/// Only `let [` is a lookahead restriction, so sloppy-mode `let = 1` and `let in obj` are expressions.
pub fn isLetIdentifier(parser: *Parser) Error!bool {
    std.debug.assert(parser.current_token.tag == .let);

    const next = parser.peekAhead();

    return !canStartLetBinding(next.tag);
}

/// Returns whether the current `using` token is an `IdentifierReference` rather than a declaration keyword.
pub fn isUsingIdentifier(parser: *Parser) Error!bool {
    std.debug.assert(parser.current_token.tag == .using);

    const next = parser.peekAhead();

    // [+Using] using [no LineTerminator here] BindingList
    return next.hasLineTerminatorBefore() or !canStartBindingIdentifier(next.tag);
}

/// Returns whether `await using x` on one line heads an `AwaitUsingDeclaration` in an [+Await] context.
/// `await [no LineTerminator here] using [no LineTerminator here] Binding`
pub fn isAwaitUsingDeclarationAhead(parser: *Parser) Error!bool {
    std.debug.assert(parser.current_token.tag == .await);

    if (!parser.context.await) return false;

    var peek = parser.beginPeek();
    defer peek.end();

    const using_token = peek.next();
    if (using_token.tag != .using) return false;
    if (using_token.hasLineTerminatorBefore()) return false;

    const binding = peek.next();
    if (binding.hasLineTerminatorBefore()) return false;

    return canStartBindingIdentifier(binding.tag);
}
