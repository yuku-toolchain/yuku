// pratt parser for JavaScript expressions
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_precedence
// https://tdop.github.io/
const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const Token = @import("../token.zig").Token;
const Precedence = @import("../token.zig").Precedence;

const jsx = @import("jsx/root.zig");
const array = @import("array.zig");
const object = @import("object.zig");
const literals = @import("literals.zig");
const functions = @import("functions.zig");
const class = @import("class.zig");
const decorator = @import("decorators.zig");
const parenthesized = @import("parenthesized.zig");
const modules = @import("modules.zig");
const grammar = @import("../grammar.zig");
const ecmascript = @import("../ecmascript.zig");
const dialect = @import("../dialect/root.zig");
const ts = @import("ts/types.zig");

pub const ParseExpressionOptions = struct {
    /// `[In]`
    allow_in: bool = true,
};

pub fn parseExpression(
    parser: *Parser,
    min_precedence: u8,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    const first = try parsePrefix(parser, min_precedence, options) orelse return null;
    return parseExpressionFrom(parser, first, min_precedence, options);
}

/// Continues an expression from its first operand, already parsed.
pub inline fn parseExpressionFrom(
    parser: *Parser,
    first: ast.NodeIndex,
    min_precedence: u8,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    var left = first;
    const is_ts = parser.tree.isTs();

    while (true) {
        if (comptime dialect.has(.operator)) {
            switch (try dialect.claim(.operator, parser, .{ left, min_precedence })) {
                .decline => {},
                .node => |node| {
                    left = node;
                    continue;
                },
                .failed => return null,
                .end => break,
            }
        }

        const current_token = parser.current_token;

        // `f<T>(x)` binds as a call before the precedence gate can read a relational `f < T`
        if (is_ts and ts.isAngleOpen(current_token.tag) and
            maxLeftPrecedence(parser.tree.data(left)) >= Precedence.Call)
        {
            if (try ts.parseTypeArgumentedCallOrInstantiation(parser, left)) |node| {
                left = node;
                continue;
            }
        }

        const infix_precedence = infixPrecedence(current_token, is_ts);

        if (infix_precedence < min_precedence or infix_precedence == 0) break;

        // `a++()` and `() => {}()` stop here and surface as a missing semicolon
        if (infix_precedence > maxLeftPrecedence(parser.tree.data(left))) break;

        if (current_token.tag == .in and !options.allow_in) break;

        left = try parseInfix(parser, infix_precedence, left, options) orelse return null;
    }

    return left;
}

inline fn infixPrecedence(token: Token, is_ts: bool) u8 {
    switch (token.tag) {
        .increment, .decrement, .as, .satisfies => {
            if (token.hasLineTerminatorBefore()) return 0;
        },
        .logical_not => {
            if (!is_ts or token.hasLineTerminatorBefore()) return 0;
        },
        else => {},
    }

    return token.tag.precedence();
}

fn parsePrefix(
    parser: *Parser,
    precedence: u8,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    if (comptime dialect.has(.expression)) {
        if (try dialect.at(.expression, parser, .{})) |expression| return expression;
    }

    const tag = parser.current_token.tag;

    if (tag == .identifier) {
        @branchHint(.likely);
        return literals.parseIdentifier(parser);
    }

    if (tag == .increment or tag == .decrement) return parseUpdateExpression(parser, true, .null);
    if (tag.isUnaryOperator()) return parseUnaryExpression(parser);
    if (tag == .left_paren) return parseParenthesizedOrArrowFunction(parser, precedence, options);
    if (tag == .await and parser.context.await) return parseAwaitExpression(parser);
    if (tag == .yield and parser.context.yield and precedence < Precedence.Additive) {
        return parseYieldExpression(parser, options);
    }
    if (tag == .new) return parseNewExpression(parser);
    if (tag == .import) return parseImportExpression(parser);
    if (tag == .async) return parseAsyncFunctionOrArrow(parser, precedence, options);

    if (tag == .less_than) {
        if (parser.tree.isTs()) {
            const start = parser.current_token.span.start;
            if (precedence <= Precedence.Assignment) {
                if (try ts.tryParseGenericArrow(parser, false, start, options)) |arrow| {
                    return arrow;
                }
            }
            if (!parser.tree.isJsx()) return ts.parseTypeAssertion(parser);
        }
        if (parser.tree.isJsx()) return jsx.parseJsxExpression(parser);
    }

    return parsePrimaryExpression(parser, precedence);
}

fn parseInfix(
    parser: *Parser,
    precedence: u8,
    left: ast.NodeIndex,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    const current = parser.current_token;
    const tag = current.tag;

    if (tag == .dot) return parseStaticMemberExpression(parser, left);
    if (tag == .left_paren) return parseCallExpression(parser, left, false, .null);
    if (tag == .left_bracket) return parseComputedMemberExpression(parser, left, false);

    if (tag.isBinaryOperator()) return parseBinaryExpression(parser, precedence, left, options);
    if (tag.isLogicalOperator()) return parseLogicalExpression(parser, precedence, left, options);
    if (tag.isAssignmentOperator()) {
        return parseAssignmentExpression(parser, precedence, left, options);
    }

    if (parser.tree.isTs()) {
        switch (tag) {
            .as, .satisfies => return ts.parseAsOrSatisfiesExpression(parser, left),
            .logical_not => if (!current.hasLineTerminatorBefore()) {
                return ts.parseNonNullExpression(parser, left);
            },
            else => {},
        }
    }

    if (tag == .arrow and !current.hasLineTerminatorBefore()) {
        return parseSimpleArrowFunction(parser, left, options);
    }

    switch (tag) {
        .increment, .decrement => return parseUpdateExpression(parser, false, left),
        .question => return parseConditionalExpression(parser, precedence, left, options),
        .comma => return parseSequenceExpression(parser, precedence, left, options),
        .template_head, .no_substitution_template => return parseTaggedTemplateExpression(
            parser,
            left,
            .null,
        ),
        .optional_chaining => return parseOptionalChain(parser, left),
        else => {},
    }

    try parser.report(
        current.span,
        try parser.fmt("Unexpected token '{s}' in expression", .{parser.describeToken(current)}),
        .{ .help = "This token cannot be used here. " ++
            "Expected an operator, semicolon, or end of expression." },
    );

    return null;
}

inline fn parsePrimaryExpression(parser: *Parser, precedence: u8) Error!?ast.NodeIndex {
    if (parser.current_token.tag.isNumericLiteral()) {
        return literals.parseNumericLiteral(parser);
    }

    return switch (parser.current_token.tag) {
        .private_identifier => blk: {
            const node = try literals.parsePrivateIdentifier(parser) orelse break :blk null;

            if (parser.current_token.tag != .in or precedence > Precedence.Relational) {
                try parser.report(
                    parser.tree.span(node),
                    "Private names are only valid in property accesses (`obj.#field`)" ++
                        " or `in` expressions (`#field in obj`)",
                    .{},
                );
            }

            break :blk node;
        },
        .string_literal => literals.parseStringLiteral(parser),
        .true, .false => literals.parseBooleanLiteral(parser),
        .null_literal => literals.parseNullLiteral(parser),
        .this => parseThisExpression(parser),
        .super => parseSuperExpression(parser),
        .slash, .slash_assign => literals.parseRegExpLiteral(parser),
        .template_head => literals.parseTemplateLiteral(parser, false),
        .no_substitution_template => literals.parseNoSubstitutionTemplate(parser, false),
        .left_bracket => parseArrayExpression(parser),
        .left_brace => parseObjectExpression(parser),
        .async => blk: {
            const async_token = parser.current_token;
            const async_id = try literals.parseIdentifier(parser) orelse break :blk null;
            if (parser.current_token.tag == .function) {
                if (!parser.current_token.hasLineTerminatorBefore()) {
                    break :blk try parseAsyncFunctionExpression(parser, &async_token);
                }
            }
            break :blk async_id;
        },
        .function => functions.parseFunction(parser, .{ .is_expression = true }, null),
        .class => class.parseClass(parser, .{ .is_expression = true }, null),
        .at => decorator.parseDecoratedClass(parser, .{ .is_expression = true }),
        else => {
            if (parser.current_token.tag.isIdentifierLike()) {
                return literals.parseIdentifier(parser);
            }

            const token = parser.current_token;
            try parser.report(
                token.span,
                try parser.fmt("Unexpected token '{s}'", .{parser.describeToken(token)}),
                .{ .help = "Expected an expression" },
            );
            return null;
        },
    };
}

fn parseParenthesizedExpression(parser: *Parser) Error!?ast.NodeIndex {
    const cover = try parenthesized.parseCover(parser) orelse return null;
    return parenthesized.coverToParenthesizedExpression(parser, cover);
}

fn parseParenthesizedOrArrowFunction(
    parser: *Parser,
    precedence: u8,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (parser.tree.isTs() and precedence <= Precedence.Assignment) {
        switch (ts.classifyArrowHead(parser)) {
            .yes => return ts.parseArrow(parser, false, start, options),
            .maybe => if (try ts.tryParseArrow(parser, false, start, options)) |arrow| {
                return arrow;
            },
            .no => {},
        }
    }

    const cover = try parenthesized.parseCover(parser) orelse return null;

    // ts arrows all went through the tristate above, a stray `:` belongs to the outer context
    if (isArrowNext(parser, precedence)) {
        return parenthesized.coverToArrowFunction(parser, cover, start, options);
    }

    return parenthesized.coverToParenthesizedExpression(parser, cover);
}

fn parseSimpleArrowFunction(
    parser: *Parser,
    left: ast.NodeIndex,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    const span = parser.tree.span(left);
    if (parser.tree.data(left) != .identifier_reference) {
        try parser.report(span, "Unexpected '=>'", .{
            .help = "'=>' is only valid after a single identifier " ++
                "or a parenthesized parameter list",
        });
        return null;
    }
    return parenthesized.identifierToArrowFunction(parser, left, false, span.start, options);
}

fn parseAsyncFunctionExpression(parser: *Parser, async_token: *const Token) Error!?ast.NodeIndex {
    std.debug.assert(async_token.tag == .async);
    std.debug.assert(parser.current_token.tag == .function);
    std.debug.assert(!parser.current_token.hasLineTerminatorBefore());

    try parser.reportIfEscapedKeyword(async_token.*);
    return functions.parseFunction(
        parser,
        .{ .is_expression = true, .is_async = true },
        async_token.span.start,
    );
}

fn parseAsyncFunctionOrArrow(
    parser: *Parser,
    precedence: u8,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    const async_token = parser.current_token;
    const start = async_token.span.start;
    const async_id = try literals.parseIdentifier(parser) orelse return null;

    if (parser.current_token.hasLineTerminatorBefore()) return async_id;

    const next = parser.current_token;

    if (next.tag == .function) return parseAsyncFunctionExpression(parser, &async_token);

    if (next.tag == .left_paren) {
        // an async head can still be a call, so even a sure arrow is only tried
        if (precedence <= Precedence.Assignment and ts.classifyArrowHead(parser) != .no) {
            if (try ts.tryParseArrow(parser, true, start, options)) |arrow| {
                try parser.reportIfEscapedKeyword(async_token);
                return arrow;
            }
        }

        const head = parser.checkpoint();
        const cover = try parenthesized.parseCover(parser) orelse return null;

        // the lookahead turned it down, so it is reparsed as an arrow to report why
        if (isArrowNext(parser, precedence)) {
            parser.rewind(head);
            try parser.reportIfEscapedKeyword(async_token);
            return ts.parseArrow(parser, true, start, options);
        }

        return try parenthesized.coverToCallExpression(parser, cover, async_id);
    }

    if (parser.tree.isTs() and next.tag == .less_than and precedence <= Precedence.Assignment) {
        if (try ts.tryParseGenericArrow(parser, true, start, options)) |arrow| {
            try parser.reportIfEscapedKeyword(async_token);
            return arrow;
        }
    }

    if (next.tag.isIdentifierLike() and precedence <= Precedence.Assignment) {
        const after_id = parser.peekAhead();
        if (after_id.tag == .arrow and !after_id.hasLineTerminatorBefore()) {
            try parser.reportIfEscapedKeyword(async_token);
            // an async arrow's parameter is read where `await` is a keyword
            const outer_await = parser.context.await;
            parser.context.await = true;
            const id = try literals.parseIdentifier(parser) orelse return null;
            parser.context.await = outer_await;
            return parenthesized.identifierToArrowFunction(parser, id, true, start, options);
        }
    }

    return async_id;
}

inline fn isArrowNext(parser: *Parser, precedence: u8) bool {
    const token = parser.current_token;
    return token.tag == .arrow and !token.hasLineTerminatorBefore() and
        precedence <= Precedence.Assignment;
}

fn parseUnaryExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag.isUnaryOperator());
    const operator_token = parser.current_token;
    try parser.advance() orelse return null;

    const argument = try parseExpression(parser, Precedence.Unary, .{}) orelse return null;
    const span: ast.Span = .{
        .start = operator_token.span.start,
        .end = parser.tree.span(argument).end,
    };

    if (parser.current_token.tag == .exponent) {
        try parser.report(
            span,
            "The left-hand side of '**' cannot be an unparenthesized unary expression",
            .{ .help = "Add parentheses to make precedence explicit, " ++
                "for example `(-x) ** y` or `-(x ** y)`." },
        );
    }

    return try parser.tree.addNode(.{ .unary_expression = .{
        .argument = argument,
        .operator = ast.UnaryOperator.fromToken(operator_token.tag),
    } }, span);
}

fn parseAwaitExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .await);
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const argument = try parseExpression(parser, Precedence.Unary, .{}) orelse return null;
    const span: ast.Span = .{ .start = start, .end = parser.tree.span(argument).end };

    if (parser.current_token.tag == .exponent) {
        try parser.report(
            span,
            "The left-hand side of '**' cannot be an unparenthesized await expression",
            .{ .help = "Add parentheses to make precedence explicit, " ++
                "for example `(await x) ** y`." },
        );
    }

    return try parser.tree.addNode(.{ .await_expression = .{ .argument = argument } }, span);
}

fn parseYieldExpression(parser: *Parser, options: ParseExpressionOptions) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .yield);
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;
    try parser.advance() orelse return null;

    const delegate = parser.current_token.tag == .star and
        !parser.current_token.hasLineTerminatorBefore();
    if (delegate) {
        end = parser.current_token.span.end;
        try parser.advance() orelse return null;
    }

    var argument: ast.NodeIndex = .null;
    if (!parser.current_token.hasLineTerminatorBefore()) {
        switch (parser.current_token.tag) {
            .eof, .right_brace, .right_paren, .right_bracket, .colon, .comma, .semicolon => {},
            else => {
                // YieldExpression[?In]
                argument = try parseExpression(parser, Precedence.Assignment, options) orelse
                    return null;
                end = parser.tree.span(argument).end;
            },
        }
    }

    if (delegate and argument == .null) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected expression after 'yield*'",
            .{},
        );
        return null;
    }

    return try parser.tree.addNode(
        .{ .yield_expression = .{ .argument = argument, .delegate = delegate } },
        .{ .start = start, .end = end },
    );
}

fn parseThisExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .this);
    const span = parser.current_token.span;
    try parser.advance() orelse return null;
    return try parser.tree.addNode(.{ .this_expression = .{} }, span);
}

fn parseSuperExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .super);
    const span = parser.current_token.span;
    try parser.advance() orelse return null;
    const tag = parser.current_token.tag;
    if (tag != .left_paren and tag != .dot and tag != .left_bracket) {
        try parser.report(
            parser.current_token.span,
            "'super' must be followed by a call or property access",
            .{ .help = "use 'super()' to call parent constructor, " ++
                "'super.property' or 'super[property]' to access parent members" },
        );
        return null;
    }
    return try parser.tree.addNode(.{ .super = .{} }, span);
}

/// Parses `import.meta` or `import(...)`.
fn parseImportExpression(parser: *Parser) Error!?ast.NodeIndex {
    // `import` is a keyword here but stored as an IdentifierName, so reject escapes
    try parser.reportIfEscapedKeyword(parser.current_token);
    const name = try literals.parseIdentifierName(parser) orelse return null;

    switch (parser.current_token.tag) {
        .dot => try parser.advance() orelse return null,
        .left_paren => return modules.parseDynamicImport(parser, name, null),
        else => {
            try parser.report(
                parser.current_token.span,
                "'import' keyword is not allowed here",
                .{ .help = "Use 'import.meta' for module metadata or " ++
                    "'import()' for dynamic imports." },
            );
            return null;
        },
    }

    switch (parser.current_token.tag) {
        inline .source, .@"defer" => |tag| {
            try parser.advance() orelse return null;
            const phase = @field(ast.ImportPhase, @tagName(tag));
            return modules.parseDynamicImport(parser, name, phase);
        },
        else => return parseMetaProperty(
            parser,
            name,
            "meta",
            "The only valid meta properties for 'import' are " ++
                "'import.meta', 'import.source()', or 'import.defer()'",
            "Did you mean 'import.meta', 'import.source(\"...\")' or 'import.defer(\"...\")'?",
        ),
    }
}

fn parseMetaProperty(
    parser: *Parser,
    meta: ast.NodeIndex,
    comptime property_name: []const u8,
    message: []const u8,
    help: []const u8,
) Error!?ast.NodeIndex {
    if (!std.mem.eql(u8, parser.current_token.text(parser.source), property_name)) {
        try parser.report(parser.current_token.span, message, .{ .help = help });
        return null;
    }

    const property = try literals.parseIdentifierName(parser) orelse return null;

    return try parser.tree.addNode(
        .{ .meta_property = .{ .meta = meta, .property = property } },
        .{ .start = parser.tree.span(meta).start, .end = parser.tree.span(property).end },
    );
}

fn parseNewExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .new);
    const start = parser.current_token.span.start;
    try parser.reportIfEscapedKeyword(parser.current_token);
    const new = try literals.parseIdentifierName(parser) orelse return null;

    if (parser.current_token.tag == .dot) {
        try parser.advance() orelse return null;
        return parseMetaProperty(
            parser,
            new,
            "target",
            "The only valid meta property for 'new' is 'new.target'",
            "Did you mean 'new.target'?",
        );
    }

    var callee: ast.NodeIndex = switch (parser.current_token.tag) {
        .left_paren => try parseParenthesizedExpression(parser) orelse return null,
        .new => try parseNewExpression(parser) orelse return null,
        else => try parsePrimaryExpression(parser, Precedence.New) orelse return null,
    };

    var type_arguments: ast.NodeIndex = .null;

    while (true) {
        // committed type arguments end the callee, unless a tagged template takes them
        if (type_arguments != .null and
            parser.current_token.tag != .template_head and
            parser.current_token.tag != .no_substitution_template)
        {
            break;
        }

        callee = switch (parser.current_token.tag) {
            .dot => try parseStaticMemberExpression(parser, callee) orelse return null,
            .left_bracket => try parseComputedMemberExpression(parser, callee, false) orelse
                return null,
            .template_head, .no_substitution_template => blk: {
                const tagged = try parseTaggedTemplateExpression(
                    parser,
                    callee,
                    type_arguments,
                ) orelse return null;
                type_arguments = .null;
                break :blk tagged;
            },
            // `new (a?.b)!()` is NewExpression(TSNonNull(a?.b), ()), the `!` binds to the callee
            .logical_not => if (parser.tree.isTs() and
                !parser.current_token.hasLineTerminatorBefore())
                try ts.parseNonNullExpression(parser, callee) orelse return null
            else
                break,
            .optional_chaining => {
                try parser.report(
                    parser.current_token.span,
                    "Optional chaining is not allowed in new expression",
                    .{ .help = "Remove the '?.' operator or use regular member access." },
                );
                return null;
            },
            .less_than, .left_shift => blk: {
                if (!parser.tree.isTs()) break;
                type_arguments = try ts.tryParseTypeArgumentsInExpression(parser);
                if (type_arguments == .null) break;
                break :blk callee;
            },
            else => break,
        };
    }

    if (parser.tree.data(callee) == .super) {
        try parser.report(
            parser.tree.span(callee),
            "'super' cannot be used as the target of 'new'",
            .{ .help = "Call the parent constructor with 'super()', or construct a" ++
                " parent member with 'new super.prop()'." },
        );
        return null;
    }

    const end_node = if (type_arguments != .null) type_arguments else callee;
    const arguments: Arguments = if (parser.current_token.tag == .left_paren)
        try parseArguments(
            parser,
            "Expected ')' after constructor arguments",
            "Constructor calls must end with ')'.",
        ) orelse return null
    else
        .{ .nodes = .empty, .end = parser.tree.span(end_node).end };

    return try parser.tree.addNode(.{ .new_expression = .{
        .callee = callee,
        .arguments = arguments.nodes,
        .type_arguments = type_arguments,
    } }, .{ .start = start, .end = arguments.end });
}

fn parseUpdateExpression(parser: *Parser, prefix: bool, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    try parser.advance() orelse return null;

    const argument = if (prefix)
        try parseExpression(parser, Precedence.Unary, .{}) orelse return null
    else
        left;
    const argument_span = parser.tree.span(argument);
    const unwrapped = ecmascript.unwrapParens(&parser.tree, argument);

    if (!isSimpleAssignmentTarget(parser, unwrapped)) {
        try parser.report(
            argument_span,
            "Invalid operand for increment/decrement operator",
            .{ .help = "The '++' and '--' operators require a variable or " ++
                "property reference, not a literal or complex expression." },
        );
        return null;
    }

    const span: ast.Span = if (prefix)
        .{ .start = operator_token.span.start, .end = argument_span.end }
    else
        .{ .start = argument_span.start, .end = operator_token.span.end };

    return try parser.tree.addNode(.{ .update_expression = .{
        .argument = unwrapped,
        .operator = ast.UpdateOperator.fromToken(operator_token.tag),
        .prefix = prefix,
    } }, span);
}

fn parseBinaryExpression(
    parser: *Parser,
    precedence: u8,
    left: ast.NodeIndex,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    const operator = ast.BinaryOperator.fromToken(parser.current_token.tag);
    try parser.advance() orelse return null;

    // `**` is right associative
    const next_precedence = if (operator == .exponent) precedence else precedence + 1;

    const right = try parseExpression(parser, next_precedence, options) orelse return null;

    if (parser.tree.data(right) == .private_identifier) {
        try parser.report(
            parser.tree.span(right),
            "Private names are only allowed in property accesses (`obj.#field`)" ++
                " or in `in` expressions (`#field in obj`)",
            .{},
        );
    }

    return try parser.tree.addNode(
        .{ .binary_expression = .{ .left = left, .right = right, .operator = operator } },
        .{ .start = parser.tree.span(left).start, .end = parser.tree.span(right).end },
    );
}

fn parseLogicalExpression(
    parser: *Parser,
    precedence: u8,
    left: ast.NodeIndex,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    const operator = ast.LogicalOperator.fromToken(parser.current_token.tag);
    try parser.advance() orelse return null;

    const right = try parseExpression(parser, precedence + 1, options) orelse return null;
    const span: ast.Span = .{
        .start = parser.tree.span(left).start,
        .end = parser.tree.span(right).end,
    };

    const left_data = parser.tree.data(left);
    const nested = if (left_data == .logical_expression) left_data else parser.tree.data(right);
    if (nested == .logical_expression) {
        const is_nullish = operator == .nullish_coalescing;
        if (is_nullish != (nested.logical_expression.operator == .nullish_coalescing)) {
            try parser.report(
                span,
                "Logical expressions and nullish coalescing cannot be mixed",
                .{ .help = "Wrap either expression in parentheses" },
            );
            return null;
        }
    }

    return try parser.tree.addNode(
        .{ .logical_expression = .{ .left = left, .right = right, .operator = operator } },
        span,
    );
}

fn parseSequenceExpression(
    parser: *Parser,
    precedence: u8,
    left: ast.NodeIndex,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);
    try parser.scratch_a.append(parser.allocator(), left);
    var last = left;

    while (parser.current_token.tag == .comma) {
        try parser.advance() orelse return null;
        last = try parseExpression(parser, precedence + 1, options) orelse return null;
        try parser.scratch_a.append(parser.allocator(), last);
    }

    return try parser.tree.addNode(
        .{ .sequence_expression = .{
            .expressions = try parser.flushToExtras(&parser.scratch_a, checkpoint),
        } },
        .{ .start = parser.tree.span(left).start, .end = parser.tree.span(last).end },
    );
}

fn parseAssignmentExpression(
    parser: *Parser,
    precedence: u8,
    left: ast.NodeIndex,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    const operator = ast.AssignmentOperator.fromToken(parser.current_token.tag);
    const left_span = parser.tree.span(left);
    var target = left;

    if (operator == .assign) {
        try grammar.expressionToPattern(parser, left, .assignable);
    } else if (isSimpleAssignmentTarget(parser, left)) {
        // mirror the paren-stripping that pattern conversion does for `=`
        target = ecmascript.unwrapParens(&parser.tree, left);
    } else {
        @branchHint(.unlikely);

        try parser.report(
            left_span,
            "Invalid assignment target",
            .{ .help = "The left-hand side of a compound assignment must be " ++
                "a variable or property access" },
        );
    }

    try parser.advance() orelse return null;

    // a paren stripped in the rhs must not displace the lhs target record, `[(a) = ((b) = c)]`
    const saved_stripped_paren = parser.state.stripped_paren;
    const right = try parseExpression(parser, precedence, options) orelse return null;
    parser.state.stripped_paren = saved_stripped_paren;

    return try parser.tree.addNode(
        .{ .assignment_expression = .{ .left = target, .right = right, .operator = operator } },
        .{ .start = left_span.start, .end = parser.tree.span(right).end },
    );
}

fn parseConditionalExpression(
    parser: *Parser,
    precedence: u8,
    @"test": ast.NodeIndex,
    options: ParseExpressionOptions,
) Error!?ast.NodeIndex {
    const test_span = parser.tree.span(@"test");
    try parser.advance() orelse return null;

    // the consequent is AssignmentExpression[+In]
    // an arrow return type can eat the ternary `:` in `c ? (a): T => b : d`,
    // so a consequent left without its `:` reparses with arrow return types restricted
    const consequent = if (!parser.tree.isTs())
        try parseExpression(parser, precedence, .{}) orelse return null
    else consequent: {
        const before = parser.checkpoint();
        const permissive = try parseExpression(parser, precedence, .{}) orelse return null;
        if (parser.current_token.tag == .colon) break :consequent permissive;

        parser.rewind(before);
        const saved_return_type = parser.ts_context.allow_arrow_return_type;
        parser.ts_context.allow_arrow_return_type = false;
        defer parser.ts_context.allow_arrow_return_type = saved_return_type;
        break :consequent try parseExpression(parser, precedence, .{}) orelse return null;
    };

    if (!try parser.expect(
        .colon,
        "Expected ':' after conditional expression consequent",
        "The ternary operator requires a colon (:) to separate " ++
            "the consequent and alternate expressions.",
    )) return null;

    // the alternate is AssignmentExpression[?In] and right associative
    const alternate = try parseExpression(parser, precedence, options) orelse return null;

    return try parser.tree.addNode(.{ .conditional_expression = .{
        .@"test" = @"test",
        .consequent = consequent,
        .alternate = alternate,
    } }, .{ .start = test_span.start, .end = parser.tree.span(alternate).end });
}

/// Returns whether `index` is a SimpleAssignmentTarget, looking through parentheses
/// and transparent TypeScript wrappers such as `as`, `satisfies`, `<T>`, and postfix `!`.
pub fn isSimpleAssignmentTarget(parser: *Parser, index: ast.NodeIndex) bool {
    return switch (parser.tree.data(index)) {
        .identifier_reference, .binding_identifier => true,
        .member_expression => |member| !member.optional,
        inline .parenthesized_expression,
        .ts_non_null_expression,
        .ts_as_expression,
        .ts_satisfies_expression,
        .ts_type_assertion,
        => |wrapper| isSimpleAssignmentTarget(parser, wrapper.expression),
        else => false,
    };
}

fn parseArrayExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_bracket);
    const cover = try array.parseCover(parser) orelse return null;
    return try array.coverToExpression(parser, cover);
}

fn parseObjectExpression(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_brace);
    const cover = try object.parseCover(parser) orelse return null;
    return try object.coverToExpression(parser, cover);
}

fn parseStaticMemberExpression(parser: *Parser, object_node: ast.NodeIndex) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .dot);
    try parser.advance() orelse return null;
    return parseMemberProperty(parser, object_node, false);
}

fn parseMemberProperty(
    parser: *Parser,
    object_node: ast.NodeIndex,
    optional: bool,
) Error!?ast.NodeIndex {
    const tag = parser.current_token.tag;

    const property = if (tag.isIdentifierLike())
        try literals.parseIdentifierName(parser) orelse return null
    else if (tag == .private_identifier)
        try literals.parsePrivateIdentifier(parser) orelse return null
    else {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected property name after '.'",
            .{ .help = "Use an identifier or private identifier (#name) for member access." },
        );
        return null;
    };

    return try parser.tree.addNode(.{ .member_expression = .{
        .object = object_node,
        .property = property,
        .computed = false,
        .optional = optional,
    } }, .{ .start = parser.tree.span(object_node).start, .end = parser.tree.span(property).end });
}

fn parseComputedMemberExpression(
    parser: *Parser,
    object_node: ast.NodeIndex,
    optional: bool,
) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_bracket);
    const open_bracket_span = parser.current_token.span;
    try parser.advance() orelse return null;

    const property = try parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    const end = parser.current_token.span.end;
    if (parser.current_token.tag != .right_bracket) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected ']' after computed property",
            .{
                .help = "Computed member access must end with ']'.",
                .labels = try parser.labels(&.{parser.label(open_bracket_span, "Opened here")}),
            },
        );
        return null;
    }
    try parser.advance() orelse return null;

    return try parser.tree.addNode(.{ .member_expression = .{
        .object = object_node,
        .property = property,
        .computed = true,
        .optional = optional,
    } }, .{ .start = parser.tree.span(object_node).start, .end = end });
}

/// Parses `callee(args)`. `type_arguments` is `.null` for a plain call.
pub fn parseCallExpression(
    parser: *Parser,
    callee_node: ast.NodeIndex,
    optional: bool,
    type_arguments: ast.NodeIndex,
) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .left_paren);
    const start = parser.tree.span(callee_node).start;
    const arguments = try parseArguments(
        parser,
        "Expected ')' after function arguments",
        "Function calls must end with ')'. Check for missing commas or unclosed parentheses.",
    ) orelse return null;

    return try parser.tree.addNode(.{ .call_expression = .{
        .callee = callee_node,
        .arguments = arguments.nodes,
        .optional = optional,
        .type_arguments = type_arguments,
    } }, .{ .start = start, .end = arguments.end });
}

const Arguments = struct { nodes: ast.IndexRange, end: u32 };

fn parseArguments(parser: *Parser, message: []const u8, help: []const u8) Error!?Arguments {
    std.debug.assert(parser.current_token.tag == .left_paren);
    const open_paren_span = parser.current_token.span;
    try parser.advance() orelse return null;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag != .right_paren and parser.current_token.tag != .eof) {
        const argument = if (parser.current_token.tag == .spread)
            try parseSpreadElement(parser) orelse return null
        else
            try parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        try parser.scratch_a.append(parser.allocator(), argument);

        if (parser.current_token.tag != .comma) break;
        try parser.advance() orelse return null;
    }

    const nodes = try parser.flushToExtras(&parser.scratch_a, checkpoint);
    const end = parser.current_token.span.end;
    if (parser.current_token.tag != .right_paren) {
        try parser.reportExpected(parser.current_token.span, message, .{
            .help = help,
            .labels = try parser.labels(&.{parser.label(open_paren_span, "Opened here")}),
        });
        return null;
    }
    try parser.advance() orelse return null;

    return .{ .nodes = nodes, .end = end };
}

/// Parses a `...argument` spread element.
pub fn parseSpreadElement(parser: *Parser) Error!?ast.NodeIndex {
    std.debug.assert(parser.current_token.tag == .spread);
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null;

    const argument = try parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

    return try parser.tree.addNode(
        .{ .spread_element = .{ .argument = argument } },
        .{ .start = start, .end = parser.tree.span(argument).end },
    );
}

/// Parses a tagged template. `type_arguments` is `.null` for a plain tag.
pub fn parseTaggedTemplateExpression(
    parser: *Parser,
    tag_node: ast.NodeIndex,
    type_arguments: ast.NodeIndex,
) Error!?ast.NodeIndex {
    const start = parser.tree.span(tag_node).start;

    const quasi = if (parser.current_token.tag == .no_substitution_template)
        try literals.parseNoSubstitutionTemplate(parser, true) orelse return null
    else
        try literals.parseTemplateLiteral(parser, true) orelse return null;

    return try parser.tree.addNode(.{ .tagged_template_expression = .{
        .tag = tag_node,
        .quasi = quasi,
        .type_arguments = type_arguments,
    } }, .{ .start = start, .end = parser.tree.span(quasi).end });
}

fn parseOptionalChain(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const is_ts = parser.tree.isTs();
    const chain_start = parser.tree.span(left).start;
    try parser.advance() orelse return null;

    var expr = try parseOptionalChainElement(parser, left) orelse return null;

    while (true) {
        switch (parser.current_token.tag) {
            .dot => expr = try parseStaticMemberExpression(parser, expr) orelse return null,
            .left_bracket => expr = try parseComputedMemberExpression(parser, expr, false) orelse
                return null,
            .left_paren => expr = try parseCallExpression(parser, expr, false, .null) orelse
                return null,
            .optional_chaining => {
                try parser.advance() orelse return null;
                expr = try parseOptionalChainElement(parser, expr) orelse return null;
            },
            // `m?.[0]!` is ChainExpression(TSNonNullExpression(m?.[0]))
            .logical_not => {
                if (!is_ts or parser.current_token.hasLineTerminatorBefore()) break;
                expr = try ts.parseNonNullExpression(parser, expr) orelse return null;
            },
            // a generic call stays in the chain, bare type arguments close it
            .less_than, .left_shift => {
                if (!is_ts) break;
                const checkpoint = parser.checkpoint();
                const type_arguments = try ts.tryParseTypeArgumentsInExpression(parser);
                if (type_arguments == .null) break;
                if (parser.current_token.tag != .left_paren) {
                    parser.rewind(checkpoint);
                    break;
                }
                expr = try parseCallExpression(parser, expr, false, type_arguments) orelse
                    return null;
            },
            .template_head, .no_substitution_template => {
                try parser.report(
                    parser.current_token.span,
                    "Tagged template expressions are not permitted in an optional chain",
                    .{ .help = "Remove the optional chaining operator '?.' before " ++
                        "the template literal or add parentheses." },
                );
                break;
            },
            else => break,
        }
    }

    return try parser.tree.addNode(
        .{ .chain_expression = .{ .expression = expr } },
        .{ .start = chain_start, .end = parser.tree.span(expr).end },
    );
}

// caller already consumed the `?.`
fn parseOptionalChainElement(parser: *Parser, object_node: ast.NodeIndex) Error!?ast.NodeIndex {
    const tag = parser.current_token.tag;

    if (tag.isIdentifierLike() or tag == .private_identifier) {
        return parseMemberProperty(parser, object_node, true);
    }

    // only a generic call follows `?.` directly, `a?.<T>` alone or before a template rewinds
    if (parser.tree.isTs() and ts.isAngleOpen(tag)) {
        const checkpoint = parser.checkpoint();
        const type_arguments = try ts.tryParseTypeArgumentsInExpression(parser);
        if (type_arguments != .null and parser.current_token.tag == .left_paren) {
            return parseCallExpression(parser, object_node, true, type_arguments);
        }
        parser.rewind(checkpoint);
    }

    return switch (tag) {
        .left_bracket => parseComputedMemberExpression(parser, object_node, true),
        .left_paren => parseCallExpression(parser, object_node, true, .null),
        .template_head, .no_substitution_template => {
            try parser.report(
                parser.current_token.span,
                "Tagged template expressions are not permitted in an optional chain",
                .{ .help = "Remove the optional chaining operator '?.' " ++
                    "before the template literal." },
            );
            return null;
        },
        else => {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected property name, '[', or '(' after '?.'",
                .{ .help = "Optional chaining must be followed by property access (.x), " ++
                    "computed access ([x]), or a call (())." },
            );
            return null;
        },
    };
}

pub const LeftHandSideContext = enum {
    /// `[k]` is a computed member access.
    extends_clause,
    /// `[k]` begins the next class element's key.
    decorator,
};

/// Parses a LeftHandSideExpression.
/// https://tc39.es/ecma262/#prod-LeftHandSideExpression
pub fn parseLeftHandSideExpression(
    parser: *Parser,
    context: LeftHandSideContext,
) Error!?ast.NodeIndex {
    var expr: ast.NodeIndex = switch (parser.current_token.tag) {
        .left_paren => try parseParenthesizedExpression(parser) orelse return null,
        .new => try parseNewExpression(parser) orelse return null,
        .import => try parseImportExpression(parser) orelse return null,
        else => try parsePrimaryExpression(parser, Precedence.Call) orelse return null,
    };

    const is_ts = parser.tree.isTs();

    while (true) {
        expr = switch (parser.current_token.tag) {
            .dot => try parseStaticMemberExpression(parser, expr) orelse return null,
            .left_bracket => switch (context) {
                .extends_clause => try parseComputedMemberExpression(parser, expr, false) orelse
                    return null,
                .decorator => break,
            },
            .left_paren => try parseCallExpression(parser, expr, false, .null) orelse return null,
            .template_head, .no_substitution_template => try parseTaggedTemplateExpression(
                parser,
                expr,
                .null,
            ) orelse return null,
            .optional_chaining => try parseOptionalChain(parser, expr) orelse return null,
            .logical_not => if (is_ts and !parser.current_token.hasLineTerminatorBefore())
                try ts.parseNonNullExpression(parser, expr) orelse return null
            else
                break,
            .less_than, .left_shift => if (is_ts)
                (try ts.parseTypeArgumentedCallOrInstantiation(parser, expr)) orelse break
            else
                break,
            else => break,
        };
    }

    return expr;
}

inline fn maxLeftPrecedence(data: ast.NodeData) u8 {
    return switch (data) {
        .arrow_function_expression, .yield_expression => Precedence.Comma,
        .update_expression,
        .unary_expression,
        .await_expression,
        .binary_expression,
        .logical_expression,
        .conditional_expression,
        .assignment_expression,
        .sequence_expression,
        .ts_as_expression,
        .ts_satisfies_expression,
        => Precedence.Unary,
        else => std.math.maxInt(u8),
    };
}
