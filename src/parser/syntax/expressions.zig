// pure pratt parser for javascript expressions:
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_precedence#table
// https://tdop.github.io/

const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const TokenTag = @import("../token.zig").TokenTag;
const Token = @import("../token.zig").Token;
const std = @import("std");
const Precedence = @import("../token.zig").Precedence;

const jsx = @import("jsx/root.zig");
const statements = @import("statements.zig");
const variables = @import("variables.zig");
const array = @import("array.zig");
const object = @import("object.zig");
const literals = @import("literals.zig");
const functions = @import("functions.zig");
const class = @import("class.zig");
const extensions = @import("extensions.zig");
const parenthesized = @import("parenthesized.zig");
const patterns = @import("patterns.zig");
const modules = @import("modules.zig");
const grammar = @import("../grammar.zig");
const ts = @import("ts/types.zig");

const ParseExpressionOpts = struct {
    /// whether we are parsing this expression in a cover context.
    /// when true, we don't treat the expressions as patterns and also don't decide whether to parse them as patterns
    /// until the top level context is known after the cover is parsed.
    in_cover: bool = false,
    /// whether to stop at `in` when `allow_in` is disabled. only the direct caller
    /// that sets `allow_in = false` should pass `true` here, recursive calls (inside
    /// parentheses, computed members, etc.) leave this `false` so `in` is parsed as
    /// a binary operator normally.
    respect_allow_in: bool = false,
};

pub fn parseExpression(parser: *Parser, min_precedence: u8, opts: ParseExpressionOpts) Error!?ast.NodeIndex {
    var left = try parsePrefix(parser, opts, min_precedence) orelse return null;
    const is_ts = parser.tree.isTs();

    while (true) {
        const current_token = parser.current_token;

        // `<` is dispatched before the binary precedence gate so it
        // can act as a call-level postfix instead of relational.
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

        // for example:
        //  a++()        <- can't call an update expression
        //  () => {}()   <- can't call an arrow function
        // breaking here produces natural "expected semicolon" error
        if (infix_precedence > maxLeftPrecedence(parser.tree.data(left))) break;

        if (opts.respect_allow_in and current_token.tag == .in and !parser.context.in) break;

        left = try parseInfix(parser, infix_precedence, left) orelse return null;
    }

    return left;
}

/// precedence of `token` as an infix operator. the token mask records the
/// infix precedence. a few infix-capable tokens degrade
/// to 0 here to encode the ecmascript "[no LineTerminator here]"
/// restricted productions that would otherwise let the pratt loop fold
/// two statements into one under ASI:
///
/// - `a [no LineTerminator here] ++` / `--` (postfix update).
/// - `a [no LineTerminator here] as T` / `satisfies T` (ts narrowing).
/// - `a [no LineTerminator here] !` (ts non-null assertion).
/// `!` is also infix only in TypeScript, in plain JS it is purely prefix.
///
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

fn parsePrefix(parser: *Parser, opts: ParseExpressionOpts, precedence: u8) Error!?ast.NodeIndex {
    const tag = parser.current_token.tag;

    // regular identifiers
    if (tag == .identifier) {
        @branchHint(.likely);
        return literals.parseIdentifier(parser);
    }

    if (tag == .increment or tag == .decrement) {
        return parseUpdateExpression(parser, true, .null);
    }

    if (tag == .at) {
        const start = parser.current_token.span.start;
        const decorators = try extensions.parseDecorators(parser) orelse return null;
        return class.parseClassDecorated(parser, .{ .is_expression = true }, start, decorators);
    }

    if (tag.isUnaryOperator()) {
        return parseUnaryExpression(parser);
    }

    if (tag == .left_paren) {
        return parseParenthesizedOrArrowFunction(parser, null, precedence);
    }

    if (tag == .await and parser.context.@"await") {
        const await_start = parser.current_token.span.start;
        try parser.advance() orelse return null; // consume 'await'
        return parseAwaitExpression(parser, await_start);
    }

    if (tag == .yield and parser.context.yield and precedence < Precedence.Additive) {
        return parseYieldExpression(parser);
    }

    if (tag == .new) {
        return parseNewExpression(parser);
    }

    if (tag == .import) {
        return parseImportExpression(parser, null);
    }

    if (tag == .async) {
        return parseAsyncFunctionOrArrow(parser, precedence);
    }

    if (tag == .less_than) {
        if (parser.tree.isTs()) {
            const start = parser.current_token.span.start;
            if (try ts.tryParseGenericArrow(parser, false, start)) |arrow| return arrow;
            if (!parser.tree.isJsx()) return ts.parseTypeAssertion(parser);
        }
        if (parser.tree.isJsx()) return jsx.parseJsxExpression(parser);
    }

    return parsePrimaryExpression(parser, opts);
}

fn parseInfix(parser: *Parser, precedence: u8, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const current = parser.current_token;

    switch (current.tag) {
        .as, .satisfies => if (parser.tree.isTs()) return ts.parseAsOrSatisfiesExpression(parser, left),
        .logical_not => if (parser.tree.isTs() and !current.hasLineTerminatorBefore()) return ts.parseNonNullExpression(parser, left),
        else => {},
    }

    if (current.tag.isBinaryOperator()) {
        return parseBinaryExpression(parser, precedence, left);
    }

    if (current.tag.isLogicalOperator()) {
        return parseLogicalExpression(parser, precedence, left);
    }

    if (current.tag.isAssignmentOperator()) {
        return parseAssignmentExpression(parser, precedence, left);
    }

    if (current.tag == .arrow and !current.hasLineTerminatorBefore()) {
        return parseSimpleArrowFunction(parser, left);
    }

    switch (current.tag) {
        .increment, .decrement => return parseUpdateExpression(parser, false, left),
        .question => return parseConditionalExpression(parser, precedence, left),
        .comma => return parseSequenceExpression(parser, precedence, left),
        .dot => return parseStaticMemberExpression(parser, left, false),
        .left_bracket => return parseComputedMemberExpression(parser, left, false),
        .left_paren => return parseCallExpression(parser, left, false, .null),
        .template_head, .no_substitution_template => return parseTaggedTemplateExpression(parser, left, .null),
        .optional_chaining => return parseOptionalChain(parser, left),
        else => {},
    }

    try parser.report(
        current.span,
        try parser.fmt("Unexpected token '{s}' in expression", .{parser.describeToken(current)}),
        .{ .help = "This token cannot be used here. Expected an operator, semicolon, or end of expression." },
    );

    return null;
}

pub inline fn parsePrimaryExpression(parser: *Parser, opts: ParseExpressionOpts) Error!?ast.NodeIndex {
    if (parser.current_token.tag.isNumericLiteral()) {
        return literals.parseNumericLiteral(parser);
    }

    return switch (parser.current_token.tag) {
        .private_identifier => blk: {
            const node = try literals.parsePrivateIdentifier(parser) orelse break :blk null;

            if (parser.current_token.tag != .in) {
                try parser.report(
                    parser.tree.span(node),
                    "Private names are only valid in property accesses (`obj.#field`) or `in` expressions (`#field in obj`)",
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
        .left_bracket => parseArrayExpression(parser, opts.in_cover),
        .left_brace => parseObjectExpression(parser, opts.in_cover),
        .function => functions.parseFunction(parser, .{ .is_expression = true }, null),
        .class => class.parseClass(parser, .{ .is_expression = true }, null),
        else => {
            // contextual keywords used as identifiers (let, as, from, get, set, etc.)
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

// parse only (a), not arrow, this function is used in the 'new' expression parsing
// where we only need parenthesized
fn parseParenthesizedExpression(parser: *Parser) Error!?ast.NodeIndex {
    const cover = try parenthesized.parseCover(parser) orelse return null;

    return parenthesized.coverToParenthesizedExpression(parser, cover);
}

/// (a) or (a, b) => ...
fn parseParenthesizedOrArrowFunction(parser: *Parser, arrow_start: ?u32, precedence: u8) Error!?ast.NodeIndex {
    const start = arrow_start orelse parser.current_token.span.start;

    // tristate dispatch for `(params) => body`. `.no` falls through to
    // the js cover grammar below.
    if (parser.tree.isTs() and precedence <= Precedence.Assignment) switch (ts.classifyArrowHead(parser)) {
        .yes => return ts.parseArrow(parser, false, start),
        .maybe => if (try ts.tryParseArrow(parser, false, start)) |arrow| return arrow,
        .no => {},
    };

    const cover = try parenthesized.parseCover(parser) orelse return null;

    // every ts arrow went through the tristate above. only js-shaped
    // arrows (`(a, b) => body`) reach here, so no `:` return-type parse
    // is needed. a stray `:` after the cover belongs to the outer
    // context (ternary else, case label, ...).
    if (parser.current_token.tag == .arrow and !parser.current_token.hasLineTerminatorBefore() and precedence <= Precedence.Assignment) {
        return parenthesized.coverToArrowFunction(parser, cover, false, start);
    }

    return parenthesized.coverToParenthesizedExpression(parser, cover);
}

/// x => ...
fn parseSimpleArrowFunction(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const data = parser.tree.data(left);

    if (data != .identifier_reference) {
        try parser.report(parser.tree.span(left), "Unexpected '=>'", .{
            .help = "'=>' is only valid after a single identifier or a parenthesized parameter list",
        });
        return null;
    }

    return parenthesized.identifierToArrowFunction(parser, left, false, parser.tree.span(left).start);
}

/// async function or async arrow function
fn parseAsyncFunctionOrArrow(parser: *Parser, precedence: u8) Error!?ast.NodeIndex {
    const is_escaped = parser.current_token.isEscaped();

    const async_id = try literals.parseIdentifier(parser) orelse return null;
    const async_span = parser.tree.span(async_id);

    // every form below requires no line break between `async` and the
    // following token. when there is one, `async` stays as an identifier.
    if (parser.current_token.hasLineTerminatorBefore()) return async_id;

    const next = parser.current_token;

    // async function ...
    if (next.tag == .function) {
        if (is_escaped) try parser.reportEscapedKeyword(async_span);
        return functions.parseFunction(parser, .{ .is_expression = true, .is_async = true }, async_span.start);
    }

    // async (params) => ...
    if (next.tag == .left_paren) {
        return parseAsyncArrowFunctionOrCall(parser, async_span, async_id, precedence, is_escaped);
    }

    // async <T>(params) => ...
    if (parser.tree.isTs() and next.tag == .less_than) {
        if (try ts.tryParseGenericArrow(parser, true, async_span.start)) |arrow| {
            if (is_escaped) try parser.reportEscapedKeyword(async_span);
            return arrow;
        }
    }

    // async ident => body
    if (next.tag.isIdentifierLike()) {
        const after_id = parser.peekAhead() orelse return null;
        if (after_id.tag == .arrow and !after_id.hasLineTerminatorBefore()) {
            if (is_escaped) try parser.reportEscapedKeyword(async_span);
            const id = try literals.parseIdentifier(parser) orelse return null;
            return parenthesized.identifierToArrowFunction(parser, id, true, async_span.start);
        }
    }

    return async_id;
}

fn parseAsyncArrowFunctionOrCall(parser: *Parser, async_span: ast.Span, async_id: ast.NodeIndex, precedence: u8, is_escaped_async: bool) Error!?ast.NodeIndex {
    const start = async_span.start;

    // tristate dispatch for `async (params) => body`. `.no` falls
    // through to the js cover path below, which also handles
    // `async(args)` as a call expression
    if (parser.tree.isTs() and precedence <= Precedence.Assignment) switch (ts.classifyArrowHead(parser)) {
        .yes => {
            if (is_escaped_async) try parser.reportEscapedKeyword(async_span);
            return ts.parseArrow(parser, true, start);
        },
        .maybe => if (try ts.tryParseArrow(parser, true, start)) |arrow| {
            if (is_escaped_async) try parser.reportEscapedKeyword(async_span);
            return arrow;
        },
        .no => {},
    };

    const saved_await_is_keyword = parser.context.@"await";

    parser.context.@"await" = true;

    defer parser.context.@"await" = saved_await_is_keyword;

    const cover = try parenthesized.parseCover(parser) orelse return null;

    // js-shaped async arrow or a plain `async(args)` call. every ts
    // variant was handled by the tristate above, so no `:` return-type
    // parse is needed here.
    if (parser.current_token.tag == .arrow and !parser.current_token.hasLineTerminatorBefore() and precedence <= Precedence.Assignment) {
        if (is_escaped_async) try parser.reportEscapedKeyword(async_span);
        return parenthesized.coverToArrowFunction(parser, cover, true, start);
    }

    return parenthesized.coverToCallExpression(parser, cover, async_id);
}

fn parseUnaryExpression(parser: *Parser) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    try parser.advance() orelse return null;

    const argument = try parseExpression(parser, Precedence.Unary, .{}) orelse return null;
    const argument_span = parser.tree.span(argument);

    if (parser.current_token.tag == .exponent) {
        try parser.report(
            .{ .start = operator_token.span.start, .end = argument_span.end },
            "The left-hand side of '**' cannot be an unparenthesized unary expression",
            .{ .help = "Add parentheses to make precedence explicit, for example `(-x) ** y` or `-(x ** y)`." },
        );
    }

    return try parser.tree.addNode(
        .{
            .unary_expression = .{
                .argument = argument,
                .operator = ast.UnaryOperator.fromToken(operator_token.tag),
            },
        },
        .{ .start = operator_token.span.start, .end = argument_span.end },
    );
}

/// `await expression`
pub fn parseAwaitExpression(parser: *Parser, await_start: u32) Error!?ast.NodeIndex {
    const argument = try parseExpression(parser, Precedence.Unary, .{}) orelse return null;
    const argument_span = parser.tree.span(argument);

    if (parser.current_token.tag == .exponent) {
        try parser.report(
            .{ .start = await_start, .end = argument_span.end },
            "The left-hand side of '**' cannot be an unparenthesized await expression",
            .{ .help = "Add parentheses to make precedence explicit, for example `(await x) ** y`." },
        );
    }

    return try parser.tree.addNode(
        .{ .await_expression = .{ .argument = argument } },
        .{ .start = await_start, .end = argument_span.end },
    );
}

/// Section 15.5 Yield Expression
/// yield
/// yield [no `LineTerminator` here] `AssignmentExpression`
/// yield [no `LineTerminator` here] * `AssignmentExpression`
fn parseYieldExpression(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;
    try parser.advance() orelse return null;

    var delegate = false;

    var argument: ast.NodeIndex = .null;

    if (parser.current_token.tag == .star and !parser.current_token.hasLineTerminatorBefore()) {
        delegate = true;
        end = parser.current_token.span.end;
        try parser.advance() orelse return null;
    }

    if (!parser.current_token.hasLineTerminatorBefore()) {
        const can_start_yield_argument = switch (parser.current_token.tag) {
            .eof, .right_brace, .right_paren, .right_bracket, .colon, .comma, .semicolon => false,
            else => true,
        };

        if (can_start_yield_argument) {
            // YieldExpression[?In]: yield [no LT] AssignmentExpression[?In]
            const expr = try parseExpression(parser, Precedence.Assignment, .{ .respect_allow_in = true }) orelse return null;

            argument = expr;

            end = parser.tree.span(argument).end;
        }
    }

    if (delegate and argument == .null) {
        try parser.reportExpected(parser.current_token.span, "Expected expression after 'yield*'", .{});
        return null;
    }

    return try parser.tree.addNode(
        .{ .yield_expression = .{ .argument = argument, .delegate = delegate } },
        .{ .start = start, .end = end },
    );
}

/// `this`
fn parseThisExpression(parser: *Parser) Error!?ast.NodeIndex {
    const this_token = parser.current_token;
    try parser.advance() orelse return null; // consume 'this'
    return try parser.tree.addNode(.{ .this_expression = .{} }, this_token.span);
}

/// `super`
fn parseSuperExpression(parser: *Parser) Error!?ast.NodeIndex {
    const super_token = parser.current_token;
    try parser.advance() orelse return null; // consume 'super'
    if (parser.current_token.tag != .left_paren and parser.current_token.tag != .dot and parser.current_token.tag != .left_bracket) {
        try parser.report(parser.current_token.span, "'super' must be followed by a call or property access", .{ .help = "use 'super()' to call parent constructor, 'super.property' or 'super[property]' to access parent members" });
        return null;
    }
    return try parser.tree.addNode(.{ .super = .{} }, super_token.span);
}

/// `import.meta` or `import(...)`
pub fn parseImportExpression(parser: *Parser, name_from_param: ?ast.NodeIndex) Error!?ast.NodeIndex {
    // in this grammar position, `import` is a keyword (`import.meta` / `import(...)`).
    // we still represent it as IdentifierName in the AST, so escaped forms must be rejected here.
    try parser.reportIfEscapedKeyword(parser.current_token);

    const name = name_from_param orelse try literals.parseIdentifierName(parser) orelse return null;

    return switch (parser.current_token.tag) {
        .dot => parseImportMetaOrPhaseImport(parser, name),
        .left_paren => modules.parseDynamicImport(parser, name, null),
        else => {
            try parser.report(
                parser.current_token.span,
                "'import' keyword is not allowed here",
                .{ .help = "Use 'import.meta' for module metadata or 'import()' for dynamic imports." },
            );
            return null;
        },
    };
}

/// `import.meta`, `import.source()`, or `import.defer()`
fn parseImportMetaOrPhaseImport(parser: *Parser, name: ast.NodeIndex) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume '.'

    const name_span = parser.tree.span(name);

    // import.source() or import.defer()
    if (parser.current_token.tag == .source) {
        try parser.advance() orelse return null; // consume 'source'
        return modules.parseDynamicImport(parser, name, .source);
    }

    if (parser.current_token.tag == .@"defer") {
        try parser.advance() orelse return null; // consume 'defer'
        return modules.parseDynamicImport(parser, name, .@"defer");
    }

    // import.meta
    if (parser.current_token.tag != .identifier or !std.mem.eql(u8, parser.current_token.text(parser.source), "meta")) {
        try parser.report(
            parser.current_token.span,
            "The only valid meta properties for 'import' are 'import.meta', 'import.source()', or 'import.defer()'",
            .{ .help = "Did you mean 'import.meta', 'import.source(\"...\")' or 'import.defer(\"...\")'?" },
        );
        return null;
    }

    const property = try literals.parseIdentifierName(parser) orelse return null; // consume 'meta'

    return try parser.tree.addNode(
        .{ .meta_property = .{ .meta = name, .property = property } },
        .{ .start = name_span.start, .end = parser.tree.span(property).end },
    );
}

/// `new.target`
fn parseNewTarget(parser: *Parser, name: ast.NodeIndex) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume '.'

    if (!std.mem.eql(u8, parser.current_token.text(parser.source), "target")) {
        try parser.report(
            parser.current_token.span,
            "The only valid meta property for 'new' is 'new.target'",
            .{ .help = "Did you mean 'new.target'?" },
        );
        return null;
    }

    const property = try literals.parseIdentifierName(parser) orelse return null; // consume 'target'

    return try parser.tree.addNode(
        .{ .meta_property = .{ .meta = name, .property = property } },
        .{ .start = parser.tree.span(name).start, .end = parser.tree.span(property).end },
    );
}

/// `new Callee`, `new Callee(args)`, or `new.target`
fn parseNewExpression(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.checkEscapedKeyword();
    const new = try literals.parseIdentifierName(parser) orelse return null; // consume 'new'

    // check for new.target
    if (parser.current_token.tag == .dot) {
        return parseNewTarget(parser, new);
    }

    var callee: ast.NodeIndex = blk: {
        // parenthesized, allows any expression inside
        if (parser.current_token.tag == .left_paren) {
            break :blk try parseParenthesizedExpression(parser) orelse return null;
        }

        // `new new Foo()`
        if (parser.current_token.tag == .new) {
            break :blk try parseNewExpression(parser) orelse return null;
        }

        // otherwise, start with a primary expression
        break :blk try parsePrimaryExpression(parser, .{}) orelse return null;
    };

    // member expression chain (. [] and tagged templates)
    while (true) {
        callee = switch (parser.current_token.tag) {
            .dot => try parseStaticMemberExpression(parser, callee, false) orelse return null,
            .left_bracket => try parseComputedMemberExpression(parser, callee, false) orelse return null,
            .template_head, .no_substitution_template => try parseTaggedTemplateExpression(parser, callee, .null) orelse return null,
            .optional_chaining => {
                try parser.report(
                    parser.current_token.span,
                    "Optional chaining is not allowed in new expression",
                    .{ .help = "Remove the '?.' operator or use regular member access." },
                );
                return null;
            },
            else => break,
        };
    }

    // `new Foo<T>(...)` or `new Foo<T>`. parse `<T>` speculatively. on
    // commit, the args fold straight into the `NewExpression`
    const type_arguments = if (parser.tree.isTs())
        try ts.tryParseTypeArgumentsInExpression(parser)
    else
        ast.NodeIndex.null;

    // optional arguments
    var arguments = ast.IndexRange.empty;

    const end = if (parser.current_token.tag == .left_paren) blk: {
        const open_paren_span = parser.current_token.span;
        try parser.advance() orelse return null;
        arguments = try parseArguments(parser) orelse return null;
        const arguments_end = parser.current_token.span.end;

        if (parser.current_token.tag != .right_paren) {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected ')' after constructor arguments",
                .{
                    .help = "Constructor calls must end with ')'.",
                    .labels = try parser.labels(&.{parser.label(open_paren_span, "Opened here")}),
                },
            );
            return null;
        }
        try parser.advance() orelse return null; // consume ')'

        break :blk arguments_end;
    } else if (type_arguments != .null)
        parser.tree.span(type_arguments).end
    else
        parser.tree.span(callee).end;

    return try parser.tree.addNode(
        .{ .new_expression = .{ .callee = callee, .arguments = arguments, .type_arguments = type_arguments } },
        .{ .start = start, .end = end },
    );
}

fn parseUpdateExpression(parser: *Parser, prefix: bool, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.UpdateOperator.fromToken(operator_token.tag);
    try parser.advance() orelse return null;

    if (prefix) {
        const argument = try parseExpression(parser, Precedence.Unary, .{}) orelse return null;
        const span = parser.tree.span(argument);

        const unwrapped = parenthesized.unwrapParens(parser, argument);

        if (!isSimpleAssignmentTarget(parser, unwrapped)) {
            try parser.report(
                span,
                "Invalid operand for increment/decrement operator",
                .{ .help = "The '++' and '--' operators require a variable or property reference, not a literal or complex expression." },
            );
            return null;
        }

        return try parser.tree.addNode(
            .{ .update_expression = .{ .argument = unwrapped, .operator = operator, .prefix = true } },
            .{ .start = operator_token.span.start, .end = span.end },
        );
    }

    const unwrapped = parenthesized.unwrapParens(parser, left);

    if (!isSimpleAssignmentTarget(parser, unwrapped)) {
        const span = parser.tree.span(left);
        try parser.report(
            span,
            "Invalid operand for increment/decrement operator",
            .{ .help = "The '++' and '--' operators require a variable or property reference, not a literal or complex expression." },
        );
        return null;
    }

    return try parser.tree.addNode(
        .{ .update_expression = .{ .argument = unwrapped, .operator = operator, .prefix = false } },
        .{ .start = parser.tree.span(left).start, .end = operator_token.span.end },
    );
}

fn parseBinaryExpression(parser: *Parser, precedence: u8, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.BinaryOperator.fromToken(operator_token.tag);
    try parser.advance() orelse return null;

    // '**' is right-associative
    const next_precedence = if (operator == .exponent) precedence else precedence + 1;

    const right = try parseExpression(parser, next_precedence, .{}) orelse return null;

    if (parser.tree.data(right) == .private_identifier) {
        try parser.report(
            parser.tree.span(right),
            "Private names are only allowed in property accesses (`obj.#field`) or in `in` expressions (`#field in obj`)",
            .{},
        );
    }

    return try parser.tree.addNode(
        .{ .binary_expression = .{ .left = left, .right = right, .operator = operator } },
        .{ .start = parser.tree.span(left).start, .end = parser.tree.span(right).end },
    );
}

fn parseLogicalExpression(parser: *Parser, precedence: u8, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    try parser.advance() orelse return null;

    const right = try parseExpression(parser, precedence + 1, .{}) orelse return null;
    const current_operator = ast.LogicalOperator.fromToken(operator_token.tag);

    // check for operator mixing: can't mix ?? with && or ||
    const left_data = parser.tree.data(left);
    const right_data = parser.tree.data(right);

    if (left_data == .logical_expression or right_data == .logical_expression) {
        const operator_to_check = if (left_data == .logical_expression) left_data.logical_expression.operator else right_data.logical_expression.operator;

        if ((current_operator == .nullish_coalescing) != (operator_to_check == .nullish_coalescing)) {
            const left_span = parser.tree.span(left);
            try parser.report(
                .{ .start = left_span.start, .end = parser.tree.span(right).end },
                "Logical expressions and nullish coalescing cannot be mixed",
                .{ .help = "Wrap either expression in parentheses" },
            );
            return null;
        }
    }

    return try parser.tree.addNode(
        .{
            .logical_expression = .{
                .left = left,
                .right = right,
                .operator = current_operator,
            },
        },
        .{ .start = parser.tree.span(left).start, .end = parser.tree.span(right).end },
    );
}

/// `a, b, c` - comma operator / sequence expression
fn parseSequenceExpression(parser: *Parser, precedence: u8, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);
    try parser.scratch_a.append(parser.allocator(), left);
    var last = left;

    while (parser.current_token.tag == .comma) {
        try parser.advance() orelse return null; // consume ','

        const expr = try parseExpression(parser, precedence + 1, .{}) orelse return null;
        try parser.scratch_a.append(parser.allocator(), expr);
        last = expr;
    }

    return try parser.tree.addNode(
        .{ .sequence_expression = .{ .expressions = try parser.addExtraFromScratch(&parser.scratch_a, checkpoint) } },
        .{ .start = parser.tree.span(left).start, .end = parser.tree.span(last).end },
    );
}

fn parseAssignmentExpression(parser: *Parser, precedence: u8, left_in: ast.NodeIndex) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.AssignmentOperator.fromToken(operator_token.tag);

    const left_span = parser.tree.span(left_in);
    var left = left_in;

    if (operator == .assign) {
        try grammar.expressionToPattern(parser, left, .assignable);
    } else if (isSimpleAssignmentTarget(parser, left)) {
        // mirror the paren-stripping that pattern conversion does for `=`
        left = parenthesized.unwrapParens(parser, left);
    } else {
        @branchHint(.unlikely);

        try parser.report(
            left_span,
            "Invalid assignment target",
            .{ .help = "The left-hand side of a compound assignment must be a variable or property access" },
        );
    }

    try parser.advance() orelse return null;

    const right = try parseExpression(parser, precedence, .{}) orelse return null;

    return try parser.tree.addNode(
        .{ .assignment_expression = .{ .left = left, .right = right, .operator = operator } },
        .{ .start = left_span.start, .end = parser.tree.span(right).end },
    );
}

/// `test ? consequent : alternate`
fn parseConditionalExpression(parser: *Parser, precedence: u8, @"test": ast.NodeIndex) Error!?ast.NodeIndex {
    const test_span = parser.tree.span(@"test");

    try parser.advance() orelse return null; // consume '?'

    // ConditionalExpression[?In]: ... ? AssignmentExpression[+In] : AssignmentExpression[?In]
    // consequent gets [+In]: `in` is always allowed, and ts arrows with a
    // return type must defer to the ternary `:` when they would eat it.
    const saved_allow_in = parser.context.in;
    const saved_allow_arrow_return_type = parser.context.allow_arrow_return_type;
    parser.context.in = true;
    parser.context.allow_arrow_return_type = false;

    const consequent = try parseExpression(parser, precedence, .{}) orelse return null;

    parser.context.in = saved_allow_in;
    parser.context.allow_arrow_return_type = saved_allow_arrow_return_type;

    if (!try parser.expect(.colon, "Expected ':' after conditional expression consequent", "The ternary operator requires a colon (:) to separate the consequent and alternate expressions.")) return null;

    // alternate gets [?In]: propagate the current `allow_in` restriction
    // right-associative, so same prec, not precedence + 1
    const alternate = try parseExpression(parser, precedence, .{ .respect_allow_in = true }) orelse return null;

    return try parser.tree.addNode(
        .{
            .conditional_expression = .{
                .@"test" = @"test",
                .consequent = consequent,
                .alternate = alternate,
            },
        },
        .{ .start = test_span.start, .end = parser.tree.span(alternate).end },
    );
}

/// SimpleAssignmentTarget: identifier or member expression, optionally
/// wrapped in parens or a transparent TS type modifier such as `as`,
/// `satisfies`, `<T>`, or postfix `!`. These wrappers do not change the
/// LHS-ness of the inner expression.
pub fn isSimpleAssignmentTarget(parser: *Parser, index: ast.NodeIndex) bool {
    return switch (parser.tree.data(index)) {
        .identifier_reference, .binding_identifier => true,
        .member_expression => |m| !m.optional, // optional chaining is not a valid assignment target
        .parenthesized_expression => |p| isSimpleAssignmentTarget(parser, p.expression),
        .ts_non_null_expression => |n| isSimpleAssignmentTarget(parser, n.expression),
        .ts_as_expression => |a| isSimpleAssignmentTarget(parser, a.expression),
        .ts_satisfies_expression => |s| isSimpleAssignmentTarget(parser, s.expression),
        .ts_type_assertion => |t| isSimpleAssignmentTarget(parser, t.expression),
        else => false,
    };
}

pub fn parseArrayExpression(parser: *Parser, in_cover: bool) Error!?ast.NodeIndex {
    const cover = try array.parseCover(parser) orelse return null;

    const needs_validation =
        // only validate if are at the top level and not in a cover context
        !in_cover and
        // and also only if we found a init name when parsing this cover
        parser.state.cover_has_init_name and
        // and also only if we are not in a pattern context, because this is going to be a pattern, so don't
        // validate it as an expression
        !isPartOfPattern(parser);

    if (!in_cover) {
        parser.state.cover_has_init_name = false;
    }

    return array.coverToExpression(parser, cover, needs_validation);
}

pub fn parseObjectExpression(parser: *Parser, in_cover: bool) Error!?ast.NodeIndex {
    const cover = try object.parseCover(parser) orelse return null;

    const needs_validation =
        // only validate if are at the top level and not in a cover context
        !in_cover and
        // and also only if we found a init name when parsing this cover
        parser.state.cover_has_init_name and
        // and also only if we are not in a pattern context, because this is going to be a pattern, so don't
        // validate it as an expression
        !isPartOfPattern(parser);

    if (!in_cover) {
        parser.state.cover_has_init_name = false;
    }

    return object.coverToExpression(parser, cover, needs_validation);
}

inline fn isPartOfPattern(parser: *Parser) bool {
    return // means this array is part of assignment expression/pattern
    parser.current_token.tag == .assign or
        // means this array is part of for-in/of
        parser.current_token.tag == .in or parser.current_token.tag == .of;
}

/// obj.prop or obj.#priv
fn parseStaticMemberExpression(parser: *Parser, object_node: ast.NodeIndex, optional: bool) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume '.'
    return parseMemberProperty(parser, object_node, optional);
}

/// property after '.' or '?.'
fn parseMemberProperty(parser: *Parser, object_node: ast.NodeIndex, optional: bool) Error!?ast.NodeIndex {
    const tag = parser.current_token.tag;

    const property = if (tag.isIdentifierLike())
        try literals.parseIdentifierName(parser)
    else if (tag == .private_identifier)
        try literals.parsePrivateIdentifier(parser)
    else {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected property name after '.'",
            .{ .help = "Use an identifier or private identifier (#name) for member access." },
        );
        return null;
    };

    const prop = property orelse return null;

    return try parser.tree.addNode(.{
        .member_expression = .{
            .object = object_node,
            .property = prop,
            .computed = false,
            .optional = optional,
        },
    }, .{ .start = parser.tree.span(object_node).start, .end = parser.tree.span(prop).end });
}

/// obj[expr]
fn parseComputedMemberExpression(parser: *Parser, object_node: ast.NodeIndex, optional: bool) Error!?ast.NodeIndex {
    const open_bracket_span = parser.current_token.span;
    try parser.advance() orelse return null; // consume '['

    const saved_allow_in = parser.context.in;
    parser.context.in = true;
    const property = try parseExpression(parser, Precedence.Lowest, .{}) orelse {
        parser.context.in = saved_allow_in;
        return null;
    };
    parser.context.in = saved_allow_in;

    const end = parser.current_token.span.end; // ']' position
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
    try parser.advance() orelse return null; // consume ']'

    return try parser.tree.addNode(.{
        .member_expression = .{
            .object = object_node,
            .property = property,
            .computed = true,
            .optional = optional,
        },
    }, .{ .start = parser.tree.span(object_node).start, .end = end });
}

/// `func(args)`. `type_arguments` is `.null` for a plain call, populated
/// by the generic-call path when a `<T>` instantiation precedes.
pub fn parseCallExpression(parser: *Parser, callee_node: ast.NodeIndex, optional: bool, type_arguments: ast.NodeIndex) Error!?ast.NodeIndex {
    const start = parser.tree.span(callee_node).start;
    const open_paren_span = parser.current_token.span;
    try parser.advance() orelse return null; // consume '('

    const args = try parseArguments(parser) orelse return null;

    const end = parser.current_token.span.end; // ')' position
    if (parser.current_token.tag != .right_paren) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected ')' after function arguments",
            .{
                .help = "Function calls must end with ')'. Check for missing commas or unclosed parentheses.",
                .labels = try parser.labels(&.{parser.label(open_paren_span, "Opened here")}),
            },
        );
        return null;
    }
    try parser.advance() orelse return null; // consume ')'

    return try parser.tree.addNode(.{
        .call_expression = .{
            .callee = callee_node,
            .arguments = args,
            .optional = optional,
            .type_arguments = type_arguments,
        },
    }, .{ .start = start, .end = end });
}

/// function call arguments
fn parseArguments(parser: *Parser) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    const saved_allow_in = parser.context.in;
    parser.context.in = true;

    while (parser.current_token.tag != .right_paren and parser.current_token.tag != .eof) {
        const arg = if (parser.current_token.tag == .spread) blk: {
            const spread_start = parser.current_token.span.start;

            try parser.advance() orelse return null; // consume '...'

            const argument = try parseExpression(parser, Precedence.Assignment, .{}) orelse {
                parser.context.in = saved_allow_in;
                return null;
            };

            const arg_span = parser.tree.span(argument);

            break :blk try parser.tree.addNode(.{
                .spread_element = .{ .argument = argument },
            }, .{ .start = spread_start, .end = arg_span.end });
        } else try parseExpression(parser, Precedence.Assignment, .{}) orelse {
            parser.context.in = saved_allow_in;

            return null;
        };

        try parser.scratch_a.append(parser.allocator(), arg);

        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return null;
        } else {
            break;
        }
    }

    parser.context.in = saved_allow_in;
    return try parser.addExtraFromScratch(&parser.scratch_a, checkpoint);
}

/// `` tag`template` ``. `type_arguments` is `.null` for a plain tag,
/// populated by the generic-tag path when a `<T>` instantiation precedes.
pub fn parseTaggedTemplateExpression(parser: *Parser, tag_node: ast.NodeIndex, type_arguments: ast.NodeIndex) Error!?ast.NodeIndex {
    const start = parser.tree.span(tag_node).start;

    const quasi = if (parser.current_token.tag == .no_substitution_template)
        try literals.parseNoSubstitutionTemplate(parser, true)
    else
        try literals.parseTemplateLiteral(parser, true);

    if (quasi == null) return null;

    const quasi_span = parser.tree.span(quasi.?);

    return try parser.tree.addNode(.{
        .tagged_template_expression = .{
            .tag = tag_node,
            .quasi = quasi.?,
            .type_arguments = type_arguments,
        },
    }, .{ .start = start, .end = quasi_span.end });
}

/// optional chain: a?.b, a?.[b], a?.()
fn parseOptionalChain(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const is_ts = parser.tree.isTs();
    const chain_start = parser.tree.span(left).start;
    try parser.advance() orelse return null; // consume '?.'

    // first optional operation
    var expr = try parseOptionalChainElement(parser, left, true) orelse return null;

    while (true) {
        switch (parser.current_token.tag) {
            .dot => expr = try parseStaticMemberExpression(parser, expr, false) orelse return null,
            .left_bracket => expr = try parseComputedMemberExpression(parser, expr, false) orelse return null,
            .left_paren => expr = try parseCallExpression(parser, expr, false, .null) orelse return null,
            .optional_chaining => {
                try parser.advance() orelse return null;
                expr = try parseOptionalChainElement(parser, expr, true) orelse return null;
            },
            // `m?.[0]!` keeps the `!` inside the chain so it lifts
            // naturally under the enclosing `ChainExpression`.
            .logical_not => {
                if (!is_ts or parser.current_token.hasLineTerminatorBefore()) break;
                const bang_end = parser.current_token.span.end;
                try parser.advance() orelse return null;
                expr = try parser.tree.addNode(
                    .{ .ts_non_null_expression = .{ .expression = expr } },
                    .{ .start = parser.tree.span(expr).start, .end = bang_end },
                );
            },
            // `a?.b<T>(c)` stays in the chain. we only commit when `(`
            // follows the type args. a bare `a?.b<T>` closes the chain
            // so the outer pratt loop builds a `TSInstantiationExpression`.
            // `<<` covers nested generics like `a?.b<<T>(x: T) => R>(c)`.
            .less_than, .left_shift => {
                if (!is_ts) break;
                const type_arguments = try ts.tryParseTypeArgumentsInExpression(parser);
                if (type_arguments == .null or parser.current_token.tag != .left_paren) break;
                expr = try parseCallExpression(parser, expr, false, type_arguments) orelse return null;
            },
            // tagged templates aren't allowed in an optional chain.
            .template_head, .no_substitution_template => {
                try parser.report(
                    parser.current_token.span,
                    "Tagged template expressions are not permitted in an optional chain",
                    .{ .help = "Remove the optional chaining operator '?.' before the template literal or add parentheses." },
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

/// parse element after ?. (property access, computed, or call), '?.' already consumed
fn parseOptionalChainElement(parser: *Parser, object_node: ast.NodeIndex, optional: bool) Error!?ast.NodeIndex {
    const tag = parser.current_token.tag;

    // identifier-like tokens become property access (a?.b)
    if (tag.isIdentifierLike() or tag == .private_identifier) {
        return parseMemberProperty(parser, object_node, optional);
    }

    // `a?.<T>(args)` generic call right after `?.`. only `<...>(...)`
    // is valid here. tagged templates and bare instantiations aren't.
    if (parser.tree.isTs() and ts.isAngleOpen(tag)) {
        const type_arguments = try ts.tryParseTypeArgumentsInExpression(parser);
        if (type_arguments != .null and parser.current_token.tag == .left_paren) {
            return parseCallExpression(parser, object_node, optional, type_arguments);
        }
    }

    return switch (tag) {
        .left_bracket => parseComputedMemberExpression(parser, object_node, optional),
        .left_paren => parseCallExpression(parser, object_node, optional, .null),
        .template_head, .no_substitution_template => {
            try parser.report(
                parser.current_token.span,
                "Tagged template expressions are not permitted in an optional chain",
                .{ .help = "Remove the optional chaining operator '?.' before the template literal." },
            );
            return null;
        },
        else => {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected property name, '[', or '(' after '?.'",
                .{ .help = "Optional chaining must be followed by property access (.x), computed access ([x]), or a call (())." },
            );
            return null;
        },
    };
}

pub const LhsContext = enum {
    /// `[k]` is a computed member access
    extends_clause,
    /// `[k]` is the next class element's key
    decorator,
};

/// https://tc39.es/ecma262/#prod-LeftHandSideExpression
pub fn parseLeftHandSideExpression(parser: *Parser, ctx: LhsContext) Error!?ast.NodeIndex {
    var expr: ast.NodeIndex = switch (parser.current_token.tag) {
        .left_paren => try parseParenthesizedExpression(parser) orelse return null,
        .new => try parseNewExpression(parser) orelse return null,
        .import => try parseImportExpression(parser, null) orelse return null,
        else => try parsePrimaryExpression(parser, .{}) orelse return null,
    };

    const is_ts = parser.tree.isTs();

    while (true) {
        expr = switch (parser.current_token.tag) {
            .dot => try parseStaticMemberExpression(parser, expr, false) orelse return null,
            .left_bracket => switch (ctx) {
                .extends_clause => try parseComputedMemberExpression(parser, expr, false) orelse return null,
                .decorator => break,
            },
            .left_paren => try parseCallExpression(parser, expr, false, .null) orelse return null,
            .template_head, .no_substitution_template => try parseTaggedTemplateExpression(parser, expr, .null) orelse return null,
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
        .update_expression, .unary_expression, .await_expression, .binary_expression, .logical_expression, .conditional_expression, .assignment_expression, .sequence_expression => Precedence.Unary,
        else => std.math.maxInt(u8),
    };
}
