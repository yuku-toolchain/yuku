const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const token = @import("../token.zig");
const std = @import("std");
const Precedence = @import("../token.zig").Precedence;

const statements = @import("statements.zig");
const array = @import("array.zig");
const object = @import("object.zig");
const literals = @import("literals.zig");
const functions = @import("functions.zig");
const class = @import("class.zig");
const parenthesized = @import("parenthesized.zig");
const patterns = @import("patterns.zig");
const modules = @import("modules.zig");

const ParseExpressionOpts = packed struct {
    /// whether to enable "expression -> pattern" validations, for example ObjectExpression -> ObjectPattern
    /// disable this when parsing expressions in cover contexts, where we don't need validations, where we do validations on top level
    enable_validation: bool = true,
    /// whether to parse the expression optionally.
    /// when true, silently returns null on immediate expression parsing failure, which means no expression found.
    /// but still reports errors on subsequent parsing failures if an expression is detected.
    optional: bool = false,
};

pub fn parseExpression(parser: *Parser, precedence: u8, opts: ParseExpressionOpts) Error!?ast.NodeIndex {
    var left = try parsePrefix(parser, opts, precedence) orelse return null;

    while (true) {
        const current_type = parser.current_token.type;
        if (current_type == .eof) break;

        if (current_type == .in and !parser.context.allow_in) break;

        const left_data = parser.getData(left);

        // yield [no LineTerminator here]
        if (parser.current_token.has_line_terminator_before) {
            if (left_data == .yield_expression) {
                break;
            }
        }

        const lbp = parser.current_token.leftBindingPower();
        if (lbp < precedence or lbp == 0) break;

        // only LeftHandSideExpressions can have postfix operations applied.
        //   a++()        <- can't call an update expression
        //   () => {}()   <- can't call an arrow function
        // breaking here produces natural "expected semicolon" error.
        if (isPostfixOperation(current_type)) {
            if (!isLeftHandSideExpression(left_data)) {
                break;
            }
        }

        left = try parseInfix(parser, lbp, left) orelse return null;
    }

    return left;
}

fn parseInfix(parser: *Parser, precedence: u8, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const current = parser.current_token;

    if (current.type.isBinaryOperator()) {
        return parseBinaryExpression(parser, precedence, left);
    }

    if (current.type.isLogicalOperator()) {
        return parseLogicalExpression(parser, precedence, left);
    }

    if (current.type.isAssignmentOperator()) {
        return parseAssignmentExpression(parser, precedence, left);
    }

    switch (current.type) {
        .increment, .decrement => return parseUpdateExpression(parser, false, left),
        .question => return parseConditionalExpression(parser, precedence, left),
        .comma => return parseSequenceExpression(parser, precedence, left),
        .dot => return parseStaticMemberExpression(parser, left, false),
        .left_bracket => return parseComputedMemberExpression(parser, left, false),
        .left_paren => return parseCallExpression(parser, left, false),
        .template_head, .no_substitution_template => return parseTaggedTemplateExpression(parser, left),
        .optional_chaining => return parseOptionalChain(parser, left),
        else => {},
    }

    try parser.reportFmt(
        current.span,
        "Unexpected token '{s}' in expression",
        .{parser.describeToken(current)},
        .{ .help = "This token cannot be used here. Expected an operator, semicolon, or end of expression." },
    );
    return null;
}

fn parsePrefix(parser: *Parser, opts: ParseExpressionOpts, precedence: u8) Error!?ast.NodeIndex {
    const token_type = parser.current_token.type;

    if (token_type == .increment or token_type == .decrement) {
        return parseUpdateExpression(parser, true, ast.null_node);
    }

    if (token_type.isUnaryOperator()) {
        return parseUnaryExpression(parser);
    }

    if (token_type == .left_paren) {
        return parseParenthesizedOrArrowFunction(parser, false, null, precedence);
    }

    if (token_type == .await and (parser.context.in_async or parser.isModule())) {
        return parseAwaitExpression(parser);
    }

    if (token_type == .yield and parser.context.in_generator and precedence <= Precedence.Assignment) {
        return parseYieldExpression(parser);
    }

    if (token_type == .new) {
        return parseNewExpression(parser);
    }

    if (token_type == .import) {
        return parseImportExpression(parser, null);
    }

    return parsePrimaryExpression(parser, opts, precedence);
}

pub inline fn parsePrimaryExpression(parser: *Parser, opts: ParseExpressionOpts, precedence: u8) Error!?ast.NodeIndex {
    return switch (parser.current_token.type) {
        .private_identifier => literals.parsePrivateIdentifier(parser),
        .string_literal => literals.parseStringLiteral(parser),
        .true, .false => literals.parseBooleanLiteral(parser),
        .null_literal => literals.parseNullLiteral(parser),
        .this => parseThisExpression(parser),
        .super => parseSuperExpression(parser),
        .numeric_literal, .hex_literal, .octal_literal, .binary_literal => literals.parseNumericLiteral(parser),
        .bigint_literal => literals.parseBigIntLiteral(parser),
        .slash, .slash_assign => literals.parseRegExpLiteral(parser),
        .template_head => literals.parseTemplateLiteral(parser),
        .no_substitution_template => literals.parseNoSubstitutionTemplate(parser),
        .left_bracket => parseArrayExpression(parser, opts.enable_validation),
        .left_brace => parseObjectExpression(parser, opts.enable_validation),
        .function => functions.parseFunction(parser, .{ .is_expression = true }, null),
        .class => class.parseClass(parser, .{ .is_expression = true }, null),
        .async => parseAsyncFunctionOrArrow(parser, precedence),
        else => {
            if (parser.current_token.type.isIdentifierLike()) {
                return parseIdentifierOrArrowFunction(parser);
            }

            if (!opts.optional) {
                const tok = parser.current_token;
                try parser.reportFmt(
                    tok.span,
                    "Unexpected token '{s}'",
                    .{parser.describeToken(tok)},
                    .{ .help = "Expected an expression" },
                );
            }

            return null;
        },
    };
}

// parse only (a), not arrow, this function is used in the 'new' expression parsing
// where we only need parenthesized
fn parseParenthesizedExpression(parser: *Parser) Error!?ast.NodeIndex {
    const cover = try parenthesized.parseCover(parser) orelse return null;

    return parenthesized.coverToExpression(parser, cover);
}

/// (a) or (a, b) => ...
fn parseParenthesizedOrArrowFunction(parser: *Parser, is_async: bool, arrow_start: ?u32, precedence: u8) Error!?ast.NodeIndex {
    const start = arrow_start orelse parser.current_token.span.start;

    const cover = try parenthesized.parseCover(parser) orelse return null;

    // [no LineTerminator here] => ConciseBody
    // arrow function's precedence is 2, assignment level
    if (parser.current_token.type == .arrow and !parser.current_token.has_line_terminator_before and precedence <= Precedence.Assignment) {
        return parenthesized.coverToArrowFunction(parser, cover, is_async, start);
    }

    // not an arrow function - convert to parenthesized expression
    return parenthesized.coverToExpression(parser, cover);
}

/// identifier, checking for arrow function: x => ...
fn parseIdentifierOrArrowFunction(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const id = try literals.parseIdentifier(parser) orelse return null;

    //  [no LineTerminator here] => ConciseBody
    if (parser.current_token.type == .arrow and !parser.current_token.has_line_terminator_before) {
        return parenthesized.identifierToArrowFunction(parser, id, false, start);
    }

    return id;
}

/// async function or async arrow function
fn parseAsyncFunctionOrArrow(parser: *Parser, precedence: u8) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const async_id = try literals.parseIdentifier(parser); // save as id and consume 'async'

    // async function ...
    if (!parser.current_token.has_line_terminator_before and parser.current_token.type == .function) {
        return functions.parseFunction(parser, .{ .is_expression = true, .is_async = true }, start);
    }

    // async (params) => ...
    if (!parser.current_token.has_line_terminator_before and parser.current_token.type == .left_paren) {
        return parseParenthesizedOrArrowFunction(parser, true, start, precedence);
    }

    // [no LineTerminator here] => ConciseBody
    // arrow function's precedence is 2
    if (parser.current_token.type.isIdentifierLike() and !parser.current_token.has_line_terminator_before and precedence <= Precedence.Assignment) {
        const id = try literals.parseIdentifier(parser) orelse return null;

        if (parser.current_token.type == .arrow and !parser.current_token.has_line_terminator_before) {
            return parenthesized.identifierToArrowFunction(parser, id, true, start);
        }

        try parser.report(
            parser.current_token.span,
            "Expected '=>' after async arrow function parameter",
            .{ .help = "Use 'async x => ...' or 'async (x) => ...' for async arrow functions." },
        );
        return null;
    }

    return async_id;
}

fn parseUnaryExpression(parser: *Parser) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    try parser.advance();

    const argument = try parseExpression(parser, 14, .{}) orelse return null;

    return try parser.addNode(
        .{
            .unary_expression = .{
                .argument = argument,
                .operator = ast.UnaryOperator.fromToken(operator_token.type),
            },
        },
        .{ .start = operator_token.span.start, .end = parser.getSpan(argument).end },
    );
}

/// `await expression`
/// https://tc39.es/ecma262/#sec-await
fn parseAwaitExpression(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume 'await'

    const argument = try parseExpression(parser, 14, .{}) orelse return null;

    return try parser.addNode(
        .{ .await_expression = .{ .argument = argument } },
        .{ .start = start, .end = parser.getSpan(argument).end },
    );
}

/// `yield`, `yield expression`, or `yield* expression`
/// https://tc39.es/ecma262/#sec-generator-function-definitions-runtime-semantics-evaluation
fn parseYieldExpression(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;
    try parser.advance();

    var delegate = false;
    if (parser.current_token.type == .star and !parser.current_token.has_line_terminator_before) {
        delegate = true;
        end = parser.current_token.span.end;
        try parser.advance();
    }

    var argument: ast.NodeIndex = ast.null_node;

    if (!parser.canInsertSemicolon() and
        parser.current_token.type != .semicolon)
    {
        if (try parseExpression(parser, Precedence.Assignment, .{ .optional = true })) |expr| {
            argument = expr;
            end = parser.getSpan(argument).end;
        }
    }

    if (delegate and ast.isNull(argument)) {
        try parser.report(parser.current_token.span, "Expected expression after 'yield*'", .{});
        return null;
    }

    if (parser.current_token.type == .dot) {
        try parser.report(parser.current_token.span, "Cannot use member access directly on yield expression", .{ .help = "Wrap the yield expression in parentheses: (yield).property or (yield expr).property" });
        return null;
    }

    return try parser.addNode(
        .{ .yield_expression = .{ .argument = argument, .delegate = delegate } },
        .{ .start = start, .end = end },
    );
}

/// `this`
/// https://tc39.es/ecma262/#sec-this-keyword
fn parseThisExpression(parser: *Parser) Error!?ast.NodeIndex {
    const this_token = parser.current_token;
    try parser.advance(); // consume 'this'
    return try parser.addNode(.this_expression, this_token.span);
}

/// `super`
/// https://tc39.es/ecma262/#sec-super-keyword
fn parseSuperExpression(parser: *Parser) Error!?ast.NodeIndex {
    const super_token = parser.current_token;
    try parser.advance(); // consume 'super'
    if (parser.current_token.type != .left_paren and parser.current_token.type != .dot and parser.current_token.type != .left_bracket) {
        try parser.report(parser.current_token.span, "'super' must be followed by a call or property access", .{ .help = "use 'super()' to call parent constructor, 'super.property' or 'super[property]' to access parent members" });
        return null;
    }
    return try parser.addNode(.super, super_token.span);
}

/// `import.meta` or `import(...)`
/// https://tc39.es/ecma262/#prod-ImportCall
/// https://tc39.es/ecma262/#prod-ImportMeta
pub fn parseImportExpression(parser: *Parser, name_from_param: ?u32) Error!?ast.NodeIndex {
    const name = name_from_param orelse try literals.parseIdentifierName(parser);

    return switch (parser.current_token.type) {
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
fn parseImportMetaOrPhaseImport(parser: *Parser, name: u32) Error!?ast.NodeIndex {
    try parser.advance(); // consume '.'

    const name_span = parser.getSpan(name);

    // import.source() or import.defer()
    if (parser.current_token.type == .source) {
        try parser.advance(); // consume 'source'
        return modules.parseDynamicImport(parser, name, .source);
    }

    if (parser.current_token.type == .@"defer") {
        try parser.advance(); // consume 'defer'
        return modules.parseDynamicImport(parser, name, .@"defer");
    }

    // import.meta
    if (parser.current_token.type != .identifier or !std.mem.eql(u8, parser.current_token.lexeme, "meta")) {
        try parser.report(
            parser.current_token.span,
            "The only valid meta properties for 'import' are 'import.meta', 'import.source()', or 'import.defer()'",
            .{ .help = "Did you mean 'import.meta', 'import.source(\"...\")' or 'import.defer(\"...\")'?" },
        );
        return null;
    }

    const property = try literals.parseIdentifierName(parser); // consume 'meta'

    return try parser.addNode(
        .{ .meta_property = .{ .meta = name, .property = property } },
        .{ .start = name_span.start, .end = parser.getSpan(property).end },
    );
}

/// `new.target`
/// https://tc39.es/ecma262/#prod-NewTarget
fn parseNewTarget(parser: *Parser, name: u32) Error!?ast.NodeIndex {
    try parser.advance(); // consume '.'

    if (!std.mem.eql(u8, parser.current_token.lexeme, "target")) {
        try parser.report(
            parser.current_token.span,
            "The only valid meta property for 'new' is 'new.target'",
            .{ .help = "Did you mean 'new.target'?" },
        );
        return null;
    }

    const property = try literals.parseIdentifierName(parser); // consume 'target'

    return try parser.addNode(
        .{ .meta_property = .{ .meta = name, .property = property } },
        .{ .start = parser.getSpan(name).start, .end = parser.getSpan(property).end },
    );
}

/// `new Callee`, `new Callee(args)`, or `new.target`
/// https://tc39.es/ecma262/#sec-new-operator
fn parseNewExpression(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const new = try literals.parseIdentifierName(parser); // consume 'new'

    // check for new.target
    if (parser.current_token.type == .dot) {
        return parseNewTarget(parser, new);
    }

    var callee: ast.NodeIndex = blk: {
        // parenthesized, allows any expression inside
        if (parser.current_token.type == .left_paren) {
            break :blk try parseParenthesizedExpression(parser) orelse return null;
        }

        // `new new Foo()`
        if (parser.current_token.type == .new) {
            break :blk try parseNewExpression(parser) orelse return null;
        }

        // otherwise, start with a primary expression
        break :blk try parsePrimaryExpression(parser, .{}, Precedence.Lowest) orelse return null;
    };

    // member expression chain (. [] and tagged templates)
    while (true) {
        callee = switch (parser.current_token.type) {
            .dot => try parseStaticMemberExpression(parser, callee, false) orelse return null,
            .left_bracket => try parseComputedMemberExpression(parser, callee, false) orelse return null,
            .template_head, .no_substitution_template => try parseTaggedTemplateExpression(parser, callee) orelse return null,
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

    // optional arguments
    var arguments = ast.IndexRange.empty;

    const end = if (parser.current_token.type == .left_paren) blk: {
        const open_paren_span = parser.current_token.span;
        try parser.advance();
        arguments = try parseArguments(parser) orelse return null;
        const arguments_end = parser.current_token.span.end;

        if (parser.current_token.type != .right_paren) {
            try parser.report(
                parser.current_token.span,
                "Expected ')' after constructor arguments",
                .{
                    .help = "Constructor calls must end with ')'.",
                    .labels = try parser.makeLabels(&.{parser.label(open_paren_span, "Opened here")}),
                },
            );
            return null;
        }
        try parser.advance(); // consume ')'

        break :blk arguments_end;
    } else parser.getSpan(callee).end;

    return try parser.addNode(
        .{ .new_expression = .{ .callee = callee, .arguments = arguments } },
        .{ .start = start, .end = end },
    );
}

fn parseUpdateExpression(parser: *Parser, prefix: bool, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.UpdateOperator.fromToken(operator_token.type);
    try parser.advance();

    if (prefix) {
        const argument = try parseExpression(parser, 14, .{}) orelse return null;
        const span = parser.getSpan(argument);

        const unwrapped = parenthesized.unwrapParenthesized(parser, argument);

        if (!isSimpleAssignmentTarget(parser, unwrapped)) {
            try parser.report(
                span,
                "Invalid operand for increment/decrement operator",
                .{ .help = "The '++' and '--' operators require a variable or property reference, not a literal or complex expression." },
            );
            return null;
        }

        return try parser.addNode(
            .{ .update_expression = .{ .argument = unwrapped, .operator = operator, .prefix = true } },
            .{ .start = operator_token.span.start, .end = span.end },
        );
    }

    const unwrapped = parenthesized.unwrapParenthesized(parser, left);

    if (!isSimpleAssignmentTarget(parser, unwrapped)) {
        const span = parser.getSpan(left);
        try parser.report(
            span,
            "Invalid operand for increment/decrement operator",
            .{ .help = "The '++' and '--' operators require a variable or property reference, not a literal or complex expression." },
        );
        return null;
    }

    return try parser.addNode(
        .{ .update_expression = .{ .argument = unwrapped, .operator = operator, .prefix = false } },
        .{ .start = parser.getSpan(left).start, .end = operator_token.span.end },
    );
}

fn parseBinaryExpression(parser: *Parser, precedence: u8, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.BinaryOperator.fromToken(operator_token.type);
    try parser.advance();

    // '**' is right-associative
    const next_precedence = if (operator == .exponent) precedence else precedence + 1;
    const right = try parseExpression(parser, next_precedence, .{}) orelse return null;

    return try parser.addNode(
        .{ .binary_expression = .{ .left = left, .right = right, .operator = operator } },
        .{ .start = parser.getSpan(left).start, .end = parser.getSpan(right).end },
    );
}

fn parseLogicalExpression(parser: *Parser, precedence: u8, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    try parser.advance();

    const right = try parseExpression(parser, precedence + 1, .{}) orelse return null;
    const current_operator = ast.LogicalOperator.fromToken(operator_token.type);

    // check for operator mixing: can't mix ?? with && or ||
    const left_data = parser.getData(left);
    const right_data = parser.getData(right);

    if (left_data == .logical_expression or right_data == .logical_expression) {
        const operator_to_check = if (left_data == .logical_expression) left_data.logical_expression.operator else right_data.logical_expression.operator;

        if ((current_operator == .nullish_coalescing) != (operator_to_check == .nullish_coalescing)) {
            const left_span = parser.getSpan(left);
            try parser.report(
                .{ .start = left_span.start, .end = parser.getSpan(right).end },
                "Logical expressions and nullish coalescing cannot be mixed",
                .{ .help = "Wrap either expression in parentheses" },
            );
            return null;
        }
    }

    return try parser.addNode(
        .{
            .logical_expression = .{
                .left = left,
                .right = right,
                .operator = current_operator,
            },
        },
        .{ .start = parser.getSpan(left).start, .end = parser.getSpan(right).end },
    );
}

/// `a, b, c` - comma operator / sequence expression
/// https://tc39.es/ecma262/#sec-comma-operator
fn parseSequenceExpression(parser: *Parser, precedence: u8, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const checkpoint = parser.scratch_a.begin();
    try parser.scratch_a.append(parser.allocator(), left);

    while (parser.current_token.type == .comma) {
        try parser.advance(); // consume ','

        const expr = try parseExpression(parser, precedence + 1, .{}) orelse {
            parser.scratch_a.reset(checkpoint);
            return null;
        };
        try parser.scratch_a.append(parser.allocator(), expr);
    }

    const expressions = parser.scratch_a.take(checkpoint);

    const first_span = parser.getSpan(expressions[0]);
    const last_span = parser.getSpan(expressions[expressions.len - 1]);

    return try parser.addNode(
        .{ .sequence_expression = .{ .expressions = try parser.addExtra(expressions) } },
        .{ .start = first_span.start, .end = last_span.end },
    );
}

fn parseAssignmentExpression(parser: *Parser, precedence: u8, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const left_unwraped = parenthesized.unwrapParenthesized(parser, left);

    const operator_token = parser.current_token;
    const operator = ast.AssignmentOperator.fromToken(operator_token.type);
    const left_span = parser.getSpan(left);

    // validate that left side can be assigned to
    if (!isValidAssignmentTarget(parser, left_unwraped)) {
        try parser.report(
            left_span,
            "Invalid left-hand side in assignment",
            .{ .help = "The left side of an assignment must be a variable, property access, or destructuring pattern." },
        );
        return null;
    }

    // logical assignments (&&=, ||=, ??=) require simple targets
    const is_logical = operator == .logical_and_assign or operator == .logical_or_assign or operator == .nullish_assign;
    if (is_logical and !isSimpleAssignmentTarget(parser, left_unwraped)) {
        try parser.report(
            left_span,
            "Invalid left-hand side in logical assignment",
            .{ .help = "Logical assignment operators (&&=, ||=, ??=) require a simple reference like a variable or property, not a destructuring pattern." },
        );
        return null;
    }

    try parser.advance();

    const right = try parseExpression(parser, precedence, .{}) orelse return null;

    return try parser.addNode(
        .{ .assignment_expression = .{ .left = left_unwraped, .right = right, .operator = operator } },
        .{ .start = left_span.start, .end = parser.getSpan(right).end },
    );
}

/// `test ? consequent : alternate`
/// https://tc39.es/ecma262/#sec-conditional-operator
fn parseConditionalExpression(parser: *Parser, precedence: u8, @"test": ast.NodeIndex) Error!?ast.NodeIndex {
    const test_span = parser.getSpan(@"test");

    try parser.advance(); // consume '?'

    // consequent
    // right-associative, so same prec, not precedence + 1
    const consequent = try parseExpression(parser, precedence, .{}) orelse return null;

    if (!try parser.expect(.colon, "Expected ':' after conditional expression consequent", "The ternary operator requires a colon (:) to separate the consequent and alternate expressions.")) {
        return null;
    }

    // alternate
    // right-associative, so same prec, not precedence + 1
    const alternate = try parseExpression(parser, precedence, .{}) orelse return null;

    return try parser.addNode(
        .{
            .conditional_expression = .{
                .@"test" = @"test",
                .consequent = consequent,
                .alternate = alternate,
            },
        },
        .{ .start = test_span.start, .end = parser.getSpan(alternate).end },
    );
}

/// AssignmentTarget: can be simple (identifier/member) or pattern (destructuring)
pub fn isValidAssignmentTarget(parser: *Parser, index: ast.NodeIndex) bool {
    const data = parser.getData(index);
    return switch (data) {
        // SimpleAssignmentTarget
        .identifier_reference => true,
        .member_expression => |m| !m.optional, // optional chaining is not a valid assignment target

        // AssignmentPattern (destructuring)
        .array_pattern, .object_pattern => true,

        else => false,
    };
}

/// SimpleAssignmentTarget: only identifier and member expressions (no destructuring)
pub fn isSimpleAssignmentTarget(parser: *Parser, index: ast.NodeIndex) bool {
    return switch (parser.getData(index)) {
        .identifier_reference => true,
        .member_expression => |m| !m.optional, // optional chaining is not a valid assignment target
        else => false,
    };
}

pub fn parseArrayExpression(parser: *Parser, enable_validation: bool) Error!?ast.NodeIndex {
    const cover = try array.parseCover(parser) orelse return null;
    const needs_validation = enable_validation and parser.state.cover_has_init_name;

    if (enable_validation) {
        parser.state.cover_has_init_name = false;
    }

    if (
    // means this array is part of assignment expression/pattern
    parser.current_token.type == .assign or
        // means this array is part of for-in/of
        parser.current_token.type == .in or parser.current_token.type == .of
        // so convert to pattern
    ) {
        // since it's part og assignment expression, we covert to pattern as assignable context
        return try array.coverToPattern(parser, cover, .assignable);
    }

    return array.coverToExpression(parser, cover, needs_validation);
}

pub fn parseObjectExpression(parser: *Parser, enable_validation: bool) Error!?ast.NodeIndex {
    const cover = try object.parseCover(parser) orelse return null;
    const needs_validation = enable_validation and parser.state.cover_has_init_name;

    if (enable_validation) {
        parser.state.cover_has_init_name = false;
    }

    if (
    // means this object is part of assignment expression/pattern
    parser.current_token.type == .assign or
        // means this object is part of for-in/of
        parser.current_token.type == .in or parser.current_token.type == .of
        // so convert to pattern
    ) {
        // since it's part og assignment expression, we covert to pattern as assignable context
        return try object.coverToPattern(parser, cover, .assignable);
    }

    return object.coverToExpression(parser, cover, needs_validation);
}

/// obj.prop or obj.#priv
fn parseStaticMemberExpression(parser: *Parser, object_node: ast.NodeIndex, optional: bool) Error!?ast.NodeIndex {
    try parser.advance(); // consume '.'
    return parseMemberProperty(parser, object_node, optional);
}

/// property after '.' or '?.'
fn parseMemberProperty(parser: *Parser, object_node: ast.NodeIndex, optional: bool) Error!?ast.NodeIndex {
    const tok_type = parser.current_token.type;

    const property = if (tok_type.isIdentifierLike())
        try literals.parseIdentifierName(parser)
    else if (tok_type == .private_identifier)
        try literals.parsePrivateIdentifier(parser)
    else {
        try parser.report(
            parser.current_token.span,
            "Expected property name after '.'",
            .{ .help = "Use an identifier or private identifier (#name) for member access." },
        );
        return null;
    };

    const prop = property orelse return null;

    return try parser.addNode(.{
        .member_expression = .{
            .object = object_node,
            .property = prop,
            .computed = false,
            .optional = optional,
        },
    }, .{ .start = parser.getSpan(object_node).start, .end = parser.getSpan(prop).end });
}

/// obj[expr]
fn parseComputedMemberExpression(parser: *Parser, object_node: ast.NodeIndex, optional: bool) Error!?ast.NodeIndex {
    const open_bracket_span = parser.current_token.span;
    try parser.advance(); // consume '['

    const saved_allow_in = parser.context.allow_in;
    parser.context.allow_in = true;
    const property = try parseExpression(parser, Precedence.Lowest, .{}) orelse {
        parser.context.allow_in = saved_allow_in;
        return null;
    };
    parser.context.allow_in = saved_allow_in;

    const end = parser.current_token.span.end; // ']' position
    if (parser.current_token.type != .right_bracket) {
        try parser.report(
            parser.current_token.span,
            "Expected ']' after computed property",
            .{
                .help = "Computed member access must end with ']'.",
                .labels = try parser.makeLabels(&.{parser.label(open_bracket_span, "Opened here")}),
            },
        );
        return null;
    }
    try parser.advance(); // consume ']'

    return try parser.addNode(.{
        .member_expression = .{
            .object = object_node,
            .property = property,
            .computed = true,
            .optional = optional,
        },
    }, .{ .start = parser.getSpan(object_node).start, .end = end });
}

/// func(args)
fn parseCallExpression(parser: *Parser, callee_node: ast.NodeIndex, optional: bool) Error!?ast.NodeIndex {
    const start = parser.getSpan(callee_node).start;
    const open_paren_span = parser.current_token.span;
    try parser.advance(); // consume '('

    const args = try parseArguments(parser) orelse return null;

    const end = parser.current_token.span.end; // ')' position
    if (parser.current_token.type != .right_paren) {
        try parser.report(
            parser.current_token.span,
            "Expected ')' after function arguments",
            .{
                .help = "Function calls must end with ')'. Check for missing commas or unclosed parentheses.",
                .labels = try parser.makeLabels(&.{parser.label(open_paren_span, "Opened here")}),
            },
        );
        return null;
    }
    try parser.advance(); // consume ')'

    return try parser.addNode(.{
        .call_expression = .{
            .callee = callee_node,
            .arguments = args,
            .optional = optional,
        },
    }, .{ .start = start, .end = end });
}

/// function call arguments
fn parseArguments(parser: *Parser) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_a.begin();

    const saved_allow_in = parser.context.allow_in;
    parser.context.allow_in = true;

    while (parser.current_token.type != .right_paren and parser.current_token.type != .eof) {
        const arg = if (parser.current_token.type == .spread) blk: {
            const spread_start = parser.current_token.span.start;
            try parser.advance(); // consume '...'
            const argument = try parseExpression(parser, Precedence.Assignment, .{}) orelse {
                parser.context.allow_in = saved_allow_in;
                return null;
            };
            const arg_span = parser.getSpan(argument);
            break :blk try parser.addNode(.{
                .spread_element = .{ .argument = argument },
            }, .{ .start = spread_start, .end = arg_span.end });
        } else try parseExpression(parser, Precedence.Assignment, .{}) orelse {
            parser.context.allow_in = saved_allow_in;
            return null;
        };

        try parser.scratch_a.append(parser.allocator(), arg);

        if (parser.current_token.type == .comma) {
            try parser.advance();
        } else {
            break;
        }
    }

    parser.context.allow_in = saved_allow_in;
    return try parser.addExtra(parser.scratch_a.take(checkpoint));
}

/// tag`template`
fn parseTaggedTemplateExpression(parser: *Parser, tag_node: ast.NodeIndex) Error!?ast.NodeIndex {
    const start = parser.getSpan(tag_node).start;

    const quasi = if (parser.current_token.type == .no_substitution_template)
        try literals.parseNoSubstitutionTemplate(parser)
    else
        try literals.parseTemplateLiteral(parser);

    if (quasi == null) return null;

    const quasi_span = parser.getSpan(quasi.?);

    return try parser.addNode(.{
        .tagged_template_expression = .{
            .tag = tag_node,
            .quasi = quasi.?,
        },
    }, .{ .start = start, .end = quasi_span.end });
}

/// optional chain: a?.b, a?.[b], a?.()
fn parseOptionalChain(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const chain_start = parser.getSpan(left).start;
    try parser.advance(); // consume '?.'

    // first optional operation
    var expr = try parseOptionalChainElement(parser, left, true) orelse return null;

    // continue parsing the chain
    while (true) {
        switch (parser.current_token.type) {
            .dot => expr = try parseStaticMemberExpression(parser, expr, false) orelse return null,
            .left_bracket => expr = try parseComputedMemberExpression(parser, expr, false) orelse return null,
            .left_paren => expr = try parseCallExpression(parser, expr, false) orelse return null,
            .optional_chaining => {
                try parser.advance();
                expr = try parseOptionalChainElement(parser, expr, true) orelse return null;
            },
            .template_head, .no_substitution_template => {
                // tagged template in optional chain, not allowed (unless line terminator separates)
                if (!parser.current_token.has_line_terminator_before) {
                    try parser.report(
                        parser.current_token.span,
                        "Tagged template expressions are not permitted in an optional chain",
                        .{ .help = "Remove the optional chaining operator '?.' before the template literal or add parentheses." },
                    );
                    return null;
                }
                break;
            },
            else => break,
        }
    }

    return try parser.addNode(.{
        .chain_expression = .{ .expression = expr },
    }, .{ .start = chain_start, .end = parser.getSpan(expr).end });
}

/// parse element after ?. (property access, computed, or call), '?.' already consumed
fn parseOptionalChainElement(parser: *Parser, object_node: ast.NodeIndex, optional: bool) Error!?ast.NodeIndex {
    const tok_type = parser.current_token.type;

    // identifier-like tokens become property access (a?.b)
    if (tok_type.isIdentifierLike() or tok_type == .private_identifier) {
        return parseMemberProperty(parser, object_node, optional);
    }

    return switch (tok_type) {
        .left_bracket => parseComputedMemberExpression(parser, object_node, optional),
        .left_paren => parseCallExpression(parser, object_node, optional),
        .template_head, .no_substitution_template => {
            try parser.report(
                parser.current_token.span,
                "Tagged template expressions are not permitted in an optional chain",
                .{ .help = "Remove the optional chaining operator '?.' before the template literal." },
            );
            return null;
        },
        else => {
            try parser.report(
                parser.current_token.span,
                "Expected property name, '[', or '(' after '?.'",
                .{ .help = "Optional chaining must be followed by property access (.x), computed access ([x]), or a call (())." },
            );
            return null;
        },
    };
}

/// https://tc39.es/ecma262/#sec-left-hand-side-expressions
/// used to parse `extends` clause, where we only need left hand side expression
pub inline fn parseLeftHandSideExpression(parser: *Parser) Error!?ast.NodeIndex {
    // base expression
    var expr: ast.NodeIndex = blk: {
        if (parser.current_token.type == .left_paren) {
            break :blk try parseParenthesizedExpression(parser) orelse return null;
        }

        if (parser.current_token.type == .new) {
            break :blk try parseNewExpression(parser) orelse return null;
        }

        if (parser.current_token.type == .import) {
            break :blk try parseImportExpression(parser, null) orelse return null;
        }

        break :blk try parsePrimaryExpression(parser, .{}, Precedence.Lowest) orelse return null;
    };

    // chain LeftHandSide operations: member access, calls, optional chaining
    while (true) {
        expr = switch (parser.current_token.type) {
            .dot => try parseStaticMemberExpression(parser, expr, false) orelse return null,
            .left_bracket => try parseComputedMemberExpression(parser, expr, false) orelse return null,
            .left_paren => try parseCallExpression(parser, expr, false) orelse return null,
            .template_head, .no_substitution_template => try parseTaggedTemplateExpression(parser, expr) orelse return null,
            .optional_chaining => try parseOptionalChain(parser, expr) orelse return null,
            else => break,
        };
    }

    return expr;
}

/// checks if a token represents a postfix operation that requires the left operand
/// to be a LeftHandSideExpression.
///
/// postfix operations are operations that:
/// 1. bind tightly to their left operand (high precedence)
/// 2. can only be applied to LeftHandSideExpressions (not to binary expressions,
///    update expressions, etc. without parentheses)
///
/// example of invalid usage without parentheses:
/// - `a++.prop` - can't access property of update expression
///
/// this operation is valid when the left side is wrapped in parentheses,
/// which creates a `parenthesized_expression` (which is a LeftHandSideExpression):
/// - `(a++).prop` - valid
///
/// just explaining things xD
fn isPostfixOperation(token_type: token.TokenType) bool {
    return switch (token_type) {
        .dot, // obj.prop
        .left_bracket, // obj[prop]
        .left_paren, // func()
        .optional_chaining, // obj?.prop
        .template_head, // tag`template`
        .no_substitution_template, // tag`template`
        => true,
        else => false,
    };
}

/// https://tc39.es/ecma262/#sec-left-hand-side-expressions
fn isLeftHandSideExpression(data: ast.NodeData) bool {
    return switch (data) {
        .arrow_function_expression, .update_expression, .unary_expression, .await_expression, .yield_expression, .binary_expression, .logical_expression, .conditional_expression, .assignment_expression, .sequence_expression => false,
        else => true,
    };
}
