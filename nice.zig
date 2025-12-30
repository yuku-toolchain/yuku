const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const Precedence = @import("../token.zig").Precedence;

const grammar = @import("../grammar.zig");

/// result from parsing array cover grammar: [a, b, ...c]
pub const ArrayCover = struct {
    elements: []const ast.NodeIndex,
    start: u32,
    end: u32,
};

/// parse array literal permissively using cover grammar: [a, b, ...c]
/// https://tc39.es/ecma262/#sec-array-initializer (covers ArrayAssignmentPattern)
pub fn parseCover(parser: *Parser) Error!?ArrayCover {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume [

    const checkpoint = parser.scratch_cover.begin();
    errdefer parser.scratch_cover.reset(checkpoint);

    var end = start + 1;

    while (parser.current_token.type != .right_bracket and parser.current_token.type != .eof) {
        // elision (holes): [,,,]
        if (parser.current_token.type == .comma) {
            try parser.scratch_cover.append(parser.allocator(), ast.null_node);
            try parser.advance();
            continue;
        }

        // spread: [...x]
        if (parser.current_token.type == .spread) {
            const spread_start = parser.current_token.span.start;
            try parser.advance();
            const argument = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse {
                parser.scratch_cover.reset(checkpoint);
                return null;
            };
            const spread_end = parser.getSpan(argument).end;
            const spread = try parser.addNode(
                .{ .spread_element = .{ .argument = argument } },
                .{ .start = spread_start, .end = spread_end },
            );
            try parser.scratch_cover.append(parser.allocator(), spread);
            end = spread_end;
        } else {
            // regular element - parse as cover element
            const element = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse {
                parser.scratch_cover.reset(checkpoint);
                return null;
            };
            try parser.scratch_cover.append(parser.allocator(), element);
            end = parser.getSpan(element).end;
        }

        // comma or end
        if (parser.current_token.type == .comma) {
            try parser.advance();
            // then it's a trailing comma
            if (parser.current_token.type == .right_bracket) {
                parser.state.cover_has_trailing_comma = start;
            }
        } else if (parser.current_token.type != .right_bracket) {
            try parser.report(
                parser.current_token.span,
                "Expected ',' or ']' in array",
                .{ .help = "Add a comma between elements or close the array with ']'." },
            );
            parser.scratch_cover.reset(checkpoint);
            return null;
        }
    }

    if (parser.current_token.type != .right_bracket) {
        try parser.report(
            .{ .start = start, .end = end },
            "Unterminated array",
            .{
                .help = "Add a closing ']' to complete the array.",
                .labels = try parser.makeLabels(&.{parser.label(.{ .start = start, .end = start + 1 }, "Opened here")}),
            },
        );
        parser.scratch_cover.reset(checkpoint);
        return null;
    }

    end = parser.current_token.span.end;
    try parser.advance(); // consume ]

    return .{
        .elements = parser.scratch_cover.take(checkpoint),
        .start = start,
        .end = end,
    };
}

/// convert array cover to ArrayExpression.
/// validates that the expression does not contain CoverInitializedName when validate=true.
pub fn coverToExpression(parser: *Parser, cover: ArrayCover, validate: bool) Error!?ast.NodeIndex {
    const array_expression = try parser.addNode(
        .{ .array_expression = .{ .elements = try parser.addExtra(cover.elements) } },
        .{ .start = cover.start, .end = cover.end },
    );

    if (validate and !try grammar.validateNoCoverInitializedSyntax(parser, array_expression)) {
        return null;
    }

    return array_expression;
}

/// convert array cover to ArrayPattern.
pub fn coverToPattern(parser: *Parser, cover: ArrayCover, context: grammar.PatternContext) Error!?ast.NodeIndex {
    const elements_range = try parser.addExtra(cover.elements);
    return toArrayPatternImpl(parser, null, elements_range, .{ .start = cover.start, .end = cover.end }, context);
}

/// convert ArrayExpression to ArrayPattern (mutates in-place).
pub fn toArrayPattern(parser: *Parser, expr_node: ast.NodeIndex, elements_range: ast.IndexRange, span: ast.Span, context: grammar.PatternContext) Error!?void {
    _ = try toArrayPatternImpl(parser, expr_node, elements_range, span, context) orelse return null;
}

fn toArrayPatternImpl(parser: *Parser, mutate_node: ?ast.NodeIndex, elements_range: ast.IndexRange, span: ast.Span, context: grammar.PatternContext) Error!?ast.NodeIndex {
    const elements = parser.getExtra(elements_range);

    var rest: ast.NodeIndex = ast.null_node;
    var elements_len = elements_range.len;

    for (elements, 0..) |elem, i| {
        if (ast.isNull(elem)) continue;

        const elem_data = parser.getData(elem);
        if (elem_data == .spread_element) {
            if (parser.state.cover_has_trailing_comma == span.start) {
                try parser.report(span, "Rest element cannot have a trailing comma in array destructuring.", .{
                    .help = "Remove the trailing comma after the rest element",
                });

                parser.state.cover_has_trailing_comma = null;

                return null;
            }

            if (i != elements_len - 1) {
                try parser.report(parser.getSpan(elem), "Rest element must be the last element", .{
                    .help = "No elements can follow the rest element in a destructuring pattern.",
                });
                return null;
            }

            try grammar.expressionToPattern(parser, elem_data.spread_element.argument, context) orelse return null;

            parser.setData(elem, .{ .binding_rest_element = .{ .argument = elem_data.spread_element.argument } });
            rest = elem;
            elements_len = @intCast(i);
            break;
        }

        try grammar.expressionToPattern(parser, elem, context) orelse return null;
    }

    const pattern_data: ast.NodeData = .{ .array_pattern = .{
        .elements = .{ .start = elements_range.start, .len = elements_len },
        .rest = rest,
    } };

    if (mutate_node) |node| {
        parser.setData(node, pattern_data);
        return node;
    }

    return try parser.addNode(pattern_data, span);
}

const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const token = @import("../token.zig");
const Precedence = @import("../token.zig").Precedence;

const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const functions = @import("functions.zig");
const expressions = @import("expressions.zig");
const statements = @import("statements.zig");

pub const ParseClassOpts = packed struct {
    is_expression: bool = false,
    /// for export default class, allows optional name but produces ClassDeclaration
    is_default_export: bool = false,
};

/// class declaration or expression
/// https://tc39.es/ecma262/#sec-class-definitions
pub fn parseClass(parser: *Parser, opts: ParseClassOpts, start_from_param: ?u32) Error!?ast.NodeIndex {
    const start = start_from_param orelse parser.current_token.span.start;

    if (!try parser.expect(.class, "Expected 'class' keyword", null)) return null;

    // export default class produces a declaration with optional name
    // regular class expression allows optional name but produces expression
    const class_type: ast.ClassType = if (opts.is_expression and !opts.is_default_export) .class_expression else .class_declaration;

    // optional class name
    var id: ast.NodeIndex = ast.null_node;

    if (parser.current_token.type.isIdentifierLike() and parser.current_token.type != .extends) {
        id = try patterns.parseBindingIdentifier(parser) orelse ast.null_node;
    }

    // name is required for regular class declarations, but optional for:
    // - class expressions
    // - export default class
    if (!opts.is_expression and !opts.is_default_export and ast.isNull(id)) {
        try parser.report(
            parser.current_token.span,
            "Class declaration requires a name",
            .{ .help = "Add a name after 'class', e.g. 'class MyClass {}'." },
        );
        return null;
    }

    // optional extends clause
    var super_class: ast.NodeIndex = ast.null_node;
    if (parser.current_token.type == .extends) {
        try parser.advance(); // consume 'extends'
        super_class = try expressions.parseLeftHandSideExpression(parser) orelse return null;
    }

    // class body
    const body = try parseClassBody(parser) orelse return null;
    const body_end = parser.getSpan(body).end;

    return try parser.addNode(.{
        .class = .{
            .type = class_type,
            .id = id,
            .super_class = super_class,
            .body = body,
        },
    }, .{ .start = start, .end = body_end });
}

/// class body: { ClassElementList }
fn parseClassBody(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (!try parser.expect(
        .left_brace,
        "Expected '{' to start class body",
        "Class body must be enclosed in braces: class Name { ... }",
    )) return null;

    const checkpoint = parser.scratch_a.begin();

    while (parser.current_token.type != .right_brace and parser.current_token.type != .eof) {
        // empty statement (semicolon)
        if (parser.current_token.type == .semicolon) {
            try parser.advance();
            continue;
        }

        const element = try parseClassElement(parser) orelse {
            parser.scratch_a.reset(checkpoint);
            return null;
        };

        try parser.scratch_a.append(parser.allocator(), element);
    }

    const end = parser.current_token.span.end;

    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close class body",
        "Add a closing brace '}' to complete the class, or check for unbalanced braces inside.",
    )) {
        parser.scratch_a.reset(checkpoint);
        return null;
    }

    return try parser.addNode(.{
        .class_body = .{ .body = try parser.addExtra(parser.scratch_a.take(checkpoint)) },
    }, .{ .start = start, .end = end });
}

/// a single class element (method, field, or static block)
fn parseClassElement(parser: *Parser) Error!?ast.NodeIndex {
    const elem_start = parser.current_token.span.start;

    var is_static = false;
    var is_async = false;
    var is_generator = false;
    var kind: ast.MethodDefinitionKind = .method;
    var computed = false;
    var key: ast.NodeIndex = ast.null_node;

    // check for 'static' modifier
    if (parser.current_token.type == .static) {
        const static_token = parser.current_token;
        try parser.advance();

        // static { } - static block
        if (parser.current_token.type == .left_brace) {
            return parseStaticBlock(parser, elem_start);
        }

        // If next token is '(' or nothing that could be a class element key, 'static' is the key
        // e.g., `static() {}` is a method named "static", not a static method
        if (parser.current_token.type == .left_paren or !isClassElementStart(parser.current_token.type)) {
            key = try parser.addNode(
                .{ .identifier_name = .{ .name_start = static_token.span.start, .name_len = @intCast(static_token.lexeme.len) } },
                static_token.span,
            );
        } else {
            is_static = true;
        }
    }

    // check for 'async' modifier (only if no key yet)
    if (ast.isNull(key) and parser.current_token.type == .async) {
        const async_token = parser.current_token;
        try parser.advance();

        // check if this is async method or 'async' as property name
        if (isClassElementKeyStart(parser.current_token.type) and !parser.current_token.has_line_terminator_before) {
            is_async = true;
        } else {
            key = try parser.addNode(
                .{ .identifier_name = .{ .name_start = async_token.span.start, .name_len = @intCast(async_token.lexeme.len) } },
                async_token.span,
            );
        }
    }

    // check for generator (*)
    if (ast.isNull(key) and parser.current_token.type == .star) {
        is_generator = true;
        try parser.advance();
    }

    // check for get/set (only if no key yet and not async/generator)
    if (ast.isNull(key) and !is_async and !is_generator and parser.current_token.type == .identifier) {
        const lexeme = parser.current_token.lexeme;

        if (std.mem.eql(u8, lexeme, "get") or std.mem.eql(u8, lexeme, "set")) {
            const get_set_token = parser.current_token;
            try parser.advance();

            // check if this is get/set accessor or just a property named 'get'/'set'
            if (isClassElementKeyStart(parser.current_token.type)) {
                kind = if (std.mem.eql(u8, lexeme, "get")) .get else .set;
            } else {
                key = try parser.addNode(
                    .{ .identifier_name = .{ .name_start = get_set_token.span.start, .name_len = @intCast(get_set_token.lexeme.len) } },
                    get_set_token.span,
                );
            }
        }
    }

    // parse the key if not already determined
    if (ast.isNull(key)) {
        const key_result = try parseClassElementKey(parser);
        key = key_result.key orelse return null;
        computed = key_result.computed;
    }

    // determine if this is constructor
    // non-static, non-computed methods with PropName "constructor" are constructors
    // PropName can come from identifier or string literal (but not computed)
    if (!is_static and !computed and kind == .method) {
        const key_data = parser.getData(key);
        if (key_data == .identifier_name) {
            const name = parser.getSourceText(key_data.identifier_name.name_start, key_data.identifier_name.name_len);
            if (std.mem.eql(u8, name, "constructor")) {
                kind = .constructor;
            }
        } else if (key_data == .string_literal) {
            const raw = parser.getSourceText(key_data.string_literal.raw_start, key_data.string_literal.raw_len);

            if (raw.len >= 2 and std.mem.eql(u8, raw[1 .. raw.len - 1], "constructor")) {
                kind = .constructor;
            }
        }
    }

    // method: key followed by (
    if (parser.current_token.type == .left_paren) {
        return parseMethodDefinition(parser, elem_start, key, computed, kind, is_static, is_async, is_generator);
    }

    if (is_async or is_generator) {
        try parser.report(
            parser.current_token.span,
            "Expected '(' for method definition",
            .{ .help = "Method definitions require a parameter list. Use 'method() {}' syntax." },
        );
        return null;
    }

    if (kind != .method) {
        try parser.report(
            parser.current_token.span,
            "Expected '(' for getter/setter definition",
            .{ .help = "Getters and setters require parentheses. Use 'get prop() {}' or 'set prop(value) {}' syntax." },
        );
        return null;
    }

    // field definition
    return parsePropertyDefinition(parser, elem_start, key, computed, is_static);
}

const KeyResult = struct {
    key: ?ast.NodeIndex,
    computed: bool,
};

/// class element key
fn parseClassElementKey(parser: *Parser) Error!KeyResult {
    // computed key
    if (parser.current_token.type == .left_bracket) {
        try parser.advance(); // consume '['
        const key = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return .{ .key = null, .computed = true };
        if (!try parser.expect(.right_bracket, "Expected ']' after computed property key", null)) {
            return .{ .key = null, .computed = true };
        }
        return .{ .key = key, .computed = true };
    }

    // #name
    if (parser.current_token.type == .private_identifier) {
        const key = try literals.parsePrivateIdentifier(parser);
        return .{ .key = key, .computed = false };
    }

    if (parser.current_token.type == .string_literal) {
        const key = try literals.parseStringLiteral(parser);
        return .{ .key = key, .computed = false };
    }

    if (parser.current_token.type.isNumericLiteral()) {
        const key = try literals.parseNumericLiteral(parser);
        return .{ .key = key, .computed = false };
    }

    // identifier-like (includes keywords)
    if (parser.current_token.type.isIdentifierLike()) {
        const tok = parser.current_token;
        try parser.advance();
        const key = try parser.addNode(
            .{ .identifier_name = .{ .name_start = tok.span.start, .name_len = @intCast(tok.lexeme.len) } },
            tok.span,
        );
        return .{ .key = key, .computed = false };
    }

    try parser.reportFmt(
        parser.current_token.span,
        "Unexpected token '{s}' as class element key",
        .{parser.describeToken(parser.current_token)},
        .{ .help = "Class element keys must be identifiers, strings, numbers, private identifiers (#name), or computed expressions [expr]." },
    );

    return .{ .key = null, .computed = false };
}

/// method definition
fn parseMethodDefinition(
    parser: *Parser,
    elem_start: u32,
    key: ast.NodeIndex,
    computed: bool,
    kind: ast.MethodDefinitionKind,
    is_static: bool,
    is_async: bool,
    is_generator: bool,
) Error!?ast.NodeIndex {
    if (kind == .constructor) {
        if (is_async) {
            try parser.report(
                parser.getSpan(key),
                "Constructor cannot be async",
                .{ .help = "Remove the 'async' modifier from the constructor." },
            );
            return null;
        }
        if (is_generator) {
            try parser.report(
                parser.getSpan(key),
                "Constructor cannot be a generator",
                .{ .help = "Remove the '*' from the constructor." },
            );
            return null;
        }
    }

    if (kind == .get and is_generator) {
        try parser.report(
            parser.getSpan(key),
            "Getter cannot be a generator",
            .{ .help = "Remove the '*' from the getter definition." },
        );
        return null;
    }

    if (kind == .set and is_generator) {
        try parser.report(
            parser.getSpan(key),
            "Setter cannot be a generator",
            .{ .help = "Remove the '*' from the setter definition." },
        );
        return null;
    }

    // Save and set context
    const saved_async = parser.context.in_async;
    const saved_generator = parser.context.in_generator;
    parser.context.in_async = is_async;
    parser.context.in_generator = is_generator;

    defer {
        parser.context.in_async = saved_async;
        parser.context.in_generator = saved_generator;
    }

    const func_start = parser.current_token.span.start;
    if (!try parser.expect(.left_paren, "Expected '(' to start method parameters", null)) {
        return null;
    }

    const params = try functions.parseFormalParamaters(parser, .unique_formal_parameters) orelse return null;
    const params_data = parser.getData(params).formal_parameters;

    if (kind == .get) {
        if (params_data.items.len != 0 or !ast.isNull(params_data.rest)) {
            try parser.report(
                parser.getSpan(params),
                "Getter must have no parameters",
                .{ .help = "Remove all parameters from the getter." },
            );
            return null;
        }
    }

    if (kind == .set) {
        if (params_data.items.len != 1 or !ast.isNull(params_data.rest)) {
            try parser.report(
                parser.getSpan(params),
                "Setter must have exactly one parameter",
                .{ .help = "Setters accept exactly one argument." },
            );
            return null;
        }
    }

    if (!try parser.expect(.right_paren, "Expected ')' after method parameters", null)) {
        return null;
    }

    // body
    const body = try functions.parseFunctionBody(parser) orelse return null;
    const body_end = parser.getSpan(body).end;

    const func = try parser.addNode(
        .{ .function = .{
            .type = .function_expression,
            .id = ast.null_node,
            .generator = is_generator,
            .async = is_async,
            .params = params,
            .body = body,
        } },
        .{ .start = func_start, .end = body_end },
    );

    return try parser.addNode(
        .{ .method_definition = .{
            .key = key,
            .value = func,
            .kind = kind,
            .computed = computed,
            .static = is_static,
        } },
        .{ .start = elem_start, .end = body_end },
    );
}

/// property/field definition
fn parsePropertyDefinition(
    parser: *Parser,
    elem_start: u32,
    key: ast.NodeIndex,
    computed: bool,
    is_static: bool,
) Error!?ast.NodeIndex {
    var value: ast.NodeIndex = ast.null_node;
    var end = parser.getSpan(key).end;

    if (parser.current_token.type == .assign) {
        try parser.advance(); // consume '='
        value = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        end = parser.getSpan(value).end;
    }

    if (parser.current_token.type == .semicolon) {
        end = parser.current_token.span.end;
        try parser.advance();
    } else if (!parser.canInsertSemicolon() and parser.current_token.type != .right_brace) {
        try parser.report(
            parser.current_token.span,
            "Expected ';' after class field",
            .{ .help = "Add a semicolon after the field declaration." },
        );
        return null;
    }

    return try parser.addNode(
        .{ .property_definition = .{
            .key = key,
            .value = value,
            .computed = computed,
            .static = is_static,
        } },
        .{ .start = elem_start, .end = end },
    );
}

/// static block: static { ... }
fn parseStaticBlock(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    if (!try parser.expect(.left_brace, "Expected '{' to start static block", null)) {
        return null;
    }

    const body = try parser.parseBody(.right_brace);

    const end = parser.current_token.span.end;

    if (!try parser.expect(.right_brace, "Expected '}' to close static block", null)) {
        return null;
    }

    return try parser.addNode(
        .{ .static_block = .{ .body = body } },
        .{ .start = start, .end = end },
    );
}

/// if token could start a class element
inline fn isClassElementStart(tok_type: token.TokenType) bool {
    return tok_type == .star or
        tok_type == .left_bracket or
        tok_type == .private_identifier or
        tok_type == .string_literal or
        tok_type.isNumericLiteral() or
        tok_type.isIdentifierLike();
}

/// if token could be a class element key (after modifiers)
inline fn isClassElementKeyStart(tok_type: token.TokenType) bool {
    return tok_type == .star or
        tok_type == .left_bracket or
        tok_type == .private_identifier or
        tok_type == .string_literal or
        tok_type.isNumericLiteral() or
        tok_type.isIdentifierLike();
}

const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const token = @import("../token.zig");
const std = @import("std");
const Precedence = @import("../token.zig").Precedence;

const statements = @import("statements.zig");
const variables = @import("variables.zig");
const array = @import("array.zig");
const object = @import("object.zig");
const literals = @import("literals.zig");
const functions = @import("functions.zig");
const class = @import("class.zig");
const parenthesized = @import("parenthesized.zig");
const patterns = @import("patterns.zig");
const modules = @import("modules.zig");
const grammar = @import("../grammar.zig");

const ParseExpressionOpts = struct {
    /// whether we are parsing this expression in a cover context.
    /// when true, we don't treat the expressions as patterns and also don't decide whether to parse them as patterns
    /// until the top level context is known after the cover is parsed.
    in_cover: bool = false,
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

    if (token_type == .await) {
        if (!(parser.context.in_async or parser.isModule())) {
            try parser.report(
                parser.current_token.span,
                "'await' is only valid in async functions and at the top level of modules",
                .{ .help = "Consider wrapping this code in an async function" },
            );

            return null;
        }

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
        .left_bracket => parseArrayExpression(parser, opts.in_cover),
        .left_brace => parseObjectExpression(parser, opts.in_cover),
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

    return parenthesized.coverToParenthesizedExpression(parser, cover);
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
    return parenthesized.coverToParenthesizedExpression(parser, cover);
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

    const async_id = try literals.parseIdentifier(parser) orelse return null;

    // async function ...
    if (!parser.current_token.has_line_terminator_before and parser.current_token.type == .function) {
        return functions.parseFunction(parser, .{ .is_expression = true, .is_async = true }, start);
    }

    // async (params) => ...
    if (!parser.current_token.has_line_terminator_before and parser.current_token.type == .left_paren) {
        return parseAsyncArrowFunctionOrCall(parser, true, start, async_id, precedence);
    }

    // [no LineTerminator here] => ConciseBody
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

fn parseAsyncArrowFunctionOrCall(parser: *Parser, is_async: bool, arrow_start: ?u32, async_id: u32, precedence: u8) Error!?ast.NodeIndex {
    const start = arrow_start orelse parser.current_token.span.start;

    const cover = try parenthesized.parseCover(parser) orelse return null;

    // [no LineTerminator here] => ConciseBody
    // async (...) => ...
    if (parser.current_token.type == .arrow and !parser.current_token.has_line_terminator_before and precedence <= Precedence.Assignment) {
        return parenthesized.coverToArrowFunction(parser, cover, is_async, start);
    }

    // async(...)
    return parenthesized.coverToCallExpression(parser, cover, async_id);
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
        const argument = try parseExpression(parser, Precedence.Multiplicative, .{}) orelse return null;
        const span = parser.getSpan(argument);

        const unwrapped = parenthesized.unwrapParens(parser, argument);

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

    const unwrapped = parenthesized.unwrapParens(parser, left);

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
    const operator_token = parser.current_token;
    const operator = ast.AssignmentOperator.fromToken(operator_token.type);

    const left_span = parser.getSpan(left);

    try grammar.expressionToPattern(parser, left, .assignable) orelse return null;

    // validate that left side can be assigned to
    if (!isValidAssignmentTarget(parser, left, operator)) {
        try parser.report(
            left_span,
            "Invalid left-hand side in assignment",
            .{ .help = "The left side of an assignment must be a variable, property access, or destructuring pattern." },
        );
        return null;
    }

    // logical assignments (&&=, ||=, ??=) require simple targets
    const is_logical = operator == .logical_and_assign or operator == .logical_or_assign or operator == .nullish_assign;
    if (is_logical and !isSimpleAssignmentTarget(parser, left)) {
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
        .{ .assignment_expression = .{ .left = left, .right = right, .operator = operator } },
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
pub fn isValidAssignmentTarget(parser: *Parser, index: ast.NodeIndex, operator: ast.AssignmentOperator) bool {
    const data = parser.getData(index);

    // object and array patterns as assignment targets are only
    // valid if the operator is assignment (=)
    if (operator == .assign and (data == .object_pattern or data == .array_pattern))
        return true;

    return switch (data) {
        // SimpleAssignmentTarget
        .identifier_reference, .binding_identifier => true,
        .member_expression => |m| !m.optional,

        else => false,
    };
}

/// SimpleAssignmentTarget: only identifier and member expressions (no destructuring)
pub fn isSimpleAssignmentTarget(parser: *Parser, index: ast.NodeIndex) bool {
    return switch (parser.getData(index)) {
        .identifier_reference, .binding_identifier => true,
        .member_expression => |m| !m.optional, // optional chaining is not a valid assignment target
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

fn isPartOfPattern(parser: *Parser) bool {
    return // means this array is part of assignment expression/pattern
    parser.current_token.type == .assign or
        // means this array is part of for-in/of
        parser.current_token.type == .in or parser.current_token.type == .of;
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

const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const patterns = @import("patterns.zig");

const ParseFunctionOpts = packed struct {
    is_async: bool = false,
    is_expression: bool = false,
    is_declare: bool = false,
    /// for export default function, allows optional name but produces FunctionDeclaration
    is_default_export: bool = false,
};

pub fn parseFunction(parser: *Parser, opts: ParseFunctionOpts, start_from_param: ?u32) Error!?ast.NodeIndex {
    const start = start_from_param orelse parser.current_token.span.start;

    if (!try parser.expect(
        .function,
        "Expected 'function' keyword",
        null,
    )) return null;

    // export default function produces a declaration with optional name
    // regular function expression allows optional name but produces expression
    const function_type: ast.FunctionType = if (opts.is_expression and !opts.is_default_export)
        .function_expression
    else if (opts.is_declare)
        .ts_declare_function
    else
        .function_declaration;

    var is_generator = false;

    if (parser.current_token.type == .star) {
        is_generator = true;
        try parser.advance();
    }

    parser.context.in_async = false;
    parser.context.in_generator = false;

    const id = if (parser.current_token.type.isIdentifierLike())
        try patterns.parseBindingIdentifier(parser) orelse ast.null_node
    else
        ast.null_node;

    const saved_async = parser.context.in_async;
    const saved_generator = parser.context.in_generator;

    parser.context.in_async = opts.is_async;
    parser.context.in_generator = is_generator;

    defer {
        parser.context.in_async = saved_async;
        parser.context.in_generator = saved_generator;
    }

    // name is required for regular function declarations, but optional for:
    // - function expressions
    // - export default function
    if (!opts.is_expression and !opts.is_default_export and ast.isNull(id)) {
        try parser.report(
            parser.current_token.span,
            "Function declaration requires a name",
            .{ .help = "Add a name after 'function', e.g. 'function myFunc() {}'." },
        );
        return null;
    }

    if (!try parser.expect(
        .left_paren,
        "Expected '(' to start parameter list",
        "Function parameters must be enclosed in parentheses: function name(a, b) {}",
    )) return null;

    const params = try parseFormalParamaters(parser, .formal_parameters) orelse return null;

    const params_end = parser.current_token.span.end; // including )

    if (!try parser.expect(
        .right_paren,
        "Expected ')' to close parameter list",
        "Add a closing parenthesis ')' after the parameters, or check for missing commas between parameters.",
    )) {
        return null;
    }

    var body = ast.null_node;

    if (opts.is_declare) {
        if (parser.current_token.type == .left_brace) {
            try parser.report(
                parser.current_token.span,
                "TS(1183): An implementation cannot be declared in ambient contexts.",
                .{ .help = "Remove the function body or remove the 'declare' modifier" },
            );
            return null;
        }
    } else {
        body = try parseFunctionBody(parser) orelse ast.null_node;
    }

    const end = if (!ast.isNull(body)) parser.getSpan(body).end else params_end;

    if (parser.context.in_single_statement_context and is_generator) {
        @branchHint(.unlikely);
        try parser.report(
            .{ .start = start, .end = params_end },
            "Generators can only be declared at the top level or inside a block",
            .{},
        );
        return null;
    }

    return try parser.addNode(.{
        .function = .{
            .type = function_type,
            .id = id,
            .generator = is_generator,
            .async = opts.is_async,
            .params = params,
            .body = body,
        },
    }, .{
        .start = start,
        .end = end,
    });
}

pub fn parseFunctionBody(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (!try parser.expect(
        .left_brace,
        "Expected '{' to start function body",
        "Function bodies must be enclosed in braces: function name() { ... }",
    )) return null;

    const saved_in_function = parser.context.in_function;
    parser.context.in_function = true;

    defer {
        parser.context.in_function = saved_in_function;
    }

    const body = try parser.parseBody(.right_brace);

    const end = parser.current_token.span.end;

    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close function body",
        "Add a closing brace '}' to complete the function, or check for unbalanced braces inside.",
    )) return null;

    return try parser.addNode(.{ .function_body = .{ .body = body } }, .{ .start = start, .end = end });
}

pub fn parseFormalParamaters(parser: *Parser, kind: ast.FormalParameterKind) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var end: u32 = parser.current_token.span.end;

    const params_checkpoint = parser.scratch_a.begin();

    var rest = ast.null_node;

    while (true) {
        if (parser.current_token.type == .right_paren or parser.current_token.type == .eof) break;

        if (parser.current_token.type == .spread) {
            rest = try patterns.parseBindingRestElement(parser) orelse ast.null_node;
            if (!ast.isNull(rest)) {
                end = parser.getSpan(rest).end;
            }

            if (parser.current_token.type == .comma and !ast.isNull(rest)) {
                try parser.report(
                    .{ .start = parser.getSpan(rest).start, .end = parser.current_token.span.end },
                    "Rest parameter must be the last parameter",
                    .{ .help = "Move the '...rest' parameter to the end of the parameter list, or remove trailing parameters." },
                );

                parser.scratch_a.reset(params_checkpoint);

                return null;
            }
        } else {
            const param = try parseFormalParamater(parser) orelse break;

            end = parser.getSpan(param).end;

            try parser.scratch_a.append(parser.allocator(), param);
        }

        if (parser.current_token.type == .comma) {
            try parser.advance();
        } else break;
    }

    return try parser.addNode(.{ .formal_parameters = .{
        .items = try parser.addExtra(parser.scratch_a.take(params_checkpoint)),
        .rest = rest,
        .kind = kind,
    } }, .{ .start = start, .end = end });
}

pub fn parseFormalParamater(parser: *Parser) Error!?ast.NodeIndex {
    var pattern = try patterns.parseBindingPattern(parser) orelse return null;

    if (parser.current_token.type == .assign) {
        pattern = try patterns.parseAssignmentPattern(parser, pattern) orelse return null;
    }

    return try parser.addNode(.{ .formal_parameter = .{ .pattern = pattern } }, parser.getSpan(pattern));
}

// https://tc39.es/ecma262/#sec-static-semantics-issimpleparameterlist
pub fn isSimpleParametersList(parser: *Parser, formal_parameters: ast.NodeIndex) bool {
    const data = parser.getData(formal_parameters).formal_parameters;

    if (!ast.isNull(data.rest)) {
        return false;
    }

    const items = parser.getExtra(data.items);
    for (items) |item| {
        const param = parser.getData(item).formal_parameter;
        const pattern = parser.getData(param.pattern);
        if (pattern != .binding_identifier) {
            return false;
        }
    }

    return true;
}

const ast = @import("../ast.zig");
const lexer = @import("../lexer.zig");
const Token = @import("../token.zig").Token;
const Precedence = @import("../token.zig").Precedence;
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const expressions = @import("expressions.zig");

pub inline fn parseStringLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance();
    return try parser.addNode(.{
        .string_literal = .{
            .raw_start = token.span.start,
            .raw_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}

pub inline fn parseBooleanLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance();
    return try parser.addNode(.{
        .boolean_literal = .{ .value = token.type == .true },
    }, token.span);
}

pub inline fn parseNullLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance();
    return try parser.addNode(.null_literal, token.span);
}

pub inline fn parseNumericLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance();
    return try parser.addNode(.{
        .numeric_literal = .{
            .raw_start = token.span.start,
            .raw_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}

pub inline fn parseBigIntLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance();
    return try parser.addNode(.{
        .bigint_literal = .{
            .raw_start = token.span.start,
            .raw_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}

pub fn parseRegExpLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    const regex = parser.lexer.reScanAsRegex(token) catch |e| {
        try parser.report(token.span, lexer.getLexicalErrorMessage(e), .{ .help = lexer.getLexicalErrorHelp(e) });
        return null;
    };
    try parser.replaceTokenAndAdvance(parser.lexer.createToken(
        .regex_literal,
        parser.source[regex.span.start..regex.span.end],
        regex.span.start,
        regex.span.end,
    ));
    return try parser.addNode(.{
        .regexp_literal = .{
            .pattern_start = @intCast(regex.span.start + 1),
            .pattern_len = @intCast(regex.pattern.len),
            .flags_start = @intCast(regex.span.end - regex.flags.len),
            .flags_len = @intCast(regex.flags.len),
        },
    }, regex.span);
}

pub fn parseNoSubstitutionTemplate(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance();
    const element_span = getTemplateElementSpan(token);
    const element = try parser.addNode(.{
        .template_element = .{
            .raw_start = element_span.start,
            .raw_len = @intCast(element_span.end - element_span.start),
            .tail = true,
        },
    }, element_span);
    return try parser.addNode(.{
        .template_literal = .{
            .quasis = try parser.addExtra(&[_]ast.NodeIndex{element}),
            .expressions = ast.IndexRange.empty,
        },
    }, token.span);
}

pub fn parseTemplateLiteral(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const quasis_checkpoint = parser.scratch_a.begin();
    const exprs_checkpoint = parser.scratch_b.begin();

    const head = parser.current_token;
    const head_span = getTemplateElementSpan(head);

    try parser.scratch_a.append(parser.allocator(), try parser.addNode(.{
        .template_element = .{
            .raw_start = head_span.start,
            .raw_len = @intCast(head_span.end - head_span.start),
            .tail = false,
        },
    }, head_span));

    try parser.advance();

    var end: u32 = undefined;
    while (true) {
        const expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
        try parser.scratch_b.append(parser.allocator(), expr);

        const token = parser.current_token;
        const is_tail = token.type == .template_tail;

        switch (token.type) {
            .template_middle, .template_tail => {
                const span = getTemplateElementSpan(token);
                try parser.scratch_a.append(parser.allocator(), try parser.addNode(.{
                    .template_element = .{
                        .raw_start = span.start,
                        .raw_len = @intCast(span.end - span.start),
                        .tail = is_tail,
                    },
                }, span));

                if (is_tail) {
                    end = token.span.end;
                    try parser.advance();
                    break;
                }
                try parser.advance();
            },
            else => {
                try parser.report(
                    token.span,
                    "Unexpected token in template literal expression",
                    .{ .help = "Template expressions must be followed by '}' to continue the template string. Check for unmatched braces." },
                );
                parser.scratch_a.reset(quasis_checkpoint);
                parser.scratch_b.reset(exprs_checkpoint);
                return null;
            },
        }
    }

    return try parser.addNode(.{
        .template_literal = .{
            .quasis = try parser.addExtra(parser.scratch_a.take(quasis_checkpoint)),
            .expressions = try parser.addExtra(parser.scratch_b.take(exprs_checkpoint)),
        },
    }, .{ .start = start, .end = end });
}

inline fn getTemplateElementSpan(token: @import("../token.zig").Token) ast.Span {
    return switch (token.type) {
        .template_head, .template_middle => .{
            .start = token.span.start + 1,
            .end = token.span.end - 2,
        },
        .template_tail, .no_substitution_template => .{
            .start = token.span.start + 1,
            .end = token.span.end - 1,
        },
        else => unreachable,
    };
}

pub inline fn parseIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    if (!try validateIdentifier(parser, "an identifier", parser.current_token)) {
        return null;
    }

    const tok = parser.current_token;
    try parser.advance();
    return try parser.addNode(.{
        .identifier_reference = .{
            .name_start = tok.span.start,
            .name_len = @intCast(tok.lexeme.len),
        },
    }, tok.span);
}

pub inline fn parsePrivateIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    const token = parser.current_token;
    try parser.advance();
    return try parser.addNode(.{
        .private_identifier = .{
            .name_start = token.span.start,
            .name_len = @intCast(token.lexeme.len),
        },
    }, token.span);
}

pub fn parseIdentifierName(parser: *Parser) Error!ast.NodeIndex {
    const tok = parser.current_token;
    try parser.advance();
    return try parser.addNode(.{
        .identifier_name = .{
            .name_start = tok.span.start,
            .name_len = @intCast(tok.lexeme.len),
        },
    }, tok.span);
}

pub fn parseLabelIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    if (!try validateIdentifier(parser, "a label", parser.current_token)) {
        return null;
    }

    const current = parser.current_token;
    try parser.advance();

    return try parser.addNode(.{
        .label_identifier = .{
            .name_start = current.span.start,
            .name_len = @intCast(current.lexeme.len),
        },
    }, current.span);
}

pub inline fn validateIdentifier(parser: *Parser, comptime as_what: []const u8, token: Token) Error!bool {
    if (!token.type.isIdentifierLike()) {
        try parser.reportFmt(
            token.span,
            "Expected identifier {s}, found '{s}'",
            .{ as_what, parser.describeToken(token) },
            .{ .help = "Identifiers must start with a letter, underscore (_), or dollar sign ($)" },
        );

        return false;
    }

    if (token.type.isReserved()) {
        try parser.reportFmt(
            token.span,
            "'{s}' is a reserved word and cannot be used as {s}",
            .{ token.lexeme, as_what },
            .{},
        );

        return false;
    }

    if (token.type == .yield and parser.context.in_generator) {
        try parser.reportFmt(
            token.span,
            "Cannot use 'yield' as {s} in a generator context",
            .{as_what},
            .{},
        );

        return false;
    }

    return true;
}

const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const token = @import("../token.zig");
const Precedence = @import("../token.zig").Precedence;

const expressions = @import("expressions.zig");
const statements = @import("statements.zig");
const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const functions = @import("functions.zig");
const class = @import("class.zig");
const variables = @import("variables.zig");

pub fn parseImportDeclaration(parser: *Parser) Error!?ast.NodeIndex {
    if (!parser.isModule()) {
        try parser.report(parser.current_token.span, "'import' statement is only valid in module mode", .{
            .help = "Use dynamic import() for script mode",
        });
        return null;
    }

    const start = parser.current_token.span.start;
    try parser.advance(); // consume 'import'

    // side-effect import: import 'module'
    if (parser.current_token.type == .string_literal) {
        return parseSideEffectImport(parser, start, null);
    }

    var phase: ?ast.ImportPhase = null;

    const next = try parser.lookAhead();

    // import source X from "X"
    if (parser.current_token.type == .source and next.type.isIdentifierLike() and next.type != .from) {
        phase = .source;
        try parser.advance();
    }
    // import defer * as X from "X"
    else if (parser.current_token.type == .@"defer" and next.type == .star) {
        phase = .@"defer";
        try parser.advance();
    }

    // regular import, parse import clause (specifiers)
    const specifiers = try parseImportClause(parser) orelse return null;

    if (parser.current_token.type != .from) {
        try parser.report(parser.current_token.span, "Expected 'from' after import clause", .{
            .help = "Import statements require 'from' followed by a module specifier: import x from 'module'",
        });
        return null;
    }

    try parser.advance(); // consume 'from'

    const source = try parseModuleSpecifier(parser) orelse return null;

    // parse optional 'with' clause
    const attributes = try parseWithClause(parser);

    const end = try parser.eatSemicolon(parser.getSpan(source).end) orelse return null;

    return try parser.addNode(.{
        .import_declaration = .{
            .specifiers = specifiers,
            .source = source,
            .attributes = attributes,
            .phase = phase,
        },
    }, .{ .start = start, .end = end });
}

/// side-effect import: import 'module'
fn parseSideEffectImport(parser: *Parser, start: u32, phase: ?ast.ImportPhase) Error!?ast.NodeIndex {
    const source = try parseModuleSpecifier(parser) orelse return null;
    const attributes = try parseWithClause(parser);
    const end = try parser.eatSemicolon(parser.getSpan(source).end) orelse return null;

    return try parser.addNode(.{
        .import_declaration = .{
            .specifiers = ast.IndexRange.empty,
            .source = source,
            .attributes = attributes,
            .phase = phase,
        },
    }, .{ .start = start, .end = end });
}

/// ImportClause :
///   ImportedDefaultBinding
///   NameSpaceImport
///   NamedImports
///   ImportedDefaultBinding , NameSpaceImport
///   ImportedDefaultBinding , NamedImports
fn parseImportClause(parser: *Parser) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_a.begin();

    // namespace import: * as name
    if (parser.current_token.type == .star) {
        const ns = try parseImportNamespaceSpecifier(parser) orelse {
            parser.scratch_a.reset(checkpoint);
            return null;
        };
        try parser.scratch_a.append(parser.allocator(), ns);
        return try parser.addExtra(parser.scratch_a.take(checkpoint));
    }

    // named imports: { foo, bar }
    if (parser.current_token.type == .left_brace) {
        return parseNamedImports(parser);
    }

    // default import: import foo from 'module'
    const default_import = try parseImportDefaultSpecifier(parser) orelse {
        parser.scratch_a.reset(checkpoint);
        return null;
    };

    try parser.scratch_a.append(parser.allocator(), default_import);

    //     import foo, * as bar from 'module'
    // or: import foo, { bar } from 'module'
    if (parser.current_token.type == .comma) {
        try parser.advance(); // consume ','

        if (parser.current_token.type == .star) {
            const ns = try parseImportNamespaceSpecifier(parser) orelse {
                parser.scratch_a.reset(checkpoint);
                return null;
            };
            try parser.scratch_a.append(parser.allocator(), ns);
        } else if (parser.current_token.type == .left_brace) {
            const named = try parseNamedImports(parser) orelse {
                parser.scratch_a.reset(checkpoint);
                return null;
            };
            // append all named imports
            for (parser.getExtra(named)) |spec| {
                try parser.scratch_a.append(parser.allocator(), spec);
            }
        } else {
            try parser.report(parser.current_token.span, "Expected namespace import (* as name) or named imports ({...}) after ','", .{});
            parser.scratch_a.reset(checkpoint);
            return null;
        }
    }

    return try parser.addExtra(parser.scratch_a.take(checkpoint));
}

/// default import specifier: import foo from 'module'
///                                  ~~~
fn parseImportDefaultSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const local = try parseImportedBinding(parser) orelse return null;
    const end = parser.getSpan(local).end;

    return try parser.addNode(.{
        .import_default_specifier = .{ .local = local },
    }, .{ .start = start, .end = end });
}

/// namespace import: * as name
fn parseImportNamespaceSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (!try parser.expect(.star, "Expected '*' for namespace import", null)) return null;

    if (parser.current_token.type != .as) {
        try parser.report(parser.current_token.span, "Expected 'as' after '*' in namespace import", .{
            .help = "Namespace imports must use the form: * as name",
        });
        return null;
    }
    try parser.advance(); // consume 'as'

    const local = try parseImportedBinding(parser) orelse return null;
    const end = parser.getSpan(local).end;

    return try parser.addNode(.{
        .import_namespace_specifier = .{ .local = local },
    }, .{ .start = start, .end = end });
}

/// named imports: { foo, bar as baz }
fn parseNamedImports(parser: *Parser) Error!?ast.IndexRange {
    const checkpoint = parser.scratch_a.begin();

    if (!try parser.expect(.left_brace, "Expected '{' to start named imports", null)) return null;

    while (parser.current_token.type != .right_brace and parser.current_token.type != .eof) {
        const spec = try parseImportSpecifier(parser) orelse {
            parser.scratch_a.reset(checkpoint);
            return null;
        };
        try parser.scratch_a.append(parser.allocator(), spec);

        if (parser.current_token.type == .comma) {
            try parser.advance();
        } else {
            break;
        }
    }

    if (!try parser.expect(.right_brace, "Expected '}' to close named imports", null)) {
        parser.scratch_a.reset(checkpoint);
        return null;
    }

    return try parser.addExtra(parser.scratch_a.take(checkpoint));
}

/// import specifier: foo or foo as bar or "string" as bar
///                   ~~~    ~~~~~~~~~~    ~~~~~~~~~~~~~~~
fn parseImportSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const imported_token = parser.current_token;

    // parse imported name
    const imported = try parseModuleExportName(parser) orelse return null;

    var local: ast.NodeIndex = undefined;

    // check for 'as' alias
    if (parser.current_token.type == .as) {
        try parser.advance(); // consume 'as'
        local = try parseImportedBinding(parser) orelse return null;
    } else {
        // no alias - local is the same as imported
        // but we need to convert IdentifierName to BindingIdentifier if it's not a string

        const imported_data = parser.getData(imported);

        if (imported_data == .string_literal) {
            try parser.report(parser.getSpan(imported), "String literal imports require an 'as' clause", .{
                .help = "Use: import { \"name\" as localName } from 'module'",
            });
            return null;
        }

        // convert identifier_name to binding_identifier
        // since it is now a binding identifier, we need to validate like reserved words, etc.
        if (!try literals.validateIdentifier(parser, "an imported binding", imported_token)) {
            return null;
        }

        const id_data = imported_data.identifier_name;

        parser.setData(imported, .{
            .binding_identifier = .{
                .name_start = id_data.name_start,
                .name_len = id_data.name_len,
            },
        });

        local = imported;
    }

    const end = parser.getSpan(local).end;

    return try parser.addNode(.{
        .import_specifier = .{
            .imported = imported,
            .local = local,
        },
    }, .{ .start = start, .end = end });
}

/// ImportedBinding: BindingIdentifier[~Yield, +Await]
fn parseImportedBinding(parser: *Parser) Error!?ast.NodeIndex {
    return patterns.parseBindingIdentifier(parser);
}

pub fn parseExportDeclaration(parser: *Parser) Error!?ast.NodeIndex {
    if (!parser.isModule()) {
        try parser.report(parser.current_token.span, "'export' statement is only valid in module mode", .{
            .help = "Export declarations can only appear in module mode",
        });
        return null;
    }

    const start = parser.current_token.span.start;
    try parser.advance(); // consume 'export'

    // export = expression
    if (parser.isTs() and parser.current_token.type == .assign) {
        return parseTSExportAssignment(parser, start);
    }

    // export as namespace name
    if (parser.isTs() and parser.current_token.type == .as) {
        return parseTSNamespaceExportDeclaration(parser, start);
    }

    // export default ...
    if (parser.current_token.type == .default) {
        return parseExportDefaultDeclaration(parser, start);
    }

    // export * from 'module'
    // export * as name from 'module'
    if (parser.current_token.type == .star) {
        return parseExportAllDeclaration(parser, start);
    }

    // export { foo, bar }
    // export { foo } from 'module'
    if (parser.current_token.type == .left_brace) {
        return parseExportNamedFromClause(parser, start);
    }

    // export var/let/const/function/class
    return parseExportWithDeclaration(parser, start);
}

/// export = expression
fn parseTSExportAssignment(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    try parser.advance(); // consume '='

    const expression = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
    const end = try parser.eatSemicolon(parser.getSpan(expression).end) orelse return null;

    return try parser.addNode(.{
        .ts_export_assignment = .{ .expression = expression },
    }, .{ .start = start, .end = end });
}

/// export as namespace name
fn parseTSNamespaceExportDeclaration(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    try parser.advance(); // consume 'as'

    if (parser.current_token.type != .namespace) {
        try parser.report(parser.current_token.span, "Expected 'namespace' after 'export as'", .{});
        return null;
    }

    try parser.advance(); // consume 'namespace'

    const id = try literals.parseIdentifierName(parser);
    const end = try parser.eatSemicolon(parser.getSpan(id).end) orelse return null;

    return try parser.addNode(.{
        .ts_namespace_export_declaration = .{ .id = id },
    }, .{ .start = start, .end = end });
}

/// export default declaration
fn parseExportDefaultDeclaration(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    try parser.advance(); // consume 'default'

    var declaration: ast.NodeIndex = undefined;
    var is_decl = false;

    // export default function [name]() {}
    if (parser.current_token.type == .function) {
        declaration = try functions.parseFunction(parser, .{ .is_default_export = true }, null) orelse return null;
        is_decl = true;
    }

    // export default async function [name]() {}
    else if (parser.current_token.type == .async and !parser.current_token.has_line_terminator_before) {
        const async_start = parser.current_token.span.start;
        try parser.advance(); // consume 'async'
        if (parser.current_token.type == .function) {
            declaration = try functions.parseFunction(parser, .{ .is_default_export = true, .is_async = true }, async_start) orelse return null;
            is_decl = true;
        } else {
            // if it's not a async function, it's an identifier
            // export default async;
            const async_end = async_start + 5;

            declaration = try parser.addNode(.{
                .identifier_reference = .{
                    .name_start = async_start,
                    .name_len = @intCast(async_end - async_start),
                },
            }, .{ .start = async_start, .end = async_end });
        }
    }

    // export default class [name] {}
    else if (parser.current_token.type == .class) {
        declaration = try class.parseClass(parser, .{ .is_default_export = true }, null) orelse return null;
        is_decl = true;
    }
    // export default expression
    else {
        declaration = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
    }

    const decl_span = parser.getSpan(declaration);

    // function/class declarations don't need semicolon
    const end = if (is_decl)
        decl_span.end
    else
        try parser.eatSemicolon(decl_span.end) orelse return null;

    return try parser.addNode(.{
        .export_default_declaration = .{ .declaration = declaration },
    }, .{ .start = start, .end = end });
}

/// export * from 'module' or export * as name from 'module'
fn parseExportAllDeclaration(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    try parser.advance(); // consume '*'

    var exported: ast.NodeIndex = ast.null_node;

    // export * as name from 'module'
    if (parser.current_token.type == .as) {
        try parser.advance(); // consume 'as'
        exported = try parseModuleExportName(parser) orelse return null;
    }

    // expect 'from'
    if (parser.current_token.type != .from) {
        try parser.report(parser.current_token.span, "Expected 'from' after export *", .{
            .help = "Export all declarations require 'from': export * from 'module'",
        });
        return null;
    }
    try parser.advance(); // consume 'from'

    const source = try parseModuleSpecifier(parser) orelse return null;
    const attributes = try parseWithClause(parser);
    const end = try parser.eatSemicolon(parser.getSpan(source).end) orelse return null;

    return try parser.addNode(.{
        .export_all_declaration = .{
            .exported = exported,
            .source = source,
            .attributes = attributes,
        },
    }, .{ .start = start, .end = end });
}

/// export { foo, bar } or export { foo } from 'module'
fn parseExportNamedFromClause(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    const result = try parseExportSpecifiers(parser) orelse return null;
    const specifiers = result.specifiers;

    var source: ast.NodeIndex = ast.null_node;
    var attributes: ast.IndexRange = ast.IndexRange.empty;
    var end = parser.current_token.span.start;

    // re-export: export { foo } from 'module'
    if (parser.current_token.type == .from) {
        try parser.advance(); // consume 'from'
        source = try parseModuleSpecifier(parser) orelse return null;
        attributes = try parseWithClause(parser);
        end = parser.getSpan(source).end;
    } else {
        const specs = parser.getExtra(specifiers);

        for (specs, 0..) |spec_idx, i| {
            const specifier = parser.getData(spec_idx).export_specifier;
            const local_data = parser.getData(specifier.local);
            const local_span = parser.getSpan(specifier.local);

            if (local_data == .string_literal) {
                try parser.report(local_span, "A string literal cannot be used as an exported binding without 'from'", .{
                    .help = "Use: export { \"name\" } from 'some-module' or export { localName as \"name\" }",
                });
                return null;
            }

            const local_token_type: token.TokenType = @enumFromInt(result.local_token_types[i]);

            if (local_token_type.isStrictModeReserved()) {
                const local_name = parser.getSourceText(local_data.identifier_name.name_start, local_data.identifier_name.name_len);

                try parser.reportFmt(
                    local_span,
                    "A reserved word cannot be used as an exported binding without 'from'",
                    .{},
                    .{ .help = try parser.formatMessage("Did you mean `export {{ {s} as {s} }} from 'some-module'`?", .{ local_name, local_name }) },
                );
                return null;
            }
        }
    }

    end = try parser.eatSemicolon(end) orelse return null;

    return try parser.addNode(.{
        .export_named_declaration = .{
            .declaration = ast.null_node,
            .specifiers = specifiers,
            .source = source,
            .attributes = attributes,
        },
    }, .{ .start = start, .end = end });
}

/// export var/let/const/function/class
fn parseExportWithDeclaration(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    var declaration: ast.NodeIndex = undefined;

    switch (parser.current_token.type) {
        .@"var", .@"const", .let => {
            declaration = try variables.parseVariableDeclaration(parser, false) orelse return null;
        },
        .function => {
            declaration = try functions.parseFunction(parser, .{}, null) orelse return null;
        },
        .async => {
            const async_start = parser.current_token.span.start;
            try parser.advance(); // consume 'async'
            declaration = try functions.parseFunction(parser, .{ .is_async = true }, async_start) orelse return null;
        },
        .class => {
            declaration = try class.parseClass(parser, .{}, null) orelse return null;
        },
        else => {
            try parser.report(parser.current_token.span, "Expected declaration after 'export'", .{
                .help = "Use 'export var', 'export let', 'export const', 'export function', or 'export class'",
            });
            return null;
        },
    }

    return try parser.addNode(.{
        .export_named_declaration = .{
            .declaration = declaration,
            .specifiers = ast.IndexRange.empty,
            .source = ast.null_node,
            .attributes = ast.IndexRange.empty,
        },
    }, .{ .start = start, .end = parser.getSpan(declaration).end });
}

const ExportSpecifiersResult = struct {
    specifiers: ast.IndexRange,
    local_token_types: []const u32,
};

/// export specifiers: { foo, bar as baz }
fn parseExportSpecifiers(parser: *Parser) Error!?ExportSpecifiersResult {
    const checkpoint = parser.scratch_a.begin();
    const token_checkpoint = parser.scratch_b.begin();

    if (!try parser.expect(.left_brace, "Expected '{' to start export specifiers", null)) return null;

    while (parser.current_token.type != .right_brace and parser.current_token.type != .eof) {
        const local_token_type = parser.current_token.type;

        const spec = try parseExportSpecifier(parser) orelse {
            parser.scratch_a.reset(checkpoint);
            parser.scratch_b.reset(token_checkpoint);
            return null;
        };

        try parser.scratch_a.append(parser.allocator(), spec);

        try parser.scratch_b.append(parser.allocator(), @intFromEnum(local_token_type));

        if (parser.current_token.type == .comma) {
            try parser.advance();
        } else {
            break;
        }
    }

    if (!try parser.expect(.right_brace, "Expected '}' to close export specifiers", null)) {
        parser.scratch_a.reset(checkpoint);
        parser.scratch_b.reset(token_checkpoint);
        return null;
    }

    return .{
        .specifiers = try parser.addExtra(parser.scratch_a.take(checkpoint)),
        .local_token_types = parser.scratch_b.take(token_checkpoint),
    };
}

/// export specifier: foo or foo as bar
fn parseExportSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    // local name (can be identifier or string literal)
    const local = try parseModuleExportName(parser) orelse return null;

    var exported: ast.NodeIndex = undefined;

    if (parser.current_token.type == .as) {
        try parser.advance(); // consume 'as'
        exported = try parseModuleExportName(parser) orelse return null;
    } else {
        // exported is the same as local
        exported = local;
    }

    const end = parser.getSpan(exported).end;

    return try parser.addNode(.{
        .export_specifier = .{
            .local = local,
            .exported = exported,
        },
    }, .{ .start = start, .end = end });
}

/// ModuleExportName: IdentifierName or StringLiteral
fn parseModuleExportName(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.type == .string_literal) {
        return literals.parseStringLiteral(parser);
    }

    if (parser.current_token.type.isIdentifierLike()) {
        return try literals.parseIdentifierName(parser);
    }

    try parser.report(parser.current_token.span, "Expected identifier or string literal", .{});

    return null;
}

/// ModuleSpecifier: StringLiteral
fn parseModuleSpecifier(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.type != .string_literal) {
        try parser.report(parser.current_token.span, "Expected module specifier", .{
            .help = "Module specifiers must be string literals, e.g., './module.js' or 'package'",
        });
        return null;
    }

    return literals.parseStringLiteral(parser);
}

/// WithClause / ImportAttributes
/// WithClause :
///   with { }
///   with { WithEntries ,? }
fn parseWithClause(parser: *Parser) Error!ast.IndexRange {
    // check for 'with' or 'assert' keyword
    if (parser.current_token.type != .with and parser.current_token.type != .assert) {
        return ast.IndexRange.empty;
    }

    try parser.advance(); // consume 'with' or 'assert'

    if (!try parser.expect(.left_brace, "Expected '{' after 'with' in import attributes", null)) {
        return ast.IndexRange.empty;
    }

    const checkpoint = parser.scratch_a.begin();

    while (parser.current_token.type != .right_brace and parser.current_token.type != .eof) {
        const attr = try parseImportAttribute(parser) orelse {
            parser.scratch_a.reset(checkpoint);
            return ast.IndexRange.empty;
        };
        try parser.scratch_a.append(parser.allocator(), attr);

        if (parser.current_token.type == .comma) {
            try parser.advance();
        } else {
            break;
        }
    }

    if (!try parser.expect(.right_brace, "Expected '}' to close import attributes", null)) {
        parser.scratch_a.reset(checkpoint);
        return ast.IndexRange.empty;
    }

    return parser.addExtra(parser.scratch_a.take(checkpoint));
}

/// ImportAttribute: key : value
fn parseImportAttribute(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    // IdentifierName or StringLiteral
    const key = try parseAttributeKey(parser) orelse return null;

    if (!try parser.expect(.colon, "Expected ':' in import attribute", null)) return null;

    // value (must be StringLiteral)
    if (parser.current_token.type != .string_literal) {
        try parser.report(parser.current_token.span, "Import attribute value must be a string literal", .{});
        return null;
    }

    const value = try literals.parseStringLiteral(parser) orelse return null;

    return try parser.addNode(.{
        .import_attribute = .{
            .key = key,
            .value = value,
        },
    }, .{ .start = start, .end = parser.getSpan(value).end });
}

/// AttributeKey: IdentifierName or StringLiteral
fn parseAttributeKey(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.type == .string_literal) {
        return literals.parseStringLiteral(parser);
    }

    if (parser.current_token.type.isIdentifierLike()) {
        return try literals.parseIdentifierName(parser);
    }

    try parser.report(parser.current_token.span, "Expected identifier or string literal for attribute key", .{});
    return null;
}

/// dynamic import: import(source), import(source, options), import.source(source), import.defer(source)
pub fn parseDynamicImport(parser: *Parser, import_keyword: ast.NodeIndex, phase: ?ast.ImportPhase) Error!?ast.NodeIndex {
    const start = parser.getSpan(import_keyword).start;

    if (!try parser.expect(.left_paren, "Expected '(' after import", null)) return null;

    // source expression
    const source = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

    var options: ast.NodeIndex = ast.null_node;

    // check for options argument (only for regular imports, not phase imports)
    if (phase == null and parser.current_token.type == .comma) {
        // allow trailing comma
        try parser.advance(); // consume ','

        if (parser.current_token.type != .right_paren) {
            options = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

            // allow trailing comma after options
            if (parser.current_token.type == .comma) {
                try parser.advance();
            }
        }
    }

    const end = parser.current_token.span.end;

    if (!try parser.expect(.right_paren, "Expected ')' after import()", "Dynamic import call must end with ')'")) return null;

    return try parser.addNode(.{
        .import_expression = .{
            .source = source,
            .options = options,
            .phase = phase,
        },
    }, .{ .start = start, .end = end });
}

const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const token = @import("../token.zig");
const Precedence = @import("../token.zig").Precedence;

const literals = @import("literals.zig");
const grammar = @import("../grammar.zig");
const functions = @import("functions.zig");

/// result from parsing object cover grammar: {a, b: c, ...d}
pub const ObjectCover = struct {
    properties: []const ast.NodeIndex,
    start: u32,
    end: u32,
};

/// parse object literal permissively using cover grammar: {a, b: c, ...d}
/// returns raw properties for later conversion to ObjectExpression or ObjectPattern.
/// https://tc39.es/ecma262/#sec-object-initializer (covers ObjectAssignmentPattern)
pub fn parseCover(parser: *Parser) Error!?ObjectCover {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume {

    const checkpoint = parser.scratch_cover.begin();
    errdefer parser.scratch_cover.reset(checkpoint);

    var end = start + 1;

    while (parser.current_token.type != .right_brace and parser.current_token.type != .eof) {
        // spread: {...x}
        if (parser.current_token.type == .spread) {
            const spread_start = parser.current_token.span.start;
            try parser.advance();
            const argument = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse {
                parser.scratch_cover.reset(checkpoint);
                return null;
            };
            const spread_end = parser.getSpan(argument).end;
            const spread = try parser.addNode(
                .{ .spread_element = .{ .argument = argument } },
                .{ .start = spread_start, .end = spread_end },
            );
            try parser.scratch_cover.append(parser.allocator(), spread);
            end = spread_end;
        } else {
            // property
            const prop = try parseCoverProperty(parser) orelse {
                parser.scratch_cover.reset(checkpoint);
                return null;
            };
            try parser.scratch_cover.append(parser.allocator(), prop);
            end = parser.getSpan(prop).end;
        }

        // comma or end
        if (parser.current_token.type == .comma) {
            try parser.advance();
            // then it's a trailing comma
            if (parser.current_token.type == .right_brace) {
                parser.state.cover_has_trailing_comma = start;
            }
        } else if (parser.current_token.type != .right_brace) {
            try parser.report(
                parser.current_token.span,
                "Expected ',' or '}' in object",
                .{ .help = "Add a comma between properties or close the object with '}'." },
            );
            parser.scratch_cover.reset(checkpoint);
            return null;
        }
    }

    if (parser.current_token.type != .right_brace) {
        try parser.report(
            .{ .start = start, .end = end },
            "Unterminated object",
            .{
                .help = "Add a closing '}' to complete the object.",
                .labels = try parser.makeLabels(&.{parser.label(.{ .start = start, .end = start + 1 }, "Opened here")}),
            },
        );
        parser.scratch_cover.reset(checkpoint);
        return null;
    }

    end = parser.current_token.span.end;
    try parser.advance(); // consume }

    return .{
        .properties = parser.scratch_cover.take(checkpoint),
        .start = start,
        .end = end,
    };
}

/// parse a single property in object cover grammar.
fn parseCoverProperty(parser: *Parser) Error!?ast.NodeIndex {
    const prop_start = parser.current_token.span.start;
    var is_async = false;
    var is_generator = false;
    var kind: ast.PropertyKind = .init;
    var computed = false;

    var key: ast.NodeIndex = ast.null_node;
    var key_identifier_token: ?token.Token = null;

    // check for async, consume it, then decide if it's a modifier or key based on what follows
    if (parser.current_token.type == .async) {
        const async_token = parser.current_token;
        try parser.advance();

        if (isPropertyKeyStart(parser.current_token.type)) {
            is_async = true;
        } else {
            // it's a key named "async"
            key = try parser.addNode(
                .{ .identifier_name = .{ .name_start = async_token.span.start, .name_len = @intCast(async_token.lexeme.len) } },
                async_token.span,
            );
        }
    }

    // check for generator, only if we don't already have a key
    if (ast.isNull(key) and parser.current_token.type == .star) {
        is_generator = true;
        try parser.advance();
    }

    // check for get/set, only if no async/generator modifiers and no key yet
    if (ast.isNull(key) and !is_async and !is_generator and parser.current_token.type == .identifier) {
        const lexeme = parser.current_token.lexeme;

        if (std.mem.eql(u8, lexeme, "get") or std.mem.eql(u8, lexeme, "set")) {
            const get_set_token = parser.current_token;
            try parser.advance();

            if (isPropertyKeyStart(parser.current_token.type)) {
                kind = if (std.mem.eql(u8, lexeme, "get")) .get else .set;
            } else {
                key = try parser.addNode(
                    .{ .identifier_name = .{ .name_start = get_set_token.span.start, .name_len = @intCast(get_set_token.lexeme.len) } },
                    get_set_token.span,
                );
            }
        }
    }

    // parse property key if not already determined
    if (ast.isNull(key)) {
        if (parser.current_token.type == .left_bracket) {
            computed = true;
            try parser.advance();
            key = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse return null;
            if (!try parser.expect(.right_bracket, "Expected ']' after computed property key", null)) {
                return null;
            }
        } else if (parser.current_token.type.isIdentifierLike()) {
            key_identifier_token = parser.current_token;
            key = try literals.parseIdentifierName(parser);
        } else if (parser.current_token.type == .string_literal) {
            key = try literals.parseStringLiteral(parser) orelse return null;
        } else if (parser.current_token.type.isNumericLiteral()) {
            key = try literals.parseNumericLiteral(parser) orelse return null;
        } else {
            try parser.reportFmt(
                parser.current_token.span,
                "Unexpected token '{s}' as property key",
                .{parser.describeToken(parser.current_token)},
                .{ .help = "Property keys must be identifiers, strings, numbers, or computed expressions [expr]." },
            );
            return null;
        }
    }

    const key_span = parser.getSpan(key);

    // method definition, key followed by (
    if (parser.current_token.type == .left_paren) {
        return parseObjectMethodProperty(parser, prop_start, key, computed, kind, is_async, is_generator);
    }

    // if we had async, generator, or get/set prefix but no (, it's an error
    if (is_async or is_generator or kind != .init) {
        try parser.report(
            parser.current_token.span,
            "Expected '(' for method definition",
            .{ .help = "Method definitions require a parameter list. Use 'method() {}' syntax." },
        );
        return null;
    }

    // regular property: key: value
    if (parser.current_token.type == .colon) {
        try parser.advance();
        const value = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse return null;
        return try parser.addNode(
            .{ .object_property = .{ .key = key, .value = value, .kind = .init, .method = false, .shorthand = false, .computed = computed } },
            .{ .start = prop_start, .end = parser.getSpan(value).end },
        );
    }

    // CoverInitializedName: a = default
    if (parser.current_token.type == .assign) {
        if (computed) {
            try parser.report(
                key_span,
                "Computed property cannot have a default value without ':'",
                .{ .help = "Use '[key]: value = default' syntax instead." },
            );
            return null;
        }

        const key_data = parser.getData(key);
        if (key_data != .identifier_name) {
            try parser.report(
                key_span,
                "Invalid shorthand property initializer",
                .{ .help = "Only identifier keys can have default values. Use 'key: value = default' syntax." },
            );
            return null;
        }

        try parser.advance();
        const default_value = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse return null;

        const id_ref = try parser.addNode(
            .{ .identifier_reference = .{ .name_start = key_data.identifier_name.name_start, .name_len = key_data.identifier_name.name_len } },
            key_span,
        );

        const assign_expr = try parser.addNode(
            .{ .assignment_expression = .{ .left = id_ref, .right = default_value, .operator = .assign } },
            .{ .start = key_span.start, .end = parser.getSpan(default_value).end },
        );

        parser.state.cover_has_init_name = true;

        return try parser.addNode(
            .{ .object_property = .{ .key = key, .value = assign_expr, .kind = .init, .method = false, .shorthand = true, .computed = false } },
            .{ .start = prop_start, .end = parser.getSpan(default_value).end },
        );
    }

    if (computed) {
        try parser.report(
            key_span,
            "Computed property must have a value",
            .{ .help = "Add ': value' after the computed key." },
        );
        return null;
    }

    // shorthand property: { a }

    if (key_identifier_token) |key_token| {
        if (!try literals.validateIdentifier(parser, "an identifier", key_token)) {
            return null;
        }
    }

    const key_data = parser.getData(key);

    if (key_data != .identifier_name) {
        try parser.report(
            key_span,
            "Shorthand property must be an identifier",
            .{ .help = "String and numeric keys require explicit ': value' syntax." },
        );
        return null;
    }

    const value = try parser.addNode(
        .{ .identifier_reference = .{ .name_start = key_data.identifier_name.name_start, .name_len = key_data.identifier_name.name_len } },
        key_span,
    );

    return try parser.addNode(
        .{ .object_property = .{ .key = key, .value = value, .kind = .init, .method = false, .shorthand = true, .computed = false } },
        .{ .start = prop_start, .end = key_span.end },
    );
}

inline fn isPropertyKeyStart(token_type: @import("../token.zig").TokenType) bool {
    return token_type == .star or
        token_type == .left_bracket or
        token_type.isIdentifierLike() or
        token_type == .string_literal or
        token_type.isNumericLiteral();
}

/// parse a method definition: key(...) { ... }
fn parseObjectMethodProperty(
    parser: *Parser,
    prop_start: u32,
    key: ast.NodeIndex,
    computed: bool,
    kind: ast.PropertyKind,
    is_async: bool,
    is_generator: bool,
) Error!?ast.NodeIndex {
    if (kind == .get and is_generator) {
        try parser.report(
            .{ .start = prop_start, .end = parser.current_token.span.end },
            "Getter cannot be a generator",
            .{ .help = "Remove the '*' from the getter definition." },
        );
        return null;
    }

    if (kind == .set and is_generator) {
        try parser.report(
            .{ .start = prop_start, .end = parser.current_token.span.end },
            "Setter cannot be a generator",
            .{ .help = "Remove the '*' from the setter definition." },
        );
        return null;
    }

    const saved_async = parser.context.in_async;
    const saved_generator = parser.context.in_generator;

    parser.context.in_async = is_async;
    parser.context.in_generator = is_generator;

    defer {
        parser.context.in_async = saved_async;
        parser.context.in_generator = saved_generator;
    }

    const func_start = parser.current_token.span.start;
    if (!try parser.expect(.left_paren, "Expected '(' to start method parameters", null)) {
        return null;
    }

    const params = try functions.parseFormalParamaters(parser, .unique_formal_parameters) orelse return null;
    const params_data = parser.getData(params).formal_parameters;

    // validate getter has no parameters
    if (kind == .get) {
        if (params_data.items.len != 0 or !ast.isNull(params_data.rest)) {
            try parser.report(
                parser.getSpan(params),
                "Getter must have no parameters",
                .{ .help = "Remove all parameters from the getter." },
            );
            return null;
        }
    }

    // validate setter has exactly one parameter
    if (kind == .set) {
        if (params_data.items.len != 1 or !ast.isNull(params_data.rest)) {
            try parser.report(
                parser.getSpan(params),
                "Setter must have exactly one parameter",
                .{ .help = "Setters accept exactly one argument." },
            );
            return null;
        }
    }

    if (!try parser.expect(.right_paren, "Expected ')' after method parameters", null)) {
        return null;
    }

    // parse body
    const body = try functions.parseFunctionBody(parser) orelse return null;
    const body_end = parser.getSpan(body).end;

    // create function expression for the method value
    const func = try parser.addNode(
        .{ .function = .{
            .type = .function_expression,
            .id = ast.null_node,
            .generator = is_generator,
            .async = is_async,
            .params = params,
            .body = body,
        } },
        .{ .start = func_start, .end = body_end },
    );

    // for methods, kind is always .init (getters/setters have their own kind)
    const prop_kind: ast.PropertyKind = kind;

    const is_method = kind == .init;

    return try parser.addNode(
        .{ .object_property = .{
            .key = key,
            .value = func,
            .kind = prop_kind,
            .method = is_method,
            .shorthand = false,
            .computed = computed,
        } },
        .{ .start = prop_start, .end = body_end },
    );
}

/// convert object cover to ObjectExpression.
/// validates that the expression does not contain CoverInitializedName when validate=true.
pub fn coverToExpression(parser: *Parser, cover: ObjectCover, validate: bool) Error!?ast.NodeIndex {
    const object_expression = try parser.addNode(
        .{ .object_expression = .{ .properties = try parser.addExtra(cover.properties) } },
        .{ .start = cover.start, .end = cover.end },
    );

    if (validate and !try grammar.validateNoCoverInitializedSyntax(parser, object_expression)) {
        return null;
    }

    return object_expression;
}

/// convert object cover to ObjectPattern.
pub fn coverToPattern(parser: *Parser, cover: ObjectCover, context: grammar.PatternContext) Error!?ast.NodeIndex {
    const properties_range = try parser.addExtra(cover.properties);
    return toObjectPatternImpl(parser, null, properties_range, .{ .start = cover.start, .end = cover.end }, context);
}

/// convert ObjectExpression to ObjectPattern (mutates in-place).
pub fn toObjectPattern(parser: *Parser, expr_node: ast.NodeIndex, properties_range: ast.IndexRange, span: ast.Span, context: grammar.PatternContext) Error!?void {
    _ = try toObjectPatternImpl(parser, expr_node, properties_range, span, context) orelse return null;
}

fn toObjectPatternImpl(parser: *Parser, mutate_node: ?ast.NodeIndex, properties_range: ast.IndexRange, span: ast.Span, context: grammar.PatternContext) Error!?ast.NodeIndex {
    const properties = parser.getExtra(properties_range);

    var rest: ast.NodeIndex = ast.null_node;
    var properties_len = properties_range.len;

    for (properties, 0..) |prop, i| {
        const prop_data = parser.getData(prop);

        if (prop_data == .spread_element) {
            if (parser.state.cover_has_trailing_comma == span.start) {
                try parser.report(span, "Rest element cannot have a trailing comma in object destructuring.", .{
                    .help = "Remove the trailing comma after the rest element",
                });

                parser.state.cover_has_trailing_comma = null;

                return null;
            }

            if (i != properties.len - 1) {
                try parser.report(parser.getSpan(prop), "Rest element must be the last property", .{
                    .help = "No properties can follow the rest element in a destructuring pattern.",
                });
                return null;
            }

            const arg = prop_data.spread_element.argument;
            const arg_data = parser.getData(arg);

            if (arg_data != .identifier_reference) {
                try parser.report(parser.getSpan(arg), "Rest element argument must be an identifier", .{
                    .help = "Object rest patterns only accept simple identifiers.",
                });
                return null;
            }

            parser.setData(arg, .{ .binding_identifier = .{
                .name_start = arg_data.identifier_reference.name_start,
                .name_len = arg_data.identifier_reference.name_len,
            } });

            parser.setData(prop, .{ .binding_rest_element = .{ .argument = arg } });
            rest = prop;
            properties_len = @intCast(i);
            break;
        }

        if (prop_data != .object_property) {
            try parser.report(parser.getSpan(prop), "Invalid property in object pattern", .{});
            return null;
        }

        const obj_prop = prop_data.object_property;

        if (obj_prop.method) {
            try parser.report(parser.getSpan(prop), "Method cannot appear in destructuring pattern", .{
                .help = "Use a regular property instead of a method definition.",
            });
            return null;
        }

        if (obj_prop.kind != .init) {
            try parser.report(parser.getSpan(prop), "Getter/setter cannot appear in destructuring pattern", .{
                .help = "Use a regular property instead of a getter or setter.",
            });
            return null;
        }

        try grammar.expressionToPattern(parser, obj_prop.value, context) orelse return null;

        parser.setData(prop, .{ .binding_property = .{
            .key = obj_prop.key,
            .value = obj_prop.value,
            .shorthand = obj_prop.shorthand,
            .computed = obj_prop.computed,
        } });
    }

    const pattern_data: ast.NodeData = .{ .object_pattern = .{
        .properties = .{ .start = properties_range.start, .len = properties_len },
        .rest = rest,
    } };

    if (mutate_node) |node| {
        parser.setData(node, pattern_data);
        return node;
    }

    return try parser.addNode(pattern_data, span);
}

const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const token = @import("../token.zig");
const Precedence = @import("../token.zig").Precedence;

const grammar = @import("../grammar.zig");
const functions = @import("functions.zig");
const expressions = @import("expressions.zig");
const array = @import("array.zig");
const object = @import("object.zig");

/// cover grammar result for parenthesized expressions and arrow parameters.
/// https://tc39.es/ecma262/#prod-CoverParenthesizedExpressionAndArrowParameterList
pub const ParenthesizedCover = struct {
    /// parsed elements (expressions + spread elements)
    elements: []const ast.NodeIndex,
    start: u32,
    end: u32,
    /// trailing comma present (valid for arrow params, not for parenthesized expr)
    has_trailing_comma: bool,
};

/// parse CoverParenthesizedExpressionAndArrowParameterList.
/// returns the cover which can be converted to either parenthesized expression or arrow params.
pub fn parseCover(parser: *Parser) Error!?ParenthesizedCover {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume (

    const checkpoint = parser.scratch_cover.begin();
    errdefer parser.scratch_cover.reset(checkpoint);

    var end = start + 1;
    var has_trailing_comma = false;

    // empty parens: ()
    if (parser.current_token.type == .right_paren) {
        end = parser.current_token.span.end;
        try parser.advance();
        return .{
            .elements = parser.scratch_cover.take(checkpoint),
            .start = start,
            .end = end,
            .has_trailing_comma = false,
        };
    }

    while (parser.current_token.type != .right_paren and parser.current_token.type != .eof) {
        // rest element: (...x)
        if (parser.current_token.type == .spread) {
            const spread_start = parser.current_token.span.start;
            try parser.advance();

            const argument = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse {
                parser.scratch_cover.reset(checkpoint);
                return null;
            };
            const spread_end = parser.getSpan(argument).end;

            // for now, store as spread_element; will convert to rest param for arrow functions
            const rest = try parser.addNode(
                .{ .spread_element = .{ .argument = argument } },
                .{ .start = spread_start, .end = spread_end },
            );

            try parser.scratch_cover.append(parser.allocator(), rest);

            end = spread_end;

            if (parser.current_token.type == .comma) {
                try parser.advance();
                has_trailing_comma = true;
            }

            continue;
        }

        // regular element
        const element = try grammar.parseExpressionInCover(parser, Precedence.Assignment) orelse {
            parser.scratch_cover.reset(checkpoint);
            return null;
        };

        try parser.scratch_cover.append(parser.allocator(), element);

        end = parser.getSpan(element).end;

        // comma or end
        if (parser.current_token.type == .comma) {
            try parser.advance();
            has_trailing_comma = parser.current_token.type == .right_paren;
        } else if (parser.current_token.type != .right_paren) {
            try parser.report(
                parser.current_token.span,
                "Expected ',' or ')' in parenthesized expression",
                .{ .help = "Add a comma between elements or close with ')'." },
            );
            parser.scratch_cover.reset(checkpoint);
            return null;
        }
    }

    if (parser.current_token.type != .right_paren) {
        try parser.report(
            .{ .start = start, .end = end },
            "Unterminated parenthesized expression",
            .{
                .help = "Add a closing ')' to complete the expression.",
                .labels = try parser.makeLabels(&.{parser.label(.{ .start = start, .end = start + 1 }, "Opened here")}),
            },
        );
        parser.scratch_cover.reset(checkpoint);
        return null;
    }

    end = parser.current_token.span.end;
    try parser.advance(); // consume )

    return .{
        .elements = parser.scratch_cover.take(checkpoint),
        .start = start,
        .end = end,
        .has_trailing_comma = has_trailing_comma,
    };
}

/// convert cover to CallExpression.
pub fn coverToCallExpression(parser: *Parser, cover: ParenthesizedCover, callee: ast.NodeIndex) Error!?ast.NodeIndex {
    // validate no CoverInitializedName in nested objects
    for (cover.elements) |elem| {
        if (!try grammar.validateNoCoverInitializedSyntax(parser, elem)) {
            return null;
        }
    }

    return try parser.addNode(
        .{ .call_expression = .{ .callee = callee, .arguments = try parser.addExtra(cover.elements), .optional = false } },
        .{ .start = parser.getSpan(callee).start, .end = cover.end },
    );
}

/// convert cover to ParenthesizedExpression.
pub fn coverToParenthesizedExpression(parser: *Parser, cover: ParenthesizedCover) Error!?ast.NodeIndex {
    // empty parens () without arrow is invalid
    if (cover.elements.len == 0) {
        try parser.report(
            .{ .start = cover.start, .end = cover.end },
            "Empty parentheses are only valid as arrow function parameters",
            .{},
        );
        return null;
    }

    if (cover.has_trailing_comma) {
        try parser.report(
            .{ .start = cover.start, .end = cover.end },
            "Trailing comma is not allowed in parenthesized expression",
            .{ .help = "Remove the trailing comma or use as arrow function parameters." },
        );
        return null;
    }

    // validate no CoverInitializedName in nested objects
    for (cover.elements) |elem| {
        if (parser.getData(elem) == .spread_element) {
            try parser.report(
                parser.getSpan(elem),
                "Rest element is not allowed in parenthesized expression",
                .{ .help = "Spread in parentheses is only valid for arrow function parameters." },
            );

            return null;
        }

        if (!try grammar.validateNoCoverInitializedSyntax(parser, elem)) {
            return null;
        }
    }

    if (cover.elements.len == 1) {
        return try parser.addNode(
            .{ .parenthesized_expression = .{ .expression = cover.elements[0] } },
            .{ .start = cover.start, .end = cover.end },
        );
    }

    const first_span = parser.getSpan(cover.elements[0]);
    const last_span = parser.getSpan(cover.elements[cover.elements.len - 1]);

    const seq_expr = try parser.addNode(
        .{ .sequence_expression = .{ .expressions = try parser.addExtra(cover.elements) } },
        .{ .start = first_span.start, .end = last_span.end },
    );

    return try parser.addNode(
        .{ .parenthesized_expression = .{ .expression = seq_expr } },
        .{ .start = cover.start, .end = cover.end },
    );
}

/// convert cover to ArrowFunctionExpression parameters and body.
pub fn coverToArrowFunction(parser: *Parser, cover: ParenthesizedCover, is_async: bool, arrow_start: u32) Error!?ast.NodeIndex {
    try parser.advance(); // consume =>

    // convert elements to formal parameters
    const params = try convertToFormalParameters(parser, cover) orelse return null;

    // arrow body (expression or block)
    const body_result = try parseArrowBody(parser, is_async) orelse return null;

    return try parser.addNode(
        .{ .arrow_function_expression = .{
            .expression = body_result.is_expression,
            .async = is_async,
            .params = params,
            .body = body_result.body,
        } },
        .{ .start = arrow_start, .end = parser.getSpan(body_result.body).end },
    );
}

/// convert a single identifier to arrow function (x => body case).
pub fn identifierToArrowFunction(parser: *Parser, id: ast.NodeIndex, is_async: bool, start: u32) Error!?ast.NodeIndex {
    try parser.advance(); // consume =>

    // convert identifier_reference to binding_identifier
    const id_data = parser.getData(id).identifier_reference;

    parser.setData(id, .{ .binding_identifier = .{
        .name_start = id_data.name_start,
        .name_len = id_data.name_len,
    } });

    const param = try parser.addNode(
        .{ .formal_parameter = .{ .pattern = id } },
        parser.getSpan(id),
    );

    // create formal_parameters with single param
    const params_range = try parser.addExtra(&[_]ast.NodeIndex{param});

    const params = try parser.addNode(
        .{ .formal_parameters = .{ .items = params_range, .rest = ast.null_node, .kind = .arrow_formal_parameters } },
        parser.getSpan(id),
    );

    // parse arrow body
    const body_result = try parseArrowBody(parser, is_async) orelse return null;

    return try parser.addNode(
        .{ .arrow_function_expression = .{
            .expression = body_result.is_expression,
            .async = is_async,
            .params = params,
            .body = body_result.body,
        } },
        .{ .start = start, .end = parser.getSpan(body_result.body).end },
    );
}

const ArrowBodyResult = struct {
    body: ast.NodeIndex,
    is_expression: bool,
};

fn parseArrowBody(parser: *Parser, is_async: bool) Error!?ArrowBodyResult {
    const saved_async = parser.context.in_async;
    const saved_generator = parser.context.in_generator;
    const saved_in_function = parser.context.in_function;

    parser.context.in_async = is_async;
    parser.context.in_generator = false;
    parser.context.in_function = true;

    defer {
        parser.context.in_generator = saved_generator;
        parser.context.in_async = saved_async;
        parser.context.in_function = saved_in_function;
    }

    if (parser.current_token.type == .left_brace) {
        // block body: () => { ... }
        const body = try functions.parseFunctionBody(parser) orelse return null;
        return .{ .body = body, .is_expression = false };
    }

    // expression body: () => expr
    // arrow body is parsed at assignment precedence
    const expr = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
    return .{ .body = expr, .is_expression = true };
}

fn convertToFormalParameters(parser: *Parser, cover: ParenthesizedCover) Error!?ast.NodeIndex {
    const checkpoint = parser.scratch_cover.begin();
    errdefer parser.scratch_cover.reset(checkpoint);

    var rest: ast.NodeIndex = ast.null_node;

    for (cover.elements) |elem| {
        if (!ast.isNull(rest)) {
            try parser.report(
                parser.getSpan(rest),
                "Rest parameter must be last formal parameter",
                .{ .help = "Move the rest parameter to the end of the parameter list" },
            );

            return null;
        }

        if (parser.getData(elem) == .spread_element) {
            const spread_data = parser.getData(elem).spread_element;

            try grammar.expressionToPattern(parser, spread_data.argument, .binding) orelse {
                parser.scratch_cover.reset(checkpoint);
                return null;
            };

            parser.setData(elem, .{ .binding_rest_element = .{ .argument = spread_data.argument } });

            rest = elem;

            continue;
        }

        const param = try convertToFormalParameter(parser, elem) orelse {
            parser.scratch_cover.reset(checkpoint);
            return null;
        };

        try parser.scratch_cover.append(parser.allocator(), param);
    }

    const items = try parser.addExtra(parser.scratch_cover.take(checkpoint));

    return try parser.addNode(
        .{ .formal_parameters = .{ .items = items, .rest = rest, .kind = .arrow_formal_parameters } },
        .{ .start = cover.start, .end = cover.end },
    );
}

fn convertToFormalParameter(parser: *Parser, expr: ast.NodeIndex) Error!?ast.NodeIndex {
    // convert expression to binding pattern
    try grammar.expressionToPattern(parser, expr, .binding) orelse return null;

    // expr is now pattern

    return try parser.addNode(
        .{ .formal_parameter = .{ .pattern = expr } },
        parser.getSpan(expr),
    );
}

pub fn unwrapParens(parser: *Parser, node: ast.NodeIndex) ast.NodeIndex {
    const data = parser.getData(node);

    if (data == .parenthesized_expression) {
        return unwrapParens(parser, data.parenthesized_expression.expression);
    }

    return node;
}

const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const Precedence = @import("../token.zig").Precedence;

const array = @import("array.zig");
const object = @import("object.zig");
const literals = @import("literals.zig");
const expressions = @import("expressions.zig");

pub inline fn parseBindingPattern(parser: *Parser) Error!?ast.NodeIndex {
    if (parser.current_token.type.isIdentifierLike()) {
        return parseBindingIdentifier(parser);
    }

    return switch (parser.current_token.type) {
        .left_bracket => parseArrayPattern(parser),
        .left_brace => parseObjectPattern(parser),
        else => {
            try parser.reportFmt(
                parser.current_token.span,
                "Unexpected token '{s}' in binding pattern",
                .{parser.describeToken(parser.current_token)},
                .{ .help = "Expected an identifier, array pattern ([a, b]), or object pattern ({a, b})." },
            );
            return null;
        },
    };
}

pub inline fn parseBindingIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    if (!try literals.validateIdentifier(parser, "an identifier", parser.current_token)) {
        return null;
    }

    const current = parser.current_token;
    try parser.advance();

    return try parser.addNode(
        .{
            .binding_identifier = .{
                .name_start = current.span.start,
                .name_len = @intCast(current.lexeme.len),
            },
        },
        current.span,
    );
}

fn parseArrayPattern(parser: *Parser) Error!?ast.NodeIndex {
    const cover = try array.parseCover(parser) orelse return null;
    return try array.coverToPattern(parser, cover, .binding);
}

fn parseObjectPattern(parser: *Parser) Error!?ast.NodeIndex {
    const cover = try object.parseCover(parser) orelse return null;
    return try object.coverToPattern(parser, cover, .binding);
}

pub fn parseAssignmentPattern(parser: *Parser, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const start = parser.getSpan(left).start;

    if (parser.current_token.type != .assign) return left;

    try parser.advance();

    // right side is AssignmentExpression, not Expression (so 2)
    const right = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

    return try parser.addNode(
        .{ .assignment_pattern = .{ .left = left, .right = right } },
        .{ .start = start, .end = parser.getSpan(right).end },
    );
}

pub fn parseBindingRestElement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume ...

    const argument = try parseBindingPattern(parser) orelse return null;
    const end = parser.getSpan(argument).end;

    return try parser.addNode(
        .{ .binding_rest_element = .{ .argument = argument } },
        .{ .start = start, .end = end },
    );
}

pub fn isDestructuringPattern(parser: *Parser, index: ast.NodeIndex) bool {
    return switch (parser.getData(index)) {
        .array_pattern, .object_pattern => true,
        .assignment_pattern => |pattern| isDestructuringPattern(parser, pattern.left),
        else => false,
    };
}

const std = @import("std");
const ast = @import("../ast.zig");
const token = @import("../token.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const Precedence = @import("../token.zig").Precedence;

const expressions = @import("expressions.zig");
const variables = @import("variables.zig");
const parenthesized = @import("parenthesized.zig");
const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const functions = @import("functions.zig");
const class = @import("class.zig");
const grammar = @import("../grammar.zig");
const modules = @import("modules.zig");

const ParseStatementOpts = struct {
    can_be_single_statement_context: bool = false,
};

pub fn parseStatement(parser: *Parser, opts: ParseStatementOpts) Error!?ast.NodeIndex {
    parser.context.in_single_statement_context = false;

    if (parser.current_token.type == .left_brace) {
        return parseBlockStatement(parser);
    }

    if (opts.can_be_single_statement_context) {
        parser.context.in_single_statement_context = true;
    }

    if (parser.current_token.type == .await) {
        const next = try parser.lookAhead();
        if (next.type == .using) {
            try parser.advance();
            return variables.parseVariableDeclaration(parser, true);
        }
    }

    if (parser.current_token.type == .import) {
        const next = try parser.lookAhead();
        if (next.type != .left_paren) {
            return modules.parseImportDeclaration(parser);
        }
    }

    if (parser.current_token.type == .async) {
        const next = try parser.lookAhead();
        if (next.type == .function and !next.has_line_terminator_before) {
            const start = parser.current_token.span.start;
            try parser.advance();
            return functions.parseFunction(parser, .{ .is_async = true }, start);
        }
    }

    const statement = switch (parser.current_token.type) {
        .@"var", .@"const", .let, .using => variables.parseVariableDeclaration(parser, false),
        .function => functions.parseFunction(parser, .{}, null),
        .class => class.parseClass(parser, .{}, null),
        .@"export" => modules.parseExportDeclaration(parser),
        .@"if" => parseIfStatement(parser),
        .@"switch" => parseSwitchStatement(parser),
        .@"for" => parseForStatement(parser, false),
        .@"while" => parseWhileStatement(parser),
        .do => parseDoWhileStatement(parser),
        .with => parseWithStatement(parser),
        .@"break" => parseBreakStatement(parser),
        .@"continue" => parseContinueStatement(parser),
        .@"return" => parseReturnStatement(parser),
        .throw => parseThrowStatement(parser),
        .@"try" => parseTryStatement(parser),
        .debugger => parseDebuggerStatement(parser),
        .semicolon => parseEmptyStatement(parser),
        else => parseExpressionStatementOrLabeledOrDirective(parser),
    };

    parser.context.in_single_statement_context = false;

    return statement;
}

fn parseExpressionStatementOrLabeledOrDirective(parser: *Parser) Error!?ast.NodeIndex {
    const expression = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    const expression_span = parser.getSpan(expression);
    const expression_data = parser.getData(expression);

    // labeled statement: identifier ':'
    if (expression_data == .identifier_reference and parser.current_token.type == .colon) {
        return parseLabeledStatement(parser, expression);
    }

    const start = expression_span.start;

    if (expression_data == .string_literal and parser.state.in_directive_prologue) {
        const value_start = expression_data.string_literal.raw_start + 1;
        const value_len: u16 = expression_data.string_literal.raw_len - 2;

        if (std.mem.eql(u8, parser.getSourceText(value_start, value_len), "use strict")) {
            parser.strict_mode = true;
            parser.lexer.strict_mode = true;
        }

        return try parser.addNode(.{
            .directive = .{
                .expression = expression,
                .value_start = value_start,
                .value_len = value_len,
            },
        }, .{ .start = start, .end = try parser.eatSemicolon(expression_span.end) orelse return null });
    }

    return try parser.addNode(
        .{ .expression_statement = .{ .expression = expression } },
        .{ .start = start, .end = try parser.eatSemicolon(expression_span.end) orelse return null },
    );
}

/// https://tc39.es/ecma262/#sec-labelled-statements
fn parseLabeledStatement(parser: *Parser, identifier: ast.NodeIndex) Error!?ast.NodeIndex {
    const id_data = parser.getData(identifier);
    const id_span = parser.getSpan(identifier);

    // IdentifierReference to LabelIdentifier
    const label = try parser.addNode(.{
        .label_identifier = .{
            .name_start = id_data.identifier_reference.name_start,
            .name_len = id_data.identifier_reference.name_len,
        },
    }, id_span);

    try parser.advance(); // consume ':'

    const body = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    return try parser.addNode(.{
        .labeled_statement = .{ .label = label, .body = body },
    }, .{ .start = id_span.start, .end = parser.getSpan(body).end });
}

/// https://tc39.es/ecma262/#prod-BlockStatement
pub fn parseBlockStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (!try parser.expect(
        .left_brace,
        "Expected '{' to start block statement",
        "Block statements must be enclosed in braces: { ... }",
    )) return null;

    const body = try parser.parseBody(.right_brace);

    const end = parser.current_token.span.end;

    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close block statement",
        "Add a closing brace '}' to complete the block statement, or check for unbalanced braces inside.",
    )) return null;

    return try parser.addNode(.{ .block_statement = .{ .body = body } }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#sec-switch-statement
pub fn parseSwitchStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume 'switch'

    if (!try parser.expect(.left_paren, "Expected '(' after 'switch'", null)) return null;

    const discriminant = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after switch expression", null)) return null;
    if (!try parser.expect(.left_brace, "Expected '{' to start switch body", null)) return null;

    const cases = try parseSwitchCases(parser);

    const end = parser.current_token.span.end;
    if (!try parser.expect(.right_brace, "Expected '}' to close switch body", null)) return null;

    return try parser.addNode(.{
        .switch_statement = .{
            .discriminant = discriminant,
            .cases = cases,
        },
    }, .{ .start = start, .end = end });
}

fn parseSwitchCases(parser: *Parser) Error!ast.IndexRange {
    const checkpoint = parser.scratch_a.begin();

    while (parser.current_token.type == .case or parser.current_token.type == .default) {
        const case_node = try parseSwitchCase(parser) orelse continue;
        try parser.scratch_a.append(parser.allocator(), case_node);
    }

    return parser.addExtra(parser.scratch_a.take(checkpoint));
}

fn parseSwitchCase(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const is_default = parser.current_token.type == .default;

    try parser.advance(); // consume 'case' or 'default'

    var test_expr: ast.NodeIndex = ast.null_node;

    if (!is_default) {
        test_expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    }

    const colon_end = parser.current_token.span.end;
    if (!try parser.expect(.colon, "Expected ':' after case", null)) return null;

    const consequent = try parseCaseConsequent(parser);
    const end = if (consequent.len > 0)
        parser.getSpan(parser.getExtra(consequent)[consequent.len - 1]).end
    else
        colon_end;

    return try parser.addNode(.{
        .switch_case = .{
            .@"test" = test_expr,
            .consequent = consequent,
        },
    }, .{ .start = start, .end = end });
}

fn parseCaseConsequent(parser: *Parser) Error!ast.IndexRange {
    const checkpoint = parser.scratch_b.begin();

    while (parser.current_token.type != .case and
        parser.current_token.type != .default and
        parser.current_token.type != .right_brace and
        parser.current_token.type != .eof)
    {
        if (try parseStatement(parser, .{})) |stmt| {
            try parser.scratch_b.append(parser.allocator(), stmt);
        } else {
            break;
        }
    }

    return parser.addExtra(parser.scratch_b.take(checkpoint));
}

/// https://tc39.es/ecma262/#sec-if-statement
pub fn parseIfStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume 'if'

    if (!try parser.expect(.left_paren, "Expected '(' after 'if'", null)) return null;

    const test_expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after if condition", null)) return null;

    const consequent = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    var end = parser.getSpan(consequent).end;
    var alternate: ast.NodeIndex = ast.null_node;

    if (parser.current_token.type == .@"else") {
        try parser.advance(); // consume 'else'
        alternate = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;
        end = parser.getSpan(alternate).end;
    }

    return try parser.addNode(.{
        .if_statement = .{
            .@"test" = test_expr,
            .consequent = consequent,
            .alternate = alternate,
        },
    }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#sec-while-statement
fn parseWhileStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume 'while'

    if (!try parser.expect(.left_paren, "Expected '(' after 'while'", null)) return null;

    const test_expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after while condition", null)) return null;

    const body = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    return try parser.addNode(.{
        .while_statement = .{
            .@"test" = test_expr,
            .body = body,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// https://tc39.es/ecma262/#sec-do-while-statement
fn parseDoWhileStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume 'do'

    const body = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    if (!try parser.expect(.@"while", "Expected 'while' after do statement body", null)) return null;
    if (!try parser.expect(.left_paren, "Expected '(' after 'while'", null)) return null;

    const test_expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    const rparen_end = parser.current_token.span.end;
    if (!try parser.expect(.right_paren, "Expected ')' after while condition", null)) return null;

    const end = try parser.eatSemicolonLenient(rparen_end);

    return try parser.addNode(.{
        .do_while_statement = .{
            .body = body,
            .@"test" = test_expr,
        },
    }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#sec-with-statement
fn parseWithStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (parser.strict_mode) {
        try parser.report(parser.current_token.span, "'with' statement is not allowed in strict mode", .{});
        return null;
    }

    try parser.advance(); // consume 'with'

    if (!try parser.expect(.left_paren, "Expected '(' after 'with'", null)) return null;

    const object = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after with expression", null)) return null;

    const body = try parseStatement(parser, .{}) orelse return null;

    return try parser.addNode(.{
        .with_statement = .{
            .object = object,
            .body = body,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// EmptyStatement: `;`
fn parseEmptyStatement(parser: *Parser) Error!?ast.NodeIndex {
    const span = parser.current_token.span;
    try parser.advance(); // consume ';'
    return try parser.addNode(.empty_statement, span);
}

/// https://tc39.es/ecma262/#sec-break-statement
fn parseBreakStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;
    try parser.advance(); // consume 'break'

    var label: ast.NodeIndex = ast.null_node;

    // break [no LineTerminator here] LabelIdentifier;
    if (!parser.canInsertSemicolon() and parser.current_token.type != .semicolon) {
        const label_node = try literals.parseLabelIdentifier(parser) orelse return null;
        label = label_node;
        end = parser.getSpan(label_node).end;
    }

    end = try parser.eatSemicolon(end) orelse return null;

    return try parser.addNode(.{ .break_statement = .{ .label = label } }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#sec-continue-statement
fn parseContinueStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;
    try parser.advance(); // consume 'continue'

    var label: ast.NodeIndex = ast.null_node;

    // continue [no LineTerminator here] LabelIdentifier;
    if (!parser.canInsertSemicolon() and parser.current_token.type != .semicolon) {
        const label_node = try literals.parseLabelIdentifier(parser) orelse return null;
        label = label_node;
        end = parser.getSpan(label_node).end;
    }

    end = try parser.eatSemicolon(end) orelse return null;

    return try parser.addNode(.{ .continue_statement = .{ .label = label } }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#sec-for-statement
/// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements
fn parseForStatement(parser: *Parser, is_await: bool) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume 'for'

    // check for `for await (...)`
    if (parser.current_token.type == .await) {
        if (!parser.context.in_async and !parser.isModule()) {
            try parser.report(parser.current_token.span, "'for await' is only valid in async functions or modules", .{});
            return null;
        }
        try parser.advance(); // consume 'await'

        // continue parsing with is_await = true

        if (!try parser.expect(.left_paren, "Expected '(' after 'for await'", null)) return null;

        return parseForHead(parser, start, true);
    }

    if (!try parser.expect(.left_paren, "Expected '(' after 'for'", null)) return null;

    // first part and determine which kind of for statement this is
    return parseForHead(parser, start, is_await);
}

/// parse the head of a for statement to determine its type
fn parseForHead(parser: *Parser, start: u32, is_await: bool) Error!?ast.NodeIndex {
    const token_type = parser.current_token.type;

    // empty init: for (;;)
    if (token_type == .semicolon) {
        return parseForStatementRest(parser, start, ast.null_node);
    }

    // variable declaration: for (var/let/const ... )
    if (token_type == .@"var" or token_type == .let or token_type == .@"const") {
        return parseForWithDeclaration(parser, start, is_await);
    }

    // expression or assignment target: for (expr in/of ...) or for (expr; ...)
    return parseForWithExpression(parser, start, is_await);
}

/// for loop starting with variable declaration
fn parseForWithDeclaration(parser: *Parser, start: u32, is_await: bool) Error!?ast.NodeIndex {
    const decl_start = parser.current_token.span.start;
    const kind = parseVariableKindForLoop(parser) orelse return null;

    // first declarator
    const first_declarator = try parseForLoopDeclarator(parser) orelse return null;
    const first_end = parser.getSpan(first_declarator).end;

    // check for for-in/for-of
    if (parser.current_token.type == .in) {
        // for (var/let/const x in ...)
        if (is_await) {
            try parser.report(parser.current_token.span, "'for await' requires 'of', not 'in'", .{});
            return null;
        }

        const decl = try createSingleDeclaration(parser, kind, first_declarator, decl_start, first_end);
        return parseForInStatementRest(parser, start, decl);
    }

    if (parser.current_token.type == .of) {
        // for (var/let/const x of ...)
        const decl = try createSingleDeclaration(parser, kind, first_declarator, decl_start, first_end);
        return parseForOfStatementRest(parser, start, decl, is_await);
    }

    // regular for statement, might have more declarators
    var end = first_end;

    const checkpoint = parser.scratch_a.begin();

    try parser.scratch_a.append(parser.allocator(), first_declarator);

    // additional declarators: for (let a = 1, b = 2; ...)
    while (parser.current_token.type == .comma) {
        try parser.advance();
        const declarator = try parseForLoopDeclarator(parser) orelse {
            parser.scratch_a.reset(checkpoint);
            return null;
        };
        try parser.scratch_a.append(parser.allocator(), declarator);
        end = parser.getSpan(declarator).end;
    }

    const declarators = parser.scratch_a.take(checkpoint);

    const declarators_range = try parser.addExtra(declarators);

    // init is required for non idenitifer id's in regular loop
    // for example, this is an error:
    // for (let { a: b = let };;) {}
    for (declarators) |decl| {
        const data = parser.getData(decl).variable_declarator;
        const id_data = parser.getData(data.id);
        if (ast.isNull(data.init) and id_data != .binding_identifier) {
            try parser.report(parser.getSpan(data.id), "Missing initializer in destructuring declaration", .{ .help = "Add an initializer (e.g. ` = undefined`) here" });
            return null;
        }
    }

    const decl = try parser.addNode(.{
        .variable_declaration = .{
            .declarators = declarators_range,
            .kind = kind,
        },
    }, .{ .start = decl_start, .end = end });

    return parseForStatementRest(parser, start, decl);
}

/// for loop starting with expression
fn parseForWithExpression(parser: *Parser, start: u32, is_await: bool) Error!?ast.NodeIndex {
    // disable 'in' as binary operator while parsing for-loop initializer
    const saved_allow_in = parser.context.allow_in;
    parser.context.allow_in = false;
    const expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse {
        parser.context.allow_in = saved_allow_in;
        return null;
    };
    parser.context.allow_in = saved_allow_in;

    // check for for-in/for-of
    if (parser.current_token.type == .in) {
        // for (expr in ...)
        if (is_await) {
            try parser.report(parser.current_token.span, "'for await' requires 'of', not 'in'", .{});
            return null;
        }

        try grammar.expressionToPattern(parser, expr, .assignable) orelse return null;

        // expr is now pattern

        return parseForInStatementRest(parser, start, expr);
    }

    if (parser.current_token.type == .of) {
        // for (expr of ...)
        try grammar.expressionToPattern(parser, expr, .assignable) orelse return null;

        // expr is now pattern

        return parseForOfStatementRest(parser, start, expr, is_await);
    }

    // regular for statement
    return parseForStatementRest(parser, start, expr);
}

/// parse rest of regular for statement after init
fn parseForStatementRest(parser: *Parser, start: u32, init: ast.NodeIndex) Error!?ast.NodeIndex {
    if (!try parser.expect(.semicolon, "Expected ';' after for-loop init", null)) return null;

    var test_expr: ast.NodeIndex = ast.null_node;

    if (parser.current_token.type != .semicolon) {
        test_expr = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    }

    if (!try parser.expect(.semicolon, "Expected ';' after for-loop condition", null)) return null;

    var update: ast.NodeIndex = ast.null_node;

    if (parser.current_token.type != .right_paren) {
        update = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
    }

    if (!try parser.expect(.right_paren, "Expected ')' after for-loop update", null)) return null;

    const body = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    return try parser.addNode(.{
        .for_statement = .{
            .init = init,
            .@"test" = test_expr,
            .update = update,
            .body = body,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// rest of for-in statement after left
fn parseForInStatementRest(parser: *Parser, start: u32, left: ast.NodeIndex) Error!?ast.NodeIndex {
    try parser.advance(); // consume 'in'

    const right = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after for-in expression", null)) return null;

    const body = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    return try parser.addNode(.{
        .for_in_statement = .{
            .left = left,
            .right = right,
            .body = body,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// rest of for-of statement after left
fn parseForOfStatementRest(parser: *Parser, start: u32, left: ast.NodeIndex, is_await: bool) Error!?ast.NodeIndex {
    try parser.advance(); // consume 'of'

    // for-of right side is AssignmentExpression, not Expression (no comma)
    const right = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after for-of expression", null)) return null;

    const body = try parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    return try parser.addNode(.{
        .for_of_statement = .{
            .left = left,
            .right = right,
            .body = body,
            .await = is_await,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// https://tc39.es/ecma262/#sec-return-statement
fn parseReturnStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;

    if (!parser.context.in_function) {
        try parser.report(
            .{ .start = start, .end = end },
            "'return' statement is only valid inside a function",
            .{ .help = "Remove the 'return' statement or wrap the code in a function." },
        );
        return null;
    }

    try parser.advance(); // consume 'return'

    var argument: ast.NodeIndex = ast.null_node;

    // return [no LineTerminator here] Expression?
    if (!parser.canInsertSemicolon() and parser.current_token.type != .semicolon) {
        argument = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;
        end = parser.getSpan(argument).end;
    }

    end = try parser.eatSemicolon(end) orelse return null;

    return try parser.addNode(.{ .return_statement = .{ .argument = argument } }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#sec-throw-statement
fn parseThrowStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume 'throw'

    // throw [no LineTerminator here] Expression
    if (parser.current_token.has_line_terminator_before) {
        try parser.report(parser.current_token.span, "Illegal newline after throw", .{
            .help = "The thrown expression must be on the same line as 'throw'",
        });
        return null;
    }

    const argument = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    const end = try parser.eatSemicolon(parser.getSpan(argument).end) orelse return null;

    return try parser.addNode(.{ .throw_statement = .{ .argument = argument } }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#sec-try-statement
fn parseTryStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume 'try'

    const block = try parseBlockStatement(parser) orelse return null;

    var handler: ast.NodeIndex = ast.null_node;
    var finalizer: ast.NodeIndex = ast.null_node;
    var end = parser.getSpan(block).end;

    if (parser.current_token.type == .@"catch") {
        handler = try parseCatchClause(parser) orelse return null;
        end = parser.getSpan(handler).end;
    }

    if (parser.current_token.type == .finally) {
        try parser.advance(); // consume 'finally'
        finalizer = try parseBlockStatement(parser) orelse return null;
        end = parser.getSpan(finalizer).end;
    }

    if (ast.isNull(handler) and ast.isNull(finalizer)) {
        try parser.report(parser.current_token.span, "Try statement requires catch or finally clause", .{});
        return null;
    }

    return try parser.addNode(.{
        .try_statement = .{
            .block = block,
            .handler = handler,
            .finalizer = finalizer,
        },
    }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#prod-Catch
fn parseCatchClause(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume 'catch'

    var param: ast.NodeIndex = ast.null_node;

    // optional catch binding: catch (param) or catch
    if (parser.current_token.type == .left_paren) {
        try parser.advance(); // consume '('
        param = try patterns.parseBindingPattern(parser) orelse return null;
        if (!try parser.expect(.right_paren, "Expected ')' after catch parameter", null)) return null;
    }

    const body = try parseBlockStatement(parser) orelse return null;

    return try parser.addNode(.{
        .catch_clause = .{
            .param = param,
            .body = body,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// https://tc39.es/ecma262/#sec-debugger-statement
fn parseDebuggerStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;
    try parser.advance(); // consume 'debugger'
    end = try parser.eatSemicolon(end) orelse return null;
    return try parser.addNode(.debugger_statement, .{ .start = start, .end = end });
}

/// variable kind for for loops
fn parseVariableKindForLoop(parser: *Parser) ?ast.VariableKind {
    const token_type = parser.current_token.type;
    parser.advance() catch return null;

    return switch (token_type) {
        .let => .let,
        .@"const" => .@"const",
        .@"var" => .@"var",
        else => null,
    };
}

/// a single variable declarator for for loops
fn parseForLoopDeclarator(parser: *Parser) Error!?ast.NodeIndex {
    const decl_start = parser.current_token.span.start;
    const id = try patterns.parseBindingPattern(parser) orelse return null;

    var init: ast.NodeIndex = ast.null_node;
    var end = parser.getSpan(id).end;

    if (parser.current_token.type == .assign) {
        try parser.advance();
        init = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        end = parser.getSpan(init).end;
    }

    return try parser.addNode(.{ .variable_declarator = .{ .id = id, .init = init } }, .{ .start = decl_start, .end = end });
}

/// create a variable declaration with a single declarator
fn createSingleDeclaration(parser: *Parser, kind: ast.VariableKind, declarator: ast.NodeIndex, decl_start: u32, decl_end: u32) Error!ast.NodeIndex {
    const checkpoint = parser.scratch_a.begin();
    try parser.scratch_a.append(parser.allocator(), declarator);

    return try parser.addNode(.{
        .variable_declaration = .{
            .declarators = try parser.addExtra(parser.scratch_a.take(checkpoint)),
            .kind = kind,
        },
    }, .{ .start = decl_start, .end = decl_end });
}

const ast = @import("../ast.zig");
const token = @import("../token.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const Precedence = @import("../token.zig").Precedence;
const expressions = @import("expressions.zig");
const patterns = @import("patterns.zig");

pub fn parseVariableDeclaration(parser: *Parser, await_using: bool) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const kind = parseVariableKind(parser, await_using) orelse return null;

    const checkpoint = parser.scratch_a.begin();

    const first_declarator = try parseVariableDeclarator(parser, kind) orelse {
        parser.scratch_a.reset(checkpoint);
        return null;
    };

    try parser.scratch_a.append(parser.allocator(), first_declarator);

    var end = parser.getSpan(first_declarator).end;

    // additional declarators: let a, b, c;
    while (parser.current_token.type == .comma) {
        try parser.advance();
        const declarator = try parseVariableDeclarator(parser, kind) orelse {
            parser.scratch_a.reset(checkpoint);
            return null;
        };
        try parser.scratch_a.append(parser.allocator(), declarator);
        end = parser.getSpan(declarator).end;
    }

    const span: ast.Span = .{ .start = start, .end = try parser.eatSemicolon(end) orelse return null };

    // lexical declarations are only allowed inside block statements
    if (parser.context.in_single_statement_context and (kind == .let or kind == .@"const")) {
        @branchHint(.unlikely);

        try parser.report(
            span,
            "Lexical declaration cannot appear in a single-statement context",
            .{ .help = "Wrap this declaration in a block statement" },
        );

        return null;
    }

    return try parser.addNode(
        .{
            .variable_declaration = .{
                .declarators = try parser.addExtra(parser.scratch_a.take(checkpoint)),
                .kind = kind,
            },
        },
        span,
    );
}

inline fn parseVariableKind(parser: *Parser, await_using: bool) ?ast.VariableKind {
    const token_type = parser.current_token.type;
    parser.advance() catch return null;

    return switch (token_type) {
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

fn parseVariableDeclarator(parser: *Parser, kind: ast.VariableKind) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const id = try patterns.parseBindingPattern(parser) orelse return null;

    var init: ast.NodeIndex = ast.null_node;
    var end = parser.getSpan(id).end;

    // initializer if present
    if (parser.current_token.type == .assign) {
        try parser.advance();
        init = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        end = parser.getSpan(init).end;
    } else if (patterns.isDestructuringPattern(parser, id)) {
        try parser.report(
            parser.getSpan(id),
            "Destructuring declaration must have an initializer",
            .{ .help = "Add '= value' to provide the object or array to destructure from." },
        );
        return null;
    } else if (kind == .@"const") {
        try parser.report(
            parser.getSpan(id),
            "'const' declarations must be initialized",
            .{ .help = "Add '= value' to initialize the constant, or use 'let' if you need to assign it later." },
        );
        return null;
    } else if (kind == .using or kind == .await_using) {
        const keyword = if (kind == .using) "using" else "await using";
        try parser.reportFmt(
            parser.getSpan(id),
            "'{s}' declarations must be initialized",
            .{keyword},
            .{ .help = "Disposable resources require an initial value that implements the dispose protocol." },
        );
        return null;
    }

    return try parser.addNode(
        .{ .variable_declarator = .{ .id = id, .init = init } },
        .{ .start = start, .end = end },
    );
}

const std = @import("std");
const token = @import("token.zig");

pub const Comment = struct {
    type: Type,
    start: u32,
    end: u32,

    pub const Type = enum {
        line,
        block,

        pub fn toString(self: Type) []const u8 {
            return switch (self) {
                .line => "Line",
                .block => "Block",
            };
        }
    };

    pub fn getValue(self: Comment, source: []const u8) []const u8 {
        return switch (self.type) {
            // Skip "//" prefix
            .line => source[self.start + 2 .. self.end],
            // Skip "/*" prefix and "*/" suffix
            .block => source[self.start + 2 .. self.end - 2],
        };
    }
};

/// index into the ast node array. `null_node` for optional nodes.
pub const NodeIndex = u32;
pub const null_node: NodeIndex = std.math.maxInt(NodeIndex);
pub const Span = token.Span;

/// range of indices in the extra array for storing node lists.
pub const IndexRange = struct {
    start: u32,
    len: u32,

    pub const empty: IndexRange = .{ .start = 0, .len = 0 };
};

/// https://tc39.es/ecma262/#sec-binary-operators
pub const BinaryOperator = enum {
    equal, // ==
    not_equal, // !=
    strict_equal, // ===
    strict_not_equal, // !==

    less_than, // <
    less_than_or_equal, // <=
    greater_than, // >
    greater_than_or_equal, // >=

    add, // +
    subtract, // -
    multiply, // *
    divide, // /
    modulo, // %
    exponent, // **

    bitwise_or, // |
    bitwise_xor, // ^
    bitwise_and, // &
    left_shift, // <<
    right_shift, // >>
    unsigned_right_shift, // >>>

    in, // in
    instanceof, // instanceof

    pub fn fromToken(tok: token.TokenType) BinaryOperator {
        return switch (tok) {
            .equal => .equal,
            .not_equal => .not_equal,
            .strict_equal => .strict_equal,
            .strict_not_equal => .strict_not_equal,
            .less_than => .less_than,
            .less_than_equal => .less_than_or_equal,
            .greater_than => .greater_than,
            .greater_than_equal => .greater_than_or_equal,
            .plus => .add,
            .minus => .subtract,
            .star => .multiply,
            .slash => .divide,
            .percent => .modulo,
            .exponent => .exponent,
            .bitwise_or => .bitwise_or,
            .bitwise_xor => .bitwise_xor,
            .bitwise_and => .bitwise_and,
            .left_shift => .left_shift,
            .right_shift => .right_shift,
            .unsigned_right_shift => .unsigned_right_shift,
            .in => .in,
            .instanceof => .instanceof,
            else => unreachable,
        };
    }

    pub fn toToken(self: BinaryOperator) token.TokenType {
        return switch (self) {
            .equal => .equal,
            .not_equal => .not_equal,
            .strict_equal => .strict_equal,
            .strict_not_equal => .strict_not_equal,
            .less_than => .less_than,
            .less_than_or_equal => .less_than_equal,
            .greater_than => .greater_than,
            .greater_than_or_equal => .greater_than_equal,
            .add => .plus,
            .subtract => .minus,
            .multiply => .star,
            .divide => .slash,
            .modulo => .percent,
            .exponent => .exponent,
            .bitwise_or => .bitwise_or,
            .bitwise_xor => .bitwise_xor,
            .bitwise_and => .bitwise_and,
            .left_shift => .left_shift,
            .right_shift => .right_shift,
            .unsigned_right_shift => .unsigned_right_shift,
            .in => .in,
            .instanceof => .instanceof,
        };
    }

    pub fn toString(self: BinaryOperator) []const u8 {
        return self.toToken().toString().?;
    }
};

pub const LogicalOperator = enum {
    @"and", // &&
    @"or", // ||
    nullish_coalescing, // ??

    pub fn fromToken(tok: token.TokenType) LogicalOperator {
        return switch (tok) {
            .logical_and => .@"and",
            .logical_or => .@"or",
            .nullish_coalescing => .nullish_coalescing,
            else => unreachable,
        };
    }

    pub fn toToken(self: LogicalOperator) token.TokenType {
        return switch (self) {
            .@"and" => .logical_and,
            .@"or" => .logical_or,
            .nullish_coalescing => .nullish_coalescing,
        };
    }

    pub fn toString(self: LogicalOperator) []const u8 {
        return self.toToken().toString().?;
    }
};

pub const UnaryOperator = enum {
    negate, // -
    positive, // +
    logical_not, // !
    bitwise_not, // ~
    typeof, // typeof
    void, // void
    delete, // delete

    pub fn fromToken(tok: token.TokenType) UnaryOperator {
        return switch (tok) {
            .minus => .negate,
            .plus => .positive,
            .logical_not => .logical_not,
            .bitwise_not => .bitwise_not,
            .typeof => .typeof,
            .void => .void,
            .delete => .delete,
            else => unreachable,
        };
    }

    pub fn toToken(self: UnaryOperator) token.TokenType {
        return switch (self) {
            .negate => .minus,
            .positive => .plus,
            .logical_not => .logical_not,
            .bitwise_not => .bitwise_not,
            .typeof => .typeof,
            .void => .void,
            .delete => .delete,
        };
    }

    pub fn toString(self: UnaryOperator) []const u8 {
        return self.toToken().toString().?;
    }
};

pub const UpdateOperator = enum {
    increment, // ++
    decrement, // --

    pub fn fromToken(tok: token.TokenType) UpdateOperator {
        return switch (tok) {
            .increment => .increment,
            .decrement => .decrement,
            else => unreachable,
        };
    }

    pub fn toToken(self: UpdateOperator) token.TokenType {
        return switch (self) {
            .increment => .increment,
            .decrement => .decrement,
        };
    }

    pub fn toString(self: UpdateOperator) []const u8 {
        return self.toToken().toString().?;
    }
};

pub const AssignmentOperator = enum {
    assign, // =
    add_assign, // +=
    subtract_assign, // -=
    multiply_assign, // *=
    divide_assign, // /=
    modulo_assign, // %=
    exponent_assign, // **=
    left_shift_assign, // <<=
    right_shift_assign, // >>=
    unsigned_right_shift_assign, // >>>=
    bitwise_or_assign, // |=
    bitwise_xor_assign, // ^=
    bitwise_and_assign, // &=
    logical_or_assign, // ||=
    logical_and_assign, // &&=
    nullish_assign, // ??=

    pub fn fromToken(tok: token.TokenType) AssignmentOperator {
        return switch (tok) {
            .assign => .assign,
            .plus_assign => .add_assign,
            .minus_assign => .subtract_assign,
            .star_assign => .multiply_assign,
            .slash_assign => .divide_assign,
            .percent_assign => .modulo_assign,
            .exponent_assign => .exponent_assign,
            .left_shift_assign => .left_shift_assign,
            .right_shift_assign => .right_shift_assign,
            .unsigned_right_shift_assign => .unsigned_right_shift_assign,
            .bitwise_or_assign => .bitwise_or_assign,
            .bitwise_xor_assign => .bitwise_xor_assign,
            .bitwise_and_assign => .bitwise_and_assign,
            .logical_or_assign => .logical_or_assign,
            .logical_and_assign => .logical_and_assign,
            .nullish_assign => .nullish_assign,
            else => unreachable,
        };
    }

    pub fn toToken(self: AssignmentOperator) token.TokenType {
        return switch (self) {
            .assign => .assign,
            .add_assign => .plus_assign,
            .subtract_assign => .minus_assign,
            .multiply_assign => .star_assign,
            .divide_assign => .slash_assign,
            .modulo_assign => .percent_assign,
            .exponent_assign => .exponent_assign,
            .left_shift_assign => .left_shift_assign,
            .right_shift_assign => .right_shift_assign,
            .unsigned_right_shift_assign => .unsigned_right_shift_assign,
            .bitwise_or_assign => .bitwise_or_assign,
            .bitwise_xor_assign => .bitwise_xor_assign,
            .bitwise_and_assign => .bitwise_and_assign,
            .logical_or_assign => .logical_or_assign,
            .logical_and_assign => .logical_and_assign,
            .nullish_assign => .nullish_assign,
        };
    }

    pub fn toString(self: AssignmentOperator) []const u8 {
        return self.toToken().toString().?;
    }
};

pub const VariableKind = enum {
    @"var",
    let,
    @"const",
    using,
    await_using,

    pub fn toString(self: VariableKind) []const u8 {
        return switch (self) {
            .await_using => "await using",
            .@"var" => "var",
            .let => "let",
            .@"const" => "const",
            .using => "using",
        };
    }
};

pub const PropertyKind = enum {
    init,
    get,
    set,

    pub fn toString(self: PropertyKind) []const u8 {
        return switch (self) {
            .init => "init",
            .get => "get",
            .set => "set",
        };
    }
};

/// https://tc39.es/ecma262/#sec-class-definitions
pub const ClassType = enum {
    class_declaration,
    class_expression,
};

/// https://tc39.es/ecma262/#prod-MethodDefinition
pub const MethodDefinitionKind = enum {
    constructor,
    method,
    get,
    set,

    pub fn toString(self: MethodDefinitionKind) []const u8 {
        return switch (self) {
            .constructor => "constructor",
            .method => "method",
            .get => "get",
            .set => "set",
        };
    }
};

/// https://tc39.es/ecma262/#sec-class-definitions
pub const Class = struct {
    type: ClassType,
    /// BindingIdentifier (optional, may be null_node for class expressions)
    id: NodeIndex,
    /// Expression (optional, may be null_node if no extends clause)
    super_class: NodeIndex,
    /// ClassBody
    body: NodeIndex,
};

/// https://tc39.es/ecma262/#prod-ClassBody
pub const ClassBody = struct {
    /// ClassElement[]
    body: IndexRange,
};

/// https://tc39.es/ecma262/#prod-MethodDefinition
pub const MethodDefinition = struct {
    /// PropertyKey (IdentifierName | PrivateIdentifier | Expression)
    key: NodeIndex,
    /// Function
    value: NodeIndex,
    kind: MethodDefinitionKind,
    computed: bool,
    static: bool,
};

/// https://tc39.es/ecma262/#prod-FieldDefinition
pub const PropertyDefinition = struct {
    /// PropertyKey (IdentifierName | PrivateIdentifier | Expression)
    key: NodeIndex,
    /// Expression (optional, may be null_node)
    value: NodeIndex,
    computed: bool,
    static: bool,
};

/// https://tc39.es/ecma262/#prod-ClassStaticBlock
pub const StaticBlock = struct {
    /// Statement[]
    body: IndexRange,
};

/// https://tc39.es/ecma262/#prod-FormalParameters
pub const FormalParameterKind = enum {
    /// https://tc39.es/ecma262/#prod-FormalParameters
    formal_parameters,
    /// https://tc39.es/ecma262/#prod-UniqueFormalParameters
    unique_formal_parameters,
    /// https://tc39.es/ecma262/#prod-ArrowFormalParameters
    arrow_formal_parameters,
    /// Part of TypeScript type signatures
    signature,
};

/// `left operator right`
/// https://tc39.es/ecma262/#sec-binary-operators
pub const BinaryExpression = struct {
    /// Expression
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
    operator: BinaryOperator,
};

/// `left operator right`
/// https://tc39.es/ecma262/#sec-binary-logical-operators
pub const LogicalExpression = struct {
    /// Expression
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
    operator: LogicalOperator,
};

/// `test ? consequent : alternate`
/// https://tc39.es/ecma262/#sec-conditional-operator
pub const ConditionalExpression = struct {
    /// Expression (ShortCircuitExpression)
    @"test": NodeIndex,
    /// Expression (AssignmentExpression)
    consequent: NodeIndex,
    /// Expression (AssignmentExpression)
    alternate: NodeIndex,
};

/// `operator argument`
/// https://tc39.es/ecma262/#sec-unary-operators
pub const UnaryExpression = struct {
    /// Expression
    argument: NodeIndex,
    operator: UnaryOperator,
};

/// `++argument` or `argument++`
/// https://tc39.es/ecma262/#sec-update-expressions
pub const UpdateExpression = struct {
    /// SimpleAssignmentTarget (IdentifierReference | MemberExpression)
    argument: NodeIndex,
    operator: UpdateOperator,
    prefix: bool,
};

/// `left operator right`
/// https://tc39.es/ecma262/#sec-assignment-operators
pub const AssignmentExpression = struct {
    /// AssignmentTarget (IdentifierReference | MemberExpression | ArrayPattern | ObjectPattern)
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
    operator: AssignmentOperator,
};

/// https://tc39.es/ecma262/#sec-variable-statement
pub const VariableDeclaration = struct {
    kind: VariableKind,
    /// VariableDeclarator[]
    declarators: IndexRange,
};

/// `id = init`
pub const VariableDeclarator = struct {
    /// BindingPattern
    id: NodeIndex,
    /// Expression (optional, may be null_node)
    init: NodeIndex,
};

/// `expression;`
pub const ExpressionStatement = struct {
    /// Expression
    expression: NodeIndex,
};

/// `if (test) consequent else alternate`
/// https://tc39.es/ecma262/#sec-if-statement
pub const IfStatement = struct {
    /// Expression (the condition)
    @"test": NodeIndex,
    /// Statement (the if-body)
    consequent: NodeIndex,
    /// Statement (optional, may be null_node for no else clause)
    alternate: NodeIndex,
};

/// `switch (discriminant) { cases }`
/// https://tc39.es/ecma262/#sec-switch-statement
pub const SwitchStatement = struct {
    /// Expression (the value to match)
    discriminant: NodeIndex,
    /// SwitchCase[]
    cases: IndexRange,
};

/// `for (init; test; update) body`
/// https://tc39.es/ecma262/#sec-for-statement
pub const ForStatement = struct {
    /// VariableDeclaration | Expression | null
    init: NodeIndex,
    /// Expression | null
    @"test": NodeIndex,
    /// Expression | null
    update: NodeIndex,
    /// Statement
    body: NodeIndex,
};

/// `for (left in right) body`
/// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements
pub const ForInStatement = struct {
    /// VariableDeclaration | AssignmentTarget (IdentifierReference | MemberExpression | ArrayPattern | ObjectPattern)
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
    /// Statement
    body: NodeIndex,
};

/// `for (left of right) body` or `for await (left of right) body`
/// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements
pub const ForOfStatement = struct {
    /// VariableDeclaration | AssignmentTarget (IdentifierReference | MemberExpression | ArrayPattern | ObjectPattern)
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
    /// Statement
    body: NodeIndex,
    /// true for `for await (...)`
    await: bool,
};

/// `break;` or `break label;`
/// https://tc39.es/ecma262/#sec-break-statement
pub const BreakStatement = struct {
    /// LabelIdentifier (optional, may be null_node)
    label: NodeIndex,
};

/// `continue;` or `continue label;`
/// https://tc39.es/ecma262/#sec-continue-statement
pub const ContinueStatement = struct {
    /// LabelIdentifier (optional, may be null_node)
    label: NodeIndex,
};

/// `label: statement`
/// https://tc39.es/ecma262/#sec-labelled-statements
pub const LabeledStatement = struct {
    /// LabelIdentifier
    label: NodeIndex,
    /// Statement
    body: NodeIndex,
};

/// `case test: consequent` or `default: consequent`
/// https://tc39.es/ecma262/#prod-CaseClause
pub const SwitchCase = struct {
    /// Expression (optional, null_node for default case)
    @"test": NodeIndex,
    /// Statement[]
    consequent: IndexRange,
};

/// `return;` or `return expression;`
/// https://tc39.es/ecma262/#sec-return-statement
pub const ReturnStatement = struct {
    /// Expression (optional, may be null_node)
    argument: NodeIndex,
};

/// `throw expression;`
/// https://tc39.es/ecma262/#sec-throw-statement
pub const ThrowStatement = struct {
    /// Expression (required)
    argument: NodeIndex,
};

/// `try { } catch { } finally { }`
/// https://tc39.es/ecma262/#sec-try-statement
pub const TryStatement = struct {
    /// BlockStatement
    block: NodeIndex,
    /// CatchClause (optional, may be null_node)
    handler: NodeIndex,
    /// BlockStatement (optional, may be null_node)
    finalizer: NodeIndex,
};

/// `catch (param) { body }`
/// https://tc39.es/ecma262/#prod-Catch
pub const CatchClause = struct {
    /// BindingPattern (optional, may be null_node for `catch { }`)
    param: NodeIndex,
    /// BlockStatement
    body: NodeIndex,
};

/// `while (test) body`
/// https://tc39.es/ecma262/#sec-while-statement
pub const WhileStatement = struct {
    /// Expression
    @"test": NodeIndex,
    /// Statement
    body: NodeIndex,
};

/// `do body while (test);`
/// https://tc39.es/ecma262/#sec-do-while-statement
pub const DoWhileStatement = struct {
    /// Statement
    body: NodeIndex,
    /// Expression
    @"test": NodeIndex,
};

/// `with (object) body`
/// https://tc39.es/ecma262/#sec-with-statement
pub const WithStatement = struct {
    /// Expression
    object: NodeIndex,
    /// Statement
    body: NodeIndex,
};

/// https://tc39.es/ecma262/#sec-literals-string-literals
pub const StringLiteral = struct {
    raw_start: u32,
    raw_len: u16,
};

/// https://tc39.es/ecma262/#sec-literals-numeric-literals
pub const NumericLiteral = struct {
    raw_start: u32,
    raw_len: u16,
};

/// https://tc39.es/ecma262/#sec-ecmascript-language-lexical-grammar-literals
pub const BigIntLiteral = struct {
    raw_start: u32,
    raw_len: u16,
};

pub const BooleanLiteral = struct {
    value: bool,
};

/// https://tc39.es/ecma262/#sec-literals-regular-expression-literals
pub const RegExpLiteral = struct {
    pattern_start: u32,
    pattern_len: u16,
    flags_start: u32,
    flags_len: u8,
};

/// https://tc39.es/ecma262/#sec-template-literals
pub const TemplateLiteral = struct {
    /// TemplateElement[]
    quasis: IndexRange,
    /// Expression[]
    expressions: IndexRange,
};

/// quasi
pub const TemplateElement = struct {
    raw_start: u32,
    raw_len: u16,
    tail: bool,
};

/// used in expressions
/// https://tc39.es/ecma262/#sec-identifiers
pub const IdentifierReference = struct {
    name_start: u32,
    name_len: u16,
};

/// `#name`
pub const PrivateIdentifier = struct {
    name_start: u32,
    name_len: u16,
};

/// used in declarations
/// https://tc39.es/ecma262/#sec-identifiers
pub const BindingIdentifier = struct {
    name_start: u32,
    name_len: u16,
};

/// property keys, meta properties
pub const IdentifierName = struct {
    name_start: u32,
    name_len: u16,
};

/// https://tc39.es/ecma262/#prod-LabelIdentifier
pub const LabelIdentifier = struct {
    name_start: u32,
    name_len: u16,
};

/// `pattern = init`
/// https://tc39.es/ecma262/#prod-AssignmentPattern
pub const AssignmentPattern = struct {
    /// BindingPattern
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
};

/// `...argument`
/// https://tc39.es/ecma262/#prod-BindingRestElement
pub const BindingRestElement = struct {
    /// BindingPattern
    argument: NodeIndex,
};

/// `[a, b, ...rest]`
/// https://tc39.es/ecma262/#prod-ArrayBindingPattern
pub const ArrayPattern = struct {
    /// (BindingPattern | null)[] - null for holes
    elements: IndexRange,
    /// BindingRestElement (optional, may be null_node)
    rest: NodeIndex,
};

/// `{a, b: c, ...rest}`
/// https://tc39.es/ecma262/#prod-ObjectBindingPattern
pub const ObjectPattern = struct {
    /// BindingProperty[]
    properties: IndexRange,
    /// BindingRestElement (optional, may be null_node)
    rest: NodeIndex,
};

/// `key: value` or `key` (shorthand)
pub const BindingProperty = struct {
    /// PropertyKey
    key: NodeIndex,
    /// BindingPattern
    value: NodeIndex,
    shorthand: bool,
    computed: bool,
};

/// `[a, b, ...c]`
pub const ArrayExpression = struct {
    /// (Expression | SpreadElement | null)[] - null for holes
    elements: IndexRange,
};

/// `{a: 1, b, ...c}`
pub const ObjectExpression = struct {
    /// (ObjectProperty | SpreadElement)[]
    properties: IndexRange,
};

/// `...argument`
pub const SpreadElement = struct {
    /// Expression
    argument: NodeIndex,
};

/// `key: value`, getter/setter, or method
pub const ObjectProperty = struct {
    /// PropertyKey
    key: NodeIndex,
    /// Expression (for init) or Function (for methods/getters/setters)
    value: NodeIndex,
    kind: PropertyKind,
    method: bool,
    shorthand: bool,
    computed: bool,
};

pub const Program = struct {
    source_type: SourceType,
    /// (Statement | Directive)[]
    body: IndexRange,
};

pub const SourceType = enum {
    script,
    module,

    pub fn toString(self: SourceType) []const u8 {
        return switch (self) {
            .script => "script",
            .module => "module",
        };
    }
};

/// `"use strict";`
pub const Directive = struct {
    /// StringLiteral
    expression: NodeIndex,
    /// value without quotes
    value_start: u32,
    value_len: u16,
};

pub const FunctionType = enum {
    function_declaration,
    function_expression,
    ts_declare_function,
    // https://github.com/typescript-eslint/typescript-eslint/pull/1289
    // TODO:
    // declare class MyClass {
    //  myMethod(): void;
    // }
    //
    // interface MyInterface {
    //  myFunction(): string;
    // }
    ts_empty_body_function_expression,
};

/// https://tc39.es/ecma262/#sec-function-definitions
pub const Function = struct {
    type: FunctionType,
    /// BindingIdentifier (optional, may be null_node for anonymous functions)
    id: NodeIndex,
    generator: bool,
    async: bool,
    /// FormalParameters
    params: NodeIndex,
    /// FunctionBody (optional, may be null_node for declarations/overloads)
    body: NodeIndex,
};

/// https://tc39.es/ecma262/#prod-FunctionBody
pub const FunctionBody = struct {
    // (Statement | Directive)[]
    body: IndexRange,
};

/// https://tc39.es/ecma262/#prod-BlockStatement
pub const BlockStatement = struct {
    // (Statement | Directive)[]
    body: IndexRange,
};

/// https://tc39.es/ecma262/#prod-FormalParameters
pub const FormalParameters = struct {
    /// FormalParameter[]
    items: IndexRange,
    /// BindingRestElement (optional, may be null_node)
    rest: NodeIndex,
    kind: FormalParameterKind,
};

/// https://tc39.es/ecma262/#prod-FormalParameter
pub const FormalParameter = struct {
    /// BindingPattern
    pattern: NodeIndex,
};

/// https://tc39.es/ecma262/#prod-ParenthesizedExpression
pub const ParenthesizedExpression = struct {
    /// Expression
    expression: NodeIndex,
};

// https://tc39.es/ecma262/#prod-ArrowFunction
pub const ArrowFunctionExpression = struct {
    /// Is the function body an arrow expression? i.e. `() => expr` instead of `() => {}`
    expression: bool,
    /// async (a, b) => {}
    async: bool,
    /// FormalParameters
    params: NodeIndex,
    /// FunctionBody
    body: NodeIndex,
    // TODO: add pure field too, `true` if the function is marked with a `/*#__NO_SIDE_EFFECTS__*/` comment
    // TODO: handle PIFE ("Possibly-Invoked Function Expression") cases, there are other needs which are needed this
};

/// `a, b, c`
/// https://tc39.es/ecma262/#prod-Expression
pub const SequenceExpression = struct {
    /// Expression[]
    expressions: IndexRange,
};

/// `obj.prop`, `obj[expr]`, `obj.#priv`
/// https://tc39.es/ecma262/#sec-property-accessors
pub const MemberExpression = struct {
    /// Expression - the object being accessed
    object: NodeIndex,
    /// Expression (computed) | IdentifierName (static) | PrivateIdentifier (private)
    property: NodeIndex,
    /// true for obj[expr], false for obj.prop
    computed: bool,
    /// true for obj?.prop (optional chaining)
    optional: bool,
};

/// `func()`, `func?.()`
/// https://tc39.es/ecma262/#sec-function-calls
pub const CallExpression = struct {
    /// Expression - the function being called
    callee: NodeIndex,
    /// (Expression | SpreadElement)[]
    arguments: IndexRange,
    /// true for func?.() (optional chaining)
    optional: bool,
};

/// `foo?.bar`, `foo?.bar.baz`, `foo?.()`
/// Wraps an optional chain expression
/// https://tc39.es/ecma262/#sec-optional-chains
pub const ChainExpression = struct {
    /// ChainElement (CallExpression | MemberExpression with optional somewhere in chain)
    expression: NodeIndex,
};

/// `` tag`hello ${name}` ``
/// https://tc39.es/ecma262/#sec-tagged-templates
pub const TaggedTemplateExpression = struct {
    /// Expression - the tag function
    tag: NodeIndex,
    /// TemplateLiteral
    quasi: NodeIndex,
};

/// `new Callee()`, `new Callee(args)`
/// https://tc39.es/ecma262/#sec-new-operator
pub const NewExpression = struct {
    /// Expression - the constructor being called
    callee: NodeIndex,
    /// (Expression | SpreadElement)[]
    arguments: IndexRange,
};

/// `await expression`
/// https://tc39.es/ecma262/#sec-await
pub const AwaitExpression = struct {
    /// Expression
    argument: NodeIndex,
};

/// `yield expression` or `yield* expression`
/// https://tc39.es/ecma262/#sec-generator-function-definitions-runtime-semantics-evaluation
pub const YieldExpression = struct {
    /// Expression (optional, may be null_node)
    argument: NodeIndex,
    /// true for `yield*`, false for `yield`
    delegate: bool,
};

/// `import.meta` or `new.target`
/// https://tc39.es/ecma262/#prod-MetaProperty
pub const MetaProperty = struct {
    /// IdentifierName ('import' or 'new')
    meta: NodeIndex,
    /// IdentifierName ('meta' or 'target')
    property: NodeIndex,
};

/// import or export kind for TypeScript
pub const ImportOrExportKind = enum {
    value,
    type,

    pub fn toString(self: ImportOrExportKind) []const u8 {
        return switch (self) {
            .value => "value",
            .type => "type",
        };
    }
};

/// import phase for source phase imports and deferred imports
/// https://github.com/estree/estree/blob/master/stage3/source-phase-imports.md
/// https://github.com/estree/estree/blob/master/stage3/defer-import-eval.md
pub const ImportPhase = enum {
    /// `import source x from "x"` or `import.source("x")`
    source,
    /// `import defer * as x from "x"` or `import.defer("x")`
    @"defer",
};

/// `import(source)` or `import(source, options)` or `import.source(source)` or `import.defer(source)`
/// https://tc39.es/ecma262/#sec-import-calls
pub const ImportExpression = struct {
    /// Expression - the module specifier
    source: NodeIndex,
    /// Expression (optional, may be null_node) - import options/attributes
    options: NodeIndex,
    /// import phase: source, defer, or null (regular import)
    phase: ?ImportPhase,
};

/// `import ... from 'source'` or `import 'source'`
/// https://tc39.es/ecma262/#sec-imports
pub const ImportDeclaration = struct {
    /// ImportDeclarationSpecifier[] - null for side-effect imports (import 'foo')
    specifiers: IndexRange,
    /// StringLiteral - the module specifier
    source: NodeIndex,
    /// ImportAttribute[] - import attributes/assertions
    attributes: IndexRange,
    /// import phase: source, defer, or null (regular import)
    phase: ?ImportPhase,
};

/// `import {imported as local} from "source"`
///          ~~~~~~~~~~~~~~~~~
/// https://tc39.es/ecma262/#prod-ImportSpecifier
pub const ImportSpecifier = struct {
    /// ModuleExportName (IdentifierName or StringLiteral) - imported symbol
    imported: NodeIndex,
    /// BindingIdentifier - local binding
    local: NodeIndex,
};

/// `import local from "source"`
///         ~~~~~
/// https://tc39.es/ecma262/#prod-ImportedDefaultBinding
pub const ImportDefaultSpecifier = struct {
    /// BindingIdentifier - local binding
    local: NodeIndex,
};

/// `import * as local from "source"`
/// https://tc39.es/ecma262/#prod-NameSpaceImport
pub const ImportNamespaceSpecifier = struct {
    /// BindingIdentifier - local binding
    local: NodeIndex,
};

/// `type: "json"` in import attributes
pub const ImportAttribute = struct {
    /// ImportAttributeKey (IdentifierName or StringLiteral)
    key: NodeIndex,
    /// StringLiteral
    value: NodeIndex,
};

/// `export { foo, bar }` or `export { foo } from 'source'` or `export var/let/const/function/class`
/// https://tc39.es/ecma262/#prod-ExportDeclaration
pub const ExportNamedDeclaration = struct {
    /// Declaration (optional, may be null_node) - for `export var x`
    declaration: NodeIndex,
    /// ExportSpecifier[]
    specifiers: IndexRange,
    /// StringLiteral (optional, may be null_node) - for re-exports
    source: NodeIndex,
    /// ImportAttribute[] - export attributes/assertions
    attributes: IndexRange,
};

/// `export default expression`
/// https://tc39.es/ecma262/#prod-ExportDeclaration
pub const ExportDefaultDeclaration = struct {
    /// Expression | FunctionDeclaration | ClassDeclaration
    declaration: NodeIndex,
};

/// `export * from 'source'` or `export * as name from 'source'`
/// https://tc39.es/ecma262/#prod-ExportDeclaration
pub const ExportAllDeclaration = struct {
    /// ModuleExportName (optional, may be null_node) - for `export * as name`
    exported: NodeIndex,
    /// StringLiteral - the module specifier
    source: NodeIndex,
    /// ImportAttribute[] - export attributes/assertions
    attributes: IndexRange,
};

/// `export { local as exported }`
/// https://tc39.es/ecma262/#prod-ExportSpecifier
pub const ExportSpecifier = struct {
    /// ModuleExportName (IdentifierName/IdentifierReference or StringLiteral) - local binding
    local: NodeIndex,
    /// ModuleExportName (IdentifierName or StringLiteral) - exported name
    exported: NodeIndex,
};

/// `export = expression`
pub const TSExportAssignment = struct {
    /// Expression
    expression: NodeIndex,
};

/// `export as namespace name`
pub const TSNamespaceExportDeclaration = struct {
    /// IdentifierName
    id: NodeIndex,
};

pub const NodeData = union(enum) {
    sequence_expression: SequenceExpression,
    parenthesized_expression: ParenthesizedExpression,
    arrow_function_expression: ArrowFunctionExpression,
    function: Function,
    function_body: FunctionBody,
    block_statement: BlockStatement,
    formal_parameters: FormalParameters,
    formal_parameter: FormalParameter,
    binary_expression: BinaryExpression,
    logical_expression: LogicalExpression,
    conditional_expression: ConditionalExpression,
    unary_expression: UnaryExpression,
    update_expression: UpdateExpression,
    assignment_expression: AssignmentExpression,
    array_expression: ArrayExpression,
    object_expression: ObjectExpression,
    spread_element: SpreadElement,
    object_property: ObjectProperty,
    member_expression: MemberExpression,
    call_expression: CallExpression,
    chain_expression: ChainExpression,
    tagged_template_expression: TaggedTemplateExpression,
    new_expression: NewExpression,
    await_expression: AwaitExpression,
    yield_expression: YieldExpression,
    meta_property: MetaProperty,
    class: Class,
    class_body: ClassBody,
    method_definition: MethodDefinition,
    property_definition: PropertyDefinition,
    static_block: StaticBlock,
    super,
    string_literal: StringLiteral,
    numeric_literal: NumericLiteral,
    bigint_literal: BigIntLiteral,
    boolean_literal: BooleanLiteral,
    null_literal,
    this_expression,
    regexp_literal: RegExpLiteral,
    template_literal: TemplateLiteral,
    template_element: TemplateElement,
    identifier_reference: IdentifierReference,
    private_identifier: PrivateIdentifier,
    binding_identifier: BindingIdentifier,
    identifier_name: IdentifierName,
    label_identifier: LabelIdentifier,
    expression_statement: ExpressionStatement,
    if_statement: IfStatement,
    switch_statement: SwitchStatement,
    switch_case: SwitchCase,
    for_statement: ForStatement,
    for_in_statement: ForInStatement,
    for_of_statement: ForOfStatement,
    while_statement: WhileStatement,
    do_while_statement: DoWhileStatement,
    break_statement: BreakStatement,
    continue_statement: ContinueStatement,
    labeled_statement: LabeledStatement,
    with_statement: WithStatement,
    return_statement: ReturnStatement,
    throw_statement: ThrowStatement,
    try_statement: TryStatement,
    catch_clause: CatchClause,
    debugger_statement,
    empty_statement,
    variable_declaration: VariableDeclaration,
    variable_declarator: VariableDeclarator,
    directive: Directive,
    assignment_pattern: AssignmentPattern,
    binding_rest_element: BindingRestElement,
    array_pattern: ArrayPattern,
    object_pattern: ObjectPattern,
    binding_property: BindingProperty,
    program: Program,
    import_expression: ImportExpression,
    import_declaration: ImportDeclaration,
    import_specifier: ImportSpecifier,
    import_default_specifier: ImportDefaultSpecifier,
    import_namespace_specifier: ImportNamespaceSpecifier,
    import_attribute: ImportAttribute,
    export_named_declaration: ExportNamedDeclaration,
    export_default_declaration: ExportDefaultDeclaration,
    export_all_declaration: ExportAllDeclaration,
    export_specifier: ExportSpecifier,
    ts_export_assignment: TSExportAssignment,
    ts_namespace_export_declaration: TSNamespaceExportDeclaration,
};

pub const Node = struct {
    data: NodeData,
    span: Span,
};

pub inline fn isNull(index: NodeIndex) bool {
    return index == null_node;
}

const Parser = @import("parser.zig").Parser;
const Error = @import("parser.zig").Error;
const ast = @import("ast.zig");

const object = @import("syntax/object.zig");
const expressions = @import("syntax/expressions.zig");
const array = @import("syntax/array.zig");

/// parse an expression within a cover grammar context without validation.
/// validation is deferred until the top-level context is known.
pub inline fn parseExpressionInCover(parser: *Parser, precedence: u8) Error!?ast.NodeIndex {
    return expressions.parseExpression(parser, precedence, .{ .in_cover = true });
}

/// validate that an expression doesn't contain CoverInitializedName.
pub fn validateNoCoverInitializedSyntax(parser: *Parser, expr: ast.NodeIndex) Error!bool {
    const data = parser.getData(expr);

    switch (data) {
        .object_expression => |obj| {
            const properties = parser.getExtra(obj.properties);
            for (properties) |prop| {
                if (ast.isNull(prop)) continue;

                const prop_data = parser.getData(prop);

                switch (prop_data) {
                    .object_property => |obj_prop| {
                        if (obj_prop.shorthand and isCoverInitializedName(parser, obj_prop.value)) {
                            try reportCoverInitializedNameError(parser, prop);
                            return false;
                        }

                        if (!try validateNoCoverInitializedSyntax(parser, obj_prop.value)) {
                            return false;
                        }
                    },
                    .spread_element => |spread| {
                        if (!try validateNoCoverInitializedSyntax(parser, spread.argument)) {
                            return false;
                        }
                    },
                    else => {},
                }
            }
        },
        .array_expression => |arr| {
            const elements = parser.getExtra(arr.elements);
            for (elements) |elem| {
                if (ast.isNull(elem)) continue;
                if (!try validateNoCoverInitializedSyntax(parser, elem)) {
                    return false;
                }
            }
        },
        .object_property => |obj_prop| {
            if (obj_prop.shorthand and isCoverInitializedName(parser, obj_prop.value)) {
                try reportCoverInitializedNameError(parser, expr);
                return false;
            }
        },
        .spread_element => |spread| {
            return validateNoCoverInitializedSyntax(parser, spread.argument);
        },
        .parenthesized_expression => |paren| {
            return validateNoCoverInitializedSyntax(parser, paren.expression);
        },
        .sequence_expression => |seq| {
            for (parser.getExtra(seq.expressions)) |e| {
                if (!try validateNoCoverInitializedSyntax(parser, e)) return false;
            }
        },
        else => {},
    }

    return true;
}

/// check if a node is a CoverInitializedName (assignment expression with = operator).
/// CoverInitializedName: { a = 1 } where the value is AssignmentExpression
pub inline fn isCoverInitializedName(parser: *Parser, node: ast.NodeIndex) bool {
    const data = parser.getData(node);
    return data == .assignment_expression and data.assignment_expression.operator == .assign;
}

pub inline fn reportCoverInitializedNameError(parser: *Parser, node: ast.NodeIndex) Error!void {
    try parser.report(
        parser.getSpan(node),
        "Shorthand property cannot have a default value in object expression",
        .{ .help = "Use '{ a: a = 1 }' syntax or this is only valid in destructuring patterns." },
    );
}

pub const PatternContext = enum {
    /// binding patterns for function parameters, variable declarations, etc.
    binding,
    /// assignable patterns for assignment expressions.
    assignable,
};

/// convert an expression node to a destructuring pattern (mutates in-place).
/// the context determines what syntax is allowed.
pub fn expressionToPattern(
    parser: *Parser,
    expr: ast.NodeIndex,
    context: PatternContext,
    // if return null, there is a error reported, so caller do 'orelse return null'
) Error!?void {
    const data = parser.getData(expr);

    switch (data) {
        .identifier_reference => |id| {
            parser.setData(expr, .{ .binding_identifier = .{
                .name_start = id.name_start,
                .name_len = id.name_len,
            } });
        },

        .assignment_expression => |assign| {
            if (assign.operator != .assign) {
                try parser.report(
                    parser.getSpan(expr),
                    "Invalid assignment operator in destructuring pattern",
                    .{ .help = "Only '=' is allowed in destructuring defaults, not compound operators like '+='." },
                );
                return null;
            }

            try expressionToPattern(parser, assign.left, context) orelse return null;

            parser.setData(expr, .{ .assignment_pattern = .{
                .left = assign.left,
                .right = assign.right,
            } });
        },

        .array_expression => |arr| {
            try array.toArrayPattern(parser, expr, arr.elements, parser.getSpan(expr), context) orelse return null;
        },

        .object_expression => |obj| {
            try object.toObjectPattern(parser, expr, obj.properties, parser.getSpan(expr), context) orelse return null;
        },

        .chain_expression => {
            try parser.report(
                parser.getSpan(expr),
                "Optional chaining is not allowed in destructuring pattern",
                .{ .help = "Optional chaining ('?.') cannot be used as an assignment target in destructuring patterns." },
            );

            return null;
        },

        .member_expression => {
            if (context != .assignable) {
                try parser.report(
                    parser.getSpan(expr),
                    "Member expression is not allowed in binding pattern",
                    .{ .help = "Function parameters and variable declarations can only bind to identifiers, not member expressions like 'obj.prop' or 'obj[key]'. Use a simple identifier instead." },
                );
                return null;
            }
        },

        .parenthesized_expression => |paren| {
            if (context != .assignable) {
                try parser.report(
                    parser.getSpan(expr),
                    "Parentheses are not allowed in this binding pattern",
                    .{ .help = "Remove the extra parentheses. Binding patterns can only be identifiers, destructuring patterns, or assignment patterns, not parenthesized expressions." },
                );
                return null;
            }

            try expressionToPattern(parser, paren.expression, context) orelse return null;

            if (!expressions.isSimpleAssignmentTarget(parser, paren.expression)) {
                try parser.report(
                    parser.getSpan(paren.expression),
                    "Parenthesized expression in destructuring pattern must be a simple assignment target",
                    .{ .help = "Only identifiers or member expressions (without optional chaining) are allowed inside parentheses in destructuring patterns." },
                );
                return null;
            }

            parser.setData(expr, parser.getData(paren.expression));
            parser.setSpan(expr, parser.getSpan(paren.expression));
        },

        .binding_identifier, .array_pattern, .object_pattern, .assignment_pattern => {},

        else => {
            try parser.report(
                parser.getSpan(expr),
                "Invalid element in destructuring pattern",
                .{ .help = "Expected an identifier, array pattern, object pattern, or assignment pattern." },
            );
            return null;
        },
    }
}

const std = @import("std");
const token = @import("token.zig");
const ast = @import("ast.zig");
const util = @import("util");
const parser = @import("parser.zig");

pub const LexicalError = error{
    UnterminatedString,
    UnterminatedRegex,
    NonTerminatedTemplateLiteral,
    UnterminatedRegexLiteral,
    InvalidRegexLineTerminator,
    InvalidRegex,
    InvalidIdentifierStart,
    InvalidIdentifierContinue,
    UnterminatedMultiLineComment,
    InvalidUnicodeEscape,
    InvalidHexEscape,
    InvalidOctalEscape,
    OctalEscapeInStrict,
    OctalLiteralInStrict,
    InvalidBinaryLiteral,
    InvalidOctalLiteralDigit,
    InvalidHexLiteral,
    InvalidExponentPart,
    NumericSeparatorMisuse,
    ConsecutiveNumericSeparators,
    MultipleDecimalPoints,
    InvalidBigIntSuffix,
    IdentifierAfterNumericLiteral,
    InvalidUtf8,
    OutOfMemory,
};

// TODO:
// [ ] some simd optimizations

pub const Lexer = struct {
    strict_mode: bool,
    source: []const u8,
    source_len: u32,
    /// token start position, retained for lexical error recovery if scan fails
    token_start: u32,
    /// current byte index being scanned in the source
    cursor: u32,

    template_depth: u32,
    /// stack of brace depths for each template nesting level.
    /// each entry tracks nested braces within that template's ${} expression.
    brace_depth_stack: [16]u32,

    has_line_terminator_before: bool,
    comments: std.ArrayList(ast.Comment),
    allocator: std.mem.Allocator,
    source_type: parser.SourceType,

    pub fn init(source: []const u8, allocator: std.mem.Allocator, source_type: parser.SourceType, strict_mode: bool) error{OutOfMemory}!Lexer {
        return .{
            .strict_mode = strict_mode,
            .source = source,
            .source_len = @intCast(source.len),
            .token_start = 0,
            .cursor = 0,
            .template_depth = 0,
            .brace_depth_stack = .{0} ** 16,
            .has_line_terminator_before = false,
            .comments = try .initCapacity(allocator, source.len / 3),
            .allocator = allocator,
            .source_type = source_type,
        };
    }

    inline fn peek(self: *const Lexer, offset: u32) u8 {
        if (offset > self.source_len or self.cursor >= self.source_len - offset) {
            return 0;
        }
        return self.source[self.cursor + offset];
    }

    pub fn nextToken(self: *Lexer) LexicalError!token.Token {
        try self.skipSkippable();

        if (self.cursor >= self.source_len) {
            return self.createToken(.eof, "", self.cursor, self.cursor);
        }

        self.token_start = self.cursor;
        const current_char = self.source[self.cursor];

        return switch (current_char) {
            '+', '*', '-', '!', '<', '>', '=', '|', '&', '^', '%', '/', '?' => self.scanPunctuation(),
            '.' => self.scanDot(),
            '0'...'9' => try self.scanNumber(),
            '"', '\'' => self.scanString(),
            '`' => self.scanTemplateLiteral(),
            '~', '(', ')', '{', '[', ']', ';', ',', ':' => self.scanSimplePunctuation(),
            '}' => self.handleRightBrace(),
            else => try self.scanIdentifierOrKeyword(),
        };
    }

    inline fn scanSimplePunctuation(self: *Lexer) token.Token {
        const start = self.cursor;
        const c = self.source[self.cursor];
        self.cursor += 1;

        const token_type: token.TokenType = switch (c) {
            '~' => .bitwise_not,
            '(' => .left_paren,
            ')' => .right_paren,
            '{' => blk: {
                // track nested braces inside template expressions
                if (self.template_depth > 0) {
                    self.brace_depth_stack[self.template_depth - 1] += 1;
                }
                break :blk .left_brace;
            },
            '[' => .left_bracket,
            ']' => .right_bracket,
            ';' => .semicolon,
            ',' => .comma,
            ':' => .colon,
            else => unreachable,
        };

        return self.createToken(token_type, self.source[start..self.cursor], start, self.cursor);
    }

    inline fn makePunctuationToken(self: *Lexer, len: u32, token_type: token.TokenType, start: u32) token.Token {
        self.cursor += len;
        return self.createToken(token_type, self.source[start..self.cursor], start, self.cursor);
    }

    fn scanPunctuation(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        const c0 = self.source[self.cursor];
        const c1 = self.peek(1);
        const c2 = self.peek(2);
        const c3 = self.peek(3);

        return switch (c0) {
            '+' => switch (c1) {
                '+' => self.makePunctuationToken(2, .increment, start),
                '=' => self.makePunctuationToken(2, .plus_assign, start),
                else => self.makePunctuationToken(1, .plus, start),
            },
            '-' => switch (c1) {
                '-' => self.makePunctuationToken(2, .decrement, start),
                '=' => self.makePunctuationToken(2, .minus_assign, start),
                else => self.makePunctuationToken(1, .minus, start),
            },
            '*' => if (c1 == '*' and c2 == '=')
                self.makePunctuationToken(3, .exponent_assign, start)
            else switch (c1) {
                '*' => self.makePunctuationToken(2, .exponent, start),
                '=' => self.makePunctuationToken(2, .star_assign, start),
                else => self.makePunctuationToken(1, .star, start),
            },
            '/' => if (c1 == '=') self.makePunctuationToken(2, .slash_assign, start) else self.makePunctuationToken(1, .slash, start),
            '%' => switch (c1) {
                '=' => self.makePunctuationToken(2, .percent_assign, start),
                else => self.makePunctuationToken(1, .percent, start),
            },
            '<' => if (c1 == '<' and c2 == '=')
                self.makePunctuationToken(3, .left_shift_assign, start)
            else switch (c1) {
                '<' => self.makePunctuationToken(2, .left_shift, start),
                '=' => self.makePunctuationToken(2, .less_than_equal, start),
                else => self.makePunctuationToken(1, .less_than, start),
            },
            '>' => if (c1 == '>' and c2 == '=')
                self.makePunctuationToken(3, .right_shift_assign, start)
            else if (c1 == '>' and c2 == '>')
                if (c3 == '=') self.makePunctuationToken(4, .unsigned_right_shift_assign, start) else self.makePunctuationToken(3, .unsigned_right_shift, start)
            else switch (c1) {
                '>' => self.makePunctuationToken(2, .right_shift, start),
                '=' => self.makePunctuationToken(2, .greater_than_equal, start),
                else => self.makePunctuationToken(1, .greater_than, start),
            },
            '=' => if (c1 == '=' and c2 == '=')
                self.makePunctuationToken(3, .strict_equal, start)
            else switch (c1) {
                '=' => self.makePunctuationToken(2, .equal, start),
                '>' => self.makePunctuationToken(2, .arrow, start),
                else => self.makePunctuationToken(1, .assign, start),
            },
            '!' => if (c1 == '=' and c2 == '=')
                self.makePunctuationToken(3, .strict_not_equal, start)
            else switch (c1) {
                '=' => self.makePunctuationToken(2, .not_equal, start),
                else => self.makePunctuationToken(1, .logical_not, start),
            },
            '&' => if (c1 == '&' and c2 == '=')
                self.makePunctuationToken(3, .logical_and_assign, start)
            else switch (c1) {
                '&' => self.makePunctuationToken(2, .logical_and, start),
                '=' => self.makePunctuationToken(2, .bitwise_and_assign, start),
                else => self.makePunctuationToken(1, .bitwise_and, start),
            },
            '|' => if (c1 == '|' and c2 == '=')
                self.makePunctuationToken(3, .logical_or_assign, start)
            else switch (c1) {
                '|' => self.makePunctuationToken(2, .logical_or, start),
                '=' => self.makePunctuationToken(2, .bitwise_or_assign, start),
                else => self.makePunctuationToken(1, .bitwise_or, start),
            },
            '^' => switch (c1) {
                '=' => self.makePunctuationToken(2, .bitwise_xor_assign, start),
                else => self.makePunctuationToken(1, .bitwise_xor, start),
            },
            '?' => if (c1 == '?' and c2 == '=')
                self.makePunctuationToken(3, .nullish_assign, start)
            else switch (c1) {
                '?' => self.makePunctuationToken(2, .nullish_coalescing, start),
                '.' => if (std.ascii.isDigit(c2))
                    self.makePunctuationToken(1, .question, start)
                else
                    self.makePunctuationToken(2, .optional_chaining, start),
                else => self.makePunctuationToken(1, .question, start),
            },
            else => unreachable,
        };
    }

    fn scanString(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        const quote = self.source[start];
        self.cursor += 1;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];

            if (c == '\\') {
                try self.consumeEscape();
                continue;
            }

            if (c == quote) {
                self.cursor += 1;
                return self.createToken(.string_literal, self.source[start..self.cursor], start, self.cursor);
            }

            if (c == '\n' or c == '\r') {
                return error.UnterminatedString;
            }

            self.cursor += 1;
        }

        return error.UnterminatedString;
    }

    fn scanTemplateLiteral(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        self.cursor += 1;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];

            if (c == '\\') {
                try self.consumeEscape();
                continue;
            }

            if (c == '`') {
                self.cursor += 1;
                const end = self.cursor;
                return self.createToken(.no_substitution_template, self.source[start..end], start, end);
            }

            if (c == '$' and self.peek(1) == '{') {
                self.cursor += 2;
                const end = self.cursor;
                // initialize brace depth for this template level before incrementing depth
                self.brace_depth_stack[self.template_depth] = 0;
                self.template_depth += 1;
                return self.createToken(.template_head, self.source[start..end], start, end);
            }

            self.cursor += 1;
        }

        return error.NonTerminatedTemplateLiteral;
    }

    fn scanTemplateMiddleOrTail(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        self.cursor += 1;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];
            if (c == '\\') {
                try self.consumeEscape();
                continue;
            }
            if (c == '`') {
                self.cursor += 1;
                const end = self.cursor;
                if (self.template_depth > 0) {
                    self.template_depth -= 1;
                }
                return self.createToken(.template_tail, self.source[start..end], start, end);
            }
            if (c == '$' and self.peek(1) == '{') {
                self.cursor += 2;
                const end = self.cursor;
                return self.createToken(.template_middle, self.source[start..end], start, end);
            }
            self.cursor += 1;
        }
        return error.NonTerminatedTemplateLiteral;
    }

    fn consumeEscape(self: *Lexer) LexicalError!void {
        self.cursor += 1; // skip backslash

        if (self.cursor >= self.source_len) {
            return error.UnterminatedString;
        }

        const c = self.source[self.cursor];

        const lt_len = util.Utf.lineTerminatorLen(self.source, self.cursor);

        if (lt_len > 0) {
            self.cursor += lt_len;
            return;
        }

        brk: switch (c) {
            '0' => {
                const c1 = self.peek(1);

                if (!util.Utf.isOctalDigit(c1)) {
                    self.cursor += 1; // null escape
                    break :brk;
                }

                if (self.strict_mode) return error.OctalEscapeInStrict;
                try self.consumeOctal();
            },
            'x' => {
                try self.consumeHex();
            },
            'u' => {
                try self.consumeUnicodeEscape();
            },
            '1'...'7' => {
                if (self.strict_mode) return error.OctalEscapeInStrict;
                try self.consumeOctal();
            },
            '8'...'9' => {
                if (self.strict_mode) return error.InvalidOctalEscape;
                self.cursor += 1;
            },
            else => {
                self.cursor += 1;
            },
        }
    }

    fn consumeOctal(self: *Lexer) LexicalError!void {
        const result = util.Utf.parseOctal(self.source, self.cursor);
        if (result.end == self.cursor) {
            return error.InvalidOctalEscape;
        }
        self.cursor = @intCast(result.end);
    }

    fn consumeHex(self: *Lexer) LexicalError!void {
        if (util.Utf.parseHex2(self.source, self.cursor + 1)) |r| {
            self.cursor = @intCast(r.end);
        } else {
            return error.InvalidHexEscape;
        }
    }

    fn consumeUnicodeEscape(self: *Lexer) LexicalError!void {
        self.cursor += 1; // skip 'u'

        if (self.cursor < self.source_len and self.source[self.cursor] == '{') {
            // \u{XXXXX}
            self.cursor += 1;
            const start = self.cursor;
            const end = std.mem.indexOfScalarPos(u8, self.source, self.cursor, '}') orelse
                return error.InvalidUnicodeEscape;

            if (util.Utf.parseHexVariable(self.source, start, end - start)) |r| {
                if (r.has_digits and r.end == end) {
                    self.cursor = @intCast(end + 1); // skip past '}'
                } else {
                    return error.InvalidUnicodeEscape;
                }
            } else {
                return error.InvalidUnicodeEscape;
            }
        } else {
            // \uXXXX format
            if (util.Utf.parseHex4(self.source, self.cursor)) |r| {
                self.cursor = @intCast(r.end);
            } else {
                return error.InvalidUnicodeEscape;
            }
        }
    }

    fn handleRightBrace(self: *Lexer) LexicalError!token.Token {
        // inside a template expression, check if this } closes a nested brace
        // or the template substitution itself
        if (self.template_depth > 0) {
            const depth_idx = self.template_depth - 1;
            if (self.brace_depth_stack[depth_idx] > 0) {
                // this } closes a nested brace (e.g., arrow function body, object literal)
                self.brace_depth_stack[depth_idx] -= 1;
            } else {
                // this } closes the template substitution ${...}
                return self.scanTemplateMiddleOrTail();
            }
        }

        const start = self.cursor;
        self.cursor += 1;
        return self.createToken(.right_brace, self.source[start..self.cursor], start, self.cursor);
    }

    pub fn reScanAsRegex(self: *Lexer, slash_token: token.Token) LexicalError!struct { span: token.Span, pattern: []const u8, flags: []const u8, lexeme: []const u8 } {
        self.cursor = slash_token.span.start;

        const start = self.cursor;
        var closing_delimeter_pos: u32 = 0;
        self.cursor += 1; // consume '/'
        var in_class = false;

        while (self.cursor < self.source_len) {
            if (util.Utf.isLineTerminator(self.source, self.cursor)) {
                return error.InvalidRegexLineTerminator;
            }

            const c = self.source[self.cursor];

            if (c == '\\') {
                self.cursor += 1; // consume '\'

                if (self.cursor >= self.source_len) {
                    return error.UnterminatedRegexLiteral;
                }

                if (util.Utf.isLineTerminator(self.source, self.cursor)) {
                    return error.InvalidRegexLineTerminator;
                }

                self.cursor += 1; // consume escaped char
                continue;
            }

            if (c == '[') {
                in_class = true;
                self.cursor += 1;
                continue;
            }
            if (c == ']' and in_class) {
                in_class = false;
                self.cursor += 1;
                continue;
            }
            if (c == '/' and !in_class) {
                self.cursor += 1;

                closing_delimeter_pos = self.cursor;

                while (self.cursor < self.source_len and
                    std.ascii.isAlphabetic(self.source[self.cursor]))
                {
                    self.cursor += 1;
                }

                const end = self.cursor;

                const pattern = self.source[start + 1 .. closing_delimeter_pos - 1];
                const flags = self.source[closing_delimeter_pos..end];

                return .{ .span = .{ .start = start, .end = end }, .lexeme = self.source[start..end], .pattern = pattern, .flags = flags };
            }

            self.cursor += 1;
        }
        return error.UnterminatedRegexLiteral;
    }

    fn scanDot(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        const c1 = self.peek(1);
        const c2 = self.peek(2);

        if (std.ascii.isDigit(c1)) {
            return self.scanNumber();
        }
        if (c1 == '.' and c2 == '.') {
            return self.makePunctuationToken(3, .spread, start);
        }
        return self.makePunctuationToken(1, .dot, start);
    }

    inline fn scanIdentifierBody(self: *Lexer) !void {
        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];
            if (std.ascii.isAscii(c)) {
                @branchHint(.likely);
                if (c == '\\') {
                    @branchHint(.cold);
                    if (self.peek(1) != 'u') {
                        return error.InvalidUnicodeEscape;
                    }
                    const c2 = self.peek(2);
                    if (c2 != '{') {
                        // \uXXXX format - decode and validate
                        if (util.Utf.parseHex4(self.source, self.cursor + 2)) |r| {
                            if (!util.UnicodeId.canContinueIdentifier(r.value)) {
                                return error.InvalidIdentifierContinue;
                            }
                        }
                    }
                    self.cursor += 1; // consume backslash to get to 'u'
                    try self.consumeUnicodeEscape();
                } else {
                    if ((c >= 'a' and c <= 'z') or
                        (c >= 'A' and c <= 'Z') or
                        (c >= '0' and c <= '9') or
                        c == '_' or c == '$')
                    {
                        self.cursor += 1;
                    } else {
                        break;
                    }
                }
            } else {
                @branchHint(.cold);
                const cp = try util.Utf.codePointAt(self.source, self.cursor);
                if (util.UnicodeId.canContinueIdentifier(cp.value)) {
                    self.cursor += cp.len;
                } else {
                    break;
                }
            }
        }
    }

    fn scanIdentifierOrKeyword(self: *Lexer) !token.Token {
        const start = self.cursor;

        const is_private = self.source[self.cursor] == '#';
        if (is_private) {
            self.cursor += 1;
        }

        const first_char = self.source[self.cursor];
        if (std.ascii.isAscii(first_char)) {
            @branchHint(.likely);
            if (first_char == '\\') {
                if (self.peek(1) != 'u') {
                    return error.InvalidUnicodeEscape;
                }
                const c2 = self.peek(2);
                if (c2 != '{') {
                    if (util.Utf.parseHex4(self.source, self.cursor + 2)) |r| {
                        if (!util.UnicodeId.canStartIdentifier(r.value)) {
                            return error.InvalidIdentifierStart;
                        }
                    }
                }
                self.cursor += 1; // consume backslash to get to 'u'
                try self.consumeUnicodeEscape();
            } else {
                if (!((first_char >= 'a' and first_char <= 'z') or
                    (first_char >= 'A' and first_char <= 'Z') or
                    first_char == '_' or first_char == '$'))
                {
                    @branchHint(.cold);
                    return error.InvalidIdentifierStart;
                }
                self.cursor += 1;
            }
            try self.scanIdentifierBody();
        } else {
            @branchHint(.cold);
            const c_cp = try util.Utf.codePointAt(self.source, self.cursor);
            if (!util.UnicodeId.canStartIdentifier(c_cp.value)) {
                return error.InvalidIdentifierStart;
            }
            self.cursor += c_cp.len;
            try self.scanIdentifierBody();
        }

        const lexeme = self.source[start..self.cursor];
        const token_type: token.TokenType = if (is_private) .private_identifier else self.getKeywordType(lexeme);
        return self.createToken(token_type, lexeme, start, self.cursor);
    }

    fn getKeywordType(_: *Lexer, lexeme: []const u8) token.TokenType {
        switch (lexeme.len) {
            2 => {
                switch (lexeme[1]) {
                    'f' => {
                        return switch (lexeme[0]) {
                            'i' => .@"if",
                            'o' => .of,
                            else => .identifier,
                        };
                    },
                    'n' => if (lexeme[0] == 'i') return .in,
                    'o' => if (lexeme[0] == 'd') return .do,
                    's' => if (lexeme[0] == 'a') return .as,
                    else => {},
                }
            },
            3 => {
                switch (lexeme[0]) {
                    'f' => if (lexeme[1] == 'o' and lexeme[2] == 'r') return .@"for",
                    'l' => if (lexeme[1] == 'e' and lexeme[2] == 't') return .let,
                    'n' => if (lexeme[1] == 'e' and lexeme[2] == 'w') return .new,
                    't' => if (lexeme[1] == 'r' and lexeme[2] == 'y') return .@"try",
                    'v' => if (lexeme[1] == 'a' and lexeme[2] == 'r') return .@"var",
                    else => {},
                }
            },
            4 => {
                switch (lexeme[1]) {
                    'a' => if (lexeme[0] == 'c' and lexeme[2] == 's' and lexeme[3] == 'e') return .case,
                    'h' => if (lexeme[0] == 't' and lexeme[2] == 'i' and lexeme[3] == 's') return .this,
                    'l' => if (lexeme[0] == 'e' and lexeme[2] == 's' and lexeme[3] == 'e') return .@"else",
                    'n' => if (lexeme[0] == 'e' and lexeme[2] == 'u' and lexeme[3] == 'm') return .@"enum",
                    'o' => if (lexeme[0] == 'v' and lexeme[2] == 'i' and lexeme[3] == 'd') return .void,
                    'i' => if (lexeme[0] == 'w' and lexeme[2] == 't' and lexeme[3] == 'h') return .with,
                    'u' => if (lexeme[0] == 'n' and lexeme[2] == 'l' and lexeme[3] == 'l') return .null_literal,
                    'r' => {
                        if (lexeme[0] == 't' and lexeme[2] == 'u' and lexeme[3] == 'e') return .true;
                        if (lexeme[0] == 'f' and lexeme[2] == 'o' and lexeme[3] == 'm') return .from;
                    },
                    else => {},
                }
            },
            5 => {
                switch (lexeme[0]) {
                    'a' => {
                        if (lexeme[1] == 'w' and lexeme[2] == 'a' and lexeme[3] == 'i' and lexeme[4] == 't')
                            return .await;
                        if (lexeme[1] == 's' and lexeme[2] == 'y' and lexeme[3] == 'n' and lexeme[4] == 'c')
                            return .async;
                    },
                    'b' => if (lexeme[1] == 'r' and lexeme[2] == 'e' and lexeme[3] == 'a' and lexeme[4] == 'k')
                        return .@"break",
                    'c' => {
                        if (lexeme[1] == 'o' and lexeme[2] == 'n' and lexeme[3] == 's' and lexeme[4] == 't')
                            return .@"const";
                        if (lexeme[1] == 'l' and lexeme[2] == 'a' and lexeme[3] == 's' and lexeme[4] == 's')
                            return .class;
                        if (lexeme[1] == 'a' and lexeme[2] == 't' and lexeme[3] == 'c' and lexeme[4] == 'h')
                            return .@"catch";
                    },
                    'd' => if (lexeme[1] == 'e' and lexeme[2] == 'f' and lexeme[3] == 'e' and lexeme[4] == 'r')
                        return .@"defer",
                    'f' => if (lexeme[1] == 'a' and lexeme[2] == 'l' and lexeme[3] == 's' and lexeme[4] == 'e')
                        return .false,
                    's' => if (lexeme[1] == 'u' and lexeme[2] == 'p' and lexeme[3] == 'e' and lexeme[4] == 'r')
                        return .super,
                    't' => if (lexeme[1] == 'h' and lexeme[2] == 'r' and lexeme[3] == 'o' and lexeme[4] == 'w')
                        return .throw,
                    'u' => if (lexeme[1] == 's' and lexeme[2] == 'i' and lexeme[3] == 'n' and lexeme[4] == 'g')
                        return .using,
                    'w' => if (lexeme[1] == 'h' and lexeme[2] == 'i' and lexeme[3] == 'l' and lexeme[4] == 'e')
                        return .@"while",
                    'y' => if (lexeme[1] == 'i' and lexeme[2] == 'e' and lexeme[3] == 'l' and lexeme[4] == 'd')
                        return .yield,
                    else => {},
                }
            },
            6 => {
                switch (lexeme[0]) {
                    'a' => if (lexeme[1] == 's' and lexeme[2] == 's' and lexeme[3] == 'e' and lexeme[4] == 'r' and lexeme[5] == 't')
                        return .assert,
                    'd' => if (lexeme[1] == 'e' and lexeme[2] == 'l' and lexeme[3] == 'e' and lexeme[4] == 't' and lexeme[5] == 'e')
                        return .delete,
                    'e' => if (lexeme[1] == 'x' and lexeme[2] == 'p' and lexeme[3] == 'o' and lexeme[4] == 'r' and lexeme[5] == 't')
                        return .@"export",
                    'i' => if (lexeme[1] == 'm' and lexeme[2] == 'p' and lexeme[3] == 'o' and lexeme[4] == 'r' and lexeme[5] == 't')
                        return .import,
                    'p' => if (lexeme[1] == 'u' and lexeme[2] == 'b' and lexeme[3] == 'l' and lexeme[4] == 'i' and lexeme[5] == 'c')
                        return .public,
                    'r' => if (lexeme[1] == 'e' and lexeme[2] == 't' and lexeme[3] == 'u' and lexeme[4] == 'r' and lexeme[5] == 'n')
                        return .@"return",
                    's' => {
                        if (lexeme[1] == 'w' and lexeme[2] == 'i' and lexeme[3] == 't' and lexeme[4] == 'c' and lexeme[5] == 'h')
                            return .@"switch";
                        if (lexeme[1] == 't' and lexeme[2] == 'a' and lexeme[3] == 't' and lexeme[4] == 'i' and lexeme[5] == 'c')
                            return .static;
                        if (lexeme[1] == 'o' and lexeme[2] == 'u' and lexeme[3] == 'r' and lexeme[4] == 'c' and lexeme[5] == 'e')
                            return .source;
                    },
                    't' => if (lexeme[1] == 'y' and lexeme[2] == 'p' and lexeme[3] == 'e' and lexeme[4] == 'o' and lexeme[5] == 'f')
                        return .typeof,
                    else => {},
                }
            },
            7 => {
                switch (lexeme[0]) {
                    'd' => {
                        if (std.mem.eql(u8, lexeme, "default")) return .default;
                        if (std.mem.eql(u8, lexeme, "declare")) return .declare;
                    },
                    'e' => if (std.mem.eql(u8, lexeme, "extends")) return .extends,
                    'f' => if (std.mem.eql(u8, lexeme, "finally")) return .finally,
                    'p' => {
                        if (std.mem.eql(u8, lexeme, "private")) return .private;
                        if (std.mem.eql(u8, lexeme, "package")) return .package;
                    },
                    else => {},
                }
            },
            8 => {
                switch (lexeme[0]) {
                    'c' => if (std.mem.eql(u8, lexeme, "continue")) return .@"continue",
                    'd' => if (std.mem.eql(u8, lexeme, "debugger")) return .debugger,
                    'f' => if (std.mem.eql(u8, lexeme, "function")) return .function,
                    else => {},
                }
            },
            9 => {
                switch (lexeme[0]) {
                    'i' => if (std.mem.eql(u8, lexeme, "interface")) return .interface,
                    'n' => if (std.mem.eql(u8, lexeme, "namespace")) return .namespace,
                    'p' => if (std.mem.eql(u8, lexeme, "protected")) return .protected,
                    else => {},
                }
            },
            10 => {
                switch (lexeme[0]) {
                    'i' => {
                        if (std.mem.eql(u8, lexeme, "instanceof")) return .instanceof;
                        if (std.mem.eql(u8, lexeme, "implements")) return .implements;
                    },
                    else => {},
                }
            },
            else => {},
        }
        return .identifier;
    }

    fn scanNumber(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        var token_type: token.TokenType = .numeric_literal;
        var is_legacy_octal = false;

        // handle prefixes: 0x, 0o, 0b
        if (self.source[self.cursor] == '0') {
            const prefix = std.ascii.toLower(self.peek(1));

            switch (prefix) {
                'x' => {
                    token_type = .hex_literal;
                    self.cursor += 2;
                    const hex_start = self.cursor;
                    try self.consumeHexDigits();
                    if (self.cursor == hex_start) return error.InvalidHexLiteral;
                },
                'o' => {
                    token_type = .octal_literal;
                    self.cursor += 2;
                    const oct_start = self.cursor;
                    try self.consumeOctalDigits();
                    if (self.cursor == oct_start) return error.InvalidOctalLiteralDigit;
                },
                'b' => {
                    token_type = .binary_literal;
                    self.cursor += 2;
                    const bin_start = self.cursor;
                    try self.consumeBinaryDigits();
                    if (self.cursor == bin_start) return error.InvalidBinaryLiteral;
                },
                '0'...'7' => {
                    // potential legacy octal: 01, 07, etc.
                    is_legacy_octal = true;
                    try self.consumeDecimalDigits();

                    for (self.source[start..self.cursor]) |c| {
                        if (c == '8' or c == '9') {
                            is_legacy_octal = false;
                            break;
                        }
                    }

                    if (is_legacy_octal and self.strict_mode) {
                        return error.OctalLiteralInStrict;
                    }
                },
                else => {
                    try self.consumeDecimalDigits();
                },
            }
        } else {
            try self.consumeDecimalDigits();
        }

        // handle decimal point only for regular numbers, not legacy octals
        if (token_type == .numeric_literal and
            self.cursor < self.source_len and self.source[self.cursor] == '.')
        {
            const next = self.peek(1);
            if (next == '_') return error.NumericSeparatorMisuse;

            if (is_legacy_octal and !std.ascii.isDigit(next)) {
                // don't consume the '.', it's member access (e.g., 01.toString())
            } else {
                self.cursor += 1;
                if (std.ascii.isDigit(next)) try self.consumeDecimalDigits();
            }
        }

        // handle exponent (only for regular numbers)
        if (token_type == .numeric_literal and self.cursor < self.source_len) {
            const exp_char = std.ascii.toLower(self.source[self.cursor]);
            if (exp_char == 'e') {
                try self.consumeExponent();
            }
        }

        // handle bigint suffix 'n'
        if (self.cursor < self.source_len and self.source[self.cursor] == 'n') {
            // bigint cannot have decimal point or exponent
            if (token_type == .numeric_literal) {
                const lexeme = self.source[start..self.cursor];
                for (lexeme) |c| {
                    if (c == '.' or std.ascii.toLower(c) == 'e') {
                        return error.InvalidBigIntSuffix;
                    }
                }
            }

            self.cursor += 1;
            token_type = .bigint_literal;
        }

        // identifier cannot immediately follow a numeric literal
        if (self.cursor < self.source_len) {
            const c = self.source[self.cursor];
            if (std.ascii.isAlphabetic(c) or c == '_' or c == '$' or c == '\\') return error.IdentifierAfterNumericLiteral;
        }

        return self.createToken(token_type, self.source[start..self.cursor], start, self.cursor);
    }

    inline fn consumeDigits(self: *Lexer, comptime isValidDigit: fn (u8) bool) LexicalError!void {
        var last_was_separator = false;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];
            if (isValidDigit(c)) {
                self.cursor += 1;
                last_was_separator = false;
            } else if (c == '_') {
                if (last_was_separator) {
                    return error.ConsecutiveNumericSeparators;
                }
                self.cursor += 1;
                last_was_separator = true;
            } else {
                break;
            }
        }

        if (last_was_separator) {
            return error.NumericSeparatorMisuse;
        }
    }

    inline fn consumeDecimalDigits(self: *Lexer) LexicalError!void {
        return self.consumeDigits(std.ascii.isDigit);
    }

    inline fn consumeHexDigits(self: *Lexer) LexicalError!void {
        return self.consumeDigits(std.ascii.isHex);
    }

    inline fn consumeOctalDigits(self: *Lexer) LexicalError!void {
        return self.consumeDigits(util.Utf.isOctalDigit);
    }

    inline fn consumeBinaryDigits(self: *Lexer) LexicalError!void {
        const isBinary = struct {
            fn check(c: u8) bool {
                return c == '0' or c == '1';
            }
        }.check;
        return self.consumeDigits(isBinary);
    }

    fn consumeExponent(self: *Lexer) LexicalError!void {
        self.cursor += 1; // skip 'e' or 'E'

        if (self.cursor >= self.source_len) {
            return error.InvalidExponentPart;
        }

        // handle optional sign: + or -
        const c = self.source[self.cursor];
        if (c == '+' or c == '-') {
            self.cursor += 1;
        }

        const exp_start = self.cursor;
        try self.consumeDecimalDigits();

        if (self.cursor == exp_start) {
            return error.InvalidExponentPart;
        }
    }

    inline fn skipSkippable(self: *Lexer) LexicalError!void {
        // track if we're at a logical line start (start of file OR after a line terminator)
        // this persists through whitespace/comment skipping for HTML close comment detection.
        var at_line_start = self.cursor == 0 or self.has_line_terminator_before;

        while (self.cursor < self.source_len) {
            // consume any ECMAScript LineTerminatorSequence (LF, CR, CRLF, LS, PS)
            const lt_len = util.Utf.lineTerminatorLen(self.source, self.cursor);

            if (lt_len > 0) {
                self.has_line_terminator_before = true;
                at_line_start = true;
                self.cursor += lt_len;
                continue;
            }

            const c = self.source[self.cursor];

            if (std.ascii.isAscii(c)) {
                @branchHint(.likely);

                switch (c) {
                    ' ', '\t', '\u{000B}', '\u{000C}' => {
                        self.cursor += 1;
                        continue;
                    },

                    '/' => {
                        const next = self.peek(1);
                        if (next == '/') {
                            try self.scanLineComment();
                            // scanLineComment stops before the line terminator; next loop iteration consumes it
                            continue;
                        } else if (next == '*') {
                            try self.scanBlockComment();
                            // scanBlockComment updates has_line_terminator_before if it saw any
                            if (self.has_line_terminator_before) at_line_start = true;
                            continue;
                        }
                        break;
                    },

                    '<' => {
                        // HTML-style comments (<!-- ... -->) are only valid in script mode
                        if (self.source_type == .script) {
                            const c1 = self.peek(1);
                            const c2 = self.peek(2);
                            const c3 = self.peek(3);
                            if (c1 == '!' and c2 == '-' and c3 == '-') {
                                try self.scanHtmlComment();
                                // scanHtmlComment stops before the line terminator (or consumes -->)
                                continue;
                            }
                        }
                        break;
                    },

                    '-' => {
                        // HTML-style close comment --> is only valid at line start in script mode
                        // "line start" means start of file or after a line terminator,
                        // with only whitespace/comments before it.
                        if (self.source_type == .script and at_line_start) {
                            const c1 = self.peek(1);
                            const c2 = self.peek(2);
                            if (c1 == '-' and c2 == '>') {
                                try self.scanHtmlCloseComment();
                                // stops before the line terminator; next loop consumes it
                                continue;
                            }
                        }
                        break;
                    },

                    else => break,
                }
            } else {
                @branchHint(.unlikely);

                const cp = try util.Utf.codePointAt(self.source, self.cursor);
                if (util.Utf.isMultiByteSpace(cp.value)) {
                    self.cursor += cp.len;
                    continue;
                }
                break;
            }
        }
    }

    /// scans a single-line comment (// ...)
    fn scanLineComment(self: *Lexer) LexicalError!void {
        const start = self.cursor;
        self.cursor += 2; // skip '//'

        while (self.cursor < self.source_len) {
            if (util.Utf.isLineTerminator(self.source, self.cursor)) break;
            self.cursor += 1;
        }

        self.comments.append(self.allocator, .{
            .type = .line,
            .start = start,
            .end = self.cursor,
        }) catch return error.OutOfMemory;
    }

    /// scans a multi-line comment (/* ... */)
    fn scanBlockComment(self: *Lexer) LexicalError!void {
        const start = self.cursor;
        self.cursor += 2; // skip '/*'

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];

            const lt_len = util.Utf.lineTerminatorLen(self.source, self.cursor);
            if (lt_len > 0) {
                self.has_line_terminator_before = true;
                self.cursor += lt_len;
                continue;
            }

            if (c == '*' and self.peek(1) == '/') {
                self.cursor += 2; // skip '*/'
                self.comments.append(self.allocator, .{
                    .type = .block,
                    .start = start,
                    .end = self.cursor,
                }) catch return error.OutOfMemory;
                return;
            }

            self.cursor += 1;
        }

        return error.UnterminatedMultiLineComment;
    }

    /// scans an HTML-style comment (<!-- ... --> or <!-- ... end of line)
    fn scanHtmlComment(self: *Lexer) LexicalError!void {
        const start = self.cursor;
        self.cursor += 4; // skip '<!--'

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];

            // check for early termination with -->
            if (c == '-' and self.peek(1) == '-' and self.peek(2) == '>') {
                self.cursor += 3; // skip '-->'
                self.comments.append(self.allocator, .{
                    .type = .line,
                    .start = start,
                    .end = self.cursor,
                }) catch return error.OutOfMemory;
                return;
            }

            if (util.Utf.isLineTerminator(self.source, self.cursor)) break;

            self.cursor += 1;
        }

        // comment ends at end of line
        self.comments.append(self.allocator, .{
            .type = .line,
            .start = start,
            .end = self.cursor,
        }) catch return error.OutOfMemory;
    }

    /// scans an HTML-style close comment (--> ... end of line)
    fn scanHtmlCloseComment(self: *Lexer) LexicalError!void {
        const start = self.cursor;
        self.cursor += 3; // skip '-->'

        while (self.cursor < self.source_len) {
            if (util.Utf.isLineTerminator(self.source, self.cursor)) break;
            self.cursor += 1;
        }

        self.comments.append(self.allocator, .{
            .type = .line,
            .start = start,
            .end = self.cursor,
        }) catch return error.OutOfMemory;
    }

    pub inline fn createToken(self: *Lexer, token_type: token.TokenType, lexeme: []const u8, start: u32, end: u32) token.Token {
        const tok = token.Token{
            .type = token_type,
            .lexeme = lexeme,
            .span = .{ .start = start, .end = end },
            .has_line_terminator_before = self.has_line_terminator_before,
        };

        self.has_line_terminator_before = false;
        return tok;
    }
};

pub fn getLexicalErrorMessage(error_type: LexicalError) []const u8 {
    return switch (error_type) {
        error.InvalidHexEscape => "Invalid hexadecimal escape sequence",
        error.UnterminatedString => "Unterminated string literal",
        error.UnterminatedRegex => "Unterminated regular expression",
        error.NonTerminatedTemplateLiteral => "Unterminated template literal",
        error.UnterminatedRegexLiteral => "Unterminated regular expression literal",
        error.InvalidRegexLineTerminator => "Line terminator not allowed in regular expression literal",
        error.InvalidRegex => "Invalid regular expression",
        error.InvalidIdentifierStart => "Invalid character at start of identifier",
        error.InvalidIdentifierContinue => "Invalid character in identifier",
        error.UnterminatedMultiLineComment => "Unterminated multi-line comment",
        error.InvalidUnicodeEscape => "Invalid Unicode escape sequence",
        error.InvalidOctalEscape => "Invalid octal escape sequence",
        error.OctalEscapeInStrict => "Octal escape sequences are not allowed in strict mode",
        error.OctalLiteralInStrict => "Octal literals are not allowed in strict mode",
        error.InvalidBinaryLiteral => "Binary literal must contain at least one binary digit",
        error.InvalidOctalLiteralDigit => "Octal literal must contain at least one octal digit",
        error.InvalidHexLiteral => "Hexadecimal literal must contain at least one hex digit",
        error.InvalidExponentPart => "Exponent part is missing a number",
        error.NumericSeparatorMisuse => "Numeric separator cannot appear at the end of a numeric literal",
        error.ConsecutiveNumericSeparators => "Numeric literal cannot contain consecutive separators",
        error.MultipleDecimalPoints => "Numeric literal cannot contain multiple decimal points",
        error.InvalidBigIntSuffix => "BigInt literal cannot contain decimal point or exponent",
        error.IdentifierAfterNumericLiteral => "Identifier cannot immediately follow a numeric literal",
        error.InvalidUtf8 => "Invalid UTF-8 byte sequence",
        error.OutOfMemory => "Out of memory",
    };
}

pub fn getLexicalErrorHelp(error_type: LexicalError) []const u8 {
    return switch (error_type) {
        error.InvalidHexEscape => "Try adding two hexadecimal digits here (e.g., \\x41 for 'A')",
        error.UnterminatedString => "Try adding a closing quote here to complete the string",
        error.UnterminatedRegex => "Try adding a closing slash (/) here to complete the regex",
        error.NonTerminatedTemplateLiteral => "Try adding a closing backtick (`) here to complete the template",
        error.UnterminatedRegexLiteral => "Try adding a closing slash (/) here, optionally followed by flags (g, i, m, etc.)",
        error.InvalidRegexLineTerminator => "Try removing the line break here or escaping it within the regex pattern",
        error.InvalidRegex => "Try checking the regex syntax here for unclosed groups, invalid escapes, or malformed patterns",
        error.InvalidIdentifierStart => "Try starting the identifier here with a letter (a-z, A-Z), underscore (_), or dollar sign ($)",
        error.InvalidIdentifierContinue => "Try using a valid identifier character here (letters, digits, underscore, or dollar sign)",
        error.UnterminatedMultiLineComment => "Try adding the closing delimiter (*/) here to complete the comment",
        error.InvalidUnicodeEscape => "Try using \\uHHHH (4 hex digits) or \\u{HHHHHH} (1-6 hex digits) here",
        error.InvalidOctalEscape => "Try using a valid octal sequence here (\\0-7, \\00-77, or \\000-377)",
        error.OctalEscapeInStrict => "Try replacing this octal escape with \\xHH (hex) or \\uHHHH (unicode) instead",
        error.OctalLiteralInStrict => "Try replacing this octal literal with a decimal number, or use 0xHH (hex) or 0bBB (binary) instead",
        error.InvalidBinaryLiteral => "Try adding at least one binary digit (0 or 1) here after '0b'",
        error.InvalidOctalLiteralDigit => "Try adding at least one octal digit (0-7) here after '0o'",
        error.InvalidHexLiteral => "Try adding at least one hex digit (0-9, a-f, A-F) here after '0x'",
        error.InvalidExponentPart => "Try adding digits here after the exponent (e.g., e10, e-5, E+2)",
        error.NumericSeparatorMisuse => "Try removing the trailing underscore here or adding more digits after it",
        error.ConsecutiveNumericSeparators => "Try removing one of the consecutive underscores here",
        error.MultipleDecimalPoints => "Try removing the extra decimal point here",
        error.InvalidBigIntSuffix => "Try removing the 'n' suffix here, or remove the decimal point/exponent from the number",
        error.IdentifierAfterNumericLiteral => "Try adding whitespace here between the number and identifier",
        error.InvalidUtf8 => "The source file contains invalid UTF-8 encoding. Ensure the file is saved with valid UTF-8 encoding",
        error.OutOfMemory => "The system ran out of memory while parsing",
    };
}

const std = @import("std");
const token = @import("token.zig");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");

const statements = @import("syntax/statements.zig");

pub const Severity = enum {
    @"error",
    warning,
    hint,
    info,

    pub fn toString(self: Severity) []const u8 {
        return switch (self) {
            .@"error" => "error",
            .warning => "warning",
            .hint => "hint",
            .info => "info",
        };
    }
};

pub const Label = struct {
    span: ast.Span,
    message: []const u8,
};

pub const Diagnostic = struct {
    severity: Severity = .@"error",
    message: []const u8,
    span: ast.Span,
    help: ?[]const u8 = null,
    labels: []const Label = &.{},
};

pub const SourceType = enum { script, module };
pub const Lang = enum { js, ts, jsx, tsx, dts };

pub const Options = struct {
    source_type: SourceType = .module,
    lang: Lang = .js,
};

/// Must be deinitialized to free the arena-allocated memory.
pub const ParseTree = struct {
    /// Root node of the AST (always a Program node)
    program: ast.NodeIndex,
    /// Source code that was parsed
    source: []const u8,
    /// All nodes in the AST
    nodes: std.MultiArrayList(ast.Node),
    /// Extra data storage for variadic node children
    extra: std.ArrayList(ast.NodeIndex),
    /// Diagnostics (errors, warnings, etc.) encountered during parsing
    diagnostics: std.ArrayList(Diagnostic),
    /// Comments collected in source code
    comments: std.ArrayList(ast.Comment),
    /// Arena allocator owning all the memory
    arena: std.heap.ArenaAllocator,
    /// Source type (script or module)
    source_type: SourceType,
    /// Language variant (js, ts, jsx, tsx, dts)
    lang: Lang,

    /// Returns true if the parse tree contains any errors.
    pub inline fn hasErrors(self: ParseTree) bool {
        for (self.diagnostics.items) |d| {
            if (d.severity == .@"error") return true;
        }
        return false;
    }

    /// Returns true if the parse tree contains any diagnostics (errors, warnings, etc.).
    pub inline fn hasDiagnostics(self: ParseTree) bool {
        return self.diagnostics.items.len > 0;
    }

    /// Frees all memory allocated by this parse tree.
    pub fn deinit(self: *const ParseTree) void {
        self.arena.deinit();
    }

    /// Gets the data for the node at the given index.
    pub inline fn getData(self: *const ParseTree, index: ast.NodeIndex) ast.NodeData {
        return self.nodes.items(.data)[index];
    }

    /// Gets the span for the node at the given index.
    pub inline fn getSpan(self: *const ParseTree, index: ast.NodeIndex) ast.Span {
        return self.nodes.items(.span)[index];
    }

    /// Gets the extra node indices for the given range.
    pub inline fn getExtra(self: *const ParseTree, range: ast.IndexRange) []const ast.NodeIndex {
        return self.extra.items[range.start..][0..range.len];
    }

    /// Gets a slice of the source text at the given position.
    pub inline fn getSourceText(self: *const ParseTree, start: u32, len: u16) []const u8 {
        return self.source[start..][0..len];
    }
};

const ParserContext = struct {
    in_async: bool = false,
    in_generator: bool = false,
    allow_in: bool = true,
    in_function: bool = false,
    /// Whether we're parsing a single statement.
    /// example:
    /// if(test) 30;
    ///          ~~
    ///           ^ this is in a single statement context
    in_single_statement_context: bool = false,
};

const ParserState = struct {
    /// tracks if the cover (array or object) we are parsing has a trailing comma
    /// value is the start index of the cover
    cover_has_trailing_comma: ?u32 = null,
    /// tracks if CoverInitializedName ({a = 1}) was parsed in current cover context.
    cover_has_init_name: bool = false,
    /// tracks if we're still in the directive prologue of a function/script body.
    in_directive_prologue: bool = true,
};

pub const Error = error{OutOfMemory};

pub const Parser = struct {
    source: []const u8,
    lexer: lexer.Lexer,
    arena: std.heap.ArenaAllocator,
    diagnostics: std.ArrayList(Diagnostic) = .empty,
    nodes: std.MultiArrayList(ast.Node) = .empty,
    extra: std.ArrayList(ast.NodeIndex) = .empty,
    current_token: token.Token,
    next_token: ?token.Token = null,

    scratch_statements: ScratchBuffer = .{},
    scratch_cover: ScratchBuffer = .{},

    // multiple scratches to handle multiple extras at the same time
    scratch_a: ScratchBuffer = .{},
    scratch_b: ScratchBuffer = .{},
    //

    context: ParserContext = .{},
    state: ParserState = .{},

    strict_mode: bool,
    source_type: SourceType,
    lang: Lang,

    pub fn init(backing_allocator: std.mem.Allocator, source: []const u8, options: Options) Parser {
        return .{
            .source = source,
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
            .source_type = options.source_type,
            .lang = options.lang,
            .strict_mode = options.source_type == .module,
            .lexer = undefined,
            .current_token = undefined,
        };
    }

    pub inline fn allocator(self: *Parser) std.mem.Allocator {
        return self.arena.allocator();
    }

    /// Parse the source code and return a ParseTree.
    /// The Parser is consumed and should not be used after calling this method.
    /// The caller owns the returned ParseTree and must call deinit() on it.
    pub fn parse(self: *Parser) Error!ParseTree {
        // init lexer
        self.lexer = try lexer.Lexer.init(self.source, self.allocator(), self.source_type, self.strict_mode);

        // let's begin
        try self.advance();

        errdefer {
            self.arena.deinit();
        }

        try self.ensureCapacity();

        const body = try self.parseBody(null);

        const end = self.current_token.span.end;

        const program = try self.addNode(
            .{
                .program = .{
                    .source_type = if (self.source_type == .module) .module else .script,
                    .body = body,
                },
            },
            .{ .start = 0, .end = end },
        );

        const tree = ParseTree{
            .program = program,
            .source = self.source,
            .nodes = self.nodes,
            .extra = self.extra,
            .diagnostics = self.diagnostics,
            .comments = self.lexer.comments,
            .arena = self.arena,
            .source_type = self.source_type,
            .lang = self.lang,
        };

        return tree;
    }

    pub fn parseBody(self: *Parser, terminator: ?token.TokenType) Error!ast.IndexRange {
        const statements_checkpoint = self.scratch_statements.begin();

        self.state.in_directive_prologue = true;

        while (!self.isAtBodyEnd(terminator)) {
            if (try statements.parseStatement(self, .{})) |statement| {
                try self.scratch_statements.append(self.allocator(), statement);

                if (self.state.in_directive_prologue and self.getData(statement) != .directive) {
                    self.state.in_directive_prologue = false;
                }
            } else {
                self.state.in_directive_prologue = false;
                try self.synchronize(terminator);
            }
        }

        return self.addExtra(self.scratch_statements.take(statements_checkpoint));
    }

    inline fn isAtBodyEnd(self: *Parser, terminator: ?token.TokenType) bool {
        return self.current_token.type == .eof or
            (terminator != null and self.current_token.type == terminator.?);
    }

    pub inline fn isTs(self: *Parser) bool {
        return self.lang == .ts or self.lang == .tsx or self.lang == .dts;
    }

    pub inline fn isModule(self: *Parser) bool {
        return self.source_type == .module;
    }

    // utils

    pub inline fn addNode(self: *Parser, data: ast.NodeData, span: ast.Span) Error!ast.NodeIndex {
        const index: ast.NodeIndex = @intCast(self.nodes.len);
        try self.nodes.append(self.allocator(), .{ .data = data, .span = span });
        return index;
    }

    pub fn addExtra(self: *Parser, indices: []const ast.NodeIndex) Error!ast.IndexRange {
        const start: u32 = @intCast(self.extra.items.len);
        const len: u32 = @intCast(indices.len);
        try self.extra.appendSlice(self.allocator(), indices);
        return .{ .start = start, .len = len };
    }

    pub inline fn getSpan(self: *const Parser, index: ast.NodeIndex) ast.Span {
        return self.nodes.items(.span)[index];
    }

    pub inline fn getData(self: *const Parser, index: ast.NodeIndex) ast.NodeData {
        return self.nodes.items(.data)[index];
    }

    pub inline fn setData(self: *Parser, index: ast.NodeIndex, data: ast.NodeData) void {
        self.nodes.items(.data)[index] = data;
    }

    pub inline fn setSpan(self: *Parser, index: ast.NodeIndex, span: ast.Span) void {
        self.nodes.items(.span)[index] = span;
    }

    pub inline fn getExtra(self: *const Parser, range: ast.IndexRange) []const ast.NodeIndex {
        return self.extra.items[range.start..][0..range.len];
    }

    pub inline fn getSourceText(self: *const Parser, start: u32, len: u16) []const u8 {
        return self.source[start..][0..len];
    }

    inline fn nextTokenOrEof(self: *Parser) Error!token.Token {
        return self.lexer.nextToken() catch |e| blk: {
            if (e == error.OutOfMemory) return error.OutOfMemory;
            const lex_err: lexer.LexicalError = @errorCast(e);
            try self.diagnostics.append(self.allocator(), .{
                .message = lexer.getLexicalErrorMessage(lex_err),
                .span = .{ .start = self.lexer.token_start, .end = self.lexer.cursor },
                .help = lexer.getLexicalErrorHelp(lex_err),
            });
            break :blk token.Token.eof(0);
        };
    }

    pub fn advance(self: *Parser) Error!void {
        if (self.next_token) |tok| {
            self.current_token = tok;
            self.next_token = null;
        } else {
            self.current_token = try self.nextTokenOrEof();
        }
    }

    // lazy next token prefetching for lookahead.
    // if lookahead has already cached the next token, `advance` will use it,
    // so there's no extra cost for lookAhead.
    pub fn lookAhead(self: *Parser) Error!token.Token {
        if (self.next_token) |tok| {
            return tok;
        }

        if (self.current_token.type == .eof) {
            return token.Token.eof(0);
        }

        self.next_token = try self.nextTokenOrEof();

        if (self.next_token.?.type == .eof) {
            self.next_token = null;
            return token.Token.eof(0);
        }

        return self.next_token.?;
    }

    pub inline fn replaceTokenAndAdvance(self: *Parser, tok: token.Token) Error!void {
        self.current_token = tok;
        try self.advance();
    }

    pub inline fn expect(self: *Parser, token_type: token.TokenType, message: []const u8, help: ?[]const u8) Error!bool {
        if (self.current_token.type == token_type) {
            try self.advance();
            return true;
        }

        try self.report(self.current_token.span, message, .{ .help = help });
        return false;
    }

    pub inline fn eatSemicolon(self: *Parser, end: u32) Error!?u32 {
        if (self.current_token.type == .semicolon) {
            const semicolon_end = self.current_token.span.end;
            try self.advance();
            return semicolon_end;
        } else {
            if (!self.canInsertSemicolon()) {
                try self.reportFmt(self.current_token.span, "Expected a semicolon or an implicit semicolon after a statement, but found '{s}'", .{self.describeToken(self.current_token)}, .{ .help = "Try inserting a semicolon here" });
                return null;
            }
        }

        return end;
    }

    /// lenient semicolon consumption for statements with special ASI exceptions.
    ///
    /// ES2015+ ASI rule: "The previous token is ) and the inserted semicolon would
    /// then be parsed as the terminating semicolon of a do-while statement (14.7.2)"
    ///
    /// allows `do {} while (false) foo()`, semicolon optional after the `)`.
    pub inline fn eatSemicolonLenient(self: *Parser, end: u32) Error!u32 {
        if (self.current_token.type == .semicolon) {
            const semicolon_end = self.current_token.span.end;
            try self.advance();
            return semicolon_end;
        }
        return end;
    }

    /// https://tc39.es/ecma262/#sec-rules-of-automatic-semicolon-insertion
    pub inline fn canInsertSemicolon(self: *Parser) bool {
        const current_token = self.current_token;
        return current_token.type == .eof or current_token.has_line_terminator_before or current_token.type == .right_brace;
    }

    pub inline fn describeToken(_: *Parser, tok: token.Token) []const u8 {
        return if (tok.type == .eof) "end of file" else tok.lexeme;
    }

    pub const ReportOptions = struct {
        severity: Severity = .@"error",
        help: ?[]const u8 = null,
        labels: []const Label = &.{},
    };

    pub inline fn report(self: *Parser, span: ast.Span, message: []const u8, opts: ReportOptions) Error!void {
        try self.diagnostics.append(self.allocator(), .{
            .severity = opts.severity,
            .message = message,
            .span = span,
            .help = opts.help,
            .labels = opts.labels,
        });
    }

    pub inline fn reportFmt(self: *Parser, span: ast.Span, comptime format: []const u8, args: anytype, opts: ReportOptions) Error!void {
        const message = try std.fmt.allocPrint(self.allocator(), format, args);
        try self.report(span, message, opts);
    }

    pub inline fn label(_: *Parser, span: ast.Span, message: []const u8) Label {
        return .{ .span = span, .message = message };
    }

    pub fn makeLabels(self: *Parser, labels: []const Label) Error![]const Label {
        return try self.allocator().dupe(Label, labels);
    }

    pub fn formatMessage(self: *Parser, comptime format: []const u8, args: anytype) Error![]u8 {
        return try std.fmt.allocPrint(self.allocator(), format, args);
    }

    fn synchronize(self: *Parser, terminator: ?token.TokenType) Error!void {
        var has_advanced = false;

        while (self.current_token.type != .eof) {
            // stop at the block terminator to avoid consuming the closing brace
            if (terminator) |t| {
                if (self.current_token.type == t) return;
            }

            if (self.current_token.type == .semicolon) {
                try self.advance();
                return;
            }

            // check for statement-starting keywords at statement boundaries
            // these are recovery points when they appear after a line terminator
            if (self.current_token.has_line_terminator_before) {
                switch (self.current_token.type) {
                    .class, .function, .@"var", .@"for", .@"if", .@"while", .@"return", .let, .@"const", .using, .@"try", .throw, .debugger, .@"break", .@"continue", .@"switch", .do, .with, .async, .@"export", .import => return,
                    else => {},
                }
            }

            // also check for statement-starting keywords and block starts
            // block starts are always safe recovery points
            switch (self.current_token.type) {
                .left_brace => return,
                .class, .function, .@"var", .@"for", .@"if", .@"while", .@"return", .let, .@"const", .using, .@"try", .throw, .debugger, .@"break", .@"continue", .@"switch", .do, .with, .async, .@"export", .import => {
                    // if we've advanced past the error location, statement-starting keywords
                    // are likely the start of a new statement and safe to stop at
                    if (has_advanced) {
                        return;
                    }
                },
                else => {},
            }

            try self.advance();
            has_advanced = true;
        }
    }

    fn ensureCapacity(self: *Parser) Error!void {
        if (self.nodes.capacity > 0) return;

        const alloc = self.allocator();
        const estimated_nodes = @max(1024, (self.source.len * 3) / 4);
        const estimated_extra = estimated_nodes / 2;

        try self.nodes.ensureTotalCapacity(alloc, estimated_nodes);
        try self.extra.ensureTotalCapacity(alloc, estimated_extra);
        try self.diagnostics.ensureTotalCapacity(alloc, 32);
        try self.scratch_cover.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_statements.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_a.items.ensureTotalCapacity(alloc, 256);
        try self.scratch_b.items.ensureTotalCapacity(alloc, 256);
    }
};

const ScratchBuffer = struct {
    items: std.ArrayList(ast.NodeIndex) = .empty,

    pub inline fn begin(self: *ScratchBuffer) usize {
        return self.items.items.len;
    }

    pub inline fn append(self: *ScratchBuffer, alloc: std.mem.Allocator, index: ast.NodeIndex) Error!void {
        try self.items.append(alloc, index);
    }

    pub inline fn take(self: *ScratchBuffer, checkpoint: usize) []const ast.NodeIndex {
        const slice = self.items.items[checkpoint..];
        self.items.shrinkRetainingCapacity(checkpoint);
        return slice;
    }

    pub inline fn reset(self: *ScratchBuffer, checkpoint: usize) void {
        self.items.shrinkRetainingCapacity(checkpoint);
    }
};

/// Parse JavaScript/TypeScript source code into an AST.
/// Returns a ParseTree that must be freed with deinit().
pub fn parse(backing_allocator: std.mem.Allocator, source: []const u8, options: Options) Error!ParseTree {
    var parser = Parser.init(backing_allocator, source, options);
    return parser.parse();
}

pub const estree = @import("estree.zig");

// implement semantic analysis
// scopes
// symbol tables
// etc etc
//
// TODO:
//
// Redeclaration checks.
// It is a Syntax Error if FunctionBodyContainsUseStrict of FunctionBody is true and IsSimpleParameterList of FormalParameters is false.
// It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the LexicallyDeclaredNames of FunctionBody.
// It is a Syntax Error if FormalParameters Contains SuperProperty is true.
// It is a Syntax Error if FunctionBody Contains SuperProperty is true.
// It is a Syntax Error if FormalParameters Contains SuperCall is true.
// It is a Syntax Error if FormalParameters Contains YieldExpression is true.
// It is a Syntax Error if FormalParameters Contains AwaitExpression is true.
// It is a Syntax Error if FunctionBody Contains SuperCall is true.
// 'evals' and 'arguments' in binding identifier and identifier reference.
// Reserved checks: https://tc39.es/ecma262/#prod-ReservedWord
// It is a Syntax Error if HasDirectSuper of MethodDefinition is true.
// It is a Syntax Error if ConciseBodyContainsUseStrict of ConciseBody is true and IsSimpleParameterList of ArrowParameters is false.
// It is a Syntax Error if any element of the BoundNames of ArrowParameters also occurs in the LexicallyDeclaredNames of ConciseBody.
// (delete unary) It is a Syntax Error if IsStrict(the UnaryExpression) is true and the derived UnaryExpression is PrimaryExpression : IdentifierReference , MemberExpression : MemberExpression . PrivateIdentifier , CallExpression : CallExpression . PrivateIdentifier , OptionalChain : ?. PrivateIdentifier , or OptionalChain : OptionalChain . PrivateIdentifier .
// (delete unary) It is a Syntax Error if the derived UnaryExpression is
// PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
// and CoverParenthesizedExpressionAndArrowParameterList ultimately derives a phrase that, if used in place of UnaryExpression, would produce a Syntax Error according to these rules. This rule is recursively applied.
// Note
// (delete unary) The last rule means that expressions such as delete (((foo))) produce early errors because of recursive application of the first rule.
// ImportMeta :
//  import.meta
//  It is a Syntax Error if the syntactic goal symbol is not Module.
// It is a Syntax Error if this BreakStatement is not nested, directly or indirectly (but not crossing function or static initialization block boundaries), within an IterationStatement or a SwitchStatement.
// It is a Syntax Error if this ContinueStatement is not nested, directly or indirectly (but not crossing function or static initialization block boundaries), within an IterationStatement.
// It is a Syntax Error if any element of the BoundNames of ForDeclaration also occurs in the VarDeclaredNames of Statement.
// It is a Syntax Error if the BoundNames of ForDeclaration contains any duplicate entries.
// In strict mode code, functions can only be declared at top level or inside a block
//
// ImportDeclaration:
// - It is a Syntax Error if the BoundNames of ImportDeclaration contains any duplicate entries.
//
// WithClause:
// - It is a Syntax Error if WithClauseToAttributes of WithClause has two different entries a and b such that a.[[Key]] is b.[[Key]].
//
// ExportDeclaration:
// - It is a Syntax Error if the ExportedNames of ModuleItemList contains any duplicate entries.
// - For each IdentifierName n in the ReferencedBindings of NamedExports:
//   It is a Syntax Error if the StringValue of n is a ReservedWord or the StringValue of n is one of
//   "implements", "interface", "let", "package", "private", "protected", "public", or "static".
//   Note: This is already checked in parser during export parsing for local exports without 'from'.
//
// module-level semantic checks:
// - It is a Syntax Error if the LexicallyDeclaredNames of ModuleItemList contains any duplicate entries.
// - It is a Syntax Error if any element of the LexicallyDeclaredNames of ModuleItemList also occurs in the VarDeclaredNames of ModuleItemList.
// - It is a Syntax Error if the ExportedBindings of ModuleItemList does not also occur in the VarDeclaredNames of ModuleItemList,
//   or the LexicallyDeclaredNames of ModuleItemList, or the ImportedLocalNames of ModuleItemList.
// - It is a Syntax Error if ModuleItemList Contains super.
// - It is a Syntax Error if ModuleItemList Contains NewTarget (except in functions).
// 'let' is reserved in strict mode code.
// export statements cannot be outside of a module.
// 'default' case cannot appear more than once in a switch statement.
// for-in/of loop variable declaration may not have an initializer

pub const Precedence = struct {
    pub const Lowest: u8 = 0;
    pub const Sequence: u8 = 1;
    pub const Assignment: u8 = 2;
    pub const Conditional: u8 = 3;
    pub const Nullish: u8 = 4;
    pub const LogicalOr: u8 = 5;
    pub const LogicalAnd: u8 = 6;
    pub const BitwiseOr: u8 = 7;
    pub const BitwiseXor: u8 = 8;
    pub const BitwiseAnd: u8 = 9;
    pub const Equality: u8 = 10;
    pub const Relational: u8 = 11;
    pub const Shift: u8 = 12;
    pub const Additive: u8 = 13;
    pub const Multiplicative: u8 = 14;
    pub const Exponent: u8 = 15;
    pub const Unary: u8 = 16;
    pub const Postfix: u8 = 17;
    pub const Member: u8 = 18;
};

pub const Mask = struct {
    pub const IsNumericLiteral: u32 = 1 << 12;
    pub const IsBinaryOp: u32 = 1 << 13;
    pub const IsLogicalOp: u32 = 1 << 14;
    pub const IsUnaryOp: u32 = 1 << 15;
    pub const IsAssignmentOp: u32 = 1 << 16;
    pub const IsIdentifierLike: u32 = 1 << 17;
    /// reserved words that are always reserved
    pub const IsReserved: u32 = 1 << 18;
    /// reserved words that are only reserved in strict mode
    pub const IsStrictModeReserved: u32 = 1 << 19;

    pub const PrecShift: u32 = 7;
    pub const PrecOverlap: u32 = 31;
};

pub const TokenType = enum(u32) {
    numeric_literal = 1 | Mask.IsNumericLiteral, // e.g., "123", "45.67"
    hex_literal = 2 | Mask.IsNumericLiteral, // e.g., "0xFF", "0x1A"
    octal_literal = 3 | Mask.IsNumericLiteral, // e.g., "0o777", "0o12"
    binary_literal = 4 | Mask.IsNumericLiteral, // e.g., "0b1010", "0b11"
    bigint_literal = 5 | Mask.IsNumericLiteral, // e.g., "123n", "456n"

    string_literal = 6, // e.g., "'hello'", "\"world\""
    regex_literal = 7, // e.g., "/abc/g", "/[0-9]+/i"

    no_substitution_template = 8, // e.g., "`hello`"
    template_head = 9, // e.g., "`hello ${"
    template_middle = 10, // e.g., "} world ${"
    template_tail = 11, // e.g., "} end`"

    true = 12 | Mask.IsReserved | Mask.IsIdentifierLike, // "true"
    false = 13 | Mask.IsReserved | Mask.IsIdentifierLike, // "false"
    null_literal = 14 | Mask.IsReserved | Mask.IsIdentifierLike, // "null"

    plus = 15 | (11 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnaryOp, // "+"
    minus = 16 | (11 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnaryOp, // "-"
    star = 17 | (12 << Mask.PrecShift) | Mask.IsBinaryOp, // "*"
    slash = 18 | (12 << Mask.PrecShift) | Mask.IsBinaryOp, // "/"
    percent = 19 | (12 << Mask.PrecShift) | Mask.IsBinaryOp, // "%"
    exponent = 20 | (13 << Mask.PrecShift) | Mask.IsBinaryOp, // "**"

    assign = 21 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "="
    plus_assign = 22 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "+="
    minus_assign = 23 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "-="
    star_assign = 24 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "*="
    slash_assign = 25 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "/="
    percent_assign = 26 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "%="
    exponent_assign = 27 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "**="

    increment = 28 | (15 << Mask.PrecShift), // "++"
    decrement = 29 | (15 << Mask.PrecShift), // "--"

    equal = 30 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "=="
    not_equal = 31 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "!="
    strict_equal = 32 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "==="
    strict_not_equal = 33 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "!=="
    less_than = 34 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // "<"
    greater_than = 35 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // ">"
    less_than_equal = 36 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // "<="
    greater_than_equal = 37 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // ">="

    logical_and = 38 | (4 << Mask.PrecShift) | Mask.IsLogicalOp, // "&&"
    logical_or = 39 | (3 << Mask.PrecShift) | Mask.IsLogicalOp, // "||"
    logical_not = 40 | (14 << Mask.PrecShift) | Mask.IsUnaryOp, // "!"

    bitwise_and = 41 | (7 << Mask.PrecShift) | Mask.IsBinaryOp, // "&"
    bitwise_or = 42 | (5 << Mask.PrecShift) | Mask.IsBinaryOp, // "|"
    bitwise_xor = 43 | (6 << Mask.PrecShift) | Mask.IsBinaryOp, // "^"
    bitwise_not = 44 | (14 << Mask.PrecShift) | Mask.IsUnaryOp, // "~"
    left_shift = 45 | (10 << Mask.PrecShift) | Mask.IsBinaryOp, // "<<"
    right_shift = 46 | (10 << Mask.PrecShift) | Mask.IsBinaryOp, // ">>"
    unsigned_right_shift = 47 | (10 << Mask.PrecShift) | Mask.IsBinaryOp, // ">>>"

    bitwise_and_assign = 48 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "&="
    bitwise_or_assign = 49 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "|="
    bitwise_xor_assign = 50 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "^="
    left_shift_assign = 51 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "<<="
    right_shift_assign = 52 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // ">>="
    unsigned_right_shift_assign = 53 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // ">>>="

    nullish_coalescing = 54 | (3 << Mask.PrecShift) | Mask.IsLogicalOp, // "??"
    nullish_assign = 55 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "??="
    logical_and_assign = 56 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "&&="
    logical_or_assign = 57 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "||="
    optional_chaining = 58, // "?."

    left_paren = 59, // "("
    right_paren = 60, // ")"
    left_brace = 61, // "{"
    right_brace = 62, // "}"
    left_bracket = 63, // "["
    right_bracket = 64, // "]"
    semicolon = 65, // ";"
    comma = 66 | (1 << Mask.PrecShift), // ","
    dot = 67, // "."
    spread = 68, // "..."
    arrow = 69, // "=>"
    question = 70 | (2 << Mask.PrecShift), // "?"
    colon = 71, // ":"

    @"if" = 72 | Mask.IsReserved | Mask.IsIdentifierLike, // "if"
    @"else" = 73 | Mask.IsReserved | Mask.IsIdentifierLike, // "else"
    @"switch" = 74 | Mask.IsReserved | Mask.IsIdentifierLike, // "switch"
    case = 75 | Mask.IsReserved | Mask.IsIdentifierLike, // "case"
    default = 76 | Mask.IsReserved | Mask.IsIdentifierLike, // "default"
    @"for" = 77 | Mask.IsReserved | Mask.IsIdentifierLike, // "for"
    @"while" = 78 | Mask.IsReserved | Mask.IsIdentifierLike, // "while"
    do = 79 | Mask.IsReserved | Mask.IsIdentifierLike, // "do"
    @"break" = 80 | Mask.IsReserved | Mask.IsIdentifierLike, // "break"
    @"continue" = 81 | Mask.IsReserved | Mask.IsIdentifierLike, // "continue"

    function = 82 | Mask.IsReserved | Mask.IsIdentifierLike, // "function"
    @"return" = 83 | Mask.IsReserved | Mask.IsIdentifierLike, // "return"
    async = 84 | Mask.IsIdentifierLike, // "async"
    await = 85 | Mask.IsIdentifierLike, // "await"
    yield = 86 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "yield"

    @"var" = 87 | Mask.IsReserved | Mask.IsIdentifierLike, // "var"
    let = 88 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "let"
    @"const" = 89 | Mask.IsReserved | Mask.IsIdentifierLike, // "const"
    using = 90 | Mask.IsIdentifierLike, // "using"

    class = 91 | Mask.IsReserved | Mask.IsIdentifierLike, // "class"
    extends = 92 | Mask.IsReserved | Mask.IsIdentifierLike, // "extends"
    super = 93 | Mask.IsReserved | Mask.IsIdentifierLike, // "super"
    static = 94 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "static"
    @"enum" = 95 | Mask.IsReserved | Mask.IsIdentifierLike, // "enum"
    public = 96 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "public"
    private = 97 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "private"
    protected = 98 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "protected"
    interface = 99 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "interface"
    implements = 100 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "implements"
    package = 101 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "package"

    import = 102 | Mask.IsReserved | Mask.IsIdentifierLike, // "import"
    @"export" = 103 | Mask.IsReserved | Mask.IsIdentifierLike, // "export"
    from = 104 | Mask.IsIdentifierLike, // "from"
    as = 105 | Mask.IsIdentifierLike, // "as"
    namespace = 106 | Mask.IsIdentifierLike, // "namespace"
    assert = 107 | Mask.IsIdentifierLike, // "assert" (import assertions)
    source = 108 | Mask.IsIdentifierLike, // "source" (source phase imports)
    @"defer" = 109 | Mask.IsIdentifierLike, // "defer" (deferred imports)

    @"try" = 110 | Mask.IsReserved | Mask.IsIdentifierLike, // "try"
    @"catch" = 111 | Mask.IsReserved | Mask.IsIdentifierLike, // "catch"
    finally = 112 | Mask.IsReserved | Mask.IsIdentifierLike, // "finally"
    throw = 113 | Mask.IsReserved | Mask.IsIdentifierLike, // "throw"

    new = 114 | Mask.IsReserved | Mask.IsIdentifierLike, // "new"
    this = 115 | Mask.IsReserved | Mask.IsIdentifierLike, // "this"
    typeof = 116 | (14 << Mask.PrecShift) | Mask.IsUnaryOp | Mask.IsReserved | Mask.IsIdentifierLike, // "typeof"
    instanceof = 117 | (9 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsReserved | Mask.IsIdentifierLike, // "instanceof"
    in = 118 | (9 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsReserved | Mask.IsIdentifierLike, // "in"
    of = 119 | Mask.IsIdentifierLike, // "of"
    delete = 120 | (14 << Mask.PrecShift) | Mask.IsUnaryOp | Mask.IsReserved | Mask.IsIdentifierLike, // "delete"
    void = 121 | (14 << Mask.PrecShift) | Mask.IsUnaryOp | Mask.IsReserved | Mask.IsIdentifierLike, // "void"
    with = 122 | Mask.IsReserved | Mask.IsIdentifierLike, // "with"
    debugger = 123 | Mask.IsReserved | Mask.IsIdentifierLike, // "debugger"

    identifier = 124 | Mask.IsIdentifierLike, // e.g., "myVar", "foo", "_bar"
    private_identifier = 125, // e.g., "#privateField", "#method"

    // typescript
    declare = 126 | Mask.IsIdentifierLike, // "declare"

    eof = 127, // end of file

    pub fn precedence(self: TokenType) u5 {
        return @intCast((@intFromEnum(self) >> Mask.PrecShift) & Mask.PrecOverlap);
    }

    pub fn is(self: TokenType, mask: u32) bool {
        return (@intFromEnum(self) & mask) != 0;
    }

    pub fn isNumericLiteral(self: TokenType) bool {
        return self.is(Mask.IsNumericLiteral);
    }

    pub fn isBinaryOperator(self: TokenType) bool {
        return self.is(Mask.IsBinaryOp);
    }

    pub fn isLogicalOperator(self: TokenType) bool {
        return self.is(Mask.IsLogicalOp);
    }

    pub fn isUnaryOperator(self: TokenType) bool {
        return self.is(Mask.IsUnaryOp);
    }

    pub fn isAssignmentOperator(self: TokenType) bool {
        return self.is(Mask.IsAssignmentOp);
    }

    /// returns true for identifier-like tokens.
    /// includes: identifiers, all keywords, literal keywords.
    pub fn isIdentifierLike(self: TokenType) bool {
        return self.is(Mask.IsIdentifierLike);
    }

    /// returns true for unconditionally reserved keywords.
    /// these can NEVER be used as identifiers.
    pub fn isReserved(self: TokenType) bool {
        return self.is(Mask.IsReserved);
    }

    /// returns true for keywords reserved ONLY in strict mode.
    /// these can be identifiers in sloppy mode but not in strict mode.
    /// includes: let, static, implements, interface, package, private, protected, public, yield
    pub fn isStrictModeReserved(self: TokenType) bool {
        return self.is(Mask.IsStrictModeReserved);
    }

    pub fn toString(self: TokenType) ?[]const u8 {
        return switch (self) {
            .true => "true",
            .false => "false",
            .null_literal => "null",

            .plus => "+",
            .minus => "-",
            .star => "*",
            .slash => "/",
            .percent => "%",
            .exponent => "**",

            .assign => "=",
            .plus_assign => "+=",
            .minus_assign => "-=",
            .star_assign => "*=",
            .slash_assign => "/=",
            .percent_assign => "%=",
            .exponent_assign => "**=",

            .increment => "++",
            .decrement => "--",

            .equal => "==",
            .not_equal => "!=",
            .strict_equal => "===",
            .strict_not_equal => "!==",
            .less_than => "<",
            .greater_than => ">",
            .less_than_equal => "<=",
            .greater_than_equal => ">=",

            .logical_and => "&&",
            .logical_or => "||",
            .logical_not => "!",

            .bitwise_and => "&",
            .bitwise_or => "|",
            .bitwise_xor => "^",
            .bitwise_not => "~",
            .left_shift => "<<",
            .right_shift => ">>",
            .unsigned_right_shift => ">>>",

            .bitwise_and_assign => "&=",
            .bitwise_or_assign => "|=",
            .bitwise_xor_assign => "^=",
            .left_shift_assign => "<<=",
            .right_shift_assign => ">>=",
            .unsigned_right_shift_assign => ">>>=",

            .nullish_coalescing => "??",
            .nullish_assign => "??=",
            .logical_and_assign => "&&=",
            .logical_or_assign => "||=",
            .optional_chaining => "?.",

            .left_paren => "(",
            .right_paren => ")",
            .left_brace => "{",
            .right_brace => "}",
            .left_bracket => "[",
            .right_bracket => "]",
            .semicolon => ";",
            .comma => ",",
            .dot => ".",
            .spread => "...",
            .arrow => "=>",
            .question => "?",
            .colon => ":",

            .@"if" => "if",
            .@"else" => "else",
            .@"switch" => "switch",
            .case => "case",
            .default => "default",
            .@"for" => "for",
            .@"while" => "while",
            .do => "do",
            .@"break" => "break",
            .@"continue" => "continue",

            .function => "function",
            .@"return" => "return",
            .async => "async",
            .await => "await",
            .yield => "yield",

            .@"var" => "var",
            .let => "let",
            .@"const" => "const",
            .using => "using",

            .class => "class",
            .extends => "extends",
            .super => "super",
            .static => "static",
            .@"enum" => "enum",
            .public => "public",
            .private => "private",
            .protected => "protected",
            .interface => "interface",
            .implements => "implements",
            .package => "package",

            .import => "import",
            .@"export" => "export",
            .from => "from",
            .as => "as",
            .namespace => "namespace",
            .assert => "assert",
            .source => "source",
            .@"defer" => "defer",

            .@"try" => "try",
            .@"catch" => "catch",
            .finally => "finally",
            .throw => "throw",

            .new => "new",
            .this => "this",
            .typeof => "typeof",
            .instanceof => "instanceof",
            .in => "in",
            .of => "of",
            .delete => "delete",
            .void => "void",
            .with => "with",
            .debugger => "debugger",

            .declare => "declare",

            .eof,
            .numeric_literal,
            .hex_literal,
            .octal_literal,
            .binary_literal,
            .bigint_literal,
            .string_literal,
            .regex_literal,
            .no_substitution_template,
            .template_head,
            .template_middle,
            .template_tail,
            .identifier,
            .private_identifier,
            => null,
        };
    }
};

pub const Span = struct {
    start: u32,
    end: u32,
};

pub const Token = struct {
    span: Span,
    type: TokenType,
    lexeme: []const u8,
    has_line_terminator_before: bool,

    pub inline fn eof(pos: u32) Token {
        return Token{ .lexeme = "", .span = .{ .start = pos, .end = pos }, .type = .eof, .has_line_terminator_before = false };
    }

    pub fn leftBindingPower(self: *const Token) u5 {
        // handle: [no LineTerminator here] ++ --
        if ((self.type == .increment or self.type == .decrement) and self.has_line_terminator_before) {
            return 0; // can't be infix, start new expression
        }

        if (self.type.isBinaryOperator() or self.type.isLogicalOperator() or
            self.type.isAssignmentOperator() or self.type == .increment or self.type == .decrement)
        {
            return self.type.precedence();
        }

        return switch (self.type) {
            .dot, .optional_chaining, .left_bracket, .left_paren => 17,
            // tagged template: only when no line terminator before the template
            .template_head, .no_substitution_template => if (!self.has_line_terminator_before) 17 else 0,
            .comma => 1,
            .question => 2,
            else => 0, // can't be infix
        };
    }
};

const std = @import("std");

pub fn parseJSNumeric(str: []const u8) !f64 {
    if (str.len == 0) return error.InvalidCharacter;

    var buf: [128]u8 = undefined;
    var len: usize = 0;
    for (str) |c| {
        if (c != '_') {
            if (len >= buf.len) return error.Overflow;
            buf[len] = c;
            len += 1;
        }
    }
    const s = buf[0..len];
    if (s.len == 0) return error.InvalidCharacter;

    if (s.len >= 2 and s[0] == '0') {
        switch (s[1]) {
            'x', 'X' => return @floatFromInt(try std.fmt.parseInt(i64, s[2..], 16)),
            'b', 'B' => return @floatFromInt(try std.fmt.parseInt(i64, s[2..], 2)),
            'o', 'O' => return @floatFromInt(try std.fmt.parseInt(i64, s[2..], 8)),
            '0'...'7' => {
                for (s[1..]) |c| {
                    if (c < '0' or c > '7') break;
                } else {
                    return @floatFromInt(try std.fmt.parseInt(i64, s[1..], 8));
                }
            },
            else => {},
        }
    }

    return std.fmt.parseFloat(f64, s) catch @floatFromInt(try std.fmt.parseInt(i64, s, 10));
}

pub const UnicodeId = @import("unicode_id.zig");
pub const Utf = @import("utf.zig");
pub const Number = @import("number.zig");

// Generated file, do not edit.
// See: scripts/generate_unicode_id.zig

// inspired by https://github.com/dtolnay/unicode-ident

pub fn canStartIdentifier(cp: u32) bool {
    if (cp < 128) {
        return (cp >= 'a' and cp <= 'z') or
            (cp >= 'A' and cp <= 'Z') or
            cp == '_' or cp == '$';
    }

    return queryBitTable(cp, &id_start_root, &id_start_leaf);
}

pub fn canContinueIdentifier(cp: u32) bool {
    if (cp < 128) {
        return (cp >= 'a' and cp <= 'z') or
            (cp >= 'A' and cp <= 'Z') or
            cp == '_' or cp == '$' or
            (cp >= '0' and cp <= '9');
    }

    return queryBitTable(cp, &id_continue_root, &id_continue_leaf);
}

const chunk_size = 512;
const bits_per_word = 32;
const leaf_chunk_width = 16;

inline fn queryBitTable(cp: u32, comptime root: []const u8, comptime leaf: []const u64) bool {
    const chunk_idx = cp / chunk_size;
    const leaf_base = @as(u32, root[chunk_idx]) * leaf_chunk_width;
    const offset_in_chunk = cp - (chunk_idx * chunk_size);
    const word_idx = leaf_base + (offset_in_chunk / bits_per_word);
    const bit_position: u5 = @truncate(offset_in_chunk % bits_per_word);
    const word = leaf[word_idx];
    return (word >> bit_position) & 1 == 1;
}

const id_start_root = [_]u8{
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
    0x10, 0x11, 0x11, 0x11, 0x11, 0x11, 0x12, 0x11, 0x13, 0x11, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x15, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x16, 0x17, 0x18, 0x19, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x1a, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x1b, 0x1c, 0x1d, 0x1e,
    0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e,
    0x14, 0x2f, 0x30, 0x11, 0x11, 0x11, 0x11, 0x31, 0x14, 0x14, 0x32, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x33, 0x14, 0x34, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x35, 0x11, 0x11, 0x11, 0x14, 0x36, 0x37, 0x38, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x39, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x3a, 0x3b, 0x3c, 0x11, 0x11, 0x11, 0x11, 0x3d, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x3e, 0x3f, 0x11, 0x11, 0x11, 0x40,
    0x41, 0x42, 0x43, 0x44, 0x45, 0x11, 0x11, 0x46, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x47, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x48, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x49, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x4a, 0x14, 0x4b, 0x11, 0x11, 0x11, 0x11, 0x14, 0x4c, 0x11, 0x11,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x4d, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x4e, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
};

pub const id_start_leaf = [_]u64{
    0x00,       0x00,       0x7fffffe,  0x7fffffe,  0x00,       0x4200400,  0xff7fffff, 0xff7fffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x3ffc3,    0x501f,
    0x00,       0x00,       0x00,       0xbcdf0000, 0xffffd740, 0xfffffffb, 0xffffffff, 0xffbfffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xfffffc03, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xfffeffff, 0x27fffff,  0xffffffff, 0x1ff,      0x00,       0xffff0000, 0x787ff,
    0x00,       0xffffffff, 0x7ff,      0xfffec000, 0xffffffff, 0xffffffff, 0x2fffff,   0x9c00c060,
    0xfffd0000, 0xffff,     0xffffe000, 0xffffffff, 0xffffffff, 0x2003f,    0xfffffc00, 0x43007ff,
    0x43fffff,  0x110,      0x1ffffff,  0xffff07ff, 0xfeff,     0xffffffff, 0x3ff,      0x00,
    0xfffffff0, 0x23ffffff, 0xff010000, 0xfffe0003, 0xfff99fe1, 0x23c5fdff, 0xb0004000, 0x10030003,
    0xfff987e0, 0x36dfdff,  0x5e000000, 0x1c0000,   0xfffbbfe0, 0x23edfdff, 0x10000,    0x2000003,
    0xfff99fe0, 0x23edfdff, 0xb0000000, 0x20003,    0xd63dc7e8, 0x3ffc718,  0x10000,    0x00,
    0xfffddfe0, 0x23fffdff, 0x37000000, 0x03,       0xfffddfe1, 0x23effdff, 0x70000000, 0x60003,
    0xfffddff0, 0x27ffffff, 0x80704000, 0xfc000003, 0xfc7fffe0, 0x2ffbffff, 0x7f,       0x00,
    0xfffffffe, 0xdffff,    0x7f,       0x00,       0xfffff7d6, 0x200dffaf, 0xf000005f, 0x00,
    0x01,       0x00,       0xfffffeff, 0x1fff,     0x1f00,     0x00,       0x00,       0x00,
    0xffffffff, 0x800007ff, 0x3c3f0000, 0xffe1c062, 0x4003,     0xffffffff, 0xffff20bf, 0xf7ffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0x3d7f3dff, 0xffffffff, 0xffff3dff, 0x7f3dffff, 0xff7fff3d, 0xffffffff,
    0xff3dffff, 0xffffffff, 0x7ffffff,  0x00,       0xffff,     0xffffffff, 0xffffffff, 0x3f3fffff,
    0xfffffffe, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffff9fff, 0x7fffffe,  0xffffffff, 0xffffffff, 0x1ffc7ff,
    0x8003ffff, 0x3ffff,    0x3ffff,    0x1dfff,    0xffffffff, 0xfffff,    0x10800000, 0x00,
    0x00,       0xffffffff, 0xffffffff, 0x1ffffff,  0xffffffff, 0xffff05ff, 0xffffffff, 0x3fffff,
    0x7fffffff, 0x00,       0xffff0000, 0x1f3fff,   0xffffffff, 0xffff0fff, 0x3ff,      0x00,
    0x7fffff,   0xffffffff, 0x1fffff,   0x00,       0x00,       0x80,       0x00,       0x00,
    0xffffffe0, 0xfffff,    0x1fe0,     0x00,       0xfffffff8, 0xfc00c001, 0xffffffff, 0x3f,
    0xffffffff, 0x0f,       0xfc00e000, 0x3fffffff, 0xffff07ff, 0xe7ffffff, 0x00,       0x46fde00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0x3f3fffff, 0xffffffff, 0xaaff3f3f, 0x3fffffff, 0xffffffff, 0x5fdfffff, 0xfcf1fdc,  0x1fdc1fff,
    0x00,       0x00,       0x00,       0x80020000, 0x1fff0000, 0x00,       0x00,       0x00,
    0x3f2ffc84, 0xf3fffd50, 0x43e0,     0xffffffff, 0x1ff,      0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xc781f,
    0xffffffff, 0xffff20bf, 0xffffffff, 0x80ff,     0x7fffff,   0x7f7f7f7f, 0x7f7f7f7f, 0x00,
    0xe0,       0x1f3e03fe, 0xfffffffe, 0xffffffff, 0xf87fffff, 0xfffffffe, 0xffffffff, 0xf7ffffff,
    0xffffffe0, 0xfffeffff, 0xffffffff, 0xffffffff, 0x7fff,     0xffffffff, 0x00,       0xffff0000,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x1fff,     0x00,       0xffff0000, 0x3fffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffff1fff, 0xc00,      0xffffffff, 0x80007fff, 0x3fffffff, 0xffffffff, 0xffffffff, 0xffff,
    0xff800000, 0xfffffffc, 0xffffffff, 0xffffffff, 0xfffff9ff, 0xffffffff, 0x1fffffff, 0xfffe0000,
    0xfffff7bb, 0x07,       0xffffffff, 0xfffff,    0xfffffffc, 0xfffff,    0x00,       0x68fc0000,
    0xfffffc00, 0xffff003f, 0x7f,       0x1fffffff, 0xfffffff0, 0x7ffff,    0x8000,     0x7c00ffdf,
    0xffffffff, 0x1ff,      0xff7,      0xc47fffff, 0xffffffff, 0x3e62ffff, 0x38000005, 0x1c07ff,
    0x7e7e7e,   0xffff7f7f, 0xf7ffffff, 0xffff03ff, 0xffffffff, 0xffffffff, 0xffffffff, 0x07,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffff000f, 0xfffff87f, 0xfffffff,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffff3fff, 0xffffffff, 0xffffffff, 0x3ffffff,  0x00,
    0xa0f8007f, 0x5f7ffdff, 0xffffffdb, 0xffffffff, 0xffffffff, 0x3ffff,    0xfff80000, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0x3fffffff, 0xffff0000, 0xffffffff, 0xfffcffff, 0xffffffff, 0xff,       0xfff0000,
    0x00,       0x00,       0x00,       0xffdf0000, 0xffffffff, 0xffffffff, 0xffffffff, 0x1fffffff,
    0x00,       0x7fffffe,  0x7fffffe,  0xffffffc0, 0xffffffff, 0x7fffffff, 0x1cfcfcfc, 0x00,
    0xffffefff, 0xb7ffff7f, 0x3fff3fff, 0x00,       0xffffffff, 0xffffffff, 0xffffffff, 0x7ffffff,
    0x00,       0x00,       0xffffffff, 0x1fffff,   0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x1fffffff, 0xffffffff, 0x1ffff,    0x00,
    0xffffffff, 0xffffe000, 0xffff07ff, 0x3fffff,   0x3fffffff, 0xffffffff, 0x3eff0f,   0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x3fffffff, 0xffff0000, 0xff0fffff, 0xfffffff,
    0xffffffff, 0xffff00ff, 0xffffffff, 0xf7ff000f, 0xffb7f7ff, 0x1bfbfffb, 0xffffffff, 0xfffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0x7fffff,   0x3fffff,   0xff,       0xffffffbf, 0x7fdffff,  0x00,       0x00,
    0xfffffd3f, 0x91bfffff, 0x3fffff,   0x7fffff,   0x7fffffff, 0x00,       0x00,       0x37ffff,
    0x3fffff,   0x3ffffff,  0x3ffffff,  0x00,       0xffffffff, 0xc0ffffff, 0x00,       0x00,
    0xfeef0001, 0x3fffff,   0x00,       0x1fffffff, 0x1fffffff, 0x00,       0xfffffeff, 0x1f,
    0xffffffff, 0x3fffff,   0x3fffff,   0x7ffff,    0x3ffff,    0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0x1ff,      0x00,       0xffffffff, 0x7ffff,    0xffffffff, 0x7ffff,
    0xffffffff, 0x0f,       0xfffffc00, 0xffff803f, 0x3f,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0xffffffff, 0x303ff,    0xfc,       0x00,
    0x1fffffff, 0xffff0080, 0x3f,       0xffff0000, 0x03,       0xffff0000, 0x1f,       0x7fffff,
    0xfffffff8, 0xffffff,   0x00,       0x260000,   0xfffffff8, 0xffff,     0xffff0000, 0x1ff,
    0xfffffff8, 0x7f,       0xffff0090, 0x47ffff,   0xfffffff8, 0x7ffff,    0x1400001e, 0x00,
    0xfffbffff, 0x80000fff, 0x01,       0x00,       0xbfffbd7f, 0xffff01ff, 0x7fffffff, 0x00,
    0xfff99fe0, 0x23edfdff, 0xe0010000, 0x03,       0xffff4bff, 0xbfffff,   0xa0000,    0x00,
    0xffffffff, 0x1fffff,   0x80000780, 0x03,       0xffffffff, 0xffff,     0xb0,       0x00,
    0x00,       0x00,       0x00,       0x00,       0xffffffff, 0x7fff,     0xf000000,  0x00,
    0xffffffff, 0xffff,     0x10,       0x00,       0xffffffff, 0x10007ff,  0x00,       0x00,
    0x7ffffff,  0x00,       0x7f,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0xfff,      0x00,       0x00,       0x00,       0xffffffff, 0xffffffff, 0x80000000,
    0xff6ff27f, 0x8000ffff, 0x02,       0x00,       0x00,       0xfffffcff, 0x1ffff,    0x0a,
    0xfffff801, 0x407ffff,  0xf0010000, 0xffffffff, 0x200003ff, 0xffff0000, 0xffffffff, 0x1ffffff,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0xffffffff, 0x01,
    0xfffffdff, 0x7fff,     0x01,       0xfffc0000, 0xffff,     0x00,       0x00,       0x00,
    0xfffffb7f, 0x1ffff,    0x40,       0xfffffdbf, 0x10003ff,  0xffff0000, 0xfffffff,  0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x7ffff,
    0xfffdfff4, 0xfffff,    0x00,       0x00,       0x00,       0x10000,    0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x3ffffff,  0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0x7fff,     0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0x0f,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0xffff0000, 0xffffffff, 0xffffffff, 0x1ffff,
    0xffffffff, 0xffff,     0x7e,       0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x7ffffff,
    0xffffffff, 0xffffffff, 0x7f,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x3fffffff, 0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0x1ffffff,  0x7fffffff, 0xffff0000, 0xffffffff, 0x7fffffff, 0xffff0000, 0x3fff,
    0xffffffff, 0xffff,     0x0f,       0xe0fffff8, 0xffff,     0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0xffffffff, 0x1fff,     0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0xffffffff, 0xffffffff, 0x00,       0xf9ffffff, 0xfffff,    0x00,
    0xffffffff, 0xffffffff, 0x107ff,    0x00,       0xfff80000, 0x00,       0x00,       0x7c000b,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x3fffff,   0x80000000,
    0x7fffffff, 0x00,       0x00,       0x00,       0xffffffff, 0xffffffff, 0xffffffff, 0x7ffff,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x6fef0000,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0x40007,    0x270000,   0xffff00f0, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xfffffff,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0x1fff07ff, 0x3ff01ff,  0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffdfffff, 0xffffffff, 0xdfffffff, 0xebffde64, 0xffffffef, 0xffffffff,
    0xdfdfe7bf, 0x7bffffff, 0xfffdfc5f, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffff3f, 0xf7fffffd, 0xf7ffffff,
    0xffdfffff, 0xffdfffff, 0xffff7fff, 0xffff7fff, 0xfffffdff, 0xfffffdff, 0xff7,      0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x7fffffff, 0x7e0,      0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0xffff0000, 0xffffffff, 0x3fff,     0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0x3f801fff, 0x4000,     0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0xffff0000, 0x3fff,     0xffffffff, 0xfff,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0xffff0000, 0xfff,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0xffff0000, 0x13fff,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x7fffffff, 0xc01f3fb7,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x7fff6f7f,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x1f,       0x00,
    0xffffffff, 0xffffffff, 0x80f,      0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffef, 0xaf7fe96,  0xaa96ea84, 0x5ef7f796, 0xffffbff,  0xffffbee,  0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0x3fffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffff3fff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffff0001,
    0xffffffff, 0xffffffff, 0x3fffffff, 0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x3fffffff, 0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffff07ff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0x3ffffff,  0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
};

pub const id_continue_root = [_]u8{
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
    0x10, 0x11, 0x11, 0x11, 0x11, 0x11, 0x12, 0x11, 0x13, 0x11, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x15, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x16, 0x17, 0x18, 0x19, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x1a, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x1b, 0x1c, 0x1d, 0x1e,
    0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e,
    0x14, 0x2f, 0x30, 0x11, 0x11, 0x11, 0x11, 0x31, 0x14, 0x14, 0x32, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x33, 0x14, 0x34, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x35, 0x11, 0x11, 0x11, 0x14, 0x36, 0x37, 0x38, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x39, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x3a, 0x3b, 0x3c, 0x11, 0x11, 0x11, 0x11, 0x3d, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x3e, 0x3f, 0x40, 0x41, 0x42, 0x43, 0x11, 0x44, 0x11, 0x45,
    0x46, 0x47, 0x48, 0x49, 0x4a, 0x11, 0x11, 0x4b, 0x11, 0x11, 0x11, 0x11, 0x11, 0x4c, 0x11, 0x11,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x4d, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x4e, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x4f, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x50, 0x14, 0x51, 0x11, 0x11, 0x11, 0x11, 0x14, 0x52, 0x11, 0x11,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x53, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x14, 0x54, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x55, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
    0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
};

const id_continue_leaf = [_]u64{
    0x00,       0x3ff0000,  0x87fffffe, 0x7fffffe,  0x00,       0x4a00400,  0xff7fffff, 0xff7fffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x3ffc3,    0x501f,
    0xffffffff, 0xffffffff, 0xffffffff, 0xbcdfffff, 0xffffd7c0, 0xfffffffb, 0xffffffff, 0xffbfffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xfffffcfb, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xfffeffff, 0x27fffff,  0xffffffff, 0xfffe01ff, 0xbfffffff, 0xffff00b6, 0x787ff,
    0x7ff0000,  0xffffffff, 0xffffffff, 0xffffc3ff, 0xffffffff, 0xffffffff, 0x9fefffff, 0x9ffffdff,
    0xffff0000, 0xffffffff, 0xffffe7ff, 0xffffffff, 0xffffffff, 0x3ffff,    0xffffffff, 0x243fffff,
    0xffffffff, 0x3fff,     0xfffffff,  0xffff07ff, 0xff80feff, 0xffffffff, 0xffffffff, 0xfffffffb,
    0xffffffff, 0xffffffff, 0xffffffff, 0xfffeffcf, 0xfff99fef, 0xf3c5fdff, 0xb080799f, 0x5003ffcf,
    0xfff987ee, 0xd36dfdff, 0x5e023987, 0x3fffc0,   0xfffbbfee, 0xf3edfdff, 0x13bbf,    0xfe00ffcf,
    0xfff99fee, 0xf3edfdff, 0xb0e0399f, 0x2ffcf,    0xd63dc7ec, 0xc3ffc718, 0x813dc7,   0xffc0,
    0xfffddfff, 0xf3fffdff, 0x37603ddf, 0xffcf,     0xfffddfef, 0xf3effdff, 0x70603ddf, 0xeffcf,
    0xfffddfff, 0xffffffff, 0x80f07ddf, 0xfc00ffcf, 0xfc7fffee, 0x2ffbffff, 0xff5f847f, 0xcffc0,
    0xfffffffe, 0x7ffffff,  0x3ff7fff,  0x00,       0xfffff7d6, 0x3fffffaf, 0xf3ff7f5f, 0x00,
    0x3000001,  0xc2a003ff, 0xfffffeff, 0xfffe1fff, 0xfeffffdf, 0x1fffffff, 0x40,       0x00,
    0xffffffff, 0xffffffff, 0xffff03ff, 0xffffffff, 0x3fffffff, 0xffffffff, 0xffff20bf, 0xf7ffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0x3d7f3dff, 0xffffffff, 0xffff3dff, 0x7f3dffff, 0xff7fff3d, 0xffffffff,
    0xff3dffff, 0xffffffff, 0xe7ffffff, 0x3fe00,    0xffff,     0xffffffff, 0xffffffff, 0x3f3fffff,
    0xfffffffe, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffff9fff, 0x7fffffe,  0xffffffff, 0xffffffff, 0x1ffc7ff,
    0x803fffff, 0x1fffff,   0xfffff,    0xddfff,    0xffffffff, 0xffffffff, 0x308fffff, 0x3ff,
    0x3ffb800,  0xffffffff, 0xffffffff, 0x1ffffff,  0xffffffff, 0xffff07ff, 0xffffffff, 0x3fffff,
    0x7fffffff, 0xfff0fff,  0xffffffc0, 0x1f3fff,   0xffffffff, 0xffff0fff, 0x7ff03ff,  0x00,
    0xfffffff,  0xffffffff, 0x7fffffff, 0x9fffffff, 0x3ff03ff,  0xbfff0080, 0x3fffffff, 0xfff,
    0xffffffff, 0xffffffff, 0x3ff1fff,  0xff800,    0xffffffff, 0xffffffff, 0xffffffff, 0xfffff,
    0xffffffff, 0xffffff,   0xffffe3ff, 0x3fffffff, 0xffff07ff, 0xe7ffffff, 0xfff70000, 0x7ffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0x3f3fffff, 0xffffffff, 0xaaff3f3f, 0x3fffffff, 0xffffffff, 0x5fdfffff, 0xfcf1fdc,  0x1fdc1fff,
    0x3000,     0x80000000, 0x100001,   0x80020000, 0x1fff0000, 0x00,       0x1fff0000, 0x1ffe2,
    0x3f2ffc84, 0xf3fffd50, 0x43e0,     0xffffffff, 0x1ff,      0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xff81f,
    0xffffffff, 0xffff20bf, 0xffffffff, 0x800080ff, 0x7fffff,   0x7f7f7f7f, 0x7f7f7f7f, 0xffffffff,
    0xe0,       0x1f3efffe, 0xfffffffe, 0xffffffff, 0xfe7fffff, 0xfffffffe, 0xffffffff, 0xffffffff,
    0xffffffe0, 0xfffeffff, 0xffffffff, 0xffffffff, 0x7fff,     0xffffffff, 0x00,       0xffff0000,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x1fff,     0x00,       0xffff0000, 0x3fffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffff1fff, 0xfff,      0xffffffff, 0xbff0ffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x3ffff,
    0xff800000, 0xfffffffc, 0xffffffff, 0xffffffff, 0xfffff9ff, 0xffffffff, 0x1fffffff, 0xfffe0000,
    0xffffffff, 0x10ff,     0xffffffff, 0xfffff,    0xffffffff, 0xffffffff, 0x3ff003f,  0xe8ffffff,
    0xffffffff, 0xffff3fff, 0xfffff,    0x1fffffff, 0xffffffff, 0xffffffff, 0x3ff8001,  0x7fffffff,
    0xffffffff, 0x7fffff,   0x3ff3fff,  0xfc7fffff, 0xffffffff, 0xffffffff, 0x38000007, 0x7cffff,
    0x7e7e7e,   0xffff7f7f, 0xf7ffffff, 0xffff03ff, 0xffffffff, 0xffffffff, 0xffffffff, 0x3ff37ff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffff000f, 0xfffff87f, 0xfffffff,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffff3fff, 0xffffffff, 0xffffffff, 0x3ffffff,  0x00,
    0xe0f8007f, 0x5f7ffdff, 0xffffffdb, 0xffffffff, 0xffffffff, 0x3ffff,    0xfff80000, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0x3fffffff, 0xffff0000, 0xffffffff, 0xfffcffff, 0xffffffff, 0xff,       0xfff0000,
    0xffff,     0x18ffff,   0xe000,     0xffdf0000, 0xffffffff, 0xffffffff, 0xffffffff, 0x1fffffff,
    0x3ff0000,  0x87fffffe, 0x7fffffe,  0xffffffe0, 0xffffffff, 0x7fffffff, 0x1cfcfcfc, 0x00,
    0xffffefff, 0xb7ffff7f, 0x3fff3fff, 0x00,       0xffffffff, 0xffffffff, 0xffffffff, 0x7ffffff,
    0x00,       0x00,       0xffffffff, 0x1fffff,   0x00,       0x00,       0x00,       0x20000000,
    0x00,       0x00,       0x00,       0x00,       0x1fffffff, 0xffffffff, 0x1ffff,    0x01,
    0xffffffff, 0xffffe000, 0xffff07ff, 0x7ffffff,  0x3fffffff, 0xffffffff, 0x3eff0f,   0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x3fffffff, 0xffff03ff, 0xff0fffff, 0xfffffff,
    0xffffffff, 0xffff00ff, 0xffffffff, 0xf7ff000f, 0xffb7f7ff, 0x1bfbfffb, 0xffffffff, 0xfffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0x7fffff,   0x3fffff,   0xff,       0xffffffbf, 0x7fdffff,  0x00,       0x00,
    0xfffffd3f, 0x91bfffff, 0x3fffff,   0x7fffff,   0x7fffffff, 0x00,       0x00,       0x37ffff,
    0x3fffff,   0x3ffffff,  0x3ffffff,  0x00,       0xffffffff, 0xc0ffffff, 0x00,       0x00,
    0xfeeff06f, 0x873fffff, 0x00,       0x1fffffff, 0x1fffffff, 0x00,       0xfffffeff, 0x7f,
    0xffffffff, 0x3fffff,   0x3fffff,   0x7ffff,    0x3ffff,    0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0x1ff,      0x00,       0xffffffff, 0x7ffff,    0xffffffff, 0x7ffff,
    0xffffffff, 0x3ff00ff,  0xffffffff, 0xffffbe3f, 0x3f,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0xffffffff, 0x31bff,    0xfc,       0xfc000000,
    0x1fffffff, 0xffff0080, 0x1ffff,    0xffff0000, 0x3f,       0xffff0000, 0x1f,       0x7fffff,
    0xffffffff, 0xffffffff, 0x7f,       0x803fffc0, 0xffffffff, 0x7ffffff,  0xffff0004, 0x3ff01ff,
    0xffffffff, 0xffdfffff, 0xffff00f0, 0x4fffff,   0xffffffff, 0xffffffff, 0x17ffde1f, 0x00,
    0xfffbffff, 0xc0ffffff, 0x03,       0x00,       0xbfffbd7f, 0xffff01ff, 0xffffffff, 0x3ff07ff,
    0xfff99fef, 0xfbedfdff, 0xe081399f, 0x1f1fcf,   0xffff4bff, 0xffbfffff, 0xff7a5,    0x06,
    0xffffffff, 0xffffffff, 0xc3ff07ff, 0x03,       0xffffffff, 0xffffffff, 0x3ff00bf,  0x00,
    0x00,       0x00,       0x00,       0x00,       0xffffffff, 0xff3fffff, 0x3f000001, 0x00,
    0xffffffff, 0xffffffff, 0x3ff0011,  0x00,       0xffffffff, 0x1ffffff,  0xffff03ff, 0x0f,
    0xe7ffffff, 0x3ff0fff,  0x7f,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0x7ffffff,  0x00,       0x00,       0x00,       0xffffffff, 0xffffffff, 0x800003ff,
    0xff6ff27f, 0xf9bfffff, 0x3ff000f,  0x00,       0x00,       0xfffffcff, 0xfcffffff, 0x1b,
    0xffffffff, 0x7fffffff, 0xffff0080, 0xffffffff, 0x23ffffff, 0xffff0000, 0xffffffff, 0x1ffffff,
    0x00,       0x00,       0x00,       0xff,       0x00,       0x00,       0xffffffff, 0x3ff0001,
    0xfffffdff, 0xff7fffff, 0x3ff0001,  0xfffc0000, 0xfffcffff, 0x7ffeff,   0x00,       0x00,
    0xfffffb7f, 0xb47fffff, 0x3ff00ff,  0xfffffdbf, 0x1fb7fff,  0xffff03ff, 0xfffffff,  0x3ff,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x7fffff,
    0xfffdffff, 0xc7ffffff, 0x7ff0007,  0x00,       0x00,       0x10000,    0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x3ffffff,  0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0x7fff,     0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0x0f,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0xffff0000, 0xffffffff, 0xffffffff, 0x1ffff,
    0xffffffff, 0xffff,     0x3fffff,   0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x7ffffff,
    0xffffffff, 0xffffffff, 0x7f,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0x3ffffff,  0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0x1ffffff,  0x7fffffff, 0xffff03ff, 0xffffffff, 0x7fffffff, 0xffff03ff, 0x1f3fff,
    0xffffffff, 0x7fffff,   0x3ff000f,  0xe0fffff8, 0xffff,     0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0xffffffff, 0x3ff1fff,  0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0xffffffff, 0xffffffff, 0x00,       0xf9ffffff, 0xfffff,    0x00,
    0xffffffff, 0xffffffff, 0xffff87ff, 0xffffffff, 0xffff80ff, 0x00,       0x00,       0x7f001b,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x3fffff,   0x80000000,
    0x7fffffff, 0x00,       0x00,       0x00,       0xffffffff, 0xffffffff, 0xffffffff, 0x7ffff,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x6fef0000,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0x40007,    0x270000,   0xffff00f0, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xfffffff,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0x1fff07ff, 0x63ff01ff, 0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x3ff0000,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0xffff3fff, 0x7f,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0xf807e3e0, 0xfe7,      0x3c00,     0x00,       0x00,
    0x00,       0x00,       0x1c,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffdfffff, 0xffffffff, 0xdfffffff, 0xebffde64, 0xffffffef, 0xffffffff,
    0xdfdfe7bf, 0x7bffffff, 0xfffdfc5f, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffff3f, 0xf7fffffd, 0xf7ffffff,
    0xffdfffff, 0xffdfffff, 0xffff7fff, 0xffff7fff, 0xfffffdff, 0xfffffdff, 0xffffcff7, 0xffffffff,
    0xffffffff, 0xf87fffff, 0xffffffff, 0x201fff,   0xf8000010, 0xfffe,     0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x7fffffff, 0x7e0,      0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xf9ffff7f, 0xffff07db, 0xffffffff, 0x3fff,     0x8000,     0x00,       0x00,       0x00,
    0xffffffff, 0x3fff1fff, 0x43ff,     0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0xffff0000, 0x7fff,     0xffffffff, 0x3ffffff,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0xffff0000, 0x3ffffff,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0xffff0000, 0x7ffffff,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x7fffffff, 0xc03fffff,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x7fff6f7f,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x7f001f,   0x00,
    0xffffffff, 0xffffffff, 0x3ff0fff,  0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffef, 0xaf7fe96,  0xaa96ea84, 0x5ef7f796, 0xffffbff,  0xffffbee,  0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x3ff0000,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0x3fffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffff3fff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffff0001,
    0xffffffff, 0xffffffff, 0x3fffffff, 0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x3fffffff, 0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffff07ff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff, 0xffffffff, 0x3ffffff,  0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,       0x00,
    0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffff,
};

const std = @import("std");

pub const CodePoint = struct { len: u3, value: u21 };

pub const Utf8Error = error{InvalidUtf8};

pub fn codePointAt(str: []const u8, i: u32) Utf8Error!CodePoint {
    const len = std.unicode.utf8ByteSequenceLength(str[i]) catch return error.InvalidUtf8;
    const codepoint = switch (len) {
        1 => str[i],
        2 => std.unicode.utf8Decode2(.{ str[i], str[i + 1] }),
        3 => std.unicode.utf8Decode3(.{ str[i], str[i + 1], str[i + 2] }),
        4 => std.unicode.utf8Decode4(.{ str[i], str[i + 1], str[i + 2], str[i + 3] }),
        else => unreachable,
    };
    return .{ .len = @intCast(len), .value = codepoint catch return error.InvalidUtf8 };
}

pub fn isOctalDigit(digit: u8) bool {
    return digit >= '0' and digit <= '7';
}

pub fn lineTerminatorLen(source: []const u8, pos: usize) u8 {
    if (pos >= source.len) return 0;

    const c = source[pos];

    // LF
    if (c == '\n') return 1;

    // CR or CRLF
    if (c == '\r') {
        if (pos + 1 < source.len and source[pos + 1] == '\n') return 2;
        return 1;
    }

    // LS (U+2028) / PS (U+2029) encoded as UTF-8: E2 80 A8 / E2 80 A9
    return isUnicodeSeparator(source, pos);
}

/// normalize line terminators to LF (\n) in the output buffer
/// this handles CR, CRLF, and converts them all to LF
/// returns the number of bytes consumed from source
pub fn normalizeLineEnding(source: []const u8, pos: usize) struct { len: u8, normalized: u8 } {
    const c = source[pos];

    // CRLF -> LF
    if (c == '\r') {
        if (pos + 1 < source.len and source[pos + 1] == '\n') {
            return .{ .len = 2, .normalized = '\n' };
        }
        // standalone CR -> LF
        return .{ .len = 1, .normalized = '\n' };
    }

    // already LF, pass through
    if (c == '\n') {
        return .{ .len = 1, .normalized = '\n' };
    }

    // not a line ending
    return .{ .len = 1, .normalized = c };
}

pub inline fn isLineTerminator(source: []const u8, pos: usize) bool {
    return lineTerminatorLen(source, pos) > 0;
}

/// check if the byte sequence at `pos` is U+2028 (Line Separator) or U+2029 (Paragraph Separator)
/// returns the length (3 bytes) if true, otherwise 0
pub fn isUnicodeSeparator(source: []const u8, pos: usize) u8 {
    if (pos + 2 < source.len and source[pos] == 0xE2 and source[pos + 1] == 0x80) {
        if (source[pos + 2] == 0xA8 or source[pos + 2] == 0xA9) {
            return 3;
        }
    }
    return 0;
}

pub fn isMultiByteSpace(cp: u21) bool {
    return switch (cp) {
        '\u{FEFF}',
        '\u{00A0}',
        '\u{2000}',
        '\u{2001}'...'\u{200A}',
        '\u{202F}',
        '\u{205F}',
        '\u{3000}',
        '\u{1680}',
        => true,
        else => false,
    };
}

pub fn parseOctal(input: []const u8, start: usize) struct { value: u21, end: usize } {
    var value: u16 = 0;
    var i = start;
    const max: usize = if (input[start] <= '3') 3 else 2;
    var count: usize = 0;
    while (i < input.len and count < max) : (i += 1) {
        if (input[i] >= '0' and input[i] <= '7') {
            value = (value << 3) | (input[i] - '0');
            count += 1;
        } else break;
    }
    return .{ .value = @intCast(value), .end = i };
}

/// exactly 2 hex digits (for \xHH escape sequences)
pub fn parseHex2(input: []const u8, start: usize) ?struct { value: u21, end: usize } {
    if (start + 2 > input.len) return null;

    const hi = hexVal(input[start]) orelse return null;
    const lo = hexVal(input[start + 1]) orelse return null;

    return .{ .value = (@as(u21, hi) << 4) | lo, .end = start + 2 };
}

/// exactly 4 hex digits (for \uHHHH escape sequences)
pub fn parseHex4(input: []const u8, start: usize) ?struct { value: u21, end: usize } {
    if (start + 4 > input.len) return null;

    var value: u21 = 0;
    for (0..4) |j| {
        const d = hexVal(input[start + j]) orelse return null;
        value = (value << 4) | d;
    }

    return .{ .value = value, .end = start + 4 };
}

/// variable-length hex digits until closing brace (for \u{H...} escape sequences)
/// allows leading zeros but validates the value is <= 0x10FFFF
pub fn parseHexVariable(input: []const u8, start: usize, max_digits: usize) ?struct { value: u21, end: usize, has_digits: bool } {
    var value: u32 = 0;
    var i = start;
    var count: usize = 0;
    var has_digits = false;

    while (i < input.len and count < max_digits) {
        if (hexVal(input[i])) |d| {
            // check for overflow before shifting
            if (value > 0x10FFFF) {
                // continue scanning but mark as overflow
                value = 0xFFFFFFFF;
            } else {
                value = (value << 4) | d;
            }
            has_digits = true;
            count += 1;
            i += 1;
        } else {
            break;
        }
    }

    if (!has_digits) return null;
    if (value > 0x10FFFF) return null;
    return .{ .value = @intCast(value), .end = i, .has_digits = has_digits };
}

pub fn hexVal(c: u8) ?u8 {
    return if (c >= '0' and c <= '9') c - '0' else if (c >= 'a' and c <= 'f') c - 'a' + 10 else if (c >= 'A' and c <= 'F') c - 'A' + 10 else null;
}

pub fn buildUtf16PosMap(allocator: std.mem.Allocator, source: []const u8) ![]u32 {
    var map = try allocator.alloc(u32, source.len + 1);
    var byte_pos: usize = 0;
    var utf16_pos: u32 = 0;

    while (byte_pos < source.len) {
        map[byte_pos] = utf16_pos;
        const len = std.unicode.utf8ByteSequenceLength(source[byte_pos]) catch 1;
        utf16_pos += if (len == 4) 2 else 1; // surrogate pair for 4-byte sequences
        for (1..len) |i| {
            if (byte_pos + i < source.len) map[byte_pos + i] = utf16_pos;
        }
        byte_pos += len;
    }
    map[source.len] = utf16_pos;
    return map;
}

const std = @import("std");
const js = @import("js");

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}).init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file_path = "test.js";

    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    var buffer: [4096]u8 = undefined;
    var reader = file.reader(&buffer);
    const contents = try reader.interface.allocRemaining(allocator, std.Io.Limit.limited(10 * 1024 * 1024));
    defer allocator.free(contents);

    var start = try std.time.Timer.start();
    const tree = try js.parse(std.heap.page_allocator, contents, .{ .source_type = .module });
    defer tree.deinit();

    const taken = start.read();

    const taken_ms = @as(f64, @floatFromInt(taken)) / ns_to_ms;

    var line_count: usize = 1;
    for (contents) |c| {
        if (c == '\n') line_count += 1;
    }

    const million_lines_per_sec = (@as(f64, @floatFromInt(line_count)) / 1_000_000.0) / (taken_ms / 1000.0);

    const mb_per_sec = (@as(f64, @floatFromInt(contents.len)) / 1_000_000.0) / (taken_ms / 1000.0);

    const json = try js.estree.toJSON(&tree, allocator, .{});

    defer allocator.free(json);

    std.debug.print("\n{s}\n", .{json});

    if (tree.hasDiagnostics()) {
        for (tree.diagnostics.items) |err| {
            const start_pos = getLineAndColumn(contents, err.span.start);
            const end_pos = getLineAndColumn(contents, err.span.end);

            std.debug.print("\nError: {s} at test.js:{d}:{d} to test.js:{d}:{d}\n", .{ err.message, start_pos.line, start_pos.col, end_pos.line, end_pos.col });
            if (err.help) |help| std.debug.print("  Help: {s}\n\n", .{help});
            if (err.labels.len > 0) {
                for (err.labels) |label| {
                    const label_start_pos = getLineAndColumn(contents, label.span.start);
                    const label_end_pos = getLineAndColumn(contents, label.span.end);

                    std.debug.print("  Label: {s} at test.js:{d}:{d} to test.js:{d}:{d}\n", .{ label.message, label_start_pos.line, label_start_pos.col, label_end_pos.line, label_end_pos.col });
                }
            }
        }
    }

    std.debug.print("\n\n{d:.2}ms | {d:.2} million lines/sec | {d:.2} MB/s\n\n", .{ taken_ms, million_lines_per_sec, mb_per_sec });
}

const ns_to_ms = 1_000_000.0;

fn getLineAndColumn(contents: []const u8, offset: usize) struct { line: usize, col: usize } {
    var line: usize = 1;
    var col: usize = 1;

    for (contents[0..@min(offset, contents.len)]) |char| {
        if (char == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    return .{ .line = line, .col = col };
}

const wasm_allocator = std.heap.wasm_allocator;

pub export fn alloc(size: usize) ?[*]u8 {
    const buf = wasm_allocator.alloc(u8, size) catch return null;
    return buf.ptr;
}

pub export fn free(ptr: [*]u8, size: usize) void {
    wasm_allocator.free(ptr[0..size]);
}

/// returns packed u64: high 32 bits = length, low 32 bits = pointer.
/// returns 0 if parsing failed.
/// caller must free with: free(ptr, len)
pub export fn parse(
    source_bytes: [*]const u8,
    len: u32,
    source_type: u32,
    lang: u32,
) u64 {
    const source: []const u8 = if (len == 0) &[_]u8{} else source_bytes[0..len];

    const st: js.SourceType = if (source_type == 0) .script else .module;
    const l: js.Lang = switch (lang) {
        0 => .js,
        1 => .ts,
        2 => .jsx,
        3 => .tsx,
        4 => .dts,
        else => .js,
    };

    const options = js.Options{
        .source_type = st,
        .lang = l,
    };

    var parse_tree = js.parse(wasm_allocator, source, options) catch {
        return 0;
    };
    defer parse_tree.deinit();

    const json_str = js.estree.toJSON(&parse_tree, wasm_allocator, .{ .pretty = false }) catch {
        return 0;
    };

    const ptr: u64 = @intFromPtr(json_str.ptr);
    const json_len: u64 = json_str.len;

    return (json_len << 32) | ptr;
}
