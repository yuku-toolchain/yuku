const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");

const grammar = @import("../grammar.zig");
const expressions = @import("expressions.zig");

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
            const argument = try grammar.parseCoverExpression(parser, 2) orelse {
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
            const element = try grammar.parseCoverExpression(parser, 2) orelse {
                parser.scratch_cover.reset(checkpoint);
                return null;
            };
            try parser.scratch_cover.append(parser.allocator(), element);
            end = parser.getSpan(element).end;
        }

        // comma or end
        if (parser.current_token.type == .comma) {
            try parser.advance();
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
            .{ .help = "Add a closing ']' to complete the array." },
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
/// validates that does not contain CoverInitializedName when validate=true.
pub fn coverToExpression(parser: *Parser, cover: ArrayCover, validate: bool) Error!?ast.NodeIndex {
    if (validate) {
        for (cover.elements) |elem| {
            if (ast.isNull(elem)) continue;
            if (!try grammar.validateNoInvalidCoverSyntax(parser, elem)) return null;
        }
    }
    return try parser.addNode(
        .{ .array_expression = .{ .elements = try parser.addExtra(cover.elements) } },
        .{ .start = cover.start, .end = cover.end },
    );
}

/// convert array cover to ArrayPattern.
pub fn coverToPattern(parser: *Parser, cover: ArrayCover, context: grammar.PatternContext) Error!?ast.NodeIndex {
    const elements_range = try parser.addExtra(cover.elements);
    return toArrayPatternImpl(parser, null, elements_range, .{ .start = cover.start, .end = cover.end }, context);
}

/// convert ArrayExpression to ArrayPattern (mutates in-place).
pub fn toArrayPattern(parser: *Parser, expr_node: ast.NodeIndex, elements_range: ast.IndexRange, context: grammar.PatternContext) Error!?ast.NodeIndex {
    return toArrayPatternImpl(parser, expr_node, elements_range, undefined, context);
}

fn toArrayPatternImpl(parser: *Parser, mutate_node: ?ast.NodeIndex, elements_range: ast.IndexRange, span: ast.Span, context: grammar.PatternContext) Error!?ast.NodeIndex {
    const elements = parser.getExtra(elements_range);

    var rest: ast.NodeIndex = ast.null_node;
    var elements_len = elements_range.len;

    for (elements, 0..) |elem, i| {
        if (ast.isNull(elem)) continue;

        const elem_data = parser.getData(elem);
        if (elem_data == .spread_element) {
            if (i != elements_len - 1) {
                try parser.report(parser.getSpan(elem), "Rest element must be the last element", .{
                    .help = "No elements can follow the rest element in a destructuring pattern.",
                });
                return null;
            }

            const pattern = try grammar.expressionToPattern(parser, elem_data.spread_element.argument, context) orelse return null;

            parser.setData(elem, .{ .binding_rest_element = .{ .argument = pattern } });
            rest = elem;
            elements_len = @intCast(i);
            break;
        }

        _ = try grammar.expressionToPattern(parser, elem, context) orelse return null;
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

================
File: src/js/syntax/class.zig
================
const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const token = @import("../token.zig");

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
        super_class = try expressions.parseExpression(parser, 17, .{}) orelse return null;
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
        const key = try expressions.parseExpression(parser, 2, .{}) orelse return .{ .key = null, .computed = true };
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
        .{parser.current_token.lexeme},
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
        value = try expressions.parseExpression(parser, 2, .{}) orelse return null;
        end = parser.getSpan(value).end;
    }

    if (parser.current_token.type == .semicolon) {
        end = parser.current_token.span.end;
        try parser.advance();
    } else if (!statements.canInsertSemicolon(parser) and parser.current_token.type != .right_brace) {
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

================
File: src/js/syntax/expressions.zig
================
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const token = @import("../token.zig");
const std = @import("std");

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

pub fn parseExpression(parser: *Parser, precedence: u5, opts: ParseExpressionOpts) Error!?ast.NodeIndex {
    var left = try parsePrefix(parser, opts) orelse return null;

    while (true) {
        const current_type = parser.current_token.type;
        if (current_type == .eof) break;

        if (current_type == .in and !parser.context.allow_in) break;

        if (parser.current_token.has_line_terminator_before) {
            const left_data = parser.getData(left);

            // yield [no LineTerminator here]
            if (left_data == .yield_expression) {
                break;
            }
        }

        const left_binding_power = parser.current_token.leftBindingPower();
        if (precedence > left_binding_power or left_binding_power == 0) break;

        left = try parseInfix(parser, left_binding_power, left) orelse return null;
    }

    return left;
}

fn parseInfix(parser: *Parser, precedence: u5, left: ast.NodeIndex) Error!?ast.NodeIndex {
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
        .{current.lexeme},
        .{ .help = "This token cannot be used here. Expected an operator, semicolon, or end of expression." },
    );
    return null;
}

fn parsePrefix(parser: *Parser, opts: ParseExpressionOpts) Error!?ast.NodeIndex {
    const token_type = parser.current_token.type;

    if (token_type == .increment or token_type == .decrement) {
        return parseUpdateExpression(parser, true, ast.null_node);
    }

    if (token_type.isUnaryOperator()) {
        return parseUnaryExpression(parser);
    }

    if (token_type == .left_paren) {
        return parseParenthesizedOrArrowFunction(parser, false, null);
    }

    if (token_type == .await and (parser.context.in_async or parser.isModule())) {
        return parseAwaitExpression(parser);
    }

    if (token_type == .yield and parser.context.in_generator) {
        return parseYieldExpression(parser);
    }

    if (token_type == .new) {
        return parseNewExpression(parser);
    }

    if (token_type == .import) {
        return parseImportExpression(parser);
    }

    return parsePrimaryExpression(parser, opts);
}

inline fn parsePrimaryExpression(parser: *Parser, opts: ParseExpressionOpts) Error!?ast.NodeIndex {
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
        .async => parseAsyncFunctionOrArrow(parser),
        else => {
            if (parser.current_token.type.isIdentifierLike()) {
                return parseIdentifierOrArrowFunction(parser);
            }

            if (!opts.optional) {
                const tok = parser.current_token;
                try parser.reportFmt(
                    tok.span,
                    "Unexpected token '{s}'",
                    .{tok.lexeme},
                    .{ .help = "Expected an expression (identifier, literal, array, object, or parenthesized expression)." },
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
fn parseParenthesizedOrArrowFunction(parser: *Parser, is_async: bool, arrow_start: ?u32) Error!?ast.NodeIndex {
    const start = arrow_start orelse parser.current_token.span.start;

    const cover = try parenthesized.parseCover(parser) orelse return null;

    //  [no LineTerminator here] => ConciseBody
    if (parser.current_token.type == .arrow and !parser.current_token.has_line_terminator_before) {
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
fn parseAsyncFunctionOrArrow(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    const async_id = try literals.parseIdentifier(parser); // save as id and consume 'async'

    // async function ...
    if (parser.current_token.type == .function) {
        return functions.parseFunction(parser, .{ .is_expression = true, .is_async = true }, start);
    }

    // async (params) => ...
    if (parser.current_token.type == .left_paren) {
        return parseParenthesizedOrArrowFunction(parser, true, start);
    }

    //  [no LineTerminator here] => ConciseBody
    if (parser.current_token.type == .identifier and !parser.current_token.has_line_terminator_before) {
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

    if (!statements.canInsertSemicolon(parser) and
        parser.current_token.type != .semicolon)
    {
        if (try parseExpression(parser, 2, .{ .optional = true })) |expr| {
            argument = expr;
            end = parser.getSpan(argument).end;
        }
    }

    if (delegate and ast.isNull(argument)) {
        try parser.report(parser.current_token.span, "Expected expression after 'yield*'", .{});
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
    return try parser.addNode(.super, super_token.span);
}

/// `import.meta` or `import(...)`
/// https://tc39.es/ecma262/#prod-ImportCall
/// https://tc39.es/ecma262/#prod-ImportMeta
fn parseImportExpression(parser: *Parser) Error!?ast.NodeIndex {
    const name = try literals.parseIdentifierName(parser);

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
        break :blk try parsePrimaryExpression(parser, .{}) orelse return null;
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
        try parser.advance();
        arguments = try parseArguments(parser) orelse return null;
        const arguments_end = parser.current_token.span.end;

        if (!try parser.expect(.right_paren, "Expected ')' after constructor arguments", "Constructor calls must end with ')'.")) {
            return null;
        }

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

fn parseBinaryExpression(parser: *Parser, precedence: u5, left: ast.NodeIndex) Error!?ast.NodeIndex {
    const operator_token = parser.current_token;
    const operator = ast.BinaryOperator.fromToken(operator_token.type);
    try parser.advance();

    const next_precedence = if (operator == .exponent) precedence else precedence + 1;
    const right = try parseExpression(parser, next_precedence, .{}) orelse return null;

    return try parser.addNode(
        .{ .binary_expression = .{ .left = left, .right = right, .operator = operator } },
        .{ .start = parser.getSpan(left).start, .end = parser.getSpan(right).end },
    );
}

fn parseLogicalExpression(parser: *Parser, precedence: u5, left: ast.NodeIndex) Error!?ast.NodeIndex {
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
fn parseSequenceExpression(parser: *Parser, precedence: u5, left: ast.NodeIndex) Error!?ast.NodeIndex {
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

fn parseAssignmentExpression(parser: *Parser, precedence: u5, left: ast.NodeIndex) Error!?ast.NodeIndex {
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
fn parseConditionalExpression(parser: *Parser, precedence: u5, @"test": ast.NodeIndex) Error!?ast.NodeIndex {
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
    try parser.advance(); // consume '['

    const saved_allow_in = parser.context.allow_in;
    parser.context.allow_in = true;
    const property = try parseExpression(parser, 0, .{}) orelse {
        parser.context.allow_in = saved_allow_in;
        return null;
    };
    parser.context.allow_in = saved_allow_in;

    const end = parser.current_token.span.end; // ']' position
    if (!try parser.expect(.right_bracket, "Expected ']' after computed property", "Computed member access must end with ']'.")) {
        return null;
    }

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
    try parser.advance(); // consume '('

    const args = try parseArguments(parser) orelse return null;

    const end = parser.current_token.span.end; // ')' position
    if (!try parser.expect(.right_paren, "Expected ')' after function arguments", "Function calls must end with ')'. Check for missing commas or unclosed parentheses.")) {
        return null;
    }

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
            const argument = try parseExpression(parser, 2, .{}) orelse {
                parser.context.allow_in = saved_allow_in;
                return null;
            };
            const arg_span = parser.getSpan(argument);
            break :blk try parser.addNode(.{
                .spread_element = .{ .argument = argument },
            }, .{ .start = spread_start, .end = arg_span.end });
        } else try parseExpression(parser, 2, .{}) orelse {
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

    // Continue parsing the chain
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

================
File: src/js/syntax/functions.zig
================
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

================
File: src/js/syntax/literals.zig
================
const ast = @import("../ast.zig");
const lexer = @import("../lexer.zig");
const Token = @import("../token.zig").Token;
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
        const expr = try expressions.parseExpression(parser, 0, .{}) orelse return null;
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

================
File: src/js/syntax/modules.zig
================
const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const token = @import("../token.zig");

const expressions = @import("expressions.zig");
const statements = @import("statements.zig");
const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const functions = @import("functions.zig");
const class = @import("class.zig");
const variables = @import("variables.zig");

pub fn parseImportDeclaration(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance(); // consume 'import'

    // side-effect import: import 'module'
    if (parser.current_token.type == .string_literal) {
        return parseSideEffectImport(parser, start, null);
    }

    // check for import phase: source or defer
    var phase: ?ast.ImportPhase = null;

    if (parser.current_token.type == .source) {
        phase = .source;
        try parser.advance(); // consume 'source'
        return parseSourcePhaseImport(parser, start);
    } else if (parser.current_token.type == .@"defer") {
        phase = .@"defer";
        try parser.advance(); // consume 'defer'
        return parseDeferPhaseImport(parser, start);
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

    const end = try parser.eatSemicolon(parser.getSpan(source).end);

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
    const end = try parser.eatSemicolon(parser.getSpan(source).end);

    return try parser.addNode(.{
        .import_declaration = .{
            .specifiers = ast.IndexRange.empty,
            .source = source,
            .attributes = attributes,
            .phase = phase,
        },
    }, .{ .start = start, .end = end });
}

/// source phase import: import source X from "X"
/// source phase imports must have exactly one ImportDefaultSpecifier
fn parseSourcePhaseImport(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    // parse the default binding
    const default_specifier = try parseImportDefaultSpecifier(parser) orelse return null;

    // store in specifiers array
    const checkpoint = parser.scratch_a.begin();
    try parser.scratch_a.append(parser.allocator(), default_specifier);

    const specifiers = try parser.addExtra(parser.scratch_a.take(checkpoint));

    if (parser.current_token.type != .from) {
        try parser.report(parser.current_token.span, "Expected 'from' after source phase import binding", .{
            .help = "Source phase imports require 'from': import source x from 'module'",
        });
        return null;
    }

    try parser.advance(); // consume 'from'

    const source = try parseModuleSpecifier(parser) orelse return null;

    // source phase imports do not support import attributes

    const end = try parser.eatSemicolon(parser.getSpan(source).end);

    return try parser.addNode(.{
        .import_declaration = .{
            .specifiers = specifiers,
            .source = source,
            .attributes = ast.IndexRange.empty,
            .phase = .source,
        },
    }, .{ .start = start, .end = end });
}

/// defer phase import: import defer * as X from "X"
/// defer phase imports must have exactly one ImportNamespaceSpecifier
fn parseDeferPhaseImport(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    // defer imports require namespace import: * as name
    if (parser.current_token.type != .star) {
        try parser.report(parser.current_token.span, "Expected '*' for defer phase import", .{
            .help = "Defer phase imports must use namespace form: import defer * as name from 'module'",
        });
        return null;
    }

    const ns_specifier = try parseImportNamespaceSpecifier(parser) orelse return null;

    // store in specifiers array
    const checkpoint = parser.scratch_a.begin();
    try parser.scratch_a.append(parser.allocator(), ns_specifier);
    const specifiers = try parser.addExtra(parser.scratch_a.take(checkpoint));

    if (parser.current_token.type != .from) {
        try parser.report(parser.current_token.span, "Expected 'from' after defer phase import binding", .{
            .help = "Defer phase imports require 'from': import defer * as x from 'module'",
        });
        return null;
    }

    try parser.advance();

    const source = try parseModuleSpecifier(parser) orelse return null;

    // defer phase imports do not support import attributes

    const end = try parser.eatSemicolon(parser.getSpan(source).end);

    return try parser.addNode(.{
        .import_declaration = .{
            .specifiers = specifiers,
            .source = source,
            .attributes = ast.IndexRange.empty,
            .phase = .@"defer",
        },
    }, .{ .start = start, .end = end });
}

/// import clause
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

    const expression = try expressions.parseExpression(parser, 2, .{}) orelse return null;
    const end = try parser.eatSemicolon(parser.getSpan(expression).end);

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
    const end = try parser.eatSemicolon(parser.getSpan(id).end);

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
            // async as an identifier, parse as expression
            // forgive me, actually this is cool
            parser.current_token = .{
                .type = .identifier,
                .span = .{ .start = async_start, .end = async_start + 5 },
                .lexeme = "async",
                .has_line_terminator_before = false,
            };

            declaration = try expressions.parseExpression(parser, 2, .{}) orelse return null;
        }
    }

    // export default class [name] {}
    else if (parser.current_token.type == .class) {
        declaration = try class.parseClass(parser, .{ .is_default_export = true }, null) orelse return null;
        is_decl = true;
    }
    // export default expression
    else {
        declaration = try expressions.parseExpression(parser, 2, .{}) orelse return null;
    }

    const decl_span = parser.getSpan(declaration);

    // function/class declarations don't need semicolon
    const end = if (is_decl)
        decl_span.end
    else
        try parser.eatSemicolon(decl_span.end);

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
    const end = try parser.eatSemicolon(parser.getSpan(source).end);

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

    end = try parser.eatSemicolon(end);

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
            declaration = try variables.parseVariableDeclaration(parser) orelse return null;
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
    const source = try expressions.parseExpression(parser, 2, .{}) orelse return null;

    var options: ast.NodeIndex = ast.null_node;

    // check for options argument (only for regular imports, not phase imports)
    if (phase == null and parser.current_token.type == .comma) {
        // allow trailing comma
        try parser.advance(); // consume ','

        if (parser.current_token.type != .right_paren) {
            options = try expressions.parseExpression(parser, 2, .{}) orelse return null;

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

================
File: src/js/syntax/object.zig
================
const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const token = @import("../token.zig");

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
            const argument = try grammar.parseCoverExpression(parser, 2) orelse {
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
            .{ .help = "Add a closing '}' to complete the object." },
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
            key = try grammar.parseCoverExpression(parser, 2) orelse return null;
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
                .{parser.current_token.lexeme},
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
        const value = try grammar.parseCoverExpression(parser, 2) orelse return null;
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
        const default_value = try grammar.parseCoverExpression(parser, 2) orelse return null;

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
/// validates that does not contain CoverInitializedName when validate=true.
pub fn coverToExpression(parser: *Parser, cover: ObjectCover, validate: bool) Error!?ast.NodeIndex {
    const object_expression = try parser.addNode(
        .{ .object_expression = .{ .properties = try parser.addExtra(cover.properties) } },
        .{ .start = cover.start, .end = cover.end },
    );

    if (validate and !try grammar.validateNoInvalidCoverSyntax(parser, object_expression)) {
        return null;
    }

    return object_expression;
}

/// convert object cover to ObjectPattern.
pub fn coverToPattern(parser: *Parser, cover: ObjectCover, context: grammar.PatternContext) Error!?ast.NodeIndex {
    const properties_range = try parser.addExtra(cover.properties);
    return toObjectPatternImpl(parser, null, properties_range, .{ .start = cover.start, .end = cover.end }, context);
}

/// convert ObjectExpression to ObjectPattern.
pub fn toObjectPattern(parser: *Parser, expr_node: ast.NodeIndex, properties_range: ast.IndexRange, context: grammar.PatternContext) Error!?ast.NodeIndex {
    return toObjectPatternImpl(parser, expr_node, properties_range, undefined, context);
}

fn toObjectPatternImpl(parser: *Parser, mutate_node: ?ast.NodeIndex, properties_range: ast.IndexRange, span: ast.Span, context: grammar.PatternContext) Error!?ast.NodeIndex {
    const properties = parser.getExtra(properties_range);

    var rest: ast.NodeIndex = ast.null_node;
    var properties_len = properties_range.len;

    for (properties, 0..) |prop, i| {
        const prop_data = parser.getData(prop);

        if (prop_data == .spread_element) {
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
                    .help = "Object rest patterns only accept simple identifiers, not nested patterns.",
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

        const value_pattern = try grammar.expressionToPattern(parser, obj_prop.value, context) orelse return null;

        parser.setData(prop, .{ .binding_property = .{
            .key = obj_prop.key,
            .value = value_pattern,
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

================
File: src/js/syntax/parenthesized.zig
================
const std = @import("std");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");
const token = @import("../token.zig");

const grammar = @import("../grammar.zig");
const functions = @import("functions.zig");
const expressions = @import("expressions.zig");
const array = @import("array.zig");
const object = @import("object.zig");

/// cover grammar result for parenthesized expressions and arrow parameters.
/// https://tc39.es/ecma262/#prod-CoverParenthesizedExpressionAndArrowParameterList
pub const ParenthesizedCover = struct {
    /// parsed elements (expressions that may become parameters)
    elements: []const ast.NodeIndex,
    /// rest element if any (for arrow function rest param)
    rest: ast.NodeIndex,
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

    var rest: ast.NodeIndex = ast.null_node;
    var end = start + 1;
    var has_trailing_comma = false;

    // empty parens: ()
    if (parser.current_token.type == .right_paren) {
        end = parser.current_token.span.end;
        try parser.advance();
        return .{
            .elements = parser.scratch_cover.take(checkpoint),
            .rest = rest,
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

            const argument = try grammar.parseCoverExpression(parser, 2) orelse {
                parser.scratch_cover.reset(checkpoint);
                return null;
            };
            const spread_end = parser.getSpan(argument).end;

            // for now, store as spread_element; will convert to rest param for arrow functions
            rest = try parser.addNode(
                .{ .spread_element = .{ .argument = argument } },
                .{ .start = spread_start, .end = spread_end },
            );
            end = spread_end;

            // rest must be last (can have trailing comma in some cases)
            if (parser.current_token.type == .comma) {
                try parser.advance();
                has_trailing_comma = true;
            }
            break;
        }

        // regular element
        const element = try grammar.parseCoverExpression(parser, 2) orelse {
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
            .{ .help = "Add a closing ')' to complete the expression." },
        );
        parser.scratch_cover.reset(checkpoint);
        return null;
    }

    end = parser.current_token.span.end;
    try parser.advance(); // consume )

    return .{
        .elements = parser.scratch_cover.take(checkpoint),
        .rest = rest,
        .start = start,
        .end = end,
        .has_trailing_comma = has_trailing_comma,
    };
}

/// convert cover to ParenthesizedExpression.
pub fn coverToExpression(parser: *Parser, cover: ParenthesizedCover) Error!?ast.NodeIndex {
    // empty parens () without arrow is invalid
    if (cover.elements.len == 0 and ast.isNull(cover.rest)) {
        try parser.report(
            .{ .start = cover.start, .end = cover.end },
            "Empty parentheses are only valid as arrow function parameters",
            .{ .help = "Use '() => ...' for arrow functions or add an expression inside." },
        );
        return null;
    }

    // rest element without arrow is a spread, which is invalid in parenthesized expr
    if (!ast.isNull(cover.rest)) {
        try parser.report(
            parser.getSpan(cover.rest),
            "Rest element is not allowed in parenthesized expression",
            .{ .help = "Spread in parentheses is only valid for arrow function parameters." },
        );
        return null;
    }

    // trailing comma is invalid in parenthesized expressions
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
        if (!try grammar.validateNoInvalidCoverSyntax(parser, elem)) {
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
    const expr = try expressions.parseExpression(parser, 2, .{}) orelse return null;
    return .{ .body = expr, .is_expression = true };
}

fn convertToFormalParameters(parser: *Parser, cover: ParenthesizedCover) Error!?ast.NodeIndex {
    const checkpoint = parser.scratch_cover.begin();
    errdefer parser.scratch_cover.reset(checkpoint);

    for (cover.elements) |elem| {
        const param = try convertToFormalParameter(parser, elem) orelse {
            parser.scratch_cover.reset(checkpoint);
            return null;
        };
        try parser.scratch_cover.append(parser.allocator(), param);
    }

    // handle rest parameter
    var rest: ast.NodeIndex = ast.null_node;

    if (!ast.isNull(cover.rest)) {
        const spread_data = parser.getData(cover.rest).spread_element;
        const pattern = try grammar.expressionToPattern(parser, spread_data.argument, .binding) orelse {
            parser.scratch_cover.reset(checkpoint);
            return null;
        };
        parser.setData(cover.rest, .{ .binding_rest_element = .{ .argument = pattern } });
        rest = cover.rest;
    }

    const items = try parser.addExtra(parser.scratch_cover.take(checkpoint));

    return try parser.addNode(
        .{ .formal_parameters = .{ .items = items, .rest = rest, .kind = .arrow_formal_parameters } },
        .{ .start = cover.start, .end = cover.end },
    );
}

fn convertToFormalParameter(parser: *Parser, expr: ast.NodeIndex) Error!?ast.NodeIndex {
    // convert expression to binding pattern
    const pattern = try grammar.expressionToPattern(parser, expr, .binding) orelse return null;

    return try parser.addNode(
        .{ .formal_parameter = .{ .pattern = pattern } },
        parser.getSpan(expr),
    );
}
pub fn unwrapParenthesized(parser: *Parser, node: ast.NodeIndex) ast.NodeIndex {
    const data = parser.getData(node);

    if (data == .parenthesized_expression) {
        return unwrapParenthesized(parser, data.parenthesized_expression.expression);
    }

    return node;
}

================
File: src/js/syntax/patterns.zig
================
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const ast = @import("../ast.zig");

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
                .{parser.current_token.lexeme},
                .{ .help = "Expected an identifier, array pattern ([a, b]), or object pattern ({a, b})." },
            );
            return null;
        },
    };
}

pub inline fn parseBindingIdentifier(parser: *Parser) Error!?ast.NodeIndex {
    if (!parser.current_token.type.isIdentifierLike()) {
        try parser.reportFmt(
            parser.current_token.span,
            "Expected identifier, found '{s}'",
            .{parser.current_token.lexeme},
            .{ .help = "A variable name must be a valid JavaScript identifier." },
        );
        return null;
    }

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
    const right = try expressions.parseExpression(parser, 2, .{}) orelse return null;

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

================
File: src/js/syntax/statements.zig
================
const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const expressions = @import("expressions.zig");
const variables = @import("variables.zig");
const parenthesized = @import("parenthesized.zig");
const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const functions = @import("functions.zig");
const class = @import("class.zig");
const grammar = @import("../grammar.zig");
const modules = @import("modules.zig");

pub fn parseStatement(parser: *Parser) Error!?ast.NodeIndex {
    return switch (parser.current_token.type) {
        .@"var", .@"const", .let, .using => variables.parseVariableDeclaration(parser),
        .function => functions.parseFunction(parser, .{}, null),
        .class => class.parseClass(parser, .{}, null),
        .async => blk: {
            const start = parser.current_token.span.start;
            try parser.advance(); // consume 'async'
            break :blk try functions.parseFunction(parser, .{ .is_async = true }, start);
        },
        .declare => blk: {
            if (!parser.isTs()) {
                break :blk try parseExpressionStatementOrLabeledOrDirective(parser);
            }
            const start = parser.current_token.span.start;
            try parser.advance(); // consume 'declare'
            break :blk try functions.parseFunction(parser, .{ .is_declare = true }, start);
        },
        .import => blk: {
            if (!parser.isModule()) {
                try parser.report(parser.current_token.span, "'import' statement is only valid in module mode", .{
                    .help = "Use dynamic import() for script mode",
                });
                break :blk null;
            }

            break :blk modules.parseImportDeclaration(parser);
        },
        .@"export" => blk: {
            if (!parser.isModule()) {
                try parser.report(parser.current_token.span, "'export' statement is only valid in module mode", .{
                    .help = "Export declarations can only appear in module mode",
                });
                break :blk null;
            }

            break :blk modules.parseExportDeclaration(parser);
        },
        .left_brace => parseBlockStatement(parser),
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
}

fn parseExpressionStatementOrLabeledOrDirective(parser: *Parser) Error!?ast.NodeIndex {
    const expression = try expressions.parseExpression(parser, 0, .{}) orelse return null;
    const expression_span = parser.getSpan(expression);
    const expression_data = parser.getData(expression);

    // labeled statement: identifier ':'
    if (expression_data == .identifier_reference and parser.current_token.type == .colon) {
        return parseLabeledStatementRest(parser, expression);
    }

    if (parser.current_token.type != .semicolon and !canInsertSemicolon(parser)) {
        try parser.report(.{ .start = expression_span.end, .end = expression_span.end }, "Expected a semicolon or an implicit semicolon after a statement, but found none", .{ .help = "Try inserting a semicolon here" });
        return null;
    }

    const start = expression_span.start;
    const end = try parser.eatSemicolon(expression_span.end);

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
        }, .{ .start = start, .end = end });
    }

    return try parser.addNode(
        .{ .expression_statement = .{ .expression = expression } },
        .{ .start = start, .end = end },
    );
}

/// https://tc39.es/ecma262/#sec-labelled-statements
fn parseLabeledStatementRest(parser: *Parser, identifier: ast.NodeIndex) Error!?ast.NodeIndex {
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

    const body = try parseStatement(parser) orelse {
        try parser.report(parser.current_token.span, "Expected statement after label", .{});
        return null;
    };

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

    const discriminant = try expressions.parseExpression(parser, 0, .{}) orelse return null;

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
        test_expr = try expressions.parseExpression(parser, 0, .{}) orelse return null;
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
        if (try parseStatement(parser)) |stmt| {
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

    const test_expr = try expressions.parseExpression(parser, 0, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after if condition", null)) return null;

    const consequent = try parseStatement(parser) orelse return null;

    var end = parser.getSpan(consequent).end;
    var alternate: ast.NodeIndex = ast.null_node;

    if (parser.current_token.type == .@"else") {
        try parser.advance(); // consume 'else'
        alternate = try parseStatement(parser) orelse return null;
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

    const test_expr = try expressions.parseExpression(parser, 0, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after while condition", null)) return null;

    const body = try parseStatement(parser) orelse return null;

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

    const body = try parseStatement(parser) orelse return null;

    if (!try parser.expect(.@"while", "Expected 'while' after do statement body", null)) return null;
    if (!try parser.expect(.left_paren, "Expected '(' after 'while'", null)) return null;

    const test_expr = try expressions.parseExpression(parser, 0, .{}) orelse return null;

    const rparen_end = parser.current_token.span.end;
    if (!try parser.expect(.right_paren, "Expected ')' after while condition", null)) return null;

    const end = try parser.eatSemicolon(rparen_end);

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

    const object = try expressions.parseExpression(parser, 0, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after with expression", null)) return null;

    const body = try parseStatement(parser) orelse return null;

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
    if (!parser.current_token.has_line_terminator_before and parser.current_token.type.isIdentifierLike()) {
        const label_node = try literals.parseLabelIdentifier(parser) orelse return null;
        label = label_node;
        end = parser.getSpan(label_node).end;
    }

    end = try parser.eatSemicolon(end);

    return try parser.addNode(.{ .break_statement = .{ .label = label } }, .{ .start = start, .end = end });
}

/// https://tc39.es/ecma262/#sec-continue-statement
fn parseContinueStatement(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;
    try parser.advance(); // consume 'continue'

    var label: ast.NodeIndex = ast.null_node;

    // continue [no LineTerminator here] LabelIdentifier;
    if (!parser.current_token.has_line_terminator_before and parser.current_token.type.isIdentifierLike()) {
        const label_node = try literals.parseLabelIdentifier(parser) orelse return null;
        label = label_node;
        end = parser.getSpan(label_node).end;
    }

    end = try parser.eatSemicolon(end);

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
    const expr = try expressions.parseExpression(parser, 0, .{}) orelse {
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

        const pattern = try grammar.expressionToPattern(parser, expr, .assignable) orelse return null;

        return parseForInStatementRest(parser, start, pattern);
    }

    if (parser.current_token.type == .of) {
        // for (expr of ...)
        const pattern = try grammar.expressionToPattern(parser, expr, .assignable) orelse return null;
        return parseForOfStatementRest(parser, start, pattern, is_await);
    }

    // regular for statement
    return parseForStatementRest(parser, start, expr);
}

/// parse rest of regular for statement after init
fn parseForStatementRest(parser: *Parser, start: u32, init: ast.NodeIndex) Error!?ast.NodeIndex {
    if (!try parser.expect(.semicolon, "Expected ';' after for-loop init", null)) return null;

    var test_expr: ast.NodeIndex = ast.null_node;

    if (parser.current_token.type != .semicolon) {
        test_expr = try expressions.parseExpression(parser, 0, .{}) orelse return null;
    }

    if (!try parser.expect(.semicolon, "Expected ';' after for-loop condition", null)) return null;

    var update: ast.NodeIndex = ast.null_node;

    if (parser.current_token.type != .right_paren) {
        update = try expressions.parseExpression(parser, 0, .{}) orelse return null;
    }

    if (!try parser.expect(.right_paren, "Expected ')' after for-loop update", null)) return null;

    const body = try parseStatement(parser) orelse return null;

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

    const right = try expressions.parseExpression(parser, 0, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after for-in expression", null)) return null;

    const body = try parseStatement(parser) orelse return null;

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
    const right = try expressions.parseExpression(parser, 2, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after for-of expression", null)) return null;

    const body = try parseStatement(parser) orelse return null;

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
    if (!canInsertSemicolon(parser) and parser.current_token.type != .semicolon) {
        argument = try expressions.parseExpression(parser, 0, .{}) orelse return null;
        end = parser.getSpan(argument).end;
    }

    end = try parser.eatSemicolon(end);

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

    const argument = try expressions.parseExpression(parser, 0, .{}) orelse return null;

    const end = try parser.eatSemicolon(parser.getSpan(argument).end);

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
    end = try parser.eatSemicolon(end);
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
        init = try expressions.parseExpression(parser, 2, .{}) orelse return null;
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

pub inline fn canInsertSemicolon(parser: *Parser) bool {
    const current_token = parser.current_token;
    return current_token.type == .eof or current_token.has_line_terminator_before or current_token.type == .right_brace;
}

================
File: src/js/syntax/variables.zig
================
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const expressions = @import("expressions.zig");
const patterns = @import("patterns.zig");

pub fn parseVariableDeclaration(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    const kind = parseVariableKind(parser) orelse return null;

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

    return try parser.addNode(
        .{
            .variable_declaration = .{
                .declarators = try parser.addExtra(parser.scratch_a.take(checkpoint)),
                .kind = kind,
            },
        },
        .{ .start = start, .end = try parser.eatSemicolon(end) },
    );
}

inline fn parseVariableKind(parser: *Parser) ?ast.VariableKind {
    const token_type = parser.current_token.type;
    parser.advance() catch return null;

    return switch (token_type) {
        .let => .let,
        .@"const" => .@"const",
        .@"var" => .@"var",
        .using => .using,

        .await => if (parser.current_token.type == .using) blk: {
            parser.advance() catch return null;
            break :blk .await_using;
        } else null,

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
        init = try expressions.parseExpression(parser, 2, .{}) orelse return null;
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

================
File: src/js/grammar.zig
================
const Parser = @import("parser.zig").Parser;
const Error = @import("parser.zig").Error;
const ast = @import("ast.zig");
const std = @import("std");

const object = @import("syntax/object.zig");
const expressions = @import("syntax/expressions.zig");
const array = @import("syntax/array.zig");

/// parse an element within a cover grammar context (used for nested arrays/objects).
/// without validation. validation is deferred until the top-level context is known:
/// - if parent becomes an expression -> validate at top level
/// - if parent becomes a pattern -> no validation needed
pub inline fn parseCoverExpression(parser: *Parser, precedence: u5) Error!?ast.NodeIndex {
    return expressions.parseExpression(parser, precedence, .{ .enable_validation = false });
}

/// validate that an expression doesn't contain CoverInitializedName.
pub fn validateNoInvalidCoverSyntax(parser: *Parser, expr: ast.NodeIndex) Error!bool {
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

                        if (!try validateNoInvalidCoverSyntax(parser, obj_prop.value)) {
                            return false;
                        }
                    },
                    .spread_element => |spread| {
                        if (!try validateNoInvalidCoverSyntax(parser, spread.argument)) {
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
                if (!try validateNoInvalidCoverSyntax(parser, elem)) {
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
            return validateNoInvalidCoverSyntax(parser, spread.argument);
        },
        .parenthesized_expression => |paren| {
            return validateNoInvalidCoverSyntax(parser, paren.expression);
        },
        .sequence_expression => |seq| {
            for (parser.getExtra(seq.expressions)) |e| {
                if (!try validateNoInvalidCoverSyntax(parser, e)) return false;
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

/// convert an expression node to a destructuring pattern.
/// the context determines what syntax is allowed.
pub fn expressionToPattern(
    parser: *Parser,
    expr: ast.NodeIndex,
    context: PatternContext,
) Error!?ast.NodeIndex {
    const data = parser.getData(expr);

    switch (data) {
        .identifier_reference => |id| {
            parser.setData(expr, .{ .binding_identifier = .{
                .name_start = id.name_start,
                .name_len = id.name_len,
            } });
            return expr;
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

            const left_pattern = try expressionToPattern(parser, assign.left, context) orelse return null;

            parser.setData(expr, .{ .assignment_pattern = .{
                .left = left_pattern,
                .right = assign.right,
            } });
            return expr;
        },

        .array_expression => |arr| {
            return array.toArrayPattern(parser, expr, arr.elements, context);
        },

        .object_expression => |obj| {
            return object.toObjectPattern(parser, expr, obj.properties, context);
        },

        .member_expression => |member| {
            if (member.optional) {
                try parser.report(
                    parser.getSpan(expr),
                    "Optional chaining is not allowed in destructuring pattern",
                    .{ .help = "Optional chaining ('?.') cannot be used as an assignment target in destructuring patterns." },
                );
                return null;
            }

            if (context != .assignable) {
                try parser.report(
                    parser.getSpan(expr),
                    "Member expression is not allowed in binding pattern",
                    .{ .help = "Function parameters and variable declarations can only bind to identifiers, not member expressions like 'obj.prop' or 'obj[key]'. Use a simple identifier instead." },
                );
                return null;
            }

            return expr;
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

            if (!expressions.isSimpleAssignmentTarget(parser, paren.expression)) {
                try parser.report(
                    parser.getSpan(paren.expression),
                    "Parenthesized expression in destructuring pattern must be a simple assignment target",
                    .{ .help = "Only identifiers or member expressions (without optional chaining) are allowed inside parentheses in destructuring patterns." },
                );
                return null;
            }

            const inner_pattern = try expressionToPattern(parser, paren.expression, context) orelse return null;

            parser.setData(expr, parser.getData(inner_pattern));
            parser.setSpan(expr, parser.getSpan(inner_pattern));

            return inner_pattern;
        },

        .binding_identifier, .array_pattern, .object_pattern, .assignment_pattern => {
            return expr;
        },

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

================
File: src/js/parser.zig
================
const std = @import("std");
const token = @import("token.zig");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");

const expressions = @import("syntax/expressions.zig");
const literals = @import("syntax/literals.zig");
const variables = @import("syntax/variables.zig");
const functions = @import("syntax/functions.zig");
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
    is_strict: bool = false,
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

    pub inline fn hasErrors(self: ParseTree) bool {
        for (self.diagnostics.items) |d| {
            if (d.severity == .@"error") return true;
        }
        return false;
    }

    pub inline fn hasDiagnostics(self: ParseTree) bool {
        return self.diagnostics.items.len > 0;
    }

    pub fn deinit(self: *const ParseTree) void {
        self.arena.deinit();
    }

    pub inline fn getData(self: *const ParseTree, index: ast.NodeIndex) ast.NodeData {
        return self.nodes.items(.data)[index];
    }

    pub inline fn getSpan(self: *const ParseTree, index: ast.NodeIndex) ast.Span {
        return self.nodes.items(.span)[index];
    }

    pub inline fn getExtra(self: *const ParseTree, range: ast.IndexRange) []const ast.NodeIndex {
        return self.extra.items[range.start..][0..range.len];
    }

    pub inline fn getSourceType(self: *const ParseTree) SourceType {
        return self.source_type;
    }

    pub inline fn getLang(self: *const ParseTree) Lang {
        return self.lang;
    }
};

const ParserContext = struct {
    in_async: bool,
    in_generator: bool,
    allow_in: bool,
    in_function: bool,
};

const ParserState = struct {
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

    scratch_statements: ScratchBuffer = .{},
    scratch_cover: ScratchBuffer = .{},

    // multiple scratches to handle multiple extras at the same time
    scratch_a: ScratchBuffer = .{},
    scratch_b: ScratchBuffer = .{},
    //

    context: ParserContext,
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
            .strict_mode = options.is_strict or options.source_type == .module,
            .lexer = undefined,
            .current_token = undefined,
            .context = .{ .in_async = false, .in_generator = false, .allow_in = true, .in_function = false },
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
            if (try statements.parseStatement(self)) |statement| {
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

    pub fn lookAhead(self: *Parser) ?token.Token {
        return self.current_token;
    }

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

    pub fn advance(self: *Parser) Error!void {
        self.current_token = self.lexer.nextToken() catch |e| blk: {
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

    pub inline fn eatSemicolon(self: *Parser, end: u32) Error!u32 {
        // consume optional semicolon and adjust span
        if (self.current_token.type == .semicolon) {
            const semicolon_end = self.current_token.span.end;
            try self.advance();
            return semicolon_end;
        }

        return end;
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

    pub inline fn label(self: *Parser, span: ast.Span, message: []const u8) Label {
        _ = self;
        return .{ .span = span, .message = message };
    }

    pub fn makeLabels(self: *Parser, labels: []const Label) Error![]const Label {
        return try self.allocator().dupe(Label, labels);
    }

    pub fn formatMessage(self: *Parser, comptime format: []const u8, args: anytype) Error![]u8 {
        return try std.fmt.allocPrint(self.allocator(), format, args);
    }

    // this is very basic now
    fn synchronize(self: *Parser, terminator: ?token.TokenType) Error!void {
        try self.advance();

        while (self.current_token.type != .eof) {
            // stop at the block terminator to avoid consuming the closing brace
            if (terminator) |t| {
                if (self.current_token.type == t) return;
            }

            if (self.current_token.type == .semicolon) {
                try self.advance();
                return;
            }

            switch (self.current_token.type) {
                .class, .function, .@"var", .@"for", .@"if", .@"while", .@"return", .let, .@"const", .using => return,
                else => {},
            }

            try self.advance();
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

================
File: src/js/root.zig
================
const parser = @import("parser.zig");

pub const parse = parser.parse;
pub const Parser = parser.Parser;
pub const ParseTree = parser.ParseTree;
pub const Options = parser.Options;
pub const Diagnostic = parser.Diagnostic;
pub const Severity = parser.Severity;
pub const Label = parser.Label;
pub const SourceType = parser.SourceType;
pub const Lang = parser.Lang;

pub const estree = @import("estree.zig");

================
File: src/js/semantic.zig
================
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

================
File: src/util/number.zig
================
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

================
File: src/util/root.zig
================
const std = @import("std");

pub const UnicodeId = @import("unicode_id.zig");
pub const Utf = @import("utf.zig");
pub const Number = @import("number.zig");

================
File: src/util/utf.zig
================
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

/// ECMAScript LineTerminator: LF, CR, LS (U+2028), PS (U+2029)
pub fn lineTerminatorLen(source: []const u8, pos: usize) u8 {
    if (pos >= source.len) return 0;
    const c = source[pos];
    if (c == '\n' or c == '\r') return 1;
    if (c == 0xE2 and pos + 2 < source.len and source[pos + 1] == 0x80) {
        if (source[pos + 2] == 0xA8 or source[pos + 2] == 0xA9) return 3;
    }
    return 0;
}

/// Check if byte at position is start of a line terminator
pub inline fn isLineTerminator(source: []const u8, pos: usize) bool {
    return lineTerminatorLen(source, pos) > 0;
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

================
File: src/main.zig
================
const std = @import("std");
const js = @import("js");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
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
    const tree = try js.parse(std.heap.page_allocator, contents, .{});
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
