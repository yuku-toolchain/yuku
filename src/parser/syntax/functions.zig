const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const expressions = @import("expressions.zig");
const class = @import("class.zig");
const ts = @import("ts/types.zig");

const ParseFunctionOptions = struct {
    is_async: bool = false,
    is_expression: bool = false,
    is_declare: bool = false,
    // `export default function` may omit the name yet stays a declaration
    is_default_export: bool = false,
};

pub fn parseFunction(
    parser: *Parser,
    options: ParseFunctionOptions,
    start_from_param: ?u32,
) Error!?ast.NodeIndex {
    std.debug.assert(start_from_param != null or parser.at(.function));

    const start = start_from_param orelse parser.current_token.span.start;
    if (!try parser.expect(.function, "Expected 'function'", null)) return null;

    const is_function_expression = options.is_expression and !options.is_default_export;
    const is_generator = try parser.eat(.star) orelse return null;

    const outer = parser.context;
    defer parser.leaveFunction(outer);

    // an expression's name follows its own yield and await rules, a declaration's the outer ones
    if (is_function_expression) {
        parser.context.yield = is_generator;
        parser.context.await = options.is_async;
    }

    const id = if (parser.current_token.tag.isIdentifierLike())
        try literals.parseBindingIdentifier(parser) orelse .null
    else
        .null;

    // `function* yield(){}` is fine but `function* f(yield){}` is not
    parser.context.yield = is_generator;
    parser.context.await = options.is_async;

    if (!options.is_expression and !options.is_default_export and id == .null) {
        try parser.report(
            parser.current_token.span,
            "A function declaration without the 'default' modifier must have a name",
            .{ .help = "Add a name after 'function'" },
        );
    }

    const is_ts = parser.tree.isTs();
    const type_parameters: ast.NodeIndex = if (is_ts) try ts.parseTypeParameters(parser) else .null;
    const params = try parseFormalParameters(parser, .formal_parameters, false) orelse return null;
    const params_end = parser.tree.span(params).end;

    var return_type: ast.NodeIndex = .null;
    var return_type_end: u32 = params_end;
    if (is_ts and parser.at(.colon)) {
        return_type = try ts.parseReturnTypeAnnotation(parser) orelse return null;
        return_type_end = parser.tree.span(return_type).end;
    }

    const is_ambient_declaration = parser.ts_context.ambient and !is_function_expression;
    if (is_ambient_declaration and parser.at(.left_brace)) {
        return parser.fail(
            parser.current_token.span,
            "An implementation cannot be declared in ambient contexts",
            .{ .help = "Remove the function body, or the surrounding 'declare' modifier" },
        );
    }

    const presence: BodyPresence = if (!is_ts or is_function_expression) .required else .optional;
    const body: ast.NodeIndex = if (is_ambient_declaration)
        .null
    else
        try parseFunctionBody(parser, presence) orelse .null;

    const end = if (body != .null)
        parser.tree.span(body).end
    else
        try parser.eatSemicolon(return_type_end) orelse return null;

    const function_type: ast.FunctionType = if (is_function_expression)
        .function_expression
    else if (body == .null)
        .ts_declare_function
    else
        .function_declaration;

    if (parser.context.single_statement) {
        @branchHint(.unlikely);

        if (options.is_async) try parser.report(
            parser.current_token.span,
            "Async functions can only be declared at the top level or inside a block",
            .{},
        );
        if (is_generator) try parser.report(
            .{ .start = start, .end = params_end },
            "Generators can only be declared at the top level or inside a block",
            .{},
        );
    }

    return try parser.tree.addNode(.{ .function = .{
        .type = function_type,
        .id = id,
        .generator = is_generator,
        .async = options.is_async,
        .declare = options.is_declare,
        .params = params,
        .body = body,
        .type_parameters = type_parameters,
        .return_type = return_type,
    } }, .{ .start = start, .end = end });
}

pub const BodyPresence = enum { required, optional };

// `.null` when an optional body is absent
pub inline fn parseFunctionBody(parser: *Parser, presence: BodyPresence) Error!?ast.NodeIndex {
    if (presence == .optional and !parser.at(.left_brace)) return .null;
    return parseBlockFunctionBody(parser);
}

fn parseBlockFunctionBody(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    if (!try parser.expect(.left_brace, "Expected '{' to start the function body", null)) {
        return null;
    }

    const saved_return = parser.context.@"return";
    parser.context.@"return" = true;
    defer parser.context.@"return" = saved_return;

    const body = try @call(.always_inline, Parser.parseBody, .{ parser, .right_brace, .function });
    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close the function body",
        "Check that every '{' in the function has a matching '}'",
    )) return null;
    return try parser.finish(start, .{ .function_body = .{ .body = body } });
}

pub fn parseFormalParameters(
    parser: *Parser,
    kind: ast.FormalParameterKind,
    allow_parameter_properties: bool,
) Error!?ast.NodeIndex {
    const is_ts = parser.tree.isTs();
    const start = parser.current_token.span.start;
    if (!try parser.expect(.left_paren, "Expected '(' to start the parameter list", null)) {
        return null;
    }

    const items = parser.list();
    defer items.deinit();
    var rest: ast.NodeIndex = .null;

    while (!parser.at(.right_paren) and !parser.at(.eof)) {
        const param_start = parser.current_token.span.start;

        const decorators: ast.IndexRange = if (is_ts and parser.at(.at))
            try class.parseDecorators(parser, .empty) orelse return null
        else
            .empty;

        if (parser.at(.spread)) {
            rest = try patterns.parseBindingRestElement(parser) orelse .null;
            if (rest != .null) ts.applyDecoratorsToPattern(parser, rest, decorators);

            if (parser.at(.comma) and rest != .null) {
                const is_trailing = parser.peekAhead().tag == .right_paren;
                // tsc allows `(...rest,)` in ambient contexts only
                const span: ast.Span = .{
                    .start = parser.tree.span(rest).start,
                    .end = parser.current_token.span.end,
                };
                if (!is_trailing or !parser.ts_context.ambient) return parser.fail(
                    span,
                    if (is_trailing)
                        "A rest parameter cannot have a trailing comma"
                    else
                        "A rest parameter must be last in a parameter list",
                    .{ .help = if (is_trailing)
                        "Remove the trailing comma"
                    else
                        "Move the rest parameter to the end" },
                );
            }
        } else {
            const param = try parseFormalParameter(
                parser,
                allow_parameter_properties,
                decorators,
                param_start,
            ) orelse break;
            try items.append(param);
        }

        if (!(try parser.eat(.comma) orelse return null)) break;
    }

    if (!try parser.expect(.right_paren, "Expected ')' to close the parameter list", null)) {
        return null;
    }
    return try parser.finish(start, .{ .formal_parameters = .{
        .items = try items.finish(),
        .rest = rest,
        .kind = kind,
    } });
}

fn parseFormalParameter(
    parser: *Parser,
    allow_parameter_properties: bool,
    decorators: ast.IndexRange,
    start: u32,
) Error!?ast.NodeIndex {
    const is_ts = parser.tree.isTs();

    if (is_ts and parser.at(.this)) return parseThisParameter(parser);

    var is_property = false;
    var accessibility: ast.Accessibility = .none;
    var readonly = false;
    var override = false;
    if (is_ts and allow_parameter_properties) while (isParameterPropertyModifierStart(parser)) {
        const token = parser.current_token;
        try parser.advanceWithoutEscapeCheck() orelse return null;
        try parser.reportIfEscapedKeyword(token);
        switch (token.tag) {
            inline .public, .private, .protected => |tag| {
                accessibility = @field(ast.Accessibility, @tagName(tag));
            },
            .readonly => readonly = true,
            .override => override = true,
            else => unreachable,
        }
        is_property = true;
    };

    var pattern = try patterns.parseBindingPattern(parser) orelse return null;

    if (is_property and patterns.isDestructuringPattern(parser, pattern)) try parser.report(
        parser.tree.span(pattern),
        "A parameter property may not be declared using a binding pattern",
        .{ .help = "Use an identifier, then destructure it in the constructor body" },
    );

    if (is_ts) {
        if (try parser.eat(.question) orelse return null) {
            ts.markPatternOptional(parser, pattern, parser.prev_token_end);
        }
        if (parser.at(.colon)) {
            pattern = try ts.parsePatternTypeAnnotation(parser, pattern) orelse return null;
        }
    }

    if (try parser.eat(.assign) orelse return null) {
        const default_value = try expressions.parseAssignment(parser) orelse return null;
        pattern = try parser.finishAt(parser.tree.span(pattern).start, default_value, .{
            .assignment_pattern = .{ .left = pattern, .right = default_value },
        });
    }

    if (is_property) return try parser.finishAt(start, pattern, .{ .ts_parameter_property = .{
        .decorators = decorators,
        .parameter = pattern,
        .override = override,
        .readonly = readonly,
        .accessibility = accessibility,
    } });

    ts.applyDecoratorsToPattern(parser, pattern, decorators);

    return try parser.tree.addNode(
        .{ .formal_parameter = .{ .pattern = pattern } },
        parser.tree.span(pattern),
    );
}

pub fn checkAccessorArity(parser: *Parser, kind: anytype, params: ast.NodeIndex) Error!bool {
    const arity_expected: u32, const message: []const u8, const help: []const u8 = switch (kind) {
        .get => .{ 0, "A 'get' accessor cannot have parameters", "Remove the parameters" },
        .set => .{
            1,
            "A 'set' accessor must have exactly one parameter",
            "Declare one parameter for the assigned value",
        },
        else => return true,
    };

    const data = parser.tree.data(params).formal_parameters;
    const items = parser.tree.extra(data.items);
    const has_this = items.len > 0 and
        parser.tree.data(parser.tree.data(items[0]).formal_parameter.pattern) == .ts_this_parameter;
    const arity = data.items.len - @intFromBool(has_this);
    if (arity == arity_expected and data.rest == .null) return true;

    try parser.report(parser.tree.span(params), message, .{ .help = help });
    return false;
}

// wrapped in `formal_parameter` so signature walks and the decoder unwrap rule keep working
fn parseThisParameter(parser: *Parser) Error!?ast.NodeIndex {
    const start = try parser.take(.this) orelse return null;
    var end = parser.prev_token_end;

    var type_annotation: ast.NodeIndex = .null;
    if (parser.at(.colon)) {
        type_annotation = try ts.parseTypeAnnotation(parser) orelse return null;
        end = parser.tree.span(type_annotation).end;
    }

    const span: ast.Span = .{ .start = start, .end = end };
    const this_param = try parser.tree.addNode(
        .{ .ts_this_parameter = .{ .type_annotation = type_annotation } },
        span,
    );
    return try parser.tree.addNode(.{ .formal_parameter = .{ .pattern = this_param } }, span);
}

// `constructor(readonly)` names a parameter, not a modifier
fn isParameterPropertyModifierStart(parser: *Parser) bool {
    switch (parser.current_token.tag) {
        .public, .private, .protected, .readonly, .override => {},
        else => return false,
    }
    const next = parser.peekAhead();
    if (next.hasLineTerminatorBefore()) return false;
    return switch (next.tag) {
        .left_bracket, .left_brace, .spread => true,
        else => next.tag.isIdentifierLike(),
    };
}
