const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const decorator = @import("decorators.zig");
const ts = @import("ts/types.zig");
const dialect = @import("../dialect.zig");

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
    std.debug.assert(start_from_param != null or parser.current_token.tag == .function);

    const start = start_from_param orelse parser.current_token.span.start;
    if (!try parser.expect(.function, "Expected 'function' keyword", null)) return null;

    const is_function_expression = options.is_expression and !options.is_default_export;
    const is_generator = parser.current_token.tag == .star;
    if (is_generator) try parser.advance() orelse return null;

    const outer_yield_is_keyword = parser.context.yield;
    const outer_await_is_keyword = parser.context.await;
    defer parser.context.yield = outer_yield_is_keyword;
    defer parser.context.await = outer_await_is_keyword;

    // an expression's name follows its own yield and await rules, a declaration's the outer ones
    parser.context.yield = if (is_function_expression) is_generator else outer_yield_is_keyword;
    parser.context.await = if (is_function_expression) options.is_async else outer_await_is_keyword;

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
            "Function declaration requires a name",
            .{ .help = "Add a name after 'function', e.g. 'function myFunc() {}'." },
        );
    }

    const is_ts = parser.tree.isTs();
    const type_parameters: ast.NodeIndex = if (is_ts) try ts.parseTypeParameters(parser) else .null;
    const params = try parseFormalParameters(parser, .formal_parameters, false) orelse return null;
    const params_end = parser.tree.span(params).end;

    var return_type: ast.NodeIndex = .null;
    var return_type_end: u32 = params_end;
    if (is_ts and parser.current_token.tag == .colon) {
        return_type = try ts.parseReturnTypeAnnotation(parser) orelse return null;
        return_type_end = parser.tree.span(return_type).end;
    }

    const is_ambient_declaration = parser.ts_context.ambient and !is_function_expression;
    if (is_ambient_declaration and parser.current_token.tag == .left_brace) {
        try parser.report(
            parser.current_token.span,
            "An implementation cannot be declared in ambient contexts",
            .{ .help = "Remove the function body or the surrounding 'declare' modifier" },
        );
        return null;
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

        if (options.is_async) {
            try parser.report(
                parser.current_token.span,
                "Async functions can only be declared at the top level or inside a block",
                .{},
            );
        }

        if (is_generator) {
            try parser.report(
                .{ .start = start, .end = params_end },
                "Generators can only be declared at the top level or inside a block",
                .{},
            );
        }
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
    if (comptime dialect.has(.function_body)) {
        if (try parseDialectFunctionBody(parser)) |body| return body;
    }

    if (presence == .optional and parser.current_token.tag != .left_brace) return .null;
    return parseBlockFunctionBody(parser);
}

fn parseDialectFunctionBody(parser: *Parser) Error!??ast.NodeIndex {
    const saved = parser.context;
    parser.context.@"return" = true;
    parser.context.directive_prologue = true;
    defer parser.context.@"return" = saved.@"return";
    defer parser.context.directive_prologue = saved.directive_prologue;

    return dialect.at(.function_body, parser, .{});
}

fn parseBlockFunctionBody(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    if (!try parser.expect(
        .left_brace,
        "Expected '{' to start function body",
        "Function bodies must be enclosed in braces: function name() { ... }",
    )) return null;

    const saved_return = parser.context.@"return";
    parser.context.@"return" = true;
    defer parser.context.@"return" = saved_return;

    const body = try parser.parseBody(.right_brace, .function);
    const end = parser.current_token.span.end;
    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close function body",
        "Add a closing brace '}' to complete the function, or check for unbalanced braces inside.",
    )) return null;

    return try parser.tree.addNode(
        .{ .function_body = .{ .body = body } },
        .{ .start = start, .end = end },
    );
}

/// Parses a parenthesized parameter list.
/// `allow_parameter_properties` enables the constructor-only `constructor(public x: T)` shorthand.
pub fn parseFormalParameters(
    parser: *Parser,
    kind: ast.FormalParameterKind,
    allow_parameter_properties: bool,
) Error!?ast.NodeIndex {
    const is_ts = parser.tree.isTs();
    const start = parser.current_token.span.start;
    if (!try parser.expect(.left_paren, "Expected '(' to start parameter list", null)) return null;

    const params_checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(params_checkpoint);

    var rest: ast.NodeIndex = .null;

    while (parser.current_token.tag != .right_paren and parser.current_token.tag != .eof) {
        const param_start = parser.current_token.span.start;

        const decorators: ast.IndexRange = if (is_ts and parser.current_token.tag == .at)
            try decorator.parseDecorators(parser) orelse return null
        else
            .empty;

        if (parser.current_token.tag == .spread) {
            rest = try patterns.parseBindingRestElement(parser) orelse .null;
            if (rest != .null) ts.applyDecoratorsToPattern(parser, rest, decorators);

            if (parser.current_token.tag == .comma and rest != .null) {
                const span: ast.Span = .{
                    .start = parser.tree.span(rest).start,
                    .end = parser.current_token.span.end,
                };
                if (parser.peekAhead().tag == .right_paren) {
                    // tsc allows `(...rest,)` in ambient contexts only
                    if (!parser.ts_context.ambient) {
                        try parser.report(
                            span,
                            "Rest parameter may not have a trailing comma",
                            .{ .help = "Remove the trailing comma after the rest parameter." },
                        );
                        return null;
                    }
                } else {
                    try parser.report(
                        span,
                        "Rest parameter must be the last parameter",
                        .{ .help = "Move the '...rest' parameter to the end of the parameter" ++
                            " list, or remove trailing parameters." },
                    );
                    return null;
                }
            }
        } else {
            const param = try parseFormalParameter(
                parser,
                allow_parameter_properties,
                decorators,
                param_start,
            ) orelse break;
            try parser.scratch_a.append(parser.allocator(), param);
        }

        if (parser.current_token.tag != .comma) break;
        try parser.advance() orelse return null;
    }

    const end = parser.current_token.span.end;
    if (!try parser.expect(.right_paren, "Expected ')' to close parameter list", null)) return null;

    return try parser.tree.addNode(.{ .formal_parameters = .{
        .items = try parser.flushToExtras(&parser.scratch_a, params_checkpoint),
        .rest = rest,
        .kind = kind,
    } }, .{ .start = start, .end = end });
}

const ParameterPropertyModifiers = struct {
    accessibility: ast.Accessibility = .none,
    readonly: bool = false,
    override: bool = false,
    present: bool = false,
};

/// Parses one parameter after the caller has collected its `decorators` and `start`.
fn parseFormalParameter(
    parser: *Parser,
    allow_parameter_properties: bool,
    decorators: ast.IndexRange,
    start: u32,
) Error!?ast.NodeIndex {
    const is_ts = parser.tree.isTs();

    if (is_ts and parser.current_token.tag == .this) return parseThisParameter(parser);

    const modifiers = if (is_ts and allow_parameter_properties)
        try parseParameterPropertyModifiers(parser) orelse return null
    else
        ParameterPropertyModifiers{};

    var pattern = try patterns.parseBindingPattern(parser) orelse return null;

    if (modifiers.present and patterns.isDestructuringPattern(parser, pattern)) {
        try parser.report(
            parser.tree.span(pattern),
            "A parameter property may not be declared using a binding pattern",
            .{ .help = "Name the parameter, then destructure it inside the constructor body." },
        );
    }

    if (is_ts) {
        if (parser.current_token.tag == .question) {
            const question_end = parser.current_token.span.end;
            try parser.advance() orelse return null;
            ts.markPatternOptional(parser, pattern, question_end);
        }
        if (parser.current_token.tag == .colon) {
            pattern = try ts.parsePatternTypeAnnotation(parser, pattern) orelse return null;
        }
    }

    if (parser.current_token.tag == .assign) {
        pattern = try patterns.parseAssignmentPattern(parser, pattern) orelse return null;
    }

    if (modifiers.present) {
        return try parser.tree.addNode(.{ .ts_parameter_property = .{
            .decorators = decorators,
            .parameter = pattern,
            .override = modifiers.override,
            .readonly = modifiers.readonly,
            .accessibility = modifiers.accessibility,
        } }, .{ .start = start, .end = parser.tree.span(pattern).end });
    }

    ts.applyDecoratorsToPattern(parser, pattern, decorators);

    return try parser.tree.addNode(
        .{ .formal_parameter = .{ .pattern = pattern } },
        parser.tree.span(pattern),
    );
}

fn parseParameterPropertyModifiers(parser: *Parser) Error!?ParameterPropertyModifiers {
    var modifiers: ParameterPropertyModifiers = .{};
    while (isParameterPropertyModifierStart(parser)) {
        const token = parser.current_token;
        try parser.advanceWithoutEscapeCheck() orelse return null;
        try parser.reportIfEscapedKeyword(token);
        switch (token.tag) {
            .public => modifiers.accessibility = .public,
            .private => modifiers.accessibility = .private,
            .protected => modifiers.accessibility = .protected,
            .readonly => modifiers.readonly = true,
            .override => modifiers.override = true,
            else => unreachable,
        }
        modifiers.present = true;
    }
    return modifiers;
}

/// Returns whether `params` has the arity an accessor of `kind` requires, ignoring a leading
/// `this` parameter. Always true when `kind` is not an accessor.
pub fn checkAccessorArity(parser: *Parser, kind: anytype, params: ast.NodeIndex) Error!bool {
    const arity_expected: u32, const message: []const u8, const help: []const u8 = switch (kind) {
        .get => .{ 0, "Getter must have no parameters", "Remove all parameters from the getter." },
        .set => .{
            1,
            "Setter must have exactly one parameter",
            "Setters accept exactly one argument.",
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
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;
    try parser.advance() orelse return null;

    var type_annotation: ast.NodeIndex = .null;
    if (parser.current_token.tag == .colon) {
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
