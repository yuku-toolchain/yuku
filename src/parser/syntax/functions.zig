const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const TokenTag = @import("../token.zig").TokenTag;

const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const extensions = @import("extensions.zig");
const ts_types = @import("ts/types.zig");

const ParseFunctionOpts = struct {
    is_async: bool = false,
    is_expression: bool = false,
    /// sets the `declare` flag on the resulting `Function`. ambient
    /// policy comes from `parser.context.in_ambient`.
    is_declare: bool = false,
    /// `export default function`: name is optional but the result is
    /// still a `FunctionDeclaration`.
    is_default_export: bool = false,
};

pub fn parseFunction(parser: *Parser, opts: ParseFunctionOpts, start_from_param: ?u32) Error!?ast.NodeIndex {
    const start = start_from_param orelse parser.current_token.span.start;

    if (!try parser.expect(
        .function,
        "Expected 'function' keyword",
        null,
    )) return null;

    const is_function_expression = opts.is_expression and !opts.is_default_export;

    var is_generator = false;

    if (parser.current_token.tag == .star) {
        is_generator = true;
        try parser.advance() orelse return null;
    }

    const outer_yield_is_keyword = parser.context.yield_is_keyword;
    const outer_await_is_keyword = parser.context.await_is_keyword;

    defer {
        parser.context.yield_is_keyword = outer_yield_is_keyword;
        parser.context.await_is_keyword = outer_await_is_keyword;
    }

    // yield rules differ for declarations vs expressions. inside a
    // generator body:
    //   function yield(){}        declaration. invalid.
    //   (function yield(){})      expression. ok.
    //   (function* yield(){})     invalid.
    parser.context.yield_is_keyword = if (is_function_expression)
        is_generator
    else
        outer_yield_is_keyword;

    // await rules also differ. in script code:
    //   async function await(){}     declaration. valid.
    //   (async function await(){})   expression. invalid.
    // declarations inherit the outer Await context. async expressions
    // force Await for their own name.
    parser.context.await_is_keyword = if (is_function_expression)
        opts.is_async
    else
        outer_await_is_keyword;

    const id = if (parser.current_token.tag.isIdentifierLike())
        try literals.parseBindingIdentifier(parser) orelse .null
    else
        .null;

    // params and body run under the function's own generator context.
    // `function* yield(){}` is fine but `function* f(yield){}` is not.
    parser.context.yield_is_keyword = is_generator;
    parser.context.await_is_keyword = opts.is_async;

    // name is required for plain declarations. optional for function
    // expressions and `export default function`.
    if (!opts.is_expression and !opts.is_default_export and id == .null) {
        try parser.report(
            parser.current_token.span,
            "Function declaration requires a name",
            .{ .help = "Add a name after 'function', e.g. 'function myFunc() {}'." },
        );
    }

    // `function f<T, U extends V>(...)`
    const type_parameters: ast.NodeIndex = if (parser.tree.isTs())
        try ts_types.parseTypeParameters(parser)
    else
        .null;

    const is_unique_formal_parameters = is_generator or opts.is_async;

    const params = try parseFormalParameters(parser, if (is_unique_formal_parameters) .unique_formal_parameters else .formal_parameters, false) orelse return null;

    const params_end = parser.tree.getSpan(params).end;

    // optional `: ReturnType` annotation.
    var return_type: ast.NodeIndex = .null;
    var return_type_end: u32 = params_end;

    if (parser.tree.isTs() and parser.current_token.tag == .colon) {
        const annotation = try ts_types.parseReturnTypeAnnotation(parser) orelse return null;
        return_type = annotation;
        return_type_end = parser.tree.getSpan(annotation).end;
    }

    var body: ast.NodeIndex = .null;

    // function expressions always carry a body, only declarations obey ambient rules.
    const is_ambient_declaration = parser.context.in_ambient and !is_function_expression;

    if (is_ambient_declaration and parser.current_token.tag == .left_brace) {
        try parser.report(
            parser.current_token.span,
            "An implementation cannot be declared in ambient contexts",
            .{ .help = "Remove the function body or the surrounding 'declare' modifier" },
        );
        return null;
    }

    // ts ambient declarations and overload signatures are body-less.
    const has_body = !is_ambient_declaration and (!parser.tree.isTs() or
        is_function_expression or parser.current_token.tag == .left_brace);

    if (has_body) {
        body = try parseFunctionBody(parser) orelse .null;
    }

    const end = if (body != .null)
        parser.tree.getSpan(body).end
    else
        try parser.eatSemicolon(return_type_end) orelse return null;

    const function_type: ast.FunctionType = if (is_function_expression)
        .function_expression
    else if (body == .null)
        .ts_declare_function
    else
        .function_declaration;

    if (parser.context.in_single_statement_context) {
        @branchHint(.unlikely);

        if (opts.is_async) {
            try parser.report(parser.current_token.span, "Async functions can only be declared at the top level or inside a block", .{});
        }

        if (is_generator) {
            try parser.report(
                .{ .start = start, .end = params_end },
                "Generators can only be declared at the top level or inside a block",
                .{},
            );
        }
    }

    return try parser.tree.createNode(.{
        .function = .{
            .type = function_type,
            .id = id,
            .generator = is_generator,
            .async = opts.is_async,
            .declare = opts.is_declare,
            .params = params,
            .body = body,
            .type_parameters = type_parameters,
            .return_type = return_type,
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

    const saved_allow_return_statement = parser.context.allow_return_statement;

    parser.context.allow_return_statement = true;

    defer {
        parser.context.allow_return_statement = saved_allow_return_statement;
    }

    const body = try parser.parseBody(.right_brace, .function);

    const end = parser.current_token.span.end;

    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close function body",
        "Add a closing brace '}' to complete the function, or check for unbalanced braces inside.",
    )) return null;

    return try parser.tree.createNode(.{ .function_body = .{ .body = body } }, .{ .start = start, .end = end });
}

/// parses a parenthesised parameter list. `allow_parameter_properties` is
/// set only for class constructor parameters and enables ts parameter
/// property shorthand (`constructor(public x: T)`).
pub fn parseFormalParameters(parser: *Parser, kind: ast.FormalParameterKind, allow_parameter_properties: bool) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    if (!try parser.expect(.left_paren, "Expected '(' to start parameter list", null)) return null;

    const params_checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(params_checkpoint);

    var rest: ast.NodeIndex = .null;

    while (parser.current_token.tag != .right_paren and parser.current_token.tag != .eof) {
        const param_start = parser.current_token.span.start;

        const decorators: ast.IndexRange = if (parser.tree.isTs() and parser.current_token.tag == .at)
            try extensions.parseDecorators(parser) orelse return null
        else
            ast.IndexRange.empty;

        if (parser.current_token.tag == .spread) {
            // decorators on `...rest` are silently dropped
            rest = try patterns.parseBindingRestElement(parser) orelse .null;

            if (parser.current_token.tag == .comma and rest != .null) {
                try parser.report(
                    .{ .start = parser.tree.getSpan(rest).start, .end = parser.current_token.span.end },
                    "Rest parameter must be the last parameter",
                    .{ .help = "Move the '...rest' parameter to the end of the parameter list, or remove trailing parameters." },
                );
                return null;
            }
        } else {
            const param = try parseFormalParameter(parser, allow_parameter_properties, decorators, param_start) orelse break;
            try parser.scratch_a.append(parser.allocator(), param);
        }

        if (parser.current_token.tag != .comma) break;
        try parser.advance() orelse return null;
    }

    const end = parser.current_token.span.end;
    if (!try parser.expect(.right_paren, "Expected ')' to close parameter list", null)) return null;

    return try parser.tree.createNode(.{ .formal_parameters = .{
        .items = try parser.createExtraFromScratch(&parser.scratch_a, params_checkpoint),
        .rest = rest,
        .kind = kind,
    } }, .{ .start = start, .end = end });
}

/// `decorators` and `start` are pre-collected by `parseFormalParameters`
pub fn parseFormalParameter(
    parser: *Parser,
    allow_parameter_properties: bool,
    decorators: ast.IndexRange,
    start: u32,
) Error!?ast.NodeIndex {
    if (parser.tree.isTs() and parser.current_token.tag == .this) {
        return try parseThisParameter(parser);
    }

    var pp_accessibility: ast.Accessibility = .none;
    var pp_readonly = false;
    var pp_override = false;
    var has_pp_modifier = false;

    if (allow_parameter_properties and parser.tree.isTs()) {
        while (try isParameterPropertyModifierStart(parser)) {
            const modifier_token = parser.current_token;
            try parser.advanceWithoutEscapeCheck() orelse return null;
            try parser.reportIfEscapedKeyword(modifier_token);

            switch (modifier_token.tag) {
                .public => pp_accessibility = .public,
                .private => pp_accessibility = .private,
                .protected => pp_accessibility = .protected,
                .readonly => pp_readonly = true,
                .override => pp_override = true,
                else => unreachable,
            }
            has_pp_modifier = true;
        }
    }

    var pattern = try patterns.parseBindingPattern(parser) orelse return null;

    if (parser.tree.isTs() and parser.current_token.tag == .question) {
        const question_end = parser.current_token.span.end;
        try parser.advance() orelse return null;
        ts_types.markPatternOptional(parser, pattern, question_end);
    }

    if (parser.tree.isTs() and parser.current_token.tag == .colon) {
        const annotation = try ts_types.parseTypeAnnotation(parser) orelse return null;
        ts_types.applyTypeAnnotationToPattern(parser, pattern, annotation);
    }

    if (parser.current_token.tag == .assign) {
        pattern = try patterns.parseAssignmentPattern(parser, pattern) orelse return null;
    }

    if (has_pp_modifier) {
        return try parser.tree.createNode(.{ .ts_parameter_property = .{
            .decorators = decorators,
            .parameter = pattern,
            .override = pp_override,
            .readonly = pp_readonly,
            .accessibility = pp_accessibility,
        } }, .{ .start = start, .end = parser.tree.getSpan(pattern).end });
    }

    ts_types.applyDecoratorsToPattern(parser, pattern, decorators);

    return try parser.tree.createNode(.{ .formal_parameter = .{ .pattern = pattern } }, parser.tree.getSpan(pattern));
}

const AccessorSpec = struct { arity: u32, msg: []const u8, help: []const u8 };

/// `true` when valid or `kind` is not `.get`/`.set`. a leading `this:`
/// is a ts type-only parameter and never counts. `anytype` so class
/// `MethodDefinitionKind` and object `PropertyKind` share one helper.
pub fn checkAccessorArity(parser: *Parser, kind: anytype, params: ast.NodeIndex) Error!bool {
    const spec: AccessorSpec = switch (kind) {
        .get => .{ .arity = 0, .msg = "Getter must have no parameters", .help = "Remove all parameters from the getter." },
        .set => .{ .arity = 1, .msg = "Setter must have exactly one parameter", .help = "Setters accept exactly one argument." },
        else => return true,
    };

    const data = parser.tree.getData(params).formal_parameters;
    const items = parser.tree.getExtra(data.items);
    const has_this = items.len > 0 and
        parser.tree.getData(parser.tree.getData(items[0]).formal_parameter.pattern) == .ts_this_parameter;
    const arity = data.items.len - @intFromBool(has_this);
    if (arity == spec.arity and data.rest == .null) return true;

    try parser.report(parser.tree.getSpan(params), spec.msg, .{ .help = spec.help });
    return false;
}

/// parses `this` or `this: Type` as a parameter. emitted inside the
/// regular `FormalParameter` wrapper so signature walks, span tracking,
/// and the decoder's `formal_parameter` unwrap rule keep working. the
/// inner `TSThisParameter` renders in ESTree as an `Identifier` named
/// `this`, matching the @typescript-eslint/typescript-estree convention.
fn parseThisParameter(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var end = parser.current_token.span.end;

    try parser.advance() orelse return null; // consume 'this'

    var type_annotation: ast.NodeIndex = .null;

    if (parser.current_token.tag == .colon) {
        const annotation = try ts_types.parseTypeAnnotation(parser) orelse return null;
        type_annotation = annotation;
        end = parser.tree.getSpan(annotation).end;
    }

    const this_param = try parser.tree.createNode(
        .{ .ts_this_parameter = .{ .type_annotation = type_annotation } },
        .{ .start = start, .end = end },
    );

    return try parser.tree.createNode(
        .{ .formal_parameter = .{ .pattern = this_param } },
        .{ .start = start, .end = end },
    );
}

/// true when the current token is a parameter-property modifier and the
/// next token can start a binding pattern or another modifier. without
/// this lookahead, `constructor(readonly)` would eat `readonly` as a
/// modifier and then fail on the missing name.
fn isParameterPropertyModifierStart(parser: *Parser) Error!bool {
    const tag = parser.current_token.tag;
    if (!isParameterPropertyModifierTag(tag)) return false;
    const next = try parser.peekAhead() orelse return false;
    if (next.hasLineTerminatorBefore()) return false;
    return canFollowParameterPropertyModifier(next.tag);
}

inline fn isParameterPropertyModifierTag(tag: TokenTag) bool {
    return tag == .public or tag == .private or tag == .protected or
        tag == .readonly or tag == .override;
}

/// tokens that can legally follow a parameter property modifier.
inline fn canFollowParameterPropertyModifier(tag: TokenTag) bool {
    return tag.isIdentifierLike() or
        tag == .left_bracket or
        tag == .left_brace or
        tag == .spread;
}
