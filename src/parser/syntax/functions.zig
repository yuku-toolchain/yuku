const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const TokenTag = @import("../token.zig").TokenTag;

const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const ts_types = @import("ts/types.zig");

const ParseFunctionOpts = struct {
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

    // names use different yield-keyword rules for declarations vs expressions.
    // inside a generator body:
    // - `function yield(){}` (declaration) is invalid.
    // - `(function yield(){})` (expression) is ok.
    // - `(function* yield(){})` is invalid.
    parser.context.yield_is_keyword = switch (function_type) {
        .function_expression => is_generator,
        else => outer_yield_is_keyword,
    };

    // names use different await-keyword rules for declarations vs expressions.
    // in script code:
    // - `async function await(){}` (declaration) is valid.
    // - `(async function await(){})` (expression) is invalid.
    // declarations inherit outer Await context, while async expressions force Await for their name.
    parser.context.await_is_keyword = switch (function_type) {
        .function_expression => opts.is_async,
        else => outer_await_is_keyword,
    };

    const id = if (parser.current_token.tag.isIdentifierLike())
        try literals.parseBindingIdentifier(parser) orelse .null
    else
        .null;

    // params/body are validated in the function's generator context.
    // example: `function* yield(){}` ok, but `function* f(yield){}` is not.
    parser.context.yield_is_keyword = is_generator;
    parser.context.await_is_keyword = opts.is_async;

    // name is required for regular function declarations, but optional for:
    // - function expressions
    // - export default function
    if (!opts.is_expression and !opts.is_default_export and id == .null) {
        try parser.report(
            parser.current_token.span,
            "Function declaration requires a name",
            .{ .help = "Add a name after 'function', e.g. 'function myFunc() {}'." },
        );
    }

    if (!try parser.expect(
        .left_paren,
        "Expected '(' to start parameter list",
        "Function parameters must be enclosed in parentheses: function name(a, b) {}",
    )) return null;

    const is_unique_formal_parameters = is_generator or opts.is_async;

    const params = try parseFormalParamaters(parser, if (is_unique_formal_parameters) .unique_formal_parameters else .formal_parameters, false) orelse return null;

    const params_end = parser.current_token.span.end; // including )

    if (!try parser.expect(
        .right_paren,
        "Expected ')' to close parameter list",
        "Add a closing parenthesis ')' after the parameters, or check for missing commas between parameters.",
    )) return null;

    // ts return type, `function f(): Type { ... }`
    var return_type: ast.NodeIndex = .null;
    var return_type_end: u32 = params_end;

    if (parser.tree.isTs() and parser.current_token.tag == .colon) {
        const annotation = try ts_types.parseReturnTypeAnnotation(parser) orelse return null;
        return_type = annotation;
        return_type_end = parser.tree.getSpan(annotation).end;
    }

    var body: ast.NodeIndex = .null;

    if (opts.is_declare) {
        if (parser.current_token.tag == .left_brace) {
            try parser.report(
                parser.current_token.span,
                parser.withTsCode("1183", "An implementation cannot be declared in ambient contexts."),
                .{ .help = "Remove the function body or remove the 'declare' modifier" },
            );
            return null;
        }
    } else {
        body = try parseFunctionBody(parser) orelse .null;
    }

    // ambient `declare function f(): T;` takes a trailing ASI semicolon,
    // the regular declaration ends at `}` and signature-only overloads end
    // at the return type.
    const end = if (body != .null)
        parser.tree.getSpan(body).end
    else if (opts.is_declare)
        try parser.eatSemicolon(return_type_end) orelse return null
    else
        return_type_end;

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
            .params = params,
            .body = body,
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
pub fn parseFormalParamaters(parser: *Parser, kind: ast.FormalParameterKind, allow_parameter_properties: bool) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    var end: u32 = parser.current_token.span.end;

    const params_checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(params_checkpoint);

    var rest: ast.NodeIndex = .null;

    while (true) {
        if (parser.current_token.tag == .right_paren or parser.current_token.tag == .eof) break;

        if (parser.current_token.tag == .spread) {
            rest = try patterns.parseBindingRestElement(parser) orelse .null;
            if (rest != .null) {
                end = parser.tree.getSpan(rest).end;
            }

            if (parser.current_token.tag == .comma and rest != .null) {
                try parser.report(
                    .{ .start = parser.tree.getSpan(rest).start, .end = parser.current_token.span.end },
                    "Rest parameter must be the last parameter",
                    .{ .help = "Move the '...rest' parameter to the end of the parameter list, or remove trailing parameters." },
                );

                return null;
            }
        } else {
            const param = try parseFormalParamater(parser, allow_parameter_properties) orelse break;

            end = parser.tree.getSpan(param).end;

            try parser.scratch_a.append(parser.allocator(), param);
        }

        if (parser.current_token.tag == .comma) {
            try parser.advance() orelse return null;
        } else break;
    }

    return try parser.tree.createNode(.{ .formal_parameters = .{
        .items = try parser.createExtraFromScratch(&parser.scratch_a, params_checkpoint),
        .rest = rest,
        .kind = kind,
    } }, .{ .start = start, .end = end });
}

pub fn parseFormalParamater(parser: *Parser, allow_parameter_properties: bool) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;

    // `function f(this: T, ...)` and related signature forms. `this` is
    // never a real binding, so it short-circuits the modifier, pattern,
    // and default-value branches below. mirrors the `KindThisKeyword`
    // arm of `parseParameterWorker` in microsoft/typescript-go's parser.go.
    if (parser.tree.isTs() and parser.current_token.tag == .this) {
        return try parseThisParameter(parser);
    }

    // ts parameter property modifiers, only accepted inside class constructors.
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

    // `function f(x?: Type) { ... }` optional parameter marker.
    if (parser.tree.isTs() and parser.current_token.tag == .question) {
        const question_end = parser.current_token.span.end;
        try parser.advance() orelse return null;
        ts_types.markPatternOptional(parser, pattern, question_end);
    }

    // `function f(x: Type) { ... }`
    if (parser.tree.isTs() and parser.current_token.tag == .colon) {
        const annotation = try ts_types.parseTypeAnnotation(parser) orelse return null;
        ts_types.applyTypeAnnotationToPattern(parser, pattern, annotation);
    }

    if (parser.current_token.tag == .assign) {
        pattern = try patterns.parseAssignmentPattern(parser, pattern) orelse return null;
    }

    if (has_pp_modifier) {
        return try parser.tree.createNode(.{ .ts_parameter_property = .{
            .decorators = ast.IndexRange.empty,
            .parameter = pattern,
            .override = pp_override,
            .readonly = pp_readonly,
            .accessibility = pp_accessibility,
        } }, .{ .start = start, .end = parser.tree.getSpan(pattern).end });
    }

    return try parser.tree.createNode(.{ .formal_parameter = .{ .pattern = pattern } }, parser.tree.getSpan(pattern));
}

/// parses `this` or `this: Type` as a ts parameter. emitted inside the
/// regular `FormalParameter` wrapper so signature walks, span tracking,
/// and the decoder's `formal_parameter` unwrap rule all continue to work.
/// the inner `TSThisParameter` renders in ESTree as an `Identifier` with
/// `name: "this"`, matching the `@typescript-eslint/typescript-estree`
/// convention.
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

/// true when the current token is a parameter property modifier and the
/// next token can start a binding pattern or another modifier. without
/// the lookahead `constructor(readonly)` would incorrectly eat `readonly`
/// as a modifier and then fail on the missing name.
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
/// matches `canFollowModifier` in typescript-go for parameter positions.
inline fn canFollowParameterPropertyModifier(tag: TokenTag) bool {
    return tag.isIdentifierLike() or
        tag == .left_bracket or
        tag == .left_brace or
        tag == .spread;
}
