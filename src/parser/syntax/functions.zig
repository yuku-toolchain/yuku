const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

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

    const params = try parseFormalParamaters(parser, if (is_unique_formal_parameters) .unique_formal_parameters else .formal_parameters) orelse return null;

    const params_end = parser.current_token.span.end; // including )

    if (!try parser.expect(
        .right_paren,
        "Expected ')' to close parameter list",
        "Add a closing parenthesis ')' after the parameters, or check for missing commas between parameters.",
    )) return null;

    // typescript return type: `function f(): Type { ... }`
    var return_type: ast.NodeIndex = .null;
    var return_type_end: u32 = params_end;
    if (parser.tree.isTs() and parser.current_token.tag == .colon) {
        const annotation = try ts_types.parseTypeAnnotation(parser) orelse return null;
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

    const end = if (body != .null) parser.tree.getSpan(body).end else return_type_end;

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
            .declare = opts.is_declare,
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

pub fn parseFormalParamaters(parser: *Parser, kind: ast.FormalParameterKind) Error!?ast.NodeIndex {
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
            const param = try parseFormalParamater(parser) orelse break;

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

pub fn parseFormalParamater(parser: *Parser) Error!?ast.NodeIndex {
    var pattern = try patterns.parseBindingPattern(parser) orelse return null;

    // typescript: `function f(x: Type) { ... }`
    // annotation attaches to the inner binding pattern so the decoder emits
    // `{ type: "Identifier", typeAnnotation: ... }` after unwrapping FormalParameter.
    if (parser.tree.isTs() and parser.current_token.tag == .colon) {
        const annotation = try ts_types.parseTypeAnnotation(parser) orelse return null;
        ts_types.applyTypeAnnotationToPattern(parser, pattern, annotation);
    }

    if (parser.current_token.tag == .assign) {
        pattern = try patterns.parseAssignmentPattern(parser, pattern) orelse return null;
    }

    return try parser.tree.createNode(.{ .formal_parameter = .{ .pattern = pattern } }, parser.tree.getSpan(pattern));
}
