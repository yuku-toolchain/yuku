const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

const patterns = @import("patterns.zig");

const ParseFunctionOpts = packed struct {
    is_async: bool = false,
    is_expression: bool = false,
    is_declare: bool = false,
};

pub fn parseFunction(parser: *Parser, opts: ParseFunctionOpts, start_from_param: ?u32) Error!?ast.NodeIndex {
    const start = start_from_param orelse parser.current_token.span.start;

    const saved_async = parser.context.in_async;
    const saved_generator = parser.context.in_generator;

    if (opts.is_async) {
        parser.context.in_async = true;
    }

    if (!try parser.expect(
        .function,
        "Expected 'function' keyword",
        null,
    )) return null;

    const function_type: ast.FunctionType = if (opts.is_expression) .function_expression else if (opts.is_declare) .ts_declare_function else .function_declaration;

    var is_generator = false;

    if (parser.current_token.type == .star) {
        is_generator = true;
        parser.context.in_generator = true;
        try parser.advance();
    }

    const id = if (parser.current_token.type.isIdentifierLike())
        try patterns.parseBindingIdentifier(parser) orelse ast.null_node
    else
        ast.null_node;

    if (!opts.is_expression and ast.isNull(id)) {
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

    defer {
        parser.context.in_async = saved_async;
        parser.context.in_generator = saved_generator;
    }

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
