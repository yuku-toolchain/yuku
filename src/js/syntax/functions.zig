const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;

const patterns = @import("patterns.zig");

const ParseFunctionOpts = packed struct {
    is_async: bool = false,
    is_expression: bool = false,
    is_declare: bool = false,
};

pub fn parseFunction(parser: *Parser, opts: ParseFunctionOpts) ?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (opts.is_async or opts.is_declare) {
        parser.advance();
    }

    if (!parser.expect(
        .Function,
        "Expected 'function' keyword",
        null,
    )) return null;

    const function_type: ast.FunctionType = if (opts.is_expression) .FunctionExpression else if (opts.is_declare) .TSDeclareFunction else .FunctionDeclaration;

    var is_generator = false;

    if (parser.current_token.type == .Star) {
        is_generator = true;
        parser.advance();
    }

    const id = if (parser.current_token.type.isIdentifierLike())
        patterns.parseBindingIdentifier(parser) orelse ast.null_node
    else
        ast.null_node;

    if (!opts.is_expression and id == ast.null_node) {
        parser.err(
            parser.current_token.span.start,
            parser.current_token.span.end,
            "Function declaration requires a name",
            "Add a name after 'function', e.g. 'function myFunc() {}'.",
        );
        return null;
    }

    const params = parseFormalParamaters(parser) orelse return null;

    var body = ast.null_node;

    if(opts.is_declare){
        if(parser.current_token.type == .LeftBrace){
            parser.err(
                parser.current_token.span.start,
                parser.current_token.span.end,
                "TS(1183): An implementation cannot be declared in ambient contexts.",
                "Remove the function body or remove the 'declare' modifier"
            );
            return null;
        }
    } else {
        body = parseFunctionBody(parser) orelse ast.null_node;
    }

    const end = if (body != ast.null_node) parser.getSpan(body).end else parser.eatSemicolon(parser.current_token.span.start);

    return parser.addNode(.{
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

pub fn parseFunctionBody(parser: *Parser) ?ast.NodeIndex {
    const start = parser.current_token.span.start;

    if (!parser.expect(
        .LeftBrace,
        "Expected '{' to start function body",
        "Function bodies must be enclosed in braces: function name() { ... }",
    )) return null;

    const body_data = parser.parseBody();

    const end = parser.current_token.span.end;

    if (!parser.expect(
        .RightBrace,
        "Expected '}' to close function body",
        "Add a closing brace '}' to complete the function, or check for unbalanced braces inside.",
    )) return null;

    return parser.addNode(.{ .function_body = .{ .statements = body_data.statements, .directives = body_data.directives } }, .{ .start = start, .end = end });
}

pub fn parseFormalParamaters(parser: *Parser) ?ast.NodeIndex {
    if (!parser.expect(
        .LeftParen,
        "Expected '(' to start parameter list",
        "Function parameters must be enclosed in parentheses: function name(a, b) {}",
    )) return null;

    const start = parser.current_token.span.start;
    var end: u32 = parser.current_token.span.end;

    const params_checkpoint = parser.scratch_a.begin();

    var rest = ast.null_node;

    while (true) {
        if (parser.current_token.type == .RightParen or parser.current_token.type == .EOF) break;

        if (parser.current_token.type == .Spread) {
            rest = patterns.parseBindingRestElement(parser) orelse ast.null_node;
            end = parser.getSpan(rest).end;

            if (parser.current_token.type == .Comma) {
                parser.err(
                    parser.getSpan(rest).start,
                    parser.current_token.span.end,
                    "Rest parameter must be the last parameter",
                    "Move the '...rest' parameter to the end of the parameter list, or remove trailing parameters.",
                );

                parser.scratch_a.reset(params_checkpoint);

                return null;
            }
        } else {
            const param = parseFormalParamater(parser) orelse break;

            end = parser.getSpan(param).end;

            parser.scratch_a.append(parser.allocator(), param);
        }

        if (parser.current_token.type == .Comma) {
            parser.advance();
        } else break;
    }

    if (!parser.expect(
        .RightParen,
        "Expected ')' to close parameter list",
        "Add a closing parenthesis ')' after the parameters, or check for missing commas between parameters.",
    )) {
        parser.scratch_a.reset(params_checkpoint);
        return null;
    }

    return parser.addNode(.{ .formal_parameters = .{ .items = parser.addExtra(parser.scratch_a.take(params_checkpoint)), .rest = rest } }, .{ .start = start, .end = end });
}

pub fn parseFormalParamater(parser: *Parser) ?ast.NodeIndex {
    var pattern = patterns.parseBindingPattern(parser) orelse return null;

    if (parser.current_token.type == .Assign) {
        pattern = patterns.parseAssignmentPattern(parser, pattern) orelse return null;
    }

    return parser.addNode(.{ .formal_parameter = .{ .pattern = pattern } }, parser.getSpan(pattern));
}
