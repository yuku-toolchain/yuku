const ast = @import("../ast.zig");
const token = @import("../token.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const Precedence = @import("../token.zig").Precedence;

const expressions = @import("expressions.zig");
const patterns = @import("patterns.zig");
const grammar = @import("../grammar.zig");
const statements = @import("statements.zig");

/// https://tc39.es/ecma262/#sec-for-statement
/// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements
pub fn parseForStatement(parser: *Parser, is_for_await: bool) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    try parser.advance() orelse return null; // consume 'for'

    // check for `for await (...)`
    if (parser.current_token.type == .await) {
        if (!parser.context.in_async and !parser.isModule()) {
            try parser.report(parser.current_token.span, "'for await' is only valid in async functions or modules", .{});
        }
        try parser.advance() orelse return null; // consume 'await'

        // continue parsing with is_for_await = true

        if (!try parser.expect(.left_paren, "Expected '(' after 'for await'", null)) return null;

        return parseForHead(parser, start, true);
    }

    if (!try parser.expect(.left_paren, "Expected '(' after 'for'", null)) return null;

    // first part and determine which kind of for statement this is
    return parseForHead(parser, start, is_for_await);
}

/// parse the head of a for statement to determine its type
fn parseForHead(parser: *Parser, start: u32, is_for_await: bool) Error!?ast.NodeIndex {
    const token_type = parser.current_token.type;

    // empty init: for (;;)
    if (token_type == .semicolon) {
        return parseForStatementRest(parser, start, ast.null_node, is_for_await);
    }

    const decl_start = parser.current_token.span.start;

    const var_decl_kind_or_null = try parseForLoopVariableKindOrNull(parser);

    // variable declaration: for (var/let/const/using/await using ... )
    if (var_decl_kind_or_null) |kind| {
        return parseForWithDeclaration(parser, start, is_for_await, kind, decl_start);
    }

    // expression or assignment target: for (expr in/of ...) or for (expr; ...)
    return parseForWithExpression(parser, start, is_for_await);
}

/// variable kind for for loops
/// returns null if the left side of the for loop is not a variable declaration
fn parseForLoopVariableKindOrNull(parser: *Parser) Error!?ast.VariableKind {
    const token_type = parser.current_token.type;

    if (token_type == .let or token_type == .@"const" or token_type == .@"var") {
        try parser.advance() orelse return null;
        return switch (token_type) {
            .let => .let,
            .@"const" => .@"const",
            .@"var" => .@"var",
            else => unreachable,
        };
    }

    switch (token_type) {
        .using => {
            const next = try parser.lookAhead() orelse return null;

            // 'using' is an identifier in 'for (using of/in ...)' unless it's a declaration.
            if (next.type == .of or next.type == .in) return null;

            // 'using.', 'using(', 'using[' are expression forms where 'using' is an identifier.
            if (next.type == .dot or next.type == .left_paren or next.type == .left_bracket) return null;

            try parser.advance() orelse return null;
            return .using;
        },
        .await => {
            const next = try parser.lookAhead() orelse return null;
            if (next.type != .using) return null;
            try parser.advance() orelse return null; // consume 'await'
            try parser.advance() orelse return null; // consume 'using'
            return .await_using;
        },
        else => return null,
    }
}

/// for loop starting with variable declaration
fn parseForWithDeclaration(parser: *Parser, start: u32, is_for_await: bool, kind: ast.VariableKind, decl_start: u32) Error!?ast.NodeIndex {
    // first declarator
    const first_declarator = try parseForLoopDeclarator(parser) orelse return null;
    const first_end = parser.getSpan(first_declarator).end;

    // check for for-in/for-of
    if (parser.current_token.type == .in) {
        const decl = try createSingleDeclaration(parser, kind, first_declarator, decl_start, first_end);
        return parseForInStatementRest(parser, start, decl, is_for_await);
    }

    if (parser.current_token.type == .of) {
        const decl = try createSingleDeclaration(parser, kind, first_declarator, decl_start, first_end);
        return parseForOfStatementRest(parser, start, decl, is_for_await);
    }

    // regular for statement, might have more declarators
    var end = first_end;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    try parser.scratch_a.append(parser.allocator(), first_declarator);

    // additional declarators: for (let a = 1, b = 2; ...)
    while (parser.current_token.type == .comma) {
        try parser.advance() orelse return null;
        const declarator = try parseForLoopDeclarator(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), declarator);
        end = parser.getSpan(declarator).end;
    }

    const declarators = parser.scratch_a.items.items[checkpoint..parser.scratch_a.items.items.len];

    const declarators_range = try parser.addExtraFromScratch(&parser.scratch_a, checkpoint);

    // init is required for non idenitifer id's in regular loop
    // for example, this is an error:
    // for (let { a: b = let };;) {}
    for (declarators) |decl| {
        const data = parser.getData(decl).variable_declarator;
        const id_data = parser.getData(data.id);
        if (ast.isNull(data.init)) {
            if (id_data != .binding_identifier) {
                try parser.report(parser.getSpan(data.id), "Destructuring declaration in for loop initializer must be initialized", .{ .help = "Add '= value' to provide the object or array to destructure from." });
                return null;
            } else if (kind == .@"const") {
                try parser.report(parser.getSpan(data.id), "'const' declarations in for loop initializer must be initialized", .{ .help = "Add '= value' to initialize the constant in the for loop." });
                return null;
            }
        }
    }

    const decl = try parser.addNode(.{
        .variable_declaration = .{
            .declarators = declarators_range,
            .kind = kind,
        },
    }, .{ .start = decl_start, .end = end });

    return parseForStatementRest(parser, start, decl, is_for_await);
}

/// for loop starting with expression
fn parseForWithExpression(parser: *Parser, start: u32, is_for_await: bool) Error!?ast.NodeIndex {
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
        try grammar.expressionToPattern(parser, expr, .assignable) orelse return null;
        return parseForInStatementRest(parser, start, expr, is_for_await);
    }

    if (parser.current_token.type == .of) {
        try grammar.expressionToPattern(parser, expr, .assignable) orelse return null;
        return parseForOfStatementRest(parser, start, expr, is_for_await);
    }

    // regular for statement
    return parseForStatementRest(parser, start, expr, is_for_await);
}

/// parse rest of regular for statement after init
fn parseForStatementRest(parser: *Parser, start: u32, init: ast.NodeIndex, is_for_await: bool) Error!?ast.NodeIndex {
    if (is_for_await) {
        try parser.report(parser.current_token.span, "'for await' is only valid with for-of loops", .{});
        return null;
    }

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

    const body = try statements.parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

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
fn parseForInStatementRest(parser: *Parser, start: u32, left: ast.NodeIndex, is_for_await: bool) Error!?ast.NodeIndex {
    if (is_for_await) {
        try parser.report(parser.current_token.span, "'for await' requires 'of', not 'in'", .{});
        return null;
    }

    try parser.advance() orelse return null; // consume 'in'

    const right = try expressions.parseExpression(parser, Precedence.Lowest, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after for-in expression", null)) return null;

    const body = try statements.parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    return try parser.addNode(.{
        .for_in_statement = .{
            .left = left,
            .right = right,
            .body = body,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// rest of for-of statement after left
fn parseForOfStatementRest(parser: *Parser, start: u32, left: ast.NodeIndex, is_for_await: bool) Error!?ast.NodeIndex {
    try parser.advance() orelse return null; // consume 'of'

    // for-of right side is AssignmentExpression, not Expression (no comma)
    const right = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;

    if (!try parser.expect(.right_paren, "Expected ')' after for-of expression", null)) return null;

    const body = try statements.parseStatement(parser, .{ .can_be_single_statement_context = true }) orelse return null;

    return try parser.addNode(.{
        .for_of_statement = .{
            .left = left,
            .right = right,
            .body = body,
            .await = is_for_await,
        },
    }, .{ .start = start, .end = parser.getSpan(body).end });
}

/// a single variable declarator for for loops
fn parseForLoopDeclarator(parser: *Parser) Error!?ast.NodeIndex {
    const decl_start = parser.current_token.span.start;
    const id = try patterns.parseBindingPattern(parser) orelse return null;

    var init: ast.NodeIndex = ast.null_node;
    var end = parser.getSpan(id).end;

    if (parser.current_token.type == .assign) {
        try parser.advance() orelse return null;
        init = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        end = parser.getSpan(init).end;
    }

    return try parser.addNode(.{ .variable_declarator = .{ .id = id, .init = init } }, .{ .start = decl_start, .end = end });
}

/// create a variable declaration with a single declarator
fn createSingleDeclaration(parser: *Parser, kind: ast.VariableKind, declarator: ast.NodeIndex, decl_start: u32, decl_end: u32) Error!ast.NodeIndex {
    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);
    try parser.scratch_a.append(parser.allocator(), declarator);

    return try parser.addNode(.{
        .variable_declaration = .{
            .declarators = try parser.addExtraFromScratch(&parser.scratch_a, checkpoint),
            .kind = kind,
        },
    }, .{ .start = decl_start, .end = decl_end });
}
