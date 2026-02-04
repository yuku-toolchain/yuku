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
const extensions = @import("extensions.zig");

pub const ParseClassOpts = packed struct {
    /// whether the class is parsing in an expression position
    is_expression: bool = false,
    /// for export default class, allows optional name but produces ClassDeclaration
    is_default_export: bool = false,
};

/// class declaration or expression
/// https://tc39.es/ecma262/#sec-class-definitions
pub fn parseClass(parser: *Parser, opts: ParseClassOpts, start_from_param: ?u32) Error!?ast.NodeIndex {
    return parseClassDecorated(parser, opts, start_from_param, ast.IndexRange.empty);
}

pub fn parseClassDecorated(
    parser: *Parser,
    opts: ParseClassOpts,
    start_from_param: ?u32,
    decorators: ast.IndexRange,
) Error!?ast.NodeIndex {
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
        try parser.advance() orelse return null; // consume 'extends'
        super_class = try expressions.parseLeftHandSideExpression(parser) orelse return null;
    }

    // class body
    const body = try parseClassBody(parser) orelse return null;
    const body_end = parser.getSpan(body).end;

    return try parser.addNode(.{
        .class = .{
            .type = class_type,
            .decorators = decorators,
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
    defer parser.scratch_a.reset(checkpoint);

    while (true) {
        const tok_type = parser.current_token.type;
        if (tok_type == .right_brace or tok_type == .eof) break;

        // empty statement (semicolon)
        if (tok_type == .semicolon) {
            try parser.advance() orelse return null;
            continue;
        }

        const element = try parseClassElement(parser) orelse return null;
        try parser.scratch_a.append(parser.allocator(), element);
    }

    const end = parser.current_token.span.end;

    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close class body",
        "Add a closing brace '}' to complete the class, or check for unbalanced braces inside.",
    )) return null;

    return try parser.addNode(.{
        .class_body = .{ .body = try parser.addExtra(try parser.scratch_a.take(parser.allocator(), checkpoint)) },
    }, .{ .start = start, .end = end });
}

/// a single class element (method, field, or static block)
fn parseClassElement(parser: *Parser) Error!?ast.NodeIndex {
    var decorators: ast.IndexRange = ast.IndexRange.empty;
    var elem_start = parser.current_token.span.start;

    if (parser.current_token.type == .at) {
        elem_start = parser.current_token.span.start;
        decorators = try extensions.parseDecorators(parser) orelse return null;
    }

    var is_static = false;
    var is_async = false;
    var is_generator = false;
    var is_accessor = false;
    var kind: ast.MethodDefinitionKind = .method;
    var computed = false;
    var key: ast.NodeIndex = ast.null_node;

    // check for 'static' modifier
    if (parser.current_token.type == .static) {
        const static_token = parser.current_token;
        try parser.advance() orelse return null;

        // static { } - static block
        if (parser.current_token.type == .left_brace) {
            if (decorators.len != 0) {
                const first = parser.getExtra(decorators)[0];
                try parser.report(
                    parser.getSpan(first),
                    "Decorators cannot be applied to static blocks",
                    .{ .help = "Remove the decorator or apply it to a method or field instead." },
                );
            }
            return parseStaticBlock(parser, static_token.span.start);
        }

        // if next token is '(' or nothing that could be a class element key, 'static' is the key
        // e.g., `static() {}` is a method named "static", not a static method
        if (parser.current_token.type == .left_paren or !isClassElementKeyStart(parser.current_token.type)) {
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
        try parser.advance() orelse return null;

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
        try parser.advance() orelse return null;
    }

    // check for get/set (only if no key yet and not async/generator)
    if (ast.isNull(key) and !is_async and !is_generator and parser.current_token.type == .identifier) {
        const lexeme = parser.current_token.lexeme;
        const is_get = std.mem.eql(u8, lexeme, "get");
        const is_set = std.mem.eql(u8, lexeme, "set");

        if (is_get or is_set) {
            const get_set_token = parser.current_token;
            try parser.advance() orelse return null;

            // check if this is get/set accessor or just a property named 'get'/'set'
            if (isClassElementKeyStart(parser.current_token.type)) {
                kind = if (is_get) .get else .set;
            } else {
                key = try parser.addNode(
                    .{ .identifier_name = .{ .name_start = get_set_token.span.start, .name_len = @intCast(get_set_token.lexeme.len) } },
                    get_set_token.span,
                );
            }
        }
    }

    // check for accessor field (only if no key yet and not async/generator)
    if (ast.isNull(key) and !is_async and !is_generator and parser.current_token.type == .identifier) {
        const lexeme = parser.current_token.lexeme;
        if (std.mem.eql(u8, lexeme, "accessor")) {
            const accessor_token = parser.current_token;
            const next = try parser.lookAhead() orelse return null;

            if (isClassElementKeyStart(next.type)) {
                is_accessor = true;
                try parser.advance() orelse return null; // consume 'accessor'
            } else {
                key = try parser.addNode(
                    .{ .identifier_name = .{ .name_start = accessor_token.span.start, .name_len = @intCast(accessor_token.lexeme.len) } },
                    accessor_token.span,
                );
                try parser.advance() orelse return null; // consume 'accessor' as key
            }
        }
    }

    // parse the key if not already determined
    if (ast.isNull(key)) {
        const key_result = try parseClassElementKey(parser) orelse return null;
        key = key_result.key orelse return null;
        computed = key_result.computed;
    }

    // determine if this is constructor
    // non-static, non-computed methods with PropName "constructor" are constructors
    // PropName can come from identifier or string literal (but not computed)
    if (!is_static and !computed) {
        if (isKeyName(parser, key, "constructor")) {
            // constructor cannot have get/set modifiers
            if (kind == .get or kind == .set) {
                try parser.report(
                    parser.getSpan(key),
                    "Constructor can't have get/set modifier",
                    .{ .help = "Remove the get/set keyword from the constructor." },
                );
            }

            if (kind == .method) {
                kind = .constructor;
            }
        }
    }

    // method: key followed by (
    if (parser.current_token.type == .left_paren) {
        if (is_accessor) {
            try parser.report(
                parser.current_token.span,
                "Accessor properties cannot be methods",
                .{ .help = "Remove the parentheses to declare an auto-accessor field." },
            );
            return null;
        }
        return parseMethodDefinition(parser, elem_start, decorators, key, computed, kind, is_static, is_async, is_generator);
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
    return parsePropertyDefinition(parser, elem_start, decorators, key, computed, is_static, is_accessor);
}

const KeyResult = struct {
    key: ?ast.NodeIndex,
    computed: bool,
};

/// class element key
fn parseClassElementKey(parser: *Parser) Error!?KeyResult {
    // computed key
    if (parser.current_token.type == .left_bracket) {
        try parser.advance() orelse return null; // consume '['
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
        try parser.advance() orelse return null;
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
    decorators: ast.IndexRange,
    key: ast.NodeIndex,
    computed: bool,
    kind: ast.MethodDefinitionKind,
    is_static: bool,
    is_async: bool,
    is_generator: bool,
) Error!?ast.NodeIndex {
    if (is_static and !computed) {
        try validateStaticPrototypeName(parser, key);
    }

    if (kind == .constructor) {
        if (is_async) {
            try parser.report(
                parser.getSpan(key),
                "Constructor cannot be async",
                .{ .help = "Remove the 'async' modifier from the constructor." },
            );
        }
        if (is_generator) {
            try parser.report(
                parser.getSpan(key),
                "Constructor cannot be a generator",
                .{ .help = "Remove the '*' from the constructor." },
            );
        }
    } else if (is_generator) {
        if (kind == .get) {
            try parser.report(
                parser.getSpan(key),
                "Getter cannot be a generator",
                .{ .help = "Remove the '*' from the getter definition." },
            );
        } else if (kind == .set) {
            try parser.report(
                parser.getSpan(key),
                "Setter cannot be a generator",
                .{ .help = "Remove the '*' from the setter definition." },
            );
        }
    }

    if (kind == .constructor) {
        if (is_async) {
            try parser.report(
                parser.getSpan(key),
                "Constructor cannot be async",
                .{ .help = "Remove the 'async' modifier from the constructor." },
            );
        }
        if (is_generator) {
            try parser.report(
                parser.getSpan(key),
                "Constructor cannot be a generator",
                .{ .help = "Remove the '*' from the constructor." },
            );
        }
    }

    if (kind == .get and is_generator) {
        try parser.report(
            parser.getSpan(key),
            "Getter cannot be a generator",
            .{ .help = "Remove the '*' from the getter definition." },
        );
    }

    if (kind == .set and is_generator) {
        try parser.report(
            parser.getSpan(key),
            "Setter cannot be a generator",
            .{ .help = "Remove the '*' from the setter definition." },
        );
    }

    const saved_async = parser.context.in_async;
    const saved_yield_is_keyword = parser.context.yield_is_keyword;

    parser.context.in_async = is_async;
    parser.context.yield_is_keyword = is_generator;

    defer {
        parser.context.in_async = saved_async;
        parser.context.yield_is_keyword = saved_yield_is_keyword;
    }

    const func_start = parser.current_token.span.start;

    if (!try parser.expect(.left_paren, "Expected '(' to start method parameters", null)) return null;

    const params = try functions.parseFormalParamaters(parser, .unique_formal_parameters) orelse return null;
    const params_data = parser.getData(params).formal_parameters;

    if (kind == .get) {
        if (params_data.items.len != 0 or !ast.isNull(params_data.rest)) {
            try parser.report(
                parser.getSpan(params),
                "Getter must have no parameters",
                .{ .help = "Remove all parameters from the getter." },
            );
        }
    }

    if (kind == .set) {
        if (params_data.items.len != 1 or !ast.isNull(params_data.rest)) {
            try parser.report(
                parser.getSpan(params),
                "Setter must have exactly one parameter",
                .{ .help = "Setters accept exactly one argument." },
            );
        }
    }

    if (!try parser.expect(.right_paren, "Expected ')' after method parameters", null)) return null;

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
            .decorators = decorators,
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
    decorators: ast.IndexRange,
    key: ast.NodeIndex,
    computed: bool,
    is_static: bool,
    is_accessor: bool,
) Error!?ast.NodeIndex {
    if (is_static and !computed) {
        try validateStaticPrototypeName(parser, key);
    }

    var value: ast.NodeIndex = ast.null_node;
    var end = parser.getSpan(key).end;

    if (parser.current_token.type == .assign) {
        try parser.advance() orelse return null; // consume '='
        value = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        end = parser.getSpan(value).end;
    }

    const tok_type = parser.current_token.type;
    if (tok_type == .semicolon) {
        end = parser.current_token.span.end;
        try parser.advance() orelse return null;
    } else if (!parser.canInsertSemicolon(parser.current_token) and tok_type != .right_brace) {
        try parser.report(
            parser.current_token.span,
            "Expected ';' after class field",
            .{ .help = "Add a semicolon after the field declaration." },
        );
        return null;
    }

    return try parser.addNode(
        .{ .property_definition = .{
            .decorators = decorators,
            .key = key,
            .value = value,
            .computed = computed,
            .static = is_static,
            .accessor = is_accessor,
        } },
        .{ .start = elem_start, .end = end },
    );
}

/// static block: static { ... }
fn parseStaticBlock(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    if (!try parser.expect(.left_brace, "Expected '{' to start static block", null)) return null;

    const body = try parser.parseBody(.right_brace);

    const end = parser.current_token.span.end;

    if (!try parser.expect(.right_brace, "Expected '}' to close static block", null)) return null;

    return try parser.addNode(
        .{ .static_block = .{ .body = body } },
        .{ .start = start, .end = end },
    );
}

/// if token could start a class element key (after modifiers like static/async/get/set)
inline fn isClassElementKeyStart(tok_type: token.TokenType) bool {
    return tok_type == .star or
        tok_type == .left_bracket or
        tok_type == .private_identifier or
        tok_type == .string_literal or
        tok_type.isNumericLiteral() or
        tok_type.isIdentifierLike();
}

/// checks if a key node matches a given name (works for identifier and string literal keys)
fn isKeyName(parser: *Parser, key: ast.NodeIndex, comptime name: []const u8) bool {
    const key_data = parser.getData(key);
    switch (key_data) {
        .identifier_name => |id| {
            const key_name = parser.getSourceText(id.name_start, id.name_len);
            if (key_name.len != name.len) return false;
            return std.mem.eql(u8, key_name, name);
        },
        .string_literal => |str| {
            const raw = parser.getSourceText(str.raw_start, str.raw_len);
            if (raw.len < 2 + name.len) return false;
            return std.mem.eql(u8, raw[1 .. raw.len - 1], name);
        },
        else => return false,
    }
}

fn validateStaticPrototypeName(parser: *Parser, key: ast.NodeIndex) Error!void {
    if (!isKeyName(parser, key, "prototype")) return;

    try parser.report(
        parser.getSpan(key),
        "Classes may not have a static property named 'prototype'",
        .{ .help = "Remove 'static' or rename the property." },
    );
}
