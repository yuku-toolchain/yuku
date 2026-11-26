const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const TokenTag = @import("../token.zig").TokenTag;

const literals = @import("literals.zig");
const functions = @import("functions.zig");
const expressions = @import("expressions.zig");
const object = @import("object.zig");
const ts = @import("ts/types.zig");
const ts_decl = @import("ts/statements.zig");
const static_semantics = @import("spec").static_semantics;

pub const ParseClassOptions = struct {
    is_expression: bool = false,
    is_default_export: bool = false,
    is_declare: bool = false,
    is_abstract: bool = false,
    decorators: ast.IndexRange = .empty,
};

pub fn parseDecorators(parser: *Parser, leading: ast.IndexRange) Error!?ast.IndexRange {
    const decorators = parser.list();
    defer decorators.deinit();
    for (parser.tree.extra(leading)) |decorator| try decorators.append(decorator);

    while (parser.at(.at)) {
        const start = try parser.take(.at) orelse return null;
        const expression = try expressions.parseLeftHandSideExpression(parser, .decorator) orelse
            return null;
        try decorators.append(try parser.finishAt(start, expression, .{
            .decorator = .{ .expression = expression },
        }));
    }
    return try decorators.finish();
}

pub fn parseDecoratedClass(parser: *Parser, options: ParseClassOptions) Error!?ast.NodeIndex {
    std.debug.assert(parser.at(.at));
    const start = parser.current_token.span.start;
    var decorated = options;
    decorated.decorators = try parseDecorators(parser, .empty) orelse return null;
    return parseClass(parser, decorated, start);
}

/// https://tc39.es/ecma262/#sec-class-definitions
pub fn parseClass(
    parser: *Parser,
    options: ParseClassOptions,
    start_from_param: ?u32,
) Error!?ast.NodeIndex {
    std.debug.assert(start_from_param != null or
        parser.at(.class) or parser.at(.abstract) or parser.at(.declare));
    const start = start_from_param orelse parser.current_token.span.start;
    const is_ts = parser.tree.isTs();

    var is_abstract = options.is_abstract;
    var is_declare = options.is_declare;
    if (is_ts and !options.is_expression and isClassModifiersNext(parser)) {
        while (!parser.at(.class)) {
            std.debug.assert(parser.at(.abstract) or parser.at(.declare));
            if (parser.at(.abstract)) is_abstract = true else is_declare = true;
            try parser.advance() orelse return null;
        }
    }
    const saved_ambient = parser.ts_context.ambient;
    defer parser.ts_context.ambient = saved_ambient;
    if (is_declare) parser.ts_context.ambient = true;

    if (!try parser.expect(.class, "Expected 'class'", null)) return null;

    const class_type: ast.ClassType = if (options.is_expression and !options.is_default_export)
        .class_expression
    else
        .class_declaration;

    if (class_type == .class_declaration and parser.context.single_statement) {
        @branchHint(.unlikely);
        try parser.report(
            .{ .start = start, .end = parser.current_token.span.end },
            "Classes can only be declared at the top level or inside a block",
            .{ .help = "Wrap the declaration in a block" },
        );
    }

    const id: ast.NodeIndex = if (canStartClassName(parser))
        try literals.parseBindingIdentifier(parser) orelse .null
    else
        .null;

    if (id == .null and !options.is_expression and !options.is_default_export) {
        return parser.fail(
            parser.current_token.span,
            "A class declaration without the 'default' modifier must have a name",
            .{ .help = "Add a name after 'class'" },
        );
    }

    const type_parameters: ast.NodeIndex = if (is_ts) try ts.parseTypeParameters(parser) else .null;

    // `extends Base<T> {` rewinds the instantiation expression and leaves `<T>` here
    var super_class: ast.NodeIndex = .null;
    var super_type_arguments: ast.NodeIndex = .null;

    if (try parser.eat(.extends) orelse return null) {
        super_class = try expressions.parseLeftHandSideExpression(parser, .extends_clause) orelse
            return null;

        if (is_ts) switch (parser.tree.data(super_class)) {
            .ts_instantiation_expression => |inst| {
                super_class = inst.expression;
                super_type_arguments = inst.type_arguments;
            },
            else => if (ts.isAngleOpen(parser.current_token.tag)) {
                super_type_arguments = try ts.parseTypeArguments(parser);
            },
        };
    }

    const implements: ast.IndexRange = if (is_ts)
        try ts_decl.parseHeritageClause(parser, .implements, .ts_class_implements) orelse
            return null
    else
        .empty;

    const body = try parseClassBody(parser) orelse return null;

    return try parser.finish(start, .{ .class = .{
        .type = class_type,
        .decorators = options.decorators,
        .id = id,
        .super_class = super_class,
        .body = body,
        .type_parameters = type_parameters,
        .super_type_arguments = super_type_arguments,
        .implements = implements,
        .declare = is_declare,
        .abstract = is_abstract,
    } });
}

pub fn isClassModifiersNext(parser: *Parser) bool {
    var peek = parser.beginPeek();
    defer peek.end();

    var seen_declare = false;
    var seen_abstract = false;
    var token = parser.current_token;
    // each modifier at most once bounds the loop
    while (true) {
        const seen = switch (token.tag) {
            .declare => &seen_declare,
            .abstract => &seen_abstract,
            .class => return seen_declare or seen_abstract,
            else => return false,
        };
        if (seen.*) return false;
        seen.* = true;
        token = peek.next();
        if (token.hasLineTerminatorBefore()) return false;
    }
}

pub inline fn isAbstractClassNext(parser: *Parser) bool {
    std.debug.assert(parser.at(.abstract));
    const next = parser.peekAhead();
    return next.tag == .class and !next.hasLineTerminatorBefore();
}

inline fn canStartClassName(parser: *Parser) bool {
    const tag = parser.current_token.tag;
    if (!tag.isIdentifierLike() or tag == .extends) return false;
    if (parser.tree.isTs() and tag == .implements) {
        return !parser.peekAhead().tag.isIdentifierLike();
    }
    return true;
}

fn parseClassBody(parser: *Parser) Error!?ast.NodeIndex {
    const start = parser.current_token.span.start;
    if (!try parser.expect(.left_brace, "Expected '{' to start the class body", null)) return null;

    const elements = parser.list();
    defer elements.deinit();
    while (!parser.at(.right_brace) and !parser.at(.eof)) {
        if (try parser.eat(.semicolon) orelse return null) continue;
        try elements.append(try parseClassElement(parser) orelse return null);
    }

    if (!try parser.expect(
        .right_brace,
        "Expected '}' to close the class body",
        "Check that every '{' in the class has a matching '}'",
    )) return null;
    return try parser.finish(start, .{ .class_body = .{ .body = try elements.finish() } });
}

fn parseClassElement(parser: *Parser) Error!?ast.NodeIndex {
    const is_ts = parser.tree.isTs();
    const start = parser.current_token.span.start;
    const decorators: ast.IndexRange = if (parser.at(.at))
        try parseDecorators(parser, .empty) orelse return null
    else
        .empty;

    if (try tryStaticBlock(parser, decorators)) |block| return block;

    var modifiers: Modifiers = .{};
    var key = try parseModifiers(parser, &modifiers);

    if (key == .null) modifiers.generator = try parser.eat(.star) orelse return null;

    if (key == .null and is_ts and parser.at(.left_bracket) and ts.isIndexSignatureStart(parser)) {
        return parseIndexSignatureElement(parser, start, decorators, modifiers);
    }

    var computed = false;
    if (key == .null) {
        key, computed = try object.parsePropertyKey(parser, .class) orelse return null;
    }

    if (is_ts) switch (parser.current_token.tag) {
        .question => {
            modifiers.optional = true;
            try parser.advance() orelse return null;
        },
        .logical_not => if (!parser.current_token.hasLineTerminatorBefore()) {
            modifiers.definite = true;
            try parser.advance() orelse return null;
        },
        else => {},
    };

    if (!computed) try detectConstructor(parser, key, &modifiers);

    if (parser.at(.left_paren) or (is_ts and parser.at(.less_than))) {
        if (modifiers.accessor) return parser.fail(
            parser.current_token.span,
            "'accessor' modifier can only appear on a property declaration",
            .{ .help = "Remove 'accessor', or remove the parameter list" },
        );
        if (modifiers.definite) try parser.report(
            parser.tree.span(key),
            "A method cannot have a definite assignment assertion",
            .{ .help = "Remove the '!'" },
        );
        return parseMethodDefinition(parser, start, decorators, key, computed, modifiers);
    }

    if (modifiers.async or modifiers.generator) {
        return parser.failExpected("Expected '(' to start the method parameters", .{});
    }
    if (modifiers.kind == .constructor) return parser.fail(
        parser.tree.span(key),
        "Classes may not have a field named 'constructor'",
        .{ .help = "Rename the field" },
    );
    if (modifiers.kind != .method) {
        return parser.failExpected("Expected '(' to start the accessor parameters", .{});
    }
    return parsePropertyDefinition(parser, start, decorators, key, computed, modifiers);
}

fn parseIndexSignatureElement(
    parser: *Parser,
    start: u32,
    decorators: ast.IndexRange,
    modifiers: Modifiers,
) Error!?ast.NodeIndex {
    if (decorators.len != 0) try parser.report(
        parser.tree.span(parser.tree.extra(decorators)[0]),
        "Decorators cannot be applied to index signatures",
        .{ .help = "Remove the decorator" },
    );

    if (modifiers.async or modifiers.generator or modifiers.accessor or
        modifiers.kind != .method or modifiers.declare or modifiers.abstract or modifiers.override)
    {
        try parser.report(
            .{ .start = start, .end = parser.current_token.span.end },
            "Index signatures only accept 'readonly', 'static', and accessibility modifiers",
            .{},
        );
    }

    const node = try ts.parseIndexSignature(parser, start, .{
        .readonly = modifiers.readonly,
        .static = modifiers.static,
    }) orelse return null;

    if (parser.at(.semicolon)) {
        const span = parser.tree.span(node);
        parser.tree.setSpan(node, .{ .start = span.start, .end = parser.current_token.span.end });
        try parser.advance() orelse return null;
    }
    return node;
}

const Modifiers = struct {
    static: bool = false,
    async: bool = false,
    generator: bool = false,
    accessor: bool = false,
    kind: ast.MethodDefinitionKind = .method,
    declare: bool = false,
    abstract: bool = false,
    override: bool = false,
    readonly: bool = false,
    accessibility: ast.Accessibility = .none,
    optional: bool = false,
    definite: bool = false,

    fn conflicts(self: Modifiers, tag: TokenTag) bool {
        // `accessor` sits directly before the key, so a following modifier word is the key
        if (self.accessor) return true;
        return switch (tag) {
            .async, .get, .set => self.async or self.kind != .method,
            inline .static, .accessor, .declare, .abstract, .override, .readonly => |flag| {
                return @field(self, @tagName(flag));
            },
            .public, .private, .protected => self.accessibility != .none,
            else => unreachable,
        };
    }

    fn set(self: *Modifiers, tag: TokenTag) void {
        switch (tag) {
            inline .static, .async, .accessor, .declare, .abstract, .override, .readonly => |flag| {
                @field(self, @tagName(flag)) = true;
            },
            .get => self.kind = .get,
            .set => self.kind = .set,
            inline .public, .private, .protected => |access| {
                self.accessibility = @field(ast.Accessibility, @tagName(access));
            },
            else => unreachable,
        }
    }
};

fn parseModifiers(parser: *Parser, modifiers: *Modifiers) Error!ast.NodeIndex {
    while (isModifier(parser.current_token.tag, parser.tree.isTs())) {
        const token = parser.current_token;
        const next = parser.peekAhead();

        // an accessor can never be a generator, so `get *` keeps `get` as a field key
        const accessor_before_star = next.tag == .star and
            (token.tag == .get or token.tag == .set);
        const may_break_line = token.tag == .static or token.tag == .get or token.tag == .set;

        const is_modifier =
            !token.isEscaped() and
            canStartElementKey(next.tag) and
            !accessor_before_star and
            (may_break_line or !next.hasLineTerminatorBefore()) and
            !modifiers.conflicts(token.tag);
        if (!is_modifier) return try literals.parseIdentifierName(parser) orelse .null;

        try parser.advance() orelse return .null;
        modifiers.set(token.tag);
    }
    return .null;
}

// runs before modifiers so a decorated static block still parses
fn tryStaticBlock(parser: *Parser, decorators: ast.IndexRange) Error!?ast.NodeIndex {
    if (!parser.at(.static) or parser.peekAhead().tag != .left_brace) return null;

    if (decorators.len != 0) try parser.report(
        parser.tree.span(parser.tree.extra(decorators)[0]),
        "Decorators cannot be applied to static blocks",
        .{ .help = "Remove the decorator, or apply it to a method or field instead" },
    );
    const start = try parser.take(.static) orelse return null;
    if (!try parser.expect(.left_brace, "Expected '{' to start the static block", null)) {
        return null;
    }

    // ClassStaticBlockStatementList is [~Yield, +Await, ~Return]
    const outer = parser.enterFunction(true, false);
    defer parser.leaveFunction(outer);
    parser.context.@"return" = false;

    const body = try parser.parseBody(.right_brace, .other);
    if (!try parser.expect(.right_brace, "Expected '}' to close the static block", null)) {
        return null;
    }
    return try parser.finish(start, .{ .static_block = .{ .body = body } });
}

inline fn isModifier(tag: TokenTag, is_ts: bool) bool {
    return switch (tag) {
        .static, .async, .get, .set, .accessor => true,
        .declare, .public, .private, .protected, .override, .readonly, .abstract => is_ts,
        else => false,
    };
}

inline fn canStartElementKey(tag: TokenTag) bool {
    return tag.isIdentifierLike() or
        tag.isNumericLiteral() or
        tag == .string_literal or
        tag == .private_identifier or
        tag == .left_bracket or
        tag == .star;
}

fn parseMethodDefinition(
    parser: *Parser,
    start: u32,
    decorators: ast.IndexRange,
    key: ast.NodeIndex,
    computed: bool,
    modifiers: Modifiers,
) Error!?ast.NodeIndex {
    const kind = modifiers.kind;
    if (modifiers.static and !computed) try validateElementName(parser, key, true, .method);
    try validateMethodModifiers(parser, key, modifiers);

    const outer = parser.enterFunction(modifiers.async, modifiers.generator);
    defer parser.leaveFunction(outer);

    const is_ts = parser.tree.isTs();
    const function_start = parser.current_token.span.start;

    const type_parameters: ast.NodeIndex = if (is_ts) try ts.parseTypeParameters(parser) else .null;
    if (kind == .constructor and type_parameters != .null) try parser.report(
        parser.tree.span(type_parameters),
        "Type parameters cannot appear on a constructor declaration",
        .{ .help = "Declare the type parameters on the class instead" },
    );

    const params = try functions.parseFormalParameters(
        parser,
        .unique_formal_parameters,
        kind == .constructor,
    ) orelse return null;
    _ = try functions.checkAccessorArity(parser, kind, params);

    var end = parser.tree.span(params).end;
    var return_type: ast.NodeIndex = .null;
    if (is_ts and parser.at(.colon)) {
        return_type = try ts.parseReturnTypeAnnotation(parser) orelse return null;
        end = parser.tree.span(return_type).end;
    }

    const body = try functions.parseFunctionBody(parser, .optional) orelse return null;
    if (body != .null) {
        end = parser.tree.span(body).end;
        // an abstract constructor is already reported as a misplaced modifier
        if (modifiers.abstract and kind != .constructor) try parser.report(
            parser.tree.span(body),
            if (kind == .method)
                try parser.fmt(
                    "Method '{s}' cannot have an implementation because it is marked abstract",
                    .{parser.spanText(parser.tree.span(key))},
                )
            else
                "An abstract accessor cannot have an implementation",
            .{ .help = "Remove the body, or remove the 'abstract' modifier" },
        );
    } else if (is_ts) {
        end = try parser.eatSemicolon(end) orelse return null;
    } else {
        return parser.failExpected("Expected '{' to start the method body", .{});
    }

    const function = try parser.tree.addNode(.{ .function = .{
        .type = if (body == .null) .ts_empty_body_function_expression else .function_expression,
        .id = .null,
        .generator = modifiers.generator,
        .async = modifiers.async,
        .params = params,
        .body = body,
        .type_parameters = type_parameters,
        .return_type = return_type,
    } }, .{ .start = function_start, .end = end });

    return try parser.tree.addNode(.{ .method_definition = .{
        .decorators = decorators,
        .key = key,
        .value = function,
        .kind = kind,
        .computed = computed,
        .static = modifiers.static,
        .override = modifiers.override,
        .optional = modifiers.optional,
        .abstract = modifiers.abstract,
        .accessibility = modifiers.accessibility,
    } }, .{ .start = start, .end = end });
}

fn parsePropertyDefinition(
    parser: *Parser,
    start: u32,
    decorators: ast.IndexRange,
    key: ast.NodeIndex,
    computed: bool,
    modifiers: Modifiers,
) Error!?ast.NodeIndex {
    if (!computed) try validateElementName(parser, key, modifiers.static, .field);

    var end = parser.prev_token_end;

    var type_annotation: ast.NodeIndex = .null;
    if (parser.tree.isTs() and parser.at(.colon)) {
        type_annotation = try ts.parseTypeAnnotation(parser) orelse return null;
        end = parser.tree.span(type_annotation).end;
    }

    var value: ast.NodeIndex = .null;
    if (try parser.eat(.assign) orelse return null) {
        // field initializers reset [Await] and [Yield]
        // https://github.com/tc39/ecma262/issues/3333
        // https://github.com/tc39/ecma262/issues/2437
        const outer = parser.enterFunction(false, false);
        defer parser.leaveFunction(outer);

        value = try expressions.parseAssignment(parser) orelse return null;
        end = parser.tree.span(value).end;

        if (modifiers.abstract) try parser.report(
            parser.tree.span(value),
            try parser.fmt(
                "Property '{s}' cannot have an initializer because it is marked abstract",
                .{parser.spanText(parser.tree.span(key))},
            ),
            .{ .help = "Remove the initializer, or remove the 'abstract' modifier" },
        );
    }

    if (modifiers.definite) try ts.checkDefiniteAssignment(
        parser,
        parser.tree.span(key),
        type_annotation != .null,
        value != .null,
    );

    if (try parser.eat(.semicolon) orelse return null) {
        end = parser.prev_token_end;
    } else if (!parser.canInsertSemicolon()) {
        return parser.failExpected("Expected ';' after the class field", .{});
    }

    return try parser.tree.addNode(.{ .property_definition = .{
        .decorators = decorators,
        .key = key,
        .value = value,
        .computed = computed,
        .static = modifiers.static,
        .accessor = modifiers.accessor,
        .type_annotation = type_annotation,
        .declare = modifiers.declare,
        .override = modifiers.override,
        .optional = modifiers.optional,
        .definite = modifiers.definite,
        .readonly = modifiers.readonly,
        .abstract = modifiers.abstract,
        .accessibility = modifiers.accessibility,
    } }, .{ .start = start, .end = end });
}

fn detectConstructor(parser: *Parser, key: ast.NodeIndex, modifiers: *Modifiers) Error!void {
    const data = parser.tree.data(key);
    if (data == .private_identifier) {
        if (std.mem.eql(u8, parser.tree.string(data.private_identifier.name), "constructor")) {
            try parser.report(
                parser.tree.span(key),
                "Classes may not have a private member named '#constructor'",
                .{ .help = "Rename the private member" },
            );
        }
        return;
    }
    if (modifiers.static) return;
    const prop = static_semantics.propName(&parser.tree, key) orelse return;
    if (!prop.eql("constructor")) return;

    switch (modifiers.kind) {
        .method => modifiers.kind = .constructor,
        .get, .set => try parser.report(
            prop.span,
            "Class constructor may not be an accessor",
            .{ .help = "Remove the 'get' or 'set' keyword" },
        ),
        .constructor => {},
    }
}

fn validateMethodModifiers(parser: *Parser, key: ast.NodeIndex, modifiers: Modifiers) Error!void {
    const is_constructor = modifiers.kind == .constructor;
    const is_generator = modifiers.generator;
    const checks = [_]struct { bool, []const u8, []const u8 }{
        .{
            modifiers.declare,
            "'declare' modifier can only appear on a property declaration",
            "Remove 'declare'",
        },
        .{
            modifiers.readonly,
            "'readonly' modifier can only appear on a property declaration or index signature",
            "Remove 'readonly'",
        },
        .{
            is_constructor and modifiers.abstract,
            "'abstract' modifier cannot appear on a constructor declaration",
            "Remove 'abstract'",
        },
        .{
            is_constructor and modifiers.async,
            "'async' modifier cannot appear on a constructor declaration",
            "Remove 'async'",
        },
        .{
            is_constructor and is_generator,
            "Class constructor may not be a generator",
            "Remove the '*'",
        },
        .{
            is_generator and modifiers.kind == .get,
            "A 'get' accessor cannot be a generator",
            "Remove the '*'",
        },
        .{
            is_generator and modifiers.kind == .set,
            "A 'set' accessor cannot be a generator",
            "Remove the '*'",
        },
    };
    for (checks) |check| {
        if (check[0]) try parser.report(parser.tree.span(key), check[1], .{ .help = check[2] });
    }
}

fn validateElementName(
    parser: *Parser,
    key: ast.NodeIndex,
    is_static: bool,
    comptime kind: enum { method, field },
) Error!void {
    const prop = static_semantics.propName(&parser.tree, key) orelse return;
    if (is_static and prop.eql("prototype")) {
        try parser.report(
            prop.span,
            "Classes may not have a static property named 'prototype'",
            .{ .help = "Rename the property, or remove 'static'" },
        );
    } else if (kind == .field and prop.eql("constructor")) {
        try parser.report(
            prop.span,
            if (is_static)
                "Classes may not have a static field named 'constructor'"
            else
                "Classes may not have a field named 'constructor'",
            .{ .help = "Rename the field" },
        );
    }
}
