const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const TokenTag = @import("../token.zig").TokenTag;
const Precedence = @import("../token.zig").Precedence;

const literals = @import("literals.zig");
const functions = @import("functions.zig");
const expressions = @import("expressions.zig");
const object = @import("object.zig");
const decorator = @import("decorators.zig");
const ts = @import("ts/types.zig");
const ts_decl = @import("ts/statements.zig");
const static_semantics = @import("spec").static_semantics;

pub const ParseClassOptions = struct {
    is_expression: bool = false,
    /// Set for `export default class`, which may omit the name yet stays a declaration.
    is_default_export: bool = false,
    is_declare: bool = false,
    is_abstract: bool = false,
};

/// https://tc39.es/ecma262/#sec-class-definitions
pub fn parseClass(
    parser: *Parser,
    options: ParseClassOptions,
    start_from_param: ?u32,
) Error!?ast.NodeIndex {
    return parseClassDecorated(parser, options, start_from_param, .empty);
}

pub fn parseClassDecorated(
    parser: *Parser,
    options: ParseClassOptions,
    start_from_param: ?u32,
    decorators: ast.IndexRange,
) Error!?ast.NodeIndex {
    std.debug.assert(start_from_param != null or
        parser.current_token.tag == .class or
        parser.current_token.tag == .abstract or
        parser.current_token.tag == .declare);
    const start = start_from_param orelse parser.current_token.span.start;
    const is_ts = parser.tree.isTs();

    var is_abstract = options.is_abstract;
    var is_declare = options.is_declare;
    if (is_ts and !options.is_expression and isClassModifiersNext(parser)) {
        while (parser.current_token.tag != .class) {
            std.debug.assert(parser.current_token.tag == .abstract or
                parser.current_token.tag == .declare);
            if (parser.current_token.tag == .abstract) is_abstract = true else is_declare = true;
            try parser.advance() orelse return null;
        }
    }
    const saved_ambient = parser.ts_context.ambient;
    defer parser.ts_context.ambient = saved_ambient;
    if (is_declare) parser.ts_context.ambient = true;

    if (!try parser.expect(.class, "Expected 'class' keyword", null)) return null;

    const class_type: ast.ClassType = if (options.is_expression and !options.is_default_export)
        .class_expression
    else
        .class_declaration;

    if (class_type == .class_declaration and parser.context.single_statement) {
        @branchHint(.unlikely);
        try parser.report(
            .{ .start = start, .end = parser.current_token.span.end },
            "Class declarations are not allowed in single-statement contexts",
            .{ .help = "Wrap the class declaration in a block: { class C {} }" },
        );
    }

    const id: ast.NodeIndex = if (canStartClassName(parser))
        try literals.parseBindingIdentifier(parser) orelse .null
    else
        .null;

    if (id == .null and !options.is_expression and !options.is_default_export) {
        try parser.report(
            parser.current_token.span,
            "Class declaration requires a name",
            .{ .help = "Add a name after 'class', e.g. 'class MyClass {}'." },
        );
        return null;
    }

    const type_parameters: ast.NodeIndex = if (is_ts) try ts.parseTypeParameters(parser) else .null;

    // `extends Base<T> {` rewinds the instantiation expression and leaves `<T>` here
    var super_class: ast.NodeIndex = .null;
    var super_type_arguments: ast.NodeIndex = .null;

    if (parser.current_token.tag == .extends) {
        try parser.advance() orelse return null;
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

    return try parser.tree.addNode(.{ .class = .{
        .type = class_type,
        .decorators = decorators,
        .id = id,
        .super_class = super_class,
        .body = body,
        .type_parameters = type_parameters,
        .super_type_arguments = super_type_arguments,
        .implements = implements,
        .declare = is_declare,
        .abstract = is_abstract,
    } }, .{ .start = start, .end = parser.tree.span(body).end });
}

/// Returns whether `declare` and `abstract`, each at most once and in either order, then
/// `class` follow on one line.
pub fn isClassModifiersNext(parser: *Parser) bool {
    var peek = parser.beginPeek();
    defer peek.end();

    var seen_declare = false;
    var seen_abstract = false;
    var token = parser.current_token;
    // each modifier at most once bounds the loop
    while (true) {
        switch (token.tag) {
            .declare => {
                if (seen_declare) return false;
                seen_declare = true;
            },
            .abstract => {
                if (seen_abstract) return false;
                seen_abstract = true;
            },
            .class => return seen_declare or seen_abstract,
            else => return false,
        }
        token = peek.next();
        if (token.hasLineTerminatorBefore()) return false;
    }
}

/// Returns whether the current `abstract` modifies a `class` on the same line.
pub inline fn isAbstractClassNext(parser: *Parser) bool {
    std.debug.assert(parser.current_token.tag == .abstract);
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
    if (!try parser.expect(
        .left_brace,
        "Expected '{' to start class body",
        "Class body must be enclosed in braces: class Name { ... }",
    )) return null;

    const checkpoint = parser.scratch_a.begin();
    defer parser.scratch_a.reset(checkpoint);

    while (parser.current_token.tag != .right_brace and parser.current_token.tag != .eof) {
        if (parser.current_token.tag == .semicolon) {
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

    return try parser.tree.addNode(.{
        .class_body = .{ .body = try parser.flushToExtras(&parser.scratch_a, checkpoint) },
    }, .{ .start = start, .end = end });
}

fn parseClassElement(parser: *Parser) Error!?ast.NodeIndex {
    const is_ts = parser.tree.isTs();
    const element_start = parser.current_token.span.start;
    const decorators: ast.IndexRange = if (parser.current_token.tag == .at)
        try decorator.parseDecorators(parser) orelse return null
    else
        .empty;

    if (try tryStaticBlock(parser, decorators)) |block| return block;

    var modifiers: Modifiers = .{};
    var key = try parseModifiers(parser, &modifiers);

    if (key == .null and parser.current_token.tag == .star) {
        modifiers.is_generator = true;
        try parser.advance() orelse return null;
    }

    if (key == .null and
        is_ts and
        parser.current_token.tag == .left_bracket and
        ts.isIndexSignatureStart(parser))
    {
        return parseIndexSignatureElement(parser, element_start, decorators, modifiers);
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

    const is_method_start = parser.current_token.tag == .left_paren or
        (is_ts and parser.current_token.tag == .less_than);

    if (is_method_start) {
        if (modifiers.is_accessor) {
            try parser.report(
                parser.current_token.span,
                "Accessor properties cannot be methods",
                .{ .help = "Remove the parentheses to declare an auto-accessor field." },
            );
            return null;
        }
        if (modifiers.definite) {
            try parser.report(
                parser.tree.span(key),
                "Method cannot have a definite assignment assertion",
                .{ .help = "Remove the '!' or declare a property instead." },
            );
        }
        return parseMethodDefinition(parser, element_start, decorators, key, computed, modifiers);
    }

    if (modifiers.is_async or modifiers.is_generator) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '(' for method definition",
            .{ .help = "Method definitions require a parameter list. Use 'method() {}' syntax." },
        );
        return null;
    }

    if (modifiers.kind != .method) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '(' for getter/setter definition",
            .{ .help = "Getters and setters require parentheses." ++
                " Use 'get prop() {}' or 'set prop(value) {}' syntax." },
        );
        return null;
    }

    return parsePropertyDefinition(parser, element_start, decorators, key, computed, modifiers);
}

fn parseIndexSignatureElement(
    parser: *Parser,
    element_start: u32,
    decorators: ast.IndexRange,
    modifiers: Modifiers,
) Error!?ast.NodeIndex {
    if (decorators.len != 0) {
        const first = parser.tree.extra(decorators)[0];
        try parser.report(
            parser.tree.span(first),
            "Decorators cannot be applied to an index signature",
            .{},
        );
    }

    if (modifiers.is_async or modifiers.is_generator or modifiers.is_accessor or
        modifiers.kind != .method or modifiers.declare or modifiers.abstract or modifiers.override)
    {
        try parser.report(
            .{ .start = element_start, .end = parser.current_token.span.end },
            "Index signatures only accept 'readonly', 'static', and accessibility modifiers",
            .{},
        );
    }

    const node = try ts.parseIndexSignature(parser, element_start, .{
        .readonly = modifiers.readonly,
        .static = modifiers.is_static,
    }) orelse return null;

    if (parser.current_token.tag == .semicolon) {
        const span = parser.tree.span(node);
        parser.tree.setSpan(node, .{ .start = span.start, .end = parser.current_token.span.end });
        try parser.advance() orelse return null;
    }

    return node;
}

const Modifiers = struct {
    is_static: bool = false,
    is_async: bool = false,
    is_generator: bool = false,
    is_accessor: bool = false,
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
        if (self.is_accessor) return true;
        return switch (tag) {
            .async, .get, .set => self.is_async or self.kind != .method,
            .static => self.is_static,
            .accessor => self.is_accessor,
            .declare => self.declare,
            .abstract => self.abstract,
            .override => self.override,
            .readonly => self.readonly,
            .public, .private, .protected => self.accessibility != .none,
            else => unreachable,
        };
    }

    fn set(self: *Modifiers, tag: TokenTag) void {
        switch (tag) {
            .static => self.is_static = true,
            .async => self.is_async = true,
            .accessor => self.is_accessor = true,
            .get => self.kind = .get,
            .set => self.kind = .set,
            .declare => self.declare = true,
            .abstract => self.abstract = true,
            .override => self.override = true,
            .readonly => self.readonly = true,
            .public => self.accessibility = .public,
            .private => self.accessibility = .private,
            .protected => self.accessibility = .protected,
            else => unreachable,
        }
    }
};

// a modifier word becomes the key when what follows cannot continue a modifier
fn parseModifiers(parser: *Parser, modifiers: *Modifiers) Error!ast.NodeIndex {
    while (isModifier(parser.current_token.tag, parser.tree.isTs())) {
        const token = parser.current_token;
        const next = parser.peekAhead();

        // an accessor can never be a generator, so `get *` keeps `get` as a field key
        const accessor_before_star = next.tag == .star and
            (token.tag == .get or token.tag == .set);

        const is_modifier =
            canStartElementKey(next.tag) and
            !accessor_before_star and
            !(requiresSameLine(token.tag) and next.hasLineTerminatorBefore()) and
            !modifiers.conflicts(token.tag);
        if (!is_modifier) return try literals.parseIdentifierName(parser) orelse .null;

        try parser.reportIfEscapedKeyword(token);
        try parser.advanceWithoutEscapeCheck() orelse return .null;
        modifiers.set(token.tag);
    }
    return .null;
}

// runs before modifiers so a decorated static block still parses
fn tryStaticBlock(parser: *Parser, decorators: ast.IndexRange) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .static) return null;
    if (parser.peekAhead().tag != .left_brace) return null;

    const static_token = parser.current_token;
    if (decorators.len != 0) {
        const first = parser.tree.extra(decorators)[0];
        try parser.report(
            parser.tree.span(first),
            "Decorators cannot be applied to static blocks",
            .{ .help = "Remove the decorator or apply it to a method or field instead." },
        );
    }
    try parser.reportIfEscapedKeyword(static_token);
    try parser.advanceWithoutEscapeCheck() orelse return null;
    if (!try parser.expect(.left_brace, "Expected '{' to start static block", null)) return null;

    // ClassStaticBlockStatementList is [~Yield, +Await, ~Return]
    const saved_await = parser.context.await;
    const saved_yield = parser.context.yield;
    const saved_return = parser.context.@"return";
    parser.context.await = true;
    parser.context.yield = false;
    parser.context.@"return" = false;
    defer parser.context.await = saved_await;
    defer parser.context.yield = saved_yield;
    defer parser.context.@"return" = saved_return;

    const body = try parser.parseBody(.right_brace, .other);
    const end = parser.current_token.span.end;
    if (!try parser.expect(.right_brace, "Expected '}' to close static block", null)) return null;

    return try parser.tree.addNode(
        .{ .static_block = .{ .body = body } },
        .{ .start = static_token.span.start, .end = end },
    );
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

inline fn requiresSameLine(tag: TokenTag) bool {
    return switch (tag) {
        .static, .get, .set => false,
        else => true,
    };
}

fn parseMethodDefinition(
    parser: *Parser,
    element_start: u32,
    decorators: ast.IndexRange,
    key: ast.NodeIndex,
    computed: bool,
    modifiers: Modifiers,
) Error!?ast.NodeIndex {
    if (modifiers.is_static and !computed) try validateElementName(parser, key, true, .method);
    try validateMethodModifiers(parser, key, modifiers);

    const saved_await = parser.context.await;
    const saved_yield = parser.context.yield;
    parser.context.await = modifiers.is_async;
    parser.context.yield = modifiers.is_generator;
    defer parser.context.await = saved_await;
    defer parser.context.yield = saved_yield;

    const is_ts = parser.tree.isTs();
    const func_start = parser.current_token.span.start;

    const type_parameters: ast.NodeIndex = if (is_ts) try ts.parseTypeParameters(parser) else .null;

    if (modifiers.kind == .constructor and type_parameters != .null) {
        try parser.report(
            parser.tree.span(type_parameters),
            "Type parameters cannot appear on a constructor declaration",
            .{ .help = "Declare the type parameters on the class instead." },
        );
    }

    const params = try functions.parseFormalParameters(
        parser,
        .unique_formal_parameters,
        modifiers.kind == .constructor,
    ) orelse return null;
    _ = try functions.checkAccessorArity(parser, modifiers.kind, params);

    var return_type: ast.NodeIndex = .null;
    var return_type_end: u32 = parser.tree.span(params).end;
    if (is_ts and parser.current_token.tag == .colon) {
        return_type = try ts.parseReturnTypeAnnotation(parser) orelse return null;
        return_type_end = parser.tree.span(return_type).end;
    }

    const body = try functions.parseFunctionBody(parser, .optional) orelse return null;
    var end = return_type_end;
    if (body != .null) {
        end = parser.tree.span(body).end;
        // an abstract constructor is already reported as a misplaced modifier
        if (modifiers.abstract and modifiers.kind != .constructor) try parser.report(
            parser.tree.span(body),
            if (modifiers.kind == .method)
                "An abstract method cannot have an implementation"
            else
                "An abstract accessor cannot have an implementation",
            .{ .help = "Remove the body, or drop the 'abstract' modifier." },
        );
    } else if (is_ts) {
        end = try parser.eatSemicolon(return_type_end) orelse return null;
    } else {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '{' to start method body",
            .{},
        );
        return null;
    }

    const func = try parser.tree.addNode(.{ .function = .{
        .type = if (body == .null) .ts_empty_body_function_expression else .function_expression,
        .id = .null,
        .generator = modifiers.is_generator,
        .async = modifiers.is_async,
        .params = params,
        .body = body,
        .type_parameters = type_parameters,
        .return_type = return_type,
    } }, .{ .start = func_start, .end = end });

    return try parser.tree.addNode(.{ .method_definition = .{
        .decorators = decorators,
        .key = key,
        .value = func,
        .kind = modifiers.kind,
        .computed = computed,
        .static = modifiers.is_static,
        .override = modifiers.override,
        .optional = modifiers.optional,
        .abstract = modifiers.abstract,
        .accessibility = modifiers.accessibility,
    } }, .{ .start = element_start, .end = end });
}

fn parsePropertyDefinition(
    parser: *Parser,
    element_start: u32,
    decorators: ast.IndexRange,
    key: ast.NodeIndex,
    computed: bool,
    modifiers: Modifiers,
) Error!?ast.NodeIndex {
    if (!computed) try validateElementName(parser, key, modifiers.is_static, .field);

    var end = parser.prev_token_end;

    var type_annotation: ast.NodeIndex = .null;
    if (parser.tree.isTs() and parser.current_token.tag == .colon) {
        type_annotation = try ts.parseTypeAnnotation(parser) orelse return null;
        end = parser.tree.span(type_annotation).end;
    }

    var value: ast.NodeIndex = .null;
    if (parser.current_token.tag == .assign) {
        try parser.advance() orelse return null;

        // field initializers reset [Await] and [Yield]
        // https://github.com/tc39/ecma262/issues/3333
        // https://github.com/tc39/ecma262/issues/2437
        const saved_await = parser.context.await;
        const saved_yield = parser.context.yield;
        parser.context.await = false;
        parser.context.yield = false;
        defer parser.context.await = saved_await;
        defer parser.context.yield = saved_yield;

        value = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse
            return null;
        end = parser.tree.span(value).end;

        if (modifiers.abstract) {
            try parser.report(
                parser.tree.span(value),
                "An abstract property cannot have an initializer",
                .{ .help = "Remove the initializer, or drop the 'abstract' modifier." },
            );
        }
    }

    if (modifiers.definite) {
        try ts.checkDefiniteAssignment(
            parser,
            parser.tree.span(key),
            type_annotation != .null,
            value != .null,
        );
    }

    switch (parser.current_token.tag) {
        .semicolon => {
            end = parser.current_token.span.end;
            try parser.advance() orelse return null;
        },
        .right_brace => {},
        else => if (!parser.canInsertImplicitSemicolon(parser.current_token)) {
            try parser.reportExpected(
                parser.current_token.span,
                "Expected ';' after class field",
                .{ .help = "Add a semicolon after the field declaration." },
            );
            return null;
        },
    }

    return try parser.tree.addNode(.{ .property_definition = .{
        .decorators = decorators,
        .key = key,
        .value = value,
        .computed = computed,
        .static = modifiers.is_static,
        .accessor = modifiers.is_accessor,
        .type_annotation = type_annotation,
        .declare = modifiers.declare,
        .override = modifiers.override,
        .optional = modifiers.optional,
        .definite = modifiers.definite,
        .readonly = modifiers.readonly,
        .abstract = modifiers.abstract,
        .accessibility = modifiers.accessibility,
    } }, .{ .start = element_start, .end = end });
}

fn detectConstructor(parser: *Parser, key: ast.NodeIndex, modifiers: *Modifiers) Error!void {
    const data = parser.tree.data(key);
    if (data == .private_identifier) {
        if (std.mem.eql(u8, parser.tree.string(data.private_identifier.name), "constructor")) {
            try parser.report(
                parser.tree.span(key),
                "Classes can't have a private field named '#constructor'",
                .{ .help = "Use a different name for this private member." },
            );
        }
        return;
    }
    if (modifiers.is_static) return;
    const prop = static_semantics.propName(&parser.tree, key) orelse return;
    if (!prop.eql("constructor")) return;

    switch (modifiers.kind) {
        .method => modifiers.kind = .constructor,
        .get, .set => try parser.report(
            prop.span,
            "Constructor can't have get/set modifier",
            .{ .help = "Remove the get/set keyword from the constructor." },
        ),
        .constructor => {},
    }
}

fn validateMethodModifiers(parser: *Parser, key: ast.NodeIndex, modifiers: Modifiers) Error!void {
    const span = parser.tree.span(key);

    if (modifiers.declare) {
        try parser.report(
            span,
            "A 'declare' modifier cannot appear on a class element of this kind",
            .{ .help = "Remove 'declare'. Only class fields carry it." },
        );
    }
    if (modifiers.readonly) {
        try parser.report(
            span,
            "A 'readonly' modifier can only appear on a property declaration or index signature",
            .{ .help = "Remove 'readonly' from this method." },
        );
    }

    if (modifiers.kind == .constructor) {
        if (modifiers.abstract) try parser.report(span, "Constructor cannot be abstract", .{
            .help = "Remove the 'abstract' modifier. Only methods and properties can be abstract.",
        });
        if (modifiers.is_async) try parser.report(span, "Constructor cannot be async", .{
            .help = "Remove the 'async' modifier from the constructor.",
        });
        if (modifiers.is_generator) try parser.report(span, "Constructor cannot be a generator", .{
            .help = "Remove the '*' from the constructor.",
        });
        return;
    }
    if (!modifiers.is_generator) return;
    switch (modifiers.kind) {
        .get => try parser.report(span, "Getter cannot be a generator", .{
            .help = "Remove the '*' from the getter definition.",
        }),
        .set => try parser.report(span, "Setter cannot be a generator", .{
            .help = "Remove the '*' from the setter definition.",
        }),
        else => {},
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
            .{ .help = "Remove 'static' or rename the property." },
        );
    } else if (kind == .field and prop.eql("constructor")) {
        if (is_static) {
            try parser.report(
                prop.span,
                "Classes may not have a static field named 'constructor'",
                .{ .help = "Remove 'static' or rename the field." },
            );
        } else {
            try parser.report(
                prop.span,
                "Classes may not have a non-static field named 'constructor'",
                .{ .help = "Rename the field or make it a method." },
            );
        }
    }
}
