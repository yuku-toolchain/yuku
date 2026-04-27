const std = @import("std");
const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;
const TokenTag = @import("../token.zig").TokenTag;
const Precedence = @import("../token.zig").Precedence;

const literals = @import("literals.zig");
const patterns = @import("patterns.zig");
const functions = @import("functions.zig");
const expressions = @import("expressions.zig");
const statements = @import("statements.zig");
const extensions = @import("extensions.zig");
const ts_types = @import("ts/types.zig");
const ts_statements = @import("ts/statements.zig");
const ts_signatures = @import("ts/signatures.zig");
const ecmascript = @import("../ecmascript.zig");

//
// class declaration or expression
// https://tc39.es/ecma262/#sec-class-definitions
//

pub const ParseClassOpts = struct {
    // class appears in expression position
    is_expression: bool = false,
    // `export default class` allows an optional name but still produces a
    // `ClassDeclaration` in the AST.
    is_default_export: bool = false,
    // `declare class Foo {}`
    is_declare: bool = false,
    // `abstract class Foo {}`
    is_abstract: bool = false,
};

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

    const class_type: ast.ClassType = if (opts.is_expression and !opts.is_default_export)
        .class_expression
    else
        .class_declaration;

    if (class_type == .class_declaration and parser.context.in_single_statement_context) {
        @branchHint(.unlikely);
        try parser.report(
            .{ .start = start, .end = parser.current_token.span.end },
            "Class declarations are not allowed in single-statement contexts",
            .{ .help = "Wrap the class declaration in a block: { class C {} }" },
        );
    }

    var id: ast.NodeIndex = .null;

    if (try canStartClassName(parser)) {
        id = try literals.parseBindingIdentifier(parser) orelse .null;
    }

    if (id == .null and !opts.is_expression and !opts.is_default_export and !parser.tree.isTs()) {
        try parser.report(
            parser.current_token.span,
            "Class declaration requires a name",
            .{ .help = "Add a name after 'class', e.g. 'class MyClass {}'." },
        );
        return null;
    }

    // `class Foo<T, U extends V> ...`
    const type_parameters: ast.NodeIndex = if (parser.tree.isTs())
        try ts_types.parseTypeParameters(parser)
    else
        .null;

    // `extends expr <T>`, a trailing bare `<T>` rides the lhs as a
    // `TSInstantiationExpression` (when committed) or sits at the cursor
    // (when rewound on same-line `{`), both split into the class fields.
    var super_class: ast.NodeIndex = .null;
    var super_type_arguments: ast.NodeIndex = .null;

    if (parser.current_token.tag == .extends) {
        try parser.advance() orelse return null;
        super_class = try expressions.parseLeftHandSideExpression(parser, .extends_clause) orelse return null;
        if (parser.tree.isTs()) switch (parser.tree.getData(super_class)) {
            .ts_instantiation_expression => |inst| {
                super_class = inst.expression;
                super_type_arguments = inst.type_arguments;
            },
            else => if (parser.current_token.tag == .less_than) {
                super_type_arguments = try ts_types.parseTypeArguments(parser);
            },
        };
    }

    // `implements A, B.C<T>, ...`. may appear with or without a preceding
    // `extends` clause.
    const implements: ast.IndexRange = if (parser.tree.isTs())
        try ts_statements.parseImplementsClause(parser) orelse return null
    else
        .empty;

    const body = try parseClassBody(parser) orelse return null;

    return try parser.tree.createNode(.{ .class = .{
        .type = class_type,
        .decorators = decorators,
        .id = id,
        .super_class = super_class,
        .body = body,
        .type_parameters = type_parameters,
        .super_type_arguments = super_type_arguments,
        .implements = implements,
        .declare = opts.is_declare,
        .abstract = opts.is_abstract,
    } }, .{ .start = start, .end = parser.tree.getSpan(body).end });
}

fn canStartClassName(parser: *Parser) Error!bool {
    const tag = parser.current_token.tag;
    if (!tag.isIdentifierLike() or tag == .extends) return false;
    if (parser.tree.isTs() and tag == .implements) {
        const next = (try parser.peekAhead()) orelse return true;
        return !next.tag.isIdentifierLike();
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
        // stray `;` between elements.
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

    return try parser.tree.createNode(.{
        .class_body = .{ .body = try parser.createExtraFromScratch(&parser.scratch_a, checkpoint) },
    }, .{ .start = start, .end = end });
}

//
// class element: method, field, accessor, or static block
//

fn parseClassElement(parser: *Parser) Error!?ast.NodeIndex {
    const elem_start = parser.current_token.span.start;
    const decorators: ast.IndexRange = if (parser.current_token.tag == .at)
        try extensions.parseDecorators(parser) orelse return null
    else
        ast.IndexRange.empty;

    // `static { ... }` short-circuits before modifier parsing
    if (try tryStaticBlock(parser, decorators)) |block| return block;

    // gather modifiers. a modifier word is reinterpreted as the element key
    // when followed by a token that cannot continue a modifier, a line
    // terminator where ts forbids one, or a duplicate modifier
    var mods: Modifiers = .{};
    var key: ast.NodeIndex = .null;
    while (true) switch (try consumeModifier(parser, &mods)) {
        .consumed => continue,
        .key => |k| {
            key = k;
            break;
        },
        .none => break,
    };

    // `*` generator mark
    if (key == .null and parser.current_token.tag == .star) {
        mods.is_generator = true;
        try parser.advance() orelse return null;
    }

    // `[k: T]: V` index signature shares the `[` opener with computed keys.
    if (key == .null and
        parser.tree.isTs() and
        parser.current_token.tag == .left_bracket and
        try ts_signatures.isIndexSignatureStart(parser))
    {
        return parseIndexSignatureElement(parser, elem_start, decorators, mods);
    }

    // key
    var computed = false;
    if (key == .null) {
        const parsed = try parseClassElementKey(parser) orelse return null;
        key = parsed.key;
        computed = parsed.computed;
    }

    // post-key markers `?` (optional) and `!` (definite assignment).
    var optional = false;
    var definite = false;
    if (parser.tree.isTs()) switch (parser.current_token.tag) {
        .question => {
            optional = true;
            try parser.advance() orelse return null;
        },
        .logical_not => if (!parser.current_token.hasLineTerminatorBefore()) {
            definite = true;
            try parser.advance() orelse return null;
        },
        else => {},
    };

    try validatePrivateConstructor(parser, key, computed);
    try detectConstructorKind(parser, key, &mods, computed);

    // dispatch on the shape of what follows the key. `(` always starts a
    // method parameter list. in ts a leading `<` is a generic method's
    // type parameter list, so dispatch there too.
    const is_method_start = parser.current_token.tag == .left_paren or
        (parser.tree.isTs() and parser.current_token.tag == .less_than);

    if (is_method_start) {
        if (mods.is_accessor) {
            try parser.report(
                parser.current_token.span,
                "Accessor properties cannot be methods",
                .{ .help = "Remove the parentheses to declare an auto-accessor field." },
            );
            return null;
        }
        if (definite) try parser.report(
            parser.tree.getSpan(key),
            "Method cannot have a definite assignment assertion",
            .{ .help = "Remove the '!' or declare a property instead." },
        );
        return parseMethodDefinition(parser, elem_start, decorators, key, computed, mods, optional);
    }

    if (mods.is_async or mods.is_generator) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '(' for method definition",
            .{ .help = "Method definitions require a parameter list. Use 'method() {}' syntax." },
        );
        return null;
    }

    if (mods.kind != .method) {
        try parser.reportExpected(
            parser.current_token.span,
            "Expected '(' for getter/setter definition",
            .{ .help = "Getters and setters require parentheses. Use 'get prop() {}' or 'set prop(value) {}' syntax." },
        );
        return null;
    }

    return parsePropertyDefinition(parser, elem_start, decorators, key, computed, mods, optional, definite);
}

fn parseIndexSignatureElement(
    parser: *Parser,
    elem_start: u32,
    decorators: ast.IndexRange,
    mods: Modifiers,
) Error!?ast.NodeIndex {
    if (decorators.len != 0) {
        const first = parser.tree.getExtra(decorators)[0];
        try parser.report(
            parser.tree.getSpan(first),
            "Decorators cannot be applied to an index signature",
            .{},
        );
    }

    if (mods.is_async or mods.is_generator or mods.is_accessor or
        mods.kind != .method or mods.declare or mods.abstract or mods.override)
    {
        try parser.report(
            .{ .start = elem_start, .end = parser.current_token.span.end },
            "Index signatures only accept 'readonly', 'static', and accessibility modifiers",
            .{},
        );
    }

    const node = try ts_signatures.parseIndexSignature(parser, elem_start, .{
        .readonly = mods.readonly,
        .static = mods.is_static,
    }) orelse return null;

    // fold the trailing `;` into the span
    if (parser.current_token.tag == .semicolon) {
        const span = parser.tree.getSpan(node);
        parser.tree.replaceSpan(node, .{ .start = span.start, .end = parser.current_token.span.end });
        try parser.advance() orelse return null;
    }

    return node;
}

//
// modifier parsing
//

// every modifier a class element can carry. `static`, `is_async`,
// `is_generator`, `is_accessor` and the `get` / `set` kinds apply to
// plain js, the remaining fields plus `accessibility` are ts only.
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
};

const ModifierStep = union(enum) {
    // a modifier was consumed, keep looping.
    consumed,
    // a modifier-like word was actually the element key. stop.
    key: ast.NodeIndex,
    // the current token is not a modifier at all. stop.
    none,
};

// consumes one class element modifier, or recognizes a modifier-like
// word as the element key.
fn consumeModifier(parser: *Parser, mods: *Modifiers) Error!ModifierStep {
    const token = parser.current_token;
    if (!isModifier(token.tag, parser.tree.isTs())) return .none;

    const next = try parser.peekAhead() orelse return .none;

    const is_modifier =
        canStartElementKey(next.tag) and
        !(requiresSameLine(token.tag) and next.hasLineTerminatorBefore()) and
        !isSet(mods.*, token.tag);

    if (!is_modifier) {
        try parser.advanceWithoutEscapeCheck() orelse return .none;
        const key = try parser.tree.createNode(
            .{ .identifier_name = .{ .name = try parser.identifierName(token) } },
            token.span,
        );
        return .{ .key = key };
    }

    try parser.reportIfEscapedKeyword(token);
    try parser.advanceWithoutEscapeCheck() orelse return .none;
    apply(mods, token.tag);
    return .consumed;
}

// `static { ... }` fires before any modifier is consumed, so a decorated
// static block is a parse error, reported here so the block still parses.
fn tryStaticBlock(parser: *Parser, decorators: ast.IndexRange) Error!?ast.NodeIndex {
    if (parser.current_token.tag != .static) return null;
    const next = try parser.peekAhead() orelse return null;
    if (next.tag != .left_brace) return null;

    const static_token = parser.current_token;
    if (decorators.len != 0) {
        const first = parser.tree.getExtra(decorators)[0];
        try parser.report(
            parser.tree.getSpan(first),
            "Decorators cannot be applied to static blocks",
            .{ .help = "Remove the decorator or apply it to a method or field instead." },
        );
    }
    try parser.reportIfEscapedKeyword(static_token);
    try parser.advanceWithoutEscapeCheck() orelse return null;
    return parseStaticBlock(parser, static_token.span.start);
}

// tags that could serve as a class element modifier. `static`, `async`,
// `get`, `set`, and `accessor` are plain js, everything else requires ts.
inline fn isModifier(tag: TokenTag, is_ts: bool) bool {
    return switch (tag) {
        .static, .async, .get, .set, .accessor => true,
        .declare, .public, .private, .protected, .override, .readonly, .abstract => is_ts,
        else => false,
    };
}

// tokens that can legally start a class element key, including `*` for
// a generator method and `[` for a computed key. these are also the
// tokens that may legitimately follow a modifier.
inline fn canStartElementKey(tag: TokenTag) bool {
    return tag.isIdentifierLike() or
        tag.isNumericLiteral() or
        tag == .string_literal or
        tag == .private_identifier or
        tag == .left_bracket or
        tag == .star;
}

// ts modifiers and `accessor` require the next token on the same line.
// `abstract\n foo()` parses as `abstract` + `foo()`, not a single
// abstract method. plain js modifiers stay permissive.
inline fn requiresSameLine(tag: TokenTag) bool {
    return switch (tag) {
        .static, .async, .get, .set => false,
        else => true,
    };
}

// whether `tag` would apply a modifier that `m` already carries.
// duplicate modifiers are reinterpreted as the key name.
fn isSet(m: Modifiers, tag: TokenTag) bool {
    return switch (tag) {
        .static => m.is_static,
        .async => m.is_async,
        .accessor => m.is_accessor,
        .get => m.kind == .get,
        .set => m.kind == .set,
        .declare => m.declare,
        .abstract => m.abstract,
        .override => m.override,
        .readonly => m.readonly,
        .public, .private, .protected => m.accessibility != .none,
        else => unreachable,
    };
}

// commits a modifier tag to `m`. pair with `isSet` and `isModifier`.
fn apply(m: *Modifiers, tag: TokenTag) void {
    switch (tag) {
        .static => m.is_static = true,
        .async => m.is_async = true,
        .accessor => m.is_accessor = true,
        .get => m.kind = .get,
        .set => m.kind = .set,
        .declare => m.declare = true,
        .abstract => m.abstract = true,
        .override => m.override = true,
        .readonly => m.readonly = true,
        .public => m.accessibility = .public,
        .private => m.accessibility = .private,
        .protected => m.accessibility = .protected,
        else => unreachable,
    }
}

//
// element key
//

const KeyResult = struct { key: ast.NodeIndex, computed: bool };

fn parseClassElementKey(parser: *Parser) Error!?KeyResult {
    const token = parser.current_token;

    if (token.tag == .left_bracket) {
        try parser.advance() orelse return null;
        const key = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        if (!try parser.expect(.right_bracket, "Expected ']' after computed property key", null)) return null;
        return .{ .key = key, .computed = true };
    }

    if (token.tag == .private_identifier) {
        const key = try literals.parsePrivateIdentifier(parser) orelse return null;
        return .{ .key = key, .computed = false };
    }

    if (token.tag == .string_literal) {
        const key = try literals.parseStringLiteral(parser) orelse return null;
        return .{ .key = key, .computed = false };
    }

    if (token.tag.isNumericLiteral()) {
        const key = try literals.parseNumericLiteral(parser) orelse return null;
        return .{ .key = key, .computed = false };
    }

    if (token.tag.isIdentifierLike()) {
        try parser.advanceWithoutEscapeCheck() orelse return null;
        const key = try parser.tree.createNode(
            .{ .identifier_name = .{ .name = try parser.identifierName(token) } },
            token.span,
        );
        return .{ .key = key, .computed = false };
    }

    try parser.report(
        token.span,
        try parser.fmt("Unexpected token '{s}' as class element key", .{parser.describeToken(token)}),
        .{ .help = "Class element keys must be identifiers, strings, numbers, private identifiers (#name), or computed expressions [expr]." },
    );
    return null;
}

//
// member definitions
//

// method, getter, setter, or constructor. in ts mode a missing body is
// valid and produces a bodyless `TSEmptyBodyFunctionExpression`, which
// covers overload signatures, ambient members, and abstract methods.
fn parseMethodDefinition(
    parser: *Parser,
    elem_start: u32,
    decorators: ast.IndexRange,
    key: ast.NodeIndex,
    computed: bool,
    mods: Modifiers,
    optional: bool,
) Error!?ast.NodeIndex {
    if (mods.is_static and !computed) try validateStaticPrototypeOrConstructor(parser, key, .method);
    try validateMethodModifiers(parser, key, mods);

    const saved_await = parser.context.await_is_keyword;
    const saved_yield = parser.context.yield_is_keyword;
    parser.context.await_is_keyword = mods.is_async;
    parser.context.yield_is_keyword = mods.is_generator;
    defer {
        parser.context.await_is_keyword = saved_await;
        parser.context.yield_is_keyword = saved_yield;
    }

    const func_start = parser.current_token.span.start;

    // `m<T, U extends V>(...)`
    const type_parameters: ast.NodeIndex = if (parser.tree.isTs())
        try ts_types.parseTypeParameters(parser)
    else
        .null;

    const params = try functions.parseFormalParameters(parser, .unique_formal_parameters, mods.kind == .constructor) orelse return null;
    try validateGetSetParams(parser, mods.kind, params);
    const params_end = parser.tree.getSpan(params).end;

    // optional `: ReturnType` annotation.
    var return_type: ast.NodeIndex = .null;
    var return_type_end: u32 = params_end;
    if (parser.tree.isTs() and parser.current_token.tag == .colon) {
        return_type = try ts_types.parseReturnTypeAnnotation(parser) orelse return null;
        return_type_end = parser.tree.getSpan(return_type).end;
    }

    // body. required in js. in ts a missing body folds into a bodyless
    // function terminated by `;` or ASI.
    var body: ast.NodeIndex = .null;
    var function_type: ast.FunctionType = .function_expression;
    var end: u32 = return_type_end;

    if (parser.current_token.tag == .left_brace) {
        body = try functions.parseFunctionBody(parser) orelse return null;
        end = parser.tree.getSpan(body).end;
    } else if (parser.tree.isTs()) {
        function_type = .ts_empty_body_function_expression;
        end = try parser.eatSemicolon(return_type_end) orelse return null;
    } else {
        try parser.reportExpected(parser.current_token.span, "Expected '{' to start method body", .{});
        return null;
    }

    const func = try parser.tree.createNode(.{ .function = .{
        .type = function_type,
        .id = .null,
        .generator = mods.is_generator,
        .async = mods.is_async,
        .params = params,
        .body = body,
        .type_parameters = type_parameters,
        .return_type = return_type,
    } }, .{ .start = func_start, .end = end });

    return try parser.tree.createNode(.{ .method_definition = .{
        .decorators = decorators,
        .key = key,
        .value = func,
        .kind = mods.kind,
        .computed = computed,
        .static = mods.is_static,
        .override = mods.override,
        .optional = optional,
        .abstract = mods.abstract,
        .accessibility = mods.accessibility,
    } }, .{ .start = elem_start, .end = end });
}

// class field or auto-accessor (`accessor x = 1`).
fn parsePropertyDefinition(
    parser: *Parser,
    elem_start: u32,
    decorators: ast.IndexRange,
    key: ast.NodeIndex,
    computed: bool,
    mods: Modifiers,
    optional: bool,
    definite: bool,
) Error!?ast.NodeIndex {
    if (!computed) {
        if (mods.is_static)
            try validateStaticPrototypeOrConstructor(parser, key, .field)
        else
            try validateFieldConstructor(parser, key);
    }

    var end = parser.prev_token_end;

    // optional `: Type` annotation.
    var type_annotation: ast.NodeIndex = .null;
    if (parser.tree.isTs() and parser.current_token.tag == .colon) {
        type_annotation = try ts_types.parseTypeAnnotation(parser) orelse return null;
        end = parser.tree.getSpan(type_annotation).end;
    }

    // `= initializer`
    var value: ast.NodeIndex = .null;
    if (parser.current_token.tag == .assign) {
        try parser.advance() orelse return null;
        value = try expressions.parseExpression(parser, Precedence.Assignment, .{}) orelse return null;
        end = parser.tree.getSpan(value).end;
    }

    // terminator
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

    return try parser.tree.createNode(.{ .property_definition = .{
        .decorators = decorators,
        .key = key,
        .value = value,
        .computed = computed,
        .static = mods.is_static,
        .accessor = mods.is_accessor,
        .type_annotation = type_annotation,
        .declare = mods.declare,
        .override = mods.override,
        .optional = optional,
        .definite = definite,
        .readonly = mods.readonly,
        .abstract = mods.abstract,
        .accessibility = mods.accessibility,
    } }, .{ .start = elem_start, .end = end });
}

//
// static block
//

fn parseStaticBlock(parser: *Parser, start: u32) Error!?ast.NodeIndex {
    if (!try parser.expect(.left_brace, "Expected '{' to start static block", null)) return null;

    // ClassStaticBlockStatementList: StatementList[~Yield, +Await, ~Return]
    const saved_await = parser.context.await_is_keyword;
    const saved_yield = parser.context.yield_is_keyword;
    const saved_return = parser.context.allow_return_statement;
    parser.context.await_is_keyword = true;
    parser.context.yield_is_keyword = false;
    parser.context.allow_return_statement = false;
    defer {
        parser.context.await_is_keyword = saved_await;
        parser.context.yield_is_keyword = saved_yield;
        parser.context.allow_return_statement = saved_return;
    }

    const body = try parser.parseBody(.right_brace, .other);
    const end = parser.current_token.span.end;
    if (!try parser.expect(.right_brace, "Expected '}' to close static block", null)) return null;

    return try parser.tree.createNode(
        .{ .static_block = .{ .body = body } },
        .{ .start = start, .end = end },
    );
}

//
// validation
//

// `#constructor` is not allowed as a private field name
fn validatePrivateConstructor(parser: *Parser, key: ast.NodeIndex, computed: bool) Error!void {
    if (computed) return;
    const data = parser.tree.getData(key);
    if (data != .private_identifier) return;
    if (!std.mem.eql(u8, parser.tree.getString(data.private_identifier.name), "constructor")) return;
    try parser.report(
        parser.tree.getSpan(key),
        "Classes can't have a private field named '#constructor'",
        .{ .help = "Use a different name for this private member." },
    );
}

// detects an unqualified `constructor` name and promotes `kind` from
// `.method` to `.constructor`. `static` and computed keys never count
fn detectConstructorKind(parser: *Parser, key: ast.NodeIndex, mods: *Modifiers, computed: bool) Error!void {
    if (mods.is_static or computed) return;
    const prop = ecmascript.propName(&parser.tree, key) orelse return;
    if (!prop.eql("constructor")) return;

    switch (mods.kind) {
        .method => mods.kind = .constructor,
        .get, .set => try parser.report(
            prop.span,
            "Constructor can't have get/set modifier",
            .{ .help = "Remove the get/set keyword from the constructor." },
        ),
        .constructor => {},
    }
}

// forbids `async` / `*` on constructors and `*` on getters / setters.
fn validateMethodModifiers(parser: *Parser, key: ast.NodeIndex, mods: Modifiers) Error!void {
    const span = parser.tree.getSpan(key);
    if (mods.kind == .constructor) {
        if (mods.is_async) try parser.report(span, "Constructor cannot be async", .{
            .help = "Remove the 'async' modifier from the constructor.",
        });
        if (mods.is_generator) try parser.report(span, "Constructor cannot be a generator", .{
            .help = "Remove the '*' from the constructor.",
        });
        return;
    }
    if (!mods.is_generator) return;
    switch (mods.kind) {
        .get => try parser.report(span, "Getter cannot be a generator", .{
            .help = "Remove the '*' from the getter definition.",
        }),
        .set => try parser.report(span, "Setter cannot be a generator", .{
            .help = "Remove the '*' from the setter definition.",
        }),
        else => {},
    }
}

// getters take zero parameters, setters take exactly one
fn validateGetSetParams(parser: *Parser, kind: ast.MethodDefinitionKind, params: ast.NodeIndex) Error!void {
    const data = parser.tree.getData(params).formal_parameters;
    switch (kind) {
        .get => if (data.items.len != 0 or data.rest != .null) {
            try parser.report(
                parser.tree.getSpan(params),
                "Getter must have no parameters",
                .{ .help = "Remove all parameters from the getter." },
            );
        },
        .set => if (data.items.len != 1 or data.rest != .null) {
            try parser.report(
                parser.tree.getSpan(params),
                "Setter must have exactly one parameter",
                .{ .help = "Setters accept exactly one argument." },
            );
        },
        else => {},
    }
}

// static members may not be named `prototype`, static fields may not be
// named `constructor`
fn validateStaticPrototypeOrConstructor(
    parser: *Parser,
    key: ast.NodeIndex,
    kind: enum { method, field },
) Error!void {
    const prop = ecmascript.propName(&parser.tree, key) orelse return;
    if (prop.eql("prototype")) try parser.report(
        prop.span,
        "Classes may not have a static property named 'prototype'",
        .{ .help = "Remove 'static' or rename the property." },
    ) else if (kind == .field and prop.eql("constructor")) try parser.report(
        prop.span,
        "Classes may not have a static field named 'constructor'",
        .{ .help = "Remove 'static' or rename the field." },
    );
}

// non-static fields may not be named `constructor`
fn validateFieldConstructor(parser: *Parser, key: ast.NodeIndex) Error!void {
    const prop = ecmascript.propName(&parser.tree, key) orelse return;
    if (!prop.eql("constructor")) return;
    try parser.report(
        prop.span,
        "Classes may not have a non-static field named 'constructor'",
        .{ .help = "Rename the field or make it a method." },
    );
}
