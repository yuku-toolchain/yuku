const std = @import("std");
const token = @import("token.zig");

pub const Comment = struct {
    type: Type,
    start: u32,
    end: u32,

    pub const Type = enum {
        line,
        block,

        pub fn toString(self: Type) []const u8 {
            return switch (self) {
                .line => "Line",
                .block => "Block",
            };
        }
    };

    pub fn getValue(self: Comment, source: []const u8) []const u8 {
        return switch (self.type) {
            // Skip "//" prefix
            .line => source[self.start + 2 .. self.end],
            // Skip "/*" prefix and "*/" suffix
            .block => source[self.start + 2 .. self.end - 2],
        };
    }
};

/// index into the ast node array. `null_node` for optional nodes.
pub const NodeIndex = u32;
pub const null_node: NodeIndex = std.math.maxInt(NodeIndex);
pub const Span = token.Span;

/// range of indices in the extra array for storing node lists.
pub const IndexRange = struct {
    start: u32,
    len: u32,

    pub const empty: IndexRange = .{ .start = 0, .len = 0 };
};

/// https://tc39.es/ecma262/#sec-binary-operators
pub const BinaryOperator = enum {
    equal, // ==
    not_equal, // !=
    strict_equal, // ===
    strict_not_equal, // !==

    less_than, // <
    less_than_or_equal, // <=
    greater_than, // >
    greater_than_or_equal, // >=

    add, // +
    subtract, // -
    multiply, // *
    divide, // /
    modulo, // %
    exponent, // **

    bitwise_or, // |
    bitwise_xor, // ^
    bitwise_and, // &
    left_shift, // <<
    right_shift, // >>
    unsigned_right_shift, // >>>

    in, // in
    instanceof, // instanceof

    pub fn fromToken(tok: token.TokenType) BinaryOperator {
        return switch (tok) {
            .equal => .equal,
            .not_equal => .not_equal,
            .strict_equal => .strict_equal,
            .strict_not_equal => .strict_not_equal,
            .less_than => .less_than,
            .less_than_equal => .less_than_or_equal,
            .greater_than => .greater_than,
            .greater_than_equal => .greater_than_or_equal,
            .plus => .add,
            .minus => .subtract,
            .star => .multiply,
            .slash => .divide,
            .percent => .modulo,
            .exponent => .exponent,
            .bitwise_or => .bitwise_or,
            .bitwise_xor => .bitwise_xor,
            .bitwise_and => .bitwise_and,
            .left_shift => .left_shift,
            .right_shift => .right_shift,
            .unsigned_right_shift => .unsigned_right_shift,
            .in => .in,
            .instanceof => .instanceof,
            else => unreachable,
        };
    }

    pub fn toToken(self: BinaryOperator) token.TokenType {
        return switch (self) {
            .equal => .equal,
            .not_equal => .not_equal,
            .strict_equal => .strict_equal,
            .strict_not_equal => .strict_not_equal,
            .less_than => .less_than,
            .less_than_or_equal => .less_than_equal,
            .greater_than => .greater_than,
            .greater_than_or_equal => .greater_than_equal,
            .add => .plus,
            .subtract => .minus,
            .multiply => .star,
            .divide => .slash,
            .modulo => .percent,
            .exponent => .exponent,
            .bitwise_or => .bitwise_or,
            .bitwise_xor => .bitwise_xor,
            .bitwise_and => .bitwise_and,
            .left_shift => .left_shift,
            .right_shift => .right_shift,
            .unsigned_right_shift => .unsigned_right_shift,
            .in => .in,
            .instanceof => .instanceof,
        };
    }

    pub fn toString(self: BinaryOperator) []const u8 {
        return self.toToken().toString().?;
    }
};

pub const LogicalOperator = enum {
    @"and", // &&
    @"or", // ||
    nullish_coalescing, // ??

    pub fn fromToken(tok: token.TokenType) LogicalOperator {
        return switch (tok) {
            .logical_and => .@"and",
            .logical_or => .@"or",
            .nullish_coalescing => .nullish_coalescing,
            else => unreachable,
        };
    }

    pub fn toToken(self: LogicalOperator) token.TokenType {
        return switch (self) {
            .@"and" => .logical_and,
            .@"or" => .logical_or,
            .nullish_coalescing => .nullish_coalescing,
        };
    }

    pub fn toString(self: LogicalOperator) []const u8 {
        return self.toToken().toString().?;
    }
};

pub const UnaryOperator = enum {
    negate, // -
    positive, // +
    logical_not, // !
    bitwise_not, // ~
    typeof, // typeof
    void, // void
    delete, // delete

    pub fn fromToken(tok: token.TokenType) UnaryOperator {
        return switch (tok) {
            .minus => .negate,
            .plus => .positive,
            .logical_not => .logical_not,
            .bitwise_not => .bitwise_not,
            .typeof => .typeof,
            .void => .void,
            .delete => .delete,
            else => unreachable,
        };
    }

    pub fn toToken(self: UnaryOperator) token.TokenType {
        return switch (self) {
            .negate => .minus,
            .positive => .plus,
            .logical_not => .logical_not,
            .bitwise_not => .bitwise_not,
            .typeof => .typeof,
            .void => .void,
            .delete => .delete,
        };
    }

    pub fn toString(self: UnaryOperator) []const u8 {
        return self.toToken().toString().?;
    }
};

pub const UpdateOperator = enum {
    increment, // ++
    decrement, // --

    pub fn fromToken(tok: token.TokenType) UpdateOperator {
        return switch (tok) {
            .increment => .increment,
            .decrement => .decrement,
            else => unreachable,
        };
    }

    pub fn toToken(self: UpdateOperator) token.TokenType {
        return switch (self) {
            .increment => .increment,
            .decrement => .decrement,
        };
    }

    pub fn toString(self: UpdateOperator) []const u8 {
        return self.toToken().toString().?;
    }
};

pub const AssignmentOperator = enum {
    assign, // =
    add_assign, // +=
    subtract_assign, // -=
    multiply_assign, // *=
    divide_assign, // /=
    modulo_assign, // %=
    exponent_assign, // **=
    left_shift_assign, // <<=
    right_shift_assign, // >>=
    unsigned_right_shift_assign, // >>>=
    bitwise_or_assign, // |=
    bitwise_xor_assign, // ^=
    bitwise_and_assign, // &=
    logical_or_assign, // ||=
    logical_and_assign, // &&=
    nullish_assign, // ??=

    pub fn fromToken(tok: token.TokenType) AssignmentOperator {
        return switch (tok) {
            .assign => .assign,
            .plus_assign => .add_assign,
            .minus_assign => .subtract_assign,
            .star_assign => .multiply_assign,
            .slash_assign => .divide_assign,
            .percent_assign => .modulo_assign,
            .exponent_assign => .exponent_assign,
            .left_shift_assign => .left_shift_assign,
            .right_shift_assign => .right_shift_assign,
            .unsigned_right_shift_assign => .unsigned_right_shift_assign,
            .bitwise_or_assign => .bitwise_or_assign,
            .bitwise_xor_assign => .bitwise_xor_assign,
            .bitwise_and_assign => .bitwise_and_assign,
            .logical_or_assign => .logical_or_assign,
            .logical_and_assign => .logical_and_assign,
            .nullish_assign => .nullish_assign,
            else => unreachable,
        };
    }

    pub fn toToken(self: AssignmentOperator) token.TokenType {
        return switch (self) {
            .assign => .assign,
            .add_assign => .plus_assign,
            .subtract_assign => .minus_assign,
            .multiply_assign => .star_assign,
            .divide_assign => .slash_assign,
            .modulo_assign => .percent_assign,
            .exponent_assign => .exponent_assign,
            .left_shift_assign => .left_shift_assign,
            .right_shift_assign => .right_shift_assign,
            .unsigned_right_shift_assign => .unsigned_right_shift_assign,
            .bitwise_or_assign => .bitwise_or_assign,
            .bitwise_xor_assign => .bitwise_xor_assign,
            .bitwise_and_assign => .bitwise_and_assign,
            .logical_or_assign => .logical_or_assign,
            .logical_and_assign => .logical_and_assign,
            .nullish_assign => .nullish_assign,
        };
    }

    pub fn toString(self: AssignmentOperator) []const u8 {
        return self.toToken().toString().?;
    }
};

pub const VariableKind = enum {
    @"var",
    let,
    @"const",
    using,
    await_using,

    pub fn toString(self: VariableKind) []const u8 {
        return switch (self) {
            .await_using => "await using",
            .@"var" => "var",
            .let => "let",
            .@"const" => "const",
            .using => "using",
        };
    }
};

pub const PropertyKind = enum {
    init,
    get,
    set,

    pub fn toString(self: PropertyKind) []const u8 {
        return switch (self) {
            .init => "init",
            .get => "get",
            .set => "set",
        };
    }
};

/// https://tc39.es/ecma262/#prod-FormalParameters
pub const FormalParameterKind = enum {
    /// https://tc39.es/ecma262/#prod-FormalParameters
    formal_parameters,
    /// https://tc39.es/ecma262/#prod-UniqueFormalParameters
    unique_formal_parameters,
    /// https://tc39.es/ecma262/#prod-ArrowFormalParameters
    arrow_formal_parameters,
    /// Part of TypeScript type signatures
    signature,
};

/// `left operator right`
/// https://tc39.es/ecma262/#sec-binary-operators
pub const BinaryExpression = struct {
    /// Expression
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
    operator: BinaryOperator,
};

/// `left operator right`
/// https://tc39.es/ecma262/#sec-binary-logical-operators
pub const LogicalExpression = struct {
    /// Expression
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
    operator: LogicalOperator,
};

/// `test ? consequent : alternate`
/// https://tc39.es/ecma262/#sec-conditional-operator
pub const ConditionalExpression = struct {
    /// Expression (ShortCircuitExpression)
    @"test": NodeIndex,
    /// Expression (AssignmentExpression)
    consequent: NodeIndex,
    /// Expression (AssignmentExpression)
    alternate: NodeIndex,
};

/// `operator argument`
/// https://tc39.es/ecma262/#sec-unary-operators
pub const UnaryExpression = struct {
    /// Expression
    argument: NodeIndex,
    operator: UnaryOperator,
};

/// `++argument` or `argument++`
/// https://tc39.es/ecma262/#sec-update-expressions
pub const UpdateExpression = struct {
    /// SimpleAssignmentTarget (IdentifierReference | MemberExpression)
    argument: NodeIndex,
    operator: UpdateOperator,
    prefix: bool,
};

/// `left operator right`
/// https://tc39.es/ecma262/#sec-assignment-operators
pub const AssignmentExpression = struct {
    /// AssignmentTarget (IdentifierReference | MemberExpression | ArrayPattern | ObjectPattern)
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
    operator: AssignmentOperator,
};

/// https://tc39.es/ecma262/#sec-variable-statement
pub const VariableDeclaration = struct {
    kind: VariableKind,
    /// VariableDeclarator[]
    declarators: IndexRange,
};

/// `id = init`
pub const VariableDeclarator = struct {
    /// BindingPattern
    id: NodeIndex,
    /// Expression (optional, may be null_node)
    init: NodeIndex,
};

/// `expression;`
pub const ExpressionStatement = struct {
    /// Expression
    expression: NodeIndex,
};

/// https://tc39.es/ecma262/#sec-literals-string-literals
pub const StringLiteral = struct {
    raw_start: u32,
    raw_len: u16,
};

/// https://tc39.es/ecma262/#sec-literals-numeric-literals
pub const NumericLiteral = struct {
    raw_start: u32,
    raw_len: u16,
};

/// https://tc39.es/ecma262/#sec-ecmascript-language-lexical-grammar-literals
pub const BigIntLiteral = struct {
    raw_start: u32,
    raw_len: u16,
};

pub const BooleanLiteral = struct {
    value: bool,
};

/// https://tc39.es/ecma262/#sec-literals-regular-expression-literals
pub const RegExpLiteral = struct {
    pattern_start: u32,
    pattern_len: u16,
    flags_start: u32,
    flags_len: u8,
};

/// https://tc39.es/ecma262/#sec-template-literals
pub const TemplateLiteral = struct {
    /// TemplateElement[]
    quasis: IndexRange,
    /// Expression[]
    expressions: IndexRange,
};

/// quasi
pub const TemplateElement = struct {
    raw_start: u32,
    raw_len: u16,
    tail: bool,
};

/// used in expressions
/// https://tc39.es/ecma262/#sec-identifiers
pub const IdentifierReference = struct {
    name_start: u32,
    name_len: u16,
};

/// `#name`
pub const PrivateIdentifier = struct {
    name_start: u32,
    name_len: u16,
};

/// used in declarations
/// https://tc39.es/ecma262/#sec-identifiers
pub const BindingIdentifier = struct {
    name_start: u32,
    name_len: u16,
};

/// property keys, meta properties
pub const IdentifierName = struct {
    name_start: u32,
    name_len: u16,
};

/// `pattern = init`
/// https://tc39.es/ecma262/#prod-AssignmentPattern
pub const AssignmentPattern = struct {
    /// BindingPattern
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
};

/// `...argument`
/// https://tc39.es/ecma262/#prod-BindingRestElement
pub const BindingRestElement = struct {
    /// BindingPattern
    argument: NodeIndex,
};

/// `[a, b, ...rest]`
/// https://tc39.es/ecma262/#prod-ArrayBindingPattern
pub const ArrayPattern = struct {
    /// (BindingPattern | null)[] - null for holes
    elements: IndexRange,
    /// BindingRestElement (optional, may be null_node)
    rest: NodeIndex,
};

/// `{a, b: c, ...rest}`
/// https://tc39.es/ecma262/#prod-ObjectBindingPattern
pub const ObjectPattern = struct {
    /// BindingProperty[]
    properties: IndexRange,
    /// BindingRestElement (optional, may be null_node)
    rest: NodeIndex,
};

/// `key: value` or `key` (shorthand)
pub const BindingProperty = struct {
    /// PropertyKey
    key: NodeIndex,
    /// BindingPattern
    value: NodeIndex,
    shorthand: bool,
    computed: bool,
};

/// `[a, b, ...c]`
pub const ArrayExpression = struct {
    /// (Expression | SpreadElement | null)[] - null for holes
    elements: IndexRange,
};

/// `{a: 1, b, ...c}`
pub const ObjectExpression = struct {
    /// (ObjectProperty | SpreadElement)[]
    properties: IndexRange,
};

/// `...argument`
pub const SpreadElement = struct {
    /// Expression
    argument: NodeIndex,
};

/// `key: value`, getter/setter, or method
pub const ObjectProperty = struct {
    /// PropertyKey
    key: NodeIndex,
    /// Expression (for init) or Function (for methods/getters/setters)
    value: NodeIndex,
    kind: PropertyKind,
    method: bool,
    shorthand: bool,
    computed: bool,
};

pub const Program = struct {
    source_type: SourceType,
    /// (Statement | Directive)[]
    body: IndexRange,
};

pub const SourceType = enum {
    script,
    module,

    pub fn toString(self: SourceType) []const u8 {
        return switch (self) {
            .script => "script",
            .module => "module",
        };
    }
};

/// `"use strict";`
pub const Directive = struct {
    /// StringLiteral
    expression: NodeIndex,
    /// value without quotes
    value_start: u32,
    value_len: u16,
};

pub const FunctionType = enum {
    function_declaration,
    function_expression,
    ts_declare_function,
    // https://github.com/typescript-eslint/typescript-eslint/pull/1289
    // TODO:
    // declare class MyClass {
    //  myMethod(): void;
    // }
    //
    // interface MyInterface {
    //  myFunction(): string;
    // }
    ts_empty_body_function_expression,
};

/// https://tc39.es/ecma262/#sec-function-definitions
pub const Function = struct {
    type: FunctionType,
    /// BindingIdentifier (optional, may be null_node for anonymous functions)
    id: NodeIndex,
    generator: bool,
    async: bool,
    /// FormalParameters
    params: NodeIndex,
    /// FunctionBody (optional, may be null_node for declarations/overloads)
    body: NodeIndex,
};

/// https://tc39.es/ecma262/#prod-FunctionBody
pub const FunctionBody = struct {
    // (Statement | Directive)[]
    body: IndexRange,
};

/// https://tc39.es/ecma262/#prod-BlockStatement
pub const BlockStatement = struct {
    // (Statement | Directive)[]
    body: IndexRange,
};

/// https://tc39.es/ecma262/#prod-FormalParameters
pub const FormalParameters = struct {
    /// FormalParameter[]
    items: IndexRange,
    /// BindingRestElement (optional, may be null_node)
    rest: NodeIndex,
    kind: FormalParameterKind,
};

/// https://tc39.es/ecma262/#prod-FormalParameter
pub const FormalParameter = struct {
    /// BindingPattern
    pattern: NodeIndex,
};

/// https://tc39.es/ecma262/#prod-ParenthesizedExpression
pub const ParenthesizedExpression = struct {
    /// Expression
    expression: NodeIndex,
};

// https://tc39.es/ecma262/#prod-ArrowFunction
pub const ArrowFunctionExpression = struct {
    /// Is the function body an arrow expression? i.e. `() => expr` instead of `() => {}`
    expression: bool,
    /// async (a, b) => {}
    async: bool,
    /// FormalParameters
    params: NodeIndex,
    /// FunctionBody
    body: NodeIndex,
    // TODO: add pure field too, `true` if the function is marked with a `/*#__NO_SIDE_EFFECTS__*/` comment
    // TODO: handle PIFE ("Possibly-Invoked Function Expression") cases, there are other needs which are needed this
};

/// `a, b, c`
/// https://tc39.es/ecma262/#prod-Expression
pub const SequenceExpression = struct {
    /// Expression[]
    expressions: IndexRange,
};

/// `obj.prop`, `obj[expr]`, `obj.#priv`
/// https://tc39.es/ecma262/#sec-property-accessors
pub const MemberExpression = struct {
    /// Expression - the object being accessed
    object: NodeIndex,
    /// Expression (computed) | IdentifierName (static) | PrivateIdentifier (private)
    property: NodeIndex,
    /// true for obj[expr], false for obj.prop
    computed: bool,
    /// true for obj?.prop (optional chaining)
    optional: bool,
};

/// `func()`, `func?.()`
/// https://tc39.es/ecma262/#sec-function-calls
pub const CallExpression = struct {
    /// Expression - the function being called
    callee: NodeIndex,
    /// (Expression | SpreadElement)[]
    arguments: IndexRange,
    /// true for func?.() (optional chaining)
    optional: bool,
};

/// `foo?.bar`, `foo?.bar.baz`, `foo?.()`
/// Wraps an optional chain expression
/// https://tc39.es/ecma262/#sec-optional-chains
pub const ChainExpression = struct {
    /// ChainElement (CallExpression | MemberExpression with optional somewhere in chain)
    expression: NodeIndex,
};

/// `` tag`hello ${name}` ``
/// https://tc39.es/ecma262/#sec-tagged-templates
pub const TaggedTemplateExpression = struct {
    /// Expression - the tag function
    tag: NodeIndex,
    /// TemplateLiteral
    quasi: NodeIndex,
};

/// `new Callee()`, `new Callee(args)`
/// https://tc39.es/ecma262/#sec-new-operator
pub const NewExpression = struct {
    /// Expression - the constructor being called
    callee: NodeIndex,
    /// (Expression | SpreadElement)[]
    arguments: IndexRange,
};

/// `await expression`
/// https://tc39.es/ecma262/#sec-await
pub const AwaitExpression = struct {
    /// Expression
    argument: NodeIndex,
};

/// `yield expression` or `yield* expression`
/// https://tc39.es/ecma262/#sec-generator-function-definitions-runtime-semantics-evaluation
pub const YieldExpression = struct {
    /// Expression (optional, may be null_node)
    argument: NodeIndex,
    /// true for `yield*`, false for `yield`
    delegate: bool,
};

/// `import.meta` or `new.target`
/// https://tc39.es/ecma262/#prod-MetaProperty
pub const MetaProperty = struct {
    /// IdentifierName ('import' or 'new')
    meta: NodeIndex,
    /// IdentifierName ('meta' or 'target')
    property: NodeIndex,
};

pub const NodeData = union(enum) {
    sequence_expression: SequenceExpression,
    parenthesized_expression: ParenthesizedExpression,
    arrow_function_expression: ArrowFunctionExpression,
    function: Function,
    function_body: FunctionBody,
    block_statement: BlockStatement,
    formal_parameters: FormalParameters,
    formal_parameter: FormalParameter,
    binary_expression: BinaryExpression,
    logical_expression: LogicalExpression,
    conditional_expression: ConditionalExpression,
    unary_expression: UnaryExpression,
    update_expression: UpdateExpression,
    assignment_expression: AssignmentExpression,
    array_expression: ArrayExpression,
    object_expression: ObjectExpression,
    spread_element: SpreadElement,
    object_property: ObjectProperty,
    member_expression: MemberExpression,
    call_expression: CallExpression,
    chain_expression: ChainExpression,
    tagged_template_expression: TaggedTemplateExpression,
    new_expression: NewExpression,
    await_expression: AwaitExpression,
    yield_expression: YieldExpression,
    meta_property: MetaProperty,
    string_literal: StringLiteral,
    numeric_literal: NumericLiteral,
    bigint_literal: BigIntLiteral,
    boolean_literal: BooleanLiteral,
    null_literal,
    this_expression,
    regexp_literal: RegExpLiteral,
    template_literal: TemplateLiteral,
    template_element: TemplateElement,
    identifier_reference: IdentifierReference,
    private_identifier: PrivateIdentifier,
    binding_identifier: BindingIdentifier,
    identifier_name: IdentifierName,
    expression_statement: ExpressionStatement,
    variable_declaration: VariableDeclaration,
    variable_declarator: VariableDeclarator,
    directive: Directive,
    assignment_pattern: AssignmentPattern,
    binding_rest_element: BindingRestElement,
    array_pattern: ArrayPattern,
    object_pattern: ObjectPattern,
    binding_property: BindingProperty,
    program: Program,
};

pub const Node = struct {
    data: NodeData,
    span: Span,
};

pub inline fn isNull(index: NodeIndex) bool {
    return index == null_node;
}
