const std = @import("std");
const token = @import("token.zig");

pub const Severity = enum {
    @"error",
    warning,
    hint,
    info,

    pub fn toString(self: Severity) []const u8 {
        return switch (self) {
            .@"error" => "error",
            .warning => "warning",
            .hint => "hint",
            .info => "info",
        };
    }
};

pub const Label = struct {
    span: Span,
    message: []const u8,
};

pub const Diagnostic = struct {
    severity: Severity = .@"error",
    message: []const u8,
    span: Span,
    help: ?[]const u8 = null,
    labels: []const Label = &.{},
};

/// Source type of a JavaScript/TypeScript file.
/// Determines how the code is parsed and evaluated.
/// https://tc39.es/ecma262/#sec-types-of-source-code
pub const SourceType = enum {
    script,
    module,

    pub fn toString(self: SourceType) []const u8 {
        return switch (self) {
            .script => "script",
            .module => "module",
        };
    }

    /// Determines the source type based on the file extension.
    /// - `.mjs`, `.mts` are treated as modules (ES modules)
    /// - `.cjs`, `.cts` are treated as scripts (CommonJS)
    /// - All other files default to module (modern default)
    pub fn fromPath(path: []const u8) SourceType {
        if (std.mem.endsWith(u8, path, ".cjs") or std.mem.endsWith(u8, path, ".cts")) {
            return .script;
        }

        return .module;
    }
};

/// Language variant for JavaScript/TypeScript files.
/// Determines which syntax features are enabled during parsing.
pub const Lang = enum {
    js,
    ts,
    jsx,
    tsx,
    dts,

    /// Determines the language variant based on the file extension.
    /// - `.d.ts`, `.d.mts`, `.d.cts` → `dts` (TypeScript declaration files)
    /// - `.tsx` → `tsx` (TypeScript with JSX)
    /// - `.ts`, `.mts`, `.cts` → `ts` (TypeScript)
    /// - `.jsx` → `jsx` (JavaScript with JSX)
    /// - `.js`, `.mjs`, `.cjs` or unknown → `js` (JavaScript)
    pub fn fromPath(path: []const u8) Lang {
        if (std.mem.endsWith(u8, path, ".d.ts") or
            std.mem.endsWith(u8, path, ".d.mts") or
            std.mem.endsWith(u8, path, ".d.cts"))
        {
            return .dts;
        }

        if (std.mem.endsWith(u8, path, ".tsx")) return .tsx;

        if (std.mem.endsWith(u8, path, ".ts") or
            std.mem.endsWith(u8, path, ".mts") or
            std.mem.endsWith(u8, path, ".cts")) return .ts;

        if (std.mem.endsWith(u8, path, ".jsx")) return .jsx;

        return .js;
    }
};

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

/// Must be deinitialized to free the arena-allocated memory.
pub const ParseTree = struct {
    /// Root node of the AST (always a Program node)
    program: NodeIndex,
    /// Source code that was parsed
    source: []const u8,
    /// All nodes in the AST
    nodes: NodeList.Slice,
    /// Extra data storage for variadic node children
    extra: []NodeIndex,
    /// Diagnostics (errors, warnings, etc.) encountered during parsing
    diagnostics: []const Diagnostic,
    /// Comments collected in source code
    comments: []const Comment,
    /// Arena allocator owning all the memory
    arena: std.heap.ArenaAllocator,
    /// Source type (script or module)
    source_type: SourceType,
    /// Language variant (js, ts, jsx, tsx, dts)
    lang: Lang,

    /// Returns true if the parse tree contains any errors.
    pub inline fn hasErrors(self: ParseTree) bool {
        for (self.diagnostics) |d| {
            if (d.severity == .@"error") return true;
        }
        return false;
    }

    /// Returns true if the parse tree contains any diagnostics (errors, warnings, etc.).
    pub inline fn hasDiagnostics(self: ParseTree) bool {
        return self.diagnostics.len > 0;
    }

    /// Frees all memory allocated by this parse tree.
    pub fn deinit(self: *const ParseTree) void {
        self.arena.deinit();
    }

    /// Gets the data for the node at the given index.
    pub inline fn getData(self: *const ParseTree, index: NodeIndex) NodeData {
        return self.nodes.items(.data)[index];
    }

    /// Gets the span for the node at the given index.
    pub inline fn getSpan(self: *const ParseTree, index: NodeIndex) Span {
        return self.nodes.items(.span)[index];
    }

    /// Gets the extra node indices for the given range.
    pub inline fn getExtra(self: *const ParseTree, range: IndexRange) []const NodeIndex {
        return self.extra[range.start..][0..range.len];
    }

    /// Gets a slice of the source text at the given position.
    pub inline fn getSourceText(self: *const ParseTree, start: u32, len: u16) []const u8 {
        return self.source[start..][0..len];
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

    pub fn fromToken(tok: token.TokenTag) BinaryOperator {
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

    pub fn toToken(self: BinaryOperator) token.TokenTag {
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

    pub fn fromToken(tok: token.TokenTag) LogicalOperator {
        return switch (tok) {
            .logical_and => .@"and",
            .logical_or => .@"or",
            .nullish_coalescing => .nullish_coalescing,
            else => unreachable,
        };
    }

    pub fn toToken(self: LogicalOperator) token.TokenTag {
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

    pub fn fromToken(tok: token.TokenTag) UnaryOperator {
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

    pub fn toToken(self: UnaryOperator) token.TokenTag {
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

    pub fn fromToken(tok: token.TokenTag) UpdateOperator {
        return switch (tok) {
            .increment => .increment,
            .decrement => .decrement,
            else => unreachable,
        };
    }

    pub fn toToken(self: UpdateOperator) token.TokenTag {
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

    pub fn fromToken(tok: token.TokenTag) AssignmentOperator {
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

    pub fn toToken(self: AssignmentOperator) token.TokenTag {
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

/// https://tc39.es/ecma262/#sec-class-definitions
pub const ClassType = enum {
    class_declaration,
    class_expression,
};

/// https://tc39.es/ecma262/#prod-MethodDefinition
pub const MethodDefinitionKind = enum {
    constructor,
    method,
    get,
    set,

    pub fn toString(self: MethodDefinitionKind) []const u8 {
        return switch (self) {
            .constructor => "constructor",
            .method => "method",
            .get => "get",
            .set => "set",
        };
    }
};

/// https://github.com/tc39/proposal-decorators
pub const Decorator = struct {
    /// Expression
    expression: NodeIndex,
};

/// https://tc39.es/ecma262/#sec-class-definitions
pub const Class = struct {
    type: ClassType,
    /// Decorator[]
    decorators: IndexRange,
    /// BindingIdentifier (optional, may be null_node for class expressions)
    id: NodeIndex,
    /// Expression (optional, may be null_node if no extends clause)
    super_class: NodeIndex,
    /// ClassBody
    body: NodeIndex,
};

/// https://tc39.es/ecma262/#prod-ClassBody
pub const ClassBody = struct {
    /// ClassElement[]
    body: IndexRange,
};

/// https://tc39.es/ecma262/#prod-MethodDefinition
pub const MethodDefinition = struct {
    /// Decorator[]
    decorators: IndexRange,
    /// PropertyKey (IdentifierName | PrivateIdentifier | Expression)
    key: NodeIndex,
    /// Function
    value: NodeIndex,
    kind: MethodDefinitionKind,
    computed: bool,
    static: bool,
};

/// https://tc39.es/ecma262/#prod-FieldDefinition
pub const PropertyDefinition = struct {
    /// Decorator[]
    decorators: IndexRange,
    /// PropertyKey (IdentifierName | PrivateIdentifier | Expression)
    key: NodeIndex,
    /// Expression (optional, may be null_node)
    value: NodeIndex,
    computed: bool,
    static: bool,
    accessor: bool,
};

/// https://tc39.es/ecma262/#prod-ClassStaticBlock
pub const StaticBlock = struct {
    /// Statement[]
    body: IndexRange,
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

/// `if (test) consequent else alternate`
/// https://tc39.es/ecma262/#sec-if-statement
pub const IfStatement = struct {
    /// Expression (the condition)
    @"test": NodeIndex,
    /// Statement (the if-body)
    consequent: NodeIndex,
    /// Statement (optional, may be null_node for no else clause)
    alternate: NodeIndex,
};

/// `switch (discriminant) { cases }`
/// https://tc39.es/ecma262/#sec-switch-statement
pub const SwitchStatement = struct {
    /// Expression (the value to match)
    discriminant: NodeIndex,
    /// SwitchCase[]
    cases: IndexRange,
};

/// `for (init; test; update) body`
/// https://tc39.es/ecma262/#sec-for-statement
pub const ForStatement = struct {
    /// VariableDeclaration | Expression | null
    init: NodeIndex,
    /// Expression | null
    @"test": NodeIndex,
    /// Expression | null
    update: NodeIndex,
    /// Statement
    body: NodeIndex,
};

/// `for (left in right) body`
/// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements
pub const ForInStatement = struct {
    /// VariableDeclaration | AssignmentTarget (IdentifierReference | MemberExpression | ArrayPattern | ObjectPattern)
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
    /// Statement
    body: NodeIndex,
};

/// `for (left of right) body` or `for await (left of right) body`
/// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements
pub const ForOfStatement = struct {
    /// VariableDeclaration | AssignmentTarget (IdentifierReference | MemberExpression | ArrayPattern | ObjectPattern)
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
    /// Statement
    body: NodeIndex,
    /// true for `for await (...)`
    await: bool,
};

/// `break;` or `break label;`
/// https://tc39.es/ecma262/#sec-break-statement
pub const BreakStatement = struct {
    /// LabelIdentifier (optional, may be null_node)
    label: NodeIndex,
};

/// `continue;` or `continue label;`
/// https://tc39.es/ecma262/#sec-continue-statement
pub const ContinueStatement = struct {
    /// LabelIdentifier (optional, may be null_node)
    label: NodeIndex,
};

/// `label: statement`
/// https://tc39.es/ecma262/#sec-labelled-statements
pub const LabeledStatement = struct {
    /// LabelIdentifier
    label: NodeIndex,
    /// Statement
    body: NodeIndex,
};

/// `case test: consequent` or `default: consequent`
/// https://tc39.es/ecma262/#prod-CaseClause
pub const SwitchCase = struct {
    /// Expression (optional, null_node for default case)
    @"test": NodeIndex,
    /// Statement[]
    consequent: IndexRange,
};

/// `return;` or `return expression;`
/// https://tc39.es/ecma262/#sec-return-statement
pub const ReturnStatement = struct {
    /// Expression (optional, may be null_node)
    argument: NodeIndex,
};

/// `throw expression;`
/// https://tc39.es/ecma262/#sec-throw-statement
pub const ThrowStatement = struct {
    /// Expression (required)
    argument: NodeIndex,
};

/// `try { } catch { } finally { }`
/// https://tc39.es/ecma262/#sec-try-statement
pub const TryStatement = struct {
    /// BlockStatement
    block: NodeIndex,
    /// CatchClause (optional, may be null_node)
    handler: NodeIndex,
    /// BlockStatement (optional, may be null_node)
    finalizer: NodeIndex,
};

/// `catch (param) { body }`
/// https://tc39.es/ecma262/#prod-Catch
pub const CatchClause = struct {
    /// BindingPattern (optional, may be null_node for `catch { }`)
    param: NodeIndex,
    /// BlockStatement
    body: NodeIndex,
};

/// `while (test) body`
/// https://tc39.es/ecma262/#sec-while-statement
pub const WhileStatement = struct {
    /// Expression
    @"test": NodeIndex,
    /// Statement
    body: NodeIndex,
};

/// `do body while (test);`
/// https://tc39.es/ecma262/#sec-do-while-statement
pub const DoWhileStatement = struct {
    /// Statement
    body: NodeIndex,
    /// Expression
    @"test": NodeIndex,
};

/// `with (object) body`
/// https://tc39.es/ecma262/#sec-with-statement
pub const WithStatement = struct {
    /// Expression
    object: NodeIndex,
    /// Statement
    body: NodeIndex,
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
    kind: Kind,

    pub const Kind = enum {
        decimal,
        hex,
        octal,
        binary,

        pub fn fromToken(tok: token.TokenTag) Kind {
            return switch (tok) {
                .numeric_literal => .decimal,
                .hex_literal => .hex,
                .octal_literal => .octal,
                .binary_literal => .binary,
                else => unreachable,
            };
        }
    };
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
    /// True when this quasi's cooked template value is undefined per
    /// ECMAScript TV semantics.
    /// This happens when the quasi contains a NotEscapeSequence.
    /// Untagged templates are syntax errors, tagged templates allow this and
    /// produce undefined cooked values.
    is_cooked_undefined: bool = false,
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

/// https://tc39.es/ecma262/#prod-LabelIdentifier
pub const LabelIdentifier = struct {
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
    /// Hashbang comment (e.g. `#!/usr/bin/env node`), null if not present
    hashbang: ?Hashbang = null,
};

pub const Hashbang = struct {
    value_start: u32,
    value_len: u16,
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

/// import or export kind for TypeScript
pub const ImportOrExportKind = enum {
    value,
    type,

    pub fn toString(self: ImportOrExportKind) []const u8 {
        return switch (self) {
            .value => "value",
            .type => "type",
        };
    }
};

/// import phase for source phase imports and deferred imports
/// https://github.com/estree/estree/blob/master/stage3/source-phase-imports.md
/// https://github.com/estree/estree/blob/master/stage3/defer-import-eval.md
pub const ImportPhase = enum {
    /// `import source x from "x"` or `import.source("x")`
    source,
    /// `import defer * as x from "x"` or `import.defer("x")`
    @"defer",
};

/// `import(source)` or `import(source, options)` or `import.source(source)` or `import.defer(source)`
/// https://tc39.es/ecma262/#sec-import-calls
pub const ImportExpression = struct {
    /// Expression - the module specifier
    source: NodeIndex,
    /// Expression (optional, may be null_node) - import options/attributes
    options: NodeIndex,
    /// import phase: source, defer, or null (regular import)
    phase: ?ImportPhase,
};

/// `import ... from 'source'` or `import 'source'`
/// https://tc39.es/ecma262/#sec-imports
pub const ImportDeclaration = struct {
    /// ImportDeclarationSpecifier[] - null for side-effect imports (import 'foo')
    specifiers: IndexRange,
    /// StringLiteral - the module specifier
    source: NodeIndex,
    /// ImportAttribute[] - import attributes/assertions
    attributes: IndexRange,
    /// import phase: source, defer, or null (regular import)
    phase: ?ImportPhase,
};

/// `import {imported as local} from "source"`
///          ~~~~~~~~~~~~~~~~~
/// https://tc39.es/ecma262/#prod-ImportSpecifier
pub const ImportSpecifier = struct {
    /// ModuleExportName (IdentifierName or StringLiteral) - imported symbol
    imported: NodeIndex,
    /// BindingIdentifier - local binding
    local: NodeIndex,
};

/// `import local from "source"`
///         ~~~~~
/// https://tc39.es/ecma262/#prod-ImportedDefaultBinding
pub const ImportDefaultSpecifier = struct {
    /// BindingIdentifier - local binding
    local: NodeIndex,
};

/// `import * as local from "source"`
/// https://tc39.es/ecma262/#prod-NameSpaceImport
pub const ImportNamespaceSpecifier = struct {
    /// BindingIdentifier - local binding
    local: NodeIndex,
};

/// `type: "json"` in import attributes
pub const ImportAttribute = struct {
    /// ImportAttributeKey (IdentifierName or StringLiteral)
    key: NodeIndex,
    /// StringLiteral
    value: NodeIndex,
};

/// `export { foo, bar }` or `export { foo } from 'source'` or `export var/let/const/function/class`
/// https://tc39.es/ecma262/#prod-ExportDeclaration
pub const ExportNamedDeclaration = struct {
    /// Declaration (optional, may be null_node) - for `export var x`
    declaration: NodeIndex,
    /// ExportSpecifier[]
    specifiers: IndexRange,
    /// StringLiteral (optional, may be null_node) - for re-exports
    source: NodeIndex,
    /// ImportAttribute[] - export attributes/assertions
    attributes: IndexRange,
};

/// `export default expression`
/// https://tc39.es/ecma262/#prod-ExportDeclaration
pub const ExportDefaultDeclaration = struct {
    /// Expression | FunctionDeclaration | ClassDeclaration
    declaration: NodeIndex,
};

/// `export * from 'source'` or `export * as name from 'source'`
/// https://tc39.es/ecma262/#prod-ExportDeclaration
pub const ExportAllDeclaration = struct {
    /// ModuleExportName (optional, may be null_node) - for `export * as name`
    exported: NodeIndex,
    /// StringLiteral - the module specifier
    source: NodeIndex,
    /// ImportAttribute[] - export attributes/assertions
    attributes: IndexRange,
};

/// `export { local as exported }`
/// https://tc39.es/ecma262/#prod-ExportSpecifier
pub const ExportSpecifier = struct {
    /// ModuleExportName (IdentifierName/IdentifierReference or StringLiteral) - local binding
    local: NodeIndex,
    /// ModuleExportName (IdentifierName or StringLiteral) - exported name
    exported: NodeIndex,
};

/// `export = expression`
pub const TSExportAssignment = struct {
    /// Expression
    expression: NodeIndex,
};

/// `export as namespace name`
pub const TSNamespaceExportDeclaration = struct {
    /// IdentifierName
    id: NodeIndex,
};

/// `<Foo>children</Foo>` or `<Foo />`
/// https://facebook.github.io/jsx/#prod-JSXElement
pub const JSXElement = struct {
    /// JSXOpeningElement
    opening_element: NodeIndex,
    /// (JSXText | JSXElement | JSXFragment | JSXExpressionContainer | JSXSpreadChild)[]
    children: IndexRange,
    /// JSXClosingElement (optional, may be null_node for self-closing tags)
    closing_element: NodeIndex,
};

/// `<Foo bar={baz}>` or `<Foo />`
/// https://facebook.github.io/jsx/#prod-JSXOpeningElement
pub const JSXOpeningElement = struct {
    /// JSXIdentifier | JSXNamespacedName | JSXMemberExpression
    name: NodeIndex,
    /// (JSXAttribute | JSXSpreadAttribute)[]
    attributes: IndexRange,
    /// true for `<Foo />`, false for `<Foo>`
    self_closing: bool,
};

/// `</Foo>`
/// https://facebook.github.io/jsx/#prod-JSXClosingElement
pub const JSXClosingElement = struct {
    /// JSXIdentifier | JSXNamespacedName | JSXMemberExpression
    name: NodeIndex,
};

/// `<>children</>`
/// https://facebook.github.io/jsx/#prod-JSXFragment
pub const JSXFragment = struct {
    /// JSXOpeningFragment
    opening_fragment: NodeIndex,
    /// (JSXText | JSXElement | JSXFragment | JSXExpressionContainer | JSXSpreadChild)[]
    children: IndexRange,
    /// JSXClosingFragment
    closing_fragment: NodeIndex,
};

/// `<>`
pub const JSXOpeningFragment = struct {};

/// `</>`
pub const JSXClosingFragment = struct {};

/// used in jsx tag names and attributes
/// https://facebook.github.io/jsx/#prod-JSXIdentifier
pub const JSXIdentifier = struct {
    name_start: u32,
    name_len: u16,
};

/// `<namespace:name />`
/// https://facebook.github.io/jsx/#prod-JSXNamespacedName
pub const JSXNamespacedName = struct {
    /// JSXIdentifier - namespace portion
    namespace: NodeIndex,
    /// JSXIdentifier - name portion
    name: NodeIndex,
};

/// `<Foo.Bar.Baz />`
/// https://facebook.github.io/jsx/#prod-JSXMemberExpression
pub const JSXMemberExpression = struct {
    /// JSXIdentifier | JSXMemberExpression
    object: NodeIndex,
    /// JSXIdentifier
    property: NodeIndex,
};

/// `foo="bar"` or `foo={expr}` or just `foo`
/// https://facebook.github.io/jsx/#prod-JSXAttribute
pub const JSXAttribute = struct {
    /// JSXIdentifier | JSXNamespacedName
    name: NodeIndex,
    /// StringLiteral | JSXExpressionContainer | JSXElement | JSXFragment (optional, may be null_node for boolean-like attributes)
    value: NodeIndex,
};

/// `{...props}`
/// https://facebook.github.io/jsx/#prod-JSXSpreadAttribute
pub const JSXSpreadAttribute = struct {
    /// Expression - the expression being spread
    argument: NodeIndex,
};

/// `{expression}`
/// https://facebook.github.io/jsx/#prod-JSXExpressionContainer
pub const JSXExpressionContainer = struct {
    /// JSXEmptyExpression | Expression
    expression: NodeIndex,
};

/// `{}`
pub const JSXEmptyExpression = struct {};

/// text content inside JSX elements
pub const JSXText = struct {
    raw_start: u32,
    raw_len: u16,
};

/// `{...children}`
pub const JSXSpreadChild = struct {
    /// Expression - the expression being spread
    expression: NodeIndex,
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
    decorator: Decorator,
    class: Class,
    class_body: ClassBody,
    method_definition: MethodDefinition,
    property_definition: PropertyDefinition,
    static_block: StaticBlock,
    super,
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
    label_identifier: LabelIdentifier,
    expression_statement: ExpressionStatement,
    if_statement: IfStatement,
    switch_statement: SwitchStatement,
    switch_case: SwitchCase,
    for_statement: ForStatement,
    for_in_statement: ForInStatement,
    for_of_statement: ForOfStatement,
    while_statement: WhileStatement,
    do_while_statement: DoWhileStatement,
    break_statement: BreakStatement,
    continue_statement: ContinueStatement,
    labeled_statement: LabeledStatement,
    with_statement: WithStatement,
    return_statement: ReturnStatement,
    throw_statement: ThrowStatement,
    try_statement: TryStatement,
    catch_clause: CatchClause,
    debugger_statement,
    empty_statement,
    variable_declaration: VariableDeclaration,
    variable_declarator: VariableDeclarator,
    directive: Directive,
    assignment_pattern: AssignmentPattern,
    binding_rest_element: BindingRestElement,
    array_pattern: ArrayPattern,
    object_pattern: ObjectPattern,
    binding_property: BindingProperty,
    program: Program,
    import_expression: ImportExpression,
    import_declaration: ImportDeclaration,
    import_specifier: ImportSpecifier,
    import_default_specifier: ImportDefaultSpecifier,
    import_namespace_specifier: ImportNamespaceSpecifier,
    import_attribute: ImportAttribute,
    export_named_declaration: ExportNamedDeclaration,
    export_default_declaration: ExportDefaultDeclaration,
    export_all_declaration: ExportAllDeclaration,
    export_specifier: ExportSpecifier,

    // typescript
    ts_export_assignment: TSExportAssignment,
    ts_namespace_export_declaration: TSNamespaceExportDeclaration,

    // jsx
    jsx_element: JSXElement,
    jsx_opening_element: JSXOpeningElement,
    jsx_closing_element: JSXClosingElement,
    jsx_fragment: JSXFragment,
    jsx_opening_fragment: JSXOpeningFragment,
    jsx_closing_fragment: JSXClosingFragment,
    jsx_identifier: JSXIdentifier,
    jsx_namespaced_name: JSXNamespacedName,
    jsx_member_expression: JSXMemberExpression,
    jsx_attribute: JSXAttribute,
    jsx_spread_attribute: JSXSpreadAttribute,
    jsx_expression_container: JSXExpressionContainer,
    jsx_empty_expression: JSXEmptyExpression,
    jsx_text: JSXText,
    jsx_spread_child: JSXSpreadChild,
};

pub const Node = struct {
    data: NodeData,
    span: Span,
};

pub const NodeList = std.MultiArrayList(Node);

pub inline fn isNull(index: NodeIndex) bool {
    return index == null_node;
}
