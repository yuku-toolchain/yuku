const std = @import("std");
const ast_string = @import("ast_string.zig");
const TokenSpan = @import("token.zig").Span;
const TokenTag = @import("token.zig").TokenTag;

pub const String = ast_string.String;
pub const StringPool = ast_string.ASTStringPool;

pub const Span = TokenSpan;

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

/// A span inside a diagnostic that carries an explanatory message.
pub const Label = struct {
    span: Span,
    message: []const u8,
};

/// An error, warning, hint, or info produced during parsing or semantic analysis.
pub const Diagnostic = struct {
    severity: Severity = .@"error",
    message: []const u8,
    span: Span,
    help: ?[]const u8 = null,
    labels: []const Label = &.{},
};

/// Source type of a JavaScript/TypeScript file.
///
/// Determines whether the file is parsed as an ES module or a classic script.
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

/// A line or block comment from the source.
///
/// ## Example
/// ```js
/// // this is a line comment
/// /* this is a block comment */
/// ```
pub const Comment = struct {
    type: Type,
    /// comment content without the surrounding delimiters
    value: String = .empty,
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
};

/// The AST. Backed by growable arrays and an arena allocator.
///
/// Returned by `parser.parse()`. Readable immediately after parsing.
/// Can be enriched with semantic analysis or transforms. All
/// allocations go into the tree's arena, and `deinit()` frees
/// everything at once.
pub const Tree = struct {
    /// Root node of the AST (always a Program node).
    program: NodeIndex = undefined,
    /// All nodes in the AST.
    nodes: NodeList = .empty,
    /// Extra data storage for variadic node children.
    extra: std.ArrayList(NodeIndex) = .empty,
    /// Diagnostics (errors, warnings, etc.) collected during parsing and analysis.
    diagnostics: std.ArrayList(Diagnostic) = .empty,
    /// Comments found in the source code.
    comments: []const Comment = &.{},
    /// Arena allocator owning all the memory.
    arena: std.heap.ArenaAllocator,
    /// The original source text passed to the parser.
    /// Empty for trees built programmatically with `initEmpty()`.
    source: []const u8 = "",
    /// String pool for AST node string fields.
    strings: StringPool = .{},
    /// Source type (script or module).
    source_type: SourceType = .module,
    /// Language variant (js, ts, jsx, tsx, dts).
    lang: Lang = .js,

    /// Creates a tree for parsing or transforming source code.
    pub fn init(child_allocator: std.mem.Allocator, source: []const u8) Tree {
        return .{
            .arena = std.heap.ArenaAllocator.init(child_allocator),
            .source = source,
            .strings = .{ .source = source },
        };
    }

    /// Creates an empty tree for building ASTs programmatically (no source text).
    /// String fields can use string literals or `addString()` for dynamic strings.
    pub fn initEmpty(child_allocator: std.mem.Allocator) Tree {
        return .{
            .arena = std.heap.ArenaAllocator.init(child_allocator),
        };
    }

    /// Frees all memory owned by this tree.
    pub fn deinit(self: *const Tree) void {
        self.arena.deinit();
    }

    pub inline fn allocator(self: *Tree) std.mem.Allocator {
        return self.arena.allocator();
    }

    pub inline fn isTs(self: *const Tree) bool {
        return self.lang == .ts or self.lang == .tsx or self.lang == .dts;
    }

    pub inline fn isJsx(self: *const Tree) bool {
        return self.lang == .tsx or self.lang == .jsx;
    }

    pub inline fn isModule(self: *const Tree) bool {
        return self.source_type == .module;
    }

    /// Returns true if the tree contains any errors.
    pub inline fn hasErrors(self: *const Tree) bool {
        for (self.diagnostics.items) |d| {
            if (d.severity == .@"error") return true;
        }
        return false;
    }

    /// Returns true if the tree contains any diagnostics.
    pub inline fn hasDiagnostics(self: *const Tree) bool {
        return self.diagnostics.items.len > 0;
    }

    /// Appends a diagnostic to the tree.
    pub fn appendDiagnostic(self: *Tree, diag: Diagnostic) error{OutOfMemory}!void {
        try self.diagnostics.append(self.arena.allocator(), diag);
    }

    /// Returns the data for the node at the given index.
    pub inline fn getData(self: *const Tree, index: NodeIndex) NodeData {
        return self.nodes.items(.data)[@intFromEnum(index)];
    }

    /// Returns the span for the node at the given index.
    pub inline fn getSpan(self: *const Tree, index: NodeIndex) Span {
        return self.nodes.items(.span)[@intFromEnum(index)];
    }

    /// Returns the extra node indices for the given range.
    pub inline fn getExtra(self: *const Tree, range: IndexRange) []const NodeIndex {
        return self.extra.items[range.start..][0..range.len];
    }

    /// Replaces an existing node's data in-place.
    pub inline fn replaceData(self: *Tree, index: NodeIndex, data: NodeData) void {
        self.nodes.items(.data)[@intFromEnum(index)] = data;
    }

    /// Replaces an existing node's span in-place.
    pub inline fn replaceSpan(self: *Tree, index: NodeIndex, span: Span) void {
        self.nodes.items(.span)[@intFromEnum(index)] = span;
    }

    /// Creates a new node. Returns its index.
    pub inline fn createNode(self: *Tree, data: NodeData, span: Span) error{OutOfMemory}!NodeIndex {
        const index: NodeIndex = @enumFromInt(@as(u32, @intCast(self.nodes.len)));
        if (self.nodes.len < self.nodes.capacity) {
            self.nodes.appendAssumeCapacity(.{ .data = data, .span = span });
        } else {
            try self.nodes.append(self.arena.allocator(), .{ .data = data, .span = span });
        }
        return index;
    }

    /// Creates a new child list. Returns its range.
    pub inline fn createExtra(self: *Tree, children: []const NodeIndex) error{OutOfMemory}!IndexRange {
        const start: u32 = @intCast(self.extra.items.len);
        if (self.extra.items.len + children.len <= self.extra.capacity) {
            self.extra.appendSliceAssumeCapacity(children);
        } else {
            try self.extra.appendSlice(self.arena.allocator(), children);
        }
        return .{ .start = start, .len = @intCast(children.len) };
    }

    /// Pre-allocates string pool capacity. Call before bulk `addString()` calls
    /// to avoid repeated reallocations during programmatic AST building.
    pub fn ensureStringCapacity(self: *Tree, bytes: u32, entries: u32) error{OutOfMemory}!void {
        return self.strings.ensureCapacity(self.arena.allocator(), bytes, entries);
    }

    /// Returns a `String` referencing a range in the original source text.
    pub inline fn sourceSlice(self: *const Tree, start: u32, end: u32) String {
        return self.strings.sourceSlice(start, end);
    }

    /// Copies `str` into the string pool and returns its `String`.
    /// Use for escaped identifiers, transforms, and programmatic AST building.
    pub fn addString(self: *Tree, str: []const u8) error{OutOfMemory}!String {
        return self.strings.addString(self.arena.allocator(), str);
    }

    /// Returns the string content for a `String`.
    pub inline fn getString(self: *const Tree, id: String) []const u8 {
        return self.strings.get(id);
    }
};


/// Index into the AST node array.
///
/// `.null` marks an absent child for optional slots.
pub const NodeIndex = enum(u32) { null = std.math.maxInt(u32), _ };

/// Range of indices into the extra array for storing variadic node lists.
pub const IndexRange = struct {
    start: u32,
    len: u32,

    pub const empty: IndexRange = .{ .start = 0, .len = 0 };
};

/// Binary operators.
///
/// ## Example
/// ```js
/// a + b
/// a === b
/// a instanceof B
/// ```
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

    pub fn fromToken(token: TokenTag) BinaryOperator {
        return switch (token) {
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

    pub fn toToken(self: BinaryOperator) TokenTag {
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

/// Logical operators.
///
/// ## Example
/// ```js
/// a && b
/// a || b
/// a ?? b
/// ```
pub const LogicalOperator = enum {
    @"and", // &&
    @"or", // ||
    nullish_coalescing, // ??

    pub fn fromToken(token: TokenTag) LogicalOperator {
        return switch (token) {
            .logical_and => .@"and",
            .logical_or => .@"or",
            .nullish_coalescing => .nullish_coalescing,
            else => unreachable,
        };
    }

    pub fn toToken(self: LogicalOperator) TokenTag {
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

/// Unary operators.
///
/// ## Example
/// ```js
/// -x
/// !flag
/// typeof x
/// void 0
/// delete obj.prop
/// ```
pub const UnaryOperator = enum {
    negate, // -
    positive, // +
    logical_not, // !
    bitwise_not, // ~
    typeof, // typeof
    void, // void
    delete, // delete

    pub fn fromToken(token: TokenTag) UnaryOperator {
        return switch (token) {
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

    pub fn toToken(self: UnaryOperator) TokenTag {
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

/// Update operators.
///
/// ## Example
/// ```js
/// ++x
/// x--
/// ```
pub const UpdateOperator = enum {
    increment, // ++
    decrement, // --

    pub fn fromToken(token: TokenTag) UpdateOperator {
        return switch (token) {
            .increment => .increment,
            .decrement => .decrement,
            else => unreachable,
        };
    }

    pub fn toToken(self: UpdateOperator) TokenTag {
        return switch (self) {
            .increment => .increment,
            .decrement => .decrement,
        };
    }

    pub fn toString(self: UpdateOperator) []const u8 {
        return self.toToken().toString().?;
    }
};

/// Assignment operators.
///
/// ## Example
/// ```js
/// x = 1
/// x += 2
/// x &&= y
/// x ??= 0
/// ```
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

    pub fn fromToken(token: TokenTag) AssignmentOperator {
        return switch (token) {
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

    pub fn toToken(self: AssignmentOperator) TokenTag {
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

/// Variable declaration kind.
///
/// ## Example
/// ```js
/// var x;
/// let y;
/// const z = 0;
/// using r = resource();
/// await using ar = asyncResource();
/// ```
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

/// Object property kind.
///
/// ## Example
/// ```js
/// { a: 1,          // init
///   get b() {},    // get
///   set b(v) {} }  // set
/// ```
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

/// Class form.
///
/// ## Example
/// ```js
/// class Foo {}          // class_declaration
/// const x = class {};   //           class_expression
/// ```
pub const ClassType = enum {
    class_declaration,
    class_expression,
};

/// Kind of a class method definition.
///
/// ## Example
/// ```js
/// class C {
///   constructor() {}   // constructor
///   method() {}        // method
///   get prop() {}      // get
///   set prop(v) {}     // set
/// }
/// ```
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

/// The `super` keyword used as an expression head.
///
/// ## Example
/// ```js
/// foo(super);
/// //  ^^^^^ Super
/// ```
pub const Super = struct {};

/// The `null` literal.
///
/// ## Example
/// ```js
/// const x = null;
/// //        ^^^^ NullLiteral
/// ```
pub const NullLiteral = struct {};

/// The `this` keyword used as an expression.
///
/// ## Example
/// ```js
/// const x = this;
/// //        ^^^^ ThisExpression
/// ```
pub const ThisExpression = struct {};

/// The `debugger;` statement. Suspends execution when a debugger is attached.
pub const DebuggerStatement = struct {};

/// A standalone `;` used as a statement.
pub const EmptyStatement = struct {};

/// A decorator applied to a class or class member.
///
/// ## Example
/// ```js
/// class C { @foo bar() {} }
/// //         ^^^ expression (of the decorator `@foo`)
/// ```
pub const Decorator = struct {
    expression: NodeIndex,
};

/// A class declaration or expression.
///
/// ## Example
/// ```ts
/// class Foo<T> extends Base<T> implements I {}
/// //    ^^^ id
/// //       ^^^ type_parameters
/// //                   ^^^^ super_class
/// //                       ^^^ super_type_arguments
/// //                                      ^ implements[0]
/// //                                        ^^ body
/// ```
/// Column-0 modifiers: `@dec class` populates `decorators`; `abstract class`
/// sets `abstract = true`; `declare class` sets `declare = true`.
pub const Class = struct {
    type: ClassType,
    decorators: IndexRange,
    /// `.null` for anonymous class expressions
    id: NodeIndex,
    /// `.null` when the class has no `extends` clause
    super_class: NodeIndex,
    body: NodeIndex,
    /// ts: `.null` when the class has no `<T, U>` parameters
    type_parameters: NodeIndex = .null,
    /// ts: `.null` when `extends` has no `<T>` arguments
    super_type_arguments: NodeIndex = .null,
    /// ts: empty when the class has no `implements` clause
    implements: IndexRange = .empty,
    /// ts: true for `abstract class`
    abstract: bool = false,
    /// ts: true for `declare class`
    declare: bool = false,
};

/// The `{ ... }` body of a class, holding its members.
///
/// ## Example
/// ```js
/// class C {
///   a;
///   b() {}
///   static {}
/// }
/// // body: [a, b, static block]
/// ```
pub const ClassBody = struct {
    body: IndexRange,
};

/// A method, getter, setter, or constructor in a class body.
///
/// ## Example
/// ```ts
/// class C { public override foo?(): void {} }
/// //        ^^^^^^ accessibility
/// //               ^^^^^^^^ override
/// //                        ^^^ key
/// //                           ^ optional
/// //                            ^^^^^^^^^^^ value (a Function)
/// ```
/// `decorators` comes from `@dec` preceding the method; `static ...`, `*` and
/// `async` modifiers live on the member head and appear on the inner `Function`.
pub const MethodDefinition = struct {
    decorators: IndexRange,
    /// `IdentifierName`, `PrivateIdentifier`, or an expression when `computed`
    key: NodeIndex,
    /// a `Function` node
    value: NodeIndex,
    kind: MethodDefinitionKind,
    computed: bool,
    static: bool,
    /// ts: true for the `override` modifier
    override: bool = false,
    /// ts: true for optional method (`foo?()`)
    optional: bool = false,
    /// ts: `.none` when no modifier was written
    accessibility: Accessibility = .none,
};

/// A class field or auto-accessor declaration.
///
/// ## Example
/// ```ts
/// class C { public readonly foo!: number = 0 }
/// //        ^^^^^^ accessibility
/// //               ^^^^^^^^ readonly
/// //                        ^^^ key
/// //                           ^ definite
/// //                            ^^^^^^^^ type_annotation
/// //                                       ^ value
/// ```
/// `decorators` comes from `@dec` preceding the field; `declare`, `override`,
/// and `static` live on the member head; `accessor foo` sets `accessor = true`.
pub const PropertyDefinition = struct {
    decorators: IndexRange,
    /// `IdentifierName`, `PrivateIdentifier`, or an expression when `computed`
    key: NodeIndex,
    /// initializer, `.null` when absent
    value: NodeIndex,
    computed: bool,
    static: bool,
    /// true for `accessor x;` auto-accessor fields
    accessor: bool,
    /// ts: `.null` when the field has no annotation
    type_annotation: NodeIndex = .null,
    /// ts: true for the `declare` modifier
    declare: bool = false,
    /// ts: true for the `override` modifier
    override: bool = false,
    /// ts: true for optional property (`foo?: T`)
    optional: bool = false,
    /// ts: true for definite assignment assertion (`foo!: T`)
    definite: bool = false,
    /// ts: true for the `readonly` modifier
    readonly: bool = false,
    /// ts: `.none` when no modifier was written
    accessibility: Accessibility = .none,
};

/// A `static { ... }` block inside a class body.
///
/// ## Example
/// ```js
/// class C { static { init(); } }
/// //                 ^^^^^^^ body
/// ```
pub const StaticBlock = struct {
    body: IndexRange,
};

/// Which grammar production the parameter list came from.
///
/// Constrains which binding forms are legal and whether duplicates are
/// allowed in strict mode.
pub const FormalParameterKind = enum {
    /// plain `function` parameter list
    formal_parameters,
    /// parameters of a generator, async, arrow, method, or setter
    unique_formal_parameters,
    /// arrow function parameter list
    arrow_formal_parameters,
    /// parameters of a TypeScript type signature
    signature,
};

/// Binary expression with a non-logical operator.
///
/// ## Example
/// ```js
/// r = a + b
/// //  ^ left
/// //    ^ operator
/// //      ^ right
/// ```
pub const BinaryExpression = struct {
    left: NodeIndex,
    right: NodeIndex,
    operator: BinaryOperator,
};

/// Short-circuiting logical expression.
///
/// ## Example
/// ```js
/// r = a && b
/// //  ^ left
/// //    ^^ operator
/// //       ^ right
/// ```
pub const LogicalExpression = struct {
    left: NodeIndex,
    right: NodeIndex,
    operator: LogicalOperator,
};

/// Ternary expression.
///
/// ## Example
/// ```js
/// r = a ? b : c
/// //  ^ test
/// //      ^ consequent
/// //          ^ alternate
/// ```
pub const ConditionalExpression = struct {
    @"test": NodeIndex,
    consequent: NodeIndex,
    alternate: NodeIndex,
};

/// Unary prefix expression.
///
/// ## Example
/// ```js
/// typeof foo
/// //     ^^^ argument
/// ```
/// `operator` is one of `-`, `+`, `!`, `~`, `typeof`, `void`, `delete`.
pub const UnaryExpression = struct {
    argument: NodeIndex,
    operator: UnaryOperator,
};

/// `++` or `--` applied to an assignable target.
///
/// ## Example
/// ```js
/// ++x    // prefix = true
/// x--    // prefix = false
/// ```
pub const UpdateExpression = struct {
    /// an `IdentifierReference` or `MemberExpression`
    argument: NodeIndex,
    operator: UpdateOperator,
    prefix: bool,
};

/// Assignment expression.
///
/// ## Example
/// ```js
/// x += 1
/// //^^ operator
/// //   ^ right
/// ```
/// `left` is the assignment target (here `x`); it can also be an `ArrayPattern`
/// or `ObjectPattern` for destructuring assignment.
pub const AssignmentExpression = struct {
    /// an assignment target: `IdentifierReference`, `MemberExpression`,
    /// `ArrayPattern`, or `ObjectPattern`
    left: NodeIndex,
    right: NodeIndex,
    operator: AssignmentOperator,
};

/// A `var`, `let`, `const`, `using`, or `await using` declaration.
///
/// ## Example
/// ```ts
/// const x = 1, y = 2;
/// //    ^^^^^^^^^^^^ declarators
/// ```
/// `kind` is `.@"const"`. `declare = true` for `declare const ...`.
pub const VariableDeclaration = struct {
    kind: VariableKind,
    declarators: IndexRange,
    /// ts: true for `declare var x: T`
    declare: bool = false,
};

/// A single binding in a variable declaration.
///
/// ## Example
/// ```ts
/// let x!: number = 1;
/// //  ^ id
/// //   ^ definite
/// //    ^^^^^^^^ type_annotation on the binding
/// //               ^ init
/// ```
pub const VariableDeclarator = struct {
    /// a binding pattern (`BindingIdentifier`, `ArrayPattern`, `ObjectPattern`)
    id: NodeIndex,
    /// initializer, `.null` when absent
    init: NodeIndex,
    /// ts: true for definite assignment assertion (`let x!: T`)
    definite: bool = false,
};

/// A statement consisting of a single expression.
///
/// ## Example
/// ```js
/// { foo(); }
/// //^^^^^ expression
/// ```
pub const ExpressionStatement = struct {
    expression: NodeIndex,
};

/// An `if`/`else` statement.
///
/// ## Example
/// ```js
/// if (cond) thenStmt; else elseStmt;
/// //  ^^^^ test
/// //        ^^^^^^^^ consequent
/// //                       ^^^^^^^^ alternate
/// ```
pub const IfStatement = struct {
    @"test": NodeIndex,
    consequent: NodeIndex,
    /// `.null` when there is no `else` clause
    alternate: NodeIndex,
};

/// A `switch` statement.
///
/// ## Example
/// ```js
/// switch (x) { case 1: stmt; }
/// //      ^ discriminant
/// //           ^^^^^^^^^^^^^ cases
/// ```
pub const SwitchStatement = struct {
    discriminant: NodeIndex,
    cases: IndexRange,
};

/// A `for (init; test; update)` loop.
///
/// ## Example
/// ```js
/// for (let i = 0; i < n; i++) body;
/// //   ^^^^^^^^^ init
/// //              ^^^^^ test
/// //                     ^^^ update
/// //                          ^^^^^ body
/// ```
pub const ForStatement = struct {
    /// `VariableDeclaration`, expression, or `.null`
    init: NodeIndex,
    /// expression or `.null`
    @"test": NodeIndex,
    /// expression or `.null`
    update: NodeIndex,
    body: NodeIndex,
};

/// A `for (... in ...)` loop.
///
/// ## Example
/// ```js
/// for (const k in obj) body;
/// //   ^^^^^^^ left
/// //              ^^^ right
/// //                   ^^^^^ body
/// ```
pub const ForInStatement = struct {
    /// `VariableDeclaration` or an assignment target
    left: NodeIndex,
    right: NodeIndex,
    body: NodeIndex,
};

/// A `for (... of ...)` or `for await (... of ...)` loop.
///
/// ## Example
/// ```js
/// for await (const x of iter) body;
/// //  ^^^^^ await
/// //         ^^^^^^^ left
/// //                    ^^^^ right
/// //                          ^^^^^ body
/// ```
pub const ForOfStatement = struct {
    left: NodeIndex,
    right: NodeIndex,
    body: NodeIndex,
    /// true for `for await (...)`
    await: bool,
};

/// A `break` statement, optionally targeting a label.
///
/// ## Example
/// ```js
/// break;
/// break outer;
/// //    ^^^^^ label
/// ```
pub const BreakStatement = struct {
    /// `.null` for bare `break`
    label: NodeIndex,
};

/// A `continue` statement, optionally targeting a label.
///
/// ## Example
/// ```js
/// continue;
/// continue outer;
/// //       ^^^^^ label
/// ```
pub const ContinueStatement = struct {
    /// `.null` for bare `continue`
    label: NodeIndex,
};

/// A labeled statement.
///
/// ## Example
/// ```js
/// outer: for (;;) break outer;
/// //     ^^^^^^^^^^^^^^^^^^^^^ body
/// ```
/// `label` is the identifier before `:` (here `outer`).
pub const LabeledStatement = struct {
    label: NodeIndex,
    body: NodeIndex,
};

/// A single `case` or `default` clause inside a `switch`.
///
/// ## Example
/// ```js
/// case 1: doIt(); break;
/// //   ^ test
/// //      ^^^^^^^^^^^^^^ consequent
/// ```
/// `test` is `.null` for the `default` clause.
pub const SwitchCase = struct {
    /// `.null` for the `default` clause
    @"test": NodeIndex,
    consequent: IndexRange,
};

/// A `return` statement.
///
/// ## Example
/// ```js
/// return;
/// return x + 1;
/// //     ^^^^^ argument
/// ```
pub const ReturnStatement = struct {
    /// `.null` for bare `return`
    argument: NodeIndex,
};

/// A `throw` statement.
///
/// ## Example
/// ```js
/// throw new Error("boom");
/// //    ^^^^^^^^^^^^^^^^^ argument
/// ```
pub const ThrowStatement = struct {
    argument: NodeIndex,
};

/// A `try`/`catch`/`finally` statement.
///
/// ## Example
/// ```js
/// try { a } catch (e) { b } finally { c }
/// //  ^^^^^ block
/// //        ^^^^^^^^^^^^^^^ handler
/// //                                ^^^^^ finalizer
/// ```
pub const TryStatement = struct {
    block: NodeIndex,
    /// `.null` when no `catch` clause is present
    handler: NodeIndex,
    /// `.null` when no `finally` clause is present
    finalizer: NodeIndex,
};

/// The `catch` clause of a `try` statement.
///
/// ## Example
/// ```js
/// try {} catch (e) { body }
/// //            ^ param
/// //               ^^^^^^^^ body
/// ```
/// `param` is `.null` for `catch { }` with no binding.
pub const CatchClause = struct {
    /// `.null` for `catch { }` with no binding
    param: NodeIndex,
    body: NodeIndex,
};

/// A `while` statement.
///
/// ## Example
/// ```js
/// while (cond) body;
/// //     ^^^^ test
/// //           ^^^^^ body
/// ```
pub const WhileStatement = struct {
    @"test": NodeIndex,
    body: NodeIndex,
};

/// A `do { ... } while (...)` statement.
///
/// ## Example
/// ```js
/// do body; while (cond);
/// // ^^^^^ body
/// //              ^^^^ test
/// ```
pub const DoWhileStatement = struct {
    body: NodeIndex,
    @"test": NodeIndex,
};

/// A `with` statement. Forbidden in strict mode.
///
/// ## Example
/// ```js
/// with (obj) body;
/// //    ^^^ object
/// //         ^^^^^ body
/// ```
pub const WithStatement = struct {
    object: NodeIndex,
    body: NodeIndex,
};

/// A string literal.
///
/// ## Example
/// ```js
/// "hello\n"
/// //    ^^ escape sequences are decoded into `value`
/// ```
pub const StringLiteral = struct {
    /// decoded content with escape sequences resolved and quotes stripped
    value: String = .empty,
};

/// A numeric literal in one of four bases.
///
/// ## Example
/// ```js
/// 42          // decimal
/// 0xFF        // hex
/// 0o17        // octal
/// 0b1010      // binary
/// 1_000_000   // numeric separators are stripped when computing `value()`
/// ```
pub const NumericLiteral = struct {
    kind: Kind,
    /// the raw lexeme, including prefix (`0x`, `0b`, `0o`) and separators
    raw: String = .empty,

    /// Computes the IEEE 754 double value.
    pub fn value(self: NumericLiteral, tree: *const Tree) f64 {
        const raw = tree.getString(self.raw);
        if (raw.len == 0) return 0;
        // strip numeric separators
        var buf: [128]u8 = undefined;
        var len: usize = 0;
        for (raw) |c| {
            if (c != '_') {
                if (len >= buf.len) return 0;
                buf[len] = c;
                len += 1;
            }
        }
        const s = buf[0..len];
        if (s.len == 0) return 0;
        return switch (self.kind) {
            .decimal => std.fmt.parseFloat(f64, s) catch 0,
            .hex => parseIntOrFloat(s[2..], 16),
            .octal => blk: {
                // modern: 0o/0O prefix; legacy: bare 0 prefix
                const digits = if (s.len >= 2 and (s[1] == 'o' or s[1] == 'O')) s[2..] else s[1..];
                break :blk parseIntOrFloat(digits, 8);
            },
            .binary => parseIntOrFloat(s[2..], 2),
        };
    }

    fn parseIntOrFloat(digits: []const u8, base: u8) f64 {
        const v = std.fmt.parseInt(u64, digits, base) catch {
            var val: f64 = 0;
            const fbase: f64 = @floatFromInt(base);
            for (digits) |d| {
                val = val * fbase + @as(f64, @floatFromInt(std.fmt.charToDigit(d, base) catch unreachable));
            }
            return val;
        };
        return @floatFromInt(v);
    }

    pub const Kind = enum {
        decimal,
        hex,
        octal,
        binary,

        pub fn fromToken(token: TokenTag) Kind {
            return switch (token) {
                .numeric_literal => .decimal,
                .hex_literal => .hex,
                .octal_literal => .octal,
                .binary_literal => .binary,
                else => unreachable,
            };
        }
    };
};

/// A BigInt literal (numeric literal with a trailing `n`).
///
/// ## Example
/// ```js
/// 42n
/// // raw = "42"
/// 0xFFn
/// // raw = "0xff"
/// ```
pub const BigIntLiteral = struct {
    /// digits without the trailing `n`
    raw: String = .empty,
};

/// A `true` or `false` literal.
pub const BooleanLiteral = struct {
    value: bool,
};

/// A regular expression literal.
///
/// ## Example
/// ```js
/// const r = /foo/gi;
/// //         ^^^ pattern
/// //             ^^ flags
/// ```
pub const RegExpLiteral = struct {
    pattern: String = .empty,
    flags: String = .empty,
};

/// A template literal with zero or more interpolations.
///
/// ## Example
/// ```js
/// `hello ${name}!`
/// ```
/// `quasis` are the static text spans (`"hello "` and `"!"`), always
/// `expressions.len + 1` elements. `expressions` are the interpolations
/// (here, `name`).
pub const TemplateLiteral = struct {
    /// the static text spans; always `expressions.len + 1` elements
    quasis: IndexRange,
    /// the interpolated expressions
    expressions: IndexRange,
};

/// A single quasi span inside a template literal.
///
/// ## Example
/// ```js
/// `a ${x} b`
/// ```
/// The two quasi elements are `"a "` (`tail = false`) and `" b"` (`tail = true`).
pub const TemplateElement = struct {
    /// escape-decoded content, empty when `is_cooked_undefined`
    cooked: String = .empty,
    /// true for the final element (after the last interpolation)
    tail: bool,
    /// true when the cooked value is undefined per ECMAScript TV semantics
    /// (invalid escape in tagged template)
    is_cooked_undefined: bool = false,
};

/// An identifier used as an expression.
///
/// ## Example
/// ```js
/// console.log(x);
/// ```
/// `console` and `x` are both `IdentifierReference`s. `log` is an `IdentifierName`
/// because it is a property access, not a reference.
pub const IdentifierReference = struct {
    name: String = .empty,
};

/// A `#privateName` identifier used inside a class.
///
/// The stored `name` does not include the leading `#`.
///
/// ## Example
/// ```js
/// class C { #secret = 1; }
/// //        ^^^^^^^ PrivateIdentifier (name = "secret")
/// ```
pub const PrivateIdentifier = struct {
    name: String = .empty,
};

/// An identifier that introduces a new binding.
///
/// ## Example
/// ```ts
/// function foo(x?: number) {}
/// //       ^^^ BindingIdentifier (id)
/// //           ^ BindingIdentifier (optional = true, type_annotation set)
/// ```
pub const BindingIdentifier = struct {
    name: String = .empty,
    /// ts: decorators on a parameter binding (legacy `experimentalDecorators`).
    /// empty for non-parameter bindings.
    decorators: IndexRange = .empty,
    /// ts: `.null` when absent
    type_annotation: NodeIndex = .null,
    /// ts: true when marked optional (`x?: T`)
    optional: bool = false,
};

/// An identifier used as a property key or meta property name.
///
/// ## Example
/// ```js
/// obj.foo
/// //  ^^^ IdentifierName
/// { foo: 1 }
/// //^^^ IdentifierName
/// ```
pub const IdentifierName = struct {
    name: String = .empty,
};

/// An identifier used as a statement label or as the target of `break`/`continue`.
///
/// ## Example
/// ```js
/// outer: for (;;) break outer;
/// //                    ^^^^^ LabelIdentifier (break target)
/// ```
/// The label before `:` (here `outer`) is also a `LabelIdentifier`.
pub const LabelIdentifier = struct {
    name: String = .empty,
};

/// A binding pattern with a default value.
///
/// ## Example
/// ```js
/// function f(x = 0) {}
/// //         ^^^^^ AssignmentPattern
/// //         ^ left
/// //             ^ right
/// ```
pub const AssignmentPattern = struct {
    left: NodeIndex,
    right: NodeIndex,
    /// ts: decorators on a parameter binding (legacy `experimentalDecorators`)
    decorators: IndexRange = .empty,
    /// ts: `.null` when absent
    type_annotation: NodeIndex = .null,
    /// ts: true when marked optional in a parameter position
    optional: bool = false,
};

/// A `...rest` element in a binding pattern or function parameter list.
///
/// ## Example
/// ```js
/// function f(...args) {}
/// //         ^^^^^^^ BindingRestElement
/// //            ^^^^ argument
/// ```
pub const BindingRestElement = struct {
    argument: NodeIndex,
    /// ts: decorators on a parameter rest element (legacy `experimentalDecorators`)
    decorators: IndexRange = .empty,
    /// ts: `.null` when absent
    type_annotation: NodeIndex = .null,
    /// ts: true when marked optional in a parameter position
    optional: bool = false,
};

/// An array destructuring pattern.
///
/// ## Example
/// ```js
/// const [a, , b, ...rest] = arr;
/// //     ^ elements[0]
/// //          ^ elements[2]
/// //             ^^^^^^^ rest
/// ```
/// `elements[1]` is `.null` (hole between the two commas).
pub const ArrayPattern = struct {
    /// binding patterns or `.null` for holes
    elements: IndexRange,
    /// `.null` when no rest element is present
    rest: NodeIndex,
    /// ts: decorators on a parameter binding (legacy `experimentalDecorators`)
    decorators: IndexRange = .empty,
    /// ts: `.null` when absent
    type_annotation: NodeIndex = .null,
    /// ts: true when marked optional in a parameter position
    optional: bool = false,
};

/// An object destructuring pattern.
///
/// ## Example
/// ```js
/// const { a, b: c, ...rest } = obj;
/// //      ^ properties[0]
/// //         ^^^^ properties[1]
/// //               ^^^^^^^ rest
/// ```
pub const ObjectPattern = struct {
    properties: IndexRange,
    /// `.null` when no rest element is present
    rest: NodeIndex,
    /// ts: decorators on a parameter binding (legacy `experimentalDecorators`)
    decorators: IndexRange = .empty,
    /// ts: `.null` when absent
    type_annotation: NodeIndex = .null,
    /// ts: true when marked optional in a parameter position
    optional: bool = false,
};

/// A single property inside an `ObjectPattern`.
///
/// ## Example
/// ```js
/// const { a, b: c, [k]: d } = obj;
/// //      ^ shorthand
/// //         ^^^^ key/value pair (shorthand = false)
/// //               ^^^^^^ computed (key in brackets)
/// ```
pub const BindingProperty = struct {
    key: NodeIndex,
    /// a binding pattern (`BindingIdentifier`, `ArrayPattern`, etc.)
    value: NodeIndex,
    shorthand: bool,
    computed: bool,
};

/// An array literal expression.
///
/// ## Example
/// ```js
/// x = [a, , b, ...c]
/// //   ^ elements[0]
/// //        ^ elements[2]
/// //           ^^^^ elements[3] (SpreadElement)
/// ```
/// `elements[1]` is `.null` (hole between the two commas).
pub const ArrayExpression = struct {
    /// expressions, spread elements, or `.null` for holes
    elements: IndexRange,
};

/// An object literal expression.
///
/// ## Example
/// ```js
/// { a: 1, b, ...c }
/// //^^^^ properties[0]
/// //      ^ properties[1] (shorthand)
/// //         ^^^^ properties[2] (SpreadElement)
/// ```
pub const ObjectExpression = struct {
    /// object properties or spread elements
    properties: IndexRange,
};

/// A `...argument` element inside an array or object literal, or a call argument list.
///
/// ## Example
/// ```js
/// foo(...args)
/// //  ^^^^^^^ SpreadElement
/// //     ^^^^ argument
/// ```
pub const SpreadElement = struct {
    argument: NodeIndex,
};

/// A property in an object literal.
///
/// ## Example
/// ```js
/// ({
///   a: 1,        // kind = init
///   b() {},      // method = true
///   c,           // shorthand = true
///   [k]: v,      // computed = true
///   get d() {},  // kind = get
/// })
/// ```
pub const ObjectProperty = struct {
    key: NodeIndex,
    /// expression for init, or a `Function` for methods/getters/setters
    value: NodeIndex,
    kind: PropertyKind,
    method: bool,
    shorthand: bool,
    computed: bool,
};

/// The top-level node of every parsed file.
///
/// ## Example
/// ```js
/// #!/usr/bin/env node
/// "use strict";
/// import x from "y";
/// console.log(x);
/// ```
/// `hashbang` is the `#!` line if present. `body` contains directives
/// (`"use strict";`), imports, and statements.
pub const Program = struct {
    source_type: SourceType,
    /// statements and directives
    body: IndexRange,
    /// `null` when the file has no `#!` line
    hashbang: ?Hashbang = null,
};

/// A hashbang comment at the top of a source file.
///
/// ## Example
/// ```js
/// #!/usr/bin/env node
/// ```
/// `value` is everything after `#!`, so `/usr/bin/env node` here.
pub const Hashbang = struct {
    value: String = .empty,
};

/// A directive prologue such as `"use strict";`.
///
/// ## Example
/// ```js
/// "use strict";
/// ```
/// `expression` is the underlying `StringLiteral` (`"use strict"`).
/// `value` is the text between the quotes (`use strict`).
pub const Directive = struct {
    /// the underlying `StringLiteral`
    expression: NodeIndex,
    /// the directive text without surrounding quotes
    value: String = .empty,
};

/// Form of a function node, matching its ESTree output type.
pub const FunctionType = enum {
    function_declaration,
    function_expression,
    /// `declare function f(): void;`
    ts_declare_function,
    /// body-less signature used in ambient contexts and overloads
    ts_empty_body_function_expression,
};

/// A function declaration or expression.
///
/// The `declare function` form is encoded by `type = .ts_declare_function`;
/// there is no separate flag.
///
/// ## Example
/// ```ts
/// async function* foo<T>(x: T): T { yield x; }
/// //            ^ generator
/// //              ^^^ id
/// //                 ^^^ type_parameters
/// //                    ^^^^^^ params
/// //                          ^^^ return_type
/// //                              ^^^^^^^^^^^^ body
/// ```
/// `async = true` for `async function`.
pub const Function = struct {
    type: FunctionType,
    /// `.null` for anonymous functions
    id: NodeIndex,
    generator: bool,
    async: bool,
    params: NodeIndex,
    /// `.null` for `declare function` and empty-body signatures
    body: NodeIndex,
    /// ts: `.null` when absent
    type_parameters: NodeIndex = .null,
    /// ts: `.null` when absent
    return_type: NodeIndex = .null,
};

/// The body of a function. Structurally identical to `BlockStatement`
/// but kept distinct so the root of a function stays typed.
pub const FunctionBody = struct {
    body: IndexRange,
};

/// A braced block statement.
///
/// ## Example
/// ```js
/// { stmt1; stmt2; }
/// //^^^^^^^^^^^^^ body
/// ```
pub const BlockStatement = struct {
    body: IndexRange,
};

/// The parameter list of a function.
///
/// ## Example
/// ```js
/// function f(a, b = 1, ...rest) {}
/// //         ^^^^^^^^ items
/// //                   ^^^^^^^ rest
/// ```
pub const FormalParameters = struct {
    items: IndexRange,
    /// `.null` when no rest element is present
    rest: NodeIndex,
    kind: FormalParameterKind,
};

/// A thin wrapper marking a binding pattern as a function parameter.
///
/// TypeScript metadata (decorators, type annotation, optional) lives on
/// the inner pattern. The ESTree decoder unwraps this node to that pattern.
pub const FormalParameter = struct {
    /// a binding pattern (`BindingIdentifier`, `ObjectPattern`, `ArrayPattern`,
    /// `AssignmentPattern`)
    pattern: NodeIndex,
};

/// An expression wrapped in parentheses, preserved when `preserveParens` is on.
///
/// ## Example
/// ```js
/// x + (a + b)
/// //   ^^^^^ expression
/// ```
pub const ParenthesizedExpression = struct {
    expression: NodeIndex,
};

/// An arrow function expression.
///
/// ## Example
/// ```ts
/// async <T>(x: T): T => x
/// //    ^^^ type_parameters
/// //       ^^^^^^ params
/// //             ^^^ return_type
/// //                    ^ body (expression = true)
/// ```
/// `async = true` for `async (...) => ...`.
pub const ArrowFunctionExpression = struct {
    /// true for concise body `() => expr`; false for block body `() => { ... }`
    expression: bool,
    async: bool,
    params: NodeIndex,
    /// a `FunctionBody` when `expression` is false, otherwise an expression
    body: NodeIndex,
    /// ts: `.null` when absent
    type_parameters: NodeIndex = .null,
    /// ts: `.null` when absent
    return_type: NodeIndex = .null,
};

/// A comma-separated sequence of expressions.
///
/// ## Example
/// ```js
/// x = (a, b, c)
/// //   ^^^^^^^ expressions
/// ```
pub const SequenceExpression = struct {
    expressions: IndexRange,
};

/// Property access, in static, computed, or optional form.
///
/// ## Example
/// ```js
/// obj.foo          // computed = false, optional = false
/// obj[expr]        // computed = true
/// obj?.foo         // optional = true
/// obj.#priv        // property is a PrivateIdentifier
/// ```
pub const MemberExpression = struct {
    object: NodeIndex,
    /// an expression when `computed`, otherwise `IdentifierName` or `PrivateIdentifier`
    property: NodeIndex,
    computed: bool,
    /// true for `obj?.foo`
    optional: bool,
};

/// A function call.
///
/// ## Example
/// ```ts
/// foo<T>(a, ...b)
/// // ^^^ type_arguments
/// //     ^^^^^^^ arguments
/// ```
/// `callee` is the called expression (here `foo`). `optional = true` for `foo?.()`.
pub const CallExpression = struct {
    callee: NodeIndex,
    /// expressions or spread elements
    arguments: IndexRange,
    /// true for `foo?.()`
    optional: bool,
    /// ts: `.null` when absent
    type_arguments: NodeIndex = .null,
};

/// Wraps an optional chain so that short-circuiting applies to the whole chain.
///
/// ## Example
/// ```js
/// r = foo?.bar.baz
/// //  ^^^^^^^^^^^^ expression
/// ```
pub const ChainExpression = struct {
    /// a `CallExpression` or `MemberExpression` with an optional link in the chain
    expression: NodeIndex,
};

/// A tagged template expression.
///
/// ## Example
/// ```ts
/// obj.tag<T>`hello`
/// //  ^^^ tag (the last property access)
/// //     ^^^ type_arguments
/// //        ^^^^^^^ quasi (TemplateLiteral)
/// ```
pub const TaggedTemplateExpression = struct {
    tag: NodeIndex,
    quasi: NodeIndex,
    /// ts: `.null` when absent
    type_arguments: NodeIndex = .null,
};

/// A `new` expression.
///
/// ## Example
/// ```ts
/// new Foo<T>(a, b)
/// //  ^^^ callee
/// //     ^^^ type_arguments
/// //         ^^^^ arguments
/// ```
pub const NewExpression = struct {
    callee: NodeIndex,
    arguments: IndexRange,
    /// ts: `.null` when absent
    type_arguments: NodeIndex = .null,
};

/// An `await` expression.
///
/// ## Example
/// ```js
/// await promise
/// //    ^^^^^^^ argument
/// ```
pub const AwaitExpression = struct {
    argument: NodeIndex,
};

/// A `yield` or `yield*` expression.
///
/// ## Example
/// ```js
/// yield x      // delegate = false
/// yield* iter  // delegate = true
/// yield        // argument = .null
/// ```
pub const YieldExpression = struct {
    /// `.null` for bare `yield`
    argument: NodeIndex,
    /// true for `yield*`
    delegate: bool,
};

/// A meta property.
///
/// ## Example
/// ```js
/// import.meta
/// //     ^^^^ property
/// new.target
/// //  ^^^^^^ property
/// ```
/// `meta` is the head keyword (`import` or `new`).
pub const MetaProperty = struct {
    meta: NodeIndex,
    property: NodeIndex,
};

/// `value` vs `type` on a TypeScript import or export specifier.
///
/// ## Example
/// ```ts
/// import type { T } from "m";
/// //     ^^^^ kind = type
/// import { type T, v } from "m";
/// //       ^^^^ specifier kind = type
/// //               ^ specifier kind = value
/// ```
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

/// Accessibility modifier on a TypeScript class member.
///
/// `.none` means no modifier was written (distinct from `public`, which
/// was written explicitly).
pub const Accessibility = enum {
    none,
    public,
    private,
    protected,

    pub fn toString(self: Accessibility) []const u8 {
        return switch (self) {
            .none => "",
            .public => "public",
            .private => "private",
            .protected => "protected",
        };
    }
};

/// Stage 3 import phase modifier.
///
/// ## Example
/// ```js
/// import source x from "m";  // phase = .source
/// import defer * as x from "m";  // phase = .defer
/// ```
pub const ImportPhase = enum {
    source,
    @"defer",
};

/// A dynamic `import()` call or phased import.
///
/// ## Example
/// ```js
/// import("m")                 // phase = null
/// import("m", { with: {} })   // options set
/// import.source("m")          // phase = .source
/// import.defer("m")           // phase = .defer
/// ```
pub const ImportExpression = struct {
    source: NodeIndex,
    /// `.null` when no options argument was passed
    options: NodeIndex,
    /// `null` for a plain `import(...)`
    phase: ?ImportPhase,
};

/// A static `import` declaration.
///
/// ## Example
/// ```ts
/// import { foo as bar } from "m";
/// //       ^^^^^^^^^^ specifiers[0]
/// //                         ^^^ source
/// ```
/// `import_kind = .type` for `import type { ... }`; `phase` is set for Stage 3
/// `import source` / `import defer` forms; `attributes` holds a trailing
/// `with { ... }` clause.
pub const ImportDeclaration = struct {
    specifiers: IndexRange,
    source: NodeIndex,
    attributes: IndexRange,
    /// `null` for a regular import
    phase: ?ImportPhase,
    /// ts: `.type` for `import type { ... }`
    import_kind: ImportOrExportKind = .value,
};

/// A single `{ imported as local }` specifier in an import declaration.
///
/// ## Example
/// ```ts
/// import { foo as bar } from "m";
/// //       ^^^ imported
/// //              ^^^ local
///
/// import { type T } from "m";
/// //       ^^^^ import_kind = type
/// ```
pub const ImportSpecifier = struct {
    /// `IdentifierName` or `StringLiteral`
    imported: NodeIndex,
    local: NodeIndex,
    /// ts: `.type` for `import { type X }`
    import_kind: ImportOrExportKind = .value,
};

/// The default-binding specifier in an import declaration.
///
/// ## Example
/// ```js
/// import local from "m";
/// //     ^^^^^ local
/// ```
pub const ImportDefaultSpecifier = struct {
    local: NodeIndex,
};

/// A `* as local` namespace import specifier.
///
/// ## Example
/// ```js
/// import * as ns from "m";
/// //          ^^ local
/// ```
pub const ImportNamespaceSpecifier = struct {
    local: NodeIndex,
};

/// A single `key: value` attribute in a `with { ... }` clause.
///
/// ## Example
/// ```js
/// import x from "m" with { type: "json" };
/// //                       ^^^^ key
/// //                             ^^^^^^ value
/// ```
pub const ImportAttribute = struct {
    /// `IdentifierName` or `StringLiteral`
    key: NodeIndex,
    value: NodeIndex,
};

/// An `export { ... }` or `export <decl>` declaration.
///
/// ## Example
/// ```ts
/// export type { foo, bar } from "m";
/// //     ^^^^ export_kind = type
/// //            ^^^^^^^^ specifiers
/// //                            ^^^ source
/// ```
/// For `export const x = 1`, `declaration` is the inner `VariableDeclaration`
/// and `specifiers` is empty.
pub const ExportNamedDeclaration = struct {
    /// `.null` for the `export { ... }` form
    declaration: NodeIndex,
    specifiers: IndexRange,
    /// `.null` when there is no `from` clause
    source: NodeIndex,
    attributes: IndexRange,
    /// ts: `.type` for `export type { ... }`
    export_kind: ImportOrExportKind = .value,
};

/// An `export default ...` declaration.
///
/// ## Example
/// ```js
/// export default foo;
/// //             ^^^ declaration
/// export default function () {}
/// //             ^^^^^^^^^^^^^^ declaration (Function)
/// ```
pub const ExportDefaultDeclaration = struct {
    /// an expression, `Function`, or `Class`
    declaration: NodeIndex,
};

/// An `export * from "m"` or `export * as ns from "m"` declaration.
///
/// ## Example
/// ```ts
/// export * as ns from "m";
/// //          ^^ exported
/// //                  ^^^ source
/// ```
/// `export_kind = .type` for `export type * from "..."`.
pub const ExportAllDeclaration = struct {
    /// `.null` for `export *` without `as`
    exported: NodeIndex,
    source: NodeIndex,
    attributes: IndexRange,
    /// ts: `.type` for `export type * from "..."`
    export_kind: ImportOrExportKind = .value,
};

/// A single `{ local as exported }` specifier in an export declaration.
///
/// ## Example
/// ```ts
/// export { foo as bar };
/// //       ^^^ local
/// //              ^^^ exported
///
/// export { type X };
/// //       ^^^^ export_kind = type
/// ```
pub const ExportSpecifier = struct {
    /// `IdentifierReference`, or `IdentifierName` / `StringLiteral` when re-exporting from a module
    local: NodeIndex,
    /// `IdentifierName` or `StringLiteral`
    exported: NodeIndex,
    /// ts: `.type` for `export { type X }`
    export_kind: ImportOrExportKind = .value,
};

/// TypeScript `export = expr` (CommonJS-style ambient export).
///
/// ## Example
/// ```ts
/// export = MyNamespace;
/// //       ^^^^^^^^^^^ expression
/// ```
pub const TSExportAssignment = struct {
    expression: NodeIndex,
};

/// TypeScript `export as namespace Name` (UMD ambient namespace export).
///
/// ## Example
/// ```ts
/// export as namespace MyLib;
/// //                  ^^^^^ id
/// ```
pub const TSNamespaceExportDeclaration = struct {
    id: NodeIndex,
};

/// A `: Type` annotation wrapper.
///
/// The span starts at the `:` token and covers the inner type.
///
/// ## Example
/// ```ts
/// let x: number = 0;
/// //   ^^^^^^^^ TSTypeAnnotation
/// //     ^^^^^^ type_annotation
/// ```
pub const TSTypeAnnotation = struct {
    /// the inner `TSType` node
    type_annotation: NodeIndex,
};

/// A JSX element, possibly self-closing.
///
/// ## Example
/// ```jsx
/// x = <Foo bar="baz">hello</Foo>
/// //  ^^^^^^^^^^^^^^^ opening_element
/// //                 ^^^^^ children
/// //                      ^^^^^^ closing_element
/// ```
/// `closing_element = .null` for self-closing tags like `<Foo />`.
pub const JSXElement = struct {
    opening_element: NodeIndex,
    children: IndexRange,
    /// `.null` for self-closing tags
    closing_element: NodeIndex,
};

/// The opening `<Foo ...>` of a JSX element.
///
/// ## Example
/// ```tsx
/// <Foo<T> bar={baz} />
/// //  ^^^ type_arguments
/// //      ^^^^^^^^^ attributes
/// ```
/// `name` is the tag (here `Foo`). `self_closing = true` for the trailing `/>`.
pub const JSXOpeningElement = struct {
    /// `JSXIdentifier`, `JSXNamespacedName`, or `JSXMemberExpression`
    name: NodeIndex,
    attributes: IndexRange,
    self_closing: bool,
    /// ts: `.null` when absent
    type_arguments: NodeIndex = .null,
};

/// The closing `</Foo>` of a JSX element.
pub const JSXClosingElement = struct {
    /// `JSXIdentifier`, `JSXNamespacedName`, or `JSXMemberExpression`
    name: NodeIndex,
};

/// A JSX fragment `<>...</>`.
///
/// ## Example
/// ```jsx
/// x = <>hello</>
/// //    ^^^^^ children
/// ```
/// `opening_fragment` is `<>`, `closing_fragment` is `</>`.
pub const JSXFragment = struct {
    opening_fragment: NodeIndex,
    children: IndexRange,
    closing_fragment: NodeIndex,
};

/// The opening `<>` of a JSX fragment.
pub const JSXOpeningFragment = struct {};

/// The closing `</>` of a JSX fragment.
pub const JSXClosingFragment = struct {};

/// An identifier used as a JSX tag name or attribute name.
///
/// ## Example
/// ```jsx
/// <Foo bar="baz" />
/// //   ^^^ JSXIdentifier (attribute name)
/// ```
/// The tag `Foo` is also a `JSXIdentifier`.
pub const JSXIdentifier = struct {
    name: String = .empty,
};

/// A JSX name of the form `namespace:name`.
///
/// ## Example
/// ```jsx
/// <svg:path />
/// //   ^^^^ name
/// ```
/// `namespace` is the portion before `:` (here `svg`).
pub const JSXNamespacedName = struct {
    namespace: NodeIndex,
    name: NodeIndex,
};

/// A dotted JSX tag name.
///
/// ## Example
/// ```jsx
/// <Foo.Bar.Baz />
/// //       ^^^ property
/// ```
/// `object` is the qualifier (`Foo.Bar`, itself a `JSXMemberExpression`).
pub const JSXMemberExpression = struct {
    /// `JSXIdentifier` or a nested `JSXMemberExpression`
    object: NodeIndex,
    property: NodeIndex,
};

/// A single JSX attribute.
///
/// ## Example
/// ```jsx
/// <Foo bar="baz" disabled />
/// //   ^^^ name
/// //       ^^^^^ value (StringLiteral)
/// //             ^^^^^^^^ boolean attribute (value = .null)
/// ```
pub const JSXAttribute = struct {
    /// `JSXIdentifier` or `JSXNamespacedName`
    name: NodeIndex,
    /// `StringLiteral`, `JSXExpressionContainer`, `JSXElement`, `JSXFragment`,
    /// or `.null` for boolean-like attributes
    value: NodeIndex,
};

/// A spread attribute `{...props}`.
///
/// ## Example
/// ```jsx
/// <Foo {...props} />
/// //    ^^^^^^^^ argument
/// ```
pub const JSXSpreadAttribute = struct {
    argument: NodeIndex,
};

/// A `{expression}` container inside JSX.
///
/// ## Example
/// ```jsx
/// <Foo bar={baz}>{children}</Foo>
/// //       ^^^^^ attribute container
/// //             ^^^^^^^^^^ child container
/// ```
pub const JSXExpressionContainer = struct {
    /// a `JSXEmptyExpression` for `{}` or any expression otherwise
    expression: NodeIndex,
};

/// The empty `{}` placeholder inside a JSX element.
pub const JSXEmptyExpression = struct {};

/// A span of raw text inside a JSX element or fragment.
///
/// ## Example
/// ```jsx
/// <Foo>hello world</Foo>
/// //   ^^^^^^^^^^^ value
/// ```
pub const JSXText = struct {
    value: String = .empty,
};

/// A spread child `{...children}` inside a JSX element.
///
/// ## Example
/// ```jsx
/// <Foo>{...kids}</Foo>
/// //    ^^^^^^^ expression
/// ```
pub const JSXSpreadChild = struct {
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
    super: Super,
    string_literal: StringLiteral,
    numeric_literal: NumericLiteral,
    bigint_literal: BigIntLiteral,
    boolean_literal: BooleanLiteral,
    null_literal: NullLiteral,
    this_expression: ThisExpression,
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
    debugger_statement: DebuggerStatement,
    empty_statement: EmptyStatement,
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
    ts_type_annotation: TSTypeAnnotation,

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
