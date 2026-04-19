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

/// Form of a function node.
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
/// the inner pattern.
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

// ts: annotation wrapper

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

// ts: primitive keyword types

/// The `any` primitive type. Disables all type checking for the annotated value.
///
/// ## Example
/// ```ts
/// let x: any;
/// //     ^^^ TSAnyKeyword
/// ```
pub const TSAnyKeyword = struct {};

/// The `unknown` primitive type. The type-safe counterpart of `any`.
///
/// ## Example
/// ```ts
/// let x: unknown;
/// //     ^^^^^^^ TSUnknownKeyword
/// ```
pub const TSUnknownKeyword = struct {};

/// The `never` primitive type. Represents values that never occur (for example
/// the return type of a function that always throws).
///
/// ## Example
/// ```ts
/// function fail(): never { throw new Error(); }
/// //               ^^^^^ TSNeverKeyword
/// ```
pub const TSNeverKeyword = struct {};

/// The `void` keyword used in type position. Typically the return type of a
/// function that returns no meaningful value.
///
/// ## Example
/// ```ts
/// function log(): void {}
/// //              ^^^^ TSVoidKeyword
/// ```
pub const TSVoidKeyword = struct {};

/// The `null` keyword used in type position.
///
/// ## Example
/// ```ts
/// let x: string | null;
/// //              ^^^^ TSNullKeyword
/// ```
pub const TSNullKeyword = struct {};

/// The `undefined` keyword used in type position.
///
/// ## Example
/// ```ts
/// let x: string | undefined;
/// //              ^^^^^^^^^ TSUndefinedKeyword
/// ```
pub const TSUndefinedKeyword = struct {};

/// The `string` primitive type.
///
/// ## Example
/// ```ts
/// let x: string;
/// //     ^^^^^^ TSStringKeyword
/// ```
pub const TSStringKeyword = struct {};

/// The `number` primitive type.
///
/// ## Example
/// ```ts
/// let x: number;
/// //     ^^^^^^ TSNumberKeyword
/// ```
pub const TSNumberKeyword = struct {};

/// The `bigint` primitive type. Integer values of arbitrary precision.
///
/// ## Example
/// ```ts
/// let x: bigint;
/// //     ^^^^^^ TSBigIntKeyword
/// ```
pub const TSBigIntKeyword = struct {};

/// The `boolean` primitive type.
///
/// ## Example
/// ```ts
/// let x: boolean;
/// //     ^^^^^^^ TSBooleanKeyword
/// ```
pub const TSBooleanKeyword = struct {};

/// The `symbol` primitive type.
///
/// ## Example
/// ```ts
/// let x: symbol;
/// //     ^^^^^^ TSSymbolKeyword
/// ```
pub const TSSymbolKeyword = struct {};

/// The `object` primitive type. Any non-primitive value.
///
/// ## Example
/// ```ts
/// let x: object;
/// //     ^^^^^^ TSObjectKeyword
/// ```
pub const TSObjectKeyword = struct {};

/// The `intrinsic` keyword. Marks a type as built into the TypeScript compiler
/// (for example `Uppercase<T>` and other string-manipulation utilities).
///
/// ## Example
/// ```ts
/// type Uppercase<S extends string> = intrinsic;
/// //                                 ^^^^^^^^^ TSIntrinsicKeyword
/// ```
pub const TSIntrinsicKeyword = struct {};

/// The polymorphic `this` type. Refers to the type of the enclosing class or
/// interface at the usage site.
///
/// ## Example
/// ```ts
/// class C { self(): this { return this; } }
/// //                ^^^^ TSThisType
/// ```
pub const TSThisType = struct {};

// ts: named references

/// A reference to a named type, optionally applied to type arguments.
///
/// `type_name` carries the identifier or dotted path that names the type, and
/// `type_arguments` carries the `<T, U>` instantiation when present.
///
/// ## Example
/// ```ts
/// let x: Foo;
/// //     ^^^ TSTypeReference (type_arguments = .null)
/// let y: Promise<number>;
/// //     ^^^^^^^^^^^^^^^ TSTypeReference
/// //     ^^^^^^^ type_name
/// //            ^^^^^^^^ type_arguments
/// let z: Tools.Pos;
/// //     ^^^^^^^^^ TSTypeReference (type_name is a TSQualifiedName)
/// ```
pub const TSTypeReference = struct {
    /// `IdentifierReference`, `TSQualifiedName`, or `ThisExpression`
    type_name: NodeIndex,
    /// `TSTypeParameterInstantiation` or `.null` when absent
    type_arguments: NodeIndex = .null,
};

/// A dotted type name like `A.B.C`. Left associative, so `A.B.C` is parsed as
/// `(A.B).C` with the outer `TSQualifiedName` holding the inner one as `left`.
///
/// ## Example
/// ```ts
/// let x: Tools.Pos;
/// //     ^^^^^^^^^ TSQualifiedName
/// //     ^^^^^ left (IdentifierReference, or nested TSQualifiedName)
/// //           ^^^ right (IdentifierName)
/// ```
pub const TSQualifiedName = struct {
    /// `IdentifierReference`, `TSQualifiedName`, or `ThisExpression`
    left: NodeIndex,
    /// `IdentifierName`
    right: NodeIndex,
};

// ts: type parameters

/// A single `<T>` type parameter introduced by a generic declaration. Carries
/// the parameter name along with the optional `extends` constraint, optional
/// default type, and the three variance / invariance modifier keywords.
///
/// The span starts at the first modifier keyword when present, otherwise at
/// the name, and ends at the default (if present), the constraint (if
/// present), or the name.
///
/// ## Example
/// ```ts
/// type Foo<T extends Bar = Baz> = T;
/// //       ^^^^^^^^^^^^^^^^^^^ TSTypeParameter
/// //       ^                   name
/// //                 ^^^       constraint
/// //                       ^^^ default
/// type Rec<in out K, const T> = ...;
/// //       ^^^^^^^^^          in = true, out = true
/// //                 ^^^^^^^  const = true
/// ```
pub const TSTypeParameter = struct {
    /// `BindingIdentifier` naming the parameter
    name: NodeIndex,
    /// `TSType` bound introduced by `extends`, or `.null` when absent
    constraint: NodeIndex = .null,
    /// default `TSType` introduced by `=`, or `.null` when absent
    default: NodeIndex = .null,
    /// `in` variance modifier keyword was present
    in: bool = false,
    /// `out` variance modifier keyword was present
    out: bool = false,
    /// `const` modifier keyword was present
    @"const": bool = false,
};

/// The `<T, U>` type parameter list introduced by a generic declaration
/// (type alias, interface, class, function, method, constructor, mapped
/// type, and so on). Holds the parameters in source order.
///
/// ## Example
/// ```ts
/// type Pair<A, B extends A> = [A, B];
/// //       ^^^^^^^^^^^^^^^^ TSTypeParameterDeclaration
/// //        ^  ^^^^^^^^^^^  params
/// ```
pub const TSTypeParameterDeclaration = struct {
    /// the `TSTypeParameter` entries in source order
    params: IndexRange,
};

/// The `<T, U>` type argument list applied to a type reference, call site,
/// `new` expression, tagged template, JSX opening element, or instantiation
/// expression. Holds the arguments in source order.
///
/// ## Example
/// ```ts
/// let x: Promise<number, string>;
/// //            ^^^^^^^^^^^^^^^^ TSTypeParameterInstantiation
/// //             ^^^^^^  ^^^^^^ params
/// ```
pub const TSTypeParameterInstantiation = struct {
    /// the `TSType` arguments in source order
    params: IndexRange,
};

// ts: literal in type position

/// A literal value used in type position. Wraps a string, numeric, bigint,
/// boolean, or no-substitution template literal directly, or a `UnaryExpression`
/// when the literal is preceded by a `-` or `+` sign (for example `-1`).
///
/// Template literals that contain interpolations use `TSTemplateLiteralType`
/// rather than `TSLiteralType`. The `null` keyword uses `TSNullKeyword`.
///
/// ## Example
/// ```ts
/// type A = "hello";
/// //       ^^^^^^^ TSLiteralType
/// type B = 42;
/// //       ^^ TSLiteralType
/// type C = true;
/// //       ^^^^ TSLiteralType
/// type D = -1;
/// //       ^^ TSLiteralType (literal is a UnaryExpression)
/// ```
pub const TSLiteralType = struct {
    /// `StringLiteral`, `NumericLiteral`, `BigIntLiteral`, `BooleanLiteral`,
    /// `TemplateLiteral`, or `UnaryExpression` wrapping one of the numeric kinds
    literal: NodeIndex,
};

/// A template literal used in type position with one or more interpolations.
///
/// Parallels the expression-level `TemplateLiteral` but with types filling the
/// interpolation slots instead of expressions. `quasis` holds the static text
/// spans as `TemplateElement` nodes and always has exactly `types.len + 1`
/// elements. The final quasi is marked `tail = true`.
///
/// A template literal with no interpolations is still parsed as a
/// `TSLiteralType` wrapping a `TemplateLiteral`, not as a
/// `TSTemplateLiteralType`.
///
/// The span covers the opening and closing backticks.
///
/// ## Example
/// ```ts
/// type Greeting<N extends string> = `Hello, ${N}!`;
/// //                                 ^^^^^^^^^^^^^ TSTemplateLiteralType
/// //                                 ^^^^^^^^      quasis[0] ("Hello, ")
/// //                                         ^     types[0]  (N)
/// //                                          ^^   quasis[1] ("!", tail)
/// type Dot<T extends string, U extends string> = `${T}.${U}`;
/// //                                              ^^^^^^^^^ TSTemplateLiteralType
/// //                                              quasis: ["", ".", ""]
/// //                                              types:  [T, U]
/// ```
pub const TSTemplateLiteralType = struct {
    /// the static text spans; always `types.len + 1` elements
    quasis: IndexRange,
    /// the interpolated types, one per `${...}` slot
    types: IndexRange,
};

// ts: postfix types

/// An array type. Applies the postfix `[]` suffix to an element type.
///
/// Stacks naturally: `T[][]` is a `TSArrayType` whose `element_type` is another
/// `TSArrayType`.
///
/// ## Example
/// ```ts
/// let xs: number[];
/// //      ^^^^^^^^ TSArrayType
/// //      ^^^^^^ element_type (TSNumberKeyword)
/// let ys: string[][];
/// //      ^^^^^^^^^^ TSArrayType (element_type is another TSArrayType)
/// ```
pub const TSArrayType = struct {
    /// the inner type that `[]` is applied to
    element_type: NodeIndex,
};

/// An indexed access type. Looks up the type of the property named by
/// `index_type` on `object_type`, mirroring expression-level member access
/// but in type position.
///
/// Stacks naturally: `T[K][L]` is a `TSIndexedAccessType` whose `object_type`
/// is another `TSIndexedAccessType`.
///
/// ## Example
/// ```ts
/// type A = Person["age"];
/// //       ^^^^^^^^^^^^^ TSIndexedAccessType
/// //       ^^^^^^ object_type
/// //              ^^^^^ index_type (TSLiteralType)
/// type B = T[K];
/// //       ^^^^ TSIndexedAccessType
/// ```
pub const TSIndexedAccessType = struct {
    /// the type being indexed into
    object_type: NodeIndex,
    /// the type used as the index key
    index_type: NodeIndex,
};

// ts: tuple types

/// A tuple type. A fixed length sequence of positional or named elements
/// whose types can differ from slot to slot. Elements may be marked optional
/// with `?` and the trailing element may be a rest element with `...`.
///
/// Each entry in `element_types` is one of:
/// - a plain `TSType` for a positional element
/// - a `TSOptionalType` for `Type?` in positional form
/// - a `TSRestType` for `...Type` in positional form, optionally wrapping a
///   `TSNamedTupleMember`
/// - a `TSNamedTupleMember` for `label: Type` or `label?: Type`
///
/// The span covers the opening `[` through the closing `]`.
///
/// ## Example
/// ```ts
/// type T = [string, number?, ...boolean[]];
/// //       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ TSTupleType
/// type P = [first: string, second: number];
/// //       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ TSTupleType with TSNamedTupleMember entries
/// ```
pub const TSTupleType = struct {
    /// the tuple's elements in source order
    element_types: IndexRange,
};

/// A labeled element inside a tuple type. Carries a name that appears only as
/// documentation in TypeScript (it has no runtime effect) alongside the element
/// type. The whole member may be marked optional with a trailing `?` on the
/// label; the `?` modifies the tuple slot itself rather than wrapping the inner
/// type in a `TSOptionalType`.
///
/// When preceded by `...` the member is wrapped in a `TSRestType` whose inner
/// is this `TSNamedTupleMember`, matching TypeScript's spec shape for named
/// rest elements like `[...selectors: S]`.
///
/// ## Example
/// ```ts
/// type Pair = [first: string, second?: number];
/// //           ^^^^^^^^^^^^^^ TSNamedTupleMember (optional = false)
/// //                          ^^^^^^^^^^^^^^^^ TSNamedTupleMember (optional = true)
/// type R = [...rest: number[]];
/// //        ^^^^^^^^^^^^^^^^^ TSRestType wrapping TSNamedTupleMember
/// ```
pub const TSNamedTupleMember = struct {
    /// `IdentifierName` labeling the element
    label: NodeIndex,
    /// the `TSType` of the element
    element_type: NodeIndex,
    /// true when the label is followed by `?`
    optional: bool = false,
};

/// An optional element inside a tuple type. Parsed as a `Type?` suffix on an
/// unnamed tuple element; the `?` marks the slot as optional without changing
/// the element's underlying type.
///
/// Only valid as a direct tuple element. An optional marker on a named tuple
/// element (`label?: Type`) is captured on `TSNamedTupleMember.optional`
/// rather than producing a `TSOptionalType`.
///
/// ## Example
/// ```ts
/// type T = [number, string?];
/// //                ^^^^^^^ TSOptionalType
/// //                ^^^^^^ type_annotation (TSStringKeyword)
/// ```
pub const TSOptionalType = struct {
    /// the inner `TSType` that is made optional
    type_annotation: NodeIndex,
};

/// A rest element inside a tuple type. Marks the trailing slot as consuming
/// zero or more elements whose type is the inner annotation.
///
/// The inner may be any `TSType` (typically an array or tuple type) or a
/// `TSNamedTupleMember` for named rest elements like `[...rest: T[]]`.
///
/// ## Example
/// ```ts
/// type T = [number, ...string[]];
/// //                ^^^^^^^^^^^ TSRestType
/// //                   ^^^^^^^^ type_annotation (TSArrayType)
/// type U = [...rest: number[]];
/// //        ^^^^^^^^^^^^^^^^^ TSRestType wrapping TSNamedTupleMember
/// ```
pub const TSRestType = struct {
    /// the inner type consumed by `...`
    type_annotation: NodeIndex,
};

// ts: compound types

/// A union type. Combines two or more types with `|`, representing a value
/// that is any one of the constituents.
///
/// Binds looser than `TSIntersectionType`, so `A & B | C` parses as
/// `(A & B) | C`. A leading `|` before the first operand is allowed and is
/// preserved in the span: `type A = | string` yields a single-operand
/// `TSUnionType` whose span starts at the leading `|` rather than at the
/// first operand.
///
/// ## Example
/// ```ts
/// type A = string | number | boolean;
/// //       ^^^^^^^^^^^^^^^^^^^^^^^^^ TSUnionType
/// type B =
///     | { kind: "ok" }
///     | { kind: "err"; message: string };
/// ```
pub const TSUnionType = struct {
    /// the constituent types in source order
    types: IndexRange,
};

/// An intersection type. Combines two or more types with `&`, representing a
/// value that satisfies every constituent simultaneously.
///
/// Binds tighter than `TSUnionType`, so `A | B & C` parses as `A | (B & C)`.
/// A leading `&` before the first operand is allowed and is preserved in the
/// span: `type A = & X` yields a single-operand `TSIntersectionType` whose
/// span starts at the leading `&` rather than at the first operand.
///
/// ## Example
/// ```ts
/// type A = Named & Aged;
/// //       ^^^^^^^^^^^^ TSIntersectionType
/// type B = { name: string } & { age: number } & Serializable;
/// ```
pub const TSIntersectionType = struct {
    /// the constituent types in source order
    types: IndexRange,
};

/// A conditional type. Selects between two branches based on whether
/// `check_type` is assignable to `extends_type`.
///
/// ## Example
/// ```ts
/// type IsString<T> = T extends string ? "yes" : "no";
/// //                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ TSConditionalType
/// //                 ^ check_type
/// //                           ^^^^^^ extends_type
/// //                                    ^^^^^ true_type
/// //                                            ^^^^ false_type
/// ```
pub const TSConditionalType = struct {
    /// the type on the left of `extends`
    check_type: NodeIndex,
    /// the type on the right of `extends` that `check_type` is tested against
    extends_type: NodeIndex,
    /// the type selected when `check_type` is assignable to `extends_type`
    true_type: NodeIndex,
    /// the type selected when `check_type` is not assignable to `extends_type`
    false_type: NodeIndex,
};

// ts: infer in conditional types

/// An `infer` type placeholder. Introduces a new type variable that captures
/// a position inside the extends branch of a conditional type, made available
/// in the true branch of that conditional.
///
/// The span starts at the `infer` keyword and ends at the name or the
/// constraint (when present).
///
/// ## Example
/// ```ts
/// type ReturnTypeOf<V> = V extends (...args: any[]) => infer R ? R : never;
/// //                                                   ^^^^^^^ TSInferType
/// //                                                         ^ type_parameter (name only)
/// type Head<T> = T extends [infer H extends string, ...any[]] ? H : never;
/// //                        ^^^^^^^^^^^^^^^^^^^^^^ TSInferType
/// //                              ^^^^^^^^^^^^^^^^ type_parameter (name + constraint)
/// ```
pub const TSInferType = struct {
    /// the `TSTypeParameter` holding the introduced name and optional
    /// `extends` constraint
    type_parameter: NodeIndex,
};

// ts: prefix operator type

/// The prefix operator used by `TSTypeOperator`.
///
/// `keyof` produces the union of property keys of the operand, `unique` marks
/// a unique symbol type (only meaningful before `symbol`), and `readonly`
/// applies to tuple and array types to make their elements immutable.
pub const TSTypeOperatorKind = enum {
    keyof,
    unique,
    readonly,

    pub fn toString(self: TSTypeOperatorKind) []const u8 {
        return switch (self) {
            .keyof => "keyof",
            .unique => "unique",
            .readonly => "readonly",
        };
    }
};

/// A prefix type operator. Wraps an inner type with `keyof`, `unique`, or
/// `readonly`.
///
/// The span starts at the operator keyword and extends to the end of the
/// inner type.
///
/// ## Example
/// ```ts
/// type Keys = keyof Person;
/// //          ^^^^^^^^^^^^ TSTypeOperator (operator = keyof)
/// let id: unique symbol;
/// //      ^^^^^^^^^^^^^ TSTypeOperator (operator = unique)
/// type R = readonly number[];
/// //       ^^^^^^^^^^^^^^^^^ TSTypeOperator (operator = readonly)
/// ```
pub const TSTypeOperator = struct {
    /// the prefix operator applied to the inner type
    operator: TSTypeOperatorKind,
    /// the type the operator is applied to
    type_annotation: NodeIndex,
};

// ts: grouping

/// A parenthesized type. Wraps any type in parentheses for grouping and
/// precedence control, letting the inner type bind looser than any
/// surrounding postfix operators.
///
/// The span includes the opening and closing parentheses.
///
/// ## Example
/// ```ts
/// type A = (string | number)[];
/// //       ^^^^^^^^^^^^^^^^^ TSParenthesizedType
/// type B = (() => void) | null;
/// //       ^^^^^^^^^^^^ TSParenthesizedType
/// ```
pub const TSParenthesizedType = struct {
    /// the inner type wrapped by the parentheses
    type_annotation: NodeIndex,
};

// ts: function and constructor types

/// A TypeScript function type. Describes a callable signature in type
/// position, written with `=>` between the parameter list and the return
/// type. Function types may carry generic parameters: `<T>(x: T) => T`.
///
/// ## Example
/// ```ts
/// type F = (x: number) => string;
/// //       ^^^^^^^^^^^^^^^^^^^^^ TSFunctionType
/// type G = <T>(x: T) => T;
/// //       ^^^^^^^^^^^^^^ TSFunctionType with type_parameters
/// ```
pub const TSFunctionType = struct {
    /// the generic `<...>` declaration, or `.null` when absent
    type_parameters: NodeIndex = .null,
    /// the parameter list as a `FormalParameters` node
    params: NodeIndex,
    /// a `TSTypeAnnotation` wrapping the return type, with its span starting
    /// at the `=>` token
    return_type: NodeIndex,
};

/// A TypeScript constructor type. Describes a signature that is invoked
/// with `new`, optionally marked `abstract` to forbid direct instantiation
/// of the referenced class.
///
/// ## Example
/// ```ts
/// type C = new (x: number) => Foo;
/// //       ^^^^^^^^^^^^^^^^^^^^^^^ TSConstructorType
/// type A = abstract new <T>(x: T) => T;
/// //       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ TSConstructorType (abstract = true)
/// ```
pub const TSConstructorType = struct {
    /// the generic `<...>` declaration, or `.null` when absent
    type_parameters: NodeIndex = .null,
    /// the parameter list as a `FormalParameters` node
    params: NodeIndex,
    /// a `TSTypeAnnotation` wrapping the return type, with its span starting
    /// at the `=>` token
    return_type: NodeIndex,
    /// true when the constructor is preceded by `abstract`
    abstract: bool = false,
};

// ts: module-level

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
    ts_type_annotation: TSTypeAnnotation,
    ts_any_keyword: TSAnyKeyword,
    ts_unknown_keyword: TSUnknownKeyword,
    ts_never_keyword: TSNeverKeyword,
    ts_void_keyword: TSVoidKeyword,
    ts_null_keyword: TSNullKeyword,
    ts_undefined_keyword: TSUndefinedKeyword,
    ts_string_keyword: TSStringKeyword,
    ts_number_keyword: TSNumberKeyword,
    ts_bigint_keyword: TSBigIntKeyword,
    ts_boolean_keyword: TSBooleanKeyword,
    ts_symbol_keyword: TSSymbolKeyword,
    ts_object_keyword: TSObjectKeyword,
    ts_intrinsic_keyword: TSIntrinsicKeyword,
    ts_this_type: TSThisType,
    ts_type_reference: TSTypeReference,
    ts_qualified_name: TSQualifiedName,
    ts_type_parameter: TSTypeParameter,
    ts_type_parameter_declaration: TSTypeParameterDeclaration,
    ts_type_parameter_instantiation: TSTypeParameterInstantiation,
    ts_literal_type: TSLiteralType,
    ts_template_literal_type: TSTemplateLiteralType,
    ts_array_type: TSArrayType,
    ts_indexed_access_type: TSIndexedAccessType,
    ts_tuple_type: TSTupleType,
    ts_named_tuple_member: TSNamedTupleMember,
    ts_optional_type: TSOptionalType,
    ts_rest_type: TSRestType,
    ts_union_type: TSUnionType,
    ts_intersection_type: TSIntersectionType,
    ts_conditional_type: TSConditionalType,
    ts_infer_type: TSInferType,
    ts_type_operator: TSTypeOperator,
    ts_parenthesized_type: TSParenthesizedType,
    ts_function_type: TSFunctionType,
    ts_constructor_type: TSConstructorType,
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
