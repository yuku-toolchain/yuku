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

/// An error, warning, hint, or info produced during parsing or semantic
/// analysis.
pub const Diagnostic = struct {
    severity: Severity = .@"error",
    message: []const u8,
    span: Span,
    help: ?[]const u8 = null,
    labels: []const Label = &.{},
};

/// Source type of a JavaScript or TypeScript file.
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
    /// `.mjs` and `.mts` are treated as modules, `.cjs` and `.cts` as
    /// scripts. All other files default to module.
    pub fn fromPath(path: []const u8) SourceType {
        if (std.mem.endsWith(u8, path, ".cjs") or std.mem.endsWith(u8, path, ".cts")) {
            return .script;
        }

        return .module;
    }
};

/// Language variant for JavaScript or TypeScript files. Determines which
/// syntax features are enabled during parsing.
pub const Lang = enum {
    js,
    ts,
    jsx,
    tsx,
    dts,

    /// Determines the language variant based on the file extension.
    ///
    /// `.d.ts`, `.d.mts`, `.d.cts` resolve to `dts`. `.tsx` resolves to
    /// `tsx`. `.ts`, `.mts`, `.cts` resolve to `ts`. `.jsx` resolves to
    /// `jsx`. Anything else (including `.js`, `.mjs`, `.cjs`) resolves to
    /// `js`.
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
/// // line comment
/// /* block comment */
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
    /// Index of the root node (always a `program` node).
    root: NodeIndex = undefined,
    /// All nodes in the AST.
    nodes: NodeList = .empty,
    /// Extra data storage for variadic node children. Resolved through
    /// `tree.extra(range)`.
    extras: std.ArrayList(NodeIndex) = .empty,
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
    pub fn addDiagnostic(self: *Tree, diag: Diagnostic) error{OutOfMemory}!void {
        try self.diagnostics.append(self.arena.allocator(), diag);
    }

    /// Returns the data for the node at the given index.
    pub inline fn data(self: *const Tree, index: NodeIndex) NodeData {
        return self.nodes.items(.data)[@intFromEnum(index)];
    }

    /// Returns the span for the node at the given index.
    pub inline fn span(self: *const Tree, index: NodeIndex) Span {
        return self.nodes.items(.span)[@intFromEnum(index)];
    }

    /// Returns the extra node indices for the given range.
    pub inline fn extra(self: *const Tree, range: IndexRange) []const NodeIndex {
        return self.extras.items[range.start..][0..range.len];
    }

    /// Replaces an existing node's data in-place.
    pub inline fn setData(self: *Tree, index: NodeIndex, new_data: NodeData) void {
        self.nodes.items(.data)[@intFromEnum(index)] = new_data;
    }

    /// Replaces an existing node's span in-place.
    pub inline fn setSpan(self: *Tree, index: NodeIndex, new_span: Span) void {
        self.nodes.items(.span)[@intFromEnum(index)] = new_span;
    }

    /// Creates a new node. Returns its index.
    pub inline fn addNode(self: *Tree, node_data: NodeData, node_span: Span) error{OutOfMemory}!NodeIndex {
        const index: NodeIndex = @enumFromInt(@as(u32, @intCast(self.nodes.len)));
        if (self.nodes.len < self.nodes.capacity) {
            self.nodes.appendAssumeCapacity(.{ .data = node_data, .span = node_span });
        } else {
            try self.nodes.append(self.arena.allocator(), .{ .data = node_data, .span = node_span });
        }
        return index;
    }

    /// Creates a new child list. Returns its range.
    pub inline fn addExtra(self: *Tree, children: []const NodeIndex) error{OutOfMemory}!IndexRange {
        const start: u32 = @intCast(self.extras.items.len);
        if (self.extras.items.len + children.len <= self.extras.capacity) {
            self.extras.appendSliceAssumeCapacity(children);
        } else {
            try self.extras.appendSlice(self.arena.allocator(), children);
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
    pub inline fn string(self: *const Tree, id: String) []const u8 {
        return self.strings.get(id);
    }
};

/// Index into the AST node array.
///
/// See [AST reference](https://yuku.fyi/parser/ast).
pub const NodeIndex = enum(u32) { null = std.math.maxInt(u32), _ };

/// Range of indices into the extra array for storing variadic node lists.
pub const IndexRange = struct {
    start: u32,
    len: u32,

    pub const empty: IndexRange = .{ .start = 0, .len = 0 };
};

// Node definitions follow. See `NodeIndex` for the field-annotation
// conventions used in their doc comments.

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
///
/// ## Example
/// ```js
/// debugger;
/// ```
pub const DebuggerStatement = struct {};

/// A standalone `;` used as a statement.
///
/// ## Example
/// ```js
/// ;
/// ```
pub const EmptyStatement = struct {};

/// A decorator applied to a class or class member.
///
/// See: [TC39 Decorators Proposal](https://github.com/tc39/proposal-decorators)
///
/// ## Example
/// ```js
/// class C { @foo bar() {} }
/// //         ^^^ expression
/// ```
pub const Decorator = struct {
    /// any expression
    expression: NodeIndex,
};

/// Class form.
///
/// `class_declaration` for `class Foo {}`. `class_expression` for
/// `const x = class {}`.
pub const ClassType = enum {
    class_declaration,
    class_expression,
};

/// A class declaration or expression.
///
/// See: <https://tc39.es/ecma262/#prod-ClassDeclaration>
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
///
/// `@dec class` populates `decorators`. `abstract class` sets `abstract`.
/// `declare class` sets `declare`.
pub const Class = struct {
    type: ClassType,
    /// `decorator[]`
    decorators: IndexRange,
    /// `binding_identifier`. `.null` for anonymous class expressions.
    id: NodeIndex,
    /// any expression. `.null` when the class has no `extends` clause.
    super_class: NodeIndex,
    /// `class_body`
    body: NodeIndex,
    /// `ts_type_parameter_declaration`. `.null` when the class has no `<T, U>`
    /// parameters.
    type_parameters: NodeIndex = .null,
    /// `ts_type_parameter_instantiation`. `.null` when `extends` has no `<T>`
    /// arguments.
    super_type_arguments: NodeIndex = .null,
    /// `ts_class_implements[]`. Empty when the class has no `implements`
    /// clause.
    implements: IndexRange = .empty,
    /// true for `abstract class`.
    abstract: bool = false,
    /// true for `declare class`.
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
/// ```
pub const ClassBody = struct {
    /// `method_definition`, `property_definition`, `static_block`, or `ts_index_signature`
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
/// `.none` means no modifier was written. This is distinct from `public`,
/// which is written explicitly.
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
/// //                            ^^^^^^^^^^^ value
/// ```
pub const MethodDefinition = struct {
    /// `decorator[]`
    decorators: IndexRange,
    /// `identifier_name`, `string_literal`, `numeric_literal`, `private_identifier` (class members only), or any expression when `computed = true`
    key: NodeIndex,
    /// `function`
    value: NodeIndex,
    kind: MethodDefinitionKind,
    /// true when the key is written inside `[...]`.
    computed: bool,
    /// true for the `static` modifier.
    static: bool,
    /// true for the `override` modifier.
    override: bool = false,
    /// true for an optional method (`foo?()`).
    optional: bool = false,
    /// true for the `abstract` modifier.
    abstract: bool = false,
    /// `.none` when no accessibility modifier was written.
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
pub const PropertyDefinition = struct {
    /// `decorator[]`
    decorators: IndexRange,
    /// `identifier_name`, `string_literal`, `numeric_literal`, `private_identifier` (class members only), or any expression when `computed = true`
    key: NodeIndex,
    /// any expression. `.null` when the field has no initializer.
    value: NodeIndex,
    /// true when the key is written inside `[...]`.
    computed: bool,
    /// true for the `static` modifier.
    static: bool,
    /// true for `accessor x;` auto-accessor fields.
    accessor: bool,
    /// `ts_type_annotation`. `.null` when the field has no annotation.
    type_annotation: NodeIndex = .null,
    /// true for the `declare` modifier.
    declare: bool = false,
    /// true for the `override` modifier.
    override: bool = false,
    /// true for an optional property (`foo?: T`).
    optional: bool = false,
    /// true for a definite assignment assertion (`foo!: T`).
    definite: bool = false,
    /// true for the `readonly` modifier.
    readonly: bool = false,
    /// true for the `abstract` modifier.
    abstract: bool = false,
    /// `.none` when no accessibility modifier was written.
    accessibility: Accessibility = .none,
};

/// A `static { ... }` block inside a class body.
///
/// See: [MDN Static initialization blocks](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes/Static_initialization_blocks)
///
/// ## Example
/// ```js
/// class C { static { init(); } }
/// //                 ^^^^^^^ body
/// ```
pub const StaticBlock = struct {
    /// any statement
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
/// a + b
/// // ^ left
/// //   ^ operator
/// //     ^ right
/// ```
pub const BinaryExpression = struct {
    /// any expression
    left: NodeIndex,
    /// any expression
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
/// a && b
/// // ^ left
/// //   ^^ operator
/// //      ^ right
/// ```
pub const LogicalExpression = struct {
    /// any expression
    left: NodeIndex,
    /// any expression
    right: NodeIndex,
    operator: LogicalOperator,
};

/// Ternary expression.
///
/// ## Example
/// ```js
/// a ? b : c
/// // ^ test
/// //   ^ consequent
/// //       ^ alternate
/// ```
pub const ConditionalExpression = struct {
    /// any expression
    @"test": NodeIndex,
    /// any expression
    consequent: NodeIndex,
    /// any expression
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
/// // ^^^^^^ operator
/// //     ^^^ argument
/// ```
pub const UnaryExpression = struct {
    /// any expression
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
/// ++x
/// x--
/// ```
///
/// `prefix` is true for `++x` and false for `x++`.
pub const UpdateExpression = struct {
    /// any assignment target. In practice an `identifier_reference` or
    /// `member_expression`.
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
/// // ^ left
/// //  ^^ operator
/// //     ^ right
/// ```
pub const AssignmentExpression = struct {
    /// any assignment target
    left: NodeIndex,
    /// any expression
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
/// See: <https://tc39.es/ecma262/#prod-VariableStatement>,
/// [Explicit Resource Management](https://github.com/tc39/proposal-explicit-resource-management)
///
/// ## Example
/// ```ts
/// const x = 1, y = 2;
/// //    ^^^^^^^^^^^^ declarators
/// ```
pub const VariableDeclaration = struct {
    kind: VariableKind,
    /// `variable_declarator[]`
    declarators: IndexRange,
    /// true for `declare var x: T`.
    declare: bool = false,
};

/// A single binding in a variable declaration.
///
/// ## Example
/// ```ts
/// let x!: number = 1;
/// // ^ id
/// //  ^ definite
/// //   ^^^^^^^^ type_annotation lives on `id`
/// //              ^ init
/// ```
pub const VariableDeclarator = struct {
    /// any binding pattern
    id: NodeIndex,
    /// any expression. `.null` when there is no initializer.
    init: NodeIndex,
    /// true for a definite assignment assertion (`let x!: T`).
    definite: bool = false,
};

/// A statement consisting of a single expression.
///
/// ## Example
/// ```js
/// foo();
/// // ^^^^ expression
/// ```
pub const ExpressionStatement = struct {
    /// any expression
    expression: NodeIndex,
};

/// An `if` / `else` statement.
///
/// ## Example
/// ```js
/// if (cond) thenStmt; else elseStmt;
/// //  ^^^^ test
/// //        ^^^^^^^^ consequent
/// //                       ^^^^^^^^ alternate
/// ```
pub const IfStatement = struct {
    /// any expression
    @"test": NodeIndex,
    /// any statement
    consequent: NodeIndex,
    /// any statement. `.null` when there is no `else` clause.
    alternate: NodeIndex,
};

/// A `switch` statement.
///
/// ## Example
/// ```js
/// switch (x) { case 1: stmt; }
/// //      ^ discriminant
/// //           ^^^^^^^^^^^^^ cases[0]
/// ```
pub const SwitchStatement = struct {
    /// any expression
    discriminant: NodeIndex,
    /// `switch_case[]`
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
    /// `variable_declaration` or any expression. `.null` when omitted.
    init: NodeIndex,
    /// any expression. `.null` when omitted.
    @"test": NodeIndex,
    /// any expression. `.null` when omitted.
    update: NodeIndex,
    /// any statement
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
    /// `variable_declaration` or any assignment target
    left: NodeIndex,
    /// any expression
    right: NodeIndex,
    /// any statement
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
    /// `variable_declaration` or any assignment target
    left: NodeIndex,
    /// any expression
    right: NodeIndex,
    /// any statement
    body: NodeIndex,
    /// true for `for await (...)`.
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
    /// `label_identifier`. `.null` for bare `break`.
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
    /// `label_identifier`. `.null` for bare `continue`.
    label: NodeIndex,
};

/// A labeled statement.
///
/// ## Example
/// ```js
/// outer: for (;;) break outer;
/// // ^^^ label
/// //     ^^^^^^^^^^^^^^^^^^^^^ body
/// ```
pub const LabeledStatement = struct {
    /// `label_identifier`
    label: NodeIndex,
    /// any statement
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
pub const SwitchCase = struct {
    /// any expression. `.null` for the `default` clause.
    @"test": NodeIndex,
    /// any statement
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
    /// any expression. `.null` for bare `return`.
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
    /// any expression
    argument: NodeIndex,
};

/// A `try` / `catch` / `finally` statement.
///
/// ## Example
/// ```js
/// try { a } catch (e) { b } finally { c }
/// //  ^^^^^ block
/// //        ^^^^^^^^^^^^^^^ handler
/// //                                ^^^^^ finalizer
/// ```
pub const TryStatement = struct {
    /// `block_statement`
    block: NodeIndex,
    /// `catch_clause`. `.null` when no `catch` clause is present.
    handler: NodeIndex,
    /// `block_statement`. `.null` when no `finally` clause is present.
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
pub const CatchClause = struct {
    /// any binding pattern. `.null` for `catch { }` with no binding.
    param: NodeIndex,
    /// `block_statement`
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
    /// any expression
    @"test": NodeIndex,
    /// any statement
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
    /// any statement
    body: NodeIndex,
    /// any expression
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
    /// any expression
    object: NodeIndex,
    /// any statement
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
/// 1_000_000   // separators are stripped when computing `value()`
/// ```
pub const NumericLiteral = struct {
    kind: Kind,
    /// the raw lexeme, including prefix (`0x`, `0b`, `0o`) and separators
    raw: String = .empty,

    /// Computes the IEEE 754 double value.
    pub fn value(self: NumericLiteral, tree: *const Tree) f64 {
        const raw = tree.string(self.raw);
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

/// A BigInt literal (a numeric literal with a trailing `n`).
///
/// See: [MDN BigInt](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt)
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
/// See: <https://tc39.es/ecma262/#prod-RegularExpressionLiteral>
///
/// ## Example
/// ```js
/// /foo/gi
/// // ^^^ pattern
/// //     ^^ flags
/// ```
pub const RegExpLiteral = struct {
    pattern: String = .empty,
    flags: String = .empty,
};

/// A template literal with zero or more interpolations.
///
/// See: <https://tc39.es/ecma262/#prod-TemplateLiteral>
///
/// ## Example
/// ```js
/// `hello ${name}!`
/// ```
///
/// `quasis` are the static text spans (`"hello "` and `"!"`), always
/// `expressions.len + 1` elements. `expressions` are the interpolations
/// (here, `name`).
pub const TemplateLiteral = struct {
    /// `template_element[]`. Always `expressions.len + 1` elements.
    quasis: IndexRange,
    /// any expression
    expressions: IndexRange,
};

/// A single quasi span inside a template literal.
///
/// ## Example
/// ```js
/// `a ${x} b`
/// ```
///
/// The two quasi elements are `"a "` (`tail = false`) and `" b"`
/// (`tail = true`).
pub const TemplateElement = struct {
    /// escape-decoded content, empty when `is_cooked_undefined`
    cooked: String = .empty,
    /// true for the final element (after the last interpolation)
    tail: bool,
    /// true when the cooked value is undefined per ECMAScript TV semantics
    /// (an invalid escape inside a tagged template)
    is_cooked_undefined: bool = false,
};

/// An identifier used as an expression.
///
/// See: [ECMAScript Identifiers](https://tc39.es/ecma262/#sec-identifiers)
///
/// ## Example
/// ```js
/// console.log(x);
/// ```
///
/// `console` and `x` are both `identifier_reference`s. `log` is an
/// `identifier_name` because it appears in property-access position, not
/// as a value reference.
pub const IdentifierReference = struct {
    name: String = .empty,
};

/// A `#privateName` identifier used inside a class.
///
/// The stored `name` does not include the leading `#`.
///
/// See: [MDN Private class fields](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes/Private_properties)
///
/// ## Example
/// ```js
/// class C { #secret = 1; }
/// //         ^^^^^^ name = "secret"
/// ```
pub const PrivateIdentifier = struct {
    name: String = .empty,
};

/// An identifier that introduces a new binding.
///
/// ## Example
/// ```ts
/// function foo(x?: number) {}
/// //       ^^^ id
/// //           ^ optional, with type_annotation
/// ```
pub const BindingIdentifier = struct {
    name: String = .empty,
    /// `decorator[]`. Decorators on a parameter binding (legacy
    /// `experimentalDecorators`). Empty for non-parameter bindings.
    decorators: IndexRange = .empty,
    /// `ts_type_annotation`. `.null` when absent.
    type_annotation: NodeIndex = .null,
    /// true when marked optional (`x?: T`).
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

/// An identifier used as a statement label or as the target of `break` or
/// `continue`.
///
/// ## Example
/// ```js
/// outer: for (;;) break outer;
/// // ^^^ label
/// //                    ^^^^^ break target
/// ```
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
    /// any binding pattern
    left: NodeIndex,
    /// any expression
    right: NodeIndex,
    /// `decorator[]`. Decorators on a parameter binding (legacy
    /// `experimentalDecorators`).
    decorators: IndexRange = .empty,
    /// `ts_type_annotation`. `.null` when absent.
    type_annotation: NodeIndex = .null,
    /// true when marked optional in a parameter position.
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
    /// any binding pattern
    argument: NodeIndex,
    /// `decorator[]`. Decorators on a parameter rest element (legacy
    /// `experimentalDecorators`).
    decorators: IndexRange = .empty,
    /// `ts_type_annotation`. `.null` when absent.
    type_annotation: NodeIndex = .null,
    /// true when marked optional in a parameter position.
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
///
/// `elements[1]` is `.null` (the hole between the two commas).
pub const ArrayPattern = struct {
    /// any binding pattern. Entries are `.null` for holes.
    elements: IndexRange,
    /// `binding_rest_element`. `.null` when no rest element is present.
    rest: NodeIndex,
    /// `decorator[]`. Decorators on a parameter binding (legacy
    /// `experimentalDecorators`).
    decorators: IndexRange = .empty,
    /// `ts_type_annotation`. `.null` when absent.
    type_annotation: NodeIndex = .null,
    /// true when marked optional in a parameter position.
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
    /// `binding_property[]`
    properties: IndexRange,
    /// `binding_rest_element`. `.null` when no rest element is present.
    rest: NodeIndex,
    /// `decorator[]`. Decorators on a parameter binding (legacy
    /// `experimentalDecorators`).
    decorators: IndexRange = .empty,
    /// `ts_type_annotation`. `.null` when absent.
    type_annotation: NodeIndex = .null,
    /// true when marked optional in a parameter position.
    optional: bool = false,
};

/// A single property inside an `object_pattern`.
///
/// ## Example
/// ```js
/// const { a, b: c, [k]: d } = obj;
/// //      ^ shorthand
/// //         ^^^^ key colon value
/// //               ^^^^^^ computed key
/// ```
pub const BindingProperty = struct {
    /// `identifier_name`, `string_literal`, `numeric_literal`, `private_identifier` (class members only), or any expression when `computed = true`
    key: NodeIndex,
    /// any binding pattern
    value: NodeIndex,
    /// true when written as the shorthand `{ a }` form.
    shorthand: bool,
    /// true when the key is written inside `[...]`.
    computed: bool,
};

/// An array literal expression.
///
/// ## Example
/// ```js
/// [a, , b, ...c]
/// // ^ elements[0]
/// //      ^ elements[2]
/// //         ^^^^ elements[3] (SpreadElement)
/// ```
///
/// `elements[1]` is `.null` (the hole between the two commas).
pub const ArrayExpression = struct {
    /// any expression or `spread_element`. Entries are `.null` for holes.
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
    /// `object_property` or `spread_element`.
    properties: IndexRange,
};

/// A `...argument` element inside an array or object literal, or in a call
/// argument list.
///
/// ## Example
/// ```js
/// foo(...args)
/// //  ^^^^^^^ SpreadElement
/// //     ^^^^ argument
/// ```
pub const SpreadElement = struct {
    /// any expression
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
    /// `identifier_name`, `string_literal`, `numeric_literal`, `private_identifier` (class members only), or any expression when `computed = true`
    key: NodeIndex,
    /// any expression for `init` properties. `function` for methods, getters,
    /// and setters.
    value: NodeIndex,
    kind: PropertyKind,
    /// true when written as the shorthand method form `b() {}`.
    method: bool,
    /// true when written as the shorthand `{ a }` form.
    shorthand: bool,
    /// true when the key is written inside `[...]`.
    computed: bool,
};

/// The top-level node of every parsed file.
///
/// See: <https://tc39.es/ecma262/#prod-Script>,
/// <https://tc39.es/ecma262/#prod-Module>
///
/// ## Example
/// ```js
/// #!/usr/bin/env node
/// "use strict";
/// import x from "y";
/// console.log(x);
/// ```
///
/// `hashbang` is the `#!` line if present. `body` contains directives
/// (`"use strict";`), imports, and statements.
pub const Program = struct {
    source_type: SourceType,
    /// any statement or `directive`. Import and export declarations are
    /// classified as statements.
    body: IndexRange,
    /// `null` when the file has no `#!` line.
    hashbang: ?Hashbang = null,
};

/// A hashbang comment at the top of a source file.
///
/// ## Example
/// ```js
/// #!/usr/bin/env node
/// ```
///
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
///
/// `expression` is the underlying `string_literal` (`"use strict"`).
/// `value` is the text between the quotes (`use strict`).
pub const Directive = struct {
    /// `string_literal`
    expression: NodeIndex,
    /// the directive text without surrounding quotes
    value: String = .empty,
};

/// Form of a function node.
///
/// `function_declaration` for `function foo() {}`.
/// `function_expression` for `const x = function () {}`.
/// `ts_declare_function` for body-less function declarations. Covers
/// ambient `declare function` and plain overload signatures.
/// `ts_empty_body_function_expression` for body-less function expressions.
/// Used as the `value` of body-less class methods (overloads, abstract
/// methods, ambient methods).
pub const FunctionType = enum {
    function_declaration,
    function_expression,
    ts_declare_function,
    ts_empty_body_function_expression,
};

/// A function declaration or expression.
///
/// See: <https://tc39.es/ecma262/#prod-FunctionDeclaration>
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
pub const Function = struct {
    type: FunctionType,
    /// `binding_identifier`. `.null` for anonymous functions.
    id: NodeIndex,
    generator: bool,
    async: bool,
    /// true when preceded by the `declare` modifier. Distinguishes a real
    /// ambient declaration from a plain overload signature, which share the
    /// `.ts_declare_function` shape.
    declare: bool = false,
    /// `formal_parameters`
    params: NodeIndex,
    /// `function_body`. `.null` for body-less declarations and signatures.
    body: NodeIndex,
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// `ts_type_annotation`. `.null` when absent.
    return_type: NodeIndex = .null,
};

/// The body of a function.
///
/// See: <https://tc39.es/ecma262/#prod-FunctionBody>
pub const FunctionBody = struct {
    /// any statement or `directive`.
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
    /// any statement
    body: IndexRange,
};

/// Which grammar production the parameter list came from.
///
/// Constrains which binding forms are legal and whether duplicates are
/// allowed in strict mode.
///
/// `formal_parameters` is a plain `function` parameter list.
/// `unique_formal_parameters` covers parameters of a generator, async,
/// arrow, method, or setter. `arrow_formal_parameters` covers an arrow
/// function parameter list. `signature` covers parameters of a TypeScript
/// type signature.
pub const FormalParameterKind = enum {
    formal_parameters,
    unique_formal_parameters,
    arrow_formal_parameters,
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
    /// `formal_parameter[]`. May also include `ts_parameter_property` entries
    /// (constructors only) and a leading `ts_this_parameter` (TypeScript).
    items: IndexRange,
    /// `binding_rest_element`. `.null` when no rest element is present.
    rest: NodeIndex,
    kind: FormalParameterKind,
};

/// A thin wrapper marking a binding pattern as a function parameter.
///
/// TypeScript metadata (decorators, type annotation, optional) lives on
/// the inner pattern.
pub const FormalParameter = struct {
    /// any binding pattern or `ts_this_parameter`.
    pattern: NodeIndex,
};

/// An expression wrapped in parentheses.
///
/// See: <https://tc39.es/ecma262/#prod-ParenthesizedExpression>
///
/// ## Example
/// ```js
/// x + (a + b)
/// //   ^^^^^ expression
/// ```
pub const ParenthesizedExpression = struct {
    /// any expression
    expression: NodeIndex,
};

/// An arrow function expression.
///
/// See: <https://tc39.es/ecma262/#prod-ArrowFunction>
///
/// ## Example
/// ```ts
/// async <T>(x: T): T => x
/// //    ^^^ type_parameters
/// //       ^^^^^^ params
/// //             ^^^ return_type
/// //                    ^ body (expression = true)
/// ```
pub const ArrowFunctionExpression = struct {
    /// true for a concise body `() => expr`. false for a block body
    /// `() => { ... }`.
    expression: bool,
    async: bool,
    /// `formal_parameters`
    params: NodeIndex,
    /// `function_body` when `expression` is false. any expression otherwise.
    body: NodeIndex,
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// `ts_type_annotation`. `.null` when absent.
    return_type: NodeIndex = .null,
};

/// A comma-separated sequence of expressions.
///
/// ## Example
/// ```js
/// (a, b, c)
/// // ^^^^^^^ expressions
/// ```
pub const SequenceExpression = struct {
    /// any expression
    expressions: IndexRange,
};

/// Property access, in static, computed, or optional form.
///
/// See: <https://tc39.es/ecma262/#prod-MemberExpression>
///
/// ## Example
/// ```js
/// obj.foo          // computed = false, optional = false
/// obj[expr]        // computed = true
/// obj?.foo         // optional = true
/// obj.#priv        // property is a PrivateIdentifier
/// ```
pub const MemberExpression = struct {
    /// any expression
    object: NodeIndex,
    /// any expression when `computed`. `identifier_name` or `private_identifier`
    /// otherwise.
    property: NodeIndex,
    computed: bool,
    /// true for `obj?.foo`.
    optional: bool,
};

/// A function call.
///
/// See: <https://tc39.es/ecma262/#prod-CallExpression>
///
/// ## Example
/// ```ts
/// foo<T>(a, ...b)
/// // ^^^ type_arguments
/// //     ^^^^^^^ arguments
/// ```
///
/// `optional` is true for `foo?.()`.
pub const CallExpression = struct {
    /// any expression
    callee: NodeIndex,
    /// any expression or `spread_element`.
    arguments: IndexRange,
    /// true for `foo?.()`.
    optional: bool,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
};

/// Wraps an optional chain so that short-circuiting applies to the whole
/// chain.
///
/// See: [MDN Optional chaining](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Optional_chaining)
///
/// ## Example
/// ```js
/// foo?.bar.baz
/// // ^^^^^^^^^^ expression
/// ```
pub const ChainExpression = struct {
    /// `call_expression` or `member_expression` containing an optional link
    /// somewhere in the chain.
    expression: NodeIndex,
};

/// A tagged template expression.
///
/// ## Example
/// ```ts
/// obj.tag<T>`hello`
/// //  ^^^ tag
/// //     ^^^ type_arguments
/// //        ^^^^^^^ quasi
/// ```
pub const TaggedTemplateExpression = struct {
    /// any expression
    tag: NodeIndex,
    /// `template_literal`
    quasi: NodeIndex,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
};

/// A `new` expression.
///
/// See: <https://tc39.es/ecma262/#prod-NewExpression>
///
/// ## Example
/// ```ts
/// new Foo<T>(a, b)
/// //  ^^^ callee
/// //     ^^^ type_arguments
/// //         ^^^^ arguments
/// ```
pub const NewExpression = struct {
    /// any expression
    callee: NodeIndex,
    /// any expression or `spread_element`.
    arguments: IndexRange,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
};

/// An `await` expression.
///
/// See: <https://tc39.es/ecma262/#prod-AwaitExpression>
///
/// ## Example
/// ```js
/// await promise
/// //    ^^^^^^^ argument
/// ```
pub const AwaitExpression = struct {
    /// any expression
    argument: NodeIndex,
};

/// A `yield` or `yield*` expression.
///
/// See: <https://tc39.es/ecma262/#prod-YieldExpression>
///
/// ## Example
/// ```js
/// yield x      // delegate = false
/// yield* iter  // delegate = true
/// yield        // argument = .null
/// ```
pub const YieldExpression = struct {
    /// any expression. `.null` for bare `yield`.
    argument: NodeIndex,
    /// true for `yield*`.
    delegate: bool,
};

/// A meta property.
///
/// See: <https://tc39.es/ecma262/#prod-MetaProperty>
///
/// ## Example
/// ```js
/// import.meta
/// //     ^^^^ property
/// new.target
/// //  ^^^^^^ property
/// ```
///
/// `meta` is the head keyword (`import` or `new`).
pub const MetaProperty = struct {
    /// `identifier_name`
    meta: NodeIndex,
    /// `identifier_name`
    property: NodeIndex,
};

/// `value` versus `type` on a TypeScript import or export specifier.
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
/// See: [Source phase imports](https://github.com/tc39/proposal-source-phase-imports),
/// [Deferred import evaluation](https://github.com/tc39/proposal-defer-import-eval)
///
/// ## Example
/// ```js
/// import("m")                 // phase = null
/// import("m", { with: {} })   // options set
/// import.source("m")          // phase = .source
/// import.defer("m")           // phase = .defer
/// ```
pub const ImportExpression = struct {
    /// any expression. The module specifier expression.
    source: NodeIndex,
    /// any expression. `.null` when no options argument was passed.
    options: NodeIndex,
    /// `null` for a plain `import(...)`.
    phase: ?ImportPhase,
};

/// A static `import` declaration.
///
/// See: <https://tc39.es/ecma262/#prod-ImportDeclaration>
///
/// ## Example
/// ```ts
/// import { foo as bar } from "m";
/// //       ^^^^^^^^^^ specifiers[0]
/// //                         ^^^ source
/// ```
///
/// `import_kind = .type` for `import type { ... }`. `phase` is set for the
/// `import source` and `import defer` forms. `attributes` holds a trailing
/// `with { ... }` clause.
pub const ImportDeclaration = struct {
    /// `import_specifier`, `import_default_specifier`, or
    /// `import_namespace_specifier`. Empty for side-effect-only imports.
    specifiers: IndexRange,
    /// `string_literal`
    source: NodeIndex,
    /// `import_attribute[]`
    attributes: IndexRange,
    /// `null` for a regular import.
    phase: ?ImportPhase,
    /// `.type` for `import type { ... }`.
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
    /// `identifier_name` or `string_literal`.
    imported: NodeIndex,
    /// `binding_identifier`
    local: NodeIndex,
    /// `.type` for `import { type X }`.
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
    /// `binding_identifier`
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
    /// `binding_identifier`
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
    /// `identifier_name` or `string_literal`.
    key: NodeIndex,
    /// `string_literal`
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
///
/// For `export const x = 1`, `declaration` is the inner
/// `variable_declaration` and `specifiers` is empty.
pub const ExportNamedDeclaration = struct {
    /// `variable_declaration`, `function`, `class`, `ts_type_alias_declaration`,
    /// `ts_interface_declaration`, `ts_enum_declaration`, or
    /// `ts_module_declaration`. `.null` for the `export { ... }` form.
    declaration: NodeIndex,
    /// `export_specifier[]`
    specifiers: IndexRange,
    /// `string_literal`. `.null` when there is no `from` clause.
    source: NodeIndex,
    /// `import_attribute[]`
    attributes: IndexRange,
    /// `.type` for `export type { ... }`.
    export_kind: ImportOrExportKind = .value,
};

/// An `export default ...` declaration.
///
/// ## Example
/// ```js
/// export default foo;
/// //             ^^^ declaration
/// export default function () {}
/// //             ^^^^^^^^^^^^^^ declaration
/// ```
pub const ExportDefaultDeclaration = struct {
    /// any expression, `function`, or `class`.
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
///
/// `export_kind = .type` for `export type * from "..."`.
pub const ExportAllDeclaration = struct {
    /// `identifier_name` or `string_literal`. `.null` for `export *` without
    /// `as`.
    exported: NodeIndex,
    /// `string_literal`
    source: NodeIndex,
    /// `import_attribute[]`
    attributes: IndexRange,
    /// `.type` for `export type * from "..."`.
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
    /// `identifier_reference`. `identifier_name` or `string_literal` when
    /// re-exporting from a module.
    local: NodeIndex,
    /// `identifier_name` or `string_literal`.
    exported: NodeIndex,
    /// `.type` for `export { type X }`.
    export_kind: ImportOrExportKind = .value,
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
    /// any ts type
    type_annotation: NodeIndex,
};

/// The `any` primitive type. Disables all type checking for the annotated
/// value.
///
/// See: [TypeScript Handbook any](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#any)
///
/// ## Example
/// ```ts
/// let x: any;
/// //     ^^^ TSAnyKeyword
/// ```
pub const TSAnyKeyword = struct {};

/// The `unknown` primitive type. The type-safe counterpart of `any`.
///
/// See: [TypeScript 3.0 Release Notes unknown](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-0.html#new-unknown-top-type)
///
/// ## Example
/// ```ts
/// let x: unknown;
/// //     ^^^^^^^ TSUnknownKeyword
/// ```
pub const TSUnknownKeyword = struct {};

/// The `never` primitive type. Represents values that never occur, for
/// example the return type of a function that always throws.
///
/// See: [TypeScript Handbook never](https://www.typescriptlang.org/docs/handbook/2/functions.html#never)
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

/// The `intrinsic` keyword. Marks a type as built into the TypeScript
/// compiler, for example `Uppercase<T>` and other string-manipulation
/// utilities.
///
/// ## Example
/// ```ts
/// type Uppercase<S extends string> = intrinsic;
/// //                                 ^^^^^^^^^ TSIntrinsicKeyword
/// ```
pub const TSIntrinsicKeyword = struct {};

/// The polymorphic `this` type. Refers to the type of the enclosing class
/// or interface at the usage site.
///
/// ## Example
/// ```ts
/// class C { self(): this { return this; } }
/// //                ^^^^ TSThisType
/// ```
pub const TSThisType = struct {};

/// A reference to a named type, optionally applied to type arguments.
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
    /// `identifier_reference`, `ts_qualified_name`, or `this_expression`
    type_name: NodeIndex,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
};

/// A dotted type name like `A.B.C`. Left associative, so `A.B.C` parses as
/// `(A.B).C` with the outer `ts_qualified_name` holding the inner one as
/// `left`.
///
/// ## Example
/// ```ts
/// let x: Tools.Pos;
/// //     ^^^^^^^^^ TSQualifiedName
/// //     ^^^^^ left
/// //           ^^^ right
/// ```
pub const TSQualifiedName = struct {
    /// `identifier_reference`, `identifier_name`, `ts_qualified_name`, or
    /// `this_expression`.
    left: NodeIndex,
    /// `identifier_name`
    right: NodeIndex,
};

/// The `typeof` type operator applied to a value reference. Extracts the
/// type of an existing binding or a dotted member path at the usage site.
///
/// ## Example
/// ```ts
/// let x: typeof console;
/// //     ^^^^^^^^^^^^^^ TSTypeQuery
/// let y: typeof console.log;
/// //     ^^^^^^^^^^^^^^^^^^ TSTypeQuery (expr_name is a TSQualifiedName)
/// let z: typeof Err<number>;
/// //     ^^^^^^^^^^^^^^^^^^ TSTypeQuery (with type_arguments)
/// let w: typeof import("foo").Bar;
/// //     ^^^^^^^^^^^^^^^^^^^^^^^^ TSTypeQuery (expr_name is a TSImportType)
/// ```
pub const TSTypeQuery = struct {
    /// `identifier_reference`, `ts_qualified_name`, or `ts_import_type`.
    expr_name: NodeIndex,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
};

/// A reference to a named type imported from a module path, written
/// `import("module").Foo<T>` in type position. The `import("...")` head
/// names the module. An optional dotted `qualifier` selects a specific type
/// from the module namespace, and an optional `<T, U>` instantiates a
/// generic type.
///
/// ## Example
/// ```ts
/// type A = import("./mod");
/// //       ^^^^^^^^^^^^^^^ TSImportType
/// type B = import("./mod").Foo;
/// //       ^^^^^^^^^^^^^^^^^^^ TSImportType (qualifier is an IdentifierName)
/// type C = import("./mod").Foo.Bar;
/// //       ^^^^^^^^^^^^^^^^^^^^^^^ TSImportType (qualifier is a TSQualifiedName)
/// type D = import("./mod").Foo<number>;
/// //       ^^^^^^^^^^^^^^^^^^^^^^^^^^^ TSImportType (with type_arguments)
/// type E = import("./mod", { with: { type: "json" } });
/// //                        ^^^^^^^^^^^^^^^^^^^^^^^^ options
/// ```
pub const TSImportType = struct {
    /// `string_literal` naming the imported module.
    source: NodeIndex,
    /// `object_expression` carrying the import attributes object. `.null`
    /// when absent.
    options: NodeIndex = .null,
    /// `identifier_name` for a single segment, or a left-associative
    /// `ts_qualified_name` chain whose leaves are all `identifier_name`s.
    /// `.null` when there is no qualifier.
    qualifier: NodeIndex = .null,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
};

/// A single `<T>` type parameter introduced by a generic declaration.
/// Carries the parameter name along with the optional `extends` constraint,
/// optional default type, and the three variance and invariance modifier
/// keywords.
///
/// The span starts at the first modifier keyword when present, otherwise at
/// the name, and ends at the default if present, the constraint if
/// present, or the name.
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
    /// `binding_identifier`
    name: NodeIndex,
    /// any ts type. `.null` when there is no `extends` constraint.
    constraint: NodeIndex = .null,
    /// any ts type. `.null` when there is no default.
    default: NodeIndex = .null,
    /// true when the `in` variance modifier was written.
    in: bool = false,
    /// true when the `out` variance modifier was written.
    out: bool = false,
    /// true when the `const` modifier was written.
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
    /// `ts_type_parameter[]`
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
    /// any ts type
    params: IndexRange,
};

/// A literal value used in type position. Wraps a string, numeric, bigint,
/// boolean, or no-substitution template literal directly, or a
/// `unary_expression` when the literal is preceded by a `-` or `+` sign (for
/// example `-1`).
///
/// Template literals that contain interpolations use
/// `ts_template_literal_type` instead. The `null` keyword uses `ts_null_keyword`.
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
    /// `string_literal`, `numeric_literal`, `bigint_literal`, `boolean_literal`,
    /// `template_literal`, or `unary_expression` wrapping one of the numeric
    /// kinds.
    literal: NodeIndex,
};

/// A template literal used in type position with one or more
/// interpolations.
///
/// Parallels the expression-level `template_literal` but with types filling
/// the interpolation slots instead of expressions. `quasis` holds the
/// static text spans as `template_element` nodes and always has exactly
/// `types.len + 1` elements. The final quasi is marked `tail = true`.
///
/// A template literal with no interpolations is still parsed as a
/// `ts_literal_type` wrapping a `template_literal`, not as a
/// `ts_template_literal_type`.
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
    /// `template_element[]`. Always `types.len + 1` elements.
    quasis: IndexRange,
    /// any ts type. One entry per `${...}` slot.
    types: IndexRange,
};

/// An array type. Applies the postfix `[]` suffix to an element type.
///
/// Stacks naturally. `T[][]` is a `ts_array_type` whose `element_type` is
/// another `ts_array_type`.
///
/// ## Example
/// ```ts
/// let xs: number[];
/// //      ^^^^^^^^ TSArrayType
/// //      ^^^^^^ element_type
/// let ys: string[][];
/// //      ^^^^^^^^^^ TSArrayType (element_type is another TSArrayType)
/// ```
pub const TSArrayType = struct {
    /// any ts type
    element_type: NodeIndex,
};

/// An indexed access type. Looks up the type of the property named by
/// `index_type` on `object_type`, mirroring expression-level member access
/// but in type position.
///
/// Stacks naturally. `T[K][L]` is a `ts_indexed_access_type` whose
/// `object_type` is another `ts_indexed_access_type`.
///
/// ## Example
/// ```ts
/// type A = Person["age"];
/// //       ^^^^^^^^^^^^^ TSIndexedAccessType
/// //       ^^^^^^ object_type
/// //              ^^^^^ index_type
/// type B = T[K];
/// //       ^^^^ TSIndexedAccessType
/// ```
pub const TSIndexedAccessType = struct {
    /// any ts type
    object_type: NodeIndex,
    /// any ts type
    index_type: NodeIndex,
};

/// A tuple type. A fixed length sequence of positional or named elements
/// whose types can differ from slot to slot. Elements may be marked
/// optional with `?` and the trailing element may be a rest element with
/// `...`.
///
/// Each entry in `element_types` is one of:
/// - a plain any ts type for a positional element
/// - a `ts_optional_type` for `Type?` in positional form
/// - a `ts_rest_type` for `...Type` in positional form, optionally wrapping a
///   `ts_named_tuple_member`
/// - a `ts_named_tuple_member` for `label: Type` or `label?: Type`
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
    /// any ts type, `ts_optional_type`, `ts_rest_type`, or `ts_named_tuple_member`.
    element_types: IndexRange,
};

/// A labeled element inside a tuple type. Carries a name that appears only
/// as documentation in TypeScript (it has no runtime effect) alongside the
/// element type. The whole member may be marked optional with a trailing
/// `?` on the label. The `?` modifies the tuple slot itself rather than
/// wrapping the inner type in a `ts_optional_type`.
///
/// When preceded by `...` the member is wrapped in a `ts_rest_type` whose
/// inner is this `ts_named_tuple_member`, matching TypeScript's spec shape for
/// named rest elements like `[...selectors: S]`.
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
    /// `identifier_name`
    label: NodeIndex,
    /// any ts type
    element_type: NodeIndex,
    /// true when the label is followed by `?`.
    optional: bool = false,
};

/// An optional element inside a tuple type. Parsed as a `Type?` suffix on
/// an unnamed tuple element. The `?` marks the slot as optional without
/// changing the element's underlying type.
///
/// Only valid as a direct tuple element. An optional marker on a named
/// tuple element (`label?: Type`) is captured on
/// `TSNamedTupleMember.optional` rather than producing a `ts_optional_type`.
///
/// ## Example
/// ```ts
/// type T = [number, string?];
/// //                ^^^^^^^ TSOptionalType
/// //                ^^^^^^ type_annotation
/// ```
pub const TSOptionalType = struct {
    /// any ts type
    type_annotation: NodeIndex,
};

/// A rest element inside a tuple type. Marks the trailing slot as
/// consuming zero or more elements whose type is the inner annotation.
///
/// The inner may be any any ts type (typically an array or tuple type) or a
/// `ts_named_tuple_member` for named rest elements like `[...rest: T[]]`.
///
/// ## Example
/// ```ts
/// type T = [number, ...string[]];
/// //                ^^^^^^^^^^^ TSRestType
/// //                   ^^^^^^^^ type_annotation
/// type U = [...rest: number[]];
/// //        ^^^^^^^^^^^^^^^^^ TSRestType wrapping TSNamedTupleMember
/// ```
pub const TSRestType = struct {
    /// any ts type or `ts_named_tuple_member`.
    type_annotation: NodeIndex,
};

/// A JSDoc-style nullable type marker, written as a prefix `?T` or a
/// postfix `T?`.
///
/// ## Example
/// ```ts
/// let a: ?string;
/// //     ^^^^^^^ TSJSDocNullableType (postfix = false)
/// let b: number?;
/// //     ^^^^^^^ TSJSDocNullableType (postfix = true)
/// ```
pub const TSJSDocNullableType = struct {
    /// any ts type
    type_annotation: NodeIndex,
    /// true when the `?` follows the type, false when it precedes it.
    postfix: bool = false,
};

/// A JSDoc-style non-nullable type marker. Written as a prefix `!T` or a
/// postfix `T!`.
///
/// ## Example
/// ```ts
/// let a: !string;
/// //     ^^^^^^^ TSJSDocNonNullableType (postfix = false)
/// let b: number!;
/// //     ^^^^^^^ TSJSDocNonNullableType (postfix = true)
/// ```
pub const TSJSDocNonNullableType = struct {
    /// any ts type
    type_annotation: NodeIndex,
    /// true when the `!` follows the type, false when it precedes it.
    postfix: bool = false,
};

/// A JSDoc-style unknown type written as a bare `?`, valid only in a type
/// argument slot (`Foo<?>`, `Foo<?, T>`).
///
/// ## Example
/// ```ts
/// const x = foo<?>;
/// //            ^ TSJSDocUnknownType
/// ```
pub const TSJSDocUnknownType = struct {};

/// A union type. Combines two or more types with `|`, representing a value
/// that is any one of the constituents.
///
/// Binds looser than `ts_intersection_type`, so `A & B | C` parses as
/// `(A & B) | C`. A leading `|` before the first operand is allowed and is
/// preserved in the span. `type A = | string` yields a single-operand
/// `ts_union_type` whose span starts at the leading `|` rather than at the
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
    /// any ts type
    types: IndexRange,
};

/// An intersection type. Combines two or more types with `&`, representing
/// a value that satisfies every constituent simultaneously.
///
/// ## Example
/// ```ts
/// type A = Named & Aged;
/// //       ^^^^^^^^^^^^ TSIntersectionType
/// type B = { name: string } & { age: number } & Serializable;
/// ```
pub const TSIntersectionType = struct {
    /// any ts type
    types: IndexRange,
};

/// A conditional type. Selects between two branches based on whether
/// `check_type` is assignable to `extends_type`.
///
/// See: [TypeScript Handbook Conditional Types](https://www.typescriptlang.org/docs/handbook/2/conditional-types.html)
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
    /// any ts type
    check_type: NodeIndex,
    /// any ts type
    extends_type: NodeIndex,
    /// any ts type
    true_type: NodeIndex,
    /// any ts type
    false_type: NodeIndex,
};

/// An `infer` type placeholder. Introduces a new type variable that
/// captures a position inside the extends branch of a conditional type,
/// made available in the true branch of that conditional.
///
/// The span starts at the `infer` keyword and ends at the name or the
/// constraint when present.
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
    /// `ts_type_parameter`
    type_parameter: NodeIndex,
};

/// The prefix operator used by `ts_type_operator`.
///
/// `keyof` produces the union of property keys of the operand. `unique`
/// marks a unique symbol type (only meaningful before `symbol`). `readonly`
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
    operator: TSTypeOperatorKind,
    /// any ts type
    type_annotation: NodeIndex,
};

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
    /// any ts type
    type_annotation: NodeIndex,
};

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
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// `formal_parameters`
    params: NodeIndex,
    /// `ts_type_annotation` wrapping the return type. The wrapper's span
    /// starts at the `=>` token.
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
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// `formal_parameters`
    params: NodeIndex,
    /// `ts_type_annotation` wrapping the return type. The wrapper's span
    /// starts at the `=>` token.
    return_type: NodeIndex,
    /// true when the constructor is preceded by `abstract`.
    abstract: bool = false,
};

/// A TypeScript type predicate. Appears only in a function's return-type
/// position and narrows the type of a parameter (or `this`) in the call
/// site's control-flow analysis.
///
/// See: [TypeScript Handbook Type Predicates](https://www.typescriptlang.org/docs/handbook/2/narrowing.html#using-type-predicates)
///
/// ## Example
/// ```ts
/// function isString(x: unknown): x is string { ... }
/// //                             ^^^^^^^^^^^ TSTypePredicate
/// function assertNumber(x: unknown): asserts x is number { ... }
/// //                                 ^^^^^^^^^^^^^^^^^^^ TSTypePredicate (asserts = true)
/// function assert(c: boolean): asserts c { ... }
/// //                           ^^^^^^^^^ TSTypePredicate (asserts = true, type_annotation = .null)
/// ```
pub const TSTypePredicate = struct {
    /// `identifier_name` for a named parameter, or `ts_this_type` for the
    /// implicit `this` parameter.
    parameter_name: NodeIndex,
    /// `ts_type_annotation` wrapping the narrowed type. The wrapper's span
    /// equals the inner type's span. `.null` for a bare `asserts x`
    /// predicate that carries no `is Type` clause.
    type_annotation: NodeIndex = .null,
    /// true when the predicate is introduced by the `asserts` keyword.
    asserts: bool = false,
};

/// An anonymous object type. Holds a list of signatures (properties,
/// methods, index signatures, call signatures, construct signatures) in
/// source order.
///
/// ## Example
/// ```ts
/// type Point = { x: number; y: number };
/// //           ^^^^^^^^^^^^^^^^^^^^^^^^ TSTypeLiteral
/// type Callable = { (x: number): string; length: number };
/// //              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ TSTypeLiteral
/// ```
pub const TSTypeLiteral = struct {
    /// `ts_property_signature`, `ts_method_signature`, `ts_call_signature_declaration`, `ts_construct_signature_declaration`, or `ts_index_signature`
    members: IndexRange,
};

/// The tri-state `+` or `-` modifier that may decorate the `?` or
/// `readonly` slots of a `ts_mapped_type`.
///
/// `none` means the modifier keyword is absent. `true` means it is present
/// without a sign. `plus` and `minus` explicitly add (`+?`, `+readonly`)
/// or remove (`-?`, `-readonly`) the modifier from the mapped property.
pub const TSMappedTypeModifier = enum(u2) {
    none,
    true,
    plus,
    minus,
};

/// A TypeScript mapped type. Projects every key in a union to a new
/// property type: `{ [K in Keys]: Value }`. Supports the `as` remapping
/// clause, the `?` / `+?` / `-?` optionality modifiers, and the
/// `readonly` / `+readonly` / `-readonly` mutability modifiers.
///
/// See: [TypeScript Handbook Mapped Types](https://www.typescriptlang.org/docs/handbook/2/mapped-types.html)
///
/// ## Example
/// ```ts
/// type Readonly<T> = { readonly [K in keyof T]: T[K] };
/// //                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ TSMappedType
/// //                            ^ key
/// //                                  ^^^^^^^ constraint
/// //                                              ^^^^ type_annotation
/// type Mutable<T> = { -readonly [K in keyof T]: T[K] };
/// //                  ^^^^^^^^^ readonly = minus
/// type Partial<T> = { [K in keyof T]?: T[K] };
/// //                                ^ optional = true
/// type Remap<T> = { [K in keyof T as `get${Capitalize<string & K>}`]: () => T[K] };
/// //                                ^^ name_type = template literal type
/// ```
pub const TSMappedType = struct {
    /// `binding_identifier` naming the binding introduced by the `in` clause
    /// (`K` in `[K in T]`). Visible inside `name_type` and
    /// `type_annotation`.
    key: NodeIndex,
    /// any ts type. The constraint the key iterates over, to the right of
    /// `in`.
    constraint: NodeIndex,
    /// any ts type. The optional remapping type supplied after `as`. `.null`
    /// when no `as` clause is present.
    name_type: NodeIndex = .null,
    /// any ts type. `.null` when the annotation is omitted (`{ [K in T] }`
    /// is accepted by the grammar).
    type_annotation: NodeIndex = .null,
    /// the `?` modifier
    optional: TSMappedTypeModifier = .none,
    /// the `readonly` modifier
    readonly: TSMappedTypeModifier = .none,
};

/// A property declaration inside a type literal or interface body. Written
/// as `key: Type`, with optional `readonly` and `?` modifiers. A missing
/// type annotation is allowed.
///
/// ## Example
/// ```ts
/// type T = { readonly x?: number };
/// //         ^^^^^^^^^^^^^^^^^^^^ TSPropertySignature (readonly, optional)
/// //                    ^          key
/// //                      ^^^^^^^^ type_annotation
/// ```
pub const TSPropertySignature = struct {
    /// `identifier_name`, `string_literal`, `numeric_literal`, `private_identifier` (class members only), or any expression when `computed = true`
    key: NodeIndex,
    /// `ts_type_annotation`. `.null` when absent.
    type_annotation: NodeIndex = .null,
    /// true when the key is written inside `[...]`.
    computed: bool = false,
    /// true when a `?` follows the key.
    optional: bool = false,
    /// true when preceded by the `readonly` modifier.
    readonly: bool = false,
};

/// The kind of a method signature. Accessors use `get` or `set`. All
/// other method signatures use `method`.
pub const TSMethodSignatureKind = enum(u2) {
    method,
    get,
    set,

    pub fn toString(self: TSMethodSignatureKind) []const u8 {
        return switch (self) {
            .method => "method",
            .get => "get",
            .set => "set",
        };
    }
};

/// A method, getter, or setter declaration inside a type literal or
/// interface body. Carries the key, an optional generic parameter list,
/// the parameter list, and an optional return type.
///
/// Getters accept no parameters and may declare a return type. Setters
/// accept exactly one parameter and carry no return type
/// (`return_type = .null`).
///
/// ## Example
/// ```ts
/// type T = {
///   add(a: number, b: number): number;
/// //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ TSMethodSignature (kind = method)
///   get size(): number;
/// //^^^^^^^^^^^^^^^^^^ TSMethodSignature (kind = get)
///   set name(v: string);
/// //^^^^^^^^^^^^^^^^^^^ TSMethodSignature (kind = set)
///   optional?<T>(x: T): T;
/// //^^^^^^^^^^^^^^^^^^^^^ TSMethodSignature (optional = true, type_parameters set)
/// };
/// ```
pub const TSMethodSignature = struct {
    /// `identifier_name`, `string_literal`, `numeric_literal`, `private_identifier` (class members only), or any expression when `computed = true`
    key: NodeIndex,
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// `formal_parameters`
    params: NodeIndex,
    /// `ts_type_annotation`. `.null` when absent. Setters always have
    /// `.null`.
    return_type: NodeIndex = .null,
    kind: TSMethodSignatureKind = .method,
    /// true when the key is written inside `[...]`.
    computed: bool = false,
    /// true when a `?` follows the key.
    optional: bool = false,
};

/// A bare call signature inside a type literal or interface body. Written
/// as `(params): ReturnType`. Distinct from `ts_function_type`, which is
/// used as a standalone type. Call signatures are members of an enclosing
/// object type.
///
/// ## Example
/// ```ts
/// type Callable = { <T>(x: T): T };
/// //                ^^^^^^^^^^^^^ TSCallSignatureDeclaration
/// ```
pub const TSCallSignatureDeclaration = struct {
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// `formal_parameters`
    params: NodeIndex,
    /// `ts_type_annotation`. `.null` when absent.
    return_type: NodeIndex = .null,
};

/// A bare construct signature inside a type literal or interface body.
/// Written as `new (params): ReturnType`. Distinct from `ts_constructor_type`,
/// which is used as a standalone type.
///
/// ## Example
/// ```ts
/// type Ctor = { new <T>(x: T): T };
/// //            ^^^^^^^^^^^^^^^^^ TSConstructSignatureDeclaration
/// ```
pub const TSConstructSignatureDeclaration = struct {
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// `formal_parameters`
    params: NodeIndex,
    /// `ts_type_annotation`. `.null` when absent.
    return_type: NodeIndex = .null,
};

/// An index signature inside a type literal, interface body, or class
/// body. Written as `[name: KeyType]: ValueType`, optionally preceded by
/// `readonly`. The parameter list is a small array of identifier-like
/// bindings carrying a type annotation. Almost always one entry.
///
/// ## Example
/// ```ts
/// type Dict = { [k: string]: number };
/// //            ^^^^^^^^^^^^^^^^^^^^ TSIndexSignature
/// //             ^^^^^^^^^ parameters
/// //                         ^^^^^^^ type_annotation
/// type ReadOnly = { readonly [i: number]: string };
/// //                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ TSIndexSignature (readonly)
/// ```
pub const TSIndexSignature = struct {
    /// `binding_identifier[]`. Each entry carries its own
    /// `type_annotation`.
    parameters: IndexRange,
    /// `ts_type_annotation`
    type_annotation: NodeIndex,
    /// true when preceded by the `readonly` modifier.
    readonly: bool = false,
    /// true for class-body index signatures marked `static`.
    static: bool = false,
};

/// A TypeScript `type` alias declaration. Binds an identifier to a type,
/// optionally parameterized by one or more type parameters, and optionally
/// prefixed by the `declare` modifier for ambient contexts.
///
/// See: [TypeScript Handbook Type Aliases](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#type-aliases)
///
/// ## Example
/// ```ts
/// type Maybe<T> = T | null | undefined;
/// //   ^^^^^ id
/// //        ^^^ type_parameters
/// //              ^^^^^^^^^^^^^^^^^^^^ type_annotation
/// declare type Id = number;
/// // ^^^^^^^ declare = true
/// ```
pub const TSTypeAliasDeclaration = struct {
    /// `binding_identifier`
    id: NodeIndex,
    /// `ts_type_parameter_declaration`. `.null` when the alias has no `<T>`
    /// list.
    type_parameters: NodeIndex = .null,
    /// any ts type. Stored as a bare type without a wrapping
    /// `ts_type_annotation` node.
    type_annotation: NodeIndex,
    /// true when preceded by the `declare` modifier.
    declare: bool = false,
};

/// A TypeScript `interface` declaration. Introduces a named structural
/// type with an optional list of parent interfaces and a body of
/// signatures. May be prefixed by the `declare` modifier for ambient
/// contexts.
///
/// See: [TypeScript Handbook Interfaces](https://www.typescriptlang.org/docs/handbook/2/objects.html#interfaces)
///
/// ## Example
/// ```ts
/// interface Foo<T> extends Bar, Base.Thing<T> { a: T; b(): void }
/// //        ^^^ id
/// //           ^^^ type_parameters
/// //                       ^^^^^^^^^^^^^^^^^^ extends
/// //                                         ^^^^^^^^^^^^^^^^^^^^^^ body
/// declare interface Id { x: number }
/// // ^^^^^^^ declare = true
/// ```
pub const TSInterfaceDeclaration = struct {
    /// `binding_identifier`
    id: NodeIndex,
    /// `ts_type_parameter_declaration`. `.null` when the interface has no
    /// `<T>` list.
    type_parameters: NodeIndex = .null,
    /// `ts_interface_heritage[]`. Empty when the interface has no `extends`
    /// clause.
    extends: IndexRange = .empty,
    /// `ts_interface_body`
    body: NodeIndex,
    /// true when preceded by the `declare` modifier.
    declare: bool = false,
};

/// The body of an interface. Holds the signatures (property, method, call,
/// construct, index) in source order. Shares the same signature family as
/// `ts_type_literal`.
///
/// ## Example
/// ```ts
/// interface Foo { a: T; b(): void; [k: string]: U }
/// //            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ TSInterfaceBody
/// ```
pub const TSInterfaceBody = struct {
    /// `ts_property_signature`, `ts_method_signature`, `ts_call_signature_declaration`, `ts_construct_signature_declaration`, or `ts_index_signature`
    body: IndexRange,
};

/// One entry in an interface's `extends` clause. The parent interface is
/// identified by a runtime expression (an identifier path) with an
/// optional `<T, U>` type argument list.
///
/// ## Example
/// ```ts
/// interface Foo extends Bar, Base.Thing<T> {}
/// //                    ^^^ TSInterfaceHeritage (expression = IdentifierReference "Bar")
/// //                         ^^^^^^^^^^^^^^^ TSInterfaceHeritage (expression = MemberExpression, type_arguments = <T>)
/// ```
pub const TSInterfaceHeritage = struct {
    /// `identifier_reference` or a left-associative `member_expression`
    /// chain of identifier names. Calls, computed access, and optional
    /// chaining are rejected by the grammar.
    expression: NodeIndex,
    /// `ts_type_parameter_instantiation`. `.null` when the heritage has no
    /// `<T>` arguments.
    type_arguments: NodeIndex = .null,
};

/// One entry in a class's `implements` clause. The implemented interface
/// is identified by a runtime expression (an identifier path) with an
/// optional `<T, U>` type argument list.
///
/// ## Example
/// ```ts
/// class Foo implements Bar, Base.Thing<T> {}
/// //                   ^^^ TSClassImplements (expression = IdentifierReference "Bar")
/// //                        ^^^^^^^^^^^^^^^ TSClassImplements (expression = MemberExpression, type_arguments = <T>)
/// ```
pub const TSClassImplements = struct {
    /// `identifier_reference` or a left-associative `member_expression`
    /// chain of identifier names. Calls, computed access, and optional
    /// chaining are rejected by the grammar.
    expression: NodeIndex,
    /// `ts_type_parameter_instantiation`. `.null` when the entry has no
    /// `<T>` arguments.
    type_arguments: NodeIndex = .null,
};

/// A TypeScript `enum` declaration. Creates a named runtime binding that
/// also acts as a type. May be prefixed by `const` (an enum whose members
/// are inlined at use sites) or `declare` (ambient declaration, no emitted
/// runtime), or both.
///
/// See: [TypeScript Handbook Enums](https://www.typescriptlang.org/docs/handbook/enums.html)
///
/// ## Example
/// ```ts
/// enum Color { Red, Green = 2, Blue }
/// //   ^^^^^ id
/// //         ^^^^^^^^^^^^^^^^^^^^^^^^ body
/// const enum Flags { A = 1, B = 2 }
/// // ^^^^^ is_const = true
/// declare enum Ambient { X, Y }
/// // ^^^^^^^ declare = true
/// ```
pub const TSEnumDeclaration = struct {
    /// `binding_identifier`
    id: NodeIndex,
    /// `ts_enum_body`
    body: NodeIndex,
    /// true when preceded by the `const` modifier.
    is_const: bool = false,
    /// true when preceded by the `declare` modifier.
    declare: bool = false,
};

/// The body of an enum declaration. Holds the members in source order,
/// delimited by commas (a trailing comma is permitted).
///
/// ## Example
/// ```ts
/// enum Foo { A, B = 1, C }
/// //       ^^^^^^^^^^^^^^ TSEnumBody
/// ```
pub const TSEnumBody = struct {
    /// `ts_enum_member[]`
    members: IndexRange,
};

/// A single member in an enum body. The name is an identifier, a string
/// literal, or a template literal (with or without a computed `[...]`
/// wrapper). The value is an optional initializer expression.
///
/// ## Example
/// ```ts
/// enum E {
///     A,        // id = IdentifierName "A",  initializer = .null
///     B = 1,    // id = IdentifierName "B",  initializer = NumericLiteral 1
///     "s" = 2,  // id = StringLiteral "s",   initializer = NumericLiteral 2
/// }
/// ```
pub const TSEnumMember = struct {
    /// `identifier_name`, `string_literal`, or `template_literal`. When the
    /// source used the computed `[...]` form, `computed` is true.
    id: NodeIndex,
    /// any expression. `.null` when there is no `=` initializer.
    initializer: NodeIndex = .null,
    /// true when the source wrote the name as a computed key `[...]`.
    computed: bool = false,
};

/// The keyword used to introduce a `ts_module_declaration`.
pub const TSModuleDeclarationKind = enum(u1) {
    namespace,
    module,

    pub fn toString(self: TSModuleDeclarationKind) []const u8 {
        return switch (self) {
            .namespace => "namespace",
            .module => "module",
        };
    }
};

/// A TypeScript `namespace` or `module` declaration.
///
/// See: [TypeScript Handbook Namespaces](https://www.typescriptlang.org/docs/handbook/namespaces.html)
///
/// ## Example
/// ```ts
/// namespace Foo { ... }
/// //        ^^^ id, kind = namespace
/// namespace A.B.C { ... }
/// //        ^^^^^ id (TSQualifiedName, left-associative)
/// declare module "./mod" { ... }
/// //             ^^^^^^^ id (StringLiteral), kind = module, declare = true
/// ```
pub const TSModuleDeclaration = struct {
    /// `binding_identifier`, `string_literal`, or `ts_qualified_name`.
    id: NodeIndex,
    /// `ts_module_block`. `.null` for body-less forward declarations like
    /// `declare module "foo";`.
    body: NodeIndex = .null,
    kind: TSModuleDeclarationKind,
    /// true when preceded by the `declare` modifier.
    declare: bool = false,
};

/// The body of a `namespace`, `module`, or `declare global` declaration.
/// Holds the inner statements and declarations in source order, delimited
/// by the enclosing `{` and `}`.
///
/// ## Example
/// ```ts
/// namespace Foo { var x = 1; class C {} }
/// //            ^^^^^^^^^^^^^^^^^^^^^^^^^^ TSModuleBlock
/// ```
pub const TSModuleBlock = struct {
    /// any statement
    body: IndexRange,
};

/// A TypeScript `declare global { ... }` augmentation block.
///
/// ## Example
/// ```ts
/// declare global { interface Window { x: number } }
/// //      ^^^^^^ id
/// //             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ body
/// ```
pub const TSGlobalDeclaration = struct {
    /// `identifier_name` capturing the span of the `global` keyword.
    id: NodeIndex,
    /// `ts_module_block`
    body: NodeIndex,
    /// true when preceded by the `declare` modifier. In valid TypeScript
    /// `declare global` always has `declare = true`. A bare `global { ... }`
    /// nested inside an ambient `declare namespace` body has
    /// `declare = false`.
    declare: bool = false,
};

/// A TypeScript parameter property. A constructor parameter that carries
/// an accessibility, `readonly`, or `override` modifier and therefore
/// implicitly declares a class field of the same name and initial value.
///
/// See: [TypeScript Handbook Parameter Properties](https://www.typescriptlang.org/docs/handbook/2/classes.html#parameter-properties)
///
/// ## Example
/// ```ts
/// class C {
///   constructor(
///     public x: number,        // accessibility = public
///     readonly y: string,      // readonly
///     protected override z: T, // accessibility = protected, override
///   ) {}
/// }
/// ```
pub const TSParameterProperty = struct {
    /// `decorator[]`. Decorators preceding the modifier, in source order.
    decorators: IndexRange,
    /// `binding_identifier` or `assignment_pattern`.
    parameter: NodeIndex,
    /// true for the `override` modifier.
    override: bool = false,
    /// true for the `readonly` modifier.
    readonly: bool = false,
    /// `.none` when no accessibility modifier was written.
    accessibility: Accessibility = .none,
};

/// A TypeScript explicit `this` parameter. Declares the type of the
/// `this` binding inside the function body. Not a real parameter. It
/// contributes nothing to the call's argument positions and is erased at
/// emit time. TypeScript requires `this` to be the first parameter when
/// present.
///
/// See: [TypeScript Handbook this parameters](https://www.typescriptlang.org/docs/handbook/2/functions.html#declaring-this-in-a-function)
///
/// ## Example
/// ```ts
/// function f(this: void, x: number) {}
/// //         ^^^^^^^^^^ TSThisParameter
/// type Handler = (this: Element, e: Event) => void;
/// //              ^^^^^^^^^^^^^ TSThisParameter
/// interface I { m(this: this): void }
/// //              ^^^^^^^^^^ TSThisParameter (type_annotation = TSThisType)
/// ```
pub const TSThisParameter = struct {
    /// `ts_type_annotation` wrapping the declared type of `this`. `.null`
    /// when the parameter is written as bare `this` with no annotation.
    type_annotation: NodeIndex = .null,
};

/// TypeScript `expr as Type` postfix assertion.
///
/// See: [TypeScript Handbook Type Assertions](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#type-assertions)
///
/// ## Example
/// ```ts
/// const n = value as number;
/// //        ^^^^^ expression
/// //                 ^^^^^^ type_annotation
/// ```
pub const TSAsExpression = struct {
    /// any expression
    expression: NodeIndex,
    /// any ts type
    type_annotation: NodeIndex,
};

/// TypeScript `expr satisfies Type` postfix check.
///
/// ## Example
/// ```ts
/// const config = { port: 3000 } satisfies Config;
/// //             ^^^^^^^^^^^^^^ expression
/// //                                      ^^^^^^ type_annotation
/// ```
pub const TSSatisfiesExpression = struct {
    /// any expression
    expression: NodeIndex,
    /// any ts type
    type_annotation: NodeIndex,
};

/// TypeScript `<Type>expr` prefix type assertion.
///
/// ## Example
/// ```ts
/// const n = <number>value;
/// //         ^^^^^^ type_annotation
/// //                ^^^^^ expression
/// ```
pub const TSTypeAssertion = struct {
    /// any ts type
    type_annotation: NodeIndex,
    /// any expression
    expression: NodeIndex,
};

/// TypeScript `expr!` postfix non-null assertion.
///
/// ## Example
/// ```ts
/// const n = value!;
/// //        ^^^^^ expression
/// ```
pub const TSNonNullExpression = struct {
    /// any expression
    expression: NodeIndex,
};

/// TypeScript `expr<T>` instantiation expression without call parens.
///
/// ## Example
/// ```ts
/// const f = makeBox<number>;
/// //        ^^^^^^^ expression
/// //               ^^^^^^^^ type_arguments
/// ```
pub const TSInstantiationExpression = struct {
    /// any expression. Any left-hand-side expression.
    expression: NodeIndex,
    /// `ts_type_parameter_instantiation`
    type_arguments: NodeIndex,
};

/// TypeScript `export = expr` (CommonJS-style ambient export).
///
/// See: [TypeScript Handbook export =](https://www.typescriptlang.org/docs/handbook/modules.html#export--and-import--require)
///
/// ## Example
/// ```ts
/// export = MyNamespace;
/// //       ^^^^^^^^^^^ expression
/// ```
pub const TSExportAssignment = struct {
    /// any expression
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
    /// `binding_identifier`
    id: NodeIndex,
};

/// TypeScript `import x = <module reference>` declaration. Binds the local
/// name `id` to either an external module (`require("m")`) or an entity
/// name path (`Foo.Bar`).
///
/// See: [TypeScript Handbook import = and export =](https://www.typescriptlang.org/docs/handbook/modules.html#export--and-import--require)
///
/// ## Example
/// ```ts
/// import fs = require("fs");
/// //     ^^ id
/// //          ^^^^^^^^^^^^ module_reference (TSExternalModuleReference)
/// import alias = Foo.Bar;
/// //     ^^^^^ id
/// //             ^^^^^^^ module_reference (TSQualifiedName)
/// import alias = Foo;
/// //     ^^^^^ id
/// //             ^^^ module_reference (IdentifierReference)
/// ```
pub const TSImportEqualsDeclaration = struct {
    /// `binding_identifier`
    id: NodeIndex,
    /// `ts_external_module_reference`, `identifier_reference`, or
    /// `ts_qualified_name`.
    module_reference: NodeIndex,
    /// `.type` for `import type x = ...`. `.value` otherwise.
    import_kind: ImportOrExportKind = .value,
};

/// TypeScript `require("module")` on the right hand side of an
/// `import x = require("m")` declaration.
///
/// ## Example
/// ```ts
/// import x = require("./m");
/// //         ^^^^^^^^^^^^^^^ TSExternalModuleReference
/// //                 ^^^^^^ expression
/// ```
pub const TSExternalModuleReference = struct {
    /// `string_literal` naming the required module.
    expression: NodeIndex,
};

/// A JSX element, possibly self-closing.
///
/// See: [JSX Specification JSXElement](https://facebook.github.io/jsx/#prod-JSXElement)
///
/// ## Example
/// ```jsx
/// <Foo bar="baz">hello</Foo>
/// // ^^^^^^^^^^^^^^^ opening_element
/// //                 ^^^^^ children
/// //                      ^^^^^^ closing_element
/// ```
pub const JSXElement = struct {
    /// `jsx_opening_element`
    opening_element: NodeIndex,
    /// `jsx_text`, `jsx_expression_container`, `jsx_spread_child`, `jsx_element`, or `jsx_fragment`
    children: IndexRange,
    /// `jsx_closing_element`. `.null` for self-closing tags like `<Foo />`.
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
///
/// `name` is the tag (here `Foo`). `self_closing` is true for the
/// trailing `/>`.
pub const JSXOpeningElement = struct {
    /// `jsx_identifier`, `jsx_namespaced_name`, or `jsx_member_expression`
    name: NodeIndex,
    /// `jsx_attribute` or `jsx_spread_attribute`.
    attributes: IndexRange,
    self_closing: bool,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
};

/// The closing `</Foo>` of a JSX element.
///
/// ## Example
/// ```jsx
/// </Foo>
/// // ^^^ name
/// ```
pub const JSXClosingElement = struct {
    /// `jsx_identifier`, `jsx_namespaced_name`, or `jsx_member_expression`
    name: NodeIndex,
};

/// A JSX fragment `<>...</>`.
///
/// See: [JSX Specification JSXFragment](https://facebook.github.io/jsx/#prod-JSXFragment)
///
/// ## Example
/// ```jsx
/// <>hello</>
/// //^^^^^ children
/// ```
///
/// `opening_fragment` is `<>`, `closing_fragment` is `</>`.
pub const JSXFragment = struct {
    /// `jsx_opening_fragment`
    opening_fragment: NodeIndex,
    /// `jsx_text`, `jsx_expression_container`, `jsx_spread_child`, `jsx_element`, or `jsx_fragment`
    children: IndexRange,
    /// `jsx_closing_fragment`
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
///
/// The tag `Foo` is also a `jsx_identifier`.
pub const JSXIdentifier = struct {
    name: String = .empty,
};

/// A JSX name of the form `namespace:name`.
///
/// ## Example
/// ```jsx
/// <svg:path />
/// // ^^^ namespace
/// //     ^^^^ name
/// ```
pub const JSXNamespacedName = struct {
    /// `jsx_identifier`
    namespace: NodeIndex,
    /// `jsx_identifier`
    name: NodeIndex,
};

/// A dotted JSX tag name.
///
/// ## Example
/// ```jsx
/// <Foo.Bar.Baz />
/// // ^^^^^^^ object
/// //         ^^^ property
/// ```
pub const JSXMemberExpression = struct {
    /// `jsx_identifier` or a nested `jsx_member_expression`.
    object: NodeIndex,
    /// `jsx_identifier`
    property: NodeIndex,
};

/// A single JSX attribute.
///
/// ## Example
/// ```jsx
/// <Foo bar="baz" disabled />
/// //   ^^^ name
/// //       ^^^^^ value
/// //             ^^^^^^^^ boolean attribute (value = .null)
/// ```
pub const JSXAttribute = struct {
    /// `jsx_identifier` or `jsx_namespaced_name`.
    name: NodeIndex,
    /// `string_literal`, `jsx_expression_container`, `jsx_element`, or
    /// `jsx_fragment`. `.null` for boolean-like attributes.
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
    /// any expression
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
    /// any expression. `jsx_empty_expression` for `{}`.
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
    /// any expression
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
    ts_type_query: TSTypeQuery,
    ts_import_type: TSImportType,
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
    ts_jsdoc_nullable_type: TSJSDocNullableType,
    ts_jsdoc_non_nullable_type: TSJSDocNonNullableType,
    ts_jsdoc_unknown_type: TSJSDocUnknownType,
    ts_union_type: TSUnionType,
    ts_intersection_type: TSIntersectionType,
    ts_conditional_type: TSConditionalType,
    ts_infer_type: TSInferType,
    ts_type_operator: TSTypeOperator,
    ts_parenthesized_type: TSParenthesizedType,
    ts_function_type: TSFunctionType,
    ts_constructor_type: TSConstructorType,
    ts_type_predicate: TSTypePredicate,
    ts_type_literal: TSTypeLiteral,
    ts_mapped_type: TSMappedType,
    ts_property_signature: TSPropertySignature,
    ts_method_signature: TSMethodSignature,
    ts_call_signature_declaration: TSCallSignatureDeclaration,
    ts_construct_signature_declaration: TSConstructSignatureDeclaration,
    ts_index_signature: TSIndexSignature,
    ts_type_alias_declaration: TSTypeAliasDeclaration,
    ts_interface_declaration: TSInterfaceDeclaration,
    ts_interface_body: TSInterfaceBody,
    ts_interface_heritage: TSInterfaceHeritage,
    ts_class_implements: TSClassImplements,
    ts_enum_declaration: TSEnumDeclaration,
    ts_enum_body: TSEnumBody,
    ts_enum_member: TSEnumMember,
    ts_module_declaration: TSModuleDeclaration,
    ts_module_block: TSModuleBlock,
    ts_global_declaration: TSGlobalDeclaration,
    ts_parameter_property: TSParameterProperty,
    ts_this_parameter: TSThisParameter,
    ts_as_expression: TSAsExpression,
    ts_satisfies_expression: TSSatisfiesExpression,
    ts_type_assertion: TSTypeAssertion,
    ts_non_null_expression: TSNonNullExpression,
    ts_instantiation_expression: TSInstantiationExpression,
    ts_export_assignment: TSExportAssignment,
    ts_namespace_export_declaration: TSNamespaceExportDeclaration,
    ts_import_equals_declaration: TSImportEqualsDeclaration,
    ts_external_module_reference: TSExternalModuleReference,

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

    /// True when this node produces a value at runtime.
    ///
    /// Covers literals, identifiers used as values, operator expressions,
    /// member access, calls, function and class expressions, JSX elements,
    /// and the TypeScript value-position wrappers. For dual-purpose nodes
    /// (`function`, `class`) the `type` field is consulted.
    pub fn isExpression(self: NodeData) bool {
        return switch (self) {
            .identifier_reference,
            .this_expression,
            .super,
            .meta_property,
            .string_literal,
            .numeric_literal,
            .bigint_literal,
            .boolean_literal,
            .null_literal,
            .regexp_literal,
            .template_literal,
            .binary_expression,
            .logical_expression,
            .unary_expression,
            .update_expression,
            .assignment_expression,
            .conditional_expression,
            .sequence_expression,
            .member_expression,
            .call_expression,
            .chain_expression,
            .new_expression,
            .tagged_template_expression,
            .arrow_function_expression,
            .array_expression,
            .object_expression,
            .parenthesized_expression,
            .import_expression,
            .await_expression,
            .yield_expression,
            .ts_as_expression,
            .ts_satisfies_expression,
            .ts_type_assertion,
            .ts_non_null_expression,
            .ts_instantiation_expression,
            .jsx_element,
            .jsx_fragment,
            => true,
            .function => |f| f.type == .function_expression or f.type == .ts_empty_body_function_expression,
            .class => |c| c.type == .class_expression,
            else => false,
        };
    }

    /// True when this node is valid at statement position.
    ///
    /// Covers control flow, structural statements, declarations, imports
    /// and exports, and TypeScript top-level declarations. For dual-purpose
    /// nodes (`function`, `class`) the `type` field is consulted.
    pub fn isStatement(self: NodeData) bool {
        return switch (self) {
            .if_statement,
            .switch_statement,
            .for_statement,
            .for_in_statement,
            .for_of_statement,
            .while_statement,
            .do_while_statement,
            .break_statement,
            .continue_statement,
            .labeled_statement,
            .return_statement,
            .throw_statement,
            .try_statement,
            .with_statement,
            .block_statement,
            .expression_statement,
            .empty_statement,
            .debugger_statement,
            .variable_declaration,
            .import_declaration,
            .export_named_declaration,
            .export_default_declaration,
            .export_all_declaration,
            .ts_type_alias_declaration,
            .ts_interface_declaration,
            .ts_enum_declaration,
            .ts_module_declaration,
            .ts_global_declaration,
            .ts_import_equals_declaration,
            .ts_export_assignment,
            .ts_namespace_export_declaration,
            => true,
            .function => |f| f.type == .function_declaration or f.type == .ts_declare_function,
            .class => |c| c.type == .class_declaration,
            else => false,
        };
    }

    /// True when this node is a literal value.
    pub fn isLiteral(self: NodeData) bool {
        return switch (self) {
            .string_literal,
            .numeric_literal,
            .bigint_literal,
            .boolean_literal,
            .null_literal,
            .regexp_literal,
            .template_literal,
            => true,
            else => false,
        };
    }

    /// True for nodes that introduce a function-like body. Covers `function`
    /// (in any form) and `arrow_function_expression`. Does not include
    /// `method_definition`, which wraps a `function` in its `value` field.
    pub fn isCallable(self: NodeData) bool {
        return switch (self) {
            .function,
            .arrow_function_expression,
            => true,
            else => false,
        };
    }

    /// True for binding patterns introduced by destructuring.
    ///
    /// `binding_identifier`, `array_pattern`, `object_pattern`, or
    /// `assignment_pattern`.
    pub fn isPattern(self: NodeData) bool {
        return switch (self) {
            .binding_identifier,
            .array_pattern,
            .object_pattern,
            .assignment_pattern,
            => true,
            else => false,
        };
    }

    /// True for declaration nodes that introduce one or more bindings.
    ///
    /// Covers `variable_declaration`, function and class declaration forms,
    /// imports and exports, and TypeScript declaration kinds.
    pub fn isDeclaration(self: NodeData) bool {
        return switch (self) {
            .variable_declaration,
            .import_declaration,
            .export_named_declaration,
            .export_default_declaration,
            .export_all_declaration,
            .ts_type_alias_declaration,
            .ts_interface_declaration,
            .ts_enum_declaration,
            .ts_module_declaration,
            .ts_global_declaration,
            .ts_import_equals_declaration,
            => true,
            .function => |f| f.type == .function_declaration or f.type == .ts_declare_function,
            .class => |c| c.type == .class_declaration,
            else => false,
        };
    }

    /// True for the iteration statements: `for`, `for-in`, `for-of`,
    /// `while`, and `do-while`. Useful for `break` and `continue` scope
    /// checks.
    pub fn isIteration(self: NodeData) bool {
        return switch (self) {
            .for_statement,
            .for_in_statement,
            .for_of_statement,
            .while_statement,
            .do_while_statement,
            => true,
            else => false,
        };
    }
};

pub const Node = struct {
    data: NodeData,
    span: Span,
};

pub const NodeList = std.MultiArrayList(Node);
