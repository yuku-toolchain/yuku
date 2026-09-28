//! AST node definitions. See [AST reference](https://yuku.fyi/parser/ast).
//!
//! Child fields (`NodeIndex`/`IndexRange`) of every node struct are declared
//! in source order.

const std = @import("std");
const strings = @import("strings.zig");

pub const String = strings.String;
pub const StringPool = strings.ASTStringPool;

pub const Span = @import("token.zig").Span;
pub const Token = @import("token.zig").Token;
pub const TokenTag = @import("token.zig").TokenTag;
pub const TokenFlag = @import("token.zig").TokenFlag;
pub const TokenMask = @import("token.zig").Mask;

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
pub const SourceType = enum {
    script,
    module,
    commonjs,

    pub fn toString(self: SourceType) []const u8 {
        return switch (self) {
            .script => "script",
            .module => "module",
            .commonjs => "commonjs",
        };
    }

    /// Returns `.commonjs` for `.cjs` and `.cts` paths and `.module` otherwise.
    pub fn fromPath(path: []const u8) SourceType {
        if (std.mem.endsWith(u8, path, ".cjs") or std.mem.endsWith(u8, path, ".cts")) {
            return .commonjs;
        }

        return .module;
    }
};

/// Language variant of a file. Decides which syntax features are enabled.
pub const Lang = enum {
    js,
    ts,
    jsx,
    tsx,
    dts,

    /// Resolves `.d.ts`, `.d.mts`, and `.d.cts` to `dts`, `.tsx` to `tsx`,
    /// `.ts`, `.mts`, and `.cts` to `ts`, `.jsx` to `jsx`, and anything else
    /// to `js`.
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

/// A line or block comment. `value` is the text without delimiters and
/// `span` covers the whole comment.
pub const Comment = struct {
    type: Type,
    value: String,
    span: Span,

    pub const Type = enum(u1) {
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

/// A comment bound to a host node, read with `Tree.commentsOf`.
///
/// `same_line` is true when the comment shares a line with the adjacent edge
/// of the host, its start for `before` and its end for `after`. It is always
/// false for `inside`.
pub const AttachedComment = struct {
    type: Comment.Type,
    position: Position = .before,
    same_line: bool = false,
    value: String,

    pub const Position = enum(u2) {
        before,
        after,
        inside,

        pub fn toString(self: Position) []const u8 {
            return switch (self) {
                .before => "before",
                .after => "after",
                .inside => "inside",
            };
        }
    };
};

/// A parsed or programmatically built AST. All memory lives in one arena,
/// so `deinit()` frees everything at once.
pub const Tree = struct {
    /// The `program` node.
    root: NodeIndex = undefined,
    nodes: NodeList = .empty,
    /// Child lists of all nodes, resolved with `extra()`.
    extras: std.ArrayList(NodeIndex) = .empty,
    diagnostics: std.ArrayList(Diagnostic) = .empty,
    /// Every comment in source order. Populated when the comment mode is
    /// `.flat` or `.both`.
    comments: []const Comment = &.{},
    /// Attached comments grouped by host node, read with `commentsOf()`.
    /// Populated when the comment mode is `.attached` or `.both`.
    attached_comments: []const AttachedComment = &.{},
    /// Prefix sums into `attached_comments`, `nodes.len + 1` entries long.
    attached_comment_offsets: []const u32 = &.{},
    /// Every consumed token in source order, ending with `eof`. Empty unless
    /// `Options.tokens` is set.
    tokens: []const Token = &.{},
    arena: std.heap.ArenaAllocator,
    /// Empty for trees built with `initEmpty()`.
    source: []const u8 = "",
    strings: StringPool = .{},
    source_type: SourceType = .module,
    lang: Lang = .js,

    /// Creates a tree for parsing or transforming `source`.
    pub fn init(child_allocator: std.mem.Allocator, source: []const u8) Tree {
        return .{
            .arena = std.heap.ArenaAllocator.init(child_allocator),
            .source = source,
            .strings = .{ .source = source },
        };
    }

    /// Creates a tree without source text, for building an AST with
    /// `addString()`.
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

    /// Returns true if any diagnostic is an error.
    pub inline fn hasErrors(self: *const Tree) bool {
        for (self.diagnostics.items) |d| {
            if (d.severity == .@"error") return true;
        }
        return false;
    }

    /// Returns true if the tree has any diagnostics.
    pub inline fn hasDiagnostics(self: *const Tree) bool {
        return self.diagnostics.items.len > 0;
    }

    /// Appends a diagnostic to the tree.
    pub fn addDiagnostic(self: *Tree, diag: Diagnostic) error{OutOfMemory}!void {
        try self.diagnostics.append(self.arena.allocator(), diag);
    }

    /// Returns the data for the node at the given index.
    pub inline fn data(self: *const Tree, index: NodeIndex) NodeData {
        std.debug.assert(index != .null);
        std.debug.assert(@intFromEnum(index) < self.nodes.len);
        return self.nodes.items(.data)[@intFromEnum(index)];
    }

    /// Returns the span for the node at the given index.
    pub inline fn span(self: *const Tree, index: NodeIndex) Span {
        std.debug.assert(index != .null);
        std.debug.assert(@intFromEnum(index) < self.nodes.len);
        return self.nodes.items(.span)[@intFromEnum(index)];
    }

    /// Returns the child nodes for the given range.
    pub inline fn extra(self: *const Tree, range: IndexRange) []const NodeIndex {
        std.debug.assert(range.start + range.len <= self.extras.items.len);
        return self.extras.items[range.start..][0..range.len];
    }

    /// Replaces an existing node's data in place.
    pub inline fn setData(self: *Tree, index: NodeIndex, new_data: NodeData) void {
        std.debug.assert(index != .null);
        std.debug.assert(@intFromEnum(index) < self.nodes.len);
        self.nodes.items(.data)[@intFromEnum(index)] = new_data;
    }

    /// Replaces an existing node's span in place.
    pub inline fn setSpan(self: *Tree, index: NodeIndex, new_span: Span) void {
        std.debug.assert(index != .null);
        std.debug.assert(@intFromEnum(index) < self.nodes.len);
        std.debug.assert(new_span.start <= new_span.end);
        self.nodes.items(.span)[@intFromEnum(index)] = new_span;
    }

    /// Updates the name of an identifier-shaped node in place.
    pub fn setIdentifierName(self: *Tree, index: NodeIndex, name: String) void {
        std.debug.assert(index != .null);
        switch (self.data(index)) {
            .binding_identifier => |bid| {
                var n = bid;
                n.name = name;
                self.setData(index, .{ .binding_identifier = n });
            },
            inline .identifier_reference,
            .identifier_name,
            .label_identifier,
            .private_identifier,
            .jsx_identifier,
            => |_, tag| self.setData(index, @unionInit(NodeData, @tagName(tag), .{ .name = name })),
            else => unreachable,
        }
    }

    /// Appends a node and returns its index.
    pub inline fn addNode(
        self: *Tree,
        node_data: NodeData,
        node_span: Span,
    ) error{OutOfMemory}!NodeIndex {
        std.debug.assert(self.nodes.len < std.math.maxInt(u32));
        std.debug.assert(node_span.start <= node_span.end);
        const index: NodeIndex = @enumFromInt(@as(u32, @intCast(self.nodes.len)));
        const entry: Node = .{ .data = node_data, .span = node_span };
        if (self.nodes.len < self.nodes.capacity) {
            self.nodes.appendAssumeCapacity(entry);
        } else {
            try self.nodes.append(self.arena.allocator(), entry);
        }
        return index;
    }

    /// Appends a child list and returns its range.
    pub inline fn addExtra(self: *Tree, children: []const NodeIndex) error{OutOfMemory}!IndexRange {
        std.debug.assert(self.extras.items.len + children.len <= std.math.maxInt(u32));
        const start: u32 = @intCast(self.extras.items.len);
        if (self.extras.items.len + children.len <= self.extras.capacity) {
            self.extras.appendSliceAssumeCapacity(children);
        } else {
            try self.extras.appendSlice(self.arena.allocator(), children);
        }
        return .{ .start = start, .len = @intCast(children.len) };
    }

    /// Reserves room for `entries` more strings totalling at most `bytes`,
    /// ahead of bulk `addString()` calls.
    pub fn ensureUnusedStringCapacity(
        self: *Tree,
        bytes: u32,
        entries: u32,
    ) error{OutOfMemory}!void {
        return self.strings.ensureUnusedCapacity(self.arena.allocator(), bytes, entries);
    }

    /// Returns a `String` for a range of the source text.
    pub inline fn sourceSlice(self: *const Tree, start: u32, end: u32) String {
        return self.strings.sourceSlice(start, end);
    }

    /// Interns `str` in the string pool and returns its `String`. Use for text
    /// that is not in the source, such as escaped or synthesized names.
    pub fn addString(self: *Tree, str: []const u8) error{OutOfMemory}!String {
        return self.strings.addString(self.arena.allocator(), str);
    }

    /// Returns the text of a `String`.
    pub inline fn string(self: *const Tree, id: String) []const u8 {
        return self.strings.get(id);
    }

    /// Returns the comments attached to `node`, in source order. Empty when
    /// the comment mode does not attach comments.
    pub inline fn commentsOf(self: *const Tree, node: NodeIndex) []const AttachedComment {
        std.debug.assert(node != .null);
        const offsets = self.attached_comment_offsets;
        const i = @intFromEnum(node);
        if (i + 1 >= offsets.len) return &.{};
        std.debug.assert(offsets[i] <= offsets[i + 1]);
        std.debug.assert(offsets[i + 1] <= self.attached_comments.len);
        return self.attached_comments[offsets[i]..offsets[i + 1]];
    }
};

/// Index into `Tree.nodes`. `.null` marks an absent child.
pub const NodeIndex = enum(u32) { null = std.math.maxInt(u32), _ };

/// A child list, stored as a window into `Tree.extras`.
pub const IndexRange = struct {
    start: u32,
    len: u32,

    pub const empty: IndexRange = .{ .start = 0, .len = 0 };
};

/// The `super` keyword.
pub const Super = struct {};

/// The `null` literal.
pub const NullLiteral = struct {};

/// The `this` keyword.
pub const ThisExpression = struct {};

/// A `debugger;` statement.
pub const DebuggerStatement = struct {};

/// A standalone `;` statement.
pub const EmptyStatement = struct {};

/// A `@expression` decorator.
///
/// See [TC39 decorators](https://github.com/tc39/proposal-decorators).
pub const Decorator = struct {
    /// Any expression.
    expression: NodeIndex,
};

/// Form of a `class` node.
pub const ClassType = enum {
    class_declaration,
    class_expression,
};

/// A class declaration or expression.
pub const Class = struct {
    type: ClassType,
    /// `decorator`.
    decorators: IndexRange,
    /// `binding_identifier`. `.null` when anonymous.
    id: NodeIndex,
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// Any expression. `.null` when absent.
    super_class: NodeIndex,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    super_type_arguments: NodeIndex = .null,
    /// `ts_class_implements`.
    implements: IndexRange = .empty,
    /// `class_body`.
    body: NodeIndex,
    abstract: bool = false,
    declare: bool = false,
};

/// The `{ ... }` body of a class.
pub const ClassBody = struct {
    /// `method_definition`, `property_definition`, `static_block`, or
    /// `ts_index_signature`.
    body: IndexRange,
};

/// Kind of a `method_definition`.
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

/// Accessibility modifier of a TypeScript class member. `.none` means no
/// modifier was written, which differs from an explicit `public`.
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
pub const MethodDefinition = struct {
    /// `decorator`.
    decorators: IndexRange,
    /// `identifier_name`, `string_literal`, `numeric_literal`, `bigint_literal`,
    /// or `private_identifier`. Any expression when `computed`.
    key: NodeIndex,
    /// `function`.
    value: NodeIndex,
    kind: MethodDefinitionKind,
    computed: bool,
    static: bool,
    override: bool = false,
    optional: bool = false,
    abstract: bool = false,
    accessibility: Accessibility = .none,
};

/// A class field, or an auto-accessor declared with `accessor`.
pub const PropertyDefinition = struct {
    /// `decorator`.
    decorators: IndexRange,
    /// `identifier_name`, `string_literal`, `numeric_literal`, `bigint_literal`,
    /// or `private_identifier`. Any expression when `computed`.
    key: NodeIndex,
    /// `ts_type_annotation`. `.null` when absent.
    type_annotation: NodeIndex = .null,
    /// Any expression. `.null` when absent.
    value: NodeIndex,
    computed: bool,
    static: bool,
    accessor: bool,
    declare: bool = false,
    override: bool = false,
    optional: bool = false,
    /// True for a definite assignment assertion, as in `x!: T`.
    definite: bool = false,
    readonly: bool = false,
    abstract: bool = false,
    accessibility: Accessibility = .none,
};

/// A `static { ... }` block in a class body.
pub const StaticBlock = struct {
    /// Any statement.
    body: IndexRange,
};

/// Operator of a `binary_expression`.
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

/// A binary expression with a non-logical operator.
pub const BinaryExpression = struct {
    /// Any expression, or `private_identifier` in `#x in obj`.
    left: NodeIndex,
    /// Any expression.
    right: NodeIndex,
    operator: BinaryOperator,
};

/// Operator of a `logical_expression`.
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

/// A short-circuiting `&&`, `||`, or `??` expression.
pub const LogicalExpression = struct {
    /// Any expression.
    left: NodeIndex,
    /// Any expression.
    right: NodeIndex,
    operator: LogicalOperator,
};

/// A `test ? consequent : alternate` expression.
pub const ConditionalExpression = struct {
    /// Any expression.
    @"test": NodeIndex,
    /// Any expression.
    consequent: NodeIndex,
    /// Any expression.
    alternate: NodeIndex,
};

/// Operator of a `unary_expression`.
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

/// A prefix unary expression such as `!x` or `typeof x`.
pub const UnaryExpression = struct {
    /// Any expression.
    argument: NodeIndex,
    operator: UnaryOperator,
};

/// Operator of an `update_expression`.
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

/// A prefix or postfix `++` or `--` expression.
pub const UpdateExpression = struct {
    /// Any simple assignment target.
    argument: NodeIndex,
    operator: UpdateOperator,
    prefix: bool,
};

/// Operator of an `assignment_expression`.
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

/// An assignment such as `x = 1` or `x += 1`.
pub const AssignmentExpression = struct {
    /// Any assignment target, or `array_pattern` or `object_pattern` for
    /// destructuring.
    left: NodeIndex,
    /// Any expression.
    right: NodeIndex,
    operator: AssignmentOperator,
};

/// Keyword of a `variable_declaration`.
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
pub const VariableDeclaration = struct {
    kind: VariableKind,
    /// `variable_declarator`.
    declarators: IndexRange,
    declare: bool = false,
};

/// A single binding in a variable declaration. Its type annotation lives on
/// `id`.
pub const VariableDeclarator = struct {
    /// Any binding pattern.
    id: NodeIndex,
    /// Any expression. `.null` when absent.
    init: NodeIndex,
    /// True for a definite assignment assertion, as in `let x!: T`.
    definite: bool = false,
};

/// An expression used as a statement.
pub const ExpressionStatement = struct {
    /// Any expression.
    expression: NodeIndex,
};

/// An `if` statement.
pub const IfStatement = struct {
    /// Any expression.
    @"test": NodeIndex,
    /// Any statement.
    consequent: NodeIndex,
    /// Any statement. `.null` when absent.
    alternate: NodeIndex,
};

/// A `switch` statement.
pub const SwitchStatement = struct {
    /// Any expression.
    discriminant: NodeIndex,
    /// `switch_case`.
    cases: IndexRange,
};

/// A `for (init; test; update)` loop.
pub const ForStatement = struct {
    /// `variable_declaration` or any expression. `.null` when absent.
    init: NodeIndex,
    /// Any expression. `.null` when absent.
    @"test": NodeIndex,
    /// Any expression. `.null` when absent.
    update: NodeIndex,
    /// Any statement.
    body: NodeIndex,
};

/// A `for (left in right)` loop.
pub const ForInStatement = struct {
    /// `variable_declaration`, any assignment target, or a destructuring
    /// pattern.
    left: NodeIndex,
    /// Any expression.
    right: NodeIndex,
    /// Any statement.
    body: NodeIndex,
};

/// A `for (left of right)` or `for await (left of right)` loop.
pub const ForOfStatement = struct {
    /// `variable_declaration`, any assignment target, or a destructuring
    /// pattern.
    left: NodeIndex,
    /// Any expression.
    right: NodeIndex,
    /// Any statement.
    body: NodeIndex,
    await: bool,
};

/// A `break` statement.
pub const BreakStatement = struct {
    /// `label_identifier`. `.null` when absent.
    label: NodeIndex,
};

/// A `continue` statement.
pub const ContinueStatement = struct {
    /// `label_identifier`. `.null` when absent.
    label: NodeIndex,
};

/// A labeled statement.
pub const LabeledStatement = struct {
    /// `label_identifier`.
    label: NodeIndex,
    /// Any statement.
    body: NodeIndex,
};

/// A `case` or `default` clause of a `switch` statement.
pub const SwitchCase = struct {
    /// Any expression. `.null` for `default`.
    @"test": NodeIndex,
    /// Any statement.
    consequent: IndexRange,
};

/// A `return` statement.
pub const ReturnStatement = struct {
    /// Any expression. `.null` when absent.
    argument: NodeIndex,
};

/// A `throw` statement.
pub const ThrowStatement = struct {
    /// Any expression.
    argument: NodeIndex,
};

/// A `try` statement with a `catch` clause, a `finally` block, or both.
pub const TryStatement = struct {
    /// `block_statement`.
    block: NodeIndex,
    /// `catch_clause`. `.null` when absent.
    handler: NodeIndex,
    /// `block_statement`. `.null` when absent.
    finalizer: NodeIndex,
};

/// The `catch` clause of a `try` statement.
pub const CatchClause = struct {
    /// Any binding pattern. `.null` when absent.
    param: NodeIndex,
    /// `block_statement`.
    body: NodeIndex,
};

/// A `while` loop.
pub const WhileStatement = struct {
    /// Any expression.
    @"test": NodeIndex,
    /// Any statement.
    body: NodeIndex,
};

/// A `do ... while` loop.
pub const DoWhileStatement = struct {
    /// Any statement.
    body: NodeIndex,
    /// Any expression.
    @"test": NodeIndex,
};

/// A `with` statement.
pub const WithStatement = struct {
    /// Any expression.
    object: NodeIndex,
    /// Any statement.
    body: NodeIndex,
};

/// A string literal.
pub const StringLiteral = struct {
    /// Decoded value without the quotes.
    value: String = .empty,
    /// Source text including the quotes.
    raw: String = .empty,
};

/// A numeric literal in decimal, hex, octal, or binary.
pub const NumericLiteral = struct {
    kind: Kind,
    /// Source text, including any base prefix and `_` separators.
    raw: String = .empty,

    /// Computes the IEEE 754 double value.
    pub fn value(self: NumericLiteral, tree: *const Tree) f64 {
        const raw = tree.string(self.raw);
        if (raw.len == 0) return 0;
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
                // legacy octal has a bare 0 prefix
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
                const digit_value = std.fmt.charToDigit(d, base) catch unreachable;
                val = val * fbase + @as(f64, @floatFromInt(digit_value));
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

/// A BigInt literal such as `10n`.
pub const BigIntLiteral = struct {
    /// Source text without the trailing `n`.
    raw: String = .empty,
};

/// A `true` or `false` literal.
pub const BooleanLiteral = struct {
    value: bool,
};

/// A regular expression literal such as `/ab+c/gi`.
pub const RegExpLiteral = struct {
    pattern: String = .empty,
    flags: String = .empty,
};

/// A template literal.
pub const TemplateLiteral = struct {
    /// `template_element`. Always `expressions.len + 1` entries.
    quasis: IndexRange,
    /// Any expression.
    expressions: IndexRange,
};

/// A static text span of a template literal.
pub const TemplateElement = struct {
    /// Escape-decoded text. Empty when `is_cooked_undefined`.
    cooked: String = .empty,
    /// Source text as written. Empty for synthetic nodes, which print from `cooked`.
    raw: String = .empty,
    tail: bool,
    /// True when an invalid escape in a tagged template leaves the cooked
    /// value undefined.
    is_cooked_undefined: bool = false,
};

/// An identifier that refers to a binding.
pub const IdentifierReference = struct {
    name: String = .empty,
};

/// A `#name` class member name. `name` excludes the `#`.
pub const PrivateIdentifier = struct {
    name: String = .empty,
};

/// An identifier that declares a binding.
pub const BindingIdentifier = struct {
    name: String = .empty,
    /// `decorator`. Only set on parameters.
    decorators: IndexRange = .empty,
    /// `ts_type_annotation`. `.null` when absent.
    type_annotation: NodeIndex = .null,
    optional: bool = false,
};

/// An identifier that is neither a reference nor a binding, such as a
/// property key or member name.
pub const IdentifierName = struct {
    name: String = .empty,
};

/// A statement label, also used as the target of `break` and `continue`.
pub const LabelIdentifier = struct {
    name: String = .empty,
};

/// A pattern with a default value, such as `x = 0`.
pub const AssignmentPattern = struct {
    /// `decorator`. Only set on parameters.
    decorators: IndexRange = .empty,
    /// Any binding pattern or assignment target.
    left: NodeIndex,
    /// `ts_type_annotation`. `.null` when absent.
    type_annotation: NodeIndex = .null,
    /// Any expression.
    right: NodeIndex,
    optional: bool = false,
};

/// A `...rest` element in a pattern or parameter list.
pub const BindingRestElement = struct {
    /// `decorator`. Only set on parameters.
    decorators: IndexRange = .empty,
    /// Any binding pattern or assignment target.
    argument: NodeIndex,
    /// `ts_type_annotation`. `.null` when absent.
    type_annotation: NodeIndex = .null,
    optional: bool = false,
};

/// An array destructuring pattern.
pub const ArrayPattern = struct {
    /// `decorator`. Only set on parameters.
    decorators: IndexRange = .empty,
    /// Any binding pattern or assignment target. `.null` for holes.
    elements: IndexRange,
    /// `binding_rest_element`. `.null` when absent.
    rest: NodeIndex,
    /// `ts_type_annotation`. `.null` when absent.
    type_annotation: NodeIndex = .null,
    optional: bool = false,
};

/// An object destructuring pattern.
pub const ObjectPattern = struct {
    /// `decorator`. Only set on parameters.
    decorators: IndexRange = .empty,
    /// `binding_property`.
    properties: IndexRange,
    /// `binding_rest_element`. `.null` when absent.
    rest: NodeIndex,
    /// `ts_type_annotation`. `.null` when absent.
    type_annotation: NodeIndex = .null,
    optional: bool = false,
};

/// A property in an `object_pattern`.
pub const BindingProperty = struct {
    /// `identifier_name`, `string_literal`, `numeric_literal`, or
    /// `bigint_literal`. Any expression when `computed`.
    key: NodeIndex,
    /// Any binding pattern or assignment target.
    value: NodeIndex,
    shorthand: bool,
    computed: bool,
};

/// An array literal.
pub const ArrayExpression = struct {
    /// Any expression or `spread_element`. `.null` for holes.
    elements: IndexRange,
};

/// An object literal.
pub const ObjectExpression = struct {
    /// `object_property` or `spread_element`.
    properties: IndexRange,
};

/// A `...argument` spread in an array literal, object literal, or argument
/// list.
pub const SpreadElement = struct {
    /// Any expression.
    argument: NodeIndex,
};

/// Kind of an `object_property`.
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
pub const ObjectProperty = struct {
    /// `identifier_name`, `string_literal`, `numeric_literal`, or
    /// `bigint_literal`. Any expression when `computed`.
    key: NodeIndex,
    /// Any expression, or `function` for methods, getters, and setters.
    value: NodeIndex,
    kind: PropertyKind,
    /// True for the method shorthand `b() {}`.
    method: bool,
    shorthand: bool,
    computed: bool,
};

/// The root node of every tree.
pub const Program = struct {
    /// `.script` or `.module`. CommonJS files are `.script`.
    source_type: SourceType,
    /// Any statement or `directive`.
    body: IndexRange,
    hashbang: ?Hashbang = null,
};

/// A `#!` line at the start of a file. `value` is the text after `#!`.
pub const Hashbang = struct {
    value: String = .empty,
};

/// A directive prologue entry such as `"use strict";`.
pub const Directive = struct {
    /// `string_literal`.
    expression: NodeIndex,
    /// Raw text between the quotes.
    value: String = .empty,
};

/// Form of a `function` node.
///
/// `ts_declare_function` is a body-less declaration, either `declare function`
/// or an overload signature. `ts_empty_body_function_expression` is the
/// body-less `value` of an overload, abstract, or ambient class method.
pub const FunctionType = enum {
    function_declaration,
    function_expression,
    ts_declare_function,
    ts_empty_body_function_expression,
};

/// A function declaration or expression, also used as a method `value`.
pub const Function = struct {
    type: FunctionType,
    /// `binding_identifier`. `.null` when anonymous.
    id: NodeIndex,
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// `formal_parameters`.
    params: NodeIndex,
    /// `ts_type_annotation`. `.null` when absent.
    return_type: NodeIndex = .null,
    /// `function_body`. `.null` for the body-less `ts_*` forms.
    body: NodeIndex,
    generator: bool,
    async: bool,
    /// True for `declare function`. Tells an ambient declaration apart from an
    /// overload signature, as both are `ts_declare_function`.
    declare: bool = false,
};

/// The `{ ... }` body of a function.
pub const FunctionBody = struct {
    /// Any statement or `directive`.
    body: IndexRange,
};

/// A `{ ... }` block statement.
pub const BlockStatement = struct {
    /// Any statement.
    body: IndexRange,
};

/// The grammar production a parameter list comes from. It decides whether
/// duplicate parameter names are an error.
///
/// `formal_parameters` belongs to a plain function, `unique_formal_parameters`
/// to a generator, async function, or method, `arrow_formal_parameters` to an
/// arrow function, and `signature` to a TypeScript function type or signature.
pub const FormalParameterKind = enum {
    formal_parameters,
    unique_formal_parameters,
    arrow_formal_parameters,
    signature,
};

/// The parameter list of a function or signature.
pub const FormalParameters = struct {
    /// `formal_parameter`, or `ts_parameter_property` in constructors.
    items: IndexRange,
    /// `binding_rest_element`. `.null` when absent.
    rest: NodeIndex,
    kind: FormalParameterKind,
};

/// A single parameter. Its decorators, type annotation, and optional flag
/// live on `pattern`.
pub const FormalParameter = struct {
    /// Any binding pattern or `ts_this_parameter`.
    pattern: NodeIndex,
};

/// An expression in parentheses.
pub const ParenthesizedExpression = struct {
    /// Any expression.
    expression: NodeIndex,
};

/// An arrow function.
pub const ArrowFunctionExpression = struct {
    /// True for a concise body such as `() => x`.
    expression: bool,
    async: bool,
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// `formal_parameters`.
    params: NodeIndex,
    /// `ts_type_annotation`. `.null` when absent.
    return_type: NodeIndex = .null,
    /// `function_body`, or any expression when `expression` is set.
    body: NodeIndex,
};

/// A comma-separated sequence of expressions.
pub const SequenceExpression = struct {
    /// Any expression.
    expressions: IndexRange,
};

/// A property access such as `a.b`, `a[b]`, or `a?.b`.
pub const MemberExpression = struct {
    /// Any expression.
    object: NodeIndex,
    /// `identifier_name` or `private_identifier`. Any expression when
    /// `computed`.
    property: NodeIndex,
    computed: bool,
    /// True when this link is written with `?.`.
    optional: bool,
};

/// A function call.
pub const CallExpression = struct {
    /// Any expression.
    callee: NodeIndex,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
    /// Any expression or `spread_element`.
    arguments: IndexRange,
    /// True when this link is written with `?.`.
    optional: bool,
};

/// Wraps a whole optional chain such as `a?.b.c`, so short-circuiting covers
/// every link.
pub const ChainExpression = struct {
    /// `member_expression`, `call_expression`, or `ts_non_null_expression`.
    expression: NodeIndex,
};

/// A tagged template expression.
pub const TaggedTemplateExpression = struct {
    /// Any expression.
    tag: NodeIndex,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
    /// `template_literal`.
    quasi: NodeIndex,
};

/// A `new` expression.
pub const NewExpression = struct {
    /// Any expression.
    callee: NodeIndex,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
    /// Any expression or `spread_element`.
    arguments: IndexRange,
};

/// An `await` expression.
pub const AwaitExpression = struct {
    /// Any expression.
    argument: NodeIndex,
};

/// A `yield` or `yield*` expression.
pub const YieldExpression = struct {
    /// Any expression. `.null` when absent.
    argument: NodeIndex,
    /// True for `yield*`.
    delegate: bool,
};

/// The `import.meta` or `new.target` meta property.
pub const MetaProperty = struct {
    /// `identifier_name`.
    meta: NodeIndex,
    /// `identifier_name`.
    property: NodeIndex,
};

/// Whether a TypeScript import or export is type-only, as in `import type`
/// or `export { type T }`.
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

/// Phase of an `import source` or `import defer` import.
///
/// See [source phase imports](https://github.com/tc39/proposal-source-phase-imports)
/// and [deferred import evaluation](https://github.com/tc39/proposal-defer-import-eval).
pub const ImportPhase = enum {
    source,
    @"defer",
};

/// A dynamic `import()`, `import.source()`, or `import.defer()` call.
pub const ImportExpression = struct {
    /// Any expression.
    source: NodeIndex,
    /// Any expression. `.null` when absent.
    options: NodeIndex,
    phase: ?ImportPhase,
};

/// A static `import` declaration.
pub const ImportDeclaration = struct {
    /// `import_specifier`, `import_default_specifier`, or
    /// `import_namespace_specifier`.
    specifiers: IndexRange,
    /// `string_literal`.
    source: NodeIndex,
    /// `import_attribute`.
    attributes: IndexRange,
    phase: ?ImportPhase,
    import_kind: ImportOrExportKind = .value,
};

/// A named `imported as local` import specifier.
pub const ImportSpecifier = struct {
    /// `identifier_name` or `string_literal`.
    imported: NodeIndex,
    /// `binding_identifier`.
    local: NodeIndex,
    import_kind: ImportOrExportKind = .value,
};

/// The default binding of an import declaration.
pub const ImportDefaultSpecifier = struct {
    /// `binding_identifier`.
    local: NodeIndex,
};

/// A `* as local` namespace import specifier.
pub const ImportNamespaceSpecifier = struct {
    /// `binding_identifier`.
    local: NodeIndex,
};

/// A `key: value` entry of a `with { ... }` clause.
pub const ImportAttribute = struct {
    /// `identifier_name` or `string_literal`.
    key: NodeIndex,
    /// `string_literal`.
    value: NodeIndex,
};

/// An `export { ... }` list or an exported declaration.
pub const ExportNamedDeclaration = struct {
    /// `variable_declaration`, `function`, `class`, or a TypeScript
    /// declaration. `.null` for the `export { ... }` form.
    declaration: NodeIndex,
    /// `export_specifier`.
    specifiers: IndexRange,
    /// `string_literal`. `.null` when absent.
    source: NodeIndex,
    /// `import_attribute`.
    attributes: IndexRange,
    /// `.type` for `export type { ... }` and for exported interfaces, type
    /// aliases, and `declare` declarations.
    export_kind: ImportOrExportKind = .value,
};

/// An `export default` declaration.
pub const ExportDefaultDeclaration = struct {
    /// `function`, `class`, `ts_interface_declaration`, or any expression.
    declaration: NodeIndex,
};

/// An `export * from` declaration, with an optional `as name`.
pub const ExportAllDeclaration = struct {
    /// `identifier_name` or `string_literal`. `.null` when absent.
    exported: NodeIndex,
    /// `string_literal`.
    source: NodeIndex,
    /// `import_attribute`.
    attributes: IndexRange,
    export_kind: ImportOrExportKind = .value,
};

/// A `local as exported` export specifier.
pub const ExportSpecifier = struct {
    /// `identifier_reference`, or `identifier_name` or `string_literal` when
    /// re-exporting from another module.
    local: NodeIndex,
    /// `identifier_name` or `string_literal`.
    exported: NodeIndex,
    export_kind: ImportOrExportKind = .value,
};

/// A `: Type` annotation. Its span starts at the `:`.
pub const TSTypeAnnotation = struct {
    /// Any TS type.
    type_annotation: NodeIndex,
};

/// The `any` type.
pub const TSAnyKeyword = struct {};

/// The `unknown` type.
pub const TSUnknownKeyword = struct {};

/// The `never` type.
pub const TSNeverKeyword = struct {};

/// The `void` type.
pub const TSVoidKeyword = struct {};

/// The `null` type.
pub const TSNullKeyword = struct {};

/// The `undefined` type.
pub const TSUndefinedKeyword = struct {};

/// The `string` type.
pub const TSStringKeyword = struct {};

/// The `number` type.
pub const TSNumberKeyword = struct {};

/// The `bigint` type.
pub const TSBigIntKeyword = struct {};

/// The `boolean` type.
pub const TSBooleanKeyword = struct {};

/// The `symbol` type.
pub const TSSymbolKeyword = struct {};

/// The `object` type.
pub const TSObjectKeyword = struct {};

/// The `intrinsic` keyword type.
pub const TSIntrinsicKeyword = struct {};

/// The polymorphic `this` type.
pub const TSThisType = struct {};

/// A named type reference such as `Foo` or `Promise<T>`.
pub const TSTypeReference = struct {
    /// `identifier_reference`, `ts_qualified_name`, or `this_expression`.
    type_name: NodeIndex,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
};

/// A dotted name such as `A.B.C`, nested as `(A.B).C`.
pub const TSQualifiedName = struct {
    /// `identifier_reference`, `identifier_name`, `binding_identifier`,
    /// `this_expression`, or `ts_qualified_name`.
    left: NodeIndex,
    /// `identifier_name`.
    right: NodeIndex,
};

/// A `typeof` type query such as `typeof a.b`.
pub const TSTypeQuery = struct {
    /// `identifier_reference`, `this_expression`, `ts_qualified_name`, or
    /// `ts_import_type`.
    expr_name: NodeIndex,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
};

/// An import type such as `import("m").A.B<T>`.
pub const TSImportType = struct {
    /// `string_literal`.
    source: NodeIndex,
    /// `object_expression`. `.null` when absent.
    options: NodeIndex = .null,
    /// `identifier_name`, or a `ts_qualified_name` of `identifier_name`s.
    /// `.null` when absent.
    qualifier: NodeIndex = .null,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
};

/// A type parameter such as `const T extends U = V`.
pub const TSTypeParameter = struct {
    /// `binding_identifier`.
    name: NodeIndex,
    /// Any TS type. `.null` when absent.
    constraint: NodeIndex = .null,
    /// Any TS type. `.null` when absent.
    default: NodeIndex = .null,
    in: bool = false,
    out: bool = false,
    @"const": bool = false,
};

/// A `<T, U>` type parameter list.
pub const TSTypeParameterDeclaration = struct {
    /// `ts_type_parameter`.
    params: IndexRange,
};

/// A `<A, B>` type argument list.
pub const TSTypeParameterInstantiation = struct {
    /// Any TS type.
    params: IndexRange,
};

/// A literal used as a type. Templates with interpolations use
/// `ts_template_literal_type` and `null` uses `ts_null_keyword`.
pub const TSLiteralType = struct {
    /// `string_literal`, `numeric_literal`, `bigint_literal`, `boolean_literal`,
    /// `template_literal`, or a `unary_expression` for a signed number such as
    /// `-1`.
    literal: NodeIndex,
};

/// A template literal type with at least one interpolation.
pub const TSTemplateLiteralType = struct {
    /// `template_element`. Always `types.len + 1` entries.
    quasis: IndexRange,
    /// Any TS type.
    types: IndexRange,
};

/// An array type such as `T[]`.
pub const TSArrayType = struct {
    /// Any TS type.
    element_type: NodeIndex,
};

/// An indexed access type such as `T[K]`.
pub const TSIndexedAccessType = struct {
    /// Any TS type.
    object_type: NodeIndex,
    /// Any TS type.
    index_type: NodeIndex,
};

/// A tuple type such as `[A, B?, ...C[]]`.
pub const TSTupleType = struct {
    /// Any TS type, `ts_optional_type`, `ts_rest_type`, or
    /// `ts_named_tuple_member`.
    element_types: IndexRange,
};

/// A labeled tuple element such as `label: T` or `label?: T`. A labeled rest
/// element `...label: T` is wrapped in a `ts_rest_type`.
pub const TSNamedTupleMember = struct {
    /// `identifier_name`.
    label: NodeIndex,
    /// Any TS type.
    element_type: NodeIndex,
    optional: bool = false,
};

/// An optional unlabeled tuple element such as `T?`. Labeled elements use
/// `TSNamedTupleMember.optional` instead.
pub const TSOptionalType = struct {
    /// Any TS type.
    type_annotation: NodeIndex,
};

/// A rest tuple element such as `...T[]`.
pub const TSRestType = struct {
    /// Any TS type or `ts_named_tuple_member`.
    type_annotation: NodeIndex,
};

/// A JSDoc nullable type, `?T` or `T?`.
pub const TSJSDocNullableType = struct {
    /// Any TS type.
    type_annotation: NodeIndex,
    /// True for `T?`, false for `?T`.
    postfix: bool = false,
};

/// A JSDoc non-nullable type, `!T` or `T!`.
pub const TSJSDocNonNullableType = struct {
    /// Any TS type.
    type_annotation: NodeIndex,
    /// True for `T!`, false for `!T`.
    postfix: bool = false,
};

/// A JSDoc unknown type, a bare `?` as in `Foo<?>`.
pub const TSJSDocUnknownType = struct {};

/// A union type such as `A | B`. A leading `|` is part of the span, so
/// `type A = | B` gives a single-member union.
pub const TSUnionType = struct {
    /// Any TS type.
    types: IndexRange,
};

/// An intersection type such as `A & B`.
pub const TSIntersectionType = struct {
    /// Any TS type.
    types: IndexRange,
};

/// A conditional type such as `T extends U ? X : Y`.
pub const TSConditionalType = struct {
    /// Any TS type.
    check_type: NodeIndex,
    /// Any TS type.
    extends_type: NodeIndex,
    /// Any TS type.
    true_type: NodeIndex,
    /// Any TS type.
    false_type: NodeIndex,
};

/// An `infer T` type inside the `extends` clause of a conditional type.
pub const TSInferType = struct {
    /// `ts_type_parameter`, which holds any `extends` constraint.
    type_parameter: NodeIndex,
};

/// Operator of a `ts_type_operator`.
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

/// A `keyof`, `unique`, or `readonly` type operator.
pub const TSTypeOperator = struct {
    operator: TSTypeOperatorKind,
    /// Any TS type.
    type_annotation: NodeIndex,
};

/// A type in parentheses.
pub const TSParenthesizedType = struct {
    /// Any TS type.
    type_annotation: NodeIndex,
};

/// A function type such as `(x: T) => U`.
pub const TSFunctionType = struct {
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// `formal_parameters`.
    params: NodeIndex,
    /// `ts_type_annotation` whose span starts at the `=>`.
    return_type: NodeIndex,
};

/// A constructor type such as `new (x: T) => U`.
pub const TSConstructorType = struct {
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// `formal_parameters`.
    params: NodeIndex,
    /// `ts_type_annotation` whose span starts at the `=>`.
    return_type: NodeIndex,
    abstract: bool = false,
};

/// A type predicate such as `x is T`, `asserts x`, or `asserts this is T`.
pub const TSTypePredicate = struct {
    /// `identifier_name` or `ts_this_type`.
    parameter_name: NodeIndex,
    /// `ts_type_annotation` whose span equals the inner type's. `.null` for
    /// `asserts x`.
    type_annotation: NodeIndex = .null,
    asserts: bool = false,
};

/// An object type literal such as `{ a: T }`.
pub const TSTypeLiteral = struct {
    /// `ts_property_signature`, `ts_method_signature`,
    /// `ts_call_signature_declaration`, `ts_construct_signature_declaration`,
    /// or `ts_index_signature`.
    members: IndexRange,
};

/// A `?` or `readonly` modifier of a `ts_mapped_type`. `.true` is the bare
/// modifier, `.plus` and `.minus` are its `+` and `-` forms, and `.none`
/// means it is absent.
pub const TSMappedTypeModifier = enum(u2) {
    none,
    true,
    plus,
    minus,
};

/// A mapped type such as `{ readonly [K in T as N]?: V }`.
pub const TSMappedType = struct {
    /// `binding_identifier` for `K`.
    key: NodeIndex,
    /// Any TS type after `in`.
    constraint: NodeIndex,
    /// Any TS type after `as`. `.null` when absent.
    name_type: NodeIndex = .null,
    /// Any TS type. `.null` when absent.
    type_annotation: NodeIndex = .null,
    optional: TSMappedTypeModifier = .none,
    readonly: TSMappedTypeModifier = .none,
};

/// A property signature in a type literal or interface.
pub const TSPropertySignature = struct {
    /// `identifier_name`, `string_literal`, `numeric_literal`, or
    /// `bigint_literal`. Any expression when `computed`.
    key: NodeIndex,
    /// `ts_type_annotation`. `.null` when absent.
    type_annotation: NodeIndex = .null,
    computed: bool = false,
    optional: bool = false,
    readonly: bool = false,
};

/// Kind of a `ts_method_signature`.
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

/// A method, getter, or setter signature in a type literal or interface.
pub const TSMethodSignature = struct {
    /// `identifier_name`, `string_literal`, `numeric_literal`, or
    /// `bigint_literal`. Any expression when `computed`.
    key: NodeIndex,
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// `formal_parameters`.
    params: NodeIndex,
    /// `ts_type_annotation`. `.null` when absent.
    return_type: NodeIndex = .null,
    kind: TSMethodSignatureKind = .method,
    computed: bool = false,
    optional: bool = false,
};

/// A call signature such as `(x: T): U` in a type literal or interface.
pub const TSCallSignatureDeclaration = struct {
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// `formal_parameters`.
    params: NodeIndex,
    /// `ts_type_annotation`. `.null` when absent.
    return_type: NodeIndex = .null,
};

/// A construct signature such as `new (x: T): U` in a type literal or
/// interface.
pub const TSConstructSignatureDeclaration = struct {
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// `formal_parameters`.
    params: NodeIndex,
    /// `ts_type_annotation`. `.null` when absent.
    return_type: NodeIndex = .null,
};

/// An index signature such as `[key: K]: V` in a type literal, interface, or
/// class body.
pub const TSIndexSignature = struct {
    /// `binding_identifier`, each with its own type annotation.
    parameters: IndexRange,
    /// `ts_type_annotation`.
    type_annotation: NodeIndex,
    readonly: bool = false,
    static: bool = false,
};

/// A `type` alias declaration.
pub const TSTypeAliasDeclaration = struct {
    /// `binding_identifier`.
    id: NodeIndex,
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// Any TS type.
    type_annotation: NodeIndex,
    declare: bool = false,
};

/// An `interface` declaration.
pub const TSInterfaceDeclaration = struct {
    /// `binding_identifier`.
    id: NodeIndex,
    /// `ts_type_parameter_declaration`. `.null` when absent.
    type_parameters: NodeIndex = .null,
    /// `ts_interface_heritage`.
    extends: IndexRange = .empty,
    /// `ts_interface_body`.
    body: NodeIndex,
    declare: bool = false,
};

/// The `{ ... }` body of an interface.
pub const TSInterfaceBody = struct {
    /// `ts_property_signature`, `ts_method_signature`,
    /// `ts_call_signature_declaration`, `ts_construct_signature_declaration`,
    /// or `ts_index_signature`.
    body: IndexRange,
};

/// An entry of an interface `extends` clause.
pub const TSInterfaceHeritage = struct {
    /// `identifier_reference`, `this_expression`, or a `member_expression`
    /// chain of names.
    expression: NodeIndex,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
};

/// An entry of a class `implements` clause.
pub const TSClassImplements = struct {
    /// `identifier_reference`, `this_expression`, or a `member_expression`
    /// chain of names.
    expression: NodeIndex,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
};

/// An `enum` declaration.
pub const TSEnumDeclaration = struct {
    /// `binding_identifier`.
    id: NodeIndex,
    /// `ts_enum_body`.
    body: NodeIndex,
    is_const: bool = false,
    declare: bool = false,
};

/// The `{ ... }` body of an enum.
pub const TSEnumBody = struct {
    /// `ts_enum_member`.
    members: IndexRange,
};

/// A member of an enum.
pub const TSEnumMember = struct {
    /// `identifier_name`, `string_literal`, or `template_literal`.
    id: NodeIndex,
    /// Any expression. `.null` when absent.
    initializer: NodeIndex = .null,
    computed: bool = false,
};

/// Keyword of a `ts_module_declaration`.
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

/// A `namespace` or `module` declaration.
pub const TSModuleDeclaration = struct {
    /// `binding_identifier`, `string_literal`, or `ts_qualified_name`.
    id: NodeIndex,
    /// `ts_module_block`. `.null` for a body-less `declare module "m";`.
    body: NodeIndex = .null,
    kind: TSModuleDeclarationKind,
    declare: bool = false,
};

/// The `{ ... }` body of a `namespace`, `module`, or `global` declaration.
pub const TSModuleBlock = struct {
    /// Any statement.
    body: IndexRange,
};

/// A `global { ... }` augmentation, usually written as `declare global`.
pub const TSGlobalDeclaration = struct {
    /// `identifier_name` for the `global` keyword.
    id: NodeIndex,
    /// `ts_module_block`.
    body: NodeIndex,
    /// False for a bare `global { ... }` nested in an ambient namespace.
    declare: bool = false,
};

/// A constructor parameter with an accessibility, `readonly`, or `override`
/// modifier, which also declares a class field.
pub const TSParameterProperty = struct {
    /// `decorator`.
    decorators: IndexRange,
    /// `binding_identifier` or `assignment_pattern`.
    parameter: NodeIndex,
    override: bool = false,
    readonly: bool = false,
    accessibility: Accessibility = .none,
};

/// An explicit `this` parameter, held by a `formal_parameter`.
pub const TSThisParameter = struct {
    /// `ts_type_annotation`. `.null` when absent.
    type_annotation: NodeIndex = .null,
};

/// An `expr as T` expression.
pub const TSAsExpression = struct {
    /// Any expression.
    expression: NodeIndex,
    /// Any TS type.
    type_annotation: NodeIndex,
};

/// An `expr satisfies T` expression.
pub const TSSatisfiesExpression = struct {
    /// Any expression.
    expression: NodeIndex,
    /// Any TS type.
    type_annotation: NodeIndex,
};

/// A `<T>expr` type assertion.
pub const TSTypeAssertion = struct {
    /// Any TS type.
    type_annotation: NodeIndex,
    /// Any expression.
    expression: NodeIndex,
};

/// An `expr!` non-null assertion.
pub const TSNonNullExpression = struct {
    /// Any expression.
    expression: NodeIndex,
};

/// An `expr<T>` instantiation expression without a call.
pub const TSInstantiationExpression = struct {
    /// Any expression.
    expression: NodeIndex,
    /// `ts_type_parameter_instantiation`.
    type_arguments: NodeIndex,
};

/// An `export = expr` declaration.
pub const TSExportAssignment = struct {
    /// Any expression.
    expression: NodeIndex,
};

/// An `export as namespace Name` declaration.
pub const TSNamespaceExportDeclaration = struct {
    /// `identifier_name`.
    id: NodeIndex,
};

/// An `import x = require("m")` or `import x = A.B` declaration.
pub const TSImportEqualsDeclaration = struct {
    /// `binding_identifier`.
    id: NodeIndex,
    /// `ts_external_module_reference`, `identifier_reference`, or
    /// `ts_qualified_name`.
    module_reference: NodeIndex,
    import_kind: ImportOrExportKind = .value,
};

/// The `require("m")` of an `import x = require("m")` declaration.
pub const TSExternalModuleReference = struct {
    /// `string_literal`.
    expression: NodeIndex,
};

/// A JSX element.
pub const JSXElement = struct {
    /// `jsx_opening_element`.
    opening_element: NodeIndex,
    /// `jsx_text`, `jsx_expression_container`, `jsx_spread_child`,
    /// `jsx_element`, or `jsx_fragment`.
    children: IndexRange,
    /// `jsx_closing_element`. `.null` when self-closing.
    closing_element: NodeIndex,
};

/// The opening tag of a JSX element, such as `<Foo a={b}>` or `<Foo />`.
pub const JSXOpeningElement = struct {
    /// `jsx_identifier`, `jsx_namespaced_name`, or `jsx_member_expression`.
    name: NodeIndex,
    /// `ts_type_parameter_instantiation`. `.null` when absent.
    type_arguments: NodeIndex = .null,
    /// `jsx_attribute` or `jsx_spread_attribute`.
    attributes: IndexRange,
    self_closing: bool,
};

/// The closing tag of a JSX element, such as `</Foo>`.
pub const JSXClosingElement = struct {
    /// `jsx_identifier`, `jsx_namespaced_name`, or `jsx_member_expression`.
    name: NodeIndex,
};

/// A JSX fragment `<>...</>`.
pub const JSXFragment = struct {
    /// `jsx_opening_fragment`.
    opening_fragment: NodeIndex,
    /// `jsx_text`, `jsx_expression_container`, `jsx_spread_child`,
    /// `jsx_element`, or `jsx_fragment`.
    children: IndexRange,
    /// `jsx_closing_fragment`.
    closing_fragment: NodeIndex,
};

/// The `<>` of a JSX fragment.
pub const JSXOpeningFragment = struct {};

/// The `</>` of a JSX fragment.
pub const JSXClosingFragment = struct {};

/// An identifier in a JSX tag or attribute name.
pub const JSXIdentifier = struct {
    name: String = .empty,
};

/// A JSX name of the form `namespace:name`.
pub const JSXNamespacedName = struct {
    /// `jsx_identifier`.
    namespace: NodeIndex,
    /// `jsx_identifier`.
    name: NodeIndex,
};

/// A dotted JSX tag name such as `Foo.Bar`.
pub const JSXMemberExpression = struct {
    /// `jsx_identifier` or `jsx_member_expression`.
    object: NodeIndex,
    /// `jsx_identifier`.
    property: NodeIndex,
};

/// A JSX attribute such as `a="b"` or `disabled`.
pub const JSXAttribute = struct {
    /// `jsx_identifier` or `jsx_namespaced_name`.
    name: NodeIndex,
    /// `string_literal`, `jsx_expression_container`, `jsx_element`, or
    /// `jsx_fragment`. `.null` when absent.
    value: NodeIndex,
};

/// A `{...props}` spread attribute.
pub const JSXSpreadAttribute = struct {
    /// Any expression.
    argument: NodeIndex,
};

/// A `{expression}` container in a JSX attribute value or child.
pub const JSXExpressionContainer = struct {
    /// Any expression, or `jsx_empty_expression` for `{}`.
    expression: NodeIndex,
};

/// The empty expression inside a `{}` container.
pub const JSXEmptyExpression = struct {};

/// Raw text inside a JSX element or fragment.
pub const JSXText = struct {
    value: String = .empty,
};

/// A `{...children}` spread child.
pub const JSXSpreadChild = struct {
    /// Any expression.
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

    /// True when the node produces a value at runtime. For `function` and
    /// `class`, only the expression forms count.
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
            .function => |f| f.type == .function_expression or
                f.type == .ts_empty_body_function_expression,
            .class => |c| c.type == .class_expression,
            else => false,
        };
    }

    /// True when the node is valid in statement position. For `function` and
    /// `class`, only the declaration forms count.
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

    /// True when the node is a literal value.
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

    /// True for `function` in any form and for `arrow_function_expression`.
    pub fn isCallable(self: NodeData) bool {
        return switch (self) {
            .function,
            .arrow_function_expression,
            => true,
            else => false,
        };
    }

    /// True for `binding_identifier`, `array_pattern`, `object_pattern`, and
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

    /// True for declarations, including imports, exports, and TypeScript
    /// declarations. For `function` and `class`, only the declaration forms
    /// count.
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

    /// True for `for`, `for-in`, `for-of`, `while`, and `do-while` loops.
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

    const type_context_tags = [_]std.meta.Tag(NodeData){
        .ts_type_annotation,
        .ts_type_reference,
        .ts_qualified_name,
        .ts_type_query,
        .ts_import_type,
        .ts_type_parameter,
        .ts_type_parameter_declaration,
        .ts_type_parameter_instantiation,
        .ts_literal_type,
        .ts_template_literal_type,
        .ts_array_type,
        .ts_indexed_access_type,
        .ts_tuple_type,
        .ts_named_tuple_member,
        .ts_optional_type,
        .ts_rest_type,
        .ts_jsdoc_nullable_type,
        .ts_jsdoc_non_nullable_type,
        .ts_jsdoc_unknown_type,
        .ts_union_type,
        .ts_intersection_type,
        .ts_conditional_type,
        .ts_infer_type,
        .ts_type_operator,
        .ts_parenthesized_type,
        .ts_function_type,
        .ts_constructor_type,
        .ts_type_predicate,
        .ts_type_literal,
        .ts_property_signature,
        .ts_method_signature,
        .ts_call_signature_declaration,
        .ts_construct_signature_declaration,
        .ts_index_signature,
        .ts_mapped_type,
        .ts_class_implements,
        .ts_interface_heritage,
        .ts_interface_body,
    };

    const type_context_set = std.EnumSet(std.meta.Tag(NodeData)).initMany(&type_context_tags);

    pub fn isTypeContext(self: NodeData) bool {
        return type_context_set.contains(self);
    }
};

pub const Node = struct {
    data: NodeData,
    span: Span,
};

pub const NodeList = std.MultiArrayList(Node);

comptime {
    std.debug.assert(@sizeOf(NodeData) == 44);
    std.debug.assert(@sizeOf(Node) == 52);
    std.debug.assert(@sizeOf(Class) == 40);
    std.debug.assert(@sizeOf(PropertyDefinition) == 32);
}
