const std = @import("std");

pub const Mask = struct {
    pub const IsNumericLiteral: u32 = 1 << 13;
    pub const IsBinaryOp: u32 = 1 << 14;
    pub const IsLogicalOp: u32 = 1 << 15;
    pub const IsUnaryOp: u32 = 1 << 16;
    pub const IsAssignmentOp: u32 = 1 << 17;
    pub const IsIdentifierLike: u32 = 1 << 18;
    /// reserved words that are always reserved
    pub const IsUnconditionallyReserved: u32 = 1 << 19;
    /// reserved words that are only reserved in strict mode
    pub const IsStrictModeReserved: u32 = 1 << 20;
    pub const IsKeyword: u32 = 1 << 21;

    pub const PrecShift: u32 = 8;
    pub const PrecOverlap: u32 = 0b11111;
};

pub const TokenTag = enum(u32) {
    // literals
    numeric_literal = 1 | Mask.IsNumericLiteral, // e.g., "123", "45.67"
    hex_literal = 2 | Mask.IsNumericLiteral, // e.g., "0xFF", "0x1A"
    /// modern octal literal
    octal_literal = 3 | Mask.IsNumericLiteral, // e.g., "0o777", "0o12"
    binary_literal = 4 | Mask.IsNumericLiteral, // e.g., "0b1010", "0b11"
    bigint_literal = 5 | Mask.IsNumericLiteral, // e.g., "123n", "456n"
    string_literal = 6, // e.g., "'hello'", "\"world\""
    regex_literal = 7, // e.g., "/abc/g", "/[0-9]+/i"
    no_substitution_template = 8 | (17 << Mask.PrecShift), // e.g., "`hello`"
    template_head = 9 | (17 << Mask.PrecShift), // e.g., "`hello ${"
    template_middle = 10, // e.g., "} world ${"
    template_tail = 11, // e.g., "} end`"

    // keyword literals
    true = 12 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "true"
    false = 13 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "false"
    null_literal = 14 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "null"

    // arithmetic operators
    plus = 15 | (11 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnaryOp, // "+"
    minus = 16 | (11 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnaryOp, // "-"
    star = 17 | (12 << Mask.PrecShift) | Mask.IsBinaryOp, // "*"
    slash = 18 | (12 << Mask.PrecShift) | Mask.IsBinaryOp, // "/"
    percent = 19 | (12 << Mask.PrecShift) | Mask.IsBinaryOp, // "%"
    exponent = 20 | (13 << Mask.PrecShift) | Mask.IsBinaryOp, // "**"

    // assignment operators
    assign = 21 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "="
    plus_assign = 22 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "+="
    minus_assign = 23 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "-="
    star_assign = 24 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "*="
    slash_assign = 25 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "/="
    percent_assign = 26 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "%="
    exponent_assign = 27 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "**="

    // update operators
    increment = 28 | (15 << Mask.PrecShift), // "++"
    decrement = 29 | (15 << Mask.PrecShift), // "--"

    // equality and relational operators
    equal = 30 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "=="
    not_equal = 31 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "!="
    strict_equal = 32 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "==="
    strict_not_equal = 33 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "!=="
    less_than = 34 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // "<"
    greater_than = 35 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // ">"
    less_than_equal = 36 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // "<="
    greater_than_equal = 37 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // ">="

    // logical operators
    logical_and = 38 | (4 << Mask.PrecShift) | Mask.IsLogicalOp, // "&&"
    logical_or = 39 | (3 << Mask.PrecShift) | Mask.IsLogicalOp, // "||"
    logical_not = 40 | (14 << Mask.PrecShift) | Mask.IsUnaryOp, // "!"

    // bitwise operators
    bitwise_and = 41 | (7 << Mask.PrecShift) | Mask.IsBinaryOp, // "&"
    bitwise_or = 42 | (5 << Mask.PrecShift) | Mask.IsBinaryOp, // "|"
    bitwise_xor = 43 | (6 << Mask.PrecShift) | Mask.IsBinaryOp, // "^"
    bitwise_not = 44 | Mask.IsUnaryOp, // "~"
    left_shift = 45 | (10 << Mask.PrecShift) | Mask.IsBinaryOp, // "<<"
    right_shift = 46 | (10 << Mask.PrecShift) | Mask.IsBinaryOp, // ">>"
    unsigned_right_shift = 47 | (10 << Mask.PrecShift) | Mask.IsBinaryOp, // ">>>"

    // compound bitwise assignment
    bitwise_and_assign = 48 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "&="
    bitwise_or_assign = 49 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "|="
    bitwise_xor_assign = 50 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "^="
    left_shift_assign = 51 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "<<="
    right_shift_assign = 52 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // ">>="
    unsigned_right_shift_assign = 53 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // ">>>="

    // nullish and optional
    nullish_coalescing = 54 | (3 << Mask.PrecShift) | Mask.IsLogicalOp, // "??"
    nullish_assign = 55 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "??="
    logical_and_assign = 56 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "&&="
    logical_or_assign = 57 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "||="
    optional_chaining = 58 | (17 << Mask.PrecShift), // "?."

    // punctuation
    left_paren = 59 | (17 << Mask.PrecShift), // "("
    right_paren = 60, // ")"
    left_brace = 61, // "{"
    right_brace = 62, // "}"
    left_bracket = 63 | (17 << Mask.PrecShift), // "["
    right_bracket = 64, // "]"
    semicolon = 65, // ";"
    comma = 66 | (1 << Mask.PrecShift), // ","
    dot = 67 | (17 << Mask.PrecShift), // "."
    spread = 68, // "..."
    arrow = 69 | (2 << Mask.PrecShift), // "=>"
    question = 70 | (2 << Mask.PrecShift), // "?"
    colon = 71, // ":"
    at = 72, // "@"

    // reserved keywords
    @"if" = 73 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "if"
    @"else" = 74 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "else"
    @"switch" = 75 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "switch"
    case = 76 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "case"
    default = 77 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "default"
    @"for" = 78 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "for"
    @"while" = 79 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "while"
    do = 80 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "do"
    @"break" = 81 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "break"
    @"continue" = 82 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "continue"

    function = 83 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "function"
    @"return" = 84 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "return"
    async = 85 | Mask.IsKeyword | Mask.IsIdentifierLike, // "async"
    await = 86 | Mask.IsKeyword | Mask.IsIdentifierLike, // "await"
    yield = 87 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "yield"

    @"var" = 88 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "var"
    let = 89 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "let"
    @"const" = 90 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "const"
    using = 91 | Mask.IsKeyword | Mask.IsIdentifierLike, // "using"

    class = 92 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "class"
    extends = 93 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "extends"
    super = 94 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "super"
    static = 95 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "static"
    @"enum" = 96 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "enum"
    public = 97 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "public"
    private = 98 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "private"
    protected = 99 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "protected"
    interface = 100 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "interface"
    implements = 101 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "implements"
    package = 102 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "package"

    import = 103 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "import"
    @"export" = 104 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "export"
    from = 105 | Mask.IsKeyword | Mask.IsIdentifierLike, // "from"
    as = 106 | (9 << Mask.PrecShift) | Mask.IsKeyword | Mask.IsIdentifierLike, // "as"
    namespace = 107 | Mask.IsKeyword | Mask.IsIdentifierLike, // "namespace"
    assert = 108 | Mask.IsKeyword | Mask.IsIdentifierLike, // "assert" (import assertions)
    source = 109 | Mask.IsKeyword | Mask.IsIdentifierLike, // "source" (source phase imports)
    @"defer" = 110 | Mask.IsKeyword | Mask.IsIdentifierLike, // "defer" (deferred imports)

    @"try" = 111 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "try"
    @"catch" = 112 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "catch"
    finally = 113 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "finally"
    throw = 114 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "throw"

    new = 115 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "new"
    this = 116 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "this"
    typeof = 117 | Mask.IsUnaryOp | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "typeof"
    instanceof = 118 | (9 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "instanceof"
    in = 119 | (9 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "in"
    of = 120 | Mask.IsKeyword | Mask.IsIdentifierLike, // "of"
    delete = 121 | Mask.IsUnaryOp | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "delete"
    void = 122 | Mask.IsUnaryOp | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "void"
    with = 123 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "with"
    debugger = 124 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "debugger"

    // identifiers
    identifier = 125 | Mask.IsIdentifierLike, // e.g., "myVar", "foo", "_bar"
    private_identifier = 126, // e.g., "#privateField", "#method"

    // contextual keywords (class/object bodies)
    get = 127 | Mask.IsKeyword | Mask.IsIdentifierLike, // "get"
    set = 128 | Mask.IsKeyword | Mask.IsIdentifierLike, // "set"
    accessor = 129 | Mask.IsIdentifierLike, // "accessor"
    constructor = 130 | Mask.IsIdentifierLike, // "constructor"

    // ts contextual keywords
    declare = 131 | Mask.IsKeyword | Mask.IsIdentifierLike, // "declare"
    type = 132 | Mask.IsKeyword | Mask.IsIdentifierLike, // "type"
    abstract = 133 | Mask.IsKeyword | Mask.IsIdentifierLike, // "abstract"
    override = 134 | Mask.IsKeyword | Mask.IsIdentifierLike, // "override"
    readonly = 135 | Mask.IsKeyword | Mask.IsIdentifierLike, // "readonly"
    keyof = 136 | Mask.IsKeyword | Mask.IsIdentifierLike, // "keyof"
    unique = 137 | Mask.IsKeyword | Mask.IsIdentifierLike, // "unique"
    infer = 138 | Mask.IsKeyword | Mask.IsIdentifierLike, // "infer"
    out = 139 | Mask.IsKeyword | Mask.IsIdentifierLike, // "out"
    asserts = 140 | Mask.IsKeyword | Mask.IsIdentifierLike, // "asserts"
    satisfies = 141 | (9 << Mask.PrecShift) | Mask.IsKeyword | Mask.IsIdentifierLike, // "satisfies"
    intrinsic = 142 | Mask.IsKeyword | Mask.IsIdentifierLike, // "intrinsic"
    @"is" = 143 | Mask.IsKeyword | Mask.IsIdentifierLike, // "is"
    global = 144 | Mask.IsKeyword | Mask.IsIdentifierLike, // "global"
    module = 145 | Mask.IsKeyword | Mask.IsIdentifierLike, // "module"
    require = 146 | Mask.IsKeyword | Mask.IsIdentifierLike, // "require"

    // ts primitive type keywords
    any = 147 | Mask.IsKeyword | Mask.IsIdentifierLike, // "any"
    bigint = 148 | Mask.IsKeyword | Mask.IsIdentifierLike, // "bigint"
    boolean = 149 | Mask.IsKeyword | Mask.IsIdentifierLike, // "boolean"
    never = 150 | Mask.IsKeyword | Mask.IsIdentifierLike, // "never"
    number = 151 | Mask.IsKeyword | Mask.IsIdentifierLike, // "number"
    object = 152 | Mask.IsKeyword | Mask.IsIdentifierLike, // "object"
    string = 153 | Mask.IsKeyword | Mask.IsIdentifierLike, // "string"
    symbol = 154 | Mask.IsKeyword | Mask.IsIdentifierLike, // "symbol"
    @"undefined" = 155 | Mask.IsKeyword | Mask.IsIdentifierLike, // "undefined"
    unknown = 156 | Mask.IsKeyword | Mask.IsIdentifierLike, // "unknown"

    // jsx
    jsx_identifier = 157,
    jsx_text = 158,

    eof = 159, // end of file

    pub fn precedence(self: TokenTag) u5 {
        return @intCast((@intFromEnum(self) >> Mask.PrecShift) & Mask.PrecOverlap);
    }

    pub fn hasMask(self: TokenTag, mask: u32) bool {
        return (@intFromEnum(self) & mask) != 0;
    }

    pub fn isNumericLiteral(self: TokenTag) bool {
        return self.hasMask(Mask.IsNumericLiteral);
    }

    pub fn isBinaryOperator(self: TokenTag) bool {
        return self.hasMask(Mask.IsBinaryOp);
    }

    pub fn isLogicalOperator(self: TokenTag) bool {
        return self.hasMask(Mask.IsLogicalOp);
    }

    pub fn isUnaryOperator(self: TokenTag) bool {
        return self.hasMask(Mask.IsUnaryOp);
    }

    pub fn isAssignmentOperator(self: TokenTag) bool {
        return self.hasMask(Mask.IsAssignmentOp);
    }

    /// returns true for identifier-like tokens.
    /// includes: identifiers, all keywords, literal keywords.
    pub fn isIdentifierLike(self: TokenTag) bool {
        return self.hasMask(Mask.IsIdentifierLike);
    }

    pub fn isKeyword(self: TokenTag) bool {
        return self.hasMask(Mask.IsKeyword);
    }

    /// returns true for unconditionally reserved keywords.
    /// these can NEVER be used as identifiers.
    pub fn isUnconditionallyReserved(self: TokenTag) bool {
        return self.hasMask(Mask.IsUnconditionallyReserved);
    }

    /// returns true for keywords reserved ONLY in strict mode.
    /// these can be identifiers in sloppy mode but not in strict mode.
    /// includes: let, static, implements, interface, package, private, protected, public, yield
    pub fn isStrictModeReserved(self: TokenTag) bool {
        return self.hasMask(Mask.IsStrictModeReserved);
    }

    /// returns true for any reserved keyword (unconditional or strict-mode-only).
    pub fn isReserved(self: TokenTag) bool {
        return self.hasMask(Mask.IsUnconditionallyReserved) or self.hasMask(Mask.IsStrictModeReserved);
    }

    pub fn toString(self: TokenTag) ?[]const u8 {
        return switch (self) {
            .true => "true",
            .false => "false",
            .null_literal => "null",

            .plus => "+",
            .minus => "-",
            .star => "*",
            .slash => "/",
            .percent => "%",
            .exponent => "**",

            .assign => "=",
            .plus_assign => "+=",
            .minus_assign => "-=",
            .star_assign => "*=",
            .slash_assign => "/=",
            .percent_assign => "%=",
            .exponent_assign => "**=",

            .increment => "++",
            .decrement => "--",

            .equal => "==",
            .not_equal => "!=",
            .strict_equal => "===",
            .strict_not_equal => "!==",
            .less_than => "<",
            .greater_than => ">",
            .less_than_equal => "<=",
            .greater_than_equal => ">=",

            .logical_and => "&&",
            .logical_or => "||",
            .logical_not => "!",

            .bitwise_and => "&",
            .bitwise_or => "|",
            .bitwise_xor => "^",
            .bitwise_not => "~",
            .left_shift => "<<",
            .right_shift => ">>",
            .unsigned_right_shift => ">>>",

            .bitwise_and_assign => "&=",
            .bitwise_or_assign => "|=",
            .bitwise_xor_assign => "^=",
            .left_shift_assign => "<<=",
            .right_shift_assign => ">>=",
            .unsigned_right_shift_assign => ">>>=",

            .nullish_coalescing => "??",
            .nullish_assign => "??=",
            .logical_and_assign => "&&=",
            .logical_or_assign => "||=",
            .optional_chaining => "?.",

            .left_paren => "(",
            .right_paren => ")",
            .left_brace => "{",
            .right_brace => "}",
            .left_bracket => "[",
            .right_bracket => "]",
            .semicolon => ";",
            .comma => ",",
            .dot => ".",
            .spread => "...",
            .arrow => "=>",
            .question => "?",
            .colon => ":",
            .at => "@",

            .@"if" => "if",
            .@"else" => "else",
            .@"switch" => "switch",
            .case => "case",
            .default => "default",
            .@"for" => "for",
            .@"while" => "while",
            .do => "do",
            .@"break" => "break",
            .@"continue" => "continue",

            .function => "function",
            .@"return" => "return",
            .async => "async",
            .await => "await",
            .yield => "yield",

            .@"var" => "var",
            .let => "let",
            .@"const" => "const",
            .using => "using",

            .class => "class",
            .extends => "extends",
            .super => "super",
            .static => "static",
            .@"enum" => "enum",
            .public => "public",
            .private => "private",
            .protected => "protected",
            .interface => "interface",
            .implements => "implements",
            .package => "package",

            .import => "import",
            .@"export" => "export",
            .from => "from",
            .as => "as",
            .namespace => "namespace",
            .assert => "assert",
            .source => "source",
            .@"defer" => "defer",

            .@"try" => "try",
            .@"catch" => "catch",
            .finally => "finally",
            .throw => "throw",

            .new => "new",
            .this => "this",
            .typeof => "typeof",
            .instanceof => "instanceof",
            .in => "in",
            .of => "of",
            .delete => "delete",
            .void => "void",
            .with => "with",
            .debugger => "debugger",

            .declare => "declare",

            .get => "get",
            .set => "set",
            .accessor => "accessor",
            .constructor => "constructor",

            .type => "type",
            .abstract => "abstract",
            .override => "override",
            .readonly => "readonly",
            .keyof => "keyof",
            .unique => "unique",
            .infer => "infer",
            .out => "out",
            .asserts => "asserts",
            .satisfies => "satisfies",
            .intrinsic => "intrinsic",
            .@"is" => "is",
            .global => "global",
            .module => "module",
            .require => "require",

            .any => "any",
            .bigint => "bigint",
            .boolean => "boolean",
            .never => "never",
            .number => "number",
            .object => "object",
            .string => "string",
            .symbol => "symbol",
            .@"undefined" => "undefined",
            .unknown => "unknown",

            .eof,
            .numeric_literal,
            .hex_literal,
            .octal_literal,
            .binary_literal,
            .bigint_literal,
            .string_literal,
            .regex_literal,
            .no_substitution_template,
            .template_head,
            .template_middle,
            .template_tail,
            .identifier,
            .private_identifier,
            .jsx_identifier,
            .jsx_text,
            => null,
        };
    }
};

pub const Span = struct {
    start: u32,
    end: u32,

    pub const none = .{ .start = 0, .end = 0 };
};

pub const TokenFlag = enum(u3) {
    line_terminator_before,
    invalid_escape,
    escaped,
    lone_surrogates,
};

pub inline fn flagMask(comptime flag: TokenFlag) u8 {
    return @as(u8, 1) << @intFromEnum(flag);
}

pub const Token = struct {
    span: Span,
    tag: TokenTag,
    flags: u8 = 0,

    pub inline fn eof(pos: u32) Token {
        return .{ .span = .{ .start = pos, .end = pos }, .tag = .eof };
    }

    pub inline fn len(self: Token) u32 {
        return self.span.end - self.span.start;
    }

    pub inline fn text(self: Token, source: []const u8) []const u8 {
        return source[self.span.start..self.span.end];
    }

    pub inline fn has(self: Token, comptime flag: TokenFlag) bool {
        return (self.flags & flagMask(flag)) != 0;
    }

    /// true when skipped trivia before this token contained a line terminator.
    /// parser uses this for ASI and newline-restricted grammar rules.
    pub inline fn hasLineTerminatorBefore(self: Token) bool {
        return self.has(.line_terminator_before);
    }

    /// true when scanning this token encountered an invalid escape.
    /// currently consumed by template literal parsing (tagged vs untagged behavior).
    pub inline fn hasInvalidEscape(self: Token) bool {
        return self.has(.invalid_escape);
    }

    /// true when token text came from an escaped spelling (for example, identifier/keyword escapes).
    pub inline fn isEscaped(self: Token) bool {
        return self.has(.escaped);
    }

    /// whether the string/template token have lone surrogates
    pub inline fn hasLoneSurrogates(self: Token) bool {
        return self.has(.lone_surrogates);
    }
};

pub const Precedence = struct {
    pub const Lowest: u8 = 0;
    pub const Comma: u8 = 1;
    pub const Assignment: u8 = 2;
    pub const LogicalOr: u8 = 3;
    pub const LogicalAnd: u8 = 4;
    pub const BitwiseOr: u8 = 5;
    pub const BitwiseXor: u8 = 6;
    pub const BitwiseAnd: u8 = 7;
    pub const Equality: u8 = 8;
    pub const Relational: u8 = 9;
    pub const Shift: u8 = 10;
    pub const Additive: u8 = 11;
    pub const Multiplicative: u8 = 12;
    pub const Exponentiation: u8 = 13;
    pub const Unary: u8 = 14;
    pub const Postfix: u8 = 15;
    pub const New: u8 = 16;
    pub const Call: u8 = 17;
    pub const Grouping: u8 = 18;
};

comptime {
    std.debug.assert((@sizeOf(Token) <= 16));
}
