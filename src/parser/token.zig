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

    pub const PrecShift: u32 = 8;
    pub const PrecOverlap: u32 = 0b11111;
};

pub const TokenTag = enum(u32) {
    numeric_literal = 1 | Mask.IsNumericLiteral, // e.g., "123", "45.67"
    hex_literal = 2 | Mask.IsNumericLiteral, // e.g., "0xFF", "0x1A"
    /// modern octal literal
    octal_literal = 3 | Mask.IsNumericLiteral, // e.g., "0o777", "0o12"
    binary_literal = 4 | Mask.IsNumericLiteral, // e.g., "0b1010", "0b11"
    bigint_literal = 5 | Mask.IsNumericLiteral, // e.g., "123n", "456n"

    string_literal = 7, // e.g., "'hello'", "\"world\""
    regex_literal = 8, // e.g., "/abc/g", "/[0-9]+/i"

    no_substitution_template = 9, // e.g., "`hello`"
    template_head = 10, // e.g., "`hello ${"
    template_middle = 11, // e.g., "} world ${"
    template_tail = 12, // e.g., "} end`"

    true = 13 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "true"
    false = 14 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "false"
    null_literal = 15 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "null"

    plus = 16 | (11 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnaryOp, // "+"
    minus = 17 | (11 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnaryOp, // "-"
    star = 18 | (12 << Mask.PrecShift) | Mask.IsBinaryOp, // "*"
    slash = 19 | (12 << Mask.PrecShift) | Mask.IsBinaryOp, // "/"
    percent = 20 | (12 << Mask.PrecShift) | Mask.IsBinaryOp, // "%"
    exponent = 21 | (13 << Mask.PrecShift) | Mask.IsBinaryOp, // "**"

    assign = 22 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "="
    plus_assign = 23 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "+="
    minus_assign = 24 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "-="
    star_assign = 25 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "*="
    slash_assign = 26 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "/="
    percent_assign = 27 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "%="
    exponent_assign = 28 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "**="

    increment = 29 | (15 << Mask.PrecShift), // "++"
    decrement = 30 | (15 << Mask.PrecShift), // "--"

    equal = 31 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "=="
    not_equal = 32 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "!="
    strict_equal = 33 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "==="
    strict_not_equal = 34 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "!=="
    less_than = 35 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // "<"
    greater_than = 36 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // ">"
    less_than_equal = 37 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // "<="
    greater_than_equal = 38 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // ">="

    logical_and = 39 | (4 << Mask.PrecShift) | Mask.IsLogicalOp, // "&&"
    logical_or = 40 | (3 << Mask.PrecShift) | Mask.IsLogicalOp, // "||"
    logical_not = 41 | (14 << Mask.PrecShift) | Mask.IsUnaryOp, // "!"

    bitwise_and = 42 | (7 << Mask.PrecShift) | Mask.IsBinaryOp, // "&"
    bitwise_or = 43 | (5 << Mask.PrecShift) | Mask.IsBinaryOp, // "|"
    bitwise_xor = 44 | (6 << Mask.PrecShift) | Mask.IsBinaryOp, // "^"
    bitwise_not = 45 | (14 << Mask.PrecShift) | Mask.IsUnaryOp, // "~"
    left_shift = 46 | (10 << Mask.PrecShift) | Mask.IsBinaryOp, // "<<"
    right_shift = 47 | (10 << Mask.PrecShift) | Mask.IsBinaryOp, // ">>"
    unsigned_right_shift = 48 | (10 << Mask.PrecShift) | Mask.IsBinaryOp, // ">>>"

    bitwise_and_assign = 49 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "&="
    bitwise_or_assign = 50 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "|="
    bitwise_xor_assign = 51 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "^="
    left_shift_assign = 52 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "<<="
    right_shift_assign = 53 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // ">>="
    unsigned_right_shift_assign = 54 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // ">>>="

    nullish_coalescing = 55 | (3 << Mask.PrecShift) | Mask.IsLogicalOp, // "??"
    nullish_assign = 56 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "??="
    logical_and_assign = 57 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "&&="
    logical_or_assign = 58 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "||="
    optional_chaining = 59, // "?."

    left_paren = 60, // "("
    right_paren = 61, // ")"
    left_brace = 62, // "{"
    right_brace = 63, // "}"
    left_bracket = 64, // "["
    right_bracket = 65, // "]"
    semicolon = 66, // ";"
    comma = 67 | (1 << Mask.PrecShift), // ","
    dot = 68, // "."
    spread = 69, // "..."
    arrow = 70, // "=>"
    question = 71 | (2 << Mask.PrecShift), // "?"
    colon = 72, // ":"
    at = 73, // "@"

    @"if" = 74 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "if"
    @"else" = 75 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "else"
    @"switch" = 76 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "switch"
    case = 77 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "case"
    default = 78 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "default"
    @"for" = 79 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "for"
    @"while" = 80 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "while"
    do = 81 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "do"
    @"break" = 82 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "break"
    @"continue" = 83 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "continue"

    function = 84 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "function"
    @"return" = 85 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "return"
    async = 86 | Mask.IsIdentifierLike, // "async"
    await = 87 | Mask.IsIdentifierLike, // "await"
    yield = 88 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "yield"

    @"var" = 89 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "var"
    let = 90 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "let"
    @"const" = 91 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "const"
    using = 92 | Mask.IsIdentifierLike, // "using"

    class = 93 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "class"
    extends = 94 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "extends"
    super = 95 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "super"
    static = 96 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "static"
    @"enum" = 97 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "enum"
    public = 98 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "public"
    private = 99 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "private"
    protected = 100 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "protected"
    interface = 101 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "interface"
    implements = 102 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "implements"
    package = 103 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "package"

    import = 104 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "import"
    @"export" = 105 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "export"
    from = 106 | Mask.IsIdentifierLike, // "from"
    as = 107 | Mask.IsIdentifierLike, // "as"
    namespace = 108 | Mask.IsIdentifierLike, // "namespace"
    assert = 109 | Mask.IsIdentifierLike, // "assert" (import assertions)
    source = 110 | Mask.IsIdentifierLike, // "source" (source phase imports)
    @"defer" = 111 | Mask.IsIdentifierLike, // "defer" (deferred imports)

    @"try" = 112 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "try"
    @"catch" = 113 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "catch"
    finally = 114 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "finally"
    throw = 115 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "throw"

    new = 116 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "new"
    this = 117 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "this"
    typeof = 118 | (14 << Mask.PrecShift) | Mask.IsUnaryOp | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "typeof"
    instanceof = 119 | (9 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "instanceof"
    in = 120 | (9 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "in"
    of = 121 | Mask.IsIdentifierLike, // "of"
    delete = 122 | (14 << Mask.PrecShift) | Mask.IsUnaryOp | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "delete"
    void = 123 | (14 << Mask.PrecShift) | Mask.IsUnaryOp | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "void"
    with = 124 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "with"
    debugger = 125 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "debugger"

    identifier = 126 | Mask.IsIdentifierLike, // e.g., "myVar", "foo", "_bar"
    private_identifier = 127, // e.g., "#privateField", "#method"

    // typescript
    declare = 128 | Mask.IsIdentifierLike, // "declare"

    // jsx
    jsx_identifier = 129,
    jsx_text = 130,

    // contextual keywords (class/object bodies)
    get = 132 | Mask.IsIdentifierLike, // "get"
    set = 133 | Mask.IsIdentifierLike, // "set"
    accessor = 134 | Mask.IsIdentifierLike, // "accessor"
    constructor = 135 | Mask.IsIdentifierLike, // "constructor"

    eof = 131, // end of file
    pub fn precedence(self: TokenTag) u5 {
        return @intCast((@intFromEnum(self) >> Mask.PrecShift) & Mask.PrecOverlap);
    }

    pub fn is(self: TokenTag, mask: u32) bool {
        return (@intFromEnum(self) & mask) != 0;
    }

    pub fn isNumericLiteral(self: TokenTag) bool {
        return self.is(Mask.IsNumericLiteral);
    }

    pub fn isBinaryOperator(self: TokenTag) bool {
        return self.is(Mask.IsBinaryOp);
    }

    pub fn isLogicalOperator(self: TokenTag) bool {
        return self.is(Mask.IsLogicalOp);
    }

    pub fn isUnaryOperator(self: TokenTag) bool {
        return self.is(Mask.IsUnaryOp);
    }

    pub fn isAssignmentOperator(self: TokenTag) bool {
        return self.is(Mask.IsAssignmentOp);
    }

    /// returns true for identifier-like tokens.
    /// includes: identifiers, all keywords, literal keywords.
    pub fn isIdentifierLike(self: TokenTag) bool {
        return self.is(Mask.IsIdentifierLike);
    }

    /// returns true for unconditionally reserved keywords.
    /// these can NEVER be used as identifiers.
    pub fn isUnconditionallyReserved(self: TokenTag) bool {
        return self.is(Mask.IsUnconditionallyReserved);
    }

    /// returns true for keywords reserved ONLY in strict mode.
    /// these can be identifiers in sloppy mode but not in strict mode.
    /// includes: let, static, implements, interface, package, private, protected, public, yield
    pub fn isStrictModeReserved(self: TokenTag) bool {
        return self.is(Mask.IsStrictModeReserved);
    }

    /// returns true for any reserved keyword (unconditional or strict-mode-only).
    pub fn isReserved(self: TokenTag) bool {
        return self.is(Mask.IsUnconditionallyReserved) or self.is(Mask.IsStrictModeReserved);
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
};

pub const TokenFlag = enum(u3) {
    line_terminator_before,
    invalid_escape,
    escaped,
};

pub inline fn flagMask(comptime flag: TokenFlag) u8 {
    return @as(u8, 1) << @intFromEnum(flag);
}

pub const Token = struct {
    span: Span,
    tag: TokenTag,

    flags: u8 = 0,

    pub inline fn eof(pos: u32) Token {
        return Token{
            .span = .{ .start = pos, .end = pos },
            .tag = .eof,
            .flags = 0,
        };
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

    // returns left binding power of this token to use in expression parsing loop (pratt parsing)
    pub fn leftBp(self: *const Token) u5 {
        // handle: [no LineTerminator here] ++ --
        if ((self.tag == .increment or self.tag == .decrement) and self.hasLineTerminatorBefore()) {
            return 0; // can't be infix, start new expression
        }

        if (self.tag.isBinaryOperator() or self.tag.isLogicalOperator() or
            self.tag.isAssignmentOperator() or self.tag == .increment or self.tag == .decrement)
        {
            return self.tag.precedence();
        }

        return switch (self.tag) {
            .dot, .optional_chaining, .left_bracket, .left_paren => 17,
            // tagged template: only when no line terminator before the template
            .template_head, .no_substitution_template => if (!self.hasLineTerminatorBefore()) 17 else 0,
            .comma => 1,
            .question => 2,
            else => 0, // can't be infix
        };
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
