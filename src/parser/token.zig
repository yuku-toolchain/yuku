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

pub const TokenType = enum(u32) {
    numeric_literal = 1 | Mask.IsNumericLiteral, // e.g., "123", "45.67"
    hex_literal = 2 | Mask.IsNumericLiteral, // e.g., "0xFF", "0x1A"
    octal_literal = 3 | Mask.IsNumericLiteral, // e.g., "0o777", "0o12"
    binary_literal = 4 | Mask.IsNumericLiteral, // e.g., "0b1010", "0b11"
    bigint_literal = 5 | Mask.IsNumericLiteral, // e.g., "123n", "456n"

    string_literal = 6, // e.g., "'hello'", "\"world\""
    regex_literal = 7, // e.g., "/abc/g", "/[0-9]+/i"

    no_substitution_template = 8, // e.g., "`hello`"
    template_head = 9, // e.g., "`hello ${"
    template_middle = 10, // e.g., "} world ${"
    template_tail = 11, // e.g., "} end`"

    true = 12 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "true"
    false = 13 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "false"
    null_literal = 14 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "null"

    plus = 15 | (11 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnaryOp, // "+"
    minus = 16 | (11 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnaryOp, // "-"
    star = 17 | (12 << Mask.PrecShift) | Mask.IsBinaryOp, // "*"
    slash = 18 | (12 << Mask.PrecShift) | Mask.IsBinaryOp, // "/"
    percent = 19 | (12 << Mask.PrecShift) | Mask.IsBinaryOp, // "%"
    exponent = 20 | (13 << Mask.PrecShift) | Mask.IsBinaryOp, // "**"

    assign = 21 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "="
    plus_assign = 22 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "+="
    minus_assign = 23 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "-="
    star_assign = 24 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "*="
    slash_assign = 25 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "/="
    percent_assign = 26 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "%="
    exponent_assign = 27 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "**="

    increment = 28 | (15 << Mask.PrecShift), // "++"
    decrement = 29 | (15 << Mask.PrecShift), // "--"

    equal = 30 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "=="
    not_equal = 31 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "!="
    strict_equal = 32 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "==="
    strict_not_equal = 33 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // "!=="
    less_than = 34 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // "<"
    greater_than = 35 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // ">"
    less_than_equal = 36 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // "<="
    greater_than_equal = 37 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // ">="

    logical_and = 38 | (4 << Mask.PrecShift) | Mask.IsLogicalOp, // "&&"
    logical_or = 39 | (3 << Mask.PrecShift) | Mask.IsLogicalOp, // "||"
    logical_not = 40 | (14 << Mask.PrecShift) | Mask.IsUnaryOp, // "!"

    bitwise_and = 41 | (7 << Mask.PrecShift) | Mask.IsBinaryOp, // "&"
    bitwise_or = 42 | (5 << Mask.PrecShift) | Mask.IsBinaryOp, // "|"
    bitwise_xor = 43 | (6 << Mask.PrecShift) | Mask.IsBinaryOp, // "^"
    bitwise_not = 44 | (14 << Mask.PrecShift) | Mask.IsUnaryOp, // "~"
    left_shift = 45 | (10 << Mask.PrecShift) | Mask.IsBinaryOp, // "<<"
    right_shift = 46 | (10 << Mask.PrecShift) | Mask.IsBinaryOp, // ">>"
    unsigned_right_shift = 47 | (10 << Mask.PrecShift) | Mask.IsBinaryOp, // ">>>"

    bitwise_and_assign = 48 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "&="
    bitwise_or_assign = 49 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "|="
    bitwise_xor_assign = 50 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "^="
    left_shift_assign = 51 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "<<="
    right_shift_assign = 52 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // ">>="
    unsigned_right_shift_assign = 53 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // ">>>="

    nullish_coalescing = 54 | (3 << Mask.PrecShift) | Mask.IsLogicalOp, // "??"
    nullish_assign = 55 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "??="
    logical_and_assign = 56 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "&&="
    logical_or_assign = 57 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp, // "||="
    optional_chaining = 58, // "?."

    left_paren = 59, // "("
    right_paren = 60, // ")"
    left_brace = 61, // "{"
    right_brace = 62, // "}"
    left_bracket = 63, // "["
    right_bracket = 64, // "]"
    semicolon = 65, // ";"
    comma = 66 | (1 << Mask.PrecShift), // ","
    dot = 67, // "."
    spread = 68, // "..."
    arrow = 69, // "=>"
    question = 70 | (2 << Mask.PrecShift), // "?"
    colon = 71, // ":"
    at = 72, // "@"

    @"if" = 73 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "if"
    @"else" = 74 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "else"
    @"switch" = 75 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "switch"
    case = 76 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "case"
    default = 77 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "default"
    @"for" = 78 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "for"
    @"while" = 79 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "while"
    do = 80 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "do"
    @"break" = 81 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "break"
    @"continue" = 82 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "continue"

    function = 83 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "function"
    @"return" = 84 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "return"
    async = 85 | Mask.IsIdentifierLike, // "async"
    await = 86 | Mask.IsIdentifierLike, // "await"
    yield = 87 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "yield"

    @"var" = 88 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "var"
    let = 89 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "let"
    @"const" = 90 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "const"
    using = 91 | Mask.IsIdentifierLike, // "using"

    class = 92 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "class"
    extends = 93 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "extends"
    super = 94 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "super"
    static = 95 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "static"
    @"enum" = 96 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "enum"
    public = 97 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "public"
    private = 98 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "private"
    protected = 99 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "protected"
    interface = 100 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "interface"
    implements = 101 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "implements"
    package = 102 | Mask.IsStrictModeReserved | Mask.IsIdentifierLike, // "package"

    import = 103 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "import"
    @"export" = 104 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "export"
    from = 105 | Mask.IsIdentifierLike, // "from"
    as = 106 | Mask.IsIdentifierLike, // "as"
    namespace = 107 | Mask.IsIdentifierLike, // "namespace"
    assert = 108 | Mask.IsIdentifierLike, // "assert" (import assertions)
    source = 109 | Mask.IsIdentifierLike, // "source" (source phase imports)
    @"defer" = 110 | Mask.IsIdentifierLike, // "defer" (deferred imports)

    @"try" = 111 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "try"
    @"catch" = 112 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "catch"
    finally = 113 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "finally"
    throw = 114 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "throw"

    new = 115 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "new"
    this = 116 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "this"
    typeof = 117 | (14 << Mask.PrecShift) | Mask.IsUnaryOp | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "typeof"
    instanceof = 118 | (9 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "instanceof"
    in = 119 | (9 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "in"
    of = 120 | Mask.IsIdentifierLike, // "of"
    delete = 121 | (14 << Mask.PrecShift) | Mask.IsUnaryOp | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "delete"
    void = 122 | (14 << Mask.PrecShift) | Mask.IsUnaryOp | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "void"
    with = 123 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "with"
    debugger = 124 | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike, // "debugger"

    identifier = 125 | Mask.IsIdentifierLike, // e.g., "myVar", "foo", "_bar"
    private_identifier = 126, // e.g., "#privateField", "#method"

    // typescript
    declare = 127 | Mask.IsIdentifierLike, // "declare"

    // jsx
    jsx_identifier = 128,
    jsx_text = 129,

    eof = 130, // end of file

    pub fn precedence(self: TokenType) u5 {
        return @intCast((@intFromEnum(self) >> Mask.PrecShift) & Mask.PrecOverlap);
    }

    pub fn is(self: TokenType, mask: u32) bool {
        return (@intFromEnum(self) & mask) != 0;
    }

    pub fn isNumericLiteral(self: TokenType) bool {
        return self.is(Mask.IsNumericLiteral);
    }

    pub fn isBinaryOperator(self: TokenType) bool {
        return self.is(Mask.IsBinaryOp);
    }

    pub fn isLogicalOperator(self: TokenType) bool {
        return self.is(Mask.IsLogicalOp);
    }

    pub fn isUnaryOperator(self: TokenType) bool {
        return self.is(Mask.IsUnaryOp);
    }

    pub fn isAssignmentOperator(self: TokenType) bool {
        return self.is(Mask.IsAssignmentOp);
    }

    /// returns true for identifier-like tokens.
    /// includes: identifiers, all keywords, literal keywords.
    pub fn isIdentifierLike(self: TokenType) bool {
        return self.is(Mask.IsIdentifierLike);
    }

    /// returns true for unconditionally reserved keywords.
    /// these can NEVER be used as identifiers.
    pub fn isUnconditionallyReserved(self: TokenType) bool {
        return self.is(Mask.IsUnconditionallyReserved);
    }

    /// returns true for keywords reserved ONLY in strict mode.
    /// these can be identifiers in sloppy mode but not in strict mode.
    /// includes: let, static, implements, interface, package, private, protected, public, yield
    pub fn isStrictModeReserved(self: TokenType) bool {
        return self.is(Mask.IsStrictModeReserved);
    }

    /// returns true for any reserved keyword (unconditional or strict-mode-only).
    pub fn isReserved(self: TokenType) bool {
        return self.is(Mask.IsUnconditionallyReserved) or self.is(Mask.IsStrictModeReserved);
    }

    pub fn toString(self: TokenType) ?[]const u8 {
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

pub const Token = struct {
    span: Span,
    type: TokenType,
    lexeme: []const u8,
    has_line_terminator_before: bool,

    pub inline fn eof(pos: u32) Token {
        return Token{ .lexeme = "", .span = .{ .start = pos, .end = pos }, .type = .eof, .has_line_terminator_before = false };
    }

    pub fn leftBp(self: *const Token) u5 {
        // handle: [no LineTerminator here] ++ --
        if ((self.type == .increment or self.type == .decrement) and self.has_line_terminator_before) {
            return 0; // can't be infix, start new expression
        }

        if (self.type.isBinaryOperator() or self.type.isLogicalOperator() or
            self.type.isAssignmentOperator() or self.type == .increment or self.type == .decrement)
        {
            return self.type.precedence();
        }

        return switch (self.type) {
            .dot, .optional_chaining, .left_bracket, .left_paren => 17,
            // tagged template: only when no line terminator before the template
            .template_head, .no_substitution_template => if (!self.has_line_terminator_before) 17 else 0,
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
