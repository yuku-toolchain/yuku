pub const Mask = struct {
    pub const IsNumericLiteral: u32 = 1 << 12;
    pub const IsBinaryOp: u32 = 1 << 13;
    pub const IsLogicalOp: u32 = 1 << 14;
    pub const IsUnaryOp: u32 = 1 << 15;
    pub const IsAssignmentOp: u32 = 1 << 16;
    pub const IsIdentifierLike: u32 = 1 << 17;

    pub const PrecShift: u32 = 7;
    pub const PrecOverlap: u32 = 31;
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

    true = 12 | Mask.IsIdentifierLike, // "true"
    false = 13 | Mask.IsIdentifierLike, // "false"
    null_literal = 14 | Mask.IsIdentifierLike, // "null"

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
    comma = 66, // ","
    dot = 67, // "."
    spread = 68, // "..."
    arrow = 69, // "=>"
    question = 70 | (2 << Mask.PrecShift), // "?"
    colon = 71, // ":"

    @"if" = 72 | Mask.IsIdentifierLike, // "if"
    @"else" = 73 | Mask.IsIdentifierLike, // "else"
    @"switch" = 74 | Mask.IsIdentifierLike, // "switch"
    case = 75 | Mask.IsIdentifierLike, // "case"
    default = 76 | Mask.IsIdentifierLike, // "default"
    @"for" = 77 | Mask.IsIdentifierLike, // "for"
    @"while" = 78 | Mask.IsIdentifierLike, // "while"
    do = 79 | Mask.IsIdentifierLike, // "do"
    @"break" = 80 | Mask.IsIdentifierLike, // "break"
    @"continue" = 81 | Mask.IsIdentifierLike, // "continue"

    function = 82 | Mask.IsIdentifierLike, // "function"
    @"return" = 83 | Mask.IsIdentifierLike, // "return"
    async = 84 | Mask.IsIdentifierLike, // "async"
    await = 85 | Mask.IsIdentifierLike, // "await"
    yield = 86 | Mask.IsIdentifierLike, // "yield"

    @"var" = 87 | Mask.IsIdentifierLike, // "var"
    let = 88 | Mask.IsIdentifierLike, // "let"
    @"const" = 89 | Mask.IsIdentifierLike, // "const"
    using = 90 | Mask.IsIdentifierLike, // "using"

    class = 91 | Mask.IsIdentifierLike, // "class"
    extends = 92 | Mask.IsIdentifierLike, // "extends"
    super = 93 | Mask.IsIdentifierLike, // "super"
    static = 94 | Mask.IsIdentifierLike, // "static"
    @"enum" = 95 | Mask.IsIdentifierLike, // "enum"
    public = 96 | Mask.IsIdentifierLike, // "public"
    private = 97 | Mask.IsIdentifierLike, // "private"
    protected = 98 | Mask.IsIdentifierLike, // "protected"
    interface = 99 | Mask.IsIdentifierLike, // "interface"
    implements = 100 | Mask.IsIdentifierLike, // "implements"

    import = 101 | Mask.IsIdentifierLike, // "import"
    @"export" = 102 | Mask.IsIdentifierLike, // "export"
    from = 103 | Mask.IsIdentifierLike, // "from"
    as = 104 | Mask.IsIdentifierLike, // "as"

    @"try" = 105 | Mask.IsIdentifierLike, // "try"
    @"catch" = 106 | Mask.IsIdentifierLike, // "catch"
    finally = 107 | Mask.IsIdentifierLike, // "finally"
    throw = 108 | Mask.IsIdentifierLike, // "throw"

    new = 109 | Mask.IsIdentifierLike, // "new"
    this = 110 | Mask.IsIdentifierLike, // "this"
    typeof = 111 | (14 << Mask.PrecShift) | Mask.IsUnaryOp | Mask.IsIdentifierLike, // "typeof"
    instanceof = 112 | (9 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsIdentifierLike, // "instanceof"
    in = 113 | (9 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsIdentifierLike, // "in"
    of = 114 | Mask.IsIdentifierLike, // "of"
    delete = 115 | (14 << Mask.PrecShift) | Mask.IsUnaryOp | Mask.IsIdentifierLike, // "delete"
    void = 116 | (14 << Mask.PrecShift) | Mask.IsUnaryOp | Mask.IsIdentifierLike, // "void"
    with = 117 | Mask.IsIdentifierLike, // "with"
    debugger = 118 | Mask.IsIdentifierLike, // "debugger"

    identifier = 119 | Mask.IsIdentifierLike, // e.g., "myVar", "foo", "_bar"
    private_identifier = 120, // e.g., "#privateField", "#method"

    // typescript
    declare = 121 | Mask.IsIdentifierLike, // "declare"

    eof = 122, // end of file

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

            .import => "import",
            .@"export" => "export",
            .from => "from",
            .as => "as",

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

    pub fn leftBindingPower(self: *const Token) u5 {
        // handle: [no LineTerminator here] ++ --
        if ((self.type == .increment or self.type == .decrement) and self.has_line_terminator_before) {
            return 0; // can't be infix, start new expression
        }

        if (self.type.isBinaryOperator() or self.type.isLogicalOperator() or
            self.type.isAssignmentOperator() or self.type == .increment or self.type == .decrement)
        {
            return self.type.precedence();
        }

        // conditional operator (precedence 2)
        if (self.type == .question) {
            return 2;
        }

        // member access, call, and tagged template expressions (precedence 17)
        return switch (self.type) {
            .dot, .optional_chaining, .left_bracket, .left_paren => 17,
            // tagged template: only when no line terminator before the template
            .template_head, .no_substitution_template => if (!self.has_line_terminator_before) 17 else 0,
            else => 0, // can't be infix
        };
    }
};
