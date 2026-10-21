const std = @import("std");

pub const Mask = struct {
    pub const IsNumericLiteral: u32 = 1 << 13;
    pub const IsBinaryOp: u32 = 1 << 14;
    pub const IsLogicalOp: u32 = 1 << 15;
    pub const IsUnaryOp: u32 = 1 << 16;
    pub const IsAssignmentOp: u32 = 1 << 17;
    pub const IsIdentifierLike: u32 = 1 << 18;
    pub const IsUnconditionallyReserved: u32 = 1 << 19;
    pub const IsStrictModeReserved: u32 = 1 << 20;
    pub const IsKeyword: u32 = 1 << 21;

    pub const PrecShift: u32 = 8;
    pub const PrecOverlap: u32 = 0b11111;
};

pub const TokenTag = enum(u32) {
    numeric_literal = 1 | Mask.IsNumericLiteral,
    hex_literal = 2 | Mask.IsNumericLiteral,
    octal_literal = 3 | Mask.IsNumericLiteral,
    binary_literal = 4 | Mask.IsNumericLiteral,
    bigint_literal = 5 | Mask.IsNumericLiteral,
    string_literal = 6,
    regex_literal = 7,
    no_substitution_template = 8 | (17 << Mask.PrecShift),
    template_head = 9 | (17 << Mask.PrecShift),
    template_middle = 10,
    template_tail = 11,

    true = 12 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    false = 13 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    null_literal = 14 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,

    plus = 15 | (11 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnaryOp,
    minus = 16 | (11 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnaryOp,
    star = 17 | (12 << Mask.PrecShift) | Mask.IsBinaryOp,
    slash = 18 | (12 << Mask.PrecShift) | Mask.IsBinaryOp,
    percent = 19 | (12 << Mask.PrecShift) | Mask.IsBinaryOp,
    exponent = 20 | (13 << Mask.PrecShift) | Mask.IsBinaryOp,

    assign = 21 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,
    plus_assign = 22 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,
    minus_assign = 23 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,
    star_assign = 24 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,
    slash_assign = 25 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,
    percent_assign = 26 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,
    exponent_assign = 27 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,

    increment = 28 | (15 << Mask.PrecShift),
    decrement = 29 | (15 << Mask.PrecShift),

    equal = 30 | (8 << Mask.PrecShift) | Mask.IsBinaryOp,
    not_equal = 31 | (8 << Mask.PrecShift) | Mask.IsBinaryOp,
    strict_equal = 32 | (8 << Mask.PrecShift) | Mask.IsBinaryOp,
    strict_not_equal = 33 | (8 << Mask.PrecShift) | Mask.IsBinaryOp,
    less_than = 34 | (9 << Mask.PrecShift) | Mask.IsBinaryOp,
    greater_than = 35 | (9 << Mask.PrecShift) | Mask.IsBinaryOp,
    less_than_equal = 36 | (9 << Mask.PrecShift) | Mask.IsBinaryOp,
    greater_than_equal = 37 | (9 << Mask.PrecShift) | Mask.IsBinaryOp,

    logical_and = 38 | (4 << Mask.PrecShift) | Mask.IsLogicalOp,
    logical_or = 39 | (3 << Mask.PrecShift) | Mask.IsLogicalOp,
    logical_not = 40 | (14 << Mask.PrecShift) | Mask.IsUnaryOp,

    bitwise_and = 41 | (7 << Mask.PrecShift) | Mask.IsBinaryOp,
    bitwise_or = 42 | (5 << Mask.PrecShift) | Mask.IsBinaryOp,
    bitwise_xor = 43 | (6 << Mask.PrecShift) | Mask.IsBinaryOp,
    bitwise_not = 44 | Mask.IsUnaryOp,
    left_shift = 45 | (10 << Mask.PrecShift) | Mask.IsBinaryOp,
    right_shift = 46 | (10 << Mask.PrecShift) | Mask.IsBinaryOp,
    unsigned_right_shift = 47 | (10 << Mask.PrecShift) | Mask.IsBinaryOp,

    bitwise_and_assign = 48 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,
    bitwise_or_assign = 49 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,
    bitwise_xor_assign = 50 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,
    left_shift_assign = 51 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,
    right_shift_assign = 52 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,
    unsigned_right_shift_assign = 53 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,

    nullish_coalescing = 54 | (3 << Mask.PrecShift) | Mask.IsLogicalOp,
    nullish_assign = 55 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,
    logical_and_assign = 56 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,
    logical_or_assign = 57 | (2 << Mask.PrecShift) | Mask.IsAssignmentOp,
    optional_chaining = 58 | (17 << Mask.PrecShift),

    left_paren = 59 | (17 << Mask.PrecShift),
    right_paren = 60,
    left_brace = 61,
    right_brace = 62,
    left_bracket = 63 | (17 << Mask.PrecShift),
    right_bracket = 64,
    semicolon = 65,
    comma = 66 | (1 << Mask.PrecShift),
    dot = 67 | (17 << Mask.PrecShift),
    spread = 68,
    arrow = 69 | (2 << Mask.PrecShift),
    question = 70 | (2 << Mask.PrecShift),
    colon = 71,
    at = 72,

    @"if" = 73 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    @"else" = 74 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    @"switch" = 75 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    case = 76 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    default = 77 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    @"for" = 78 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    @"while" = 79 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    do = 80 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    @"break" = 81 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    @"continue" = 82 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,

    function = 83 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    @"return" = 84 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    async = 85 | Mask.IsKeyword | Mask.IsIdentifierLike,
    await = 86 | Mask.IsKeyword | Mask.IsIdentifierLike,
    yield = 87 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike,

    @"var" = 88 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    let = 89 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike,
    @"const" = 90 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    using = 91 | Mask.IsKeyword | Mask.IsIdentifierLike,

    class = 92 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    extends = 93 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    super = 94 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    static = 95 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike,
    @"enum" = 96 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    public = 97 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike,
    private = 98 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike,
    protected = 99 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike,
    interface = 100 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike,
    implements = 101 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike,
    package = 102 | Mask.IsKeyword | Mask.IsStrictModeReserved | Mask.IsIdentifierLike,

    import = 103 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    @"export" = 104 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    from = 105 | Mask.IsKeyword | Mask.IsIdentifierLike,
    as = 106 | (9 << Mask.PrecShift) | Mask.IsKeyword | Mask.IsIdentifierLike,
    namespace = 107 | Mask.IsKeyword | Mask.IsIdentifierLike,
    assert = 108 | Mask.IsKeyword | Mask.IsIdentifierLike,
    source = 109 | Mask.IsKeyword | Mask.IsIdentifierLike,
    @"defer" = 110 | Mask.IsKeyword | Mask.IsIdentifierLike,

    @"try" = 111 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    @"catch" = 112 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    finally = 113 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    throw = 114 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,

    new = 115 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    this = 116 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    typeof = 117 | Mask.IsUnaryOp | Mask.IsKeyword | Mask.IsUnconditionallyReserved |
        Mask.IsIdentifierLike,
    instanceof = 118 | (9 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsKeyword |
        Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    in = 119 | (9 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsKeyword |
        Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    of = 120 | Mask.IsKeyword | Mask.IsIdentifierLike,
    delete = 121 | Mask.IsUnaryOp | Mask.IsKeyword | Mask.IsUnconditionallyReserved |
        Mask.IsIdentifierLike,
    void = 122 | Mask.IsUnaryOp | Mask.IsKeyword | Mask.IsUnconditionallyReserved |
        Mask.IsIdentifierLike,
    with = 123 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,
    debugger = 124 | Mask.IsKeyword | Mask.IsUnconditionallyReserved | Mask.IsIdentifierLike,

    identifier = 125 | Mask.IsIdentifierLike,
    private_identifier = 126,

    get = 127 | Mask.IsKeyword | Mask.IsIdentifierLike,
    set = 128 | Mask.IsKeyword | Mask.IsIdentifierLike,
    accessor = 129 | Mask.IsIdentifierLike,
    constructor = 130 | Mask.IsIdentifierLike,

    declare = 131 | Mask.IsKeyword | Mask.IsIdentifierLike,
    type = 132 | Mask.IsKeyword | Mask.IsIdentifierLike,
    abstract = 133 | Mask.IsKeyword | Mask.IsIdentifierLike,
    override = 134 | Mask.IsKeyword | Mask.IsIdentifierLike,
    readonly = 135 | Mask.IsKeyword | Mask.IsIdentifierLike,
    keyof = 136 | Mask.IsKeyword | Mask.IsIdentifierLike,
    unique = 137 | Mask.IsKeyword | Mask.IsIdentifierLike,
    infer = 138 | Mask.IsKeyword | Mask.IsIdentifierLike,
    out = 139 | Mask.IsKeyword | Mask.IsIdentifierLike,
    asserts = 140 | Mask.IsKeyword | Mask.IsIdentifierLike,
    satisfies = 141 | (9 << Mask.PrecShift) | Mask.IsKeyword | Mask.IsIdentifierLike,
    intrinsic = 142 | Mask.IsKeyword | Mask.IsIdentifierLike,
    is = 143 | Mask.IsKeyword | Mask.IsIdentifierLike,
    global = 144 | Mask.IsKeyword | Mask.IsIdentifierLike,
    module = 145 | Mask.IsKeyword | Mask.IsIdentifierLike,
    require = 146 | Mask.IsKeyword | Mask.IsIdentifierLike,

    any = 147 | Mask.IsKeyword | Mask.IsIdentifierLike,
    bigint = 148 | Mask.IsKeyword | Mask.IsIdentifierLike,
    boolean = 149 | Mask.IsKeyword | Mask.IsIdentifierLike,
    never = 150 | Mask.IsKeyword | Mask.IsIdentifierLike,
    number = 151 | Mask.IsKeyword | Mask.IsIdentifierLike,
    object = 152 | Mask.IsKeyword | Mask.IsIdentifierLike,
    string = 153 | Mask.IsKeyword | Mask.IsIdentifierLike,
    symbol = 154 | Mask.IsKeyword | Mask.IsIdentifierLike,
    undefined = 155 | Mask.IsKeyword | Mask.IsIdentifierLike,
    unknown = 156 | Mask.IsKeyword | Mask.IsIdentifierLike,

    jsx_identifier = 157,
    jsx_text = 158,

    eof = 159,
    /// Input that lookahead could not scan, left for the parse to report.
    invalid = 160,

    pub fn precedence(self: TokenTag) u5 {
        return @intCast((@backingInt(self) >> Mask.PrecShift) & Mask.PrecOverlap);
    }

    fn hasMask(self: TokenTag, mask: u32) bool {
        return (@backingInt(self) & mask) != 0;
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

    /// Whether the token is an identifier or any keyword.
    pub fn isIdentifierLike(self: TokenTag) bool {
        return self.hasMask(Mask.IsIdentifierLike);
    }

    pub fn isKeyword(self: TokenTag) bool {
        return self.hasMask(Mask.IsKeyword);
    }

    /// Whether the token is a keyword that can never be an identifier.
    pub fn isUnconditionallyReserved(self: TokenTag) bool {
        return self.hasMask(Mask.IsUnconditionallyReserved);
    }

    /// Whether the token is a keyword reserved only in strict mode code.
    pub fn isStrictModeReserved(self: TokenTag) bool {
        return self.hasMask(Mask.IsStrictModeReserved);
    }

    /// Whether the token is reserved unconditionally or in strict mode.
    pub fn isReserved(self: TokenTag) bool {
        return self.hasMask(Mask.IsUnconditionallyReserved | Mask.IsStrictModeReserved);
    }

    pub fn toString(self: TokenTag) ?[]const u8 {
        // a keyword tag is named as it is spelled, except `null`
        if (self.isIdentifierLike()) return switch (self) {
            .identifier => null,
            .null_literal => "null",
            else => @tagName(self),
        };
        return switch (self) {
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

            .eof,
            .invalid,
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
            .private_identifier,
            .jsx_identifier,
            .jsx_text,
            => null,

            else => unreachable,
        };
    }
};

pub const keyword_length_min = 2;
pub const keyword_length_max = 11;

/// The tag of the word `lexeme`, or `.identifier`.
pub fn keywordTag(lexeme: []const u8) TokenTag {
    if (lexeme.len < keyword_length_min or lexeme.len > keyword_length_max) {
        return .identifier;
    }
    const h = keywordHash(lexeme[0], lexeme[1], lexeme[lexeme.len - 1], lexeme.len);
    const entry = &keyword_table[h];
    if (entry.len != lexeme.len) return .identifier;
    if (!std.mem.eql(u8, entry.name[0..lexeme.len], lexeme)) return .identifier;
    return entry.tag;
}

const KeywordEntry = struct {
    name: [keyword_length_max]u8,
    len: u8,
    tag: TokenTag,
};

inline fn keywordHash(c0: u8, c1: u8, c_last: u8, length: usize) u32 {
    const h = @as(u32, c0) * 56 + @as(u32, c1) * 97 +
        @as(u32, c_last) * 108 + @as(u32, @intCast(length)) * 117;
    return h & 511;
}

const keyword_table: [512]KeywordEntry = blk: {
    @setEvalBranchQuota(20_000);
    var t: [512]KeywordEntry = @splat(.{ .name = @splat(0), .len = 0, .tag = .identifier });
    for (std.enums.values(TokenTag)) |keyword_tag| {
        if (!keyword_tag.isIdentifierLike()) continue;
        const name = keyword_tag.toString() orelse continue;
        std.debug.assert(name.len >= keyword_length_min);
        std.debug.assert(name.len <= keyword_length_max);
        const h = keywordHash(name[0], name[1], name[name.len - 1], name.len);
        if (t[h].len != 0) @compileError("keyword perfect hash collision: " ++ name);
        var entry: KeywordEntry = .{ .name = @splat(0), .len = name.len, .tag = keyword_tag };
        @memcpy(entry.name[0..name.len], name);
        t[h] = entry;
    }
    break :blk t;
};

pub const Span = extern struct {
    start: u32,
    end: u32,
};

pub const TokenFlag = enum(u3) {
    line_terminator_before,
    invalid_escape,
    escaped,
    lone_surrogates,
};

pub inline fn flagMask(comptime flag: TokenFlag) u8 {
    return @as(u8, 1) << @backingInt(flag);
}

// extern, the token list is copied to the wire as is
pub const Token = extern struct {
    span: Span,
    tag: TokenTag,
    flags: u8 = 0,
    _pad: [3]u8 = .{ 0, 0, 0 },

    pub inline fn eof(pos: u32) Token {
        return .{ .span = .{ .start = pos, .end = pos }, .tag = .eof };
    }

    pub inline fn invalid(pos: u32) Token {
        return .{ .span = .{ .start = pos, .end = pos }, .tag = .invalid };
    }

    pub inline fn len(self: Token) u32 {
        return self.span.end - self.span.start;
    }

    pub inline fn text(self: Token, source: []const u8) []const u8 {
        return source[self.span.start..self.span.end];
    }

    inline fn has(self: Token, comptime flag: TokenFlag) bool {
        return (self.flags & flagMask(flag)) != 0;
    }

    /// Whether the trivia before this token contained a line terminator.
    pub inline fn hasLineTerminatorBefore(self: Token) bool {
        return self.has(.line_terminator_before);
    }

    /// Whether scanning this token met an invalid escape, which only tagged templates tolerate.
    pub inline fn hasInvalidEscape(self: Token) bool {
        return self.has(.invalid_escape);
    }

    /// Whether the text has escapes, or a raw carriage return in a template.
    pub inline fn isEscaped(self: Token) bool {
        return self.has(.escaped);
    }

    /// Whether a string or template token contains lone surrogates.
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
    std.debug.assert(@sizeOf(Token) == 16);
    // a tag missing from `toString` hits its `unreachable` here
    for (std.enums.values(TokenTag)) |tag| _ = tag.toString();
}
