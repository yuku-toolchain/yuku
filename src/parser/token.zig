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

const contextual = Mask.IsKeyword | Mask.IsIdentifierLike;
const reserved = contextual | Mask.IsUnconditionallyReserved;
const strict_reserved = contextual | Mask.IsStrictModeReserved;

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

    true = 12 | reserved,
    false = 13 | reserved,
    null_literal = 14 | reserved,

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

    @"if" = 73 | reserved,
    @"else" = 74 | reserved,
    @"switch" = 75 | reserved,
    case = 76 | reserved,
    default = 77 | reserved,
    @"for" = 78 | reserved,
    @"while" = 79 | reserved,
    do = 80 | reserved,
    @"break" = 81 | reserved,
    @"continue" = 82 | reserved,

    function = 83 | reserved,
    @"return" = 84 | reserved,
    async = 85 | contextual,
    await = 86 | contextual,
    yield = 87 | strict_reserved,

    @"var" = 88 | reserved,
    let = 89 | strict_reserved,
    @"const" = 90 | reserved,
    using = 91 | contextual,

    class = 92 | reserved,
    extends = 93 | reserved,
    super = 94 | reserved,
    static = 95 | strict_reserved,
    @"enum" = 96 | reserved,
    public = 97 | strict_reserved,
    private = 98 | strict_reserved,
    protected = 99 | strict_reserved,
    interface = 100 | strict_reserved,
    implements = 101 | strict_reserved,
    package = 102 | strict_reserved,

    import = 103 | reserved,
    @"export" = 104 | reserved,
    from = 105 | contextual,
    as = 106 | (9 << Mask.PrecShift) | contextual,
    namespace = 107 | contextual,
    assert = 108 | contextual,
    source = 109 | contextual,
    @"defer" = 110 | contextual,

    @"try" = 111 | reserved,
    @"catch" = 112 | reserved,
    finally = 113 | reserved,
    throw = 114 | reserved,

    new = 115 | reserved,
    this = 116 | reserved,
    typeof = 117 | Mask.IsUnaryOp | reserved,
    instanceof = 118 | (9 << Mask.PrecShift) | Mask.IsBinaryOp | reserved,
    in = 119 | (9 << Mask.PrecShift) | Mask.IsBinaryOp | reserved,
    of = 120 | contextual,
    delete = 121 | Mask.IsUnaryOp | reserved,
    void = 122 | Mask.IsUnaryOp | reserved,
    with = 123 | reserved,
    debugger = 124 | reserved,

    identifier = 125 | Mask.IsIdentifierLike,
    private_identifier = 126,

    get = 127 | contextual,
    set = 128 | contextual,
    accessor = 129 | Mask.IsIdentifierLike,
    constructor = 130 | Mask.IsIdentifierLike,

    declare = 131 | contextual,
    type = 132 | contextual,
    abstract = 133 | contextual,
    override = 134 | contextual,
    readonly = 135 | contextual,
    keyof = 136 | contextual,
    unique = 137 | contextual,
    infer = 138 | contextual,
    out = 139 | contextual,
    asserts = 140 | contextual,
    satisfies = 141 | (9 << Mask.PrecShift) | contextual,
    intrinsic = 142 | contextual,
    is = 143 | contextual,
    global = 144 | contextual,
    module = 145 | contextual,
    require = 146 | contextual,

    any = 147 | contextual,
    bigint = 148 | contextual,
    boolean = 149 | contextual,
    never = 150 | contextual,
    number = 151 | contextual,
    object = 152 | contextual,
    string = 153 | contextual,
    symbol = 154 | contextual,
    undefined = 155 | contextual,
    unknown = 156 | contextual,

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

const keyword_length_min = 2;
const keyword_length_max = 11;
pub const keyword_read_len = 12;

pub fn keywordTag(lexeme: []const u8) TokenTag {
    if (lexeme.len < keyword_length_min or lexeme.len > keyword_length_max) return .identifier;
    var padded: [keyword_read_len]u8 = @splat(0);
    @memcpy(padded[0..lexeme.len], lexeme);
    return keywordTagAt(&padded, @intCast(lexeme.len));
}

pub inline fn keywordTagAt(bytes: [*]const u8, len: u32) TokenTag {
    const index = @min(len, keyword_read_len);
    const low = std.mem.readInt(u64, bytes[0..8], .little) & keyword_low_mask[index];
    const high = std.mem.readInt(u32, bytes[8..12], .little) & keyword_high_mask[index];
    const entry = &keyword_table[keywordSlot(low, high)];
    const identifier = @backingInt(TokenTag.identifier);
    const high_and_length = high | @as(u32, index) << keyword_length_shift;
    const matches = @intFromBool(entry.low == low) &
        @intFromBool(entry.high_and_length == high_and_length);
    const select = -%@as(u32, matches);
    return @fromBackingInt(identifier ^ ((@backingInt(entry.tag) ^ identifier) & select));
}

const keyword_low_mask: [keyword_read_len + 1]u64 = blk: {
    var masks: [keyword_read_len + 1]u64 = @splat(std.math.maxInt(u64));
    for (0..8) |len| masks[len] = (@as(u64, 1) << (8 * len)) - 1;
    break :blk masks;
};

const keyword_high_mask: [keyword_read_len + 1]u32 = blk: {
    var masks: [keyword_read_len + 1]u32 = @splat(0);
    for (9..keyword_read_len) |len| masks[len] = (@as(u32, 1) << (8 * (len - 8))) - 1;
    masks[keyword_read_len] = std.math.maxInt(u32);
    break :blk masks;
};

const KeywordEntry = extern struct {
    low: u64 = 0,
    high_and_length: u32 = 0,
    tag: TokenTag = .identifier,
};

const keyword_length_shift = 8 * (keyword_read_len - 8 - 1);

const keyword_slot_bits = 8;

const keyword_hash_multiplier: u64 = 0x0de0b6bd6b785d8f;

inline fn keywordSlot(low: u64, high: u32) usize {
    return @intCast(((low +% high) *% keyword_hash_multiplier) >> (64 - keyword_slot_bits));
}

const keyword_table: [1 << keyword_slot_bits]KeywordEntry = blk: {
    @setEvalBranchQuota(20_000);
    var table: [1 << keyword_slot_bits]KeywordEntry = @splat(.{});
    for (std.enums.values(TokenTag)) |keyword_tag| {
        if (!keyword_tag.isIdentifierLike()) continue;
        const name = keyword_tag.toString() orelse continue;
        std.debug.assert(name.len >= keyword_length_min and name.len <= keyword_length_max);
        var padded: [keyword_read_len]u8 = @splat(0);
        @memcpy(padded[0..name.len], name);
        const low = std.mem.readInt(u64, padded[0..8], .little);
        const high = std.mem.readInt(u32, padded[8..12], .little);
        const slot = keywordSlot(low, high);
        if (table[slot].tag != .identifier) @compileError("keyword hash collision: " ++ name);
        table[slot] = .{
            .low = low,
            .high_and_length = high | @as(u32, name.len) << keyword_length_shift,
            .tag = keyword_tag,
        };
    }
    break :blk table;
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
    std.debug.assert(@sizeOf(KeywordEntry) == 16);
    std.debug.assert(keyword_length_max < keyword_read_len);
    // a tag missing from `toString` hits its `unreachable` here
    for (std.enums.values(TokenTag)) |tag| _ = tag.toString();
}
