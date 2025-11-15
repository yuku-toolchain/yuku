const std = @import("std");

pub const Mask = struct {
    pub const IsNumericLiteral: u32 = 1 << 12;
    pub const IsBinaryOp: u32 = 1 << 13;
    pub const IsLogicalOp: u32 = 1 << 14;
    pub const IsUnaryOp: u32 = 1 << 15;

    pub const PrecShift: u32 = 7;
    pub const PrecOverlap: u32 = 31;
};

pub const TokenType = enum(u32) {
    NumericLiteral = 1 | Mask.IsNumericLiteral,
    HexLiteral = 2 | Mask.IsNumericLiteral,
    OctalLiteral = 3 | Mask.IsNumericLiteral,
    BinaryLiteral = 4 | Mask.IsNumericLiteral,
    BigIntLiteral = 5 | Mask.IsNumericLiteral,

    StringLiteral = 6,
    RegexLiteral = 7,

    NoSubstitutionTemplate = 8,
    TemplateHead = 9,
    TemplateMiddle = 10,
    TemplateTail = 11,

    True = 12,
    False = 13,
    NullLiteral = 14,

    Plus = 15 | (11 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnaryOp, // +
    Minus = 16 | (11 << Mask.PrecShift) | Mask.IsBinaryOp | Mask.IsUnaryOp, // -
    Star = 17 | (12 << Mask.PrecShift) | Mask.IsBinaryOp, // *
    Slash = 18 | (12 << Mask.PrecShift) | Mask.IsBinaryOp, // /
    Percent = 19 | (12 << Mask.PrecShift) | Mask.IsBinaryOp, // %
    Exponent = 20 | (13 << Mask.PrecShift) | Mask.IsBinaryOp, // **

    Assign = 21, // =
    PlusAssign = 22, // +=
    MinusAssign = 23, // -=
    StarAssign = 24, // *=
    SlashAssign = 25, // /=
    PercentAssign = 26, // %=
    ExponentAssign = 27, // **=

    Increment = 28, // ++
    Decrement = 29, // --

    Equal = 30 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // ==
    NotEqual = 31 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // !=
    StrictEqual = 32 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // ===
    StrictNotEqual = 33 | (8 << Mask.PrecShift) | Mask.IsBinaryOp, // !==
    LessThan = 34 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // <
    GreaterThan = 35 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // >
    LessThanEqual = 36 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // <=
    GreaterThanEqual = 37 | (9 << Mask.PrecShift) | Mask.IsBinaryOp, // >=

    LogicalAnd = 38 | (4 << Mask.PrecShift) | Mask.IsLogicalOp, // &&
    LogicalOr = 39 | (3 << Mask.PrecShift) | Mask.IsLogicalOp, // ||
    LogicalNot = 40 | (14 << Mask.PrecShift) | Mask.IsUnaryOp, // !

    BitwiseAnd = 41 | (7 << Mask.PrecShift) | Mask.IsBinaryOp, // &
    BitwiseOr = 42 | (5 << Mask.PrecShift) | Mask.IsBinaryOp, // |
    BitwiseXor = 43 | (6 << Mask.PrecShift) | Mask.IsBinaryOp, // ^
    BitwiseNot = 44 | (14 << Mask.PrecShift) | Mask.IsUnaryOp, // ~
    LeftShift = 45 | (10 << Mask.PrecShift) | Mask.IsBinaryOp, // <<
    RightShift = 46 | (10 << Mask.PrecShift) | Mask.IsBinaryOp, // >>
    UnsignedRightShift = 47 | (10 << Mask.PrecShift) | Mask.IsBinaryOp, // >>>

    BitwiseAndAssign = 48, // &=
    BitwiseOrAssign = 49, // |=
    BitwiseXorAssign = 50, // ^=
    LeftShiftAssign = 51, // <<=
    RightShiftAssign = 52, // >>=
    UnsignedRightShiftAssign = 53, // >>>=

    NullishCoalescing = 54 | (3 << Mask.PrecShift) | Mask.IsLogicalOp, // ??
    NullishAssign = 55, // ??=
    LogicalAndAssign = 56, // &&=
    LogicalOrAssign = 57, // ||=
    OptionalChaining = 58, // ?.

    LeftParen = 59, // (
    RightParen = 60, // )
    LeftBrace = 61, // {
    RightBrace = 62, // }
    LeftBracket = 63, // [
    RightBracket = 64, // ]
    Semicolon = 65, // ;
    Comma = 66, // ,
    Dot = 67, // .
    Spread = 68, // ...
    Arrow = 69, // =>
    Question = 70, // ?
    Colon = 71, // :

    If = 72,
    Else = 73,
    Switch = 74,
    Case = 75,
    Default = 76,
    For = 77,
    While = 78,
    Do = 79,
    Break = 80,
    Continue = 81,

    Function = 82,
    Return = 83,
    Async = 84,
    Await = 85,
    Yield = 86,

    Var = 87,
    Let = 88,
    Const = 89,
    Using = 90,

    Class = 91,
    Extends = 92,
    Super = 93,
    Static = 94,
    Enum = 95,
    Public = 96,
    Private = 97,
    Protected = 98,
    Interface = 99,
    Implements = 100,

    Import = 101,
    Export = 102,
    From = 103,
    As = 104,

    Try = 105,
    Catch = 106,
    Finally = 107,
    Throw = 108,

    New = 109,
    This = 110,
    Typeof = 111 | (14 << Mask.PrecShift) | Mask.IsUnaryOp,
    Instanceof = 112 | (9 << Mask.PrecShift) | Mask.IsBinaryOp,
    In = 113 | (9 << Mask.PrecShift) | Mask.IsBinaryOp,
    Of = 114,
    Delete = 115 | (14 << Mask.PrecShift) | Mask.IsUnaryOp,
    Void = 116 | (14 << Mask.PrecShift) | Mask.IsUnaryOp,
    With = 117,
    Debugger = 118,

    Identifier = 119,
    PrivateIdentifier = 120,

    EOF = 121, // end of file

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
};

pub const Span = struct {
    start: u32,
    end: u32,
};

pub const Token = struct {
    span: Span,
    type: TokenType,
    lexeme: []const u8,

    pub inline fn eof(pos: u32) Token {
        return Token{ .lexeme = "", .span = .{ .start = pos, .end = pos }, .type = .EOF };
    }
};

pub const CommentType = enum {
    SingleLine, // // comment
    MultiLine, // /* comment */
};

pub const Comment = struct {
    span: Span,
    content: []const u8,
    type: CommentType,
};
