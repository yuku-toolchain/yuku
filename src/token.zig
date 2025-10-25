const std = @import("std");

pub const TokenType = enum {
    // Numeric literals
    NumericLiteral, // 123, 3.14, 1e5
    HexLiteral, // 0xFF, 0x123
    OctalLiteral, // 0o777, 0O123
    BinaryLiteral, // 0b1010, 0B1111
    BigIntLiteral, // 123n, 0xFFn

    // String and regex literals
    StringLiteral, // "hello", 'world'
    RegexLiteral, // /pattern/flags

    // Template literals
    NoSubstitutionTemplate, // `hello world`
    TemplateHead, // `hello ${
    TemplateMiddle, // } world ${
    TemplateTail, // } end`

    // Boolean and null literals
    True, // true
    False, // false
    NullLiteral, // null

    // Arithmetic operators
    Plus, // +
    Minus, // -
    Star, // *
    Slash, // /
    Percent, // %
    Exponent, // **

    // Assignment operators
    Assign, // =
    PlusAssign, // +=
    MinusAssign, // -=
    StarAssign, // *=
    SlashAssign, // /=
    PercentAssign, // %=
    ExponentAssign, // **=

    // Increment/decrement operators
    Increment, // ++
    Decrement, // --

    // Comparison operators
    Equal, // ==
    NotEqual, // !=
    StrictEqual, // ===
    StrictNotEqual, // !==
    LessThan, // <
    GreaterThan, // >
    LessThanEqual, // <=
    GreaterThanEqual, // >=

    // Logical operators
    LogicalAnd, // &&
    LogicalOr, // ||
    LogicalNot, // !

    // Bitwise operators
    BitwiseAnd, // &
    BitwiseOr, // |
    BitwiseXor, // ^
    BitwiseNot, // ~
    LeftShift, // <<
    RightShift, // >>
    UnsignedRightShift, // >>>

    // Bitwise assignment operators
    BitwiseAndAssign, // &=
    BitwiseOrAssign, // |=
    BitwiseXorAssign, // ^=
    LeftShiftAssign, // <<=
    RightShiftAssign, // >>=
    UnsignedRightShiftAssign, // >>>=

    // Modern operators
    NullishCoalescing, // ??
    NullishAssign, // ??=
    LogicalAndAssign, // &&=
    LogicalOrAssign, // ||=
    OptionalChaining, // ?.

    // Delimiters
    LeftParen, // (
    RightParen, // )
    LeftBrace, // {
    RightBrace, // }
    LeftBracket, // [
    RightBracket, // ]
    Semicolon, // ;
    Comma, // ,
    Dot, // .
    Spread, // ...
    Arrow, // =>
    Question, // ?
    Colon, // :

    // Control flow keywords
    If,
    Else,
    Switch,
    Case,
    Default,
    For,
    While,
    Do,
    Break,
    Continue,

    // Function keywords
    Function,
    Return,
    Async,
    Await,
    Yield,

    // Variable declaration keywords
    Var,
    Let,
    Const,

    // Class and OOP keywords
    Class,
    Extends,
    Super,
    Static,
    Enum,
    Public,
    Private,
    Protected,
    Interface,
    Implements,

    // Module keywords
    Import,
    Export,
    From,
    As,

    // Exception handling keywords
    Try,
    Catch,
    Finally,
    Throw,

    // Other keywords
    New,
    This,
    Typeof,
    Instanceof,
    In,
    Of,
    Delete,
    Void,
    With,
    Debugger,

    // Identifiers
    Identifier, // variableName, $$, _, $$variable
    PrivateIdentifier, // #privateField

    // Special tokens
    EOF, // End of file
    Invalid, // Error token
};

pub const Span = struct {
    start: usize,
    end: usize,
};

pub const Token = struct { type: TokenType, lexeme: []const u8, span: Span };

pub const KeywordMap = std.StaticStringMap(TokenType).initComptime(.{
    .{ "if", .If },
    .{ "in", .In },
    .{ "do", .Do },
    .{ "as", .As },
    .{ "of", .Of },
    .{ "let", .Let },
    .{ "var", .Var },
    .{ "for", .For },
    .{ "new", .New },
    .{ "try", .Try },
    .{ "else", .Else },
    .{ "case", .Case },
    .{ "this", .This },
    .{ "void", .Void },
    .{ "with", .With },
    .{ "enum", .Enum },
    .{ "null", .NullLiteral },
    .{ "true", .True },
    .{ "from", .From },
    .{ "const", .Const },
    .{ "class", .Class },
    .{ "break", .Break },
    .{ "catch", .Catch },
    .{ "throw", .Throw },
    .{ "while", .While },
    .{ "yield", .Yield },
    .{ "super", .Super },
    .{ "false", .False },
    .{ "await", .Await },
    .{ "async", .Async },
    .{ "return", .Return },
    .{ "typeof", .Typeof },
    .{ "delete", .Delete },
    .{ "switch", .Switch },
    .{ "export", .Export },
    .{ "import", .Import },
    .{ "public", .Public },
    .{ "static", .Static },
    .{ "default", .Default },
    .{ "finally", .Finally },
    .{ "extends", .Extends },
    .{ "private", .Private },
    .{ "function", .Function },
    .{ "continue", .Continue },
    .{ "debugger", .Debugger },
    .{ "interface", .Interface },
    .{ "protected", .Protected },
    .{ "instanceof", .Instanceof },
    .{ "implements", .Implements },
});
