pub const TokenType = enum {
    NumericLiteral,     // 123, 3.14, 1e5  (done)
    HexLiteral,         // 0xFF, 0x123     (done)
    OctalLiteral,       // 0o777, 0O123    (done)
    BinaryLiteral,      // 0b1010, 0B1111  (done)
    BigIntLiteral,      // 123n, 0xFFn     (done)

    StringLiteral,      // "hello", 'world'
    RegexLiteral,       // /pattern/flags

    NoSubstitutionTemplate, // `hello world`
    TemplateHead,           // `hello ${
    TemplateMiddle,         // } world ${
    TemplateTail,           // } end`

    BooleanLiteral,     // true, false
    NullLiteral,        // null

    Plus,               // + (done)
    Minus,              // -
    Star,               // *
    Slash,              // /
    Percent,            // %
    Exponent,           // **

    Assign,             // =
    PlusAssign,         // += (done)
    MinusAssign,        // -=
    StarAssign,         // *=
    SlashAssign,        // /=
    PercentAssign,      // %=
    ExponentAssign,     // **=

    Increment,          // ++ (done)
    Decrement,          // --

    Equal,              // ==
    NotEqual,           // !=
    StrictEqual,        // ===
    StrictNotEqual,     // !==
    LessThan,           // <
    GreaterThan,        // >
    LessThanEqual,      // <=
    GreaterThanEqual,   // >=

    LogicalAnd,         // &&
    LogicalOr,          // ||
    LogicalNot,         // !

    BitwiseAnd,         // &
    BitwiseOr,          // |
    BitwiseXor,         // ^
    BitwiseNot,         // ~
    LeftShift,          // <<
    RightShift,         // >>
    UnsignedRightShift, // >>>

    BitwiseAndAssign,   // &=
    BitwiseOrAssign,    // |=
    BitwiseXorAssign,   // ^=
    LeftShiftAssign,    // <<=
    RightShiftAssign,   // >>=
    UnsignedRightShiftAssign, // >>>=

    NullishCoalescing,  // ??
    NullishAssign,      // ??=
    LogicalAndAssign,   // &&=
    LogicalOrAssign,    // ||=
    OptionalChaining,   // ?.

    LeftParen,          // (
    RightParen,         // )
    LeftBrace,          // {
    RightBrace,         // }
    LeftBracket,        // [
    RightBracket,       // ]
    Semicolon,          // ;
    Comma,              // ,
    Dot,                // .   (done)
    Spread,             // ... (done)
    Arrow,              // =>
    Question,           // ?
    Colon,              // :

    If, Else, Switch, Case, Default,
    For, While, Do, Break, Continue,

    Function, Return, Async, Await, Yield,

    Var, Let, Const,

    Class, Extends, Super, Static, Enum,

    Import, Export, From, As,

    Try, Catch, Finally, Throw,

    New, This, Typeof, Instanceof, In, Of,
    Delete, Void, With, Debugger,

    Identifier,         // variableName, $$, _, $$variable
    PrivateIdentifier,  // #privateField
    EOF,                // End of file (done)
    Invalid,            // Error token (done)
};

pub const Span = struct {
    start: usize,
    end: usize,
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    span: Span
};
