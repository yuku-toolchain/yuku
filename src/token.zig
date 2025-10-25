pub const TokenType = enum {
    NumericLiteral,     // 123, 3.14, 1e5  (done)
    HexLiteral,         // 0xFF, 0x123     (done)
    OctalLiteral,       // 0o777, 0O123    (done)
    BinaryLiteral,      // 0b1010, 0B1111  (done)
    BigIntLiteral,      // 123n, 0xFFn     (done)

    StringLiteral,      // "hello", 'world'  (done)
    RegexLiteral,       // /pattern/flags    (done)

    NoSubstitutionTemplate, // `hello world`
    TemplateHead,           // `hello ${
    TemplateMiddle,         // } world ${
    TemplateTail,           // } end`

    BooleanLiteral,     // true, false
    NullLiteral,        // null

    Plus,               // + (done)
    Minus,              // - (done)
    Star,               // * (done)
    Slash,              // / (done)
    Percent,            // % (done)
    Exponent,           // ** (done)

    Assign,             // = (done)
    PlusAssign,         // += (done)
    MinusAssign,        // -= (done)
    StarAssign,         // *= (done)
    SlashAssign,        // /= (done)
    PercentAssign,      // %= (done)
    ExponentAssign,     // **= (done)

    Increment,          // ++ (done)
    Decrement,          // -- (done)

    Equal,              // == (done)
    NotEqual,           // != (done)
    StrictEqual,        // === (done)
    StrictNotEqual,     // !== (done)
    LessThan,           // < (done)
    GreaterThan,        // > (done)
    LessThanEqual,      // <= (done)
    GreaterThanEqual,   // >= (done)

    LogicalAnd,         // && (done)
    LogicalOr,          // || (done)
    LogicalNot,         // ! (done)

    BitwiseAnd,         // & (done)
    BitwiseOr,          // | (done)
    BitwiseXor,         // ^ (done)
    BitwiseNot,         // ~ (done)
    LeftShift,          // << (done)
    RightShift,         // >> (done)
    UnsignedRightShift, // >>> (done)

    BitwiseAndAssign,   // &= (done)
    BitwiseOrAssign,    // |= (done)
    BitwiseXorAssign,   // ^= (done)
    LeftShiftAssign,    // <<= (done)
    RightShiftAssign,   // >>= (done)
    UnsignedRightShiftAssign, // >>>= (done)

    NullishCoalescing,  // ?? (done)
    NullishAssign,      // ??= (done)
    LogicalAndAssign,   // &&= (done)
    LogicalOrAssign,    // ||= (done)
    OptionalChaining,   // ?. (done)

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
    Arrow,              // => (done)
    Question,           // ? (done)
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
