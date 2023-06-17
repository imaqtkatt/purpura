
#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(String),
    Number(String),
    Fn,
    Let,
    Match,
    Spec,
    Data,

    Equal,
    NotEqual,
    EqualEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Mul,
    Div,
    Plus,
    Minus,
    PipePipe,
    AndAnd,

    FatArrow,
    Bang,
    Comma,
    Dot,
    Semicolon,
    Pipe,
    Arrow,

    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,

    String(String),
    Comment(String),
    Unexpected(String),
    EOF
}
