
#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(String),
    Number(String),
    Fn,
    Let,
    Match,
    Equal,
    EqualEqual,
    FatArrow,
    Bang,
    NotEqual,
    LessThan,
    GreaterThan,
    BiOp(char),
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
    Unexpected(String),
    EOF
}
