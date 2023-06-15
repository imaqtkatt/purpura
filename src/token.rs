
#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(String),
    Number(String),
    Equal,
    Bang,
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
