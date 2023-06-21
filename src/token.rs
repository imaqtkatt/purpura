use std::fmt::Display;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(String),
    Number(u64),
    Fn,
    Let,
    Match,
    Sig,
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
    EOF,
}

impl Token {
    pub fn is_identifier(&self) -> bool {
        matches!(self, Token::Identifier(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Token::Number(_))
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Identifier(str) => write!(f, "{}", str),
            Token::Number(num) => write!(f, "{}", num),
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Match => write!(f, "match"),
            Token::Sig => write!(f, "sig"),
            Token::Data => write!(f, "data"),

            Token::Equal => write!(f, "="),
            Token::NotEqual => write!(f, "!="),
            Token::EqualEqual => write!(f, "=="),
            Token::LessThan => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::GreaterThan => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::PipePipe => write!(f, "||"),
            Token::AndAnd => write!(f, "&&"),

            Token::FatArrow => write!(f, "=>"),
            Token::Bang => write!(f, "!"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Semicolon => write!(f, ";"),
            Token::Pipe => write!(f, "|"),
            Token::Arrow => write!(f, "->"),

            Token::LeftParenthesis => write!(f, "("),
            Token::RightParenthesis => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),

            Token::String(str) => write!(f, "\"{}\"", str),
            Token::Comment(str) => write!(f, "//{}", str),
            Token::Unexpected(str) => write!(f, "{}", str),
            Token::EOF => write!(f, "EOF"),
        }
    }
}
