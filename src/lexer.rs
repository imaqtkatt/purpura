//! Module for the lexer for the purpura language. This module gets a source code as a String and
//! uses it to split the source code into tokens. The tokens are then used by the parser to build
//! a tree.

use crate::token::Token;
use std::{iter::Peekable, str::Chars};

/// The main structure of the module. It is an iterator of tokens.
pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer {
            source: source.chars().peekable(),
        }
    }

    fn single(&mut self, token: Token) -> Token {
        self.source.next();
        token
    }

    fn skip(&mut self) -> Token {
        self.source.next();
        self.next_token()
    }

    fn read_while(&mut self, predicate: fn(char) -> bool) -> String {
        let mut string = String::new();

        while let Some(ch) = self.source.peek() {
            if predicate(*ch) {
                string.push(self.source.next().unwrap());
            } else {
                break;
            }
        }

        string
    }

    fn read_identifier(&mut self) -> Token {
        let identifier = self.read_while(|c| c.is_ascii_alphabetic());
        match identifier.as_ref() {
            "fn" => Token::Fn,
            "let" => Token::Let,
            "match" => Token::Match,
            "sig" => Token::Sig,
            "data" => Token::Data,
            _ => Token::Identifier(identifier),
        }
    }

    fn read_number(&mut self) -> Token {
        let number = self.read_while(char::is_numeric);
        Token::Number(number.parse::<u64>().unwrap())
    }

    fn read_unexpected(&mut self) -> Token {
        let unexpected = self.read_while(char::is_alphabetic);
        Token::Unexpected(unexpected)
    }

    fn read_string(&mut self) -> Token {
        self.source.next(); // ignore first quote "
        let string = self.read_while(ne_quote);
        self.source.next();
        Token::String(string)
    }

    fn maybe_arrow(&mut self) -> Token {
        self.source.next();
        match self.source.peek() {
            Some('>') => self.single(Token::Arrow),
            _ => Token::Minus,
        }
    }

    fn read_equal(&mut self) -> Token {
        self.source.next();
        match self.source.peek() {
            Some('=') => self.single(Token::EqualEqual),
            Some('>') => self.single(Token::FatArrow),
            _ => Token::Equal,
        }
    }

    fn read_bang(&mut self) -> Token {
        self.source.next();
        match self.source.peek() {
            Some('=') => self.single(Token::NotEqual),
            _ => Token::Bang,
        }
    }

    fn read_less_equal(&mut self) -> Token {
        self.source.next();
        match self.source.peek() {
            Some('=') => self.single(Token::LessEqual),
            _ => Token::LessThan,
        }
    }

    fn read_greater_equal(&mut self) -> Token {
        self.source.next();
        match self.source.peek() {
            Some('=') => self.single(Token::GreaterEqual),
            _ => Token::GreaterThan,
        }
    }

    fn read_comment(&mut self) -> Token {
        self.source.next();
        match self.source.peek() {
            Some('[') => {
                self.source.next();
                let comment = self.read_while(|c| c != ']');
                self.source.next();
                Token::Comment(comment)
            }
            _ => {
                let comment = self.read_while(|c| c != '\n');
                Token::Comment(comment)
            }
        }
    }

    fn read_pipe(&mut self) -> Token {
        self.source.next();
        match self.source.peek() {
            Some('|') => self.single(Token::PipePipe),
            _ => Token::Pipe,
        }
    }

    fn read_and(&mut self) -> Token {
        self.source.next();
        match self.source.peek() {
            Some('&') => self.single(Token::AndAnd),
            Some(c) => Token::Unexpected(c.to_string()),
            None => Token::Unexpected("".into()),
        }
    }

    /// Reads a new token from the source code. It returns the next token and consumes it.
    pub fn next_token(&mut self) -> Token {
        if let Some(ch) = self.source.peek() {
            match ch {
                '\0' | ' ' | '\t' | '\n' | '\r' => self.skip(),
                '"' => self.read_string(),
                '=' => self.read_equal(),
                '!' => self.read_bang(),
                '(' => self.single(Token::LeftParenthesis),
                ')' => self.single(Token::RightParenthesis),
                '{' => self.single(Token::LeftBrace),
                '}' => self.single(Token::RightBrace),
                '<' => self.read_less_equal(),
                '>' => self.read_greater_equal(),
                '.' => self.single(Token::Dot),
                ',' => self.single(Token::Comma),
                ';' => self.single(Token::Semicolon),
                '|' => self.read_pipe(),
                '&' => self.read_and(),
                '*' => self.single(Token::Mul),
                '/' => self.single(Token::Div),
                '+' => self.single(Token::Plus),
                '-' => self.maybe_arrow(),
                '#' => self.read_comment(),
                ch if ch.is_alphabetic() => self.read_identifier(),
                ch if ch.is_numeric() => self.read_number(),
                _ => self.read_unexpected(),
            }
        } else {
            Token::EOF
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token::EOF => None,
            token => Some(token),
        }
    }
}

fn ne_quote(c: char) -> bool {
    c != '"'
}

#[cfg(test)]
mod test {
    use std::io::Result;

    use super::Lexer;
    use super::Token;

    #[test]
    fn read_empty_string() -> Result<()> {
        let source: String = "\"\"".into();

        let mut lexer = Lexer::new(&source);

        let string_token = lexer.read_string();

        let expected_token = Token::String("".into());
        assert_eq!(string_token, expected_token);

        Ok(())
    }

    #[test]
    fn read_string() -> Result<()> {
        let source: String = "\"purpura\"".into();

        let mut lexer = Lexer::new(&source);

        let string_token = lexer.read_string();

        let expected_token = Token::String("purpura".into());
        assert_eq!(string_token, expected_token);

        Ok(())
    }

    #[test]
    fn maybe_arrow() -> Result<()> {
        let source: String = "->".into();

        let mut lexer = Lexer::new(&source);
        let maybe_arrow = lexer.maybe_arrow();

        let expected_token = Token::Arrow;

        assert_eq!(maybe_arrow, expected_token);

        Ok(())
    }

    #[test]
    fn read_equal() -> Result<()> {
        let source: String = "=".into();

        let mut lexer = Lexer::new(&source);
        let equal = lexer.read_equal();

        let expected_token = Token::Equal;

        assert_eq!(equal, expected_token);

        Ok(())
    }

    #[test]
    fn read_equal_equal() -> Result<()> {
        let source: String = "==".into();

        let mut lexer = Lexer::new(&source);
        let equal_equal = lexer.read_equal();

        let expected_token = Token::EqualEqual;

        assert_eq!(equal_equal, expected_token);

        Ok(())
    }

    #[test]
    fn reads_greater_equal() -> Result<()> {
        let source: String = "> >=".into();

        let lexer = Lexer::new(&source);
        let tokens: Vec<Token> = lexer.collect();

        let expected_tokens = vec![Token::GreaterThan, Token::GreaterEqual];

        assert_eq!(tokens, expected_tokens);

        Ok(())
    }

    #[test]
    fn reads_less_equal() -> Result<()> {
        let source: String = "< <=".into();

        let lexer = Lexer::new(&source);
        let tokens: Vec<Token> = lexer.collect();

        let expected_tokens = vec![Token::LessThan, Token::LessEqual];

        assert_eq!(tokens, expected_tokens);

        Ok(())
    }

    #[test]
    fn read_fat_arrow() -> Result<()> {
        let source: String = "=>".into();

        let mut lexer = Lexer::new(&source);
        let fat_arrow = lexer.read_equal();

        let expected_token = Token::FatArrow;

        assert_eq!(fat_arrow, expected_token);

        Ok(())
    }

    #[test]
    fn read_pipe() -> Result<()> {
        let source: String = "| ||".into();

        let mut lexer = Lexer::new(&source);

        let pipe = lexer.next();
        let pipe_pipe = lexer.next();

        assert_eq!(pipe, Some(Token::Pipe));
        assert_eq!(pipe_pipe, Some(Token::PipePipe));

        Ok(())
    }

    /// Single '&' should not be allowed
    #[test]
    fn read_and() -> Result<()> {
        let source: String = "& &&".into();

        let mut lexer = Lexer::new(&source);

        let and_unexpected = lexer.next();
        let and_and = lexer.next();

        assert_eq!(and_unexpected, Some(Token::Unexpected(" ".into())));
        assert_eq!(and_and, Some(Token::AndAnd));

        Ok(())
    }

    #[test]
    fn reads_binary_op() -> Result<()> {
        let source: String = "*      + - /".into();

        let lexer = Lexer::new(&source);
        let tokens: Vec<Token> = lexer.collect();

        let expected_tokens = vec![Token::Mul, Token::Plus, Token::Minus, Token::Div];

        assert_eq!(tokens, expected_tokens);

        Ok(())
    }

    #[test]
    fn reads_equals() -> Result<()> {
        let source: String = "= == =>".into();

        let lexer = Lexer::new(&source);
        let tokens: Vec<Token> = lexer.collect();

        let expected_tokens = vec![Token::Equal, Token::EqualEqual, Token::FatArrow];

        assert_eq!(tokens, expected_tokens);

        Ok(())
    }

    #[test]
    fn read_bang() -> Result<()> {
        let source: String = "!".into();

        let mut lexer = Lexer::new(&source);
        let bang = lexer.read_bang();

        let expected_token = Token::Bang;

        assert_eq!(bang, expected_token);

        Ok(())
    }

    #[test]
    fn read_not_equal() -> Result<()> {
        let source: String = "!=".into();

        let mut lexer = Lexer::new(&source);
        let not_equal = lexer.read_bang();

        let expected_token = Token::NotEqual;

        assert_eq!(not_equal, expected_token);

        Ok(())
    }

    #[test]
    fn reads_bang() -> Result<()> {
        let source: String = "! !=".into();

        let lexer = Lexer::new(&source);
        let tokens: Vec<Token> = lexer.collect();

        let expected_tokens = vec![Token::Bang, Token::NotEqual];

        assert_eq!(tokens, expected_tokens);

        Ok(())
    }

    #[test]
    fn bi_op_minus() -> Result<()> {
        let source: String = "- ".into();

        let mut lexer = Lexer::new(&source);
        let maybe_arrow = lexer.maybe_arrow();

        let expected_token = Token::Minus;

        assert_eq!(maybe_arrow, expected_token);

        Ok(())
    }

    #[test]
    fn read_single_line_comment() -> Result<()> {
        let source: String = r#"
        # this is a comment
        # this is another comment
        42
        "#
        .into();

        let lexer = Lexer::new(&source);
        let tokens: Vec<Token> = lexer.collect();

        let expected_tokens = vec![
            Token::Comment(" this is a comment".into()),
            Token::Comment(" this is another comment".into()),
            Token::Number(42),
        ];

        assert_eq!(tokens, expected_tokens);

        Ok(())
    }

    #[test]
    fn read_multi_line_comment() -> Result<()> {
        let source: String = r#"
        #[ big
        comment
        ]
        "#
        .into();

        let lexer = Lexer::new(&source);
        let tokens: Vec<Token> = lexer.collect();

        let expected_tokens = vec![Token::Comment(" big\n        comment\n        ".into())];

        assert_eq!(tokens, expected_tokens);

        Ok(())
    }

    #[test]
    fn reads_comments_and_other_tokens() -> Result<()> {
        let source: String = r#"
        # foo
        let x = 42
        #[ bar ]
        "#
        .into();

        let lexer = Lexer::new(&source);
        let tokens: Vec<Token> = lexer.collect();

        let expected_tokens = vec![
            Token::Comment(" foo".into()),
            Token::Let,
            Token::Identifier("x".into()),
            Token::Equal,
            Token::Number(42),
            Token::Comment(" bar ".into()),
        ];

        assert_eq!(tokens, expected_tokens);

        Ok(())
    }

    #[test]
    fn reads_identifiers() -> Result<()> {
        let source: String = "fn let match sig data".into();

        let lexer = Lexer::new(&source);
        let tokens: Vec<Token> = lexer.collect();

        let expected_tokens = vec![
            Token::Fn,
            Token::Let,
            Token::Match,
            Token::Sig,
            Token::Data,
        ];

        assert_eq!(tokens, expected_tokens);

        Ok(())
    }

    #[test]
    fn purpura_let_expression() -> Result<()> {
        let source: String = "let name = \"purpura\";".into();
        let lexer = Lexer::new(&source);

        let tokens: Vec<Token> = lexer.collect();

        let expected_tokens = vec![
            Token::Let,
            Token::Identifier("name".into()),
            Token::Equal,
            Token::String("purpura".into()),
            Token::Semicolon,
        ];

        assert_eq!(tokens, expected_tokens);

        Ok(())
    }

    #[test]
    fn read_lambda() -> Result<()> {
        let source: String = "|x| x".into();

        let tokens: Vec<Token> = Lexer::new(&source).collect();

        let expected_tokens = vec![
            Token::Pipe,
            Token::Identifier("x".into()),
            Token::Pipe,
            Token::Identifier("x".into())
        ];

        assert_eq!(tokens, expected_tokens);
        Ok(())
    }

    #[test]
    fn purpura_match_expression() -> Result<()> {
        let source: String = r#"
        match num {
        | 1 => 2
        | x => 0
        }
        "#
        .into();

        let lexer = Lexer::new(&source);
        let tokens: Vec<Token> = lexer.collect();

        let expected_tokens = vec![
            Token::Match,
            Token::Identifier("num".into()),
            Token::LeftBrace,
            Token::Pipe,
            Token::Number(1),
            Token::FatArrow,
            Token::Number(2),
            Token::Pipe,
            Token::Identifier("x".into()),
            Token::FatArrow,
            Token::Number(0),
            Token::RightBrace,
        ];

        assert_eq!(tokens, expected_tokens);

        Ok(())
    }
}
