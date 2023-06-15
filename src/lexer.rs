use std::{iter::Peekable, str::Chars};
use crate::token::Token;

pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a String) -> Lexer<'a> {
        Lexer {
            source: source.chars().peekable(),
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token();
            match token {
                Token::EOF => break,
                any => tokens.push(any)
            }
        }
        return tokens;
    }

    fn next_token(&mut self) -> Token {
        if let Some(ch) = self.source.peek() {
            match ch {
                '\0' | ' ' | '\t' | '\n' | '\r' => self.skip(),
                '"' => self.read_string(),
                '=' => self.single(Token::Equal),
                '!' => self.single(Token::Bang),
                '(' => self.single(Token::LeftParenthesis),
                ')' => self.single(Token::RightParenthesis),
                '{' => self.single(Token::LeftBrace),
                '}' => self.single(Token::RightBrace),
                '<' => self.single(Token::LessThan),
                '>' => self.single(Token::GreaterThan),
                '.' => self.single(Token::Dot),
                ',' => self.single(Token::Comma),
                ';' => self.single(Token::Semicolon),
                '|' => self.single(Token::Pipe),
                '*' => self.single(Token::BiOp('*')),
                '/' => self.single(Token::BiOp('/')),
                '+' => self.single(Token::BiOp('+')),
                '-' => self.maybe_arrow(),
                ch => {
                    if ch.is_alphabetic() {
                        return self.read_identifier();
                    } else if ch.is_numeric() {
                        return self.read_number();
                    } else {
                        return self.read_unexpected();
                    }
                }
            }
        } else {
            Token::EOF
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

    fn read_identifier(&mut self) -> Token {
        let indentifier = self.read_while(|c| {
            c.is_ascii_alphabetic()
        });
        Token::Identifier(indentifier)
    }

    fn read_number(&mut self) -> Token {
        let number = self.read_while(char::is_numeric);
        Token::Number(number)
    }

    fn read_unexpected(&mut self) -> Token {
        let unexpected = self.read_while(char::is_alphabetic);
        Token::Unexpected(unexpected)
    }

    fn read_string(&mut self) -> Token {
        self.source.next(); // ignore first quote "
        let string = self.read_while(ne_quote);
        Token::String(string)
    }

    fn read_while(&mut self, predicate: fn(char) -> bool) -> String {
        let mut string = String::new();

        while let Some(ch) = self.source.next() {
            if predicate(ch) {
                string.push(ch);
            } else {
                break;
            }
        }

        return string;
    }

    fn maybe_arrow(&mut self) -> Token {
        let asdf = self.source.next();
        println!("{:?}", asdf);
        match self.source.peek() {
            Some('>') => {
                self.source.next();
                Token::Arrow
            },
            _ => Token::BiOp('-')
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
    fn bi_op_minus() -> Result<()> {
        let source: String = "- ".into();

        let mut lexer = Lexer::new(&source);
        let maybe_arrow = lexer.maybe_arrow();

        let expected_token = Token::BiOp('-');

        assert_eq!(maybe_arrow, expected_token);

        Ok(())
    }

    #[test]
    fn lex_let_expression() -> Result<()> {
        let source: String = "let name = \"purpura\";".into();
        let mut lexer = Lexer::new(&source);

        let tokens = lexer.lex();

        let expected_tokens = vec![
            Token::Identifier("let".into()),
            Token::Identifier("name".into()),
            Token::Equal,
            Token::String("purpura".into()),
            Token::Semicolon
        ];

        assert_eq!(tokens, expected_tokens);

        Ok(())
    }
}
