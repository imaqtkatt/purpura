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

    fn next_token(&mut self) -> Token {
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
        let identifier = self.read_while(|c| {
            c.is_ascii_alphabetic()
        });
        match identifier.as_ref() {
            "fn" => Token::Fn,
            "let" => Token::Let,
            "match" => Token::Match,
            _ => Token::Identifier(identifier)
        }
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
        self.source.next();
        match self.source.peek() {
            Some('>') => self.single(Token::Arrow),
            _ => Token::BiOp('-')
        }
    }

    fn read_equal(&mut self) -> Token {
        self.source.next();
        match self.source.peek() {
            Some('=') => self.single(Token::EqualEqual),
            Some('>') => self.single(Token::FatArrow),
            _ => Token::Equal
        }
    }

    fn read_bang(&mut self) -> Token {
        self.source.next();
        match self.source.peek() {
            Some('=') => self.single(Token::NotEqual),
            _ => Token::Bang
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token::EOF => None,
            token => Some(token)
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
    fn read_fat_arrow() -> Result<()> {
        let source: String = "=>".into();

        let mut lexer = Lexer::new(&source);
        let fat_arrow = lexer.read_equal();

        let expected_token = Token::FatArrow;

        assert_eq!(fat_arrow, expected_token);

        Ok(())
    }

    #[test]
    fn reads_equals() -> Result<()> {
        let source: String = "= == =>".into();

        let lexer = Lexer::new(&source);
        let tokens: Vec<Token> = lexer.into_iter().collect();

        let expected_tokens = vec![
            Token::Equal,
            Token::EqualEqual,
            Token::FatArrow
        ];

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
        let tokens: Vec<Token> = lexer.into_iter().collect();
        
        let expected_tokens = vec![
            Token::Bang,
            Token::NotEqual
        ];

        assert_eq!(tokens, expected_tokens);

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
    fn reads_identifiers() -> Result<()> {
        let source: String = "fn let match".into();

        let lexer = Lexer::new(&source);
        let tokens: Vec<Token> = lexer.into_iter().collect();

        let expected_tokens = vec![
            Token::Fn,
            Token::Let,
            Token::Match
        ];

        assert_eq!(tokens, expected_tokens);

        Ok(())
    }

    #[test]
    fn purpura_let_expression() -> Result<()> {
        let source: String = "let name = \"purpura\";".into();
        let lexer = Lexer::new(&source);

        let tokens: Vec<Token> = lexer.into_iter().collect();

        let expected_tokens = vec![
            Token::Let,
            Token::Identifier("name".into()),
            Token::Equal,
            Token::String("purpura".into()),
            Token::Semicolon
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
        "#.into();

        let lexer = Lexer::new(&source);
        let tokens: Vec<Token> = lexer.into_iter().collect();

        let expected_tokens = vec![
            Token::Match,
            Token::Identifier("num".into()),
            Token::LeftBrace,
            Token::Pipe,
            Token::Number("1".into()),
            Token::FatArrow,
            Token::Number("2".into()),
            Token::Pipe,
            Token::Identifier("x".into()),
            Token::FatArrow,
            Token::Number("0".into()),
            Token::RightBrace
        ];

        assert_eq!(tokens, expected_tokens);

        Ok(())
    }
}
