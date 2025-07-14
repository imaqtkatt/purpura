use std::{iter::Peekable, str::Chars};

use crate::files::FileId;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum TokenKind {
    LowerIdent,
    UpperIdent,
    Number,
    String,

    Def,
    Let,
    Match,
    Case,
    Sig,
    Data,

    LParens,
    RParens,
    LBrace,
    RBrace,

    Equals,
    NotEquals,
    Define,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Star,
    Slash,
    Plus,
    Minus,

    FatArrow,
    Comma,
    Dot,
    Semicolon,
    Colon,
    Pipe,
    Arrow,

    Unexpected,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub location: crate::location::Location,
}

pub struct Lexer<'a> {
    pub src: &'a str,
    pub peekable: Peekable<Chars<'a>>,
    pub index: usize,
    pub start: usize,

    pub file_id: FileId,
}

impl<'a> Lexer<'a> {
    // pub fn new(file_id: FileId, files: &Files) -> Self {
    //     let source = files.get_source(file_id);
    //     let src = &source.as_ref();
    //     Self {
    //         src,
    //         peekable: src.chars().peekable(),
    //         index: 0,
    //         start: 0,
    //         file_id,
    //     }
    // }

    fn save(&mut self) {
        self.start = self.index;
    }

    fn advance(&mut self) -> Option<char> {
        let char = self.peekable.next()?;
        self.index += char.len_utf8();
        Some(char)
    }

    fn peek(&mut self) -> Option<&char> {
        self.peekable.peek()
    }

    fn skip_while(&mut self, f: impl Fn(&char) -> bool) {
        while let Some(c) = self.peek() {
            if f(c) {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_whitespaces(&mut self) {
        self.skip_while(|c| c.is_ascii_whitespace());
    }

    fn skip_comment(&mut self) {
        self.skip_while(|c| *c != '\n');
    }

    fn skip(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                c if c.is_ascii_whitespace() => self.skip_whitespaces(),
                '#' => self.skip_comment(),
                _ => break,
            }
        }
    }

    fn when(&mut self, f: impl Fn(&char) -> bool) -> bool {
        if self.peek().is_some_and(f) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume(&mut self, c: char) -> bool {
        self.when(|p| *p == c)
    }

    fn current_lexeme(&self) -> String {
        self.src[self.start..self.index].to_owned()
    }

    fn current_location(&self) -> crate::location::Location {
        crate::location::Location {
            start: self.start,
            end: self.index,
            file_id: self.file_id,
        }
    }

    fn read_string(&mut self) -> Token {
        self.save();

        let mut buf = String::with_capacity(16);

        while let Some(c) = self.peek() {
            match c {
                '"' => break,

                '\\' => {
                    // Handle escape sequences
                    self.advance();
                    match self.peek() {
                        Some('"') => buf.push(self.advance().unwrap()),
                        Some(c @ 'n') | Some(c @ 't') | Some(c @ 'r') | Some(c @ '\\')
                        | Some(c @ '0') => {
                            match c {
                                'n' => buf.push('\n'),
                                't' => buf.push('\t'),
                                'r' => buf.push('\r'),
                                '\\' => buf.push('\\'),
                                '0' => buf.push('\0'),
                                _ => unreachable!(),
                            }
                            self.advance();
                        }
                        Some(_) => {
                            // Unknown escape, treat as literal
                            buf.push('\\');
                            buf.push(self.advance().unwrap());
                        }
                        None => {
                            // EOF after backslash
                            let location = self.current_location();
                            return Token {
                                kind: TokenKind::Unexpected,
                                lexeme: buf,
                                location,
                            };
                        }
                    }
                }

                _ => buf.push(self.advance().unwrap()),
            }
        }

        let location = self.current_location();

        if self.consume('"') {
            Token {
                kind: TokenKind::String,
                lexeme: buf,
                location,
            }
        } else {
            Token {
                kind: TokenKind::Unexpected,
                lexeme: buf,
                location,
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip();
        self.save();

        if let Some(c) = self.advance() {
            let kind = match c {
                c if c.is_ascii_lowercase() => {
                    self.skip_while(|c| c.is_ascii_alphanumeric());
                    match &self.src[self.start..self.index] {
                        "def" => TokenKind::Def,
                        "sig" => TokenKind::Sig,
                        "data" => TokenKind::Data,
                        "let" => TokenKind::Let,
                        "match" => TokenKind::Match,
                        "case" => TokenKind::Case,
                        _ => TokenKind::LowerIdent,
                    }
                }
                c if c.is_ascii_uppercase() => {
                    self.skip_while(|c| c.is_ascii_alphanumeric());
                    TokenKind::UpperIdent
                }
                c if c.is_ascii_digit() => {
                    self.skip_while(|c| c.is_ascii_digit());
                    TokenKind::Number
                }

                '"' => return self.read_string(),

                '(' => TokenKind::LParens,
                ')' => TokenKind::RParens,
                '{' => TokenKind::LBrace,
                '}' => TokenKind::RBrace,

                ':' if self.consume('=') => TokenKind::Define,
                '=' if self.consume('>') => TokenKind::FatArrow,
                '=' => TokenKind::Equals,
                '<' if self.consume('>') => TokenKind::NotEquals,
                '<' if self.consume('=') => TokenKind::LessEqual,
                '<' => TokenKind::LessThan,
                '>' if self.consume('=') => TokenKind::GreaterEqual,
                '>' => TokenKind::GreaterThan,
                '*' => TokenKind::Star,
                '/' => TokenKind::Slash,
                '+' => TokenKind::Plus,
                '-' if self.consume('>') => TokenKind::Arrow,
                '-' => TokenKind::Minus,

                ';' => TokenKind::Semicolon,
                ':' => TokenKind::Colon,
                ',' => TokenKind::Comma,
                '.' => TokenKind::Dot,
                '|' => TokenKind::Pipe,

                _ => TokenKind::Unexpected,
            };
            Token {
                kind,
                lexeme: self.current_lexeme(),
                location: self.current_location(),
            }
        } else {
            Token {
                kind: TokenKind::Eof,
                lexeme: self.current_lexeme(),
                location: self.current_location(),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;

    #[test]
    fn test_lexer() {
        let mut files = crate::files::Files::new();
        let path = "./examples/fib.purr";
        let path = std::path::PathBuf::from(path);
        let file_id = files.add_file(&path);
        let source = files.get_source(file_id);
        let src = source.as_str();

        let mut lexer = Lexer {
            src,
            peekable: src.chars().peekable(),
            index: 0,
            start: 0,
            file_id,
        };

        loop {
            let token = lexer.next_token();
            if matches!(token.kind, crate::lexer::TokenKind::Eof) {
                break;
            }
            println!("{token:?}");
        }
    }
}
