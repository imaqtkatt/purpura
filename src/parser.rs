use crate::{lexer::Lexer, token::Token, expr::Expr};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Option<Token>,
    next: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a String) -> Self {
        let mut lexer = Lexer::new(source);

        let current = lexer.next();
        let next = lexer.next();
        
        Self {
            lexer,
            current,
            next,
        }
    }

    fn advance(&mut self) -> Option<Token> {
        let mut ret = self.lexer.next();
        std::mem::swap(&mut self.next, &mut self.current);
        std::mem::swap(&mut self.next, &mut ret);
        ret
    }

    pub fn expect(&mut self, expected: Token) -> Result<Option<Token>, String> {
        match self.current.as_ref() {
            Some(a) if *a == expected => Ok(self.advance()),
            a => Err(format!("Unexpected '{:?}'", a))
        }
    }

    fn parse_literal(&mut self) -> Result<Expr, String> {
        match self.current.as_ref() {
            Some(Token::Identifier(s)) => {
                Ok(Expr::Identifier(String::clone(s)))
            },
            Some(Token::Number(_n)) => {
                Ok(Expr::Number(0))
            }
            _ => panic!("Expected a literal")
        }
    }
}

#[cfg(test)]
mod test {
    use std::io::Result;

    use crate::expr::Expr;

    use super::Parser;

    #[test]
    fn parse_identifier_literal() -> Result<()> {
        let source: String = "foo".into();

        let mut parser = Parser::new(&source);
        let identifier = parser.parse_literal();

        let expected = Ok(Expr::Identifier("foo".into()));

        assert_eq!(identifier, expected);

        Ok(())
    }

    #[test]
    fn parse_number_literal() -> Result<()> {
        let source: String = "42".into();

        let mut parser = Parser::new(&source);
        let number_literal = parser.parse_literal();

        let expected = Ok(Expr::Number(0));

        assert_eq!(number_literal, expected);

        Ok(())
    }
}
