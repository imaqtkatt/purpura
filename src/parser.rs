use crate::{lexer::Lexer, token::Token, expr::{Expr, Operator}};

const PRECEDENCE_TABLE: &'static [[Token; 2]; 4] = &[
    [Token::AndAnd, Token::Pipe],
    [Token::GreaterThan, Token::LessThan],
    [Token::Plus, Token::Minus],
    [Token::Mul, Token::Div],
];

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
        let literal = match self.current.as_ref() {
            Some(Token::Identifier(s)) => {
                Ok(Expr::Identifier(String::clone(s)))
            },
            Some(Token::Number(string_number)) => {
                match string_number.parse::<usize>() {
                    Ok(num) => Ok(Expr::Number(num)),
                    _ => Err(format!("Could not read {:?} as number", string_number))
                }
            }
            _ => panic!("Expected a literal")
        };
        self.advance();
        literal
    }

    fn parse_operator(&mut self) -> Result<Operator, String> {
        let operator = match self.current {
            Some(Token::Mul) => Ok(Operator::Mul),
            Some(Token::Div) => Ok(Operator::Div),
            Some(Token::Plus) => Ok(Operator::Sum),
            Some(Token::Minus) => Ok(Operator::Min),
            Some(Token::GreaterThan) => Ok(Operator::Greater),
            Some(Token::LessThan) => Ok(Operator::Lesser),
            Some(Token::PipePipe) => Ok(Operator::Or),
            Some(Token::AndAnd) => Ok(Operator::And),
            _ => Err("Expected operator".into()),
        };
        self.advance();
        operator
    }

    fn parse_bi_op(&mut self, precedence: usize) -> Result<Expr, String> {
        if precedence > PRECEDENCE_TABLE.len() - 1 {
            return self.parse_literal();
        }
        // println!("{:?}", precedence);
        let mut left = self.parse_bi_op(precedence + 1)?;
        // println!("{:?}", left);
        // println!("{:?}", self.current.as_ref());
        while PRECEDENCE_TABLE[precedence].iter().any(|a| self.current.as_ref().eq(&Some(a))) {
            let operator = self.parse_operator()?;
            let right = self.parse_bi_op(precedence + 1)?;
            left = Expr::BiOp(operator, Box::new(left), Box::new(right))
        }

        Ok(left)
    }
}

#[cfg(test)]
mod test {
    use std::io::Result;

    use crate::expr::{Expr, Operator};

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

        let expected = Ok(Expr::Number(42));

        assert_eq!(number_literal, expected);

        Ok(())
    }

    #[test]
    fn parse_sum_bi_op() -> Result<()> {
        let source: String = "0 + 1".into();

        let mut parser = Parser::new(&source);
        let bi_op = parser.parse_bi_op(0);

        let expected = Expr::BiOp(
            Operator::Sum,
            Box::new(Expr::Number(0)),
            Box::new(Expr::Number(1))
        );

        assert_eq!(bi_op, Ok(expected));

        Ok(())
    }

    #[test]
    fn parse_greater_bi_op() -> Result<()> {
        let source: String = "1 > 1".into();

        let mut parser = Parser::new(&source);
        let bi_op = parser.parse_bi_op(0);

        let expected = Expr::BiOp(
            Operator::Greater,
            Box::new(Expr::Number(1)),
            Box::new(Expr::Number(1)),
        );

        assert_eq!(bi_op, Ok(expected));

        Ok(())
    }

    #[test]
    fn parse_sum_mul_bi_op() -> Result<()> {
        let source: String = "1 + 1 * 2".into();

        let mut parser = Parser::new(&source);
        let bi_op = parser.parse_bi_op(0);

        let expected = Expr::BiOp(
            Operator::Sum,
            Box::new(Expr::Number(1)),
            Box::new(Expr::BiOp(
                Operator::Mul,
                Box::new(Expr::Number(1)),
                Box::new(Expr::Number(2)),
            ))
        );

        assert_eq!(bi_op, Ok(expected));

        Ok(())
    }
}
