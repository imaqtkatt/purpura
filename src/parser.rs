//! Parser for the purpura language, the main structure of this module is the [Parser] that turns a
//! stream of tokens into a tree.

use crate::{
    expr::{Expr, Operator, Pattern},
    lexer::Lexer,
    token::Token,
};

type Result<T> = std::result::Result<T, String>;

const PRECEDENCE_TABLE: &[&[Token]] = &[
    &[Token::AndAnd, Token::PipePipe],
    &[Token::GreaterThan, Token::LessThan],
    &[Token::Plus, Token::Minus],
    &[Token::Mul, Token::Div],
];

/// This is the parser of the purpura language, it takes a stream of tokens and turns it into a
/// tree.
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
    next: Token,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer::new(source);

        let current = lexer.next();
        let next = lexer.next();

        Self {
            lexer,
            current: current.unwrap_or(Token::EOF),
            next: next.unwrap_or(Token::EOF),
        }
    }

    fn is(&self, token: Token) -> bool {
        self.current == token
    }

    fn advance(&mut self) -> Token {
        let mut ret = self.lexer.next().unwrap_or(Token::EOF);
        std::mem::swap(&mut self.next, &mut self.current);
        std::mem::swap(&mut self.next, &mut ret);
        ret
    }

    fn expect(&mut self, expected: Token) -> Result<Token> {
        match &self.current {
            a if *a == expected => Ok(self.advance()),
            _ => Err(format!("Expected {:?}, got {:?}", expected, self.current)),
        }
    }

    fn expect_identifier(&mut self) -> Result<String> {
        match &self.current {
            Token::Identifier(str) => {
                let str = str.clone();
                self.advance();
                Ok(str)
            }
            _ => Err(format!("Expected identifier, got {:?}", self.current)),
        }
    }
}

impl<'a> Parser<'a> {
    fn primary(&mut self) -> Result<Expr> {
        let literal = match &self.current {
            Token::Identifier(str) => Ok(Expr::Identifier(str.clone())),
            Token::Number(num) => Ok(Expr::Number(*num)),
            Token::String(s) => Ok(Expr::String(s.clone())),
            Token::LeftParenthesis => {
                self.advance();
                let expr = self.expr()?;
                self.expect(Token::RightParenthesis)?;
                return Ok(expr);
            }
            _ => Err(format!("Expected a literal, got {:?}", self.current)),
        };
        self.advance();
        literal
    }

    fn operator(&mut self) -> Result<Operator> {
        let operator = match self.current {
            Token::Mul => Ok(Operator::Mul),
            Token::Div => Ok(Operator::Div),
            Token::Plus => Ok(Operator::Sum),
            Token::Minus => Ok(Operator::Min),
            Token::GreaterThan => Ok(Operator::Greater),
            Token::LessThan => Ok(Operator::Lesser),
            Token::PipePipe => Ok(Operator::Or),
            Token::AndAnd => Ok(Operator::And),
            _ => Err("Expected operator".into()),
        };
        self.advance();
        operator
    }

    pub fn call(&mut self) -> Result<Expr> {
        let mut args = Vec::new();
        let callee = self.primary()?;
        if self.is(Token::LeftParenthesis) {
            self.advance();

            while !self.is(Token::RightParenthesis) {
                args.push(self.expr()?);
                if self.is(Token::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }

            self.expect(Token::RightParenthesis)?;

            Ok(Expr::Application(Box::new(callee), args))
        } else {
            Ok(callee)
        }
    }

    fn infix(&mut self, precedence: usize) -> Result<Expr> {
        if precedence > PRECEDENCE_TABLE.len() - 1 {
            return self.call();
        }

        let mut left = self.infix(precedence + 1)?;

        while PRECEDENCE_TABLE[precedence]
            .iter()
            .any(|a| self.current == *a)
        {
            let operator = self.operator()?;
            let right = self.infix(precedence + 1)?;
            left = Expr::Binary(operator, Box::new(left), Box::new(right))
        }

        Ok(left)
    }

    pub fn lambda(&mut self) -> Result<Expr> {
        self.expect(Token::Pipe)?;
        let identifier = self.expect_identifier()?;
        self.expect(Token::Pipe)?;
        let body = self.expr()?;
        Ok(Expr::Lambda(identifier, Box::new(body)))
    }

    pub fn match_expr(&mut self) -> Result<Expr> {
        self.expect(Token::Match)?;
        let expr = self.expr()?;
        self.expect(Token::LeftBrace)?;
        
        let mut patterns = Vec::new();

        while !self.is(Token::RightBrace) {
            self.expect(Token::Pipe)?;
            let left = self.expr()?;
            self.expect(Token::FatArrow)?;
            let right = self.expr()?;
            
            let pattern = Pattern::new(left, right);
            patterns.push(pattern);

            if self.is(Token::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(Token::RightBrace)?;
        
        Ok(Expr::Match(Box::new(expr), patterns))
    }

    pub fn fn_definition(&mut self) -> Result<Expr> {
        self.expect(Token::Fn)?;
        let identifier = self.expect_identifier()?;
        self.expect(Token::LeftParenthesis)?;

        let mut args = Vec::new();

        while !self.is(Token::RightParenthesis) {
            args.push(self.primary()?);
            if self.is(Token::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(Token::RightParenthesis)?;

        self.expect(Token::Equal)?;

        let expr = self.expr()?;

        self.expect(Token::Semicolon)?;

        Ok(Expr::FnDefinition(identifier, args, Box::new(expr)))
    }

    pub fn expr(&mut self) -> Result<Expr> {
        match self.current {
            Token::Fn => self.fn_definition(),
            Token::Match => self.match_expr(),
            Token::Pipe => self.lambda(),
            _ => self.infix(0),
        }
    }

    /// Parses a program and returns the root of the tree.
    pub fn parse(&mut self) -> Result<Expr> {
        self.expr()
    }
}

#[cfg(test)]
mod test {
    use std::io::Result;

    use crate::expr::{Expr, Operator, Pattern};

    use super::Parser;

    #[test]
    fn parse_identifier_literal() -> Result<()> {
        let source: String = "foo".into();

        let mut parser = Parser::new(&source);
        let identifier = parser.primary();

        let expected = Ok(Expr::Identifier("foo".into()));

        assert_eq!(identifier, expected);

        Ok(())
    }

    #[test]
    fn parse_number_literal() -> Result<()> {
        let source: String = "42".into();

        let mut parser = Parser::new(&source);
        let number_literal = parser.primary();

        let expected = Ok(Expr::Number(42));

        assert_eq!(number_literal, expected);

        Ok(())
    }

    #[test]
    fn parse_sum_bi_op() -> Result<()> {
        let source: String = "0 + 1".into();

        let mut parser = Parser::new(&source);
        let bi_op = parser.infix(0);

        let expected = Expr::Binary(
            Operator::Sum,
            Box::new(Expr::Number(0)),
            Box::new(Expr::Number(1)),
        );

        assert_eq!(bi_op, Ok(expected));

        Ok(())
    }

    #[test]
    fn parse_greater_bi_op() -> Result<()> {
        let source: String = "1 > 1".into();

        let mut parser = Parser::new(&source);
        let bi_op = parser.infix(0);

        let expected = Expr::Binary(
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
        let bi_op = parser.infix(0);

        let expected = Expr::Binary(
            Operator::Sum,
            Box::new(Expr::Number(1)),
            Box::new(Expr::Binary(
                Operator::Mul,
                Box::new(Expr::Number(1)),
                Box::new(Expr::Number(2)),
            )),
        );

        assert_eq!(bi_op, Ok(expected));

        Ok(())
    }

    #[test]
    fn test_call_and_lambda() -> Result<()> {
        let source: String = "|x| x(y,z)".into();

        let mut parser = Parser::new(&source);
        let call = parser.expr();

        let expected = Expr::Lambda(
            "x".into(),
            Box::new(Expr::Application(
                Box::new(Expr::Identifier("x".into())),
                vec![Expr::Identifier("y".into()), Expr::Identifier("z".into())],
            )),
        );

        assert_eq!(call, Ok(expected));
        Ok(())
    }

    #[test]
    fn test_fn_definition() -> Result<()> {
        let source: String = r#"
        fn id(x) = x;
        "#.into();

        let mut parser = Parser::new(&source);
        let fn_definition = parser.fn_definition();

        let expected = Expr::FnDefinition(
            "id".into(),
            vec![Expr::Identifier("x".into())],
            Box::new(Expr::Identifier("x".into())),
        );

        assert_eq!(fn_definition, Ok(expected));

        Ok(())
    }

    #[test]
    fn test_match() -> Result<()> {
        let source: String = r#"
        match num {
        | 1 => 1,
        | x => x,
        }
        "#.into();

        let mut parser = Parser::new(&source);

        let expr = parser.match_expr();

        let expected = Expr::Match(
            Box::new(Expr::Identifier("num".into())),
            vec![
                Pattern::new(
                    Expr::Number(1),
                    Expr::Number(1)
                ),
                Pattern::new(
                    Expr::Identifier("x".into()),
                    Expr::Identifier("x".into())
                ),
            ],
        );

        assert_eq!(expr, Ok(expected));
        
        Ok(())
    }
}
