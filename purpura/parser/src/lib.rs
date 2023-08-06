//! Parser for the purpura language, the main structure of this module is the [Parser] that turns a
//! stream of tokens into a tree.

pub mod expr;
pub mod spanned;
pub mod program;

use crate::expr::*;

use expr::{PatternKind, StatementKind, TypeKind};
use lexer::{token::Token, Lexer};
use location::Spanned;
use spanned::{Span, LocWith};

type Result<T> = std::result::Result<T, String>;

const PRECEDENCE_TABLE: &[&[Token]] = &[
    &[Token::AndAnd, Token::PipePipe],
    &[Token::GreaterThan, Token::LessThan, Token::GreaterEqual, Token::LessEqual],
    &[Token::Plus, Token::Minus],
    &[Token::Mul, Token::Div],
];

/// This is the parser of the purpura language, it takes a stream of tokens and turns it into a
/// tree.
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Spanned<Token>,
    next: Spanned<Token>,
}

impl<'a> Parser<'a> {
    /// Creates a new Parser from the given `source`.
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer::new(source);

        let current = lexer.next_token_spanned();
        let next = lexer.next_token_spanned();

        Self { lexer, current, next }
    }

    /// Returns true if the current token is the same as the `other`.
    fn is(&self, other: Token) -> bool {
        self.current.value == other
    }

    /// Advances the state of the Lexer.
    fn advance(&mut self) -> Spanned<Token> {
        let mut ret = self.lexer.next_token_spanned();
        std::mem::swap(&mut self.next, &mut self.current);
        std::mem::swap(&mut self.next, &mut ret);
        ret
    }

    /// Returns Ok(...) if the current token is equal to the `expected`.
    fn expect(&mut self, expected: Token) -> Result<Spanned<Token>> {
        match &self.current {
            a if a.value == expected => Ok(self.advance()),
            _ => Err(format!("Expected {:?}, got {:?}", expected, self.current)),
        }
    }

    /// Expects that the current token is an identifier.
    fn expect_identifier(&mut self) -> Result<String> {
        match &self.current.value {
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
    /// Parses a Primary Expression.
    fn primary(&mut self) -> Result<Expr> {
        let literal = match &self.current.value {
            Token::Identifier(id) => Ok(Spanned::new(
                ExprKind::Identifier(id.clone()),
                self.current.location,
            )),
            Token::Number(num) => Ok(Spanned::new(
                ExprKind::Number(*num),
                self.current.location,
            )),
            Token::String(string) => Ok(Spanned::new(
                ExprKind::String(string.clone()),
                self.current.location,
            )),
            Token::LeftParenthesis => {
                let start = self.current.location;
                self.advance();
                let expr = self.expr()?;
                let end = self.expect(Token::RightParenthesis)?;
                
                return Ok(Spanned::new(expr.value, start.with(&end)));
            }
            _ => Err(format!("Expected a literal, got {:?}", self.current.value)),
        };
        self.advance();
        literal
    }

    /// Parses an Operator.
    ///
    /// # Operators
    /// ```
    /// [*, /, +, -, >, <, ||, &&]
    /// ```
    fn operator(&mut self) -> Result<Operator> {
        let operator = match self.current.value {
            Token::Mul => Ok(Operator::Mul),
            Token::Div => Ok(Operator::Div),
            Token::Plus => Ok(Operator::Sum),
            Token::Minus => Ok(Operator::Min),
            Token::GreaterThan => Ok(Operator::Greater),
            Token::LessThan => Ok(Operator::Lesser),
            Token::GreaterEqual => Ok(Operator::GreaterEqual),
            Token::LessEqual => Ok(Operator::LessEqual),
            Token::PipePipe => Ok(Operator::Or),
            Token::AndAnd => Ok(Operator::And),
            _ => Err("Expected operator".into()),
        };
        self.advance();
        operator
    }

    /// Parses a call Expression.
    ///
    /// # Example
    /// ```
    /// succ(41)
    /// ```
    pub fn call(&mut self) -> Result<Expr> {
        let mut args = Vec::new();
        let callee = self.primary()?;
        let callee_location = callee.location;

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

            let r_paren = self.expect(Token::RightParenthesis)?;

            Ok(Spanned::new(
                ExprKind::Application(Box::new(callee), args),
                callee_location.with(&r_paren),
            ))
        } else {
            Ok(callee)
        }
    }

    /// Parses a Binary Expression.
    ///
    /// # Example
    /// ```
    /// 10 > 9
    /// ```
    fn infix(&mut self, precedence: usize) -> Result<Expr> {
        if precedence > PRECEDENCE_TABLE.len() - 1 {
            return self.call();
        }

        let mut left = self.infix(precedence + 1)?;
        let left_location = left.location;

        while PRECEDENCE_TABLE[precedence]
            .iter()
            .any(|a| self.current.value == *a)
        {
            let operator = self.operator()?;
            let right = self.infix(precedence + 1)?;
            let right_location = right.location;
            left = Spanned::new(
                ExprKind::Binary(operator, Box::new(left), Box::new(right)),
                left_location.with(&right_location),
            );
        }

        Ok(left)
    }

    /// Parses a lambda Expression.
    ///
    /// # Example
    /// ```
    /// |x| x
    /// ```
    pub fn lambda(&mut self) -> Result<Expr> {
        let pipe = self.expect(Token::Pipe)?;
        let identifier = self.expect_identifier()?;
        self.expect(Token::Pipe)?;
        let body = self.expr()?;

        let body_location = body.location;

        Ok(Spanned::new(
            ExprKind::Lambda(identifier, Box::new(body)),
            pipe.with(&body_location),
        ))
    }

    /// Parses a block Expression.
    ///
    /// # Example
    /// ```
    /// {
    ///   let x = 1;
    ///   x
    /// }
    /// ```
    pub fn block(&mut self) -> Result<Expr> {
        let l_brace = self.expect(Token::LeftBrace)?;

        let mut statements = Vec::new();

        while !self.is(Token::RightBrace) {
            statements.push(self.statement()?);
            if self.is(Token::Semicolon) {
                self.advance();
            }
        }

        let r_brace = self.expect(Token::RightBrace)?;

        Ok(Spanned::new(
            ExprKind::Block(statements),
            l_brace.with(&r_brace),
        ))
    }

    /// Recursive function to parse a Pattern.
    fn pattern_of_expr(&self, expr: Expr) -> Result<Pattern> {
        let pattern = match expr.value {
            ExprKind::Identifier(i) => Spanned::new(
                PatternKind::Identifier(i),
                expr.location,
            ),
            ExprKind::Number(n) => Spanned::new(
                PatternKind::Number(n),
                expr.location,
            ),
            ExprKind::String(s) => Spanned::new(
                PatternKind::String(s),
                expr.location,
            ),
            ExprKind::Application(expr, exprs) => {
                let identifier: Result<String> = match expr.value {
                    ExprKind::Identifier(i) => Ok(i.clone()),
                    _ => Err("Expected identifier".into()),
                };
                let identifier = identifier?;

                let mut params: Vec<Pattern> = Vec::new();
                for expr in exprs {
                    params.push(self.pattern_of_expr(expr)?);
                }

                Spanned::new(
                    PatternKind::Application(identifier, params),
                    expr.location,
                )
            }
            _ => return Err("Expected".into()),
        };
        return Ok(pattern);
    }

    /// Returns the Pattern of an Expression.
    fn pattern(&mut self) -> Result<Pattern> {
        let expr = self.expr()?;
        self.pattern_of_expr(expr)
    }

    /// Parses a match Expression.
    ///
    /// # Example
    /// ```
    /// match 1 + 1 {
    /// | 1 => 42,
    /// | x => 2,
    /// }
    /// ```
    pub fn match_expr(&mut self) -> Result<Expr> {
        let match_kw = self.expect(Token::Match)?;
        let expr = self.expr()?;
        self.expect(Token::LeftBrace)?;

        let mut patterns = Vec::new();

        while !self.is(Token::RightBrace) {
            self.expect(Token::Pipe)?;
            let left = self.pattern()?;
            self.expect(Token::FatArrow)?;
            let right = self.expr()?;

            let pattern = Arm::new(left, right);
            patterns.push(pattern);

            if self.is(Token::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        let r_brace = self.expect(Token::RightBrace)?;

        Ok(Spanned::new(
            ExprKind::Match(Box::new(expr), patterns),
            match_kw.with(&r_brace),
        ))
    }

    /// Parses a let Statement.
    ///
    /// # Example
    /// ```
    /// let x = "foo";
    /// ```
    fn let_statement(&mut self) -> Result<Statement> {
        let let_kw = self.expect(Token::Let)?;
        let identifier = self.expect_identifier()?;
        self.expect(Token::Equal)?;
        let expr = self.expr()?;

        let semicolon = self.expect(Token::Semicolon)?;

        Ok(Spanned::new(
            StatementKind::Let(identifier, expr),
            let_kw.with(&semicolon),
        ))
    }

    /// Parses a Function.
    ///
    /// # Example
    /// ```
    /// fn succ(x) = x + 1
    /// ```
    pub fn parse_fn(&mut self) -> Result<Spanned<Fn>> {
        let fn_kw = self.expect(Token::Fn)?;
        let name = self.expect_identifier()?;
        self.expect(Token::LeftParenthesis)?;

        let mut params = Vec::new();

        while !self.is(Token::RightParenthesis) {
            params.push(self.pattern()?);
            if self.is(Token::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(Token::RightParenthesis)?;

        self.expect(Token::Equal)?;

        let expr = self.expr()?;
        let expr_location = expr.location;

        let body = Box::new(expr);

        let fun = Fn { name, params, body };
        Ok(Spanned::new(fun, fn_kw.with(&expr_location)))
    }

    /// Parses an arrow type.
    /// 
    /// # Example
    /// ```
    /// Nat -> Nat
    /// ```
    fn parse_type(&mut self) -> Result<Type> {
        let left = self.parse_generic_type()?;
        let left_location = left.location;

        if self.is(Token::Arrow) {
            self.advance();
            let right = self.parse_type()?;
            let right_location = right.location;

            let arrow = Spanned::new(
                TypeKind::Arrow(Box::new(left), Box::new(right)),
                left_location.with(&right_location)
            );

            Ok(arrow)
        } else {
            Ok(left)
        }
    }

    /// Parses a Type.
    ///
    /// # Examples
    /// ```
    /// Bool     // Identifier
    /// Maybe<a> // Generic
    /// a        // TypeVariable
    /// ```
    fn parse_generic_type(&mut self) -> Result<Type> {
        if self.is(Token::Comma) {
            self.advance();
        }
        let type_location = self.current.location;
        let type_id = self.expect_identifier()?;

        if self.is(Token::LessThan) {
            self.advance();

            let mut types: Vec<Type> = Vec::new();

            while !self.is(Token::GreaterThan) {
                types.push(self.parse_type()?);
                if self.is(Token::Comma) {
                    self.advance();
                }
            }
            let gt = self.expect(Token::GreaterThan)?;
            let location = type_location.with(&gt);

            return Ok(Spanned::new(
                TypeKind::Generic(type_id, types),
                location
            ));
        }

        let first_char = type_id.chars().next().unwrap();

        if first_char.is_uppercase() {
            Ok(Spanned::new(TypeKind::Generic(type_id, vec![]), type_location))
        } else {
            Ok(Spanned::new(TypeKind::TypeVariable(type_id), type_location))
        }
    }

    /// Parses a Signature.
    ///
    /// # Example
    /// ```
    /// sig succ(Nat) -> Nat
    /// ```
    pub fn parse_sig(&mut self) -> Result<Spanned<Signature>> {
        let sig_kw = self.expect(Token::Sig)?;
        let name = self.expect_identifier()?;
        self.expect(Token::LeftParenthesis)?;

        let mut params: Vec<Type> = Vec::new();

        while !self.is(Token::RightParenthesis) {
            params.push(self.parse_type()?);
        }

        self.expect(Token::RightParenthesis)?;
        self.expect(Token::Arrow)?;

        let return_type = self.parse_type()?;
        let location = sig_kw.with(&return_type);

        let signature = Signature { name, params, return_type };
        Ok(Spanned::new(signature, location))
    }

    /// Parses a Data Type.
    ///
    /// # Example
    /// ```
    /// data Maybe<a> {
    ///     Just(a),
    ///     None(),
    /// }
    /// ```
    pub fn parse_data(&mut self) -> Result<Spanned<Data>> {
        let data_kw = self.expect(Token::Data)?;
        let name = self.expect_identifier()?;

        let mut params = Vec::new();

        if self.is(Token::LessThan) {
            self.advance();
            while !self.is(Token::GreaterThan) {
                match self.parse_type()?.value {
                    TypeKind::TypeVariable(type_variable) => {
                        params.push(type_variable);
                    }
                    _ => return Err("Expected type variable".into()),
                }
                if self.is(Token::Comma) {
                    self.advance();
                }
            }
            self.expect(Token::GreaterThan)?;
        }

        self.expect(Token::LeftBrace)?;

        let mut ctors: Vec<Constructor> = Vec::new();

        while !self.is(Token::RightBrace) {
            let name = self.expect_identifier()?;
            self.expect(Token::LeftParenthesis)?;

            let mut types: Vec<Type> = Vec::new();

            while !self.is(Token::RightParenthesis) {
                types.push(self.parse_type()?);
            }
            self.expect(Token::RightParenthesis)?;

            let ctor = Constructor { name, types };

            ctors.push(ctor);

            if self.is(Token::Comma) {
                self.advance();
            }
        }
        let r_brace = self.expect(Token::RightBrace)?;

        let data = Data { name, params, ctors };

        let location = data_kw.with(&r_brace);
        Ok(Spanned::new(data, location))
    }

    /// Parses an Expression.
    ///
    /// # Example
    /// ```
    /// |x| x + 1
    /// ```
    pub fn expr(&mut self) -> Result<Expr> {
        match self.current.value {
            Token::LeftBrace => self.block(),
            Token::Match => self.match_expr(),
            Token::Pipe => self.lambda(),
            _ => self.infix(0),
        }
    }

    /// Parses a [StatementKind::Expr].
    pub fn statement_expr(&mut self) -> Result<Statement> {
        let expr = self.expr()?;
        let location = expr.location;
        let stmt = StatementKind::Expr(Box::new(expr));
        Ok(Spanned::new(stmt, location))
    }

    /// Parses a Statement.
    ///
    /// # Examples
    ///
    /// ```
    /// 1 + 1
    /// let x = 1 + 1;
    /// ```
    pub fn statement(&mut self) -> Result<Statement> {
        match self.current.value {
            Token::Let => self.let_statement(),
            _ => self.statement_expr(),
        }
    }

    pub fn top_level(&mut self) -> Result<TopLevelKind> {
        match self.current.value {
            Token::Data => {
                let data = self.parse_data()?;
                Ok(TopLevelKind::Data(data))
            },
            Token::Fn => {
                let fun = self.parse_fn()?;
                Ok(TopLevelKind::FnDecl(fun))
            },
            Token::Sig => {
                let sig = self.parse_sig()?;
                Ok(TopLevelKind::Sig(sig))
            },
            Token::EOF => Err("Reached EOF".into()),
            _ => {
                panic!("Is not a top level token")
                // let stmt = self.statement()?;
                // Ok(TopLevelKind::Stmt(stmt))
            }
        }
    }

    /// Parses a program and returns the root of the tree.
    pub fn parse(&mut self) -> Result<program::Program> {
        let mut decls = Vec::new();
        while let Ok(tl) = self.top_level() {
            decls.push(tl);
        }
        let program = program::Program { decls };
        Ok(program)
    }
}

#[cfg(test)]
mod test {
    use super::Parser;

    type Test = Result<(), String>;

    #[test]
    fn test() -> Test {
        let source = r#"sig id(a) -> a

fn id(x) = x"#
        .trim_start();

        let mut parser = Parser::new(source);
        let x = parser.parse()?;

        println!("{:#?}", x.decls);

        Ok(())
    }
}

/*
#[cfg(test)]
mod test {
    use std::io::Result;

    use crate::expr::{Expr, Operator, Arm};

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
        /*
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
        */

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
                Arm::new(
                    Expr::Number(1),
                    Expr::Number(1)
                ),
                Arm::new(
                    Expr::Identifier("x".into()),
                    Expr::Identifier("x".into())
                ),
            ],
        );

        assert_eq!(expr, Ok(expected));

        Ok(())
    }
}
*/
