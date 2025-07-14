//! Contains the purpura language parser.

use crate::{
    files::{FileId, Files},
    lexer, report,
    tree::{self, parse},
};

pub struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
    current: lexer::Token,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest = 0,
    Logic,
    Sum,
    Product,
    Call,
    End,
}

impl Precedence {
    pub fn left(self) -> Self {
        match self {
            Precedence::Lowest => Precedence::Logic,
            Precedence::Logic => Precedence::Sum,
            Precedence::Sum => Precedence::Product,
            Precedence::Product => Precedence::Call,
            Precedence::Call => Precedence::End,
            Precedence::End => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    Expected(lexer::TokenKind, crate::location::Location),
    Unexpected(crate::location::Location),
}

impl report::Diag for ParseError {
    fn severity(&self) -> report::Severity {
        report::Severity::Error
    }

    fn message(&self) -> String {
        "parse error".to_string()
    }

    fn markers(&self) -> Vec<report::Marker> {
        match self {
            ParseError::Expected(token_kind, location) => vec![report::Marker::new(
                format!("expected '{token_kind:?}'"),
                *location,
            )],
            ParseError::Unexpected(location) => {
                vec![report::Marker::new(
                    "unexpected token".to_string(),
                    *location,
                )]
            }
        }
    }

    fn hint(&self) -> Option<String> {
        None
    }

    fn location(&self) -> crate::location::Location {
        match self {
            ParseError::Expected(_, location) => *location,
            ParseError::Unexpected(location) => *location,
        }
    }
}

macro_rules! infix {
    ($name:ident, $token:path, $op:ident, $precedence:ident) => {
        fn $name(&mut self, lhs: parse::Expression) -> ParseResult<parse::Expression> {
            let op = self.expect($token)?;
            let op = crate::location::Located::new(parse::Op::$op, op.location);
            let rhs = self.infix(Precedence::$precedence.left())?;
            Ok(parse::Expression {
                location: lhs.location.merge(rhs.location),
                kind: Box::new(parse::ExpressionKind::Binary(op, lhs, rhs)),
            })
        }
    };
}

type ParseResult<T> = std::result::Result<T, ParseError>;

impl<'a> Parser<'a> {
    pub fn parse_file(file_id: FileId, files: &'a Files) -> ParseResult<parse::Program> {
        let source = files.get_source(file_id);
        let src = source.as_str();

        let lexer = lexer::Lexer {
            src,
            peekable: src.chars().peekable(),
            index: 0,
            start: 0,
            file_id,
        };

        let mut parser = Self::new(lexer);
        parser.parse_program()
    }

    fn new(mut lexer: lexer::Lexer<'a>) -> Self {
        let current = lexer.next_token();
        Self { lexer, current }
    }

    fn advance(&mut self) -> lexer::Token {
        let next = self.lexer.next_token();
        std::mem::replace(&mut self.current, next)
    }

    fn peek(&self) -> lexer::TokenKind {
        self.current.kind
    }

    fn is(&self, kind: lexer::TokenKind) -> bool {
        self.peek() == kind
    }

    fn expect(&mut self, kind: lexer::TokenKind) -> ParseResult<lexer::Token> {
        if self.is(kind) {
            Ok(self.advance())
        } else {
            Err(ParseError::Expected(kind, self.current.location))
        }
    }

    fn unexpected<T>(&self) -> ParseResult<T> {
        Err(ParseError::Unexpected(self.current.location))
    }

    fn parse_primary(&mut self) -> ParseResult<parse::Expression> {
        match self.peek() {
            lexer::TokenKind::LowerIdent => self.parse_ident_expr(),
            lexer::TokenKind::UpperIdent => self.parse_constructor_expr(),
            lexer::TokenKind::Number => self.parse_number(),
            lexer::TokenKind::String => self.parse_string(),
            lexer::TokenKind::LParens => {
                let lparens = self.expect(lexer::TokenKind::LParens)?;
                let e = self.parse_expression()?;
                let rparens = self.expect(lexer::TokenKind::RParens)?;
                Ok(parse::Expression {
                    location: lparens.location.merge(rparens.location),
                    kind: e.kind,
                })
            }
            _ => self.unexpected()?,
        }
    }

    fn parse_ident_expr(&mut self) -> ParseResult<parse::Expression> {
        let symbol = self.parse_lower_symbol()?;
        Ok(parse::Expression {
            location: symbol.location,
            kind: Box::new(parse::ExpressionKind::Ident(symbol)),
        })
    }

    fn parse_constructor_expr(&mut self) -> ParseResult<parse::Expression> {
        let symbol = self.parse_upper_symbol()?;

        self.expect(lexer::TokenKind::LParens)?;
        let mut arguments = vec![];
        loop {
            if self.is(lexer::TokenKind::RParens) {
                break;
            }

            arguments.push(self.parse_expression()?);

            if self.is(lexer::TokenKind::Comma) {
                self.expect(lexer::TokenKind::Comma)?;
            } else {
                break;
            }
        }
        self.expect(lexer::TokenKind::RParens)?;

        Ok(parse::Expression {
            location: symbol.location,
            kind: Box::new(parse::ExpressionKind::Constructor(symbol, arguments)),
        })
    }

    fn parse_number(&mut self) -> ParseResult<parse::Expression> {
        let n = self.expect(lexer::TokenKind::Number)?;
        let number: i32 = n.lexeme.parse().unwrap();
        Ok(parse::Expression {
            location: n.location,
            kind: Box::new(parse::ExpressionKind::Number(number)),
        })
    }

    fn parse_string(&mut self) -> ParseResult<parse::Expression> {
        let s = self.expect(lexer::TokenKind::String)?;
        Ok(parse::Expression {
            location: s.location,
            kind: Box::new(parse::ExpressionKind::String(s.lexeme)),
        })
    }

    fn parse_block(&mut self) -> ParseResult<parse::Expression> {
        let lbrace = self.expect(lexer::TokenKind::LBrace)?;
        let mut statements = vec![];
        while !self.is(lexer::TokenKind::RBrace) {
            statements.push(self.parse_statement()?);
        }
        let rbrace = self.expect(lexer::TokenKind::RBrace)?;
        Ok(parse::Expression {
            location: lbrace.location.merge(rbrace.location),
            kind: Box::new(parse::ExpressionKind::Block(statements)),
        })
    }

    fn precedence(&self) -> Precedence {
        match self.peek() {
            lexer::TokenKind::LowerIdent => Precedence::Call,
            lexer::TokenKind::UpperIdent => Precedence::Call,
            lexer::TokenKind::Number => Precedence::Call,
            lexer::TokenKind::String => Precedence::Call,
            lexer::TokenKind::Def => panic!(),
            lexer::TokenKind::Let => panic!(),
            lexer::TokenKind::Match => Precedence::Call,
            lexer::TokenKind::Case => Precedence::End,
            lexer::TokenKind::Sig => panic!(),
            lexer::TokenKind::Data => panic!(),
            lexer::TokenKind::LParens => Precedence::Call,
            lexer::TokenKind::RParens => Precedence::End,
            lexer::TokenKind::LBrace => Precedence::Call,
            lexer::TokenKind::RBrace => Precedence::End,
            lexer::TokenKind::Equals => Precedence::Logic,
            lexer::TokenKind::NotEquals => Precedence::Logic,
            lexer::TokenKind::Define => Precedence::End,
            lexer::TokenKind::LessThan => todo!(),
            lexer::TokenKind::LessEqual => todo!(),
            lexer::TokenKind::GreaterThan => todo!(),
            lexer::TokenKind::GreaterEqual => todo!(),
            lexer::TokenKind::Star => Precedence::Product,
            lexer::TokenKind::Slash => Precedence::Product,
            lexer::TokenKind::Plus => Precedence::Sum,
            lexer::TokenKind::Minus => Precedence::Sum,
            lexer::TokenKind::FatArrow => Precedence::End,
            lexer::TokenKind::Comma => Precedence::End,
            lexer::TokenKind::Dot => Precedence::End,
            lexer::TokenKind::Semicolon => Precedence::End,
            lexer::TokenKind::Colon => panic!(),
            lexer::TokenKind::Pipe => panic!(),
            lexer::TokenKind::Arrow => panic!(),
            lexer::TokenKind::Unexpected => panic!(),
            lexer::TokenKind::Eof => Precedence::End,
        }
    }

    fn parse_expression(&mut self) -> ParseResult<parse::Expression> {
        match self.peek() {
            lexer::TokenKind::Match => self.parse_match_expression(),
            lexer::TokenKind::LBrace => self.parse_block(),
            lexer::TokenKind::Pipe => self.parse_lambda(),
            _ => self.infix(Precedence::Lowest),
        }
    }

    fn parse_pattern(&mut self) -> ParseResult<parse::Pattern> {
        match self.peek() {
            lexer::TokenKind::LowerIdent => {
                let symbol = self.parse_lower_symbol()?;
                Ok(parse::Pattern {
                    location: symbol.location,
                    kind: Box::new(parse::PatternKind::Ident(symbol)),
                })
            }
            lexer::TokenKind::UpperIdent => {
                let symbol = self.parse_upper_symbol()?;

                self.expect(lexer::TokenKind::LParens)?;
                let mut patterns = vec![];
                loop {
                    if self.is(lexer::TokenKind::RParens) {
                        break;
                    }

                    patterns.push(self.parse_pattern()?);

                    if self.is(lexer::TokenKind::Comma) {
                        self.expect(lexer::TokenKind::Comma)?;
                    } else {
                        break;
                    }
                }
                let rparens = self.expect(lexer::TokenKind::RParens)?;

                Ok(parse::Pattern {
                    location: symbol.location.merge(rparens.location),
                    kind: Box::new(parse::PatternKind::Constructor(symbol, patterns)),
                })
            }
            lexer::TokenKind::Number => {
                let n = self.expect(lexer::TokenKind::Number)?;
                let number: i32 = n.lexeme.parse().unwrap();
                Ok(parse::Pattern {
                    location: n.location,
                    kind: Box::new(parse::PatternKind::Number(number)),
                })
            }
            lexer::TokenKind::String => {
                let s = self.expect(lexer::TokenKind::String)?;
                Ok(parse::Pattern {
                    location: s.location,
                    kind: Box::new(parse::PatternKind::String(s.lexeme)),
                })
            }
            lexer::TokenKind::LParens => {
                let lparens = self.expect(lexer::TokenKind::LParens)?;
                let pattern = self.parse_pattern()?;

                if self.is(lexer::TokenKind::Colon) {
                    self.expect(lexer::TokenKind::Colon)?;
                    let r#type = self.parse_type()?;
                    let rparens = self.expect(lexer::TokenKind::RParens)?;
                    Ok(parse::Pattern {
                        location: lparens.location.merge(rparens.location),
                        kind: Box::new(parse::PatternKind::Annot(pattern, r#type)),
                    })
                } else {
                    let rparens = self.expect(lexer::TokenKind::RParens)?;
                    Ok(parse::Pattern {
                        location: lparens.location.merge(rparens.location),
                        kind: pattern.kind,
                    })
                }
            }
            _ => self.unexpected()?,
        }
    }

    fn parse_lambda(&mut self) -> ParseResult<parse::Expression> {
        let pipe = self.expect(lexer::TokenKind::Pipe)?;
        let mut parameters = vec![self.parse_lower_symbol()?];
        while !self.is(lexer::TokenKind::Pipe) {
            parameters.push(self.parse_lower_symbol()?);
        }
        self.expect(lexer::TokenKind::Pipe)?;

        let body = self.parse_expression()?;

        Ok(parse::Expression {
            location: pipe.location.merge(body.location),
            kind: Box::new(parse::ExpressionKind::Lambda(parameters, body)),
        })
    }

    fn parse_match_expression(&mut self) -> ParseResult<parse::Expression> {
        let r#match = self.expect(lexer::TokenKind::Match)?;
        let scrutinee = self.parse_expression()?;
        self.expect(lexer::TokenKind::Case)?;

        self.expect(lexer::TokenKind::LBrace)?;
        let mut arms = vec![];

        while !self.is(lexer::TokenKind::RBrace) {
            let pattern = self.parse_pattern()?;
            self.expect(lexer::TokenKind::FatArrow)?;
            let expression = self.parse_expression()?;

            arms.push(parse::Arm {
                pattern,
                expression,
            });

            if self.is(lexer::TokenKind::Comma) {
                self.expect(lexer::TokenKind::Comma)?;
            } else {
                break;
            }
        }
        let rbrace = self.expect(lexer::TokenKind::RBrace)?;

        Ok(parse::Expression {
            location: r#match.location.merge(rbrace.location),
            kind: Box::new(parse::ExpressionKind::Match(scrutinee, arms)),
        })
    }

    infix!(add, lexer::TokenKind::Plus, Add, Sum);
    infix!(sub, lexer::TokenKind::Minus, Sub, Sum);
    infix!(mul, lexer::TokenKind::Star, Mul, Product);
    infix!(div, lexer::TokenKind::Slash, Div, Product);

    fn infix(&mut self, precedence: Precedence) -> ParseResult<parse::Expression> {
        let mut lhs = self.parse_primary()?;

        loop {
            let p = self.precedence();
            if p >= precedence && p != Precedence::End {
                let rule = match self.peek() {
                    lexer::TokenKind::Plus => Self::add,
                    lexer::TokenKind::Minus => Self::sub,
                    lexer::TokenKind::Star => Self::mul,
                    lexer::TokenKind::Slash => Self::div,
                    lexer::TokenKind::Eof | lexer::TokenKind::Unexpected => self.unexpected()?,
                    _ => Self::application,
                };
                lhs = rule(self, lhs)?;
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn application(&mut self, lhs: parse::Expression) -> ParseResult<parse::Expression> {
        let rhs = self.infix(Precedence::Call.left())?;
        Ok(parse::Expression {
            location: lhs.location.merge(rhs.location),
            kind: Box::new(parse::ExpressionKind::Application(lhs, rhs)),
        })
    }

    fn parse_lower_symbol(&mut self) -> ParseResult<tree::Symbol> {
        let ident = self.expect(lexer::TokenKind::LowerIdent)?;
        Ok(tree::Symbol {
            location: ident.location,
            name: ident.lexeme,
        })
    }

    fn parse_upper_symbol(&mut self) -> ParseResult<tree::Symbol> {
        let ident = self.expect(lexer::TokenKind::UpperIdent)?;
        Ok(tree::Symbol {
            location: ident.location,
            name: ident.lexeme,
        })
    }

    fn parse_statement(&mut self) -> ParseResult<parse::Statement> {
        match self.peek() {
            lexer::TokenKind::Let => self.parse_let_statement(),
            _ => {
                let e = self.parse_expression()?;
                Ok(parse::Statement {
                    location: e.location,
                    kind: parse::StatementKind::Expression(e),
                })
            }
        }
    }

    fn parse_let_statement(&mut self) -> ParseResult<parse::Statement> {
        let r#let = self.expect(lexer::TokenKind::Let)?;
        let ident = self.parse_lower_symbol()?;

        let _define = self.expect(lexer::TokenKind::Define)?;

        let value = self.parse_expression()?;
        let semi = self.expect(lexer::TokenKind::Semicolon)?;

        Ok(parse::Statement {
            location: r#let.location.merge(semi.location),
            kind: parse::StatementKind::Let(ident, value),
        })
    }

    fn parse_top_level(&mut self) -> ParseResult<parse::TopLevel> {
        match self.peek() {
            lexer::TokenKind::Data => self.parse_data().map(parse::TopLevel::Data),
            lexer::TokenKind::Sig => self.parse_sig().map(parse::TopLevel::Sig),
            lexer::TokenKind::Def => self.parse_def().map(parse::TopLevel::Def),
            _ => Err(ParseError::Unexpected(self.current.location)),
        }
    }

    fn parse_primary_type(&mut self) -> ParseResult<parse::Type> {
        match self.peek() {
            lexer::TokenKind::LowerIdent => self.parse_type_variable(),
            lexer::TokenKind::UpperIdent => self.parse_generic_type(),
            lexer::TokenKind::LParens => {
                let lparens = self.expect(lexer::TokenKind::LParens)?;
                let r#type = self.parse_type()?;
                let rparens = self.expect(lexer::TokenKind::RParens)?;
                Ok(parse::Type {
                    location: lparens.location.merge(rparens.location),
                    kind: r#type.kind,
                })
            }
            _ => self.unexpected()?,
        }
    }

    fn parse_type_variable(&mut self) -> ParseResult<parse::Type> {
        let symbol = self.parse_lower_symbol()?;
        Ok(parse::Type {
            location: symbol.location,
            kind: Box::new(parse::TypeKind::Var(symbol)),
        })
    }

    fn parse_generic_type(&mut self) -> ParseResult<parse::Type> {
        let symbol = self.parse_upper_symbol()?;
        let mut location = symbol.location;

        let mut generics = vec![];
        if self.is(lexer::TokenKind::LParens) {
            self.expect(lexer::TokenKind::LParens)?;
            loop {
                if self.is(lexer::TokenKind::RParens) {
                    break;
                }

                generics.push(self.parse_primary_type()?);

                if self.is(lexer::TokenKind::Comma) {
                    self.expect(lexer::TokenKind::Comma)?;
                } else {
                    break;
                }
            }
            let rparens = self.expect(lexer::TokenKind::RParens)?;
            location = location.merge(rparens.location);
        }

        Ok(parse::Type {
            location,
            kind: Box::new(parse::TypeKind::Generic(symbol, generics)),
        })
    }

    fn parse_type(&mut self) -> ParseResult<parse::Type> {
        let mut lhs = self.parse_primary_type()?;

        if self.is(lexer::TokenKind::Arrow) {
            self.expect(lexer::TokenKind::Arrow)?;
            let rhs = self.parse_type()?;

            lhs = parse::Type {
                location: lhs.location.merge(rhs.location),
                kind: Box::new(parse::TypeKind::Arrow(lhs, rhs)),
            };
        }

        Ok(lhs)
    }

    fn parse_data(&mut self) -> ParseResult<parse::Data> {
        self.expect(lexer::TokenKind::Data)?;
        let name = self.parse_upper_symbol()?;

        let mut generics = vec![];
        if self.is(lexer::TokenKind::LParens) {
            self.expect(lexer::TokenKind::LParens)?;
            loop {
                generics.push(self.parse_lower_symbol()?);
                if self.is(lexer::TokenKind::Comma) {
                    self.expect(lexer::TokenKind::Comma)?;
                } else {
                    break;
                }
            }
            self.expect(lexer::TokenKind::RParens)?;
        }

        let mut constructors = vec![];

        self.expect(lexer::TokenKind::LBrace)?;
        while !self.is(lexer::TokenKind::RBrace) {
            fn parse_constructor<'a>(slf: &mut Parser<'a>) -> ParseResult<parse::Constructor> {
                let name = slf.parse_upper_symbol()?;

                slf.expect(lexer::TokenKind::LParens)?;
                let mut types = vec![];
                loop {
                    if slf.is(lexer::TokenKind::RParens) {
                        break;
                    }

                    types.push(slf.parse_type()?);

                    if slf.is(lexer::TokenKind::Comma) {
                        slf.expect(lexer::TokenKind::Comma)?;
                    } else {
                        break;
                    }
                }
                slf.expect(lexer::TokenKind::RParens)?;

                Ok(parse::Constructor { name, types })
            }

            constructors.push(parse_constructor(self)?);

            if self.is(lexer::TokenKind::Comma) {
                self.expect(lexer::TokenKind::Comma)?;
            } else {
                break;
            }
        }
        self.expect(lexer::TokenKind::RBrace)?;

        Ok(parse::Data {
            name,
            generics,
            constructors,
        })
    }

    fn parse_sig(&mut self) -> ParseResult<parse::Sig> {
        self.expect(lexer::TokenKind::Sig)?;
        let name = self.parse_lower_symbol()?;

        self.expect(lexer::TokenKind::Colon)?;

        let r#type = self.parse_type()?;

        Ok(parse::Sig { name, r#type })
    }

    fn parse_def(&mut self) -> ParseResult<parse::Def> {
        self.expect(lexer::TokenKind::Def)?;
        let name = self.parse_lower_symbol()?;

        let mut parameters = vec![];
        while !self.is(lexer::TokenKind::Equals) {
            parameters.push(self.parse_lower_symbol()?);
        }

        self.expect(lexer::TokenKind::Equals)?;

        let body = self.parse_expression()?;

        Ok(parse::Def {
            name,
            parameters,
            body,
        })
    }

    fn parse_program(&mut self) -> ParseResult<parse::Program> {
        let mut definitions = vec![];

        while !self.is(lexer::TokenKind::Eof) {
            definitions.push(self.parse_top_level()?);
        }

        Ok(parse::Program { definitions })
    }
}
