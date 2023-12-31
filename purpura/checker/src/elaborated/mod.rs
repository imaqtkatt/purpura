//! An elaborated tree that contains semantic information.

use location::Spanned;

use crate::types::Type;

#[derive(Debug)]
pub enum ExprKind {
    Number(u64),
    String(String),
    Identifier(String),
    Binary(Operator, Box<Expr>, Box<Expr>),
    Lambda(String, Box<Expr>),
    Application(Box<Expr>, Vec<Expr>),
    Match(Box<Expr>, CaseTree),
    Block(Vec<Statement>),
    Error,
}

#[derive(Debug)]
pub struct CaseTree {
    pub values: Vec<Expr>,
}

/// An Expression with a location.
pub type Expr = Spanned<ExprKind>;

#[derive(Debug)]
pub enum StatementKind {
    Expr(Box<Expr>),
    Let(String, Expr),
}

/// A Statement with a location.
pub type Statement = Spanned<StatementKind>;

// #[derive(Debug)]
// pub enum FnBody {
//     Block(Vec<Statement>),
//     Expr(Box<Expr>),
// }

#[derive(Debug)]
pub struct Signature {
    pub name: String,
    pub params: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug)]
pub struct Constructor {
    pub name: String,
    pub types: Vec<Type>,
}

#[derive(Debug)]
pub struct Data {
    pub name: String,
    pub params: Vec<String>,
    pub ctors: Vec<Constructor>,
}

#[derive(Debug)]
pub struct Fn {
    pub name: String,
    pub body: CaseTree,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operator {
    Mul,
    Div,
    Sum,
    Min,
    Greater,
    Lesser,
    GreaterEqual,
    LessEqual,
    And,
    Or,
}

/// An enum containing the top levels of the language.
#[derive(Debug)]
pub enum TopLevelKind {
    Data(Spanned<Data>),
    FnDecl(Spanned<Fn>),
    Sig(Spanned<Signature>),
    Stmt(Statement),
}
