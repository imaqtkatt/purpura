use location::Spanned;

#[derive(Debug)]
pub enum ExprKind {
    Number(u64),
    String(String),
    Identifier(String),
    Binary(Operator, Box<Expr>, Box<Expr>),
    Lambda(String, Box<Expr>),
    Application(Box<Expr>, Vec<Expr>),
    Match(Box<Expr>, Vec<Arm>),
    Block(Vec<Statement>),
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

#[derive(Debug)]
pub struct Arm {
    pub left: Box<Pattern>,
    pub right: Box<Expr>,
}

impl Arm {
    pub fn new(left: Pattern, right: Expr) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

#[derive(Debug)]
pub enum TypeKind {
    Generic(String, Vec<Type>),
    TypeVariable(String),
    Arrow(Box<Type>, Box<Type>),
}

pub type Type = Spanned<TypeKind>;

#[derive(Debug)]
pub enum FnBody {
    Block(Vec<Statement>),
    Expr(Box<Expr>),
}

#[derive(Debug)]
pub enum PatternKind {
    Wildcard,
    Identifier(String),
    Number(u64),
    String(String),
    Application(String, Vec<Pattern>),
}

/// A Pattern with a location.
pub type Pattern = Spanned<PatternKind>;

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
    pub params: Vec<Pattern>,
    pub body: FnBody,
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
