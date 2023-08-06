use location::Spanned;

#[derive(Debug, Clone)]
pub enum ExprKind {
    Number(u64),
    String(String),
    Identifier(String),
    Lambda(String, Box<Expr>),
    Application(Box<Expr>, Vec<Expr>),
    Match(Box<Expr>, Vec<Arm>),
    Block(Vec<Statement>),
}

/// An Expression with a location.
pub type Expr = Spanned<ExprKind>;

#[derive(Debug, Clone)]
pub enum StatementKind {
    Expr(Box<Expr>),
    Let(String, Expr),
}

/// A Statement with a location.
pub type Statement = Spanned<StatementKind>;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum TypeKind {
    Generic(String, Vec<Type>),
    TypeVariable(String),
    Arrow(Box<Type>, Box<Type>),
}

pub type Type = Spanned<TypeKind>;

// #[derive(Debug, Clone)]
// pub enum FnBody {
//     Block(Vec<Statement>),
//     Expr(Box<Expr>),
// }

// pub type FnBody = Box<Expr>;
#[derive(Debug, Clone)]
pub struct FnBody(pub Box<Expr>);

#[derive(Debug, Clone)]
pub enum PatternKind {
    Wildcard,
    Identifier(String),
    Number(u64),
    String(String),
    Application(String, Vec<Pattern>),
}

/// A Pattern with a location.
pub type Pattern = Spanned<PatternKind>;

#[derive(Debug, Clone)]
pub struct Signature {
    pub name: String,
    pub params: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct Constructor {
    pub name: String,
    pub types: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct Data {
    pub name: String,
    pub params: Vec<String>,
    pub ctors: Vec<Constructor>,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub sig: Signature,
    pub name: String,
    pub clauses: Vec<FnClause>,
}

#[derive(Debug, Clone)]
pub struct FnClause {
    pub params: Vec<Pattern>,
    pub body: FnBody,
}

/// An enum containing the top levels of the language.
#[derive(Debug, Clone)]
pub enum TopLevelKind {
    Data(Spanned<Data>),
    FnDecl(Spanned<Fn>),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<TopLevelKind>,
}
