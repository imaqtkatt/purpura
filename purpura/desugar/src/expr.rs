//! Module for the AST of the language. It declares all the parts of a desugared AST that does not
//! contains binary applications or separated sig and fn declarations.

use im_rc::HashSet;
use location::Spanned;

/// An expression is something that can be computed to a value at runtime. It always has a value
/// and a type.
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

/// An expression with a span that locates it in the source code.
pub type Expr = Spanned<ExprKind>;

/// A statement is something that does not have a value at runtime (it returns `()`) and can be
/// chained together in a block.
#[derive(Debug, Clone)]
pub enum StatementKind {
    Expr(Box<Expr>),
    Let(String, Expr),
}

/// A statement with a span that locates it in the source code.
pub type Statement = Spanned<StatementKind>;

/// An arm of a match expression in the format `Pattern => Expr`.
#[derive(Debug, Clone)]
pub struct Arm {
    pub pattern: Box<Pattern>,
    pub body: Box<Expr>,
}

impl Arm {
    pub fn new(left: Pattern, right: Expr) -> Self {
        Self {
            pattern: Box::new(left),
            body: Box::new(right),
        }
    }
}

/// A type is a description of a value. It can be a generic type, a type variable or an arrow type.
/// An arrow type is a function type. For example, `a -> b` is the type of a function that takes a
/// value of type `a` and returns a value of type `b`. A generic type is a type that has type parameters.
/// For example, `Option a` is a generic type that takes a type `a` and returns a type `Option a`.
/// A type variable is a type that is not known yet. It is used during type inference.
#[derive(Debug, Clone)]
pub enum TypeKind {
    Generic(String, Vec<Type>),
    TypeVariable(String),
    Arrow(Box<Type>, Box<Type>),
}

/// A type with a span that locates it in the source code.
pub type Type = Spanned<TypeKind>;

impl TypeKind {
    /// A function to collect all type variables in a type. For example, `Option a` has one type
    /// variable `a`.
    pub fn free_variables(&self) -> HashSet<String> {
        use TypeKind::*;

        match self {
            Generic(_, types) => types
                .iter()
                .flat_map(|t| t.value.free_variables())
                .collect::<HashSet<_>>(),
            TypeVariable(name) => vec![name.clone()].into_iter().collect(),
            Arrow(a, b) => {
                let free = a.value.free_variables();
                free.union(b.value.free_variables())
            }
        }
    }
}

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
