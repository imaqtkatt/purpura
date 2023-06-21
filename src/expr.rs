#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Number(u64),
    Identifier(String),
    Binary(Operator, Box<Expr>, Box<Expr>),
    Lambda(String, Box<Expr>),
    Application(Box<Expr>, Vec<Expr>),
    FnDefinition(String, Vec<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<Pattern>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Pattern {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

impl Pattern {
    pub fn new(left: Expr, right: Expr) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operator {
    Mul,
    Div,
    Sum,
    Min,
    Greater,
    Lesser,
    And,
    Or,
}
