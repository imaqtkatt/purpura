
#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Number(usize),
    Identifier(String),
    BiOp(Operator, Box<Expr>, Box<Expr>)
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
