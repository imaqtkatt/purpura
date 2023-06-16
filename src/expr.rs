
#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Number(usize),
    Identifier(String),
}
