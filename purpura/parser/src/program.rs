use std::fmt::Debug;

use crate::expr::TopLevelKind;

#[derive(Debug)]
pub struct Program {
    pub decls: Vec<TopLevelKind>,
}
