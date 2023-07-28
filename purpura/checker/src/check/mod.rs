// pub mod expr;
// pub mod typ;
// pub mod pat;
// pub mod top_level;

use crate::{env::Env, types::Type};

pub trait Check {
    type Out;

    fn check(self, env: Env, t: Type) -> Self::Out;
}
