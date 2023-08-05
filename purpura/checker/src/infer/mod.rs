pub mod expr;
pub mod typ;
pub mod pat;
pub mod top_level;

use crate::{env::Env, types::Type};

pub trait Infer {
    type Out;

    fn infer(self, env: Env) -> (Self::Out, Type);
}
