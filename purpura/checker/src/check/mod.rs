use crate::{env::Env, types::Type};

pub trait Check {
    type Out;

    fn check(self, env: Env, t: Type) -> Self::Out;
}
