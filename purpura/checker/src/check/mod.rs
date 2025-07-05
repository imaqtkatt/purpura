mod expr;

use location::Spanned;

use crate::{env::Env, types::Type};

pub trait Check {
    type Out;

    fn check(self, env: Env, t: Type) -> Self::Out;
}

impl<T: Check> Check for Spanned<T> {
    type Out = Spanned<T::Out>;

    fn check(self, env: Env, t: Type) -> Self::Out {
        self.map(|val| val.check(env, t))
    }
}
