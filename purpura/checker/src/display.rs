//! A bunch of implementations of display traits for the types in the type system.

use std::fmt::Display;

use crate::types::{HoleType, MonoType, PolyType};

impl Display for MonoType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_ctx(&[], f)
    }
}

impl Display for HoleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_ctx(&[], f)
    }
}

impl Display for PolyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.monotype.fmt_with_ctx(&self.binds, f)
    }
}

trait FmtCtx {
    fn fmt_with_ctx(&self, ctx: &[String], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

impl FmtCtx for MonoType {
    fn fmt_with_ctx(&self, ctx: &[String], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use MonoType::*;

        fn need_parens(r#type: &MonoType) -> bool {
            match r#type {
                Arrow(..) => true,
                Hole(hole) => match hole.get() {
                    HoleType::Bound(ref r#type) => need_parens(r#type),
                    HoleType::Unbound(..) => false,
                },
                Var(_) | Generalized(_) | Ctor(..) | Error => false,
            }
        }

        match self {
            Var(name) => write!(f, "{}", name.clone()),
            Generalized(n) => write!(
                f,
                "{}",
                ctx.get(*n)
                    .map(|x| format!("{}", x))
                    .unwrap_or_else(|| "?".to_owned())
            ),
            Hole(hole) => hole.get().fmt_with_ctx(ctx, f),
            Arrow(left, right) if need_parens(left) => {
                write!(f, "(")?;
                left.fmt_with_ctx(ctx, f)?;
                write!(f, ")")?;
                write!(f, " -> ")?;
                right.fmt_with_ctx(ctx, f)
            }
            Arrow(left, right) => {
                left.fmt_with_ctx(ctx, f)?;
                write!(f, " -> ")?;
                right.fmt_with_ctx(ctx, f)
            }
            Error => write!(f, "Error"),
            Ctor(name, vars) => {
                write!(f, "{}", name)?;
                write!(f, "(")?;
                for (i, var) in vars.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    var.fmt_with_ctx(ctx, f)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl FmtCtx for HoleType {
    fn fmt_with_ctx(&self, ctx: &[String], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use HoleType::*;

        match self {
            Unbound(name, level) => write!(f, "!{}~{}", name.clone(), level.0),
            Bound(t) => {
                write!(f, "^")?;
                t.fmt_with_ctx(ctx, f)
            }
        }
    }
}
