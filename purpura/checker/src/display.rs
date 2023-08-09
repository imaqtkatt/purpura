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
            Arrow(left, right) => {
                write!(f, "(")?;
                left.fmt_with_ctx(ctx, f)?;
                write!(f, " -> ")?;
                right.fmt_with_ctx(ctx, f)?;
                write!(f, ")")
            }
            Error => write!(f, "Error"),
            Ctor(name, vars) => {
                write!(f, "(")?;
                write!(f, "{}", name)?;
                for var in vars {
                    write!(f, " ")?;
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
