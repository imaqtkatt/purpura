use desugar::expr::{Pattern, PatternKind};

use crate::{env::Env, types::PolyType};

#[derive(Clone)]
pub enum Case<T> {
    Wildcard,
    Constructor(String, (PolyType, usize), Vec<T>),
    Number(u64),
    String(String),
}

impl Case<Pattern> {
    pub fn from_pattern(env: &Env, pat: PatternKind) -> Self {
        match pat {
            PatternKind::Wildcard => Case::Wildcard,
            PatternKind::Identifier(_) => Case::Wildcard,
            PatternKind::Number(n) => Case::Number(n),
            PatternKind::String(s) => Case::String(s),
            PatternKind::Application(name, args) => {
                let ctor = env.get_ctor(name.clone());
                Case::Constructor(name, ctor.unwrap().clone(), args)
            }
        }
    }
}
