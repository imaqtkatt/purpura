use std::collections::VecDeque;

use desugar::expr::{Pattern, PatternKind};
use location::Location;

use crate::{
    env::Env,
    types::{MonoType, Type},
};

use super::case::Case;

#[derive(Clone)]
pub struct Row<T>(VecDeque<T>);

pub fn wildcard() -> Pattern {
    location::Spanned {
        value: PatternKind::Wildcard,
        location: Location::ghost(),
    }
}

impl Row<Pattern> {
    pub fn expand(&mut self, size: usize, name: String) {
        let vec = self.split_vec(size);

        let data = PatternKind::Application(name, vec.into());

        self.0.push_front(Pattern {
            value: data,
            location: Location::ghost(),
        })
    }

    fn specialize(&self, case: Case<()>) -> Vec<Self> {
        match (case, self.0.front().unwrap().value.clone()) {
            (Case::Wildcard, PatternKind::Wildcard | PatternKind::Identifier(_)) => {
                vec![self.clone().pop_front()]
            }

            (Case::Constructor(n, _, _), PatternKind::Application(name1, args)) if n == name1 => {
                vec![self.clone().inline(args)]
            }

            (Case::Constructor(_, _, s), PatternKind::Wildcard | PatternKind::Identifier(_)) => {
                let args = vec![wildcard(); s.len()];
                vec![self.clone().inline(args)]
            }

            (Case::Number(x), PatternKind::Number(y)) if x == y => vec![self.clone().pop_front()],
            (Case::Number(_), PatternKind::Wildcard | PatternKind::Identifier(_)) => {
                vec![self.clone().pop_front()]
            }

            (Case::String(x), PatternKind::String(y)) if x == y => vec![self.clone().pop_front()],
            (Case::String(_), PatternKind::Wildcard | PatternKind::Identifier(_)) => {
                vec![self.clone().pop_front()]
            }

            _ => vec![],
        }
    }

    fn head(&self) -> Option<&Pattern> {
        self.0.front()
    }
}

impl<T> Row<T> {
    fn split_vec(&mut self, at: usize) -> VecDeque<T> {
        self.0.make_contiguous();
        let mut vec = self.0.split_off(at);
        std::mem::swap(&mut vec, &mut self.0);
        vec
    }

    fn pop_front(mut self) -> Self {
        self.0.pop_front();
        self
    }

    fn inline(mut self, vec: Vec<T>) -> Self {
        let deque: &mut VecDeque<T> = &mut self.0;
        deque.pop_front();
        let rest = std::mem::replace(deque, vec.into_iter().collect());
        deque.extend(rest);
        self
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

pub struct Matrix<T>(VecDeque<Row<T>>);

impl Matrix<Pattern> {
    pub fn specialize(&self, case: Case<()>) -> Self {
        Matrix(
            self.0
                .iter()
                .flat_map(|row| row.specialize(case.clone()))
                .collect(),
        )
    }

    fn get_used_constructor(env: &Env, pattern: &Pattern) -> Option<String> {
        match &pattern.value {
            PatternKind::Application(name, _) => Some(name.clone()),
            PatternKind::Identifier(id) => env.get_ctor(id.clone()).map(|_| id.clone()),
            PatternKind::Wildcard => todo!(),
            PatternKind::Number(_) => todo!(),
            PatternKind::String(_) => todo!(),
        }
    }

    pub fn get_used_constructors(&self, env: &Env) -> Vec<String> {
        self.0
            .iter()
            .flat_map(|row| Self::get_used_constructor(env, &row.head().unwrap()))
            .collect()
    }

    pub fn is_complete_type_sig(&self, env: &Env, name: String) {
        let names = env.data_ctor_names.get(&name);

        todo!()
    }
}

pub struct Problem {
    pub types: Row<Type>,
    pub case: Row<Pattern>,
    pub matrix: Matrix<Pattern>,
}

impl Problem {
    pub fn is_empty(&self) -> bool {
        self.matrix.0.is_empty()
    }

    pub fn is_exhaustive(&self) -> bool {
        self.case.is_empty() || !self.matrix.0.is_empty()
    }

    fn current_type(&self) -> Type {
        self.types.0.front().cloned().unwrap()
    }

    pub fn specialize(&self, case: Case<()>) -> Self {
        let mut row = self.types.clone();
        let first = row.0.pop_front().unwrap();

        let types = match (case.clone(), first.as_ref()) {
            (Case::Constructor(_, typ, _), MonoType::Ctor(_, args)) => {
                let typ = typ.0.monotype.instantiate(args);

                let args = typ.get_arrow_args();

                self.types.clone().inline(args)
            }
            _ => row.pop_front(),
        };

        Problem {
            types,
            case: self
                .case
                .clone()
                .specialize(case.clone())
                .pop()
                .unwrap_or(Row(VecDeque::default())),
            matrix: self.matrix.specialize(case),
        }
    }
}
