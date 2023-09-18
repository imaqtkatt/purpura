use std::collections::VecDeque;

use desugar::expr::{Pattern, PatternKind};
use location::Location;

use crate::elaborated::case::*;
use crate::{env::Env, types::PolyType};

fn inline<T>(deque: &mut VecDeque<T>, inline: impl IntoIterator<Item = T>) {
    deque.pop_front();
    let rest = std::mem::replace(deque, inline.into_iter().collect());
    deque.extend(rest);
}

#[derive(Clone)]
pub struct Row(Option<usize>, VecDeque<Pattern>);

impl Row {
    pub fn specialize(self, env: &Env, case: Case<()>) -> Option<Self> {
        Some(match (self.get_first_pattern(env), case) {
            (Case::Wildcard, Case::Wildcard) => self.pop_front(),

            (Case::Constructor(name1, args), Case::Constructor(name2, _)) if name1 == name2 => {
                self.inline(args)
            }

            (Case::Wildcard, Case::Constructor(ctor, _)) => todo!(),

            (Case::Constructor(_, _), Case::Number(_)) => todo!(),
            (Case::Constructor(_, _), Case::String(_)) => todo!(),
            (Case::Number(_), Case::Wildcard) => todo!(),
            (Case::Number(_), Case::Constructor(_, _)) => todo!(),
            (Case::Number(_), Case::Number(_)) => todo!(),
            (Case::Number(_), Case::String(_)) => todo!(),
            (Case::String(_), Case::Wildcard) => todo!(),
            (Case::String(_), Case::Constructor(_, _)) => todo!(),
            (Case::String(_), Case::Number(_)) => todo!(),
            (Case::String(_), Case::String(_)) => todo!(),

            _ => return None,
        })
    }

    fn get_first_pattern(&self, env: &Env) -> Case<Pattern> {
        Case::from_pattern(env, self.1.front().unwrap().value.clone())
    }

    pub fn expand(&mut self, size: usize, ctor_name: Option<String>) {
        let vec = self.split_vec(size);

        let data = match ctor_name {
            Some(name) => PatternKind::Application(name, vec),
            None => PatternKind::Wildcard,
        };

        self.1.push_front(Pattern {
            value: data,
            location: Location::ghost(),
        })
    }

    fn split_vec(&mut self, at: usize) -> Vec<Pattern> {
        self.1.make_contiguous();
        let mut vec = self.1.split_off(at);
        std::mem::swap(&mut vec, &mut self.1);
        vec.into()
    }

    fn pop_front(mut self) -> Self {
        self.1.pop_front();
        self
    }

    fn inline(mut self, vec: Vec<Pattern>) -> Self {
        inline(&mut self.1, vec);
        self
    }

    fn is_empty(&self) -> bool {
        self.1.is_empty()
    }
}
