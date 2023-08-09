//! Definitions of types used in the type checker. The type checker uses the a HIndley Milner type
// system with some extensions.

use std::{cell::RefCell, rc::Rc};

/// A type without binds.
#[derive(Debug)]
pub enum MonoType {
    Var(String),
    Generalized(usize),
    Hole(Hole),
    Arrow(Type, Type),
    Ctor(String, Vec<Type>),
    Error,
}

/// A reference counted type.
pub type Type = Rc<MonoType>;

/// A polymorphic type.
#[derive(Debug, Clone)]
pub struct PolyType {
    pub binds: Vec<String>,
    pub monotype: Type,
}

impl PolyType {
    pub fn new(binds: Vec<String>, monotype: Type) -> Self {
        Self { binds, monotype }
    }
}

/// The content of a [Hole].
#[derive(Debug, Clone)]
pub enum HoleType {
    Bound(Type),
    Unbound(String, Level),
}

/// A placeholder for an unknown type.
#[derive(Debug, Clone)]
pub struct Hole(Rc<RefCell<HoleType>>);

impl PartialEq for Hole {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Hole {}

impl Hole {
    pub fn new(name: String, level: Level) -> Self {
        Self(Rc::new(RefCell::new(HoleType::Unbound(name, level))))
    }

    pub fn get(&self) -> HoleType {
        self.0.borrow().clone()
    }

    pub fn get_mut(&self) -> std::cell::RefMut<'_, HoleType> {
        self.0.borrow_mut()
    }

    pub fn fill(&self, t: Type) {
        *self.0.borrow_mut() = HoleType::Bound(t);
    }
}

/// A De Bruijin level.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Level(pub usize);

impl Level {
    /// Increments the [Level] value.
    pub fn enter(self) -> Self {
        Self(self.0 + 1)
    }

    /// Decrements the [Level] value.
    pub fn leave(self) -> Self {
        Self(self.0 - 1)
    }
}

impl MonoType {
    pub(crate) fn instantiate(self: Type, subs: &[Type]) -> Type {
        use HoleType::*;
        use MonoType::*;

        match &&*self {
            Var(_) | Error => self.clone(),
            Generalized(n) => subs[*n].clone(),
            Hole(hole) => match hole.get() {
                Unbound(_, _) => self.clone(),
                Bound(t) => t.instantiate(subs),
            },
            Arrow(left, right) => {
                let left = left.clone().instantiate(subs);
                let right = right.clone().instantiate(subs);
                Type::new(Arrow(left, right))
            }
            Ctor(_, _) => todo!(),
        }
    }
}
