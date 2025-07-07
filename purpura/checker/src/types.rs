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

impl MonoType {
    pub fn error() -> Type {
        Type::new(Self::Error)
    }

    pub fn to_polytype(t: Rc<Self>) -> PolyType {
        PolyType {
            binds: vec![],
            monotype: t,
        }
    }
}

impl PartialEq for MonoType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (MonoType::Var(x), MonoType::Var(y)) if x == y => true,
            (MonoType::Generalized(x), MonoType::Generalized(y)) if x == y => true,

            (MonoType::Hole(x), MonoType::Hole(y)) if x == y => true,
            (MonoType::Hole(x), y) => match x.get() {
                HoleType::Bound(x_inner) => x_inner.as_ref() == y,
                HoleType::Unbound(..) => false,
            },
            (x, MonoType::Hole(y)) => match y.get() {
                HoleType::Bound(y_inner) => x == y_inner.as_ref(),
                HoleType::Unbound(..) => false,
            },

            (MonoType::Arrow(a, b), MonoType::Arrow(c, d)) => a == c && b == d,

            (MonoType::Ctor(a, b), MonoType::Ctor(c, d)) if a == c && b.len() == d.len() => {
                b.iter().zip(d).all(|(x, y)| x == y)
            }

            (MonoType::Error, _) | (_, MonoType::Error) => false,

            (_, _) => false,
        }
    }
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

    /// Instead of just instantiating the type with rules, this function skolemizes the type. that
    /// means that it replaces all the bound variables with scoped variables.
    pub(crate) fn skolemize(&self) -> Type {
        use MonoType::*;

        let mut subs = vec![];
        for bind in &self.binds {
            subs.push(Type::new(Var(bind.clone())));
        }

        self.monotype.clone().instantiate(&subs)
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
            Ctor(name, types) => {
                let instantiated_types =
                    types.iter().map(|t| t.clone().instantiate(subs)).collect();
                Type::new(Ctor(name.clone(), instantiated_types))
            }
        }
    }
}
