//! Environment for type checking. It contains a struct called [Env] that stores a bunch of things
//! that are useful for type checking and inference.

use std::{cell::RefCell, rc::Rc, str::FromStr};

use location::Location;

use crate::types::{Hole, HoleType, Level, MonoType, PolyType, Type};

/// Main environment for type checking. It contains a bunch of things that are useful for type
/// checking and inference.
#[derive(Clone)]
pub struct Env {
    pub variables: im_rc::HashMap<String, PolyType>,
    pub type_variables: im_rc::HashMap<String, Type>,
    pub let_decls: im_rc::HashMap<String, PolyType>,
    pub type_decls: im_rc::HashMap<String, usize>,
    pub ctor_decls: im_rc::HashMap<String, (PolyType, usize)>,
    pub level: RefCell<usize>,
    pub location: location::Location,
    pub counter: Rc<RefCell<usize>>,
    pub reporter: report::Reporter,
}

/// [Env] implementation for `variables` and `type_variables`.
impl Env {
    /// Adds a variable type to the environment
    pub fn add_variable(&mut self, name: String, polytype: PolyType) {
        self.variables.insert(name, polytype);
    }

    /// Adds a monotype to the environment
    pub fn add(&mut self, name: String, monotype: Type) {
        self.add_variable(name, PolyType::new(vec![], monotype));
    }

    /// Adds a type variable to the environment
    pub fn add_type_variable(&mut self, name: String, t: Type) {
        self.type_variables.insert(name, t);
    }

    /// Gets a type variable from the environment
    pub fn get_type_variable(&self, name: String) -> Option<Type> {
        self.type_variables.get(&name).cloned()
    }

    /// Gets a variable from the environment
    pub fn get_variable(&self, name: String) -> Option<&PolyType> {
        self.variables.get(&name)
    }
}

/// [Env] implementation for `level`.
impl Env {
    /// Increases the level by one.
    pub fn enter_level(&self) {
        *self.level.borrow_mut() += 1;
    }

    /// Decreases the level by one.
    pub fn leave_level(&self) {
        *self.level.borrow_mut() -= 1;
    }
}

/// [Env] implementation to create new names and holes.
impl Env {
    /// Generates a new name for a type.
    pub fn new_name(&self) -> String {
        let mut counter = self.counter.borrow_mut();
        let name = String::from_str(&format!("t_{}", *counter)).unwrap();
        *counter += 1;
        name
    }

    /// Creates a new hole
    pub fn new_hole(&self) -> Type {
        let level = Level(*self.level.borrow());
        Type::new(MonoType::Hole(Hole::new(self.new_name(), level)))
    }
}

/// [Env] implementation for instantiation and generalization.
impl Env {
    /// Instantiates a polytype into a monotype by replacing all the bound variables with holes.
    pub fn instantiate(&self, polytype: PolyType) -> Type {
        let subs = polytype
            .binds
            .iter()
            .map(|_| self.new_hole())
            .collect::<Vec<_>>();

        polytype.monotype.instantiate(&subs)
    }

    /// Generalizes a type by getting all free variables (holes) and filling it with a variable
    /// that will be bind by a forall.
    pub fn generalize(&mut self, t: Type) -> PolyType {
        let mut counter = 0;
        let level = Level(*self.level.borrow());

        Self::gen(level, t.clone(), &mut counter);
        let names = (0..counter).map(|_| self.new_name()).collect::<Vec<_>>();

        PolyType::new(names, t)
    }

    fn gen(env_level: Level, t: Type, counter: &mut usize) {
        use HoleType::*;

        match &*t {
            MonoType::Hole(hole) => match hole.get() {
                Unbound(_, level) if level.0 > env_level.0 => {
                    let gen_level = *counter;
                    *counter += 1;
                    hole.fill(Type::new(MonoType::Generalized(gen_level)))
                }
                Unbound(_, _) => (),
                Bound(t) => Self::gen(env_level, t, counter),
            },
            MonoType::Arrow(left, right) => {
                Self::gen(env_level.clone(), left.clone(), counter);
                Self::gen(env_level, right.clone(), counter);
            }
            _ => (),
        }
    }
}

impl Env {
    pub fn new(reporter: report::Reporter) -> Self {
        Env {
            variables: Default::default(),
            type_variables: Default::default(),
            let_decls: Default::default(),
            type_decls: {
              let mut hs: im_rc::HashMap<String, usize> = Default::default();
              hs.insert(String::from("Number"), 0);
              hs
            },
            level: RefCell::new(Default::default()),
            location: Location::ghost(),
            counter: Rc::new(RefCell::new(0)),
            ctor_decls: Default::default(),
            reporter,
        }
    }
}
