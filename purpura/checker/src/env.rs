use std::{cell::RefCell, rc::Rc, str::FromStr};

use location::Location;

use crate::types::{Hole, HoleType, Level, MonoType, PolyType, Type};

#[derive(Clone, Debug)]
pub struct Env {
    pub variables: im_rc::HashMap<String, PolyType>,
    pub type_variables: im_rc::HashMap<String, Type>,
    pub let_decls: im_rc::HashMap<String, PolyType>,
    pub type_decls: im_rc::HashMap<String, usize>,
    pub ctor_decls: im_rc::HashMap<String, (PolyType, usize)>,
    pub level: RefCell<usize>,
    pub location: location::Location,
    pub counter: Rc<RefCell<usize>>,
}

/// [Env] implementation for `variables` and `type_variables`.
impl Env {
    pub fn add_variable(&mut self, name: String, polytype: PolyType) {
        self.variables.insert(name, polytype);
    }

    pub fn add(&mut self, name: String, monotype: Type) {
        self.add_variable(name, PolyType::new(vec![], monotype));
    }

    pub fn add_type_variable(&mut self, name: String, t: Type) {
        self.type_variables.insert(name, t);
    }

    pub fn contains_type_variable(&self, name: String) -> Option<Type> {
        self.type_variables.get(&name).cloned()
    }

    pub fn get_type_variable(&self, name: String) -> Option<&PolyType> {
        self.variables.get(&name)
    }

    pub fn get_variable(&self, name: String) -> Option<&PolyType> {
        self.variables.get(&name)
    }
}

/// [Env] implementation for `level`.
impl Env {
    pub fn enter_level(&self) {
        *self.level.borrow_mut() += 1;
    }

    pub fn leave_level(&self) {
        *self.level.borrow_mut() -= 1;
    }
}

/// [Env] implementation to create new names and holes.
impl Env {
    pub fn new_name(&self) -> String {
        let mut counter = self.counter.borrow_mut();
        let name = String::from_str(&format!("t_{}", *counter)).unwrap();
        *counter += 1;
        name
    }

    pub fn new_hole(&self) -> Type {
        let level = Level(*self.level.borrow());
        Type::new(MonoType::Hole(Hole::new(self.new_name(), level)))
    }
}

/// [Env] implementation for instantiation and generalization.
impl Env {
    pub fn instantiate(&self, polytype: PolyType) -> Type {
        let subs = polytype
            .binds
            .iter()
            .map(|_| self.new_hole())
            .collect::<Vec<_>>();
        polytype.monotype.instantiate(&subs)
    }

    pub fn generalize(&mut self, t: Type) -> PolyType {
        let mut counter = 0;
        let names = (0..counter).map(|_| self.new_name()).collect::<Vec<_>>();
        let level = Level(*self.level.borrow());
        Self::gen(level, t.clone(), &mut counter);
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
                },
                Unbound(_, _) => (),
                Bound(t) => Self::gen(env_level, t, counter),
            },
            MonoType::Arrow(left, right) => {
                Self::gen(env_level.clone(), left.clone(), counter);
                Self::gen(env_level, right.clone(), counter);
            },
            _ => (),
        }
    }
}

/// Implements `new` function for [Env].
impl Env {
    pub fn new() -> Self {
        Self {
            variables: Default::default(),
            type_variables: Default::default(),
            let_decls: Default::default(),
            type_decls: Default::default(),
            level: RefCell::new(Default::default()),
            location: Location::ghost(),
            counter: Rc::new(RefCell::new(0)),
            ctor_decls: Default::default(),
        }
    }
}
