use crate::{env::Env, types::{self, Type, Hole}};

pub fn unify(env: Env, t1: Type, t2: Type) {
    use types::MonoType::*;

    match (&*t1, &*t2) {
        (Var(left), Var(right)) if left == right => (),

        (Generalized(left), Generalized(right)) if left == right => (),

        (Hole(left), Hole(right)) if left == right => (),
        (Hole(val), _) => unify_hole(env, val.clone(), t2.clone(), false),
        (_, Hole(val)) => unify_hole(env, val.clone(), t1.clone(), true),

        (Arrow(l1, r1), Arrow(l2, r2)) => {
            unify(env.clone(), l1.clone(), l2.clone());
            unify(env, r1.clone(), r2.clone());
        },

        (_, _) => panic!("Type mismatch between '{}' and '{}'", &t1, &t2),
    }
}

pub fn unify_hole(env: Env, hole: Hole, val: Type, flip: bool) {
    use types::HoleType::*;

    match hole.get() {
        Unbound(_, _) => {
            if occurs(hole.clone(), val.clone()) {
                panic!("occur checking")
            } else {
                *hole.get_mut() = Bound(val)
            }
        },
        Bound(value) if flip => unify(env, val, value),
        Bound(value) => unify(env, value, val),
    }
}

fn occurs(hole: Hole, t: Type) -> bool {
    use types::MonoType::*;

    match &*t {
        Var(_) | Generalized(_) | Error => false,
        Hole(val) => val.clone() == hole,
        Arrow(left, right) => {
            occurs(hole.clone(), left.clone()) || occurs(hole.clone(), right.clone())
        },
        Ctor(_, _) => todo!(),
    }
}
