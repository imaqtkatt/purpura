use crate::{
    env::Env,
    types::{self, Hole, Type},
};

pub fn unify(env: Env, t1: Type, t2: Type) {
    if !unify_go(env, t1.clone(), t2.clone()) {
        panic!("cannot unify {} and {}", t1, t2)
    }
}

fn unify_go(env: Env, t1: Type, t2: Type) -> bool {
    use types::MonoType::*;

    match (&*t1, &*t2) {
        (Var(left), Var(right)) if left == right => true,

        (Generalized(left), Generalized(right)) if left == right => true,

        (Hole(left), Hole(right)) if left == right => true,
        (Hole(val), _) => unify_hole(env, val.clone(), t2.clone(), false),
        (_, Hole(val)) => unify_hole(env, val.clone(), t1.clone(), true),

        (Arrow(l1, r1), Arrow(l2, r2)) => {
            unify_go(env.clone(), l1.clone(), l2.clone()) && unify_go(env, r1.clone(), r2.clone())
        }

        (_, _) => false,
    }
}

pub fn unify_hole(env: Env, hole: Hole, val: Type, flip: bool) -> bool {
    use types::HoleType::*;

    match hole.get() {
        Unbound(_, _) => {
            if occurs(hole.clone(), val.clone()) {
                panic!("occur checking")
            } else {
                *hole.get_mut() = Bound(val);
                true
            }
        }
        Bound(value) if flip => unify_go(env, val, value),
        Bound(value) => unify_go(env, value, val),
    }
}

fn occurs(hole: Hole, t: Type) -> bool {
    use types::MonoType::*;

    match &*t {
        Var(_) | Generalized(_) | Error => false,
        Hole(val) => val.clone() == hole,
        Arrow(left, right) => occurs(hole.clone(), left.clone()) || occurs(hole, right.clone()),
        Ctor(_, args) => args.iter().any(|arg| occurs(hole.clone(), arg.clone())),
    }
}
