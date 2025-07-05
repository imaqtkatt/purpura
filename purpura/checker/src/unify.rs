use report::ToDiagnostic;

use crate::{
    env::Env,
    types::{self, Hole, Type},
};

struct OccursCheck {
    between: (Hole, Type),
}

struct UnifyError {
    between: (Type, Type),
}

type UnifyResult = std::result::Result<(), ()>;

pub fn unify(env: Env, t1: Type, t2: Type) {
    _ = unify_go(env.clone(), t1.clone(), t2.clone());
}

fn unify_go(env: Env, t1: Type, t2: Type) -> UnifyResult {
    use types::MonoType::*;
    // println!("unify({t1:?}, {t2:?})");

    match (&*t1, &*t2) {
        (Var(left), Var(right)) if left == right => Ok(()),

        (Generalized(left), Generalized(right)) if left == right => Ok(()),

        (Hole(left), Hole(right)) if left == right => Ok(()),
        (Hole(val), _) => unify_hole(env, val.clone(), t2.clone(), false),
        (_, Hole(val)) => unify_hole(env, val.clone(), t1.clone(), true),

        (Arrow(a, b), Arrow(c, d)) => {
            unify_go(env.clone(), a.clone(), c.clone())?;
            unify_go(env, b.clone(), d.clone())
        }

        (Ctor(a, b), Ctor(c, d)) if *a == *c && b.len() == d.len() => {
            for (x, y) in b.iter().zip(d) {
                unify_go(env.clone(), x.clone(), y.clone())?;
            }
            Ok(())
        }

        (_, _) => {
            env.reporter.report(UnifyError { between: (t1, t2) });
            Err(())
        }
    }
}

pub fn unify_hole(env: Env, hole: Hole, val: Type, flip: bool) -> UnifyResult {
    use types::HoleType::*;

    match hole.get() {
        Unbound(_, _) => {
            if occurs(hole.clone(), val.clone()) {
                env.reporter.report(OccursCheck {
                    between: (hole, val),
                });
                Err(())
            } else {
                *hole.get_mut() = Bound(val);
                Ok(())
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
        Hole(val) => {
            // TODO: is this correct?
            if *val == hole {
                true
            } else {
                match val.get() {
                    types::HoleType::Bound(inner) => occurs(hole, inner),
                    types::HoleType::Unbound(..) => false,
                }
            }
        }
        Arrow(left, right) => occurs(hole.clone(), left.clone()) || occurs(hole, right.clone()),
        Ctor(_, args) => args.iter().any(|arg| occurs(hole.clone(), arg.clone())),
    }
}

impl ToDiagnostic for OccursCheck {
    fn message(&self) -> String {
        format!(
            "occurs check between {:?} and {}",
            self.between.0, self.between.1
        )
    }

    fn markers(&self) -> Vec<report::Marker> {
        vec![]
    }

    fn severity(&self) -> report::Severity {
        report::Severity::Error
    }

    fn location(&self) -> location::Location {
        location::Location::ghost()
    }
}

impl ToDiagnostic for UnifyError {
    fn message(&self) -> String {
        format!(
            "type mismatch between {} and {}",
            self.between.0, self.between.1
        )
    }

    fn markers(&self) -> Vec<report::Marker> {
        vec![]
    }

    fn severity(&self) -> report::Severity {
        report::Severity::Error
    }

    fn location(&self) -> location::Location {
        location::Location::ghost()
    }
}
