use desugar::expr::{ExprKind, StatementKind};
use location::{Location, Spanned};

use crate::{
    elaborated as elab,
    env::Env,
    infer::typ,
    types::{MonoType, PolyType, Type},
    unify,
};

use super::Infer;

impl Infer for ExprKind {
    type Out = elab::ExprKind;

    fn infer(self, env: Env) -> (Self::Out, Type) {
        use ExprKind::*;

        match self {
            Number(n) => (elab::ExprKind::Number(n), typ::type_number()),
            String(s) => (elab::ExprKind::String(s.clone()), typ::type_string()),
            Identifier(name) => match env.get_variable(name.clone()) {
                Some(t) => {
                    let tau = env.instantiate(t.clone());
                    (elab::ExprKind::Identifier(name.clone()), tau)
                }
                None => {
                    println!("Variable '{}' not found in ctx", name);
                    error()
                }
            },
            Application(e1, e2) => {
                let (elab_arrow, mut t_e1) = e1.infer(env.clone());

                let mut args = Vec::new();

                for expr in e2 {
                    match &*t_e1 {
                        MonoType::Arrow(a, b) => {
                            let (elab_t_a, t_a) = expr.infer(env.clone());
                            unify::unify(env.clone(), t_a, a.clone());
                            t_e1 = b.clone();
                            args.push(elab_t_a);
                        }
                        _ => {
                            println!("The type '{}' is not a function", t_e1);
                            return error();
                        }
                    }
                }
                (
                    elab::ExprKind::Application(Box::new(elab_arrow), args),
                    t_e1,
                )
            }
            Lambda(x, e) => {
                let new_hole = env.new_hole();
                let polytype = PolyType::new(vec![], new_hole.clone());

                let mut new_env = env.clone();
                new_env.add_variable(x.clone(), polytype);

                let inferred = e.infer(new_env);
                let arrow = Type::new(MonoType::Arrow(new_hole, inferred.1.clone()));

                (elab::ExprKind::Lambda(x, Box::new(inferred.0)), arrow)
            }
            Match(scrutinee, arms) => {
                let ret_type = env.new_hole();

                let (elab_scrutinee, scrutinee_t) = scrutinee.infer(env.clone());

                let mut values = Vec::new();

                for arm in arms {
                    let (bindings, pattern_type) = arm.left.infer(env.clone());

                    let mut env = env.clone();
                    for bind in bindings {
                        let (name, monotype) = bind;

                        let polytype = PolyType::new(vec![], monotype);
                        env.add_variable(name, polytype);
                    }

                    let (elab_arm, t) = arm.right.infer(env.clone());
                    values.push(elab_arm);

                    unify::unify(env.clone(), scrutinee_t.clone(), pattern_type);
                    unify::unify(env, ret_type.clone(), t);
                }
                let case_tree = elab::CaseTree { values };

                (
                    elab::ExprKind::Match(Box::new(elab_scrutinee), case_tree),
                    ret_type,
                )
            }
            Block(stmts) => {
                let mut ret_type = unit();

                let mut env = env.clone();

                let mut elab_stmts = Vec::new();

                for stmt in stmts {
                    let (elab_stmt, t) = stmt.infer(env.clone());
                    ret_type = t;
                    env = elab_stmt.value.1.clone();
                    elab_stmts.push(elab_stmt.map(|x| x.0));
                }

                (elab::ExprKind::Block(elab_stmts), ret_type)
            }
        }
    }
}

impl Infer for StatementKind {
    type Out = (elab::StatementKind, Env);

    fn infer(self, mut env: Env) -> (Self::Out, Type) {
        match self {
            StatementKind::Expr(e) => {
                let (inferred, t) = e.infer(env.clone());
                ((elab::StatementKind::Expr(Box::new(inferred)), env), t)
            }
            StatementKind::Let(bind, e) => {
                env.enter_level();
                let inferred = e.infer(env.clone());
                env.leave_level();

                let generalized = env.generalize(inferred.1);

                let mut new_env = env.clone();
                new_env.add_variable(bind.clone(), generalized);

                let stmt_kind = elab::StatementKind::Let(bind, inferred.0);
                ((stmt_kind, new_env), unit())
            }
        }
    }
}

impl<T: Infer> Infer for Spanned<T> {
    type Out = Spanned<T::Out>;

    fn infer(self, env: Env) -> (Self::Out, Type) {
        let (value, t) = self.value.infer(env);

        let spanned = Spanned {
            location: self.location.clone(),
            value,
        };

        (spanned, t)
    }
}

fn error() -> (elab::ExprKind, std::rc::Rc<MonoType>) {
    (elab::ExprKind::Error, Type::new(MonoType::Error))
}

fn unit() -> std::rc::Rc<MonoType> {
    Type::new(MonoType::Var("Unit".to_string()))
}
