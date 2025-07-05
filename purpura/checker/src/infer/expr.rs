use desugar::expr::{ExprKind, StatementKind, TypeKind};
use location::Spanned;

use crate::{
    elaborated as elab,
    env::Env,
    infer::{errors::InferError, typ},
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
            String(s) => (elab::ExprKind::String(s), typ::type_string()),
            Identifier(name) => match env.get_variable(&name) {
                Some(t) => {
                    let tau = env.instantiate(t.clone());
                    (elab::ExprKind::Identifier(name), tau)
                }
                None => match env.let_decls.get(&name).cloned() {
                    Some(v) => {
                        let tau = env.instantiate(v);
                        (elab::ExprKind::Identifier(name), tau)
                    }
                    None => match env.ctor_decls.get(&name).cloned() {
                        Some((t, _)) => {
                            let tau = env.instantiate(t);
                            (elab::ExprKind::Identifier(name), tau)
                        }
                        None => {
                            let err = InferError(format!("Variable '{}' not found in ctx", name));
                            env.reporter.report(err);
                            elab_error()
                        }
                    },
                },
            },
            Application(e1, e2) => {
                let (elab_arrow, t_e1) = e1.infer(env.clone());
                // println!("callee type: {t_e1:?}");

                let mut args = Vec::new();

                let mut inferred_args = vec![];

                for expr in e2 {
                    let (elab_t_a, t_a) = expr.infer(env.clone());
                    inferred_args.push(t_a);
                    args.push(elab_t_a);
                }

                let return_type = env.new_hole();
                let inferred_type = inferred_args
                    .into_iter()
                    .rfold(return_type.clone(), |acc, n| {
                        Type::new(MonoType::Arrow(n, acc))
                    });
                unify::unify(env, inferred_type, t_e1);

                (
                    elab::ExprKind::Application(Box::new(elab_arrow), args),
                    return_type,
                )
            }
            Lambda(x, e) => {
                let new_hole = env.new_hole();
                let polytype = MonoType::to_polytype(new_hole.clone());

                let mut new_env = env;
                new_env.add_variable(x.clone(), polytype);

                let (body, body_t) = e.infer(new_env);
                let arrow = Type::new(MonoType::Arrow(new_hole, body_t.clone()));

                (elab::ExprKind::Lambda(x, Box::new(body)), arrow)
            }
            Match(scrutinee, arms) => {
                let ret_type = env.new_hole();

                let (elab_scrutinee, scrutinee_t) = scrutinee.infer(env.clone());

                let mut values = Vec::new();

                for arm in arms {
                    let (bindings, pattern_type) = arm.pattern.infer(env.clone());

                    let mut env = env.clone();
                    for bind in bindings {
                        let (name, monotype) = bind;

                        env.add_variable(name, MonoType::to_polytype(monotype));
                    }

                    let (elab_arm, t) = arm.body.infer(env.clone());
                    values.push(elab_arm);

                    unify::unify(env.clone(), pattern_type, scrutinee_t.clone());
                    unify::unify(env, t, ret_type.clone());
                }
                let case_tree = elab::CaseTree { values };

                (
                    elab::ExprKind::Match(Box::new(elab_scrutinee), case_tree),
                    ret_type,
                )
            }
            Block(stmts) => {
                let mut ret_type = unit();

                let mut env = env;

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
            StatementKind::Let(bind, value) => {
                env.enter_level();
                let (value, value_t) = value.infer(env.clone());
                env.leave_level();

                let generalized = env.generalize(value_t);

                let mut new_env = env.clone();
                new_env.add_variable(bind.clone(), generalized);

                ((elab::StatementKind::Let(bind, value), new_env), unit())
            }
        }
    }
}

impl<T: Infer> Infer for Spanned<T> {
    type Out = Spanned<T::Out>;

    fn infer(self, env: Env) -> (Self::Out, Type) {
        let (value, t) = self.value.infer(env);

        let spanned = Spanned {
            location: self.location,
            value,
        };

        (spanned, t)
    }
}

fn elab_error() -> (elab::ExprKind, std::rc::Rc<MonoType>) {
    (elab::ExprKind::Error, Type::new(MonoType::Error))
}

fn unit() -> std::rc::Rc<MonoType> {
    Type::new(MonoType::Var("Unit".to_string()))
}
