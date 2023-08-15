use desugar::expr::{Data, Fn, Signature};

use crate::{
    env::Env,
    infer::errors::DefineError,
    types::{MonoType, PolyType, Type},
    unify,
};

use super::Infer;

pub trait Declare {
    fn declare(self, env: &mut Env);
}

pub trait Define {
    fn define(self, env: &mut Env);
}

impl Declare for Data {
    fn declare(self, env: &mut Env) {
        let arity = self.params.len();
        let name = self.name;

        env.type_decls.insert(name, arity);
    }
}

impl Define for Data {
    fn define(self, env: &mut Env) {
        let name = self.name.clone();

        let scheme = self.params.clone();
        let vars = scheme
            .iter()
            .enumerate()
            .map(|(i, _name)| Type::new(MonoType::Generalized(i)))
            .collect::<Vec<_>>();

        scheme.iter().zip(vars.clone()).for_each(|(name, t)| {
            env.add_type_variable(name.clone(), t);
        });

        let ret_type = Type::new(MonoType::Ctor(name, vars));

        for ctor in self.ctors.into_iter() {
            let arity = ctor.types.len();
            let accumulated = ctor
                .types
                .into_iter()
                .map(|t| t.infer(env.clone()))
                .rfold(ret_type.clone(), |acc, (_, p)| {
                    Type::new(MonoType::Arrow(p, acc))
                });

            let polytype = PolyType::new(scheme.clone(), accumulated);
            env.ctor_decls.insert(ctor.name.clone(), (polytype, arity));
        }
    }
}

impl Declare for Signature {
    fn declare(self, env: &mut Env) {
        let name = self.name.clone();

        let mut free_variables = self.return_type.value.free_variables();

        for arg in &self.params {
            free_variables = free_variables.union(arg.value.free_variables());
        }

        for (i, fv) in free_variables.iter().enumerate() {
            env.add_type_variable(fv.clone(), Type::new(MonoType::Generalized(i)));
        }

        let (_, ret_type) = self.return_type.infer(env.clone());

        env.enter_level();

        let arrow = self
            .params
            .into_iter()
            .map(|t| t.infer(env.clone()))
            .rfold(ret_type, |acc, (_, n_t)| {
                Type::new(MonoType::Arrow(n_t, acc))
            });

        env.leave_level();

        let polytype = PolyType::new(free_variables.into_iter().collect(), arrow);

        env.let_decls.insert(name, polytype);
    }
}

impl Define for Fn {
    fn define(self, env: &mut Env) {
        let params_size = self
            .clauses
            .first()
            .map(|x| x.params.len())
            .unwrap_or_default();

        let sig_type = env.let_decls.get(&self.name).unwrap();
        let sig_type = sig_type.skolemize();

        for clause in self.clauses {
            if clause.params.len() != params_size {
                let location = clause.body.0.location;
                let err = DefineError(
                    "Params size does not match the clause size".into(),
                    location,
                );
                env.reporter.report(err);
                // panic!("Params size not matches the clause size");
            }

            let mut env = env.clone();
            let mut types = Vec::new();

            for pattern in clause.params {
                let (bindings, t) = pattern.infer(env.clone());
                types.push(t);

                for bind in bindings {
                    let (name, monotype) = bind;

                    let polytype = PolyType::new(vec![], monotype);
                    env.add_variable(name, polytype);
                }
            }

            let (_elab_arm, ret_type) = clause.body.0.infer(env.clone());

            let arrow = types.into_iter().rfold(ret_type, |acc, sei_la| {
                Type::new(MonoType::Arrow(sei_la, acc))
            });

            unify::unify(env.clone(), arrow.clone(), sig_type.clone());
        }
    }
}
