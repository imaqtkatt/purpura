use desugar::expr::TypeKind;
use location::Spanned;

use crate::{
    infer::errors::InferError,
    types::{MonoType, Type},
};

use super::Infer;

impl Infer for Spanned<TypeKind> {
    type Out = ();

    fn infer(self, env: crate::env::Env) -> (Self::Out, crate::types::Type) {
        let mut hash_map = Default::default();
        let ret = infer_type(self, &mut hash_map, env);
        ((), ret)
    }
}

fn infer_type(
    type_kind: Spanned<TypeKind>,
    map: &mut im_rc::HashMap<String, Type>,
    env: crate::env::Env,
) -> std::rc::Rc<MonoType> {
    use TypeKind::*;

    match type_kind.value {
        Generic(name, args) => match env.type_decls.get(&name) {
            Some(val) if *val != args.len() => {
                let infer_error = InferError(format!(
                    "Arity error, {}/{} != {}/{}",
                    &name,
                    &val,
                    &name,
                    args.len()
                ));
                env.reporter.report(infer_error);
                Type::new(MonoType::Error)
            }
            Some(_) => {
                let args = args
                    .into_iter()
                    .map(|arg| infer_type(arg, map, env.clone()))
                    .collect::<Vec<_>>();

                Type::new(MonoType::Ctor(name, args))
            }
            None => Type::new(MonoType::Error),
        },
        TypeVariable(s) => {
            if let Some(typ) = env.get_type_variable(&s) {
                typ
            } else {
                let err = InferError(format!("Could not find type variable '{:?}'", s));
                env.reporter.report(err);
                Type::new(MonoType::Error)
            }
        }
        Arrow(left, right) => {
            let left = infer_type(*left, map, env.clone());
            let right = infer_type(*right, map, env);

            Type::new(MonoType::Arrow(left, right))
        }
    }
}

pub fn type_string() -> std::rc::Rc<MonoType> {
    Type::new(MonoType::Ctor("String".into(), vec![]))
}

pub fn type_number() -> std::rc::Rc<MonoType> {
    Type::new(MonoType::Ctor("Number".into(), vec![]))
}
