use desugar::expr::Pattern;

use crate::{types::{Type, MonoType}, unify::unify, infer::typ};

use super::Infer;

impl Infer for Pattern {
    type Out = im_rc::HashMap<String, Type>;

    fn infer(self, env: crate::env::Env) -> (Self::Out, crate::types::Type) {
        use desugar::expr::PatternKind::*;

        let mut map = im_rc::HashMap::new();

        match self.value {
            Number(_) => (map, typ::type_number()),
            String(_) => (map, typ::type_string()),
            Identifier(id) => {
                let hole = env.new_hole();

                map.insert(id, hole.clone());

                (map, hole)
            },
            Application(ctor_name, args) => {
                let Some((ctor, ctor_arity)) = env.ctor_decls.get(&ctor_name) else {
                    panic!("Constructor '{}' not found in context", ctor_name);
                };
                
                let arity = args.len();

                if *ctor_arity != arity {
                    panic!("Arity error {} != {}", *ctor_arity, arity);
                }

                let mut instantiated = env.instantiate(ctor.clone());

                for pattern in args.into_iter() {
                    let (m, t) = pattern.infer(env.clone());

                    match &*instantiated.clone() {
                        MonoType::Arrow(x, y) => {
                            instantiated = y.clone();

                            unify(env.clone(), x.clone(), t);

                            for (name, typ) in m {
                                if map.contains_key(&name) {
                                    panic!("Duplicated variable '{name}'");
                                } else {
                                    map.insert(name, typ);
                                }
                            }
                        },
                        _ => unreachable!(),
                    }
                }
                (map, instantiated)
            },
            Wildcard => (map, env.new_hole()),
        }
    }
}
