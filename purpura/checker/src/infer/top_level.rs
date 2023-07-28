use desugar::expr::{Data, Fn, Signature};

use crate::{env::Env, types::{Type, MonoType, PolyType}};

use super::Infer;

trait Declare {
    fn declare(self, env: &mut Env);
}

trait Define {
    fn define(self, env: &mut Env);
}

impl Declare for Data {
    fn declare(self, env: &mut Env) {
        let arity = self.params.len();
        let name = self.name.clone();

        env.type_decls.insert(name, arity);
    }
}

impl Define for Data {
    fn define(self, env: &mut Env) {
        let name = self.name.clone();

        let scheme = self.params.clone();
        let vars = scheme.iter()
            .enumerate()
            .map(|(i, _name)| Type::new(MonoType::Generalized(i)))
            .collect::<Vec<_>>();

        scheme.iter()
            .zip(vars.clone())
            .for_each(|(name, t)| {
                env.add_type_variable(name.clone(), t);
            });

        let ret_type = Type::new(MonoType::Ctor(name.clone(), vars));

        for ctor in self.ctors.into_iter() {
            let arity = ctor.types.len();
            let accumulated = ctor.types
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

        let (_, ret_type) = self.return_type.infer(env.clone());
        
        env.enter_level();
        let arrow = self.params.into_iter()
            .map(|t| t.infer(env.clone()))
            .rfold(ret_type, |acc, (_, n_t)| {
                Type::new(MonoType::Arrow(n_t, acc))
            });
        env.leave_level();

        let polytype = env.generalize(arrow);
        
        env.let_decls.insert(name.clone(), polytype);
    }
}

impl Define for Fn {
    fn define(self, env: &mut Env) {
        todo!()
    }
}

/*
#[cfg(test)]
mod test {
    use parser::Parser;

    use crate::env::Env;

    use super::{Define, Declare};

    #[test]
    fn testetetete() {
        let s = "data Result<a, b> { Ok(a, b), Err(b) }";

        let mut parser = Parser::new(s);

        let data = parser.parse_data().unwrap();

        let mut env = Env::new();

        data.value.define(&mut env);

        for variant in env.let_decls {
            println!("name: {} : {}", variant.0, variant.1)
        }
    }

    #[test]
    fn testessadf() {
        let s = "sig id(a, b) -> a";

        let mut parser = Parser::new(s);

        let sig = parser.parse_sig().unwrap();

        let mut env = Env::new();

        sig.value.declare(&mut env);

        println!("{:#?}", env);
    }
}
*/
