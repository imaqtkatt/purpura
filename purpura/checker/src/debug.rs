use std::fmt::Debug;

use crate::env::Env;

impl Debug for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Env")
            .field("variables", &self.variables)
            .field("type_variables", &self.type_variables)
            .field("let_decls", &self.let_decls)
            .field("type_decls", &self.type_decls)
            .field("ctor_decls", &self.ctor_decls)
            .field("level", &self.level)
            .field("location", &self.location)
            .field("counter", &self.counter)
            .finish()
    }
}
