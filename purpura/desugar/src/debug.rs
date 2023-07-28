use crate::Ctx;

impl core::fmt::Debug for Ctx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ctx")
            .field("fn_clauses", &self.fn_clauses)
            .field("signatures", &self.signatures)
            .field("data_types", &self.data_types)
            .finish()
    }
}
