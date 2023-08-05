use location::Spanned;
use desugar::expr::{ExprKind, Statement, Arm};

trait Resolve {
    fn resolve(&mut self, ctx: &mut ResolveCtx);
}

pub struct ResolveCtx {

}

impl<T : Resolve> Resolve for Spanned<T> {
    fn resolve(&mut self, ctx: &mut ResolveCtx) {
        self.value.resolve(ctx)
    }
}

impl<T : Resolve> Resolve for Vec<T> {
    fn resolve(&mut self, ctx: &mut ResolveCtx) {
        for x in self {
            x.resolve(ctx)
        }
    }
}

impl Resolve for ExprKind {
    fn resolve(&mut self, ctx: &mut ResolveCtx) {
        use ExprKind::*;
        
        match self {
            Number(_) | String(_) | Identifier(_) => (),
            Lambda(_, _) => todo!(),
            Application(e1, e2) => {
                e1.resolve(ctx);
                e2.resolve(ctx);
            },
            Match(bind, arms) => {
                bind.resolve(ctx);
                arms.resolve(ctx);
            },
            Block(stmts) => {
                stmts.resolve(ctx);
            },
        }
    }
}

impl Resolve for Statement {
    fn resolve(&mut self, ctx: &mut ResolveCtx) {
        todo!()
    }
}

impl Resolve for Arm {
    fn resolve(&mut self, ctx: &mut ResolveCtx) {
        todo!()
    }
}
