use std::collections::HashMap;

mod debug;
pub mod expr;
pub mod from;

use expr as desugar;
use location::{Location, Spanned};
use report::ToDiagnostic;

type FnClauses = Vec<desugar::FnClause>;

pub struct Ctx {
    fn_clauses: HashMap<String, FnClauses>,
    signatures: HashMap<String, Spanned<desugar::Signature>>,
    data_types: HashMap<String, Spanned<desugar::Data>>,
    reporter: report::Reporter,
}

impl Ctx {
    pub fn new(reporter: report::Reporter) -> Self {
        Self {
            fn_clauses: Default::default(),
            signatures: Default::default(),
            data_types: Default::default(),
            reporter,
        }
    }
}

pub trait Desugar {
    type Out;

    fn desugar(self, ctx: &mut Ctx) -> Self::Out;
}

struct DesugarErr(String, location::Location);

impl ToDiagnostic for DesugarErr {
    fn message(&self) -> String {
        self.0.clone()
    }

    fn markers(&self) -> Vec<report::Marker> {
        vec![]
    }

    fn severity(&self) -> report::Severity {
        report::Severity::Warning
    }

    fn location(&self) -> Location {
        self.1
    }
}

impl Desugar for parser::program::Program {
    type Out = desugar::Program;

    fn desugar(self, ctx: &mut Ctx) -> Self::Out {
        use parser::expr::TopLevelKind;

        for tl_kind in self.decls.into_iter() {
            match tl_kind {
                TopLevelKind::Data(data) => {
                    let name = data.value.name.clone();

                    let spanned = Spanned {
                        value: data.value.into(),
                        location: data.location,
                    };
                    ctx.data_types.insert(name, spanned);
                }
                TopLevelKind::Sig(sig_decl) => {
                    let name = sig_decl.value.name.clone();

                    if ctx.signatures.get_mut(&name).is_some() {
                        let err = DesugarErr(
                            format!("The signature '{}' was already defined.", name),
                            sig_decl.location,
                        );
                        ctx.reporter.report(err);
                    } else {
                        let spanned = Spanned {
                            value: sig_decl.value.into(),
                            location: sig_decl.location,
                        };
                        ctx.signatures.insert(name, spanned);
                    }
                }
                TopLevelKind::FnDecl(fn_decl) => {
                    let fn_name = fn_decl.value.name.clone();

                    if let Some(clauses) = ctx.fn_clauses.get_mut(&fn_name) {
                        clauses.push(fn_decl.value.into());
                    } else {
                        ctx.fn_clauses.insert(fn_name, vec![fn_decl.value.into()]);
                    }
                }
            }
        }

        build_program(ctx)
    }
}

fn build_program(ctx: &mut Ctx) -> expr::Program {
    let mut program = desugar::Program {
        decls: Default::default(),
    };

    for (_name, data) in std::mem::take(&mut ctx.data_types) {
        let tlk = desugar::TopLevelKind::Data(data);
        program.decls.push(tlk);
    }

    for (name, signature) in std::mem::take(&mut ctx.signatures) {
        let sig = signature.value;
        if let Some(clauses) = ctx.fn_clauses.get_mut(&name) {
            let desugar_fn = desugar::Fn {
                name,
                sig,
                clauses: std::mem::take(clauses),
            };
            let spanned = Spanned {
                value: desugar_fn,
                location: Location::ghost(),
            };
            let tlk = desugar::TopLevelKind::FnDecl(spanned);
            program.decls.push(tlk);
        } else {
            let value = desugar::Fn {
                name,
                sig,
                clauses: vec![],
            };
            let spanned = Spanned {
                value,
                location: Location::ghost(),
            };
            let tlk = desugar::TopLevelKind::FnDecl(spanned);
            program.decls.push(tlk);
        };
    }

    program
}

#[cfg(test)]
mod tests {
    use parser::Parser;

    use crate::{Ctx, Desugar};

    #[test]
    fn aksdhfk() {
        let s = r#"
data Foo { Foo() }
sig foo(a) -> a
sig foo(b) -> b
fn foo(x) = x
"#;
        let mut p = Parser::new(s);

        let prog = p.parse().unwrap();

        let (reporter, receiver) = report::Reporter::new();
        let mut ctx = Ctx::new(reporter);

        let program = prog.desugar(&mut ctx);
        report::Reporter::to_stdout(receiver);

        println!("program {:#?}", &program);
    }

    #[test]
    fn akjdsfhkasjdfhsal() {
        let s = r#"
sig sla(Nat -> Nat) -> Nat
"#;
        let mut p = Parser::new(s);

        let prog = p.parse().unwrap();

        let (reporter, receiver) = report::Reporter::new();
        let mut ctx = Ctx {
            fn_clauses: Default::default(),
            signatures: Default::default(),
            data_types: Default::default(),
            reporter,
        };
        let program = prog.desugar(&mut ctx);

        report::Reporter::to_stdout(receiver);

        println!("fn_clauses {:#?}", &program);
    }
}
