use desugar::{Ctx, Desugar};
use parser::Parser;

use checker::infer::top_level::{Declare, Define};

fn main() {
    run();
}

fn run() {
    let s = r#"
    data Bool {
      True(),
      False(),
    }

    sig neg(Bool) -> Bool
    fun neg(True())  = False()
    fun neg(False()) = True()

    data Option<a> {
      Some(a),
      None(),
    }

    sig unwrap(Option<Number>) -> Number
    fun unwrap(Some(val)) = val
    fun unwrap(None()) = 0
    "#;
    let mut p = Parser::new(s);

    let prog = p.parse().unwrap();

    let (reporter, receiver) = report::Reporter::new();
    let mut ctx = Ctx::new(reporter.clone());

    let program = prog.desugar(&mut ctx);

    println!("{program:?}");

    let mut checker_ctx = checker::env::Env::new(reporter);

    for decl in program.decls.iter().cloned() {
        if let desugar::expr::TopLevelKind::Data(data) = decl {
            data.value.declare(&mut checker_ctx);
        }
    }

    for decl in program.decls.iter().cloned() {
        if let desugar::expr::TopLevelKind::FnDecl(fn_decl) = decl {
            fn_decl.value.sig.declare(&mut checker_ctx);
        }
    }

    for decl in program.decls.iter().cloned() {
        if let desugar::expr::TopLevelKind::Data(data) = decl {
            data.value.define(&mut checker_ctx);
        }
    }

    for decl in program.decls {
        if let desugar::expr::TopLevelKind::FnDecl(fn_decl) = decl {
            fn_decl.value.define(&mut checker_ctx);
        }
    }

    println!("here {:?}", checker_ctx.type_variables);

    report::Reporter::to_stdout(receiver);
}
