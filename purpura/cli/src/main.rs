use desugar::{Ctx, Desugar};
use parser::Parser;

use checker::infer::top_level::{Declare, Define};

fn main() {
    println!("Hello, world!");
    aksdhfk();
}

fn aksdhfk() {
    let s = r#"
        data Foo { Foo() }
        sig foo(a) -> a
        fn foo(x) = x
    "#;
    let mut p = Parser::new(s);

    let prog = p.parse().unwrap();

    let (reporter, receiver) = report::Reporter::new();
    let mut ctx = Ctx::new(reporter);

    let program = prog.desugar(&mut ctx);
    report::Reporter::to_stdout(receiver);

    let mut checker_ctx = checker::env::Env::default();

    for decl in program.decls.iter().cloned() {
        if let desugar::expr::TopLevelKind::Data(data) = decl {
            data.value.declare(&mut checker_ctx);
        }
    }

    for decl in program.decls.iter().cloned() {
        if let desugar::expr::TopLevelKind::Data(data) = decl {
            data.value.define(&mut checker_ctx);
        }
    }

    for decl in program.decls.iter().cloned() {
        if let desugar::expr::TopLevelKind::FnDecl(fn_decl) = decl {
            fn_decl.value.sig.declare(&mut checker_ctx);
        }
    }

    for decl in program.decls {
        if let desugar::expr::TopLevelKind::FnDecl(fn_decl) = decl {
            fn_decl.value.define(&mut checker_ctx);
        }
    }
}
