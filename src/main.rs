use clap::Parser as ClapParser;

use crate::{
    checker::{Declare, Define, TypeKind},
    files::Files,
    tree::desugared,
};

mod checker;
mod desugar;
mod files;
mod lexer;
mod location;
mod parser;
mod report;
mod tree;

#[derive(clap::Parser)]
struct Cli {
    #[arg(short = 'f')]
    file: std::path::PathBuf,
}

fn main() {
    run();
}

fn run() {
    let cli = Cli::parse();

    let mut files = Files::new();
    let file_id = files.add_file(&cli.file);
    let program = parser::Parser::parse_file(file_id, &files);
    let (reporter, receiver) = report::Reporter::new();

    match program {
        Ok(tree) => {
            let desugared = desugar::Context::new(reporter.clone()).desugar_program(tree);
            let mut env = checker::TypeEnv::new(
                TypeKind::unit(),
                TypeKind::number(),
                TypeKind::string(),
                reporter.clone(),
            );
            for def in desugared.definitions.iter() {
                env = match def {
                    desugared::TopLevel::Data(data) => data.declare(env),
                    desugared::TopLevel::Def(def) => def.signature.declare(env),
                };
            }
            for def in desugared.definitions.into_iter() {
                env = match def {
                    desugared::TopLevel::Data(data) => data.define(env),
                    desugared::TopLevel::Def(def) => def.define(env),
                };
            }
        }
        Err(e) => {
            reporter.report(e);
            report::Reporter::to_stdout(receiver, &files);
            return;
        }
    }

    report::Reporter::to_stdout(receiver, &files)
}
