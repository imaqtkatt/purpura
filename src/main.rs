mod lexer;
mod token;

fn main() {
    println!("Hello, world!");

    let source: String = "let name = \"foo\"".into();

    let mut lexer = lexer::Lexer::new(&source);

    let tokens = lexer.lex();
    println!("{:?}", tokens);
}
