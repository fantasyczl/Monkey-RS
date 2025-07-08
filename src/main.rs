pub mod token;
pub mod lexer;
pub mod repl;
mod ast;
mod parser;
mod object;
pub mod evaluator;

use crate::repl::start_repl;

fn main() {
    match get_current_user() {
        Some(user) => println!("Hello {}! This is the Monkey programming language!\n", user),
        None => println!("Welcome to the Monkey REPL!"),
    }

    println!("Feel free to type in commands\n");
    start_repl(&mut std::io::stdin(), &mut std::io::stdout());
}

fn get_current_user() -> Option<String> {
    if let Ok(user) = std::env::var("USER") {
        Some(user)
    } else if let Ok(user) = std::env::var("USERNAME") {
        Some(user)
    } else {
        None
    }
}