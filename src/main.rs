use std::env;
use std::path::Path;

use crate::scanner::token::{Token, token_to_string, GriddedToken};
use crate::scanner::parser::clean_up;

mod scanner;
mod ast;

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() < 2 {
    println!("Ehm... pls provide a file to parse");
    return;
  }
  match scanner::parser::scan(Path::new(args.get(1).unwrap())) {
    Ok(mut vec) => {
      println!("Scanned:");
      print_vec(&vec);
      clean_up(vec.as_mut());
      // print_vec(&vec);
      println!("AST:");
      println!("{:#?}", ast::ast::ast(&vec));
    }
    Err(err) => println!("{}", err.to_string())
  }
}

fn print_vec(vec: &Box<Vec<GriddedToken>>) {
  for t in vec.iter() {
    match (*t).token {
      Token::Unknown(_) => {}
      Token::NewLine => print!("\n"),
      _ => print!("[{} ({} {})]", token_to_string(&t.token), t.pos_x, t.pos_y)
    }
  }
  println!()
}
