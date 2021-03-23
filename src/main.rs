use std::{env, fs};
use std::path::Path;

use crate::scanner::parser::clean_up;
use crate::scanner::token::{GriddedObject, Token, token_to_string};

mod scanner;
mod ast;

pub const VERBOSE_DEBUG: bool = false;

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() < 2 {
    println!("Ehm... pls provide a file to parse");
    return;
  }
  let path = Path::new(args.get(1).unwrap());
  let result = fs::read_to_string(path);
  match result {
    Ok(str) => {
      match scanner::parser::scan(&str) {
        Ok(mut vec) => {
          println!("Scanned:");
          print_vec(&vec);
          clean_up(vec.as_mut());
          // print_vec(&vec);
          println!("AST:");
          println!("{:#?}", ast::ast::ast_build(&vec, &str, path));
        }
        Err(err) => println!("{}", err.to_string())
      }
    }
    Err(_) => {}
  }

}

fn print_vec(vec: &Vec<GriddedObject<Token>>) {
  for t in vec.iter() {
    match (*t).obj {
      Token::Unknown(_) => {}
      Token::NewLine => print!("\n"),
      _ => print!("[{} ({} {}) -> ({} {})]", token_to_string(&t.obj), t.start_pos_x, t.start_pos_y, t.end_pos_x, t.end_pos_y)
    }
  }
  println!()
}
