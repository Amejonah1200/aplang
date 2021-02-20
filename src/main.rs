use std::env;
use std::path::Path;

use crate::scanner::token::{Token, token_to_string};

mod scanner;

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() < 2 {
    println!("Ehm... pls provide a file to parse");
    return;
  }
  match scanner::scanner::scan(Path::new(args.get(1).unwrap())) {
    Ok(vec) => {
      for t in vec.iter() {
        match *t {
          Token::Unknown(_) => {}
          Token::NewLine => print!("\n"),
          _ => print!("[{}]", token_to_string(t))
        }
      }
    }
    Err(err) => println!("{}", err.to_string())
  }
}
