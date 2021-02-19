use std::env;
use std::path::Path;

use crate::scanner::token::{Token, token_to_string};

mod scanner;

fn main() {
  let args: Vec<String> = env::args().collect();
  // ik, hardcode, i will see how to manage args later...
  match scanner::scanner::scan(Path::new("Z:/aplang.txt")) {
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
