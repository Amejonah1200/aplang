use crate::scanner::token::{GriddedObject, Token};
use crate::ast::expr::Expression;
use regex::Regex;

pub struct VisualisationPrinter<'a> {
  pub(crate) lines: Vec<&'a str>,
}

impl<'a> VisualisationPrinter<'a> {
  pub fn print_visualisation_expr(&self, expr: &GriddedObject<Expression>) {
    self.print_visualisation_arr(expr.start_pos(), expr.end_pos())
  }

  pub fn print_visualisation_tok(&self, token: &GriddedObject<Token>) {
    self.print_visualisation_arr(token.start_pos(), token.end_pos())
  }

  pub fn print_visualisation_arr(&self, start: [usize; 2], end: [usize; 2]) {
    for y in start[1]..=end[1] {
      let line_opt = self.lines.get(y);
      if line_opt.is_none() { break; }
      let line = line_opt.unwrap();
      if line.len() == 0 { continue; }
      eprintln!("{}", line);
      if y == start[1] {
        for _ in 0..start[0] { eprint!(" "); }
        eprint!("↑");
        if end[1] == start[1] {
          match end[0] - start[0] {
            0 => {}
            1 => eprint!("↑"),
            _ => {
              for _ in (start[0] + 1)..end[0] { eprint!("_") }
              eprint!("↑");
            }
          }
        } else {
          for _ in (start[0] + 1)..line.len() { eprint!("_") }
        }
        eprintln!();
      } else if y == end[1] {
        let range = Regex::new("^( *)").unwrap().captures(line).unwrap().get(1).unwrap().range();
        for _ in range.clone() { eprint!(" "); }
        for _ in range.end..end[0] { eprint!("_") }
        eprintln!("↑");
      } else {
        if Regex::new("[^\\s]").unwrap().find(line).is_none() { continue; }
        let range = Regex::new("^( *)").unwrap().captures(line).unwrap().get(1).unwrap().range();
        for _ in range.clone() { eprint!(" "); }
        for _ in range.end..line.len() { eprint!("_") }
        eprintln!();
      }
    }
  }
}
