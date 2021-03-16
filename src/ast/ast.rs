use std::borrow::BorrowMut;
use std::collections::HashMap;

use crate::ast::expr::{Expression, Unary};
use crate::ast::expr::Expression::*;
use crate::ast::expr::Visibility;
use crate::scanner::scanner::Scanner;
use crate::scanner::token::{GriddedToken, Token};
use crate::scanner::token::Keyword;
use crate::scanner::token::Token::*;

pub fn ast(tokens: &Vec<GriddedToken>) -> Expression {
  let mut scanner = Scanner::new(tokens);
  let mut macro_annotations = Vec::new();
  let defs = Vec::new();
  let mut option;
  // macro annotations for file
  while {
    option = macro_annotation(scanner.borrow_mut());
    option.is_some()
  } {
    macro_annotations.push(MacroAnnotation(option.unwrap()));
  }
  // declarations

  Program(macro_annotations, defs)
}

macro_rules! token_match {
  ($scanner:expr, $token:path, $err:expr) => {
    {
      let temp = $scanner.peek_or_eof();
      if !matches!(temp.token, $token) {
        error(temp, Some($token), $err);
      }
    }
  };
}

macro_rules! token_match_get {
  ($scanner:expr, $token:path, $err:expr) => {
    {
      let temp = $scanner.peek_or_eof();
      if !matches!(temp.token, $token) {
        error(temp, Some($token), $err);
      }

      temp.token.clone()
    }
  };
}

macro_rules! tokens_match {
  ($scanner:expr, $err:expr, $( $pattern:pat )|+ $( if $guard: expr )?) => {
    {
      let temp = $scanner.peek_or_eof();
      if !matches!(temp.token, $( $pattern )|+ $( if $guard )?) {
        error_str(temp, "(っ °Д °;)っ", $err);
      }
      temp.token.clone()
    }
  };
}

macro_rules! identifier_match {
  ($scanner:expr, $err:expr) => {
    {
      let temp = $scanner.peek_or_eof();
      if !matches!(temp.token, Token::Identifier(_)) {
        error_str(temp, "Identifier", $err);
      }
      temp.token.clone()
    }
  };
}



fn path(scanner: &mut Scanner<GriddedToken>) -> Vec<Token> {
  let mut vec = vec![identifier_match!(scanner, "Path")];
  while matches!(scanner.peek_or_eof().token, Dot) {
    vec.push(identifier_match!(scanner, "Path"));
  }
  scanner.peek_rewind(1);
  vec
}

macro_rules! option_none_error {
  ($token:expr, $option:expr, $expected:literal, $loc:literal) => {
    if $option.is_none() {
      error_str($token, $expected, $loc);
    }
  };
}

macro_rules! value_set {
  ($var:ident, $value:expr) => {
    { $var = $value; $var }
  };
}

fn declaration(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn visibility(scanner: &mut Scanner<GriddedToken>) -> Option<Visibility> {
  let rep = match &scanner.peek_or_eof().token {
    Keyword(key) => {
      match key {
        Keyword::Pub => Some(Visibility::Public),
        Keyword::Pak => Some(Visibility::Package),
        Keyword::Prot => Some(Visibility::Protected),
        _ => None
      }
    }
    _ => None
  };
  if rep.is_none() {
    scanner.peek_rewind(1);
  }
  rep
}

fn class_decl(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn fun_decl(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn var_decl(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn statement(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn expr_stmt(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn for_stmt(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn if_stmt(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn return_stmt(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn while_stmt(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn do_while(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn block_no_return(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn expression(scanner: &mut Scanner<GriddedToken>) -> Expression {
  assignment(scanner)
}

fn assignment(scanner: &mut Scanner<GriddedToken>) -> Expression {
  let if_ns_expr = if_expr(scanner);
  if matches!(if_ns_expr, Call(_, _) | Primary(_)) {
    let mut token;
    match value_set!(token, scanner.peek_or_eof()).token {
      PlusEqual | MinusEqual | StarEqual | StarStarEqual | SlashEqual | AmpersandEqual
      | VerticalBarEqual | LessLessEqual | GreaterGreaterEqual | Equal
      | GreaterGreaterGreaterEqual | CircumflexEqual | QuestionMarkEqual => Assignment(Box::new(if_ns_expr), token.token.clone(), {
        Box::new(assignment(scanner))
      }, ),
      _ => {
        scanner.peek_rewind(1);
        if_ns_expr
      }
    }
  } else {
    if_ns_expr
  }
}

fn if_expr(scanner: &mut Scanner<GriddedToken>) -> Expression {
  let logic_or_expr = logic_or(scanner);
  let token = scanner.peek_or_eof();
  match token.token {
    QuestionMarkColon =>
      IfNsExpr(Box::new(logic_or_expr), {
        Box::new(expression(scanner))
      }),
    QuestionMark => {
      IfExpr(Box::new(logic_or_expr), Box::new(expression(scanner)), Box::new(expression(scanner)))
    }
    _ => {
      scanner.peek_rewind(1);
      logic_or_expr
    }
  }
}

fn logic_or(scanner: &mut Scanner<GriddedToken>) -> Expression {
  let logic_and_expr = logic_and(scanner);
  let mut chain: Vec<Expression> = vec![];
  while matches!(scanner.peek_or_eof().token, DoubleVerticalBar) {
    chain.push(logic_and(scanner));
  }
  if chain.len() > 0 {
    LogicOr(Box::from(logic_and_expr), chain)
  } else {
    logic_and_expr
  }
}

fn logic_and(scanner: &mut Scanner<GriddedToken>) -> Expression {
  let equality_expr = logic_and(scanner);
  let mut chain: Vec<Expression> = vec![];
  while matches!(scanner.peek_or_eof().token, AmpersandAmpersand) {
    chain.push(logic_and(scanner));
  }
  if chain.len() > 0 {
    LogicAnd(Box::from(equality_expr), chain)
  } else {
    equality_expr
  }
}

macro_rules! binary_method {
  ($name:ident, $next_name:ident, $expr:ident, $( $pattern:pat )|+ $( if $guard: expr )?) => {
    fn $name(scanner: &mut Scanner<GriddedToken>) -> Expression {
      let next_expr = $next_name(scanner);
      let mut chain: Vec<Unary> = vec![];
      let mut token: &GriddedToken;
      while matches!(value_set!(token, scanner.peek_or_eof()).token,  $( $pattern )|+ $( if $guard )?) {
        chain.push(Unary { op: token.token.clone(), expr: Box::new($next_name(scanner)) });
      }
      if chain.len() > 0 {
        $expr(Box::from(next_expr), chain)
      } else {
        next_expr
      }
    }
  };
}

binary_method!(equality, comparison, Equality, EqualEqual | BangEqual);
binary_method!(comparison, term, Comparison, Greater | GreaterEqual | Less | LessEqual);
binary_method!(term, factor, Term, Minus | Plus);
binary_method!(factor, bit_op, Factor, Slash | Star | StarStar);
binary_method!(bit_op, unary_left, BitOp, VerticalBar | Circumflex | Ampersand);

fn unary_left(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn unary_left_op(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn unary_right(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn call(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}


fn primary(scanner: &mut Scanner<GriddedToken>) -> Expression {
  let token = scanner.peek_or_eof();
  match token.token {
    Number(_) | Identifier(_) | String(_) => Primary(crate::ast::expr::Primary::Literal(token.token.clone())),
    LeftParen => Error,
    LeftBrace => Error,
    _ => {
      error_str(token, "'(', '{', Number, Identifier or String", "primary");
      Error
    }
  }
}

fn block_return(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn function(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn parameters(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn arguments(scanner: &mut Scanner<GriddedToken>) -> Expression {
  todo!()
}

fn macro_annotation(scanner: &mut Scanner<GriddedToken>) -> Option<crate::ast::expr::MacroAnnotation> {
  if scanner.peek_search_predicate(|gt| matches!(gt.token, At | HashTag)) {
    let is_annotation: bool = {
      scanner.peek_rewind(1);
      matches!(scanner.peek().unwrap().token, At)
    };
    token_match!(scanner, LeftBracket, "macro_annotation");
    Some(crate::ast::expr::MacroAnnotation {
      is_annotation,
      path: {
        path(scanner)
      },
      mapping: {
        token_match!(scanner, LeftParen, "macro_annotation");
        let mut map = HashMap::new();
        if is_annotation {
          let mut temp;
          while matches!({temp = scanner.peek_or_eof().token.clone(); temp.clone()}, Identifier(_)| Comma) {
            if matches!(temp, Comma) { continue; }
            token_match!(scanner, Equal, "macro_annotation : Mapping");
            map.insert(temp, tokens_match!(scanner, "macro_annotation : Mapping", Keyword(_) | Number(_) | String(_)));
          }
          scanner.peek_rewind(1);
        }
        map
      },
      ids: {
        let mut ids = vec![];
        if !is_annotation {
          let mut temp;
          while matches!({temp = scanner.peek_or_eof().token.clone(); temp.clone()}, Identifier(_)| Comma) {
            if matches!(temp, Comma) { continue; }
            ids.push(temp);
          }
          scanner.peek_rewind(1);
        }
        token_match!(scanner, RightParen, "macro_annotation : Close");
        token_match!(scanner, RightBracket, "macro_annotation : Close");
        scanner.pos_to_peek();
        ids
      },
    })
  } else {
    scanner.peek_reset();
    None
  }
}

pub fn error(token: &GriddedToken, expected: Option<Token>, loc: &'static str) {
  if expected.is_some() {
    eprintln!("Unexpected {} at x: {} y: {}. Expected: {}", token.token, token.pos_x, token.pos_y, expected.unwrap());
  } else {
    eprintln!("Unexpected {} at x: {} y: {}.", token.token, token.pos_x, token.pos_y);
  }
  panic!(loc);
}

pub fn error_str(token: &GriddedToken, expected: &str, loc: &'static str) {
  eprintln!("Unexpected {} at x: {} y: {}. Expected: {}", token.token, token.pos_x, token.pos_y, expected);
  panic!(loc);
}
