use std::borrow::BorrowMut;
use std::collections::HashMap;

use crate::ast::expr::{Expression, Unary};
use crate::ast::expr::Expression::*;
use crate::ast::expr::Visibility;
use crate::scanner::scanner::Scanner;
use crate::scanner::token::{GriddedObject, Token};
use crate::scanner::token::Keyword;
use crate::scanner::token::Token::*;

struct Parser<'a> {
  scanner: Scanner<'a, GriddedObject<Token>>,
}


pub fn ast_build(tokens: &Vec<GriddedObject<Token>>) -> Expression {
  Parser { scanner: Scanner::new(tokens) }.program()
}

macro_rules! token_match {
  ($scanner:expr, $token:path, $err:expr) => {
    {
      let temp = $scanner.peek_or_eof();
      if !matches!(temp.obj, $token) {
        error(temp, Some($token), $err);
      }
    }
  };
}

macro_rules! token_match_get {
  ($scanner:expr, $token:path, $err:expr) => {
    {
      let temp = $scanner.peek_or_eof();
      if !matches!(temp.obj, $token) {
        error(temp, Some($token), $err);
      }

      temp.obj.clone()
    }
  };
}

macro_rules! tokens_match {
  ($scanner:expr, $err:expr, $( $pattern:pat )|+ $( if $guard: expr )?) => {
    {
      let temp = $scanner.peek_or_eof();
      if !matches!(temp.obj, $( $pattern )|+ $( if $guard )?) {
        error_str(temp, "(っ °Д °;)っ", $err);
      }
      temp.obj.clone()
    }
  };
}

macro_rules! identifier_match {
  ($scanner:expr, $err:expr) => {
    {
      let temp = $scanner.peek_or_eof();
      if !matches!(temp.obj, Token::Identifier(_)) {
        error_str(temp, "Identifier", $err);
      }
      temp.obj.clone()
    }
  };
}


macro_rules! option_none_error {
  ($token:expr, $option:expr, $expected:literal, $loc:literal) => {
    if $option.is_none() {
      error_str($token, $expected, $loc);
    }
  };
}

#[macro_export]
macro_rules! value_set {
  ($var:ident, $value:expr) => {
    { $var = $value; $var }
  };
}

macro_rules! binary_method {
  ($name:ident, $next_name:ident, $expr:ident, $( $pattern:pat )|+ $( if $guard: expr )?) => {
    fn $name(&mut self) -> GriddedObject<Expression> {
      let next_expr = self.$next_name();
      let mut chain: Vec<Unary> = vec![];
      let mut token: &GriddedObject<Token>;
      while matches!(value_set!(token, self.scanner.peek_or_eof()).obj,  $( $pattern )|+ $( if $guard )?) {
        chain.push(Unary { op: token.obj.clone(), expr: Box::new(self.$next_name()) });
      }
      if chain.len() > 0 {
        GriddedObject::new_rect_array(
          next_expr.start_pos(),
          $expr(Box::from(next_expr), chain),
          self.scanner.peek_coords()
        )
      } else {
        next_expr
      }
    }
  };
}

impl<'a> Parser<'a> {
  pub fn program(&mut self) -> Expression {
    let mut macro_annotations = Vec::new();
    let mut defs = Vec::new();
    let mut option;
    let mut last_y = 0;
    // macro annotations for file
    while {
      option = self.macro_annotation();
      option.is_some()
    } {
      self.scanner.pos_to_peek();
      macro_annotations.push(option.unwrap());
    }
    // declarations
    while !matches!(self.scanner.peek_or_eof().obj, EOF) {
      self.scanner.peek_reset();
      self.skip_empty_statements(true);
      if matches!(self.scanner.peek_or_eof().obj, EOF) { break; } else {
        self.scanner.peek_reset();
        defs.push(self.declaration())
      }
    }
    Program(macro_annotations, defs)
  }

  fn skip_empty_statements(&mut self, consume: bool) {
    while matches!(self.scanner.peek_or_eof().obj, Semicolon) {}
    self.scanner.peek_rewind(1);
    if consume {
      self.scanner.pos_to_peek();
    }
  }

  fn path(&mut self) -> Vec<Token> {
    let mut vec = vec![identifier_match!(self.scanner, "Path")];
    while matches!(self.scanner.peek_or_eof().obj, Dot) {
      vec.push(identifier_match!(self.scanner, "Path"));
    }
    self.scanner.peek_rewind(1);
    vec
  }


  fn declaration(&mut self) -> GriddedObject<Expression> {
    todo!("declaration")
  }

  fn visibility(&mut self) -> Option<Visibility> {
    let rep = match &self.scanner.peek_or_eof().obj {
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
      self.scanner.peek_rewind(1);
    }
    rep
  }

  fn class_decl(&mut self) -> GriddedObject<Expression> {
    todo!("class_decl")
  }

  fn fun_decl(&mut self) -> GriddedObject<Expression> {
    todo!("fun_decl")
  }

  fn var_decl(&mut self, check_annotation: bool) -> GriddedObject<Expression> {
    todo!("var_decl")
  }

  fn statement(&mut self) -> GriddedObject<Expression> {
    todo!("statement")
  }

  fn expr_stmt(&mut self) -> GriddedObject<Expression> {
    todo!("expr_stmt")
  }

  fn for_stmt(&mut self) -> GriddedObject<Expression> {
    todo!("for_stmt")
  }

  fn if_stmt(&mut self) -> GriddedObject<Expression> {
    todo!("if_stmt")
  }

  fn return_stmt(&mut self) -> GriddedObject<Expression> {
    todo!("return_stmt")
  }

  fn while_stmt(&mut self) -> GriddedObject<Expression> {
    todo!("while_stmt")
  }

  fn do_while(&mut self) -> GriddedObject<Expression> {
    todo!("do_while")
  }

  fn block(&mut self) -> GriddedObject<Expression> {
    todo!("block")
  }

  fn expression(&mut self) -> GriddedObject<Expression> {
    self.assignment()
  }

  fn assignment(&mut self) -> GriddedObject<Expression> {
    let if_ns_expr = self.if_expr();
    if matches!(if_ns_expr.obj, Call(_, _) | Primary(_)) {
      let mut token;
      match value_set!(token, self.scanner.peek_or_eof()).obj {
        PlusEqual | MinusEqual | StarEqual | StarStarEqual | SlashEqual | AmpersandEqual
        | VerticalBarEqual | LessLessEqual | GreaterGreaterEqual | Equal
        | GreaterGreaterGreaterEqual | CircumflexEqual | QuestionMarkEqual => GriddedObject::new_rect_array(
          if_ns_expr.start_pos(),
          Assignment(Box::new(if_ns_expr), token.obj.clone(), Box::new(self.assignment())),
          self.scanner.peek_coords(),
        ),
        _ => {
          self.scanner.peek_rewind(1);
          if_ns_expr
        }
      }
    } else {
      if_ns_expr
    }
  }

  fn if_expr(&mut self) -> GriddedObject<Expression> {
    let logic_or_expr = self.logic_or();
    let token = self.scanner.peek_or_eof();
    match token.obj {
      QuestionMarkColon =>
        GriddedObject::new_rect_array(
          logic_or_expr.start_pos(),
          IfNsExpr(Box::new(logic_or_expr), Box::new(self.expression())),
          self.scanner.peek_coords(),
        ),
      QuestionMark =>
        GriddedObject::new_rect_array(
          logic_or_expr.start_pos(),
          IfExpr(Box::new(logic_or_expr), Box::new(self.expression()), Box::new(self.expression())),
          self.scanner.peek_coords(),
        ),

      _ => {
        self.scanner.peek_rewind(1);
        logic_or_expr
      }
    }
  }

  fn logic_or(&mut self) -> GriddedObject<Expression> {
    let logic_and_expr = self.logic_and();
    let mut chain: Vec<GriddedObject<Expression>> = vec![];
    while matches!(self.scanner.peek_or_eof().obj, DoubleVerticalBar) {
      chain.push(self.logic_and());
    }
    if chain.len() > 0 {
      GriddedObject::new_rect_array(
        logic_and_expr.start_pos(),
        LogicOr(Box::from(logic_and_expr), chain),
        self.scanner.peek_coords(),
      )
    } else {
      logic_and_expr
    }
  }

  fn logic_and(&mut self) -> GriddedObject<Expression> {
    let equality_expr = self.equality();
    let mut chain: Vec<GriddedObject<Expression>> = vec![];
    while matches!(self.scanner.peek_or_eof().obj, AmpersandAmpersand) {
      chain.push(self.equality());
    }
    if chain.len() > 0 {
      GriddedObject::new_rect_array(
        equality_expr.start_pos(),
        LogicAnd(Box::from(equality_expr), chain),
        self.scanner.peek_coords(),
      )
    } else {
      equality_expr
    }
  }

  binary_method!(equality, comparison, Equality, EqualEqual | BangEqual);
  binary_method!(comparison, term, Comparison, Greater | GreaterEqual | Less | LessEqual);
  binary_method!(term, factor, Term, Minus | Plus);
  binary_method!(factor, bit_op, Factor, Slash | Star | StarStar);
  binary_method!(bit_op, unary_left, BitOp, VerticalBar | Circumflex | Ampersand);

  fn unary_left(&mut self) -> GriddedObject<Expression> {
    todo!("unary_left")
  }

  fn unary_left_op(&mut self) -> GriddedObject<Expression> {
    todo!("unary_left_op")
  }

  fn unary_right(&mut self) -> GriddedObject<Expression> {
    todo!("unary_right")
  }

  fn call(&mut self) -> GriddedObject<Expression> {
    todo!("call")
  }


  fn primary(&mut self) -> GriddedObject<Expression> {
    let token = self.scanner.peek_or_eof();
    match token.obj {
      Number(_) | Identifier(_) | String(_) | Char(_)=> GriddedObject::new_rect_array(
        token.start_pos(),
        Primary(crate::ast::expr::Primary::Literal(token.obj.clone())),
        self.scanner.peek_coords(),
      ),
      // LeftParen => Error,
      // LeftBrace => Error,
      _ => {
        error_str(token, "'(', '{', Number, Identifier, String or Char (depending on context!)", "primary");
        GriddedObject::new_point_array(Error, token.start_pos())
      }
    }
  }

  fn block_return(&mut self) -> GriddedObject<Expression> {
    todo!("block_return")
  }

  fn function(&mut self) -> GriddedObject<Expression> {
    todo!("function")
  }

  fn parameters(&mut self) -> GriddedObject<Expression> {
    todo!("parameters")
  }

  fn arguments(&mut self) -> GriddedObject<Expression> {
    todo!("arguments")
  }

  fn macro_annotation(&mut self) -> Option<GriddedObject<Expression>> {
    let start_peek = self.scanner.peek_get();
    let mut coords = self.scanner.peek_coords();
    if self.scanner.peek_search_predicate(|gt| matches!(gt.obj, At | HashTag)) {
      let is_annotation: bool = {
        self.scanner.peek_rewind(1);
        matches!(self.scanner.peek().unwrap().obj, At)
      };
      token_match!(self.scanner, LeftBracket, "macro_annotation");
      Some(GriddedObject::new_rect_array(coords, MacroAnnotation(crate::ast::expr::MacroAnnotation {
        is_annotation,
        path: {
          self.path()
        },
        mapping: {
          token_match!(self.scanner, LeftParen, "macro_annotation");
          let mut map = HashMap::new();
          if is_annotation {
            let mut temp;
            while matches!({temp = self.scanner.peek_or_eof().obj.clone(); temp.clone()}, Identifier(_)| Comma) {
              if matches!(temp, Comma) { continue; }
              token_match!(self.scanner, Equal, "macro_annotation : Mapping");
              map.insert(temp, tokens_match!(self.scanner, "macro_annotation : Mapping", Keyword(_) | Number(_) | String(_)));
            }
            self.scanner.peek_rewind(1);
          }
          map
        },
        ids: {
          let mut ids = vec![];
          if !is_annotation {
            let mut temp;
            while matches!({temp = self.scanner.peek_or_eof().obj.clone(); temp.clone()}, Identifier(_)| Comma) {
              if matches!(temp, Comma) { continue; }
              ids.push(temp);
            }
            self.scanner.peek_rewind(1);
          }
          token_match!(self.scanner, RightParen, "macro_annotation : Close");
          coords = self.scanner.peek_coords();
          token_match!(self.scanner, RightBracket, "macro_annotation : Close");
          ids
        },
      }), coords))
    } else {
      self.scanner.peek_set(start_peek);
      None
    }
  }
}

pub fn error(token: &GriddedObject<Token>, expected: Option<Token>, loc: &'static str) {
  if expected.is_some() {
    eprintln!("Unexpected {} at x: {} y: {}. Expected: {}", token.obj, token.start_pos_x, token.start_pos_y, expected.unwrap());
  } else {
    eprintln!("Unexpected {} at x: {} y: {}.", token.obj, token.start_pos_x, token.start_pos_y);
  }
  panic!(loc);
}

pub fn error_str(token: &GriddedObject<Token>, expected: &str, loc: &'static str) {
  eprintln!("Unexpected {} at x: {} y: {}. Expected: {}", token.obj, token.start_pos_x, token.start_pos_y, expected);
  panic!(loc);
}
