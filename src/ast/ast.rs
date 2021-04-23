extern crate alloc;

use std::borrow::Borrow;
use std::collections::HashMap;
use std::path::Path;

use crate::ast::expr::{ArrayIndex, Expression, For, Generic, Type, Unary, Wildcard};
use crate::ast::expr::Expression::*;
use crate::ast::expr::Primary::{Id, Literal, SelfIvk, SuperIvk};
use crate::ast::expr::Type::Primitive;
use crate::ast::expr::Visibility;
use crate::helper::visualisation::VisualisationPrinter;
use crate::scanner::scanner::Scanner;
use crate::scanner::token::{GriddedObject, Token};
use crate::scanner::token::Keyword;
use crate::scanner::token::Keyword::PrimitiveType;
use crate::scanner::token::PrimitiveTypeKeyword::Int;
use crate::scanner::token::Token::*;

struct Parser<'a> {
  scanner: Scanner<'a, GriddedObject<Token>>,
  visualizer: &'a VisualisationPrinter<'a>,
  path: &'a Path,
}


pub fn ast_build(tokens: &Vec<GriddedObject<Token>>, visualizer: &VisualisationPrinter, path: &Path) -> Expression {
  Parser { scanner: Scanner::new(tokens), visualizer, path }.program()
}

macro_rules! token_match {
  ($self_parser:expr, $token:path, $err:expr) => {
    {
      let temp = $self_parser.scanner.peek_or_eof().clone();
      if !matches!(temp.obj, $token) {
        $self_parser.error(&temp, Some($token), $err);
      }
    }
  };
}

macro_rules! keyword_match {
  ($self_parser:expr, $keyword:path, $err:expr) => {
    {
      let temp = $self_parser.scanner.peek_or_eof().clone();
      if !match &temp.obj {
        Keyword(kw) => matches!(kw, $keyword),
        _ => false
      } {
        $self_parser.error(&temp, Some(Keyword($keyword)), $err);
      }
    }
  };
}

macro_rules! tokens_match {
  ($self_parser:expr, $err:expr, $( $pattern:pat )|+ $( if $guard: expr )?) => {
    {
      let temp = $self_parser.scanner.peek_or_eof().clone();
      if !matches!(temp.obj, $( $pattern )|+ $( if $guard )?) {
        $self_parser.error_str(&temp, "(っ °Д °;)っ", $err);
      }
      temp
    }
  };
}

macro_rules! identifier_match {
  ($self_parser:expr, $err:expr) => {
    {
      let temp = $self_parser.scanner.peek_or_eof().clone();
      if !matches!(temp.obj, Token::Identifier(_)) {
        $self_parser.error_str(&    temp, "Identifier", $err);
      }
      temp
    }
  };
}

#[macro_export]
macro_rules! value_set {
  ($var:ident, $value:expr) => {
    { $var = $value; &$var }
  };
}

macro_rules! binary_method {
  ($name:ident, $next_name:ident, $expr:ident, $( $pattern:pat )|+ $( if $guard: expr )?) => {
    fn $name(&mut self) -> GriddedObject<Expression> {
      let next_expr = self.$next_name();
      let mut chain: Vec<Unary> = vec![];
      let mut token: &GriddedObject<Token>;
      let mut end_coords = next_expr.end_pos();
      while matches!(value_set!(token, self.scanner.peek_or_eof()).obj,  $( $pattern )|+ $( if $guard )?) {
        chain.push(Unary { op: token.clone(), expr: Box::new({
          let next = self.$next_name();
          end_coords = next.end_pos();
          next
        }) });
      }
      self.scanner.peek_rewind(1);
      if chain.len() > 0 {
        GriddedObject::new_rect_array(
          next_expr.start_pos(),
          $expr(Box::from(next_expr), chain),
          end_coords
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
      let temp = option.unwrap();
      if temp.start_pos_y - last_y > 1 {
        self.scanner.peek_reset();
        break;
      }
      last_y = temp.start_pos_y;
      self.scanner.pos_to_peek();
      macro_annotations.push(temp);
    }
    // declarations
    while !matches!(self.scanner.peek_or_eof().obj, EOF) {
      self.scanner.peek_reset();
      self.skip_empty_statements(true);
      let decisive_token = &self.scanner.peek_or_eof().obj.clone();
      if matches!(decisive_token, EOF) { break; } else {
        self.scanner.peek_rewind(1);
        match decisive_token {
          Keyword(kw) => match kw {
            Keyword::Use => defs.push(self.use_decl()),
            _ => defs.push(self.declaration())
          }
          _ => defs.push(self.declaration())
        }
        self.scanner.pos_to_peek();
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


  fn declaration(&mut self) -> GriddedObject<Expression> {
    let mut macro_annotations = Vec::new();
    let mut option;
    let start_coords = self.scanner.peek_coords()[0];
    // macro annotations for declaration
    while {
      option = self.macro_annotation();
      option.is_some()
    } {
      macro_annotations.push(option.unwrap());
    }
    let visibility = self.visibility();
    let mut decisive_token = self.scanner.peek_or_eof().clone();
    match &decisive_token.obj {
      Keyword(kw) => {
        match kw {
          Keyword::Class => {
            self.scanner.peek_rewind(1);
            self.class_decl(start_coords, macro_annotations, visibility)
          }
          Keyword::PrimitiveType(t) => {
            let id = identifier_match!(self, "declaration : Keyword/func_or_var/id");
            let decisive_token = self.scanner.peek_or_eof().clone();
            match decisive_token.obj {
              Semicolon =>
                GriddedObject::new_rect_array(
                  start_coords,
                  VarDecl(macro_annotations, visibility, GriddedObject::new_rect_array(
                    decisive_token.start_pos(),
                    Type::Primitive(t.clone()),
                    decisive_token.end_pos(),
                  ), id, None),
                  self.scanner.peek_previous_coords()[1],
                ),
              Equal =>
                GriddedObject::new_rect_array(
                  start_coords,
                  VarDecl(macro_annotations, visibility, GriddedObject::new_rect_array(
                    decisive_token.start_pos(),
                    Type::Primitive(t.clone()),
                    decisive_token.end_pos(),
                  ), id, Some(Box::new(self.expression()))),
                  {
                    token_match!(self, Semicolon, "declaration : Id/Equal/;");
                    self.scanner.peek_previous_coords()[1]
                  },
                ),
              LeftParen => {
                self.scanner.peek_rewind(2);
                self.function(
                  start_coords,
                  macro_annotations,
                  visibility,
                  None,
                  Some(GriddedObject::new_rect_array(
                    decisive_token.start_pos(),
                    Type::Primitive(t.clone()),
                    decisive_token.end_pos(),
                  )),
                )
              }
              _ => {
                self.error_str(decisive_token.borrow(), "';', '=', '('", "declaration : Keyword/func_or_var/decisive_token");
                GriddedObject::new_point(Error, 0, 0)
              }
            }
          }
          _ => {
            self.error_str(&decisive_token, "\"class\", primitive, Identifier or '<'", "declaration : Keyword/Not_allowed");
            GriddedObject::new_point(Error, 0, 0)
          }
        }
      }
      Less => {
        self.scanner.peek_rewind(1);
        let generics = self.generics();
        decisive_token = self.scanner.peek_or_eof().clone();
        self.scanner.peek_rewind(1);
        let type_expr = match decisive_token.obj {
          LeftParen => None,
          _ => Some(self.type_expr())
        };
        self.function(start_coords, macro_annotations, visibility, Some(generics), type_expr)
      }

      Identifier(_) => {
        match self.scanner.peek_or_eof().clone().obj {
          LeftParen => {
            self.scanner.peek_rewind(2);
            self.function(start_coords, macro_annotations, visibility, None, None)
          }
          Colon => {
            let type_expr = self.type_expr();
            let temp = self.scanner.peek_or_eof().clone();
            match temp.obj {
              Equal => GriddedObject::new_rect_array(
                start_coords,
                VarDecl(macro_annotations, visibility, type_expr, decisive_token, Some(Box::new(self.expression()))),
                {
                  token_match!(self, Semicolon, "declaration : Id/Colon/Equal/;");
                  self.scanner.peek_previous_coords()[1]
                },
              ),
              Semicolon => GriddedObject::new_rect_array(
                start_coords,
                VarDecl(macro_annotations, visibility, type_expr, decisive_token, None),
                self.scanner.peek_previous_coords()[1],
              ),
              _ => {
                self.error_str(&temp, "'=' or ';'", "declaration : Id/Colon/Other");
                GriddedObject::new_point(Error, 0, 0)
              }
            }
          }
          _ => {
            self.scanner.peek_rewind(2);
            let type_expr = self.type_expr();
            match self.scanner.peek_or_eof().clone().obj {
              LeftParen => {
                self.scanner.peek_rewind(1);
                self.function(start_coords, macro_annotations, visibility, None, Some(type_expr))
              }
              _ => {
                self.error(&decisive_token, Some(LeftParen), "declaration : Id/Other/Other");
                GriddedObject::new_point(Error, 0, 0)
              }
            }
          }
        }
      }
      _ => {
        self.error_str(&decisive_token, "\"class\", primitive, Identifier or '<'", "declaration : Not_allowed");
        GriddedObject::new_point(Error, 0, 0)
      }
    }
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

  fn class_decl(&mut self, start_coords: [usize; 2], annotations: Vec<GriddedObject<Expression>>, visibility: Option<Visibility>) -> GriddedObject<Expression> {
    keyword_match!(self, Keyword::Class, "class_decl : class");
    let id = identifier_match!(self, "class_decl : id");
    let mut generics = None;
    let mut supers = None;
    let mut decisive_token = self.scanner.peek_or_eof().clone();

    if matches!(decisive_token.obj, Less) {
      self.scanner.peek_rewind(1);
      generics = Some(self.generics());
      decisive_token = self.scanner.peek_or_eof().clone();
      self.scanner.peek_rewind(1);
    }

    if matches!(decisive_token.obj, Colon) {
      if generics.is_some() { self.scanner.peek_adv(1) }
      let mut vec = vec![self.type_expr()];
      while matches!(self.scanner.peek_or_eof().obj, Comma) {
        vec.push(self.type_expr());
      }
      supers = Some(vec);
    }
    self.scanner.peek_rewind(1);
    token_match!(self, LeftBrace, "class_decl : {");
    let mut defs = Vec::new();
    while !matches!(self.scanner.peek_or_eof().obj, RightBrace) {
      self.scanner.peek_rewind(1);
      self.skip_empty_statements(false);
      let decisive_token = &self.scanner.peek_or_eof().obj.clone();
      if matches!(decisive_token, RightBrace) { break; } else {
        self.scanner.peek_rewind(1);
        match decisive_token {
          Keyword(kw) => match kw {
            Keyword::Use => defs.push(self.use_decl()),
            _ => defs.push(self.declaration())
          }
          _ => defs.push(self.declaration())
        }
      }
    }
    GriddedObject::new_rect_array(start_coords, ClassDecl(annotations, visibility, id, generics, supers, defs), self.scanner.peek_previous_coords()[1])
  }

  fn use_decl(&mut self) -> GriddedObject<Expression> {
    let start_coords = self.scanner.peek_coords()[0];
    keyword_match!(self, Keyword::Use, "use_decl : use");
    let mut decisive_token = identifier_match!(self, "use_decl : first id");
    let mut path = vec![decisive_token];
    let mut is_all = false;
    while matches!({
      decisive_token = self.scanner.peek_or_eof().clone();
      &decisive_token.obj
    }, Dot) {
      match {
        decisive_token = self.scanner.peek_or_eof().clone();
        &decisive_token.obj
      } {
        Identifier(_) => path.push(decisive_token.clone()),
        Star => {
          is_all = true;
          self.scanner.peek_adv(1);
          break;
        }
        _ => self.error_str(&decisive_token, "Identifer or '*'", "use_decl : Path")
      }
    }
    self.scanner.peek_rewind(1);
    token_match!(self, Semicolon, "use_decl : ;");
    GriddedObject::new_rect_array(start_coords, UseDecl(path, is_all), self.scanner.peek_previous_coords()[1])
  }

  fn statement(&mut self) -> GriddedObject<Expression> {
    let start_coords = self.scanner.peek_coords()[0];
    match self.scanner.peek_or_eof().obj.clone() {
      Keyword(kw) => {
        self.scanner.peek_rewind(1);
        match kw {
          Keyword::If => self.if_stmt(),
          Keyword::While => self.while_stmt(),
          Keyword::Loop => self.loop_expr(),
          Keyword::Do => self.do_while_stmt(),
          Keyword::For => self.for_stmt(),
          Keyword::Return => self.return_stmt(),
          Keyword::Break => self.break_stmt(),
          _ => self.expr_stmt(),
        }
      }
      LeftBrace => {
        self.scanner.peek_rewind(1);
        self.block()
      }
      _ => GriddedObject::new_rect_array(start_coords, ExprStmt(Box::new({
        self.scanner.peek_rewind(1);
        self.expr_stmt()
      })), self.scanner.peek_previous_coords()[1])
    }
  }

  fn expr_stmt(&mut self) -> GriddedObject<Expression> {
    let expr = self.expression();
    token_match!(self, Semicolon, "expr_stmt : ;");
    GriddedObject::new_rect_array(expr.start_pos(), expr.obj, self.scanner.peek_next_coords()[1])
  }

  fn for_stmt(&mut self) -> GriddedObject<Expression> {
    let start_coords = self.scanner.peek_coords()[0];
    keyword_match!(self, Keyword::For, "for_stmt : for");
    token_match!(self, LeftParen, "for_stmt : (");
    let id = self.scanner.peek_or_eof().clone();
    token_match!(self, Colon, "for_stmt : (");
    let expr = self.expression();
    token_match!(self, RightParen, "for_stmt : )");
    GriddedObject::new_rect_array(start_coords, ForStmt(For::ForItrStmt(id, Box::new(expr)), Box::new(self.statement())), self.scanner.peek_previous_coords()[1])
  }

  fn if_stmt(&mut self) -> GriddedObject<Expression> {
    let start_coords = self.scanner.peek_coords()[0];
    keyword_match!(self, Keyword::If, "if_stmt : if");
    token_match!(self, LeftParen, "if_stmt : (");
    let expr = self.expression();
    token_match!(self, RightParen, "if_stmt : )");
    let stmt = self.statement();
    let mut else_stmt = None;
    if match self.scanner.peek_or_eof().obj.clone() {
      Keyword(kw) => matches!(kw, Keyword::Else),
      _ => false
    } {
      else_stmt = Some(Box::new(self.statement()));
    } else {
      self.scanner.peek_rewind(1);
    }
    GriddedObject::new_rect_array(start_coords, IfStmt(Box::new(expr), Box::new(stmt), else_stmt), self.scanner.peek_previous_coords()[1])
  }

  fn return_stmt(&mut self) -> GriddedObject<Expression> {
    let start_coords = self.scanner.peek_coords()[0];
    keyword_match!(self, Keyword::Return, "return_stmt : return");
    if matches!(self.scanner.peek_or_eof().obj, Semicolon) {
      GriddedObject::new_rect_array(start_coords, ReturnStmt(None), self.scanner.peek_previous_coords()[1])
    } else {
      self.scanner.peek_rewind(1);
      GriddedObject::new_rect_array(
        start_coords,
        ReturnStmt(Some(Box::new(self.expression()))),
        {
          token_match!(self, Semicolon, "return_stmt : Some/;");
          self.scanner.peek_previous_coords()[1]
        },
      )
    }
  }

  fn break_stmt(&mut self) -> GriddedObject<Expression> {
    let start_coords = self.scanner.peek_coords()[0];
    keyword_match!(self, Keyword::Break, "break_stmt : break");
    if matches!(self.scanner.peek_or_eof().obj, Semicolon) {
      GriddedObject::new_rect_array(start_coords, BreakStmt(None), self.scanner.peek_previous_coords()[1])
    } else {
      self.scanner.peek_rewind(1);
      let break_stmt = GriddedObject::new_rect_array(start_coords, BreakStmt(Some(Box::new(self.expression()))), self.scanner.peek_coords()[1]);
      token_match!(self, Semicolon, "break_stmt : ;");
      break_stmt
    }
  }

  fn while_stmt(&mut self) -> GriddedObject<Expression> {
    let start_coords = self.scanner.peek_coords()[0];
    keyword_match!(self, Keyword::While, "while_stmt : while");
    token_match!(self, LeftParen, "while_stmt : (");
    let expr = self.expression();
    token_match!(self, RightParen, "while_stmt : )");
    GriddedObject::new_rect_array(start_coords, WhileStmt(Box::new(expr), Box::new(self.statement())), self.scanner.peek_previous_coords()[1])
  }

  fn loop_expr(&mut self) -> GriddedObject<Expression> {
    let start_coords = self.scanner.peek_coords()[0];
    keyword_match!(self, Keyword::Loop, "loop : loop");
    GriddedObject::new_rect_array(start_coords, LoopExpr(Box::new(self.block())), self.scanner.peek_previous_coords()[1])
  }

  fn do_while_stmt(&mut self) -> GriddedObject<Expression> {
    let start_coords = self.scanner.peek_coords()[0];
    keyword_match!(self, Keyword::Do, "do_while_stmt : do");
    let block = self.block();
    keyword_match!(self, Keyword::While, "do_while_stmt : while");
    token_match!(self, LeftParen, "do_while_stmt : (");
    let expr = self.expression();
    token_match!(self, RightParen, "do_while_stmt : )");
    token_match!(self, Semicolon, "do_while_stmt : ;");
    GriddedObject::new_rect_array(start_coords, DoWhile(Box::new(block), Box::new(expr)), self.scanner.peek_previous_coords()[1])
  }

  fn expression(&mut self) -> GriddedObject<Expression> {
    self.assignment()
  }

  fn assignment(&mut self) -> GriddedObject<Expression> {
    let if_ns_expr = self.if_expr();
    if matches!(if_ns_expr.obj, Call(_, _, _) | Primary(_)) {
      let mut token;
      match value_set!(token, self.scanner.peek_or_eof()).obj {
        PlusEqual | MinusEqual | StarEqual | StarStarEqual | SlashEqual | AmpersandEqual
        | VerticalBarEqual | LessLessEqual | GreaterGreaterEqual | Equal
        | GreaterGreaterGreaterEqual | CircumflexEqual | QuestionMarkEqual => GriddedObject::new_rect_array(
          if_ns_expr.start_pos(),
          Assignment(Box::new(if_ns_expr), token.clone(), Box::new(self.assignment())),
          self.scanner.peek_previous_coords()[1],
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
          self.scanner.peek_previous_coords()[1],
        ),
      QuestionMark =>
        GriddedObject::new_rect_array(
          logic_or_expr.start_pos(),
          IfExpr(Box::new(logic_or_expr), Box::new(self.expression()), Box::new({
            token_match!(self, Colon, "if_expr : ?/:");
            self.expression()
          })),
          self.scanner.peek_previous_coords()[1],
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
    self.scanner.peek_rewind(1);
    if chain.len() > 0 {
      GriddedObject::new_rect_array(
        logic_and_expr.start_pos(),
        LogicOr(Box::from(logic_and_expr), chain),
        self.scanner.peek_previous_coords()[1],
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
    self.scanner.peek_rewind(1);
    if chain.len() > 0 {
      GriddedObject::new_rect_array(
        equality_expr.start_pos(),
        LogicAnd(Box::from(equality_expr), chain),
        self.scanner.peek_previous_coords()[1],
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
    let decisive_token = self.scanner.peek_or_eof();
    match decisive_token.obj {
      Bang | Tilde => GriddedObject::new_rect_array(
        decisive_token.start_pos(),
        UnaryLeft(Unary { op: decisive_token.clone(), expr: Box::new(self.unary_left()) }),
        self.scanner.peek_previous_coords()[1],
      ),
      _ => {
        self.scanner.peek_rewind(1);
        // self.unary_right()
        self.call()
      }
    }
  }

// fn unary_right(&mut self) -> GriddedObject<Expression> {
//   unimplemented!("unary_right")
// }

  fn call(&mut self) -> GriddedObject<Expression> {
    let primary = self.primary();
    let mut arguments = None;
    let mut calls = Vec::new();
    let mut decisive_token = self.scanner.peek_or_eof().obj.clone();
    self.scanner.peek_rewind(1);
    if matches!(decisive_token, LeftParen) {
      arguments = Some(self.arguments());
    }
    while {
      decisive_token = self.scanner.peek_or_eof().obj.clone();
      matches!(decisive_token, Dot | LeftBracket)
    } {
      calls.push(match decisive_token {
        Dot => {
          GriddedObject::new_rect_array(self.scanner.peek_previous_coords()[0], {
            let id = identifier_match!(self, "call : Dot/Id");
            decisive_token = self.scanner.peek_or_eof().obj.clone();
            self.scanner.peek_rewind(1);
            if matches!(decisive_token, LeftParen) {
              crate::ast::expr::Call::IdArguments(id, self.arguments())
            } else {
              crate::ast::expr::Call::Id(id)
            }
          }, self.scanner.peek_coords()[0])
        }
        LeftBracket => {
          GriddedObject::new_rect_array(self.scanner.peek_previous_coords()[0], crate::ast::expr::Call::ArrayClause({
            let index;
            if matches!(self.scanner.peek_or_eof().obj, Colon) {
              index = ArrayIndex::End(self.expression());
              token_match!(self, RightBracket, "call : :/]");
            } else {
              self.scanner.peek_rewind(1);
              let expr = self.expression();
              if matches!(self.scanner.peek_or_eof().obj, Colon) {
                if matches!(self.scanner.peek_or_eof().obj, RightBracket) {
                  index = ArrayIndex::Start(expr);
                } else {
                  self.scanner.peek_rewind(1);
                  index = ArrayIndex::StartEnd(expr, self.expression());
                  token_match!(self, RightBracket, "call : no-:/:/no-]/]");
                }
              } else {
                self.scanner.peek_rewind(1);
                token_match!(self, RightBracket, "call : no-:/no-:/]");
                index = ArrayIndex::Exact(expr)
              }
            }
            index
          }), self.scanner.peek_coords()[0])
        }
        _ => panic!("Should never happen.")
      });
    }
    self.scanner.peek_rewind(1);
    if calls.is_empty() {
      primary
    } else {
      GriddedObject::new_rect_array(primary.start_pos(), Call(Box::new(primary), arguments, calls), self.scanner.peek_previous_coords()[1])
    }
  }


  fn primary(&mut self) -> GriddedObject<Expression> {
    let token = self.scanner.peek_or_eof().clone();
    let prim = match &token.obj {
      Number(_) | String(_) | Char(_) => GriddedObject::new_rect_array(
        token.start_pos(),
        Primary(Literal(token.obj.clone())),
        token.end_pos(),
      ),
      LeftParen => {
        self.scanner.peek_rewind(1);
        let tuple = self.arguments();
        let params_opt = convert_vec_to_identifiers(&tuple);
        if tuple.len() == 0 {
          token_match!(self, ArrowSimpleRight, "primary/0/-> (for lambda)");
          GriddedObject::new_rect_array(token.start_pos(), Lambda(Vec::new(), Box::new(self.expression())), self.scanner.peek_coords()[0])
        } else {
          match params_opt {
            Some(params) => {
              GriddedObject::new_rect_array(token.start_pos(), Expression::Lambda(params, {
                token_match!(self, ArrowSimpleRight, "primary/_/-> (for lambda)");
                Box::new(self.expression())
              }), self.scanner.peek_previous_coords()[1])
            }
            None => if tuple.len() == 1 {
              GriddedObject::new_rect_array(token.start_pos(), tuple.into_iter().next().unwrap().obj, self.scanner.peek_previous_coords()[1])
            } else {
              GriddedObject::new_rect_array(token.start_pos(), Expression::Tuple(tuple), self.scanner.peek_previous_coords()[1])
            }
          }
        }
      }
      LeftBrace => {
        self.scanner.peek_rewind(1);
        self.block()
      }
      Keyword(kw) => {
        match kw {
          Keyword::Loop => {
            self.scanner.peek_rewind(1);
            self.loop_expr()
          }
          Keyword::True | Keyword::False => GriddedObject::new_rect_other(&token, Primary(Literal(token.obj.clone()))),
          Keyword::SelfKey => GriddedObject::new_rect_other(&token, Primary(SelfIvk)),
          Keyword::Super => GriddedObject::new_rect_other(&token, Primary(SuperIvk)),
          _ => {
            self.error_str(&token, "'(', '{', \"true\", \"false\", \"super\", \"self\", Number, Identifier, String or Char (depending on context!)", "primary/Keyword");
            GriddedObject::new_point_array(Error, token.start_pos())
          }
        }
      }
      Identifier(_) => GriddedObject::new_rect_other(&token, Primary(Id(token.obj.clone()))),
      _ => {
        self.error_str(&token, "'(', '{', \"true\", \"false\", Number, Identifier, String or Char (depending on context!)", "primary");
        GriddedObject::new_point_array(Error, token.start_pos())
      }
    };
    prim
  }


  fn function(&mut self, start_coords: [usize; 2], annotations: Vec<GriddedObject<Expression>>, visibility: Option<Visibility>, generics: Option<Vec<GriddedObject<Generic>>>, fun_type: Option<GriddedObject<Type>>) -> GriddedObject<Expression> {
    let id = identifier_match!(self, "function : id");
    token_match!(self, LeftParen, "function : (");
    let parameters = self.parameters();
    token_match!(self, RightParen, "function : )");
    let fun = GriddedObject::new_rect_array(start_coords, FunDecl(annotations, visibility, generics, fun_type, id, parameters, Box::new(self.block())), self.scanner.peek_previous_coords()[1]);
    fun
  }

  fn parameters(&mut self) -> Vec<(GriddedObject<Token>, GriddedObject<Type>)> {
    let mut parameters = Vec::new();
    let mut id;
    while {
      id = self.scanner.peek_or_eof().clone();
      matches!(id.obj, Identifier(_))
    } {
      token_match!(self, Colon, "parameters");
      parameters.push((id, self.type_expr()));
      if !{
        id = self.scanner.peek_or_eof().clone();
        matches!(id.obj, Comma)
      } { self.scanner.peek_rewind(1) }
    };
    self.scanner.peek_rewind(1);
    parameters
  }

  fn arguments(&mut self) -> Vec<GriddedObject<Expression>> {
    token_match!(self, LeftParen, "arguments : (");
    let mut vec = Vec::new();
    let mut decisive_token = self.scanner.peek_or_eof();
    if matches!(decisive_token.obj, RightParen) {
      return vec;
    } else {
      self.scanner.peek_rewind(1);
      vec.push(self.expression());
    }
    while !matches!({
    decisive_token  = self.scanner.peek_or_eof();
    decisive_token.obj.clone()
    }, RightParen) {
      self.scanner.peek_rewind(1);
      token_match!(self, Comma, "arguments : ,");
      vec.push(self.expression());
    }
    vec
  }

  fn block(&mut self) -> GriddedObject<Expression> {
    let start_coords = self.scanner.peek_coords()[0];
    token_match!(self, LeftBrace, "block : {");
    let mut stmts = Vec::new();
    let mut expr = None;
    let mut decisive_token;
    macro_rules! rewind_and_parse {
      ($value:expr) => {
        { self.scanner.peek_rewind(1); $value }
      };
    }
    while !matches!({
    decisive_token  = self.scanner.peek_or_eof().clone();
    decisive_token.obj.clone()
    }, RightBrace) {
      match &decisive_token.obj {
        Keyword(kw) => stmts.push(match kw {
          Keyword::For => rewind_and_parse!(self.for_stmt()),
          Keyword::Return => rewind_and_parse!(self.return_stmt()),
          Keyword::While => rewind_and_parse!(self.while_stmt()),
          Keyword::Do => rewind_and_parse!(self.do_while_stmt()),
          Keyword::Loop => rewind_and_parse!(self.loop_expr()),
          Keyword::If => {
            let if_stmt = rewind_and_parse!(self.if_stmt());
            match self.scanner.peek_or_eof().obj {
              RightBrace => {
                expr = Some(Box::new(if_stmt));
                break;
              }
              _ => self.scanner.peek_rewind(1)
            }
            if_stmt
          }
          Keyword::Let => {
            let start_coords_2 = decisive_token.start_pos();
            let id = identifier_match!(self,"block : Stms/Keyword/Let/Id");
            decisive_token = self.scanner.peek_or_eof().clone();
            let mut type_expr = None;
            if matches!(decisive_token.obj, Colon) {
              type_expr = Some(self.type_expr());
              decisive_token = self.scanner.peek_or_eof().clone();
            }
            if matches!(decisive_token.obj, Equal) {
              let expr_temp = VarStmt(id, type_expr, Some(Box::new(self.expression())));
              decisive_token = self.scanner.peek_or_eof().clone();
              match &decisive_token.obj {
                Semicolon => {
                  GriddedObject::new_rect_array(
                    start_coords_2,
                    expr_temp,
                    self.scanner.peek_previous_coords()[1],
                  )
                }
                RightBrace => {
                  expr = Some(Box::new(GriddedObject::new_rect_array(
                    start_coords_2,
                    expr_temp,
                    self.scanner.peek_previous_coords()[1],
                  )));
                  break;
                }
                _ => {
                  self.error_str(&decisive_token, "';' or '}'", "block : Stms/Keyword/Let/Equal/Expr_or_stmt");
                  GriddedObject::new_point_array(Error, decisive_token.start_pos())
                }
              }
            } else if matches!(decisive_token.obj, Semicolon) {
              GriddedObject::new_rect_array(
                start_coords_2,
                VarStmt(id, type_expr, None),
                decisive_token.end_pos(),
              )
            } else {
              self.error_str(&decisive_token, "'=' or ';'", "block : Stms/Keyword/Let/Eq-;");
              GriddedObject::new_point_array(Error, decisive_token.start_pos())
            }
          }
          _ => {
            self.error_str(&decisive_token, "\"if\", \"while\", \"do\", \"for\", \"return\", '}', var declaration or expression.", "block : Stms/Keyword");
            GriddedObject::new_point_array(Error, decisive_token.start_pos())
          }
        }),
        _ => {
          self.scanner.peek_rewind(1);
          let expr_temp = self.expression();
          decisive_token = self.scanner.peek_or_eof().clone();
          if matches!(decisive_token.obj, RightBrace) {
            expr = Some(Box::new(expr_temp));
            break;
          } else if matches!(decisive_token.obj, Semicolon) {
            stmts.push(GriddedObject::new_rect_array(
              expr_temp.start_pos(),
              ExprStmt(Box::new(expr_temp)),
              self.scanner.peek_previous_coords()[1],
            ));
          } else {
            self.error_str(&decisive_token, "'}' or ';'", "block : expr");
          }
        }
      }
    }
    GriddedObject::new_rect_array(start_coords, Block(stmts, expr), self.scanner.peek_previous_coords()[1])
  }

  fn type_expr(&mut self) -> GriddedObject<Type> {
    let start_coords = self.scanner.peek_coords()[0];
    let mut decisive_token = self.scanner.peek_or_eof().clone();
    let prim;
    match &decisive_token.obj {
      Identifier(_) => {
        self.scanner.peek_rewind(1);
        prim = self.type_path();
      }
      Keyword(kw) => {
        match kw {
          PrimitiveType(tp) => {
            prim = GriddedObject::new_rect_array(start_coords, Type::Primitive(tp.clone()), decisive_token.end_pos());
          }
          _ => {
            self.error_str(&decisive_token, "Primary Type, Identifier or '('", "type_array : Keyword/Other");
            return GriddedObject::new_point(Type::Error, 0, 0);
          }
        }
      }
      LeftParen => {
        self.scanner.peek_rewind(1);
        prim = self.type_lambda_tuple();
      }
      _ => {
        self.error_str(&decisive_token, "Primary Type, Identifier or '('", "type_array : Other");
        return GriddedObject::new_point(Type::Error, 0, 0);
      }
    }
    self.type_array(start_coords, prim)
  }

  fn type_path(&mut self) -> GriddedObject<Type> {
    let start_coords = self.scanner.peek_coords()[0];
    let path = self.path();
    if matches!(self.scanner.peek_or_eof().obj, Less) {
      self.scanner.peek_rewind(1);
      GriddedObject::new_rect_array(start_coords, Type::Object(path, Some(self.generics())), self.scanner.peek_coords()[0])
    } else {
      self.scanner.peek_rewind(1);
      GriddedObject::new_rect_array(start_coords, Type::Object(path, None), self.scanner.peek_coords()[0])
    }
  }

  fn type_lambda_tuple(&mut self) -> GriddedObject<Type> {
    let start_coords = self.scanner.peek_coords()[0];
    token_match!(self, LeftParen, "type_lambda_tuple : (");
    let mut decisive_token = self.scanner.peek_or_eof().clone();
    match decisive_token.obj {
      RightParen => {
        token_match!(self, ArrowSimpleRight, "type_lambda_tuple : RightParen/->");
        self.type_lambda_return(start_coords, Vec::new())
      }
      _ => {
        self.scanner.peek_rewind(1);
        let mut types = vec![self.type_expr()];
        while !matches!({
          decisive_token  = self.scanner.peek_or_eof().clone();
          &decisive_token.obj
        }, RightParen) {
          self.scanner.peek_rewind(1);
          token_match!(self, Comma, "type_lambda_tuple : params/,");
          types.push(self.type_expr())
        }
        if types.len() == 1 {
          token_match!(self, ArrowSimpleRight, "type_lambda_tuple : params/->");
          self.type_lambda_return(start_coords, types)
        } else if matches!(&self.scanner.peek_or_eof().obj, ArrowSimpleRight) {
          self.type_lambda_return(start_coords, types)
        } else {
          self.scanner.peek_rewind(1);
          GriddedObject::new_rect_array(start_coords, Type::ObjectTuple(types), self.scanner.peek_coords()[0])
        }
      }
    }
  }

  fn type_lambda_return(&mut self, start_coords: [usize; 2], types: Vec<GriddedObject<Type>>) -> GriddedObject<Type> {
    let mut decisive_token = self.scanner.peek_or_eof().clone();
    match &decisive_token.obj {
      LeftParen => {
        decisive_token = self.scanner.peek_or_eof().clone();
        if matches!(&decisive_token.obj, RightParen) {
          GriddedObject::new_rect_array(start_coords, Type::LambdaType(types, Box::new(None)), self.scanner.peek_coords()[0])
        } else {
          self.scanner.peek_rewind(1);
          GriddedObject::new_rect_array(start_coords, Type::LambdaType(types, Box::new(Some(self.type_expr()))), {
            token_match!(self, RightParen, "type_lambda_return : RightParen/LeftParen/Other");
            self.scanner.peek_coords()[0]
          })
        }
      }
      _ => {
        self.scanner.peek_rewind(1);
        GriddedObject::new_rect_array(start_coords, Type::LambdaType(types, Box::new(Some(self.type_expr()))), self.scanner.peek_coords()[0])
      }
    }
  }

  fn type_array(&mut self, start_coords: [usize; 2], prim: GriddedObject<Type>) -> GriddedObject<Type> {
    let mut arrays = Vec::new();
    let mut decisive_token;
    while matches!(&self.scanner.peek_or_eof().obj, LeftBracket) {
      decisive_token = self.scanner.peek_or_eof().clone();
      match &decisive_token.obj {
        RightBracket => arrays.push(None),
        Number(_) => {
          arrays.push(Some(decisive_token));
          token_match!(self, RightBracket, "type_array :]");
        }
        _ => {
          self.error_str(&decisive_token, "'[' or Number", "type_expr : [Number?]");
        }
      }
    }
    self.scanner.peek_rewind(1);
    if arrays.len() > 0 {
      GriddedObject::new_rect_array(start_coords, Type::Array(Box::new(prim), arrays), self.scanner.peek_coords()[0])
    } else {
      prim
    }
  }

  fn path(&mut self) -> Vec<GriddedObject<Token>> {
    let mut vec = vec![identifier_match!(self, "Path")];
    while matches!(self.scanner.peek_or_eof().obj, Dot) {
      vec.push(identifier_match!(self, "Path"));
    }
    self.scanner.peek_rewind(1);
    vec
  }

  fn generics(&mut self) -> Vec<GriddedObject<Generic>> {
    token_match!(self, Less, "generics : <");
    let mut vec = Vec::new();
    let mut temp = self.scanner.peek_or_eof();
    if matches!(&temp.obj, Greater) {
      return vec;
    } else {
      self.scanner.peek_rewind(1);
      vec.push(self.wildcard());
    }
    while matches!(&self.scanner.peek_or_eof().obj, Comma) {
      vec.push(self.wildcard());
    }
    self.scanner.peek_rewind(1);
    token_match!(self, Greater, "generics : >");
    vec
  }

  fn wildcard(&mut self) -> GriddedObject<Generic> {
    let start_coords = self.scanner.peek_coords()[0];
    let temp_a = self.scanner.peek_or_eof().clone();
    let temp_b = self.scanner.peek_or_eof().clone();
    if matches!(temp_b.obj, LessEqual | GreaterEqual) {
      GriddedObject::new_rect_array(start_coords, Generic {
        wildcard: match temp_b.obj {
          LessEqual => Wildcard::Super,
          GreaterEqual => Wildcard::Sub,
          _ => {
            self.error_str(&temp_b, "'>' or '<'", "wildcard : Super or Sub");
            Wildcard::Exact
          }
        },
        id: Some(temp_a),
        type_id: Some(self.type_expr()),
      }, self.scanner.peek_previous_coords()[1])
    } else {
      self.scanner.peek_rewind(1);
      if matches!(temp_a.obj, QuestionMark) {
        GriddedObject::new_rect_array(start_coords, Generic {
          wildcard: Wildcard::Exact,
          id: Some(temp_a),
          type_id: None,
        }, self.scanner.peek_previous_coords()[1])
      } else {
        self.scanner.peek_rewind(1);
        GriddedObject::new_rect_array(start_coords, Generic {
          wildcard: Wildcard::Exact,
          id: None,
          type_id: Some(self.type_expr()),
        }, self.scanner.peek_previous_coords()[1])
      }
    }
  }

  fn macro_annotation(&mut self) -> Option<GriddedObject<Expression>> {
    let start_peek = self.scanner.peek_get();
    let mut coords = self.scanner.peek_previous_coords()[1];
    if self.scanner.peek_search_predicate(|gt| matches!(gt.obj, At | HashTag)) {
      let is_annotation: bool = {
        self.scanner.peek_rewind(1);
        matches!(self.scanner.peek().unwrap().obj, At)
      };
      token_match!(self, LeftBracket, "macro_annotation");
      Some(GriddedObject::new_rect_array(coords, MacroAnnotation(crate::ast::expr::MacroAnnotation {
        is_annotation,
        path: {
          self.path()
        },
        mapping: {
          token_match!(self, LeftParen, "macro_annotation");
          let mut map = HashMap::new();
          if is_annotation {
            let mut temp;
            while matches!({temp = self.scanner.peek_or_eof().clone(); &temp.obj}, Identifier(_)| Comma) {
              if matches!(&temp.obj, Comma) { continue; }
              token_match!(self, Equal, "macro_annotation : Mapping");
              map.insert(temp, tokens_match!(self, "macro_annotation : Mapping", Keyword(_) | Number(_) | String(_)));
            }
            self.scanner.peek_rewind(1);
          }
          map
        },
        ids: {
          let mut ids = vec![];
          if !is_annotation {
            let mut temp;
            while matches!({temp = self.scanner.peek_or_eof().clone(); &temp.obj}, Identifier(_)| Comma) {
              if matches!(&temp.obj, Comma) { continue; }
              ids.push(temp);
            }
            self.scanner.peek_rewind(1);
          }
          token_match!(self, RightParen, "macro_annotation : Close");
          coords = self.scanner.peek_previous_coords()[1];
          token_match!(self, RightBracket, "macro_annotation : Close");
          ids
        },
      }), coords))
    } else {
      self.scanner.peek_set(start_peek);
      None
    }
  }

  fn error(&mut self, token: &GriddedObject<Token>, expected: Option<Token>, loc: &'static str) {
    self.visualizer.print_visualisation_tok(token);
    if expected.is_some() {
      eprintln!("Unexpected {} at x: {} y: {}. Expected: {}", token.obj, token.start_pos_x, token.start_pos_y, expected.unwrap());
    } else {
      eprintln!("Unexpected {} at x: {} y: {}.", token.obj, token.start_pos_x, token.start_pos_y);
    }
    eprintln!("{}:{}:{}", self.path.display(), token.start_pos_y + 1, token.start_pos_x + 1);
    panic!("{}", loc);
  }

  fn error_str(&mut self, token: &GriddedObject<Token>, expected: &str, loc: &'static str) {
    self.visualizer.print_visualisation_tok(token);
    eprintln!("Unexpected {} at x: {} y: {}. Expected: {}", token.obj, token.start_pos_x, token.start_pos_y, expected);
    eprintln!("{}:{}:{}", self.path.display(), token.start_pos_y + 1, token.start_pos_x + 1);
    panic!("{}", loc);
  }
}

fn convert_vec_to_identifiers(vec: &Vec<GriddedObject<Expression>>) -> Option<Vec<GriddedObject<Token>>> {
  let mut rep_vec = Vec::new();
  for expr in vec.iter() {
    match &expr.obj {
      Primary(prim) => match &prim {
        Id(tk) => {
          rep_vec.push(GriddedObject::new_rect_array(expr.start_pos(), tk.clone(), expr.end_pos()))
        }
        _ => return None
      }
      _ => return None
    }
  }
  Some(rep_vec)
}

