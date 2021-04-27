use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::scanner::token::{GriddedObject, Keyword, PrimitiveTypeKeyword, Token};

#[derive(Debug)]
pub enum Expression {
  /**
  * program        → (macro_annotation+ "\n\n")? declaration* EOF ;
  */
  Program(Vec<GriddedObject<Expression>>, Vec<GriddedObject<Expression>>),

  /**
  * declaration    → class_decl | fun_decl | var_decl | statement ;
  */
  Declaration(Box<GriddedObject<Expression>>),
  /**
  * class_decl     → macro_annotation\* visibility? "class" IDENTIFIER ( generics )?
  *               (":" type ("," type)\*)? "{" declaration* "}" ;
  */
  ClassDecl(Vec<GriddedObject<Expression>>, Option<Visibility>, GriddedObject<Token>, Option<Vec<GriddedObject<Generic>>>, Option<Vec<GriddedObject<Type>>>, Vec<GriddedObject<Expression>>),
  /**
  * fun_decl       → macro_annotation* visibility? generics? type? function ;
  */
  FunDecl(Vec<GriddedObject<Expression>>, Option<Visibility>, Option<Vec<GriddedObject<Generic>>>, Option<GriddedObject<Type>>, GriddedObject<Token>, Vec<(GriddedObject<Token>, GriddedObject<Type>)>, Box<GriddedObject<Expression>>),
  /**
  * var_decl       → macro_annotation* visibility? IDENTIFIER ( ":" type )? ( "=" expression )? ";" ;
  */
  VarDecl(Vec<GriddedObject<Expression>>, Option<Visibility>, GriddedObject<Type>, GriddedObject<Token>, Option<Box<GriddedObject<Expression>>>),
  /**
   * use_decl       → "use" IDENTIFIER ( "." IDENTIFIER)* ("." "*")? ";" ;
  */
  UseDecl(Vec<GriddedObject<Token>>, bool),

  /**
  * statement      → exp_stmt  | for_stmt  | if_stmt | return_stmt
  *                | while_stmt | do_while | block_no_return ;
  */
  Statement(Box<GriddedObject<Expression>>),
  /**
  * expr_stmt      → expression ";" ;
  */
  ExprStmt(Box<GriddedObject<Expression>>),
  /**
  * for_stmt       → "for" "(" ( var_decl | exp_stmt | ";" )? expression? ";" expression? ")" statement
  *                | "for" "(" IDENTIFIER ":" expression ")" statement ;
  */
  ForStmt(For, Box<GriddedObject<Expression>>),
  /**
  * if_stmt        → "if" "(" expression ")" statement
  *                  ( "else" statement )?
  */
  IfStmt(Box<GriddedObject<Expression>>, Box<GriddedObject<Expression>>, Option<Box<GriddedObject<Expression>>>),
  /**
  * return_stmt    → "return" expression? ";"
  */
  ReturnStmt(Option<Box<GriddedObject<Expression>>>),
  /**
  * break_stmt     → "break" expression ";" ;
  */
  BreakStmt(Option<Box<GriddedObject<Expression>>>),
  /**
  * while_stmt     → "while" "(" expression ")" statement
  */
  WhileStmt(Box<GriddedObject<Expression>>, Box<GriddedObject<Expression>>),
  /**
  * do_while       → "do" block_no_return "while" "(" expression ")" ";"
  */
  DoWhile(Box<GriddedObject<Expression>>, Box<GriddedObject<Expression>>),
  /**
  * var_stmt       → "let" IDENTIFIER ( ":" type )? ( "=" expression ) ";" ;
  */
  VarStmt(GriddedObject<Token>, Option<GriddedObject<Type>>, Option<Box<GriddedObject<Expression>>>),
  /*
  * expression     → assignment
  */
  // Expression,
  /**
  * assignment     → call ( "+" | "-" | "*" | "**" | "/" | "&" | "|" | "<<" | ">>" | ">>>" | "^" | "?")
  *               "=" assignment | if_ns_expr ;
  */
  Assignment(Box<GriddedObject<Expression>>, GriddedObject<Token>, Box<GriddedObject<Expression>>),
  /**
  * if_ns_expr     → logic_or "?:" expression                | if_expr
  */
  IfNsExpr(Box<GriddedObject<Expression>>, Box<GriddedObject<Expression>>),
  /**
  * if_expr        → logic_or "?"  expression ":" expression | unary_left_op
  */
  IfExpr(Box<GriddedObject<Expression>>, Box<GriddedObject<Expression>>, Box<GriddedObject<Expression>>),
  /**
  * logic_or       → logic_and   (   "||" logic_and                       )*
  */
  LogicOr(Box<GriddedObject<Expression>>, Vec<GriddedObject<Expression>>),
  /**
  * logic_and      → equality    (   "&&" equality                        )*
  */
  LogicAnd(Box<GriddedObject<Expression>>, Vec<GriddedObject<Expression>>),
  /**
  * equality       → comparison  ( ( "!=" | "=="              ) comparison)*
  */
  Equality(Box<GriddedObject<Expression>>, Vec<Unary>),
  /**
  * comparison     → term        ( ( ">"  | ">=" | "<" | "<=" ) term      )*
  */
  Comparison(Box<GriddedObject<Expression>>, Vec<Unary>),
  /**
  * term           → factor      ( ( "-"  | "+"               ) factor    )*
  */
  Term(Box<GriddedObject<Expression>>, Vec<Unary>),
  /**
  * factor         → bit_op      ( ( "/"  | "\*"  | "\*\*"       ) bit_op    )*
  */
  Factor(Box<GriddedObject<Expression>>, Vec<Unary>),
  /**
  * bit_op         → unary_left  ( ( "|"  | "^"  | "&"        ) unary_left)*
  */
  BitOp(Box<GriddedObject<Expression>>, Vec<Unary>),
  /**
  * unary_left     →               ( "!"  | "~"  ) unary_left | unary_right  ;
  */
  UnaryLeft(Unary),
  /**
  * unary_right    → primary       ( "--" | "++" ) ( NUMBER | "(" expression ")" ) | call  ;
  */
  UnaryRight(Unary),
  /**
  * call           → primary     ( "(" arguments? ")" )? ( "." IDENTIFIER ( "(" arguments? ")" )? | array_clause )* ;
  */
  Call(Box<GriddedObject<Expression>>, Option<Vec<GriddedObject<Expression>>>, Vec<GriddedObject<Call>>),
  /**
  * primary        → LITERAL | "self" | "super" | IDENTIFIER | lambda | "(" expression ")" | block ;
  */
  Primary(Primary),
  /**
  * block          → "{" (var_decl | statement)* expression? "}" ;
  */
  Block(Vec<GriddedObject<Expression>>, Option<Box<GriddedObject<Expression>>>),
  /**
  * loop           → "loop" block ;
  */
  LoopExpr(Box<GriddedObject<Expression>>),
  /**
  * tuple          → arguments ;
  */
  Tuple(Vec<GriddedObject<Expression>>),
  /**
  * macro_annotation → ("@" | "#") "[" IDENTIFIER ("." IDENTIFIER)*
  *                    ("(" (IDENTIFIER ("=" ("true" | "false" | NUMBER | STRING))
  *                                      ("," IDENTIFIER ("=" ("true" | "false" | NUMBER | STRING)))*
  *                                      | IDENTIFIER ("," IDENTIFIER)* ) ")")? "]"
  */
  MacroAnnotation(MacroAnnotation),
  /**
  * lambda         → "(" IDENTIFIER? ("," IDENTIFIER)* ")" "->" expression ;
  */
  Lambda(Vec<GriddedObject<Token>>, Box<GriddedObject<Expression>>),
  /**
  * Used only to satisfy the compiler.
  */
  Error,
}

impl Display for Call {
  fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self)
  }
}

impl Display for Expression {
  fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self)
  }
}


#[derive(Debug)]
pub struct MacroAnnotation {
  pub is_annotation: bool,
  pub path: Vec<GriddedObject<Token>>,
  pub mapping: HashMap<GriddedObject<Token>, GriddedObject<Token>>,
  pub ids: Vec<GriddedObject<Token>>,
}


#[derive(Debug)]
pub enum Primary {
  /**
  * "true" | "false" | NUMBER | STRING | CHAR
  */
  Literal(Token),
  Id(Token),
  SuperIvk,
  SelfIvk,
}

#[derive(Debug)]
pub enum Call {
  Id(GriddedObject<Token>),
  IdArguments(GriddedObject<Token>, Vec<GriddedObject<Expression>>),
  ArrayClause(ArrayIndex),
}

#[derive(Debug)]
pub enum ArrayIndex {
  Exact(GriddedObject<Expression>),
  Start(GriddedObject<Expression>),
  End(GriddedObject<Expression>),
  StartEnd(GriddedObject<Expression>, GriddedObject<Expression>),
}

#[derive(Debug)]
pub enum For {
  ForStmt(Box<GriddedObject<Expression>>, Option<Box<GriddedObject<Expression>>>, Option<Box<GriddedObject<Expression>>>, Box<GriddedObject<Expression>>),
  ForItrStmt(GriddedObject<Token>, Box<GriddedObject<Expression>>),
}

#[derive(Debug)]
pub struct Comparison {
  pub sign: Token,
  pub comparison: GriddedObject<Expression>,
}

#[derive(Debug)]
pub enum Wildcard {
  Exact,
  Super,
  Sub,
}

#[derive(Debug)]
pub enum Type {
  Object(Vec<GriddedObject<Token>>, Option<Vec<GriddedObject<Generic>>>),
  ObjectTuple(Vec<GriddedObject<Type>>),
  LambdaType(Vec<GriddedObject<Type>>, Box<Option<GriddedObject<Type>>>),
  Primitive(PrimitiveTypeKeyword),
  Array(Box<GriddedObject<Type>>, Vec<Option<GriddedObject<Token>>>),
  /**
  * Used only to satisfy the compiler.
  */
  Error,
}

impl Display for Type {
  fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
    write!(f, "{:?}", self)
  }
}

#[derive(Debug)]
pub struct Generic {
  pub wildcard: Wildcard,
  pub id: Option<GriddedObject<Token>>,
  pub type_ids: Option<Vec<GriddedObject<Type>>>,
}

impl Display for Generic {
  fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
    write!(f, "{:?}", self)
  }
}

#[derive(Debug)]
pub struct Unary {
  pub op: GriddedObject<Token>,
  pub expr: Box<GriddedObject<Expression>>,
}

#[derive(Debug)]
pub struct Binary {
  pub right: GriddedObject<Expression>,
  pub op: Token,
  pub left: GriddedObject<Expression>,
}

#[derive(Debug)]
pub enum Visibility {
  Public,
  Protected,
  Package,
}


