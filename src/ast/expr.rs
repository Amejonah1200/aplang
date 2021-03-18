use std::collections::HashMap;

use crate::scanner::token::{Token, GriddedObject};
use std::fmt::{Display, Formatter};

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
  * visibility     → "prot" | "pub" | "pak"
  */
  Visibility(Visibility),

  /**
  * classDecl      → macro_annotation* visibility? "class" IDENTIFIER ( "<" IDENTIFIER ("," IDENTIFIER)* ">")?
  *               (":" IDENTIFIER ("," IDENTIFIER)*)? "{" macro_annotation* declaration* "}" ;
  */
  ClassDecl(Option<Visibility>, Token, Vec<Token>, Vec<Token>),
  /**
  * fun_decl       → visibility? IDENTIFIER? function
  */
  FunDecl(Option<Visibility>, Token, Box<Box<Expression>>),
  /**
  * var_decl       → visibility? IDENTIFIER IDENTIFIER ( "=" expression )? ";"
  */
  VarDecl(Option<Visibility>, Token, Token, Option<Box<GriddedObject<Expression>>>),

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
  * for_stmt       → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement
  *                | "for" "(" IDENTIFIER ":" expression ")" statement
  */
  ForStmt(For),
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
  * while_stmt     → "while" "(" expression ")" statement
  */
  WhileStmt(Box<GriddedObject<Expression>>, Box<GriddedObject<Expression>>),
  /**
  * do_while       → "do" block_no_return "while" "(" expression ")" ";" 
  */
  DoWhile(Box<GriddedObject<Expression>>, Box<GriddedObject<Expression>>),
  /**
  * block_no_return→ "{" declaration* "}" 
  */
  BlockNoReturn(Vec<GriddedObject<Expression>>),

  /*
  * expression     → assignment 
  */
  // Expression,
  /**
  * assignment     → call ( "+" | "-" | "*" | "**" | "/" | "&" | "|" | "<<" | ">>" | ">>>" | "^" | "?")
  *               "=" assignment | if_ns_expr ;
  */
  Assignment(Box<GriddedObject<Expression>>, Token, Box<GriddedObject<Expression>>),
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
  * unary_left     →               ( "!"  | "~"  ) unary_left | unary_left_op
  */
  UnaryLeft(Unary),
  /**
  * unary_left_op  →               ( "--" | "++" ) unary_right| unary_right  
  */
  UnaryLeftOp(Unary),
  /**
  * unary_right    → primary       ( "--" | "++" )            | call         
  */
  UnaryRight(Unary),
  /**
  * call           → primary       ( "(" arguments? ")" | "." IDENTIFIER  )* 
  */
  Call(Box<GriddedObject<Expression>>, Vec<Call>),

  /**
  * primary        → "true" | "false" | "this" | "super" | NUMBER | STRING
  *                | IDENTIFIER | "(" expression ")" | block_return
  */
  Primary(Primary),
  /**
  * block_return   → "{" declaration* expression "}" 
  */
  BlockReturn(Vec<GriddedObject<Expression>>, Box<GriddedObject<Expression>>),
  /**
  * function       → IDENTIFIER "(" parameters? ")" "{" declaration* expression? "}" 
  */
  Function(Token, Option<Box<GriddedObject<Expression>>>),
  /**
  * parameters     → IDENTIFIER ":" IDENTIFIER ( "," IDENTIFIER ":" IDENTIFIER)* 
  */
  Parameters(Token, Token, Vec<GriddedObject<Expression>>),
  /**
  * arguments      → expression ( "," expression )* 
  */
  Arguments(Vec<GriddedObject<Expression>>),
  /**
  * macro_annotation → ("@" | "#") "[" IDENTIFIER ("." IDENTIFIER)*
  *                    ("(" (IDENTIFIER ("=" ("true" | "false" | NUMBER | STRING))
  *                                      ("," IDENTIFIER ("=" ("true" | "false" | NUMBER | STRING)))*
  *                                      | IDENTIFIER ("," IDENTIFIER)* ) ")")? "]"
  */
  MacroAnnotation(MacroAnnotation),
  /**
  * Used only to satisfy the compiler.
  */
  Error,
}

impl Display for Expression {
  fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", self)
  }
}


#[derive(Debug)]
pub struct MacroAnnotation {
  pub is_annotation: bool,
  pub path: Vec<Token>,
  pub mapping: HashMap<Token, Token>,
  pub ids: Vec<Token>,
}

#[derive(Debug)]
pub enum Primary {
  Literal(Token),
  BoxedExpr(Box<GriddedObject<Expression>>),
  Block(Box<GriddedObject<Expression>>),
}

#[derive(Debug)]
pub enum Call {
  Arguments(Vec<GriddedObject<Expression>>),
  Id(Token),
  ArrayClause(GriddedObject<Expression>),
}

#[derive(Debug)]
pub enum For {
  ForStmt(Box<GriddedObject<Expression>>, Option<Box<GriddedObject<Expression>>>, Option<Box<GriddedObject<Expression>>>, Box<GriddedObject<Expression>>),
  ForItrStmt(Token, Box<GriddedObject<Expression>>),
}

#[derive(Debug)]
pub struct Comparison {
  pub sign: Token,
  pub comparison: GriddedObject<Expression>,
}

#[derive(Debug)]
pub struct Unary {
  pub op: Token,
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

#[derive(Debug)]
pub enum Operator {
  Add,
  Minus,
  Multiply,
  Power,
  Divide,
  BitAnd,
  BitOr,
  ShiftLeft,
  ShiftRight,
  ShiftRightCarry,
  BitXOr,
  Question,
}


