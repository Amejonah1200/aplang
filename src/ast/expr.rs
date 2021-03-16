use std::collections::HashMap;

use crate::scanner::token::Token;

#[derive(Debug)]
pub enum Expression {
  /**
  * program        → (macro_annotation+ "\n\n")? declaration* EOF ;
  */
  Program(Vec<Expression>, Vec<Expression>),

  /**
  * declaration    → class_decl | fun_decl | var_decl | statement ;
  */
  Declaration(Box<Expression>),

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
  VarDecl(Option<Visibility>, Token, Token, Option<Box<Expression>>),

  /**
  * statement      → exp_stmt  | for_stmt  | if_stmt | return_stmt
  *                | while_stmt | do_while | block_no_return ;
  */
  Statement(Box<Expression>),

  /**
  * expr_stmt      → expression ";" ;
  */
  ExprStmt(Box<Expression>),
  /**
  * for_stmt       → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement
  *                | "for" "(" IDENTIFIER ":" expression ")" statement
  */
  ForStmt(For),
  /**
  * if_stmt        → "if" "(" expression ")" statement
  *                  ( "else" statement )?
  */
  IfStmt(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
  /**
  * return_stmt    → "return" expression? ";"
  */
  ReturnStmt(Option<Box<Expression>>),
  /**
  * while_stmt     → "while" "(" expression ")" statement
  */
  WhileStmt(Box<Expression>, Box<Expression>),
  /**
  * do_while       → "do" block_no_return "while" "(" expression ")" ";" 
  */
  DoWhile(Box<Expression>, Box<Expression>),
  /**
  * block_no_return→ "{" declaration* "}" 
  */
  BlockNoReturn(Vec<Expression>),

  /*
  * expression     → assignment 
  */
  // Expression,
  /**
  * assignment     → call ( "+" | "-" | "*" | "**" | "/" | "&" | "|" | "<<" | ">>" | ">>>" | "^" | "?")
  *               "=" assignment | if_ns_expr ;
  */
  Assignment(Box<Expression>, Token, Box<Expression>),
  /**
  * if_ns_expr     → logic_or "?:" expression                | if_expr       
  */
  IfNsExpr(Box<Expression>, Box<Expression>),
  /**
  * if_expr        → logic_or "?"  expression ":" expression | unary_left_op 
  */
  IfExpr(Box<Expression>, Box<Expression>, Box<Expression>),
  /**
  * logic_or       → logic_and   (   "||" logic_and                       )* 
  */
  LogicOr(Box<Expression>, Vec<Expression>),
  /**
  * logic_and      → equality    (   "&&" equality                        )* 
  */
  LogicAnd(Box<Expression>, Vec<Expression>),
  /**
  * equality       → comparison  ( ( "!=" | "=="              ) comparison)* 
  */
  Equality(Box<Expression>, Vec<Unary>),
  /**
  * comparison     → term        ( ( ">"  | ">=" | "<" | "<=" ) term      )* 
  */
  Comparison(Box<Expression>, Vec<Unary>),
  /**
  * term           → factor      ( ( "-"  | "+"               ) factor    )* 
  */
  Term(Box<Expression>, Vec<Unary>),
  /**
  * factor         → bit_op      ( ( "/"  | "\*"  | "\*\*"       ) bit_op    )*
  */
  Factor(Box<Expression>, Vec<Unary>),
  /**
  * bit_op         → unary_left  ( ( "|"  | "^"  | "&"        ) unary_left)* 
  */
  BitOp(Box<Expression>, Vec<Unary>),
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
  Call(Box<Expression>, Vec<Call>),

  /**
  * primary        → "true" | "false" | "this" | "super" | NUMBER | STRING
  *                | IDENTIFIER | "(" expression ")" | block_return
  */
  Primary(Primary),
  /**
  * block_return   → "{" declaration* expression "}" 
  */
  BlockReturn(Vec<Expression>, Box<Expression>),
  /**
  * function       → IDENTIFIER "(" parameters? ")" "{" declaration* expression? "}" 
  */
  Function(Token, Option<Box<Expression>>),
  /**
  * parameters     → IDENTIFIER ":" IDENTIFIER ( "," IDENTIFIER ":" IDENTIFIER)* 
  */
  Parameters(Token, Token, Vec<Expression>),
  /**
  * arguments      → expression ( "," expression )* 
  */
  Arguments(Vec<Expression>),
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
  BoxedExpr(Box<Expression>),
  Block(Box<Expression>),
}

#[derive(Debug)]
pub enum Call {
  Arguments(Vec<Expression>),
  Id(Token),
  ArrayClause(Expression),
}

#[derive(Debug)]
pub enum For {
  ForStmt(Box<Expression>, Option<Box<Expression>>, Option<Box<Expression>>, Box<Expression>),
  ForItrStmt(Token, Box<Expression>),
}

#[derive(Debug)]
pub struct Comparison {
  pub sign: Token,
  pub comparison: Expression,
}

#[derive(Debug)]
pub struct Unary {
  pub op: Token,
  pub expr: Box<Expression>,
}

#[derive(Debug)]
pub struct Binary {
  pub right: Expression,
  pub op: Token,
  pub left: Expression,
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


