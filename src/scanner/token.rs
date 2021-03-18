use std::borrow::Borrow;
use std::fmt::{Debug, Display, Formatter};

use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

use crate::ast::expr::Expression;
use crate::scanner::token::Token::NewLine;

#[derive(Debug, Clone)]
pub struct GriddedObject<T> {
  pub rect: bool,
  pub start_pos_x: usize,
  pub start_pos_y: usize,
  pub obj: T,
  pub end_pos_x: usize,
  pub end_pos_y: usize,
}

impl<T> GriddedObject<T> {
  pub fn new_point(obj: T, pos_x: usize, pos_y: usize) -> Self {
    GriddedObject { rect: false, start_pos_x: pos_x, start_pos_y: pos_y, obj, end_pos_x: pos_x, end_pos_y: pos_y }
  }

  pub fn new_point_array(obj: T, pos: [usize; 2]) -> Self {
    GriddedObject::new_point(obj, pos[0], pos[1])
  }

  pub fn new_rect(start_pos_x: usize, start_pos_y: usize, obj: T, end_pos_x: usize, end_pos_y: usize) -> Self {
    GriddedObject { rect: false, start_pos_x, start_pos_y, obj, end_pos_x, end_pos_y }
  }

  pub fn new_rect_array(start_pos: [usize; 2], obj: T, end_pos: [usize; 2]) -> Self {
    GriddedObject { rect: false, start_pos_x: start_pos[0], start_pos_y: start_pos[1], obj, end_pos_x: end_pos[0], end_pos_y: end_pos[1] }
  }

  pub fn start_pos(&self) -> [usize; 2] {
    [self.start_pos_x, self.start_pos_y]
  }

  pub fn end_pos(&self) -> [usize; 2] {
    [self.end_pos_x, self.end_pos_y]
  }
}

impl Display for GriddedObject<Token> {
  fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
    write!(f, "{} [{} {}]", self.obj, self.start_pos_x, self.start_pos_y)
  }
}

impl Display for GriddedObject<Expression> {
  fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
    write!(f, "{} [{} {} -> {} {}]", self.obj, self.start_pos_x, self.start_pos_y, self.end_pos_x, self.end_pos_y)
  }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Token {
  /**
  * Parenthesis: (
  */
  LeftParen,
  /**
  * Parenthesis: )
  */
  RightParen,
  /**
  * Curly brace: {
  */
  LeftBrace,
  /**
  * Curly brace: }
  */
  RightBrace,
  /**
  * Square Bracket: [
  */
  LeftBracket,
  /**
  * Square Bracket: ]
  */
  RightBracket,

  Comma,
  Semicolon,
  Slash,
  HashTag,
  /**
  * At: @
  */
  At,
  /**
  * Tilde: ~
  */
  Tilde,
  Apostrophe,
  DoubleQuotes,
  QuestionMark,
  QuestionMarkColon,
  Percentage,
  Dollar,
  /**
  * Circumflex: ^
  */
  Circumflex,
  Star,
  StarStar,
  /**
  * Vertical Bar: |
  */
  VerticalBar,
  /**
  * Vertical Bar x2: ||
  */
  DoubleVerticalBar,
  /**
  * Ampersand: &
  */
  Ampersand,
  /**
  * Ampersand x2: &&
  */
  AmpersandAmpersand,
  Bang,
  BangEqual,

  Equal,
  EqualEqual,
  PlusEqual,
  // +
  MinusEqual,
  // -
  StarEqual,
  // *
  StarStarEqual,
  // **
  SlashEqual,
  // /
  CircumflexEqual,
  // ^
  AmpersandEqual,
  // &
  VerticalBarEqual,
  // |
  QuestionMarkEqual,
  // ?
  TildeEqual,
  // %
  PercentageEqual,
  // <<
  LessLessEqual,
  // >>
  GreaterGreaterEqual,
  // >>>
  GreaterGreaterGreaterEqual,

  Greater,
  GreaterGreater,
  GreaterGreaterGreater,
  GreaterEqual,
  Less,
  LessLess,
  LessEqual,
  Minus,
  MinusMinus,
  Plus,
  PlusPlus,
  Colon,
  ColonColon,

  Dot,
  DotDot,
  DotDotEqual,

  /**
  * Arrow Double: ->
  */
  ArrowSimpleRight,
  /**
  * Arrow Double: <-
  */
  ArrowSimpleLeft,
  /**
  * Arrow Double: =>
  */
  ArrowDoubleRight,

  // Literals.
  String(String),
  Number(String),
  Identifier(String),
  Keyword(Keyword),
  Word(String),
  Char(char),
  Space(usize),
  Comment(String),
  NewLine,
  EscapedChar(char),

  Unknown(String),
  EOF,
}

pub const KEYWORDS: [&str; 18] = ["if", "while", "for", "var", "pub", "pak", "prot", "const", "chr", "str", "int", "float", "double", "long", "bool", "class", "super", "self"];

#[derive(FromPrimitive, ToPrimitive, Debug, Clone, Eq, PartialEq, Hash)]
pub enum Keyword {
  // flow
  If,
  While,
  For,
  // var
  Var,
  // visibility
  Pub,
  Pak,
  Prot,
  // final
  Const,
  // data
  Chr,
  Str,
  Int,
  Float,
  Double,
  Long,
  Bool,
  // structs
  Class,
  Super,
  SelfKey,
}

pub fn parse_keyword(str: &std::string::String) -> Option<Keyword> {
  for i in 0..KEYWORDS.len() {
    if str.cmp(KEYWORDS.get(i).unwrap().to_string().borrow()) == core::cmp::Ordering::Equal {
      return Some(Keyword::from_usize(i).unwrap());
    }
  }
  None
}

pub fn escaped_char_to_char(token: Token) -> Option<char> {
  match token {
    Token::EscapedChar(chr) => match chr {
      't' => Some('\t'),
      'r' => Some('\r'),
      '0' => Some('\0'),
      'n' => Some('\n'),
      '"' => Some('"'),
      '\'' => Some('\''),
      '\\' => Some('\\'),
      _ => None
    }
    _ => None
  }
}

impl Display for Token {
  fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
    write!(f, "{}", token_to_string(self))
  }
}

pub fn token_to_string(token: &Token) -> String {
  match token {
    Token::LeftParen => String::from("LeftParen"),
    Token::RightParen => String::from("RightParen"),
    Token::LeftBrace => String::from("LeftBrace"),
    Token::RightBrace => String::from("RightBrace"),
    Token::LeftBracket => String::from("LeftBracket"),
    Token::RightBracket => String::from("RightBracket"),
    Token::Comma => String::from("Comma"),
    Token::Dot => String::from("Dot"),
    Token::DotDot => String::from("DotDot"),
    Token::DotDotEqual => String::from("DotDotEqual"),
    Token::Colon => String::from("Colon"),
    Token::ColonColon => String::from("ColonColon"),
    Token::Minus => String::from("Minus"),
    Token::MinusMinus => String::from("MinusMinus"),
    Token::Plus => String::from("Plus"),
    Token::PlusPlus => String::from("PlusPlus"),
    Token::Semicolon => String::from("Semicolon"),
    Token::Slash => String::from("Slash"),
    Token::HashTag => String::from("HashTag"),
    Token::Tilde => String::from("Tilde"),
    Token::At => String::from("At"),
    Token::Apostrophe => String::from("Apostrophe"),
    Token::DoubleQuotes => String::from("DoubleQuotes"),
    Token::QuestionMark => String::from("QuestionMark"),
    Token::QuestionMarkColon => String::from("QuestionMarkColon"),
    Token::Percentage => String::from("Percentage"),
    Token::Dollar => String::from("Dollar"),
    Token::Circumflex => String::from("Circumflex"),
    Token::Star => String::from("Star"),
    Token::StarStar => String::from("StarStar"),
    Token::VerticalBar => String::from("VerticalBar"),
    Token::DoubleVerticalBar => String::from("DoubleVerticalBar"),
    Token::Ampersand => String::from("Ampersand"),
    Token::AmpersandAmpersand => String::from("AmpersandAmpersand"),
    Token::Bang => String::from("Bang"),
    Token::BangEqual => String::from("BangEqual"),
    Token::Equal => String::from("Equal"),
    Token::EqualEqual => String::from("EqualEqual"),
    Token::Greater => String::from("Greater"),
    Token::GreaterGreater => String::from("GreaterGreater"),
    Token::GreaterGreaterGreater => String::from("GreaterGreaterGreater"),
    Token::GreaterEqual => String::from("GreaterEqual"),
    Token::Less => String::from("Less"),
    Token::LessLess => String::from("LessLess"),
    Token::LessEqual => String::from("LessEqual"),
    Token::ArrowSimpleRight => String::from("ArrowSimpleRight"),
    Token::ArrowSimpleLeft => String::from("ArrowSimpleLeft"),
    Token::ArrowDoubleRight => String::from("ArrowDoubleRight"),
    Token::String(str) => String::from("String[\"") + str + "\"]",
    Token::Number(nb) => String::from("Number[") + nb + "]",
    Token::Word(kw) => String::from("Word[") + kw + "]",
    Token::Char(c) => String::from("Char['") + c.to_string().as_str() + "']",
    Token::Unknown(str) => str.to_string(),
    Token::EOF => String::from("EOF"),
    Token::PlusEqual => String::from("PlusEqual"),
    Token::MinusEqual => String::from("MinusEqual"),
    Token::StarEqual => String::from("StarEqual"),
    Token::StarStarEqual => String::from("StarStarEqual"),
    Token::SlashEqual => String::from("SlashEqual"),
    Token::LessLessEqual => String::from("LessLessEqual"),
    Token::GreaterGreaterEqual => String::from("GreaterGreaterEqual"),
    Token::GreaterGreaterGreaterEqual => String::from("GreaterGreaterGreaterEqual"),
    Token::CircumflexEqual => String::from("CircumflexEqual"),
    Token::AmpersandEqual => String::from("AmpersandEqual"),
    Token::VerticalBarEqual => String::from("VerticalBarEqual"),
    Token::QuestionMarkEqual => String::from("QuestionMarkEqual"),
    Token::TildeEqual => String::from("TildeEqual"),
    Token::PercentageEqual => String::from("PercentageEqual"),
    Token::Space(amount) => String::from(" x") + &amount.to_string(),
    Token::NewLine => String::from("\n"),
    Token::Comment(comment) => String::from("cmt: ") + comment,
    Token::EscapedChar(chr) => String::from("\\") + &chr.to_string(),
    Token::Identifier(id) => String::from("Id[\"") + id + "\"]",
    Token::Keyword(kw) => String::from("Keyword[\"") + KEYWORDS.get(kw.to_usize().unwrap()).unwrap_or(&"?") + "\"]",
  }
}
