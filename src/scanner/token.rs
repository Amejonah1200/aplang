use std::borrow::Borrow;

use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

pub struct GriddedToken {
  pub token: Token,
  pub pos_x: usize,
  pub pos_y: usize,
}

impl GriddedToken {
  pub fn new(token: Token, pos_x: usize, pos_y: usize) -> GriddedToken {
    GriddedToken { token, pos_x, pos_y }
  }
}

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
  Dot,
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
  // ^
  PercentageEqual, // %

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

pub const KEYWORDS: [&str; 15] = ["var", "if", "while", "for", "pub", "pak", "prot", "chr", "str", "const", "int", "float", "double", "long", "bool"];

#[derive(FromPrimitive, ToPrimitive)]
pub enum Keyword {
  Var,
  If,
  While,
  For,
  Pub,
  Pak,
  Prot,
  Chr,
  Str,
  Const,
  Int,
  Float,
  Double,
  Long,
  Bool,
}

pub fn parse_keyword(str: &std::string::String) -> Option<Keyword> {
  for i in 0..15 {
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
    Token::Keyword(kw) => String::from("Keyword[\"") + KEYWORDS.get(kw.to_usize().unwrap()).unwrap_or(&"?") + "\"]"
  }
}
