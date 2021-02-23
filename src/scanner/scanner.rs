use std::borrow::{Borrow, BorrowMut};
use std::cmp::Ordering;
use std::fs;
use std::io::Result;
use std::path::Path;

use predicates::{BoxPredicate, Predicate};
use predicates::prelude::predicate::function;

use crate::scanner::token::{escaped_char_to_char, GriddedToken, parse_keyword, Token};
use crate::scanner::token::Token::*;

pub struct Scanner<'a, T> {
  position: usize,
  peek: usize,
  elements: &'a Vec<T>,
}

impl<'a, T> Scanner<'a, T> {
  pub fn new(tokens_in: &'a Vec<T>) -> Self {
    Scanner { position: 0, peek: 0, elements: tokens_in }
  }

  pub fn next_adv(&mut self, nb: usize, consume: bool, use_peek: bool) {
    if consume {
      if use_peek {
        self.pos_to_peek();
      } else {
        self.position += nb;
        self.peek_reset();
      }
    } else if use_peek {
      self.peek += nb;
    }
  }

  pub fn consume(&mut self) -> Option<&T> {
    let pos_old = self.position;
    let result = self.elements.get(pos_old);
    if result.is_some() {
      self.position += 1;
      self.peek = self.position;
    }
    result
  }

  pub fn peek(&mut self) -> Option<&T> {
    let pos_peek = self.peek;
    let result = self.elements.get(pos_peek);
    if result.is_some() { self.peek += 1; }
    result
  }

  pub fn peek_search_predicate(&mut self, predicate: BoxPredicate<T>) -> bool {
    match self.peek() {
      Some(peek) => predicate.eval(peek),
      None => false
    }
  }

  pub fn peek_adv(&mut self, nb: usize) {
    if nb == 0 { return; }
    if self.peek + nb < self.elements.len() {
      self.peek += nb;
    } else {
      self.peek = self.elements.len();
    }
  }

  pub fn peek_rewind(&mut self, nb: usize) {
    if self.peek > nb {
      self.peek -= nb;
    } else {
      self.peek = 0;
    }
  }

  pub fn peek_reset(&mut self) {
    self.peek = self.position;
  }

  pub fn peek_is_eof(&self) -> bool {
    self.peek >= self.elements.len()
  }

  pub fn pos_adv(&mut self, nb: usize) {
    if nb == 0 { return; }
    if self.position + nb < self.elements.len() {
      self.position += nb;
    } else {
      self.position = self.elements.len();
    }
    self.peek_reset();
  }

  pub fn pos_rewind(&mut self, nb: usize) {
    if self.position > nb {
      self.position -= nb;
    } else {
      self.position = 0;
    }
    self.peek_reset();
  }

  pub fn pos_reset(&mut self) {
    self.position = 0;
    self.peek_reset();
  }

  pub fn pos_to_peek(&mut self) {
    if self.peek > self.elements.len() {
      self.peek = self.elements.len();
    }
    self.position = self.peek;
  }

  pub fn pos_is_eof(&self) -> bool {
    self.position >= self.elements.len()
  }

  pub fn pos(&self) -> usize { self.position }
}

impl<'a> Scanner<'a, char> {
  pub fn next_search_chars(&mut self, consume: bool, use_peek: bool, predicate: BoxPredicate<char>) -> std::string::String {
    let mut result = std::string::String::new();
    let start_pos = if use_peek { self.peek } else { self.position };
    let mut i = 0;
    while match self.elements.get(start_pos + i) {
      Some(c) if predicate.eval(c) => {
        result.push(*c);
        true
      }
      _ => false
    } { i += 1; }
    self.next_adv(i, consume, use_peek);
    result
  }

  pub fn next_chars(&mut self, mut nb: usize, consume: bool, use_peek: bool, fail_on_not_reach: bool) -> Option<std::string::String> {
    let mut result = std::string::String::new();
    let start_pos = if use_peek { self.peek } else { self.position };
    for i in 0..nb {
      match self.elements.get(start_pos + i) {
        Some(c) => result.push(*c),
        None => if fail_on_not_reach { return None; } else {
          nb = i;
          break;
        }
      }
    }
    self.next_adv(nb, consume, use_peek);
    Some(result)
  }

  pub fn consume_search_chars(&mut self, predicate: BoxPredicate<char>) -> std::string::String {
    self.next_search_chars(true, false, predicate)
  }

  pub fn consume_chars(&mut self, nb: usize, fail_on_not_reach: bool) -> Option<std::string::String> {
    self.next_chars(nb, true, false, fail_on_not_reach)
  }

  pub fn peek_char(&mut self) -> Option<&char> {
    let pos_peek = self.peek;
    let result = self.elements.get(pos_peek);
    if result.is_some() { self.peek += 1; }
    result
  }

  pub fn peek_chars(&mut self, nb: usize, fail_on_not_reach: bool) -> Option<std::string::String> {
    self.next_chars(nb, false, true, fail_on_not_reach)
  }

  pub fn peek_search(&mut self, str: &str) -> bool {
    match self.next_chars(str.len(), false, true, true) {
      Some(peek) => peek.cmp(str.to_string().borrow()) == Ordering::Equal,
      None => false
    }
  }

  pub fn peek_search_char(&mut self, chr: char) -> bool {
    match self.peek_char() {
      Some(peek) => *peek == chr,
      None => false
    }
  }

  pub fn peek_search_chars(&mut self, predicate: BoxPredicate<char>) -> std::string::String {
    self.next_search_chars(false, true, predicate)
  }
}

/**
* Omit the first char, cuz it was already consumed in scan.
* searches for the longest token, provide sorted tokens.
*/
fn search_and_consume_tokens(scanner: &mut Scanner<char>, tokens: Vec<(&str, Token)>) -> Option<Token> {
  let mut result = None;
  let mut nb_chars = 0;
  for t in tokens {
    if nb_chars < t.0.len() && scanner.peek_search(t.0) {
      result = Some(t.1);
      nb_chars = t.0.len();
    }
    scanner.peek_reset();
  }
  if result.is_some() { scanner.pos_adv(nb_chars); }
  result
}

#[macro_export]
macro_rules! post_set {
  ($var:expr, $amount:expr) => {
    {
      let old_value = $var;
      $var = $amount;
      old_value
    }
  };
}

#[macro_export]
macro_rules! post_inc {
  ($var:expr, $amount:expr) => {
    {
      let old_value = $var;
      $var += $amount;
      old_value
    }
  };
}

pub fn scan(path: &Path) -> Result<Box<Vec<GriddedToken>>> {
  let string = fs::read_to_string(path);
  let elemts = match string {
    Ok(str) => str.chars().collect(),
    Err(err) => { return Result::Err(err); }
  };
  let mut scanner = Scanner { position: 0, peek: 0, elements: &elemts };
  macro_rules! search_equal {
  ($equal:expr, $single:expr$(, $x:expr)*) => {
      search_and_consume_tokens(scanner.borrow_mut(), vec![
        $($x,)*
        ("=", $equal)
      ]).unwrap_or($single)
  };
}

  macro_rules! search_token {
  ($single:expr$(, $x:expr)*) => {
      search_and_consume_tokens(scanner.borrow_mut(), vec![
        $($x,)*
      ]).unwrap_or($single)
  };
}
  let mut vec: Box<Vec<GriddedToken>> = Box::new(Vec::new());
  let mut pos_temp = 0usize;
  let mut pos_x = 0usize;
  let mut pos_y = 0usize;
  while match scanner.consume() {
    Some(c) => {
      vec.push(match match *c {
        '(' => LeftParen,
        ')' => RightParen,
        '{' => LeftBrace,
        '}' => RightBrace,
        '[' => LeftBracket,
        ']' => RightBracket,
        ',' => Comma,
        '@' => At,
        '.' => search_token!(Dot, (".", DotDot), (".=", DotDotEqual)),
        ';' => Semicolon,
        ':' => search_token!(Colon,(":", ColonColon)),
        '-' => search_equal!(MinusEqual, Minus, (">", ArrowSimpleRight), ("-", MinusMinus)),
        '+' => search_equal!(PlusEqual, Plus, ("+", PlusPlus)),
        '/' => {
          if scanner.peek_search_char('/') {
            scanner.pos_adv(1);
            let mut cmt = std::string::String::new();
            while match scanner.consume() {
              None => false,
              Some(chr) => match chr {
                '\r' => !scanner.peek_search_char('\n'),
                '\n' => {
                  scanner.pos_rewind(1);
                  false
                }
                chr => {
                  cmt.push(*chr);
                  true
                }
              }
            } {}
            Comment(cmt)
          } else {
            search_equal!(SlashEqual, Slash)
          }
        }
        '#' => HashTag,
        '\r' => if scanner.peek_search_char('\n') {
          scanner.pos_to_peek();
          NewLine
        } else { Unknown('\r'.to_string()) },
        '\n' => NewLine,
        '~' => search_equal!(TildeEqual, Tilde),
        '?' => search_equal!(QuestionMarkEqual, QuestionMark, (":", QuestionMarkColon)),
        '%' => search_equal!(PercentageEqual, Percentage),
        '$' => Dollar,
        '^' => search_equal!(CircumflexEqual, Circumflex),
        '*' => search_equal!(StarEqual, Star, ("*", StarStar), ("*=", StarStarEqual)),
        '|' => search_equal!(VerticalBarEqual, VerticalBar, ("|", DoubleVerticalBar)),
        '&' => search_equal!(AmpersandEqual, Ampersand, ("&", AmpersandAmpersand)),
        '!' => search_equal!(BangEqual, Bang),
        '=' => search_equal!(EqualEqual, Equal, (">", ArrowDoubleRight)),
        '>' => search_equal!(GreaterEqual, Greater, (">", GreaterGreater), (">>", GreaterGreaterGreater)),
        '<' => search_equal!(LessEqual, Less, ("-", ArrowSimpleLeft),("<", LessLess)),
        ' ' | '\t' => {
          let mut amount = 1;
          while match scanner.peek_char() {
            Some(chr) => {
              if *chr == ' ' || *chr == '\t' {
                amount += 1;
                true
              } else {
                false
              }
            }
            None => false
          } {}
          scanner.pos_adv(amount - 1);
          Space(amount)
        }
        '\\' => match scanner.consume() {
          Some(chr) => EscapedChar(*chr),
          None => Char('\\')
        },
        '"' => {
          let mut str = std::string::String::new();
          let mut result = None;
          while match scanner.peek() {
            Some(chr) => {
              match chr {
                '\\' => {
                  match escaped_char_to_char(EscapedChar(*scanner.peek_char().unwrap_or('a'.borrow()))) {
                    Some(esc_chr) => str.push(esc_chr),
                    None => {
                      str.push('\\');
                      scanner.peek_rewind(1)
                    }
                  }
                  true
                }
                '\"' => {
                  result.replace(String(str.to_string()));
                  scanner.pos_to_peek();
                  false
                }
                '\r' | '\n' => {
                  scanner.peek_reset();
                  false
                } // TODO Errors
                _ => {
                  str.push(*chr);
                  true
                }
              }
            }
            None => false
          } {}
          result.unwrap_or(DoubleQuotes)
        }
        '\'' => {
          let mut result = None;
          match scanner.peek_char() {
            Some(chr) => {
              let idk_how_to_name = *chr;
              if *chr == '\\' {
                match scanner.peek_char() {
                  Some(chr2) => {
                    let chr_2_esc = *chr2;
                    if scanner.peek_search_char('\'') {
                      match escaped_char_to_char(EscapedChar(chr_2_esc)) {
                        Some(esc_chr) => {
                          result.replace(Char(esc_chr));
                          scanner.pos_to_peek();
                        }
                        _ => {}
                      }
                    }
                  }
                  _ => {}
                }
              } else if scanner.peek_search_char('\'') {
                result.replace(Char(idk_how_to_name));
                scanner.pos_to_peek();
              }
            }
            _ => {}
          }
          result.unwrap_or(Apostrophe)
        }
        '0'..='9' => {
          let mut number = std::string::String::new();
          number.push(*c);
          let mut has_dot = false;
          while match scanner.consume() {
            Some(chr) => {
              match chr {
                '0'..='9' | 'a'..='z' | 'A'..='Z' => {
                  number.push(*chr);
                  true
                }
                '.' if !has_dot => {
                  if ('0'..='9').contains(scanner.peek_char().unwrap_or('a'.borrow())) {
                    has_dot = true;
                    number.push('.');
                    true
                  } else { false }
                }
                _ => {
                  scanner.pos_rewind(1);
                  false
                }
              }
            }
            None => false
          } {}
          Number(number)
        }
        'a'..='z' | 'A'..='Z' | '_' => {
          let mut word = std::string::String::new();
          word.push(*c);
          word.push_str(scanner.consume_search_chars(BoxPredicate::new(function(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '$' | '_')))).as_str());
          match parse_keyword(&word) {
            Some(kw) => Keyword(kw),
            None => Identifier(word)
          }
        }
        other => Unknown(other.to_string())
      } {
        NewLine => GriddedToken::new(NewLine, {
          post_set!(pos_temp, scanner.pos());
          post_set!(pos_x, 0)
        }, post_inc!(pos_y, 1)),
        tok => GriddedToken::new(tok, {
          let diff = scanner.pos() - post_set!(pos_temp, scanner.pos());
          post_inc!(pos_x, diff)
        }, pos_y)
      });
      true
    }
    None => {
      // vec.push(EOF); Who needs this actually?
      false
    }
  } {
    scanner.peek_reset();
  }
  Ok(vec)
}

/**
* Get rid of Spaces, NewLines and Unknown
*/
pub fn clean_up(vec: &mut Vec<GriddedToken>) {
  vec.retain(|t| !matches!(t.token, Space(_) | NewLine | Unknown(_)))
}
