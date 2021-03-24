use std::borrow::{Borrow, BorrowMut};
use std::io::Result;

use crate::scanner::scanner::Scanner;
use crate::scanner::token::{escaped_char_to_char, GriddedObject, parse_keyword, Token};
use crate::scanner::token::Token::*;
extern crate alloc;

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

pub fn scan(str: &alloc::string::String) -> Result<Vec<GriddedObject<Token>>> {
  let chars =str.chars().collect();
  let mut scanner = Scanner::new(&chars);
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
  let mut vec: Vec<GriddedObject<Token>> = Vec::new();
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
        '>' => search_equal!(GreaterEqual, Greater, (">", GreaterGreater), (">>", GreaterGreaterGreater), (">>=", GreaterGreaterGreaterEqual)),
        '<' => search_equal!(LessEqual, Less, ("-", ArrowSimpleLeft),("<", LessLess),("<=", LessLessEqual)),
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
                }
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
                '.' => {
                  if !has_dot && ('0'..='9').contains(scanner.peek_char().unwrap_or('a'.borrow())) {
                    has_dot = true;
                    number.push('.');
                    true
                  } else {
                    scanner.pos_rewind(1);
                    false
                  }
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
          word.push_str(scanner.consume_search_chars(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '$' | '_')).as_str());
          match parse_keyword(&word) {
            Some(kw) => Keyword(kw),
            None => Identifier(word)
          }
        }
        other => Unknown(other.to_string())
      } {
        NewLine => GriddedObject::new_point(NewLine, {
          post_set!(pos_temp, scanner.pos_get());
          post_set!(pos_x, 0)
        }, post_inc!(pos_y, 1)),
        tok => {
          GriddedObject::new_vert( pos_x, pos_y,tok, {
            pos_x += (scanner.pos_get() - post_set!(pos_temp, scanner.pos_get()));
            pos_x - 1
          })
        }
      });
      true
    }
    None => {
      vec.push(GriddedObject::new_point(EOF, pos_x, pos_y));
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
pub fn clean_up(vec: &mut Vec<GriddedObject<Token>>) {
  vec.retain(|t| !matches!(t.obj, Space(_) | Unknown(_) | NewLine | Comment(_)))
}
