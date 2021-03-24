use std::borrow::Borrow;
use std::cmp::Ordering;

use crate::scanner::token::{GriddedObject, Token};
use crate::scanner::token::Token::EOF;

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

  pub fn peek_search_ignore<F1, F2>(&mut self, ignore: F1, predicate: F2) -> bool
    where F1: Fn(&T) -> bool,
          F2: Fn(&T) -> bool {
    while self.peek_search_predicate(&ignore) {}
    self.peek_rewind(1);
    self.peek_search_predicate(&predicate)
  }

  pub fn peek_search_predicate<F>(&mut self, predicate: F) -> bool
    where F: Fn(&T) -> bool {
    match self.peek() {
      Some(peek) => predicate(peek),
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

  pub fn peek_get(&self) -> usize { self.peek }
  pub fn peek_set(&mut self, peek: usize) {
    if peek < self.elements.len() {
      self.peek = peek;
    }
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

  pub fn pos_get(&self) -> usize { self.position }
  pub fn pos_set(&mut self, pos: usize) {
    if pos < self.elements.len() {
      self.position = pos;
    }
    self.peek_reset();
  }
}

impl<'a> Scanner<'a, char> {
  pub fn next_search_chars<F>(&mut self, consume: bool, use_peek: bool, predicate: F) -> std::string::String
    where F: Fn(&char) -> bool {
    let mut result = std::string::String::new();
    let start_pos = if use_peek { self.peek } else { self.position };
    let mut i = 0;
    while match self.elements.get(start_pos + i) {
      Some(c) if predicate(c) => {
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

  pub fn consume_search_chars<F>(&mut self, predicate: F) -> std::string::String
    where F: Fn(&char) -> bool {
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

  pub fn peek_search_chars<F>(&mut self, predicate: F) -> std::string::String
    where F: Fn(&char) -> bool {
    self.next_search_chars(false, true, predicate)
  }
}

impl<'a> Scanner<'a, GriddedObject<Token>> {
  pub fn peek_or_eof(&mut self) -> &GriddedObject<Token> {
    let size = self.elements.len();
    if size == 0 { panic!("A gridded token vec should at least containing an EOF Token."); }
    let result = self.elements.get(self.peek);
    if result.is_some() && !matches!(result.unwrap().obj, EOF) {
      self.peek += 1;
    }
    result.expect("A gridded token vec should terminate with an EOF Token.")
  }

  pub fn peek_rewind_not_eof(&mut self, amount: usize) {
    let result = self.elements.get(self.peek);
    if result.is_some() && !matches!(result.unwrap().obj, EOF) {
      if amount < self.peek {
        self.peek -= amount;
      } else {
        self.peek = 0;
      }
    }
  }

  pub fn peek_previous_coords(&self) -> [[usize; 2]; 2] {
    if self.peek == 0 {
      self.peek_coords()
    } else {
      let temp = self.elements.get(self.peek - 1).unwrap();
      [temp.start_pos(), temp.end_pos()]
    }
  }

  pub fn peek_coords(&self) -> [[usize; 2]; 2] {
    let temp = self.elements.get(self.peek).unwrap();
    [temp.start_pos(), temp.end_pos()]
  }

  pub fn peek_next_coords(&self) -> [[usize; 2]; 2] {
    let temp = self.elements.get(if self.peek + 1 < self.elements.len() { self.peek + 1 } else { self.peek }).unwrap();
    [temp.start_pos(), temp.end_pos()]
  }

  pub fn consume_or_eof(&mut self) -> &GriddedObject<Token> {
    let size = self.elements.len();
    if size == 0 { panic!("A gridded token vec should at least containing an EOF Token."); }
    let result = self.elements.get(self.position);
    if result.is_some() && !matches!(result.unwrap().obj, EOF) {
      self.peek += 1;
    }
    result.expect("A gridded token vec should terminate with an EOF Token.")
  }

  pub fn consume_rewind_not_eof(&mut self, amount: usize) {
    let result = self.elements.get(self.position);
    if result.is_some() && !matches!(result.unwrap().obj, EOF) {
      if amount < self.position {
        self.position -= amount;
      } else {
        self.position = 0;
      }
      self.peek_reset();
    }
  }

  pub fn consume_coords(&self) -> [usize; 2] {
    let temp = self.elements.get(self.position).unwrap();
    [temp.start_pos_x, temp.start_pos_y]
  }

  pub fn consume_next_coords(&self) -> [usize; 2] {
    let temp = self.elements.get(if self.position + 1 < self.elements.len() { self.position + 1 } else { self.position }).unwrap();
    [temp.start_pos_x, temp.start_pos_y]
  }
}
