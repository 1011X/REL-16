use std::ops::Range;

use super::token::{Token, Lexer};

// planned assembler features:
// + register allocation
// + labels
// + macros
// + 

#[derive(Clone)]
pub struct Parser<'src> {
	lexer: Lexer<'src>,
	peek: Option<Token>,
	line: usize,
	col: usize,
}

impl<'src> Parser<'src> {
	pub fn new(lexer: Lexer<'src>) -> Self {
		Self {
			lexer,
			peek: None,
			line: 1,
			col: 1,
		}
	}
	
	pub fn slice(&self) -> &str {
		self.lexer.slice()
	}
	
	pub fn span(&self) -> Range<usize> {
		self.lexer.span()
	}
	
	pub fn peek(&mut self) -> Option<&Token> {
		if self.peek.is_none() {
			self.peek = self.lexer.next();
		}
		self.peek.as_ref()
	}
	
	pub fn next(&mut self) -> Option<Token> {
		let token = match self.peek {
			None => self.lexer.next(),
			Some(_) => self.peek.take(),
		};
		
		if token == Some(Token::Newline) {
			self.line += 1;
			self.col = 0;
		} else {
			self.col += 1;
		}
		
		token
	}
	
	pub fn expect(&mut self, tok: Token) -> Option<Token> {
		if self.peek() == Some(&tok) {
			self.next()
		} else {
			None
		}
	}
	
	pub fn skip_newlines(&mut self) {
		while self.expect(Token::Newline).is_some() {
			self.line += 1;
			self.col = 0;
		}
	}
}

#[derive(Debug, Clone)]
pub enum ParseError {
	UnexpectedEof,
}

pub type ParseResult<T> = Result<T, ParseError>;
