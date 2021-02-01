pub enum Literal {
	Number(u16),
	Label(String),
	Register(Reg),
}

impl Parser<'_> {
	pub fn parse_literal(&mut self) -> ParseResult<Literal> {
		match self.next() {
			None => Err(ParseError::UnexpectedEof),
			Some(Number) => todo!(),
			Some(Label) => todo!(),
			Some(Register) => todo!(),
		}
	}
}
