pub enum Line {
	Directive(String, Vec<>),
	Instruction(Instruction),
}

impl Parser<'_> {
	pub fn parse_line(&mut self) {
		
	}
}
