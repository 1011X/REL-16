#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
pub enum Token {
	// instructions
	#[token("halt")] Halt,
	#[token("dbg")]  Debug,
	
	#[token("xor")] Xor,
	#[token("add")] Add,
	#[token("sub")] Sub,
	#[token("rol")] Rol,
	#[token("ror")] Ror,
	
	#[token("xchg")] Exchange,
	
	#[token("spc")]   SwapPC,
	#[token("spc.r")] SwapPCRev,
	
	#[token("br")]      Branch,
	#[token("br.odd")]  BranchOdd,
	#[token("br.even")] BranchEven,
	#[token("br.neg")]  BranchNeg,
	#[token("br.pos")]  BranchPos,
	
	// syntax
	#[token(",")]  Comma,
	#[token("\n")] Newline,
	
	#[regex("#.*", logos::skip)]
	Comment,
	
	// literals
	#[regex("ra|sp|[st][012]|x[0-7]|\\$[A-Za-z0-9_]+")]
	Register,
	
	#[regex("@[A-Za-z0-9_]+")]
	Label,
	
	#[regex(":[A-Za-z_][A-Za-z0-9_]*")]
	Directive,
	
	#[regex("0b[01]+|0x[0-9A-Fa-f]+|[1-9][0-9]*")]
	Number,
	
	#[regex("[ \t]+", logos::skip)]
	#[error]
	Error,
}

pub type Lexer<'src> = logos::Lexer<'src, Token>;
