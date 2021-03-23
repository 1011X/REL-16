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
	
	#[token("br")]  Branch,
	#[token("bro")] BranchOdd,
	#[token("bre")] BranchEven,
	#[token("brn")] BranchNeg,
	#[token("brp")] BranchPos,
	
	// syntax
	#[token(",")]  Comma,
	#[token("\n")] Newline,
	
	#[regex(";.*", logos::skip)]
	Comment,
	
	// literals
	#[regex("ra|sp|[st][012]|x[0-7]")]
	#[regex("\\$[A-Za-z0-9_]+")]
	Register,
	
	#[regex("@[A-Za-z0-9_]+")]
	Label,
	
	#[regex("%[A-Za-z_][A-Za-z0-9_]*")]
	Directive,
	
	#[regex("[0-9][0-9']*")]
	#[regex("0b[01][01']*")]
	#[regex("0x[0-9A-Fa-f][0-9A-Fa-f']*")]
	Number,
	
	#[regex("[ \t\r]+", logos::skip)]
	#[error]
	Error,
}

pub type Lexer<'src> = logos::Lexer<'src, Token>;
