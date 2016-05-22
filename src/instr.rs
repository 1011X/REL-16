use std::fmt;
use std::str;
use std::error::Error;
use std::num;

pub type Reg = usize; // always in range [0-7]

#[derive(Debug)]
pub enum DeserialError {
	SameRegister,
	MissingArg,
	UnknownMneumonic,
	ValueTooLarge,
	ExpectedRegister,
	Parsing(num::ParseIntError),
	Other(Box<DeserialError>),
}

impl fmt::Display for DeserialError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.description()) // Should I do this?
	}
}

impl Error for DeserialError {
	fn description(&self) -> &str {
		match *self {
			DeserialError::SameRegister =>
				"same argument used in both a controlled and mutable context",
			DeserialError::MissingArg =>
				"missing argument for instruction",
			DeserialError::UnknownMneumonic =>
				"unknown opcode mneumonic",
			DeserialError::ValueTooLarge =>
				"value for argument is too big",
			DeserialError::ExpectedRegister =>
				"expected register literal",
			DeserialError::Parsing(ref e) =>
				e.description(),
			DeserialError::Other(ref e) =>
				e.description(),
		}
	}
	
	fn cause(&self) -> Option<&Error> {
		match *self {
			DeserialError::Parsing(ref e) => Some(e),
			DeserialError::Other(ref e) => Some(e),
			_ => None,
		}
	}
}


#[derive(Debug, PartialEq, Eq)]
pub enum DecodeError {
	Unused,
	Invalid,
}

impl fmt::Display for DecodeError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.description()) // Should I do this?
	}
}

impl Error for DecodeError {
	fn description(&self) -> &str {
		match *self {
			DecodeError::Unused =>
				"unused instruction value",
			DecodeError::Invalid =>
				"invalid instruction",
		}
	}
}

/// Types of instructions:
///
/// * *Pair*: Has 1 register and an 8-bit value.
///   `1ooo orrr vvvv vvvv` where
///   `o`es represent the operation field
///   `r`es represent the register used
///   `v`es represent the value used
///
/// * *Data*: Has 12-bit value.
///   `01oo vvvv vvvv vvvv` where
///   `o`es represent the operation field
///   `v`es represent the offset value used
///
/// * *Triple*: Has 3 registers.
///   `0000 1ooa aabb bccc` where
///   `o`es represent the operation field
///   `a`es represent the first register
///   `b`es represent the second register
///   `c`es represent the third register
/// 
/// * *Double*: Has 2 registers.
///   `0000 01oo ooaa abbb` where
///   `o`es represent the operation field
///   `a`es represent the first register
///   `b`es represent the second register
///
/// * *Single*: Has 1 register.
///   `0000 001o oooo orrr` where
///   `o`es represent the operation field
///   `r`es represent the register used
/// 
/// * *Signal*: Tells the machine to do something. Has a 4-bit value.
///   `0000 0000 00ss ssss` where
///   `s`es represent the signal field
/// 
/// * *Reserved*: Unused instructions.
///   `001x xxxx xxxx xxxx`
///   `0001 xxxx xxxx xxxx`
///   `0000 0001 xxxx xxxx`
///   `0000 0000 1xxx xxxx`
///   `0000 0000 01xx xxxx`

pub enum Op {
	Halt,
	Not(Reg),
	RotateLeft(Reg),
	RotateRight(Reg),
	Increment(Reg),
	Decrement(Reg),
	Push(Reg),
	Pop(Reg),
	//Read(Reg),
	//Write(Reg),
	Swap(Reg, Reg),
	CNot(Reg, Reg),
	CAdd(Reg, Reg),
	CSub(Reg, Reg),
	Exchange(Reg, Reg),
	Immediate(Reg, u8),
	CCNot(Reg, Reg, Reg),
	CSwap(Reg, Reg, Reg),
	GoTo(u16),
	ComeFrom(u16),
	/*
	BranchGEZ(Reg, u8),
	BranchLZ(Reg, u8),
	*/
	SwapBr(Reg),
	RevSwapBr(Reg),
}

impl Op {
	
	// To guarantee VM is reversible, every operation must have an inverse.
	// Involutory instructions (instructions that are their own inverse) return self.
	pub fn invert(self) -> Op {
		match self {
			Op::Halt           => self,
			Op::Not(..)        => self,
			Op::RotateLeft(r)  => Op::RotateRight(r),
			Op::RotateRight(r) => Op::RotateLeft(r),
			Op::Increment(r)   => Op::Decrement(r),
			Op::Decrement(r)   => Op::Increment(r),
			Op::Push(r)        => Op::Pop(r),
			Op::Pop(r)         => Op::Push(r),
			Op::Swap(..)       => self,
			Op::CNot(..)       => self,
			Op::CAdd(rc, ra)   => Op::CSub(rc, ra),
			Op::CSub(rc, rs)   => Op::CAdd(rc, rs),
			Op::Exchange(..)   => self,
			Op::Immediate(..)  => self,
			Op::CCNot(..)      => self,
			Op::CSwap(..)      => self,
			
			// control flow
			Op::GoTo(off)        => Op::ComeFrom(off),
			Op::ComeFrom(off)    => Op::GoTo(off),
			/*
			Op::BranchLZ(reg, off) => 
			Op::BranchGEZ(reg, off) => 
			*/
			Op::SwapBr(reg) => Op::RevSwapBr(reg),
			Op::RevSwapBr(reg) => Op::SwapBr(reg),
		}
	}

	pub fn encode(&self) -> u16 {
		// TODO: decide if I want to have assertions for values.
		match *self {
			Op::Halt => 0,
		
			Op::Not(reg) => 0b_0000001_000000_000
				| reg as u16,
		
			Op::RotateLeft(reg) => 0b_0000001_000001_000
				| reg as u16,
		
			Op::RotateRight(reg) => 0b_0000001_000010_000
				| reg as u16,
		
			Op::Increment(reg) => 0b_0000001_000011_000
				| reg as u16,
		
			Op::Decrement(reg) => 0b_0000001_000100_000
				| reg as u16,
		
			Op::Push(reg) => 0b_0000001_000101_000
				| reg as u16,
		
			Op::Pop(reg) => 0b_0000001_000110_000
				| reg as u16,
		
			Op::Swap(rl, rr) => 0b_000001_0000_000_000
				| (rl as u16) << 3
				| rr as u16,
		
			Op::CNot(rc, rn) => 0b_000001_0001_000_000
				| (rc as u16) << 3
				| rn as u16,
		
			Op::CAdd(rc, ra) => 0b_000001_0010_000_000
				| (rc as u16) << 3
				| ra as u16,
		
			Op::CSub(rc, rs) => 0b_000001_0011_000_000
				| (rc as u16) << 3
				| rs as u16,
		
			Op::Exchange(r, ra) => 0b_000001_0100_000_000
				| (r as u16) << 3
				| ra as u16,
		
			Op::CCNot(rc0, rc1, rn) => 0b_00001_00_000_000_000
				| (rc0 as u16) << 6
				| (rc1 as u16) << 3
				| rn as u16,
		
			Op::CSwap(rc, rs0, rs1) => 0b_00001_01_000_000_000
				| (rc as u16) << 6
				| (rs0 as u16) << 3
				| rs1 as u16,
		
			Op::Immediate(reg, val) => 0b_1_0000_000_00000000
				| (reg as u16) << 8
				| val as u16,
		
			// control flow
		
			Op::GoTo(off) => 0b_01_00_000000000000
				| off as u16,
		
			Op::ComeFrom(off) => 0b_01_01_000000000000
				| off as u16,
		
			/*
			Op::BranchLZ(reg, off) => 0b_1_0001_000_00000000
				| (reg as u16) << 8
				| off as u16,
		
			Op::BranchGEZ(reg, off) => 0b_1_0010_000_00000000
				| (reg as u16) << 8
				| off as u16,
			*/
		
			Op::SwapBr(reg) => 0b_0000001_000111_000
				| reg as u16,
		
			Op::RevSwapBr(reg) => 0b_0000001_001000_000
				| reg as u16,
		}
	}
	
	pub fn decode(instr: u16) -> Result<Op, DecodeError> {
	
		match instr.leading_zeros() {
			// pair type
			0 => {
				let o = (instr >> 3 + 8) & 0b_1111;
				let r = (instr >> 8) & 0b_111;
				let v = instr & 0b_1111_1111;
			
				match o {
					0b_0000 => Ok(Op::Immediate(r as usize, v as u8)),
				
					o if o <= 0b_1111 => Err(DecodeError::Invalid),
				
					_ => unreachable!(),
				}
			}
		
			// data type
			1 => {
				let o = (instr >> 12) & 0b_11;
				let v = instr & 0b_1111_1111_1111;
			
				match o {
					0b_00 => Ok(Op::GoTo(v as u16)),
					0b_01 => Ok(Op::ComeFrom(v as u16)),
				
					o if o <= 0b_11 => Err(DecodeError::Invalid),
				
					_ => unreachable!(),
				}
			}
		
			// unused
			2...3 => Err(DecodeError::Unused),
		
			// triple type
			4 => {
				let o  = (instr >> 3 + 3 + 3) & 0b_11;
				let ra = (instr >> 3 + 3) & 0b_111;
				let rb = (instr >> 3) & 0b_111;
				let rc = instr & 0b_111;
			
				match o {
					0b_00 => Ok(Op::CCNot(ra as usize, rb as usize, rc as usize)),
					0b_01 => Ok(Op::CSwap(ra as usize, rb as usize, rc as usize)),
				
					o if o <= 0b_11 => Err(DecodeError::Invalid),
				
					_ => unreachable!(),
				}
			}
		
			// double type
			5 => {
				let o  = (instr >> 3 + 3) & 0b_1111;
				let ra = (instr >> 3) & 0b_111;
				let rb = instr & 0b_111;
			
				match o {
					0b_0000 => Ok(Op::Swap(ra as usize, rb as usize)),
					0b_0001 => Ok(Op::CNot(ra as usize, rb as usize)),
					0b_0010 => Ok(Op::CAdd(ra as usize, rb as usize)),
					0b_0011 => Ok(Op::CSub(ra as usize, rb as usize)),
					0b_0100 => Ok(Op::Exchange(ra as usize, rb as usize)),
				
					o if o <= 0b_1111 => Err(DecodeError::Invalid),
				
					_ => unreachable!(),
				}
			}
		
			// single type
			6 => {
				let o = (instr >> 3) & 0b_111111;
				let r = instr & 0b_111;
			
				match o {
					0b_000000 => Ok(Op::Not(r as usize)),
			
					0b_000001 => Ok(Op::RotateLeft(r as usize)),
					0b_000010 => Ok(Op::RotateRight(r as usize)),
			
					0b_000011 => Ok(Op::Increment(r as usize)),
					0b_000100 => Ok(Op::Decrement(r as usize)),
	
					0b_000101 => Ok(Op::Push(r as usize)),
					0b_000110 => Ok(Op::Pop(r as usize)),
					
					0b_000111 => Ok(Op::SwapBr(r as usize)),
					0b_001000 => Ok(Op::RevSwapBr(r as usize)),
				
					o if o <= 0b_111111 => Err(DecodeError::Invalid),
				
					_ => unreachable!(),
				}
			}
		
			// reserved
			7...9 => Err(DecodeError::Unused),
		
			// signal type
			10...16 => match instr {
				0b_000000 => Ok(Op::Halt),
			
				s if s <= 0b_111111 => Err(DecodeError::Invalid),
			
				_ => unreachable!(),
			},
		
			_ => unreachable!(),
		}
	}
}

impl fmt::Display for Op {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Op::Halt                => write!(f, "hlt"),
			Op::Not(r)              => write!(f, "not r{}", r),
			Op::RotateLeft(r)       => write!(f, "rol r{}", r),
			Op::RotateRight(r)      => write!(f, "ror r{}", r),
			Op::Increment(r)        => write!(f, "inc r{}", r),
			Op::Decrement(r)        => write!(f, "dec r{}", r),
			Op::Push(r)             => write!(f, "push r{}", r),
			Op::Pop(r)              => write!(f, "pop r{}", r),
			Op::Swap(rl, rr)        => write!(f, "swp r{} r{}", rl, rr),
			Op::CNot(rc, rn)        => write!(f, "cnot r{} r{}", rc, rn),
			Op::CAdd(rc, ra)        => write!(f, "add r{} r{}", rc, ra),
			Op::CSub(rc, rs)        => write!(f, "sub r{} r{}", rc, rs),
			Op::Exchange(rr, ra)    => write!(f, "xchg r{} r{}", rr, ra),
			Op::Immediate(r, v)     => write!(f, "imm r{} {}", r, v),
			Op::CCNot(rc0, rc1, rn) => write!(f, "ccn r{} r{} r{}", rc0, rc1, rn),
			Op::CSwap(rc, rs0, rs1) => write!(f, "cswp r{} r{} r{}", rc, rs0, rs1),
			Op::GoTo(off)           => write!(f, "goto {}", off),
			Op::ComeFrom(off)       => write!(f, "cmfr {}", off),
			Op::SwapBr(r)           => write!(f, "swb r{}", r),
			Op::RevSwapBr(r)        => write!(f, "rswb r{}", r),
		}
	}
}

impl str::FromStr for Op {
	type Err = DeserialError;
	
	fn from_str(s: &str) -> Result<Self, Self::Err> {
		
		// Must be `fn` because it's used by `parse_reglit`, which
		// is also a(n) `fn`.
		fn parse_byte(s: &str) -> Result<u8, DeserialError> {
			s.parse()
				.map_err(DeserialError::Parsing)
		}
		
		// Can't be a closure because `get_register` uses it.
		fn parse_reglit(s: &str) -> Result<usize, DeserialError> {
			if s.starts_with('r') {
				match parse_byte(&s[1..]) {
					Ok(byte) if byte < 8 =>
						Ok(byte as usize),
			
					Ok(_) =>
						Err(DeserialError::ValueTooLarge),
			
					Err(e) =>
						Err(DeserialError::Other(Box::new(e))),
				}
			}
			else {
				Err(DeserialError::ExpectedRegister)
			}
		}
		
		let mut tokens = s.split_whitespace();
		
		// Can't just include `tokens.next()` here because
		// then there would be a mutable reference to `tokens`
		// in `get_register()` *and* in match block below that
		// determines instruction used.
		let get_register = |token: Option<&str>| token
			.ok_or(DeserialError::MissingArg)
			.and_then(parse_reglit);
		
		// we can unwrap once because line should not be empty
		match tokens.next().unwrap() {
			"hlt" => Ok(Op::Halt),
			
			"not" => get_register(tokens.next())
				.map(Op::Not),
			
			"rol" => get_register(tokens.next())
				.map(Op::RotateLeft),
			
			"ror" => get_register(tokens.next())
				.map(Op::RotateRight),
			
			"inc" => get_register(tokens.next())
				.map(Op::Increment),
			
			"dec" => get_register(tokens.next())
				.map(Op::Decrement),
			
			"push" => get_register(tokens.next())
				.map(Op::Push),
			
			"pop" => get_register(tokens.next())
				.map(Op::Pop),
			
			"swp" => {
				let regl = get_register(tokens.next());
				let regr = get_register(tokens.next());
				
				match (regl, regr) {
					(Ok(regl), Ok(regr)) =>
						Ok(Op::Swap(regl, regr)),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(DeserialError::Other(Box::new(e))),
				}
			}
			
			"xor" => {
				let rn = get_register(tokens.next());
				let rc = get_register(tokens.next());
				
				match (rc, rn) {
					(Ok(rc), Ok(rn)) if rn != rc =>
						Ok(Op::CNot(rc, rn)),
					
					(Ok(_), Ok(_)) =>
						Err(DeserialError::SameRegister),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(DeserialError::Other(Box::new(e))),
				}
			}
			
			"add" => {
				let ra = get_register(tokens.next());
				let rc = get_register(tokens.next());
				
				match (rc, ra) {
					(Ok(rc), Ok(ra)) if ra != rc =>
						Ok(Op::CAdd(rc, ra)),
					
					(Ok(_), Ok(_)) =>
						Err(DeserialError::SameRegister),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(DeserialError::Other(Box::new(e))),
				}
			}
			
			"sub" => {
				let rs = get_register(tokens.next());
				let rc = get_register(tokens.next());
				
				match (rc, rs) {
					(Ok(rc), Ok(rs)) if rs != rc =>
						Ok(Op::CSub(rc, rs)),
					
					(Ok(_), Ok(_)) =>
						Err(DeserialError::SameRegister),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(DeserialError::Other(Box::new(e))),
				}
			}
			
			"imm" => {
				let reg = get_register(tokens.next());
				
				let value = tokens.next()
					.ok_or(DeserialError::MissingArg)
					.and_then(parse_byte);
				
				match (reg, value) {
					(Ok(reg), Ok(value)) =>
						Ok(Op::Immediate(reg, value)),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(DeserialError::Other(Box::new(e)))
				}
			}
			
			"xchg" => {
				let reg = get_register(tokens.next());
				let raddr = get_register(tokens.next());
				
				match (reg, raddr) {
					(Ok(reg), Ok(raddr)) =>
						Ok(Op::Exchange(reg, raddr)),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(DeserialError::Other(Box::new(e))),
				}
			}
			
			"ccn" => {
				let regc = get_register(tokens.next());
				let rega = get_register(tokens.next());
				let regb = get_register(tokens.next());
				
				match (rega, regb, regc) {
					(Ok(a), Ok(b), Ok(c)) if c != a && c != b =>
						Ok(Op::CCNot(a, b, c)),
					
					(Ok(_), Ok(_), Ok(_)) =>
						Err(DeserialError::SameRegister),
					
					(Err(e), _, _) | (_, Err(e), _) | (_, _, Err(e)) =>
						Err(DeserialError::Other(Box::new(e))),
				}
			}
			
			"cswp" => {
				let rega = get_register(tokens.next());
				let regb = get_register(tokens.next());
				let regc = get_register(tokens.next());
				
				match (rega, regb, regc) {
					(Ok(a), Ok(b), Ok(c)) if b != a && c != a =>
						Ok(Op::CSwap(a, b, c)),
					
					(Ok(_), Ok(_), Ok(_)) =>
						Err(DeserialError::SameRegister),
					
					(Err(e), _, _) | (_, Err(e), _) | (_, _, Err(e)) =>
						Err(DeserialError::Other(Box::new(e))),
				}
			}
			
			"jmp" => tokens.next()
				.ok_or(DeserialError::MissingArg)
				.and_then(|s| match s.parse::<u16>() {
					Ok(v) if v < 0x1000 => Ok(v),
					Ok(_) => Err(DeserialError::ValueTooLarge),
					Err(e) => Err(DeserialError::Parsing(e)),
				})
				.map(Op::GoTo),
			
			"pmj" => tokens.next()
				.ok_or(DeserialError::MissingArg)
				.and_then(|s| match s.parse::<u16>() {
					Ok(v) if v < 0x1000 => Ok(v),
					Ok(_) => Err(DeserialError::ValueTooLarge),
					Err(e) => Err(DeserialError::Parsing(e)),
				})
				.map(Op::ComeFrom),
			/*
			"bltz" => {
				let reg = get_register(tokens.next());
				
				let off = tokens.next()
					.ok_or(DeserialError::MissingArg)
					.and_then(parse_byte);
				
				match (reg, off) {
					(Ok(reg), Ok(off)) =>
						Ok(Op::BrLZ(reg, off)),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(DeserialError::Other(Box::new(e))),
				}
			}
			
			"bodd" => {
				let reg = get_register(tokens.next());
				
				let off = tokens.next()
					.ok_or(DeserialError::MissingArg)
					.and_then(parse_byte);
				
				match (reg, off) {
					(Ok(reg), Ok(off)) =>
						Ok(Op::BrGEZ(reg, off)),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(DeserialError::Other(Box::new(e))),
				}
			}
			*/
			
			"swb" => get_register(tokens.next())
				.map(Op::SwapBr),
			
			"rswb" => get_register(tokens.next())
				.map(Op::RevSwapBr),
			
			_ => Err(DeserialError::UnknownMneumonic),
		}
	}
}
