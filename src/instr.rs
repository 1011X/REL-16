use std::fmt;
use std::str;
use std::error::Error;
use std::num;

pub type Reg = usize; // always in range [0-7]

#[derive(Debug)]
pub enum DeserialError {
	MissingMneumonic,
	MissingArg,
	UnknownMneumonic,
	ValueTooLarge,
	ExpectedRegister,
	Parsing(num::ParseIntError),
}

impl fmt::Display for DeserialError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.description()) // Should I do this?
	}
}

impl Error for DeserialError {
	fn description(&self) -> &str {
		match *self {
			DeserialError::MissingMneumonic =>
				"missing mneumonic",
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
		}
	}
	
	fn cause(&self) -> Option<&Error> {
		match *self {
			DeserialError::Parsing(ref e) => Some(e),
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
	/// Stops the VM.
	Halt,
	/// Flips every bit in the given register.
	Not(Reg),
	/// Rotates value left by 1.
	RotateLeft(Reg),
	/// Rotates value right by 1.
	RotateRight(Reg),
	/// Adds 1 to the current value.
	Increment(Reg),
	/// Subtracts 1 from the current value.
	Decrement(Reg),
	/// Swaps value in given register with the value pointed to
	/// by the stack pointer (`r6`). Register will be zero.
	Push(Reg),
	/// Swaps value pointed by stack pointer (`r6`) with value
	/// in register. Register value should be zero before this
	/// operation is performed.
	Pop(Reg),
	//Read(Reg),
	//Write(Reg),
	/// Swaps value in given registers.
	Swap(Reg, Reg),
	/// Xors first register with value in second register.
	CNot(Reg, Reg),
	/// Adds first register with value in second register.
	CAdd(Reg, Reg),
	/// Subtracts first register with value in second register.
	CSub(Reg, Reg),
	/// Swaps value pointed to by the second register with value
	/// in the first register.
	Exchange(Reg, Reg),
	/// Xors an immediate value with the value in the current
	/// register. Usually the register will be zero.
	Immediate(Reg, u8),
	/// Toffoli gate. Ands first and second registers and flips
	/// bits in third register based on the result.
	CCNot(Reg, Reg, Reg),
	/// Fredkin gate. Swaps bits in second and third registers
	/// based on bits in first register.
	CSwap(Reg, Reg, Reg),
	/// Increments `br` register by the given value.
	GoTo(u16),
	/// Decrements `br` register by the given value.
	ComeFrom(u16),
	//BranchOdd(Reg, u8),
	//BranchSign(Reg, u8),
	/// Swaps values in `br` and given register.
	SwapBr(Reg),
	/// Flips `dir` bit (putting machine in reverse mode) and
	/// swaps values in `br` and given register.
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
			Op::GoTo(off)      => Op::ComeFrom(off),
			Op::ComeFrom(off)  => Op::GoTo(off),
			//Op::BranchLZ(reg, off) => 
			//Op::BranchGEZ(reg, off) => 
			Op::SwapBr(reg)    => Op::RevSwapBr(reg),
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
			Op::BranchSign(reg, off) => 0b_1_0001_000_00000000
				| (reg as u16) << 8
				| off as u16,
		
			Op::BranchOdd(reg, off) => 0b_1_0010_000_00000000
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
			Op::CNot(rc, rn)        => write!(f, "xor r{} r{}", rn, rc),
			Op::CAdd(rc, ra)        => write!(f, "add r{} r{}", ra, rc),
			Op::CSub(rc, rs)        => write!(f, "sub r{} r{}", rs, rc),
			Op::Exchange(rr, ra)    => write!(f, "xchg r{} r{}", rr, ra),
			Op::Immediate(r, v)     => write!(f, "imm r{} {}", r, v),
			Op::CCNot(rc0, rc1, rn) => write!(f, "ccn r{} r{} r{}", rc0, rc1, rn),
			Op::CSwap(rc, rs0, rs1) => write!(f, "cswp r{} r{} r{}", rc, rs0, rs1),
			Op::GoTo(off)           => write!(f, "jmp {}", off),
			Op::ComeFrom(off)       => write!(f, "pmj {}", off),
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
						Err(e),
				}
			}
			else {
				Err(DeserialError::ExpectedRegister)
			}
		}
		
		let mut tokens = s.split_whitespace();
		
		// Can't include `tokens.next()` here because then
		// there would be a mutable reference to `tokens` in
		// both `get_register()` and in match block below.
		let get_register = |token: Option<&str>| token
			.ok_or(DeserialError::MissingArg)
			.and_then(parse_reglit);
		
		match try!(tokens.next().ok_or(DeserialError::MissingMneumonic)) {
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
				let regl = try!(get_register(tokens.next()));
				let regr = try!(get_register(tokens.next()));
				
				Ok(Op::Swap(regl, regr))
			}
			
			"xor" => {
				let rn = try!(get_register(tokens.next()));
				let rc = try!(get_register(tokens.next()));
				
				Ok(Op::CNot(rc, rn))
			}
			
			"add" => {
				let ra = try!(get_register(tokens.next()));
				let rc = try!(get_register(tokens.next()));
				
				Ok(Op::CAdd(rc, ra))
			}
			
			"sub" => {
				let rs = try!(get_register(tokens.next()));
				let rc = try!(get_register(tokens.next()));
				
				Ok(Op::CSub(rc, rs))
			}
			
			"imm" => {
				let reg = try!(get_register(tokens.next()));
				
				let value = try!(tokens.next()
					.ok_or(DeserialError::MissingArg)
					.and_then(parse_byte)
				);
				
				Ok(Op::Immediate(reg, value))
			}
			
			"xchg" => {
				let reg   = try!(get_register(tokens.next()));
				let raddr = try!(get_register(tokens.next()));
				
				Ok(Op::Exchange(reg, raddr))
			}
			
			"ccn" => {
				let rc0 = try!(get_register(tokens.next()));
				let rc1 = try!(get_register(tokens.next()));
				let rn  = try!(get_register(tokens.next()));
				
				Ok(Op::CCNot(rc0, rc1, rn))
			}
			
			"cswp" => {
				let rc = try!(get_register(tokens.next()));
				let rs0 = try!(get_register(tokens.next()));
				let rs1 = try!(get_register(tokens.next()));
				
				Ok(Op::CSwap(rc, rs0, rs1))
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
				let reg = try!(get_register(tokens.next()));
				
				let off = try!(tokens.next()
					.ok_or(DeserialError::MissingArg)
					.and_then(parse_byte)
				);
				
				Ok(Op::BrLZ(reg, off))
			}
			
			"bodd" => {
				let reg = try!(get_register(tokens.next()));
				
				let off = try!(tokens.next()
					.ok_or(DeserialError::MissingArg)
					.and_then(parse_byte)
				);
				
				Ok(Op::BrGEZ(reg, off)),
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
