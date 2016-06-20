use std::fmt;
use std::str;
use std::error::Error;
use std::num;

pub type Reg = usize; // always in range [0-7]

#[derive(Debug)]
pub enum DeserialError {
	NoMneumonic,
	NoArgument,
	UnknownMneu,
	ValueTooLarge,
	NoRegister,
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
			DeserialError::NoMneumonic    => "missing mneumonic",
			DeserialError::NoArgument     => "missing argument",
			DeserialError::UnknownMneu    => "unknown mneumonic",
			DeserialError::ValueTooLarge  => "argument value is too big",
			DeserialError::NoRegister     => "expected register literal",
			DeserialError::Parsing(ref e) => e.description(),
		}
	}
	
	fn cause(&self) -> Option<&Error> {
		match *self {
			DeserialError::Parsing(ref e) => Some(e),
			_ => None,
		}
	}
}


#[derive(Debug)]
pub struct InvalidInstr;

impl fmt::Display for InvalidInstr {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "Invalid instruction found while parsing.");
	}
}

impl Error for InvalidInstr {
	fn description(&self) -> &str { "invalid instruction" }
}

/// Bit-field key:
/// * `_`: leading zero
/// * `0`/`1`: bit literal
/// * `o`: opcode field
/// * `r`/`R`: register field
/// * `v`: address field
/// 
/// Types of instructions (**subject to change**):
/// * Signal: `_______________o`
/// * Single register: `_________1ooorrr`
/// * Single register, 4-bit immediate: `_______1orrrvvvv`
/// * Double register: `______1oooRRRrrr`
/// * Triple register: `_____1orrrRRRrrr`
/// * Immediate: `____1rrrvvvvvvvv`
/// * Branches and assertions: `_1ooorrrvvvvvvvv`
/// * Jumps: `1ovvvvvvvvvvvvvv`
/// 
/// Invalid values:
/// * `__1xxxxxxxxxxxxx`
/// * `___1xxxxxxxxxxxx`
/// * `________1xxxxxxx`
/// * `__________1xxxxx`
/// * `___________1xxxx`
/// * `____________1xxx`
/// * `_____________1xx`
/// * `______________1x`

pub enum Op {
	/// Stops the machine.
	Halt,
	
	/// Flips direction bit.
	/// 
	/// This will reverse the VM until it reverses again with the same
	/// instruction, or halts.
	Reverse,
	
	/// Flips every bit in the register.
	Not(Reg),
	
	/// Adds 1 to the register's value.
	Increment(Reg),
	
	/// Subtracts 1 from the register's value.
	Decrement(Reg),
	
	/// Increments the stack pointer, then swaps the register's value with the
	/// value at the stack pointer.
	/// 
	/// The register's new value should be zero, assuming no values were leaked
	/// into the memory.
	Push(Reg),
	
	/// Swaps the register's value with the value at the stack pointer, then
	/// decrements the stack pointer.
	/// 
	/// Register value should be zero before this operation is performed, so
	/// that the memory stays cleared.
	Pop(Reg),
	
	/// Swaps the register and the program counter.
	SwapPc(Reg),
	
	/// Flips direction bit, then swaps the register and the program counter.
	RevSwapPc(Reg),
	
	/// Rotates the register's bits to the left by the given amount.
	RotLeftImm(Reg, u8),
	
	/// Rotates the register's bits to the right by the given amount.
	RotRightImm(Reg, u8),
	
	/// Swaps the registers' values.
	Swap(Reg, Reg),
	
	/// Flips bits in the first register based on bits in the second register.
	/// 
	/// Same as x86's `xor` instruction.
	CNot(Reg, Reg),
	
	/// Adds first register with value in second register.
	CAdd(Reg, Reg),
	
	/// Subtracts first register with value in second register.
	CSub(Reg, Reg),
	
	/// Swaps the first register's value with the value referenced by the
	/// second register.
	Exchange(Reg, Reg),
	
	/// Rotates the first register's bits to the left by the value in the
	/// second register.
	RotLeft(Reg, Reg),
	
	/// Rotates the first register's bits to the right by the value in the
	/// second register.
	RotRight(Reg, Reg),
	
	/// Toffoli gate; ANDs first and second registers and flips the bits in the
	/// third register based on the result.
	/// 
	/// To do this, the values are swapped with internal registers, whose
	/// initial values are zero. After the operation is done, the values are
	/// swapped back accordingly.
	CCNot(Reg, Reg, Reg),
	
	/// Fredkin gate; swaps bits in second and third registers based on bits in
	/// the first register.
	/// 
	/// To do this, the values are swapped with internal registers, whose
	/// initial values are zero. After the operation is done, the values are
	/// swapped back accordingly.
	CSwap(Reg, Reg, Reg),
	
	/// Flips bits in the register's lower half based on the bits of the
	/// immediate byte value.
	/// 
	/// This is usually used to set the register to the given value or to zero
	/// it out.
	Immediate(Reg, u8),
	
	/// Adds an immediate byte value to the branch register if the given
	/// register is odd.
	BranchOdd(Reg, u8),
	
	/// Subtracts an immediate byte value from the branch register if the given
	/// register is even.
	AssertEven(Reg, u8),
	
	/// Adds an immediate byte value to the branch register if the given
	/// register is even.
	BranchEven(Reg, u8),
	
	/// Subtracts an immediate byte value from the branch register if the given
	/// register is odd.
	AssertOdd(Reg, u8),
	
	/// Adds an immediate byte value to the branch register if the given
	/// register is below zero.
	BranchNeg(Reg, u8),
	
	/// Subtracts an immediate byte value from the branch register if the given
	/// register is zero or above.
	AssertNonneg(Reg, u8),
	
	/// Adds an immediate byte value to the branch register if the given
	/// register is zero or above.
	BranchNonneg(Reg, u8),
	
	/// Subtracts an immediate byte value from the branch register if the given
	/// register is below zero.
	AssertNeg(Reg, u8),
	
	/// Adds a value to the branch register.
	GoTo(u16),
	
	/// Subtracts a value from the branch register.
	ComeFrom(u16),
}

impl Op {
	// To guarantee VM is reversible, every operation must have an inverse.
	// Involutory instructions (which are their own inverse) return self.
	pub fn invert(self) -> Op {
		match self {
			Op::Halt               => self,
			Op::Reverse            => self,
			Op::Not(..)            => self,
			Op::Increment(r)       => Op::Decrement(r),
			Op::Decrement(r)       => Op::Increment(r),
			Op::Push(r)            => Op::Pop(r),
			Op::Pop(r)             => Op::Push(r),
			Op::SwapPc(r)          => Op::RevSwapPc(r),
			Op::RevSwapPc(r)       => Op::SwapPc(r),
			Op::RotLeftImm(r, v)   => Op::RotRightImm(r, v),
			Op::RotRightImm(r, v)  => Op::RotLeftImm(r, v),
			Op::Swap(..)           => self,
			Op::CNot(..)           => self,
			Op::CAdd(rc, ra)       => Op::CSub(rc, ra),
			Op::CSub(rc, rs)       => Op::CAdd(rc, rs),
			Op::Exchange(..)       => self,
			Op::RotLeft(rr, rv)    => Op::RotRight(rr, rv),
			Op::RotRight(rr, rv)   => Op::RotLeft(rr, rv),
			Op::CCNot(..)          => self,
			Op::CSwap(..)          => self,
			Op::Immediate(..)      => self,
			Op::BranchOdd(r, o)    => Op::AssertOdd(r, o),
			Op::BranchEven(r, o)   => Op::AssertEven(r, o),
			Op::BranchNeg(r, o)    => Op::AssertNeg(r, o),
			Op::BranchNonneg(r, o) => Op::AssertNonneg(r, o),
			Op::AssertOdd(r, o)    => Op::BranchOdd(r, o),
			Op::AssertEven(r, o)   => Op::BranchEven(r, o),
			Op::AssertNeg(r, o)    => Op::BranchNeg(r, o),
			Op::AssertNonneg(r, o) => Op::BranchNonneg(r, o),
			Op::GoTo(off)          => Op::ComeFrom(off),
			Op::ComeFrom(off)      => Op::GoTo(off),
		}
	}
	
	pub fn encode(&self) -> u16 {
		// All values in the enum should be explicitly casted, in case the
		// types change.
		match *self {
			// _______________s
			Op::Halt    => 0,
			Op::Reverse => 1,
			
			// _________1ooorrr
			Op::Not(reg)       => 0b1_000_000
				| reg as u16,
			
			Op::Increment(reg) => 0b1_001_000
				| reg as u16,
			
			Op::Decrement(reg) => 0b1_010_000
				| reg as u16,
			
			Op::Push(reg)      => 0b1_011_000
				| reg as u16,
			
			Op::Pop(reg)       => 0b1_100_000
				| reg as u16,
			
			Op::SwapPc(reg)    => 0b1_101_000
				| reg as u16,
			
			Op::RevSwapPc(reg) => 0b1_110_000
				| reg as u16,
			
			// _______1orrrvvvv
			Op::RotLeftImm(r, off)  => 0b1_0_000_0000
				| (r as u16) << 3
				| off as u16,
			
			Op::RotRightImm(r, off) => 0b1_1_000_0000
				| (r as u16) << 3
				| off as u16,
			
			// ______1oooRRRrrr
			Op::Swap(rl, rr)     => 0b1_000_000_000
				| (rl as u16) << 3
				| rr as u16,
			
			Op::CNot(rc, rn)     => 0b1_001_000_000
				| (rc as u16) << 3
				| rn as u16,
			
			Op::CAdd(rc, ra)     => 0b1_010_000_000
				| (rc as u16) << 3
				| ra as u16,
			
			Op::CSub(rc, rs)     => 0b1_011_000_000
				| (rc as u16) << 3
				| rs as u16,
			
			Op::Exchange(r, ra)  => 0b1_100_000_000
				| (r as u16) << 3
				| ra as u16,
			
			Op::RotLeft(rr, ro)  => 0b1_101_000_000
				| (rr as u16) << 3
				| ro as u16,
			
			Op::RotRight(rr, ro) => 0b1_110_000_000
				| (rr as u16) << 3
				| ro as u16,
			
			// _____1orrrRRRrrr
			Op::CCNot(rc0, rc1, rn) => 0b1_0_000_000_000
				| (rc0 as u16) << 6
				| (rc1 as u16) << 3
				| rn as u16,
			
			Op::CSwap(rc, rs0, rs1) => 0b1_1_000_000_000
				| (rc as u16) << 6
				| (rs0 as u16) << 3
				| rs1 as u16,
			
			// ____1rrrvvvvvvvv
			Op::Immediate(r, val) => 0b1_000_00000000
				| (r as u16) << 8
				| val as u16,
			
			// _1ooorrrvvvvvvvv
			Op::BranchOdd(r, off)    => 0b1_000_000_00000000
				| (r as u16) << 8
				| off as u16,
			
			Op::AssertEven(r, off)   => 0b1_001_000_00000000
				| (r as u16) << 8
				| off as u16,
			
			Op::BranchEven(r, off)   => 0b1_010_000_00000000
				| (r as u16) << 8
				| off as u16,
			
			Op::AssertOdd(r, off)    => 0b1_011_000_00000000
				| (r as u16) << 8
				| off as u16,
			
			Op::BranchNeg(r, off)    => 0b1_100_000_00000000
				| (r as u16) << 8
				| off as u16,
			
			Op::AssertNonneg(r, off) => 0b1_101_000_00000000
				| (r as u16) << 8
				| off as u16,
			
			Op::BranchNonneg(r, off) => 0b1_110_000_00000000
				| (r as u16) << 8
				| off as u16,
			
			Op::AssertNeg(r, off)    => 0b1_111_000_00000000
				| (r as u16) << 8
				| off as u16,
			
			// 1ovvvvvvvvvvvvvv
			Op::GoTo(off)     => 0b1_0_00000000000000
				| off as u16,
			
			Op::ComeFrom(off) => 0b1_1_00000000000000
				| off as u16,
		}
	}
	
	pub fn decode(instr: u16) -> Result<Op, InvalidInstr> {
	
		match instr.leading_zeros() {
			// 1ovvvvvvvvvvvvvv
			// Goto/ComeFrom
			0 => {
				let o = ((instr >> 14) & 0b_1) == 1;
				let v = instr & 0b_11_1111_1111_1111;
				
				if o {
					Ok(Op::ComeFrom(v as u16))
				}
				else {
					Ok(Op::GoTo(v as u16))
				}
			}
			
			// _1ooorrrvvvvvvvv
			// Assert*, Branch*
			1 => {
				let o = (instr >> 3 + 8) & 0b_111;
				let r = (instr >>     8) & 0b_111;
				let v =  instr           & 0b_1111_1111;
				
				Ok(match o {
					0b_000 => Op::BranchOdd(r as Reg, v as u8),
					0b_001 => Op::AssertEven(r as Reg, v as u8),
					0b_010 => Op::BranchEven(r as Reg, v as u8),
					0b_011 => Op::AssertOdd(r as Reg, v as u8),
					0b_100 => Op::BranchNeg(r as Reg, v as u8),
					0b_101 => Op::AssertNonneg(r as Reg, v as u8),
					0b_110 => Op::BranchNonneg(r as Reg, v as u8),
					0b_111 => Op::AssertNeg(r as Reg, v as u8),
					_ => unreachable!()
				})
			}
			
			// ____1rrrvvvvvvvv
			4 => {
				let r = (instr >> 8) & 0b_111;
				let v =  instr       & 0b_1111_1111;
				
				Ok(Op::Immediate(r as Reg, v as u8))
			}
		
			// _____1orrrRRRrrr
			// Toffoli, Fredkin
			5 => {
				let o  = ((instr >> 3 + 3 + 3) & 0b_1) == 0;
				let ra =  (instr >> 3 + 3)     & 0b_111;
				let rb =  (instr >> 3)         & 0b_111;
				let rc =   instr               & 0b_111;
				
				if o {
					Ok(Op::CSwap(ra as Reg, rb as Reg, rc as Reg))
				}
				else {
					Ok(Op::CCNot(ra as Reg, rb as Reg, rc as Reg))
				}
			}
			
			// ______1oooRRRrrr
			// Swap, C{Not, Add, Sub}, Rot{Left, Right}, Exchange
			6 => {
				let o  = (instr >> 3 + 3) & 0b_111;
				let ra = (instr >> 3)     & 0b_111;
				let rb =  instr           & 0b_111;
				
				match o {
					0b_000 => Ok(Op::Swap(ra as Reg, rb as Reg)),
					0b_001 => Ok(Op::CNot(ra as Reg, rb as Reg)),
					0b_010 => Ok(Op::CAdd(ra as Reg, rb as Reg)),
					0b_011 => Ok(Op::CSub(ra as Reg, rb as Reg)),
					0b_100 => Ok(Op::Exchange(ra as Reg, rb as Reg)),
					0b_101 => Ok(Op::RotLeft(ra as Reg, rb as Reg)),
					0b_110 => Ok(Op::RotRight(ra as Reg, rb as Reg)),
				
					// At this point, it's an invalid op value. We don't store
					// the value because it doesn't really matter what it is.
					// Since anything above 0b_111 is unreachable anyways, we
					// just return the error.
					_ => Err(InvalidInstr),
				}
			}
			
			// _______1orrrvvvv
			// Rot{Left, Right}Imm
			7 => {
				let o = ((instr >> 3 + 4) & 0b_1) == 1;
				let r =  (instr >> 3    ) & 0b_111;
				let v =   instr           & 0b_1111;
				
				Ok(if o { Op::RotRightImm(r as Reg, v as u8) }
				else { Op::RotLeftImm(r as Reg, v as u8) })
			}
			
			// _________1ooorrr
			// Not, Inc/Dec, Push/Pop, [Rev]SwapPc
			9 => {
				let o = (instr >> 3) & 0b_111;
				let r =  instr       & 0b_111;
				
				match o {
					0b_000 => Ok(Op::Not(r as Reg)),
					0b_001 => Ok(Op::Increment(r as Reg)),
					0b_010 => Ok(Op::Decrement(r as Reg)),
					0b_011 => Ok(Op::Push(r as Reg)),
					0b_100 => Ok(Op::Pop(r as Reg)),
					0b_101 => Ok(Op::SwapPc(r as Reg)),
					0b_110 => Ok(Op::RevSwapPc(r as Reg)),
					
					_ => Err(InvalidInstr),
				}
			}
			
			// _______________s
			15 => Ok(Op::Reverse),
			16 => Ok(Op::Halt),
			
			_ => Err(InvalidInstr),
		}
	}
}

impl fmt::Display for Op {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Op::Halt    => write!(f, "hlt"),
			Op::Reverse => write!(f, "rev"),
			
			Op::Not(r)       => write!(f, "not r{}", r),
			Op::Increment(r) => write!(f, "inc r{}", r),
			Op::Decrement(r) => write!(f, "dec r{}", r),
			Op::Push(r)      => write!(f, "push r{}", r),
			Op::Pop(r)       => write!(f, "pop r{}", r),
			Op::SwapPc(r)    => write!(f, "sp r{}", r),
			Op::RevSwapPc(r) => write!(f, "rsp r{}", r),
			
			Op::RotLeftImm(r, v)  => write!(f, "roli r{} {}", r, v),
			Op::RotRightImm(r, v) => write!(f, "rori r{} {}", r, v),
			
			Op::Swap(rl, rr)     => write!(f, "swp r{} r{}", rl, rr),
			Op::CNot(rc, rn)     => write!(f, "xor r{} r{}", rn, rc),
			Op::CAdd(rc, ra)     => write!(f, "add r{} r{}", ra, rc),
			Op::CSub(rc, rs)     => write!(f, "sub r{} r{}", rs, rc),
			Op::Exchange(rr, ra) => write!(f, "xchg r{} r{}", rr, ra),
			Op::RotLeft(rr, ro)  => write!(f, "rol r{} r{}", rr, ro),
			Op::RotRight(rr, ro) => write!(f, "ror r{} r{}", rr, ro),
			
			Op::CCNot(rc0, rc1, rn) => write!(f, "ccn r{} r{} r{}", rc0, rc1, rn),
			Op::CSwap(rc, rs0, rs1) => write!(f, "cswp r{} r{} r{}", rc, rs0, rs1),
			
			Op::Immediate(r, v)    => write!(f, "imm r{} {}", r, v),
			
			Op::BranchOdd(r, v)    => write!(f, "jpo r{} {}", r, v),
			Op::AssertEven(r, v)   => write!(f, "ape r{} {}", r, v),
			Op::BranchEven(r, v)   => write!(f, "jpe r{} {}", r, v),
			Op::AssertOdd(r, v)    => write!(f, "apo r{} {}", r, v),
			Op::BranchNeg(r, v)    => write!(f, "js r{} {}", r, v),
			Op::AssertNonneg(r, v) => write!(f, "ans r{} {}", r, v),
			Op::BranchNonneg(r, v) => write!(f, "jns r{} {}", r, v),
			Op::AssertNeg(r, v)    => write!(f, "as r{} {}", r, v),
			
			Op::GoTo(off)     => write!(f, "jmp {}", off),
			Op::ComeFrom(off) => write!(f, "pmj {}", off),
		}
	}
}

impl str::FromStr for Op {
	type Err = DeserialError;
	
	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let mut tokens = s.split_whitespace();
		
		// Parses string $string as type $from with a max value of
		// $max (inclusive), then converts it to type $to.
		macro_rules! parse_value(
			($string:expr, $from:ty => $to:ty, $max:expr) => {
				match $string.parse::<$from>() {
					Ok(value) if value <= $max => Ok(value as $to),
		
					Ok(_)  => Err(DeserialError::ValueTooLarge),
					Err(e) => Err(DeserialError::Parsing(e)),
				}
			}
		);
		
		// Parses a register literal. Returns early if an error is found.
		macro_rules! reg(() => {
			try!(tokens.next()
				.ok_or(DeserialError::NoArgument)
				.and_then(|s| {
					if s.starts_with('r') {
						parse_value!(s[1..], u8 => usize, 0b_111)
					}
					else {
						Err(DeserialError::NoRegister)
					}
				})
			)
		});
		
		// Parses a token of max value $max. Returns early if an error is found.
		macro_rules! value(
			// Parse as type $from, convert to type $to.
			($from:ty => $to:ty, $max:expr) => {
				try!(tokens.next()
					.ok_or(DeserialError::NoArgument)
					.and_then(|token| parse_value!(token, $from => $to, $max))
				)
			};
			
			// Parse and keep as type $t.
			($t:ty, $max:expr) => (value!($t => $t, $max));
		);
		
		use std::u8;
		
		Ok(match try!(tokens.next().ok_or(DeserialError::NoMneumonic)) {
			"hlt" => Op::Halt,
			"rev" => Op::Reverse,
			
			"not"  => Op::Not(reg!()),
			"inc"  => Op::Increment(reg!()),
			"dec"  => Op::Decrement(reg!()),
			"push" => Op::Push(reg!()),
			"pop"  => Op::Pop(reg!()),
			"sp"   => Op::SwapPc(reg!()),
			"rsp"  => Op::RevSwapPc(reg!()),
			
			"roli" => Op::RotLeftImm(reg!(), value!(u8, 0b_1111)),
			"rori" => Op::RotRightImm(reg!(), value!(u8, 0b_1111)),
			
			"swp"  => Op::Swap(reg!(), reg!()),
			"xor"  => Op::CNot(reg!(), reg!()),
			"add"  => Op::CAdd(reg!(), reg!()),
			"sub"  => Op::CSub(reg!(), reg!()),
			"xchg" => Op::Exchange(reg!(), reg!()),
			"rol"  => Op::RotLeft(reg!(), reg!()),
			"ror"  => Op::RotRight(reg!(), reg!()),
			
			"ccn"  => Op::CCNot(reg!(), reg!(), reg!()),
			"cswp" => Op::CSwap(reg!(), reg!(), reg!()),
			
			"imm" => Op::Immediate(reg!(), value!(u8, u8::MAX)),
			
			"jpo" => Op::BranchOdd(reg!(), value!(u8, u8::MAX)),
			"ape" => Op::AssertEven(reg!(), value!(u8, u8::MAX)),
			"jpe" => Op::BranchEven(reg!(), value!(u8, u8::MAX)),
			"apo" => Op::AssertOdd(reg!(), value!(u8, u8::MAX)),
			"js"  => Op::BranchNeg(reg!(), value!(u8, u8::MAX)),
			"ans" => Op::AssertNonneg(reg!(), value!(u8, u8::MAX)),
			"jns" => Op::BranchNonneg(reg!(), value!(u8, u8::MAX)),
			"as" => Op::AssertNeg(reg!(), value!(u8, u8::MAX)),
			
			"jmp" => Op::GoTo(value!(u16, 0b_11_1111_1111_1111)),
			"pmj" => Op::ComeFrom(value!(u16, 0b_11_1111_1111_1111)),
			
			// `return` because this `match` block is surrounded by an `Ok()`
			_ => return Err(DeserialError::UnknownMneu),
		})
	}
}
