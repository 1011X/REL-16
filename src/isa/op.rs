use std::fmt;
use std::num;
use std::str::FromStr;
use std::error::Error;

use super::reg::{self, Reg};

#[derive(Debug)]
pub enum DeserialError {
	NoMneumonic,
	NoArgument,
	UnknownMneumonic,
	ValueTooLarge,
	NoRegister,
	Value(num::ParseIntError),
	Register(reg::ParseError),
}

impl fmt::Display for DeserialError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			DeserialError::NoMneumonic =>
				write!(f, "No mneumonic found."),
			DeserialError::NoArgument =>
				write!(f, "Missing argument for this mneumonic."),
			DeserialError::UnknownMneumonic =>
				write!(f, "Did not recognize this mneumonic."),
			DeserialError::ValueTooLarge =>
				write!(f, "This value is bigger than the maximum value."),
			DeserialError::NoRegister =>
				write!(f, "A register literal was expected."),
			DeserialError::Value(ref e) =>
				write!(f, "Error parsing value: {}", e),
			DeserialError::Register(ref e) =>
				write!(f, "Error parsing register literal: {}", e),
		}
	}
}

impl Error for DeserialError {
	fn description(&self) -> &str {
		match *self {
			DeserialError::NoMneumonic      => "missing mneumonic",
			DeserialError::NoArgument       => "missing argument",
			DeserialError::UnknownMneumonic => "unknown mneumonic",
			DeserialError::ValueTooLarge    => "argument value is too big",
			DeserialError::NoRegister       => "expected register literal",
			DeserialError::Value(ref e)     => e.description(),
			DeserialError::Register(ref e)  => e.description(),
		}
	}
	
	fn cause(&self) -> Option<&Error> {
		match *self {
			DeserialError::Value(ref e)    => Some(e),
			DeserialError::Register(ref e) => Some(e),
			_ => None,
		}
	}
}


#[derive(Debug)]
pub struct InvalidInstr;

impl fmt::Display for InvalidInstr {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		f.write_str("Invalid instruction found while parsing.")
	}
}

impl Error for InvalidInstr {
	fn description(&self) -> &'static str { "invalid instruction" }
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
/// * Not: `____________1rrr`
/// * Single register: `_________1ooorrr`
/// * Single register, 4-bit immediate: `_______1orrrvvvv`
/// * Double register: `______1oooRRRrrr`
/// * Triple register: `_____1orrrRRRrrr`
/// * Immediate: `____1rrrvvvvvvvv`
/// * Branches and assertions: `_1ooorrrvvvvvvvv`
/// * Jumps: `1ovvvvvvvvvvvvvv`
/// 
/// Invalid values:
/// * `_1xxxxxxxxxxxxxx`
/// * `___1xxxxxxxxxxxx`
/// * `_________1xxxxxx`
/// * `__________1xxxxx`
/// * `___________1xxxx`
/// * `_____________1xx`
/// * `______________1x`

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
	/// Stops the machine.
	Halt,
	
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
	
	/// Signed multiplication by 2.
	/// 
	/// If the register is between -16384 and 16383, this will double its value.
	/// 
	/// If its value *r* is less than -16384, you'll get `2 * r - MIN + 1`.
	/// 
	/// Otherwise (i.e. its value *r* is greater than 16384), you'll get
	/// `2 * r - MAX`.
	Mul2(Reg),
	
	/// Signed division by 2.
	/// 
	/// If the register's value *r* is odd and greater than 0, you'll get
	/// `MAX - (MAX - r) / 2`.
	/// 
	/// If it's odd and less than 0, you'll get `MIN + (r - MIN - 1) / 2`.
	/// 
	/// Otherwise (i.e. when it's even), you'll get `x / 2`.
	Div2(Reg),
	
	/// Rotates the register's bits to the left by the given amount.
	RotLeftImm(Reg, u8),
	
	/// Rotates the register's bits to the right by the given amount.
	RotRightImm(Reg, u8),
	
	/// Swaps the registers' values.
	Swap(Reg, Reg),
	
	/// Flips bits in the first register based on bits in the second register,
	/// like x86's `xor` instruction.
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
	
	/// Sends value at first register to port given in second register.
	IO(Reg, Reg),
	
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
	
	/// Adds an immediate byte offset to the branch register if the register
	/// is an odd number.
	BranchParity(Reg, u8),
	
	/// Subtracts an immediate byte offset from the branch register if the
	/// the register is an odd number.
	AssertParity(Reg, u8),
	
	/// Adds an immediate byte offset to the branch register if the register
	/// is below zero.
	BranchSign(Reg, u8),
	
	/// Subtracts an immediate byte offset from the branch register if the
	/// register is below zero.
	AssertSign(Reg, u8),
	
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
			Op::Not(..)            => self,
			Op::Increment(r)       => Op::Decrement(r),
			Op::Decrement(r)       => Op::Increment(r),
			Op::Push(r)            => Op::Pop(r),
			Op::Pop(r)             => Op::Push(r),
			Op::SwapPc(r)          => Op::RevSwapPc(r),
			Op::RevSwapPc(r)       => Op::SwapPc(r),
			Op::Mul2(r)            => Op::Div2(r),
			Op::Div2(r)            => Op::Mul2(r),
			Op::RotLeftImm(r, v)   => Op::RotRightImm(r, v),
			Op::RotRightImm(r, v)  => Op::RotLeftImm(r, v),
			Op::Swap(..)           => self,
			Op::CNot(..)           => self,
			Op::CAdd(rc, ra)       => Op::CSub(rc, ra),
			Op::CSub(rc, rs)       => Op::CAdd(rc, rs),
			Op::Exchange(..)       => self,
			Op::IO(..)             => self,
			Op::RotLeft(rr, rv)    => Op::RotRight(rr, rv),
			Op::RotRight(rr, rv)   => Op::RotLeft(rr, rv),
			Op::CCNot(..)          => self,
			Op::CSwap(..)          => self,
			Op::Immediate(..)      => self,
			Op::BranchParity(r, o) => Op::AssertParity(r, o),
			Op::BranchSign(r, o)   => Op::AssertSign(r, o),
			Op::AssertParity(r, o) => Op::BranchParity(r, o),
			Op::AssertSign(r, o)   => Op::BranchSign(r, o),
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
			
			// ____________1rrr
			Op::Not(reg)       => 1 << 3
				| reg as u16,
			
			// _________1ooorrr
			Op::Increment(reg) => 0b1_000 << 3
				| reg as u16,
			
			Op::Decrement(reg) => 0b1_001 << 3
				| reg as u16,
			
			Op::Push(reg)      => 0b1_010 << 3
				| reg as u16,
			
			Op::Pop(reg)       => 0b1_011 << 3
				| reg as u16,
			
			Op::SwapPc(reg)    => 0b1_100 << 3
				| reg as u16,
			
			Op::RevSwapPc(reg) => 0b1_101 << 3
				| reg as u16,
			
			Op::Mul2(reg)      => 0b1_110 << 3
				| reg as u16,
			
			Op::Div2(reg)      => 0b1_111 << 3
				| reg as u16,
			
			// _______1orrrvvvv
			Op::RotLeftImm(r, off)  => 0b1_0 << 3 + 4
				| (r as u16) << 4
				| off as u16,
			
			Op::RotRightImm(r, off) => 0b1_1 << 3 + 4
				| (r as u16) << 4
				| off as u16,
			
			// ______1oooRRRrrr
			Op::Swap(rl, rr)     => 0b1_000 << 3 + 3
				| (rl as u16) << 3
				| rr as u16,
			
			Op::CNot(rc, rn)     => 0b1_001 << 3 + 3
				| (rc as u16) << 3
				| rn as u16,
			
			Op::CAdd(rc, ra)     => 0b1_010 << 3 + 3
				| (rc as u16) << 3
				| ra as u16,
			
			Op::CSub(rc, rs)     => 0b1_011 << 3 + 3
				| (rc as u16) << 3
				| rs as u16,
			
			Op::Exchange(r, ra)  => 0b1_100 << 3 + 3
				| (r as u16) << 3
				| ra as u16,
			
			Op::RotLeft(rr, ro)  => 0b1_101 << 3 + 3
				| (rr as u16) << 3
				| ro as u16,
			
			Op::RotRight(rr, ro) => 0b1_110 << 3 + 3
				| (rr as u16) << 3
				| ro as u16,
			
			Op::IO(rd, rp)       => 0b1_111 << 3 + 3
				| (rd as u16) << 3
				| rp as u16,
			
			// _____1orrrRRRrrr
			Op::CCNot(rc0, rc1, rn) => 0b1_0 << 3 + 3 + 3
				| (rc0 as u16) << 6
				| (rc1 as u16) << 3
				| rn as u16,
			
			Op::CSwap(rc, rs0, rs1) => 0b1_1 << 3 + 3 + 3
				| (rc as u16) << 6
				| (rs0 as u16) << 3
				| rs1 as u16,
			
			// ____1rrrvvvvvvvv
			Op::Immediate(r, val) => 0b1 << 3 + 8
				| (r as u16) << 8
				| val as u16,
			
			// __1oorrrvvvvvvvv
			Op::BranchParity(r, off) => 0b1_00 << 3 + 8
				| (r as u16) << 8
				| off as u16,
			
			Op::AssertParity(r, off) => 0b1_01 << 3 + 8
				| (r as u16) << 8
				| off as u16,
			
			Op::BranchSign(r, off)   => 0b1_10 << 3 + 8
				| (r as u16) << 8
				| off as u16,
			
			Op::AssertSign(r, off)   => 0b1_11 << 3 + 8
				| (r as u16) << 8
				| off as u16,
			
			// 1ovvvvvvvvvvvvvv
			Op::GoTo(off)     => 0b1_0 << 14
				| off as u16,
			
			Op::ComeFrom(off) => 0b1_1 << 14
				| off as u16,
		}
	}
	
	pub fn decode(instr: u16) -> Result<Op, InvalidInstr> {
		
		match instr.leading_zeros() {
			// 1ovvvvvvvvvvvvvv
			0 => {
				let o = ((instr >> 14) & 1) == 1;
				
				let v = instr & 0b_11_1111_1111_1111;
				
				if o { Ok(Op::ComeFrom(v as u16)) }
				else { Ok(Op::GoTo(v as u16)) }
			}
			
			// __1oorrrvvvvvvvv
			2 => {
				let o = (instr >> 8 + 3) & 0b_11;
				let r = Reg::from((instr >> 8) & 0b_111);
				let v = instr & 0b_1111_1111;
				
				match o {
					0b_00 => Ok(Op::BranchParity(r, v as u8)),
					0b_01 => Ok(Op::AssertParity(r, v as u8)),
					
					0b_10 => Ok(Op::BranchSign(r, v as u8)),
					0b_11 => Ok(Op::AssertSign(r, v as u8)),
					
					_ => unreachable!()
				}
			}
			
			// ____1rrrvvvvvvvv
			4 => {
				let r = Reg::from((instr >> 8) & 0b_111);
				let v = instr & 0xFF;
				
				Ok(Op::Immediate(r, v as u8))
			}
		
			// _____1orrrRRRrrr
			5 => {
				let o = ((instr >> 9) & 1) == 1;
				
				let ra = Reg::from((instr >> 6) & 0b_111);
				let rb = Reg::from((instr >> 3) & 0b_111);
				let rc = Reg::from(instr & 0b_111);
				
				if o { Ok(Op::CSwap(ra, rb, rc)) }
				else { Ok(Op::CCNot(ra, rb, rc)) }
			}
			
			// ______1oooRRRrrr
			6 => {
				let o = (instr >> 6) & 0b_111;
				
				let ra = Reg::from((instr >> 3) & 0b_111);
				let rb = Reg::from(instr & 0b_111);
				
				match o {
					0b_000 => Ok(Op::Swap(ra, rb)),
					0b_001 => Ok(Op::CNot(ra, rb)),
					0b_010 => Ok(Op::CAdd(ra, rb)),
					0b_011 => Ok(Op::CSub(ra, rb)),
					0b_100 => Ok(Op::Exchange(ra, rb)),
					0b_101 => Ok(Op::RotLeft(ra, rb)),
					0b_110 => Ok(Op::RotRight(ra, rb)),
					0b_111 => Ok(Op::IO(ra, rb)),
					
					_ => Err(InvalidInstr)
				}
			}
			
			// _______1orrrvvvv
			7 => {
				let o = ((instr >> 3 + 4) & 1) == 1;
				let r = Reg::from((instr >> 4) & 0b_111);
				let v = instr & 0b_1111;
				
				if o { Ok(Op::RotRightImm(r, v as u8)) }
				else { Ok(Op::RotLeftImm(r, v as u8)) }
			}
			
			// ________1oooorrr
			9 => {
				let o = (instr >> 3) & 0b_111;
				let r = Reg::from(instr & 0b_111);
				
				match o {
					0b_000 => Ok(Op::Increment(r)),
					0b_001 => Ok(Op::Decrement(r)),
					0b_010 => Ok(Op::Push(r)),
					0b_011 => Ok(Op::Pop(r)),
					0b_100 => Ok(Op::SwapPc(r)),
					0b_101 => Ok(Op::RevSwapPc(r)),
					0b_110 => Ok(Op::Mul2(r)),
					0b_111 => Ok(Op::Div2(r)),
					
					_ => Err(InvalidInstr)
				}
			}
			
			// ____________1rrr
			12 => {
				let r = Reg::from(instr & 0b_111);
				
				Ok(Op::Not(r))
			}
			
			// _______________s
			16 => Ok(Op::Halt),
			
			0...16 => Err(InvalidInstr),
			
			_ => unreachable!()
		}
	}
}

impl fmt::Display for Op {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Op::Halt => f.write_str("hlt"),
			
			Op::Not(r)       => write!(f, "not {}", r),
			Op::Increment(r) => write!(f, "inc {}", r),
			Op::Decrement(r) => write!(f, "dec {}", r),
			Op::Push(r)      => write!(f, "push {}", r),
			Op::Pop(r)       => write!(f, "pop {}", r),
			Op::SwapPc(r)    => write!(f, "spc {}", r),
			Op::RevSwapPc(r) => write!(f, "rspc {}", r),
			Op::Mul2(r)      => write!(f, "mul2 {}", r),
			Op::Div2(r)      => write!(f, "div2 {}", r),
			
			Op::RotLeftImm(r, v)  => write!(f, "roli {} {}", r, v),
			Op::RotRightImm(r, v) => write!(f, "rori {} {}", r, v),
			
			Op::Swap(rl, rr)     => write!(f, "swp {} {}", rl, rr),
			Op::CNot(rn, rc)     => write!(f, "xor {} {}", rn, rc),
			Op::CAdd(ra, rc)     => write!(f, "add {} {}", ra, rc),
			Op::CSub(rs, rc)     => write!(f, "sub {} {}", rs, rc),
			Op::Exchange(rr, ra) => write!(f, "xchg {} {}", rr, ra),
			Op::RotLeft(rr, ro)  => write!(f, "rol {} {}", rr, ro),
			Op::RotRight(rr, ro) => write!(f, "ror {} {}", rr, ro),
			Op::IO(rd, rp)       => write!(f, "io {} {}", rd, rp),
			
			Op::CCNot(rc0, rc1, rn) => write!(f, "ccn {} {} {}", rc0, rc1, rn),
			Op::CSwap(rc, rs0, rs1) => write!(f, "cswp {} {} {}", rc, rs0, rs1),
			
			Op::Immediate(r, v)    => write!(f, "xori {} {}", r, v),
			
			Op::BranchParity(r, v) => write!(f, "jp {} {}", r, v),
			Op::AssertParity(r, v) => write!(f, "ap {} {}", r, v),
			Op::BranchSign(r, v)   => write!(f, "js {} {}", r, v),
			Op::AssertSign(r, v)   => write!(f, "as {} {}", r, v),
			
			Op::GoTo(off)     => write!(f, "jmp {}", off),
			Op::ComeFrom(off) => write!(f, "pmj {}", off),
		}
	}
}

impl FromStr for Op {
	type Err = DeserialError;
	
	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let mut tokens = s.split_whitespace();
		
		// Parses a register literal. Returns early if an error is found.
		macro_rules! reg(() => {
			try!(tokens.next()
				.ok_or(DeserialError::NoArgument)
				.and_then(|s| s.parse::<Reg>()
					.map_err(DeserialError::Register)
				)
			)
		});
		
		// Parses a token as type $t with max value $max. Returns early if an error is found.
		macro_rules! value(
			($t:ty, $max:expr) => {
				try!(tokens.next()
					.ok_or(DeserialError::NoArgument)
					.and_then(|token| match token.parse::<$t>() {
						Ok(value) if value <= $max => Ok(value),

						Ok(_)  => Err(DeserialError::ValueTooLarge),
						Err(e) => Err(DeserialError::Value(e)),
					})
				)
			};
		);
		
		use std::u8;
		
		// TODO: check to make sure all tokens are exhausted.
		Ok(match try!(tokens.next().ok_or(DeserialError::NoMneumonic)) {
			"hlt" => Op::Halt,
			
			"not"  => Op::Not(reg!()),
			"inc"  => Op::Increment(reg!()),
			"dec"  => Op::Decrement(reg!()),
			"push" => Op::Push(reg!()),
			"pop"  => Op::Pop(reg!()),
			"spc"  => Op::SwapPc(reg!()),
			"rspc" => Op::RevSwapPc(reg!()),
			"mul2" => Op::Mul2(reg!()),
			"div2" => Op::Div2(reg!()),
			
			"roli" => Op::RotLeftImm(reg!(), value!(u8, 0b_1111)),
			"rori" => Op::RotRightImm(reg!(), value!(u8, 0b_1111)),
			
			"swp"  => Op::Swap(reg!(), reg!()),
			"xor"  => Op::CNot(reg!(), reg!()),
			"add"  => Op::CAdd(reg!(), reg!()),
			"sub"  => Op::CSub(reg!(), reg!()),
			"xchg" => Op::Exchange(reg!(), reg!()),
			"rol"  => Op::RotLeft(reg!(), reg!()),
			"ror"  => Op::RotRight(reg!(), reg!()),
			"io"   => Op::IO(reg!(), reg!()),
			
			"ccn"  => Op::CCNot(reg!(), reg!(), reg!()),
			"cswp" => Op::CSwap(reg!(), reg!(), reg!()),
			
			"xori" => Op::Immediate(reg!(), value!(u8, u8::MAX)),
			
			"jp" => Op::BranchParity(reg!(), value!(u8, u8::MAX)),
			"ap" => Op::AssertParity(reg!(), value!(u8, u8::MAX)),
			"js" => Op::BranchSign(reg!(), value!(u8, u8::MAX)),
			"as" => Op::AssertSign(reg!(), value!(u8, u8::MAX)),
			
			"jmp" => Op::GoTo(value!(u16, 0b_11_1111_1111_1111)),
			"pmj" => Op::ComeFrom(value!(u16, 0b_11_1111_1111_1111)),
			
			// `return` because this `match` block is surrounded by an `Ok()`
			_ => return Err(DeserialError::UnknownMneumonic),
		})
	}
}

#[cfg(test)]
mod tests {
	use super::Op;
	use super::super::reg::Reg;
	use std::str::FromStr;
	
	#[test]
	fn instruction_encoding() {
		// Make a vector with all parameters initialized to 1s
		let ops = vec![
			Op::Halt,
			Op::Not(Reg::BP),
			Op::Increment(Reg::BP),
			Op::Decrement(Reg::BP),
			Op::Push(Reg::BP),
			Op::Pop(Reg::BP),
			Op::SwapPc(Reg::BP),
			Op::RevSwapPc(Reg::BP),
			Op::Mul2(Reg::BP),
			Op::Div2(Reg::BP),
			Op::RotLeftImm(Reg::BP, 0xF),
			Op::RotRightImm(Reg::BP, 0xF),
			Op::Swap(Reg::BP, Reg::BP),
			Op::CNot(Reg::BP, Reg::BP),
			Op::CAdd(Reg::BP, Reg::BP),
			Op::CSub(Reg::BP, Reg::BP),
			Op::Exchange(Reg::BP, Reg::BP),
			Op::RotLeft(Reg::BP, Reg::BP),
			Op::RotRight(Reg::BP, Reg::BP),
			Op::IO(Reg::BP, Reg::BP),
			Op::CCNot(Reg::BP, Reg::BP, Reg::BP),
			Op::CSwap(Reg::BP, Reg::BP, Reg::BP),
			Op::Immediate(Reg::BP, 0xFF),
			Op::BranchParity(Reg::BP, 0xFF),
			Op::AssertParity(Reg::BP, 0xFF),
			Op::BranchSign(Reg::BP, 0xFF),
			Op::AssertSign(Reg::BP, 0xFF),
			Op::GoTo(0x3FFF),
			Op::ComeFrom(0x3FFF),
		];
		
		for op in ops {
			// `Op::decode` shouldn't error when decoding a valid instruction,
			// so just unwrap it.
			assert_eq!(op, Op::decode(op.encode()).unwrap());
			
			// Also test that string conversion and back is reliable.
			assert_eq!(op, Op::from_str(&op.to_string()).unwrap());
		}
	}
}
