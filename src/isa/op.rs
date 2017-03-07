use std::fmt;
use std::num;
use std::str::FromStr;
use std::error;
use std::result;

use super::reg::{self, Reg};

#[derive(Debug, PartialEq, Eq)]
pub enum Addr {
	Offset(usize),
	Label(String),
}

impl fmt::Display for Addr {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Addr::Offset(off)      => write!(f, "{}", off),
			Addr::Label(ref label) => f.write_str(label),
		}
	}
}

type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
	InvalidInstr(u16),
	UnresolvedLabel,
	
	NoMneumonic,
	NoArgument,
	NoAddress,
	ValueTooLarge,
	NoRegister,
	UnknownMneumonic(String),
	ExtraArgs(Vec<String>),
	Value(num::ParseIntError),
	Register(reg::ParseError),
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Error::InvalidInstr(i) =>
				write!(f, "Got invalid instruction: 0x{:04x}", i),
			Error::UnresolvedLabel =>
				write!(f, "Could not resolve label"),
			
			Error::NoMneumonic =>
				f.write_str("No mneumonic found."),
			Error::NoArgument =>
				f.write_str("Missing argument for this mneumonic."),
			Error::NoAddress =>
				f.write_str("No label or offset found."),
			Error::ValueTooLarge =>
				f.write_str("This value is bigger than the maximum value."),
			Error::NoRegister =>
				f.write_str("A register literal was expected."),
			
			Error::UnknownMneumonic(ref m) =>
				write!(f, "Did not recognize mneumonic: {}", m),
			Error::ExtraArgs(ref v) => 
				write!(f, "Found these extra tokens at the end: {:?}", v),
			Error::Value(ref e) =>
				write!(f, "Error parsing value: {}", e),
			Error::Register(ref e) =>
				write!(f, "Error parsing register literal: {}", e),
		}
	}
}

impl error::Error for Error {
	fn description(&self) -> &str {
		match *self {
			Error::InvalidInstr(..)     => "invalid instruction",
			Error::UnresolvedLabel      => "label not found",
			
			Error::NoMneumonic          => "missing mneumonic",
			Error::NoArgument           => "missing argument",
			Error::NoAddress            => "missing address",
			Error::ValueTooLarge        => "argument value is too big",
			Error::NoRegister           => "missing register literal",
			Error::UnknownMneumonic(..) => "unknown mneumonic",
			Error::ExtraArgs(..)        => "found extra tokens",
			Error::Value(ref e)         => e.description(),
			Error::Register(ref e)      => e.description(),
		}
	}
	
	fn cause(&self) -> Option<&error::Error> {
		match *self {
			Error::Value(ref e)    => Some(e),
			Error::Register(ref e) => Some(e),
			_ => None,
		}
	}
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
	BranchParity(Reg, Addr),
	
	/// Subtracts an immediate byte offset from the branch register if the
	/// the register is an odd number.
	AssertParity(Reg, Addr),
	
	/// Adds an immediate byte offset to the branch register if the register
	/// is below zero.
	BranchSign(Reg, Addr),
	
	/// Subtracts an immediate byte offset from the branch register if the
	/// register is below zero.
	AssertSign(Reg, Addr),
	
	/// Adds a value to the branch register.
	GoTo(Addr),
	
	/// Subtracts a value from the branch register.
	ComeFrom(Addr),
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
	
	pub fn encode(&self) -> Result<u16> {
		// All values in the enum should be explicitly casted, in case the
		// types change.
		Ok(match *self {
			// _______________0
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
			Op::BranchParity(r, Addr::Offset(off)) => 0b1_00 << 3 + 8
				| (r as u16) << 8
				| off as u16,
			
			Op::AssertParity(r, Addr::Offset(off)) => 0b1_01 << 3 + 8
				| (r as u16) << 8
				| off as u16,
			
			Op::BranchSign(r, Addr::Offset(off))   => 0b1_10 << 3 + 8
				| (r as u16) << 8
				| off as u16,
			
			Op::AssertSign(r, Addr::Offset(off))   => 0b1_11 << 3 + 8
				| (r as u16) << 8
				| off as u16,
			
			// 1ovvvvvvvvvvvvvv
			Op::GoTo(Addr::Offset(off))     => 0b1_0 << 14
				| off as u16,
			
			Op::ComeFrom(Addr::Offset(off)) => 0b1_1 << 14
				| off as u16,
			
			_ => return Err(Error::UnresolvedLabel)
		})
	}
	
	pub fn decode(instr: u16) -> Result<Op> {
		
		match instr.leading_zeros() {
			// 1ovvvvvvvvvvvvvv
			0 => {
				let o = ((instr >> 14) & 1) == 1;
				
				let v = (instr & 0b_11_1111_1111_1111) as usize;
				
				if o { Ok(Op::ComeFrom(Addr::Offset(v))) }
				else { Ok(Op::GoTo(Addr::Offset(v))) }
			}
			
			// __1oorrrvvvvvvvv
			2 => {
				let o = (instr >> 8 + 3) & 0b_11;
				let r = Reg::from((instr >> 8) & 0b_111);
				let v = (instr & 0b_1111_1111) as usize;
				
				match o {
					0b_00 => Ok(Op::BranchParity(r, Addr::Offset(v))),
					0b_01 => Ok(Op::AssertParity(r, Addr::Offset(v))),
					
					0b_10 => Ok(Op::BranchSign(r, Addr::Offset(v))),
					0b_11 => Ok(Op::AssertSign(r, Addr::Offset(v))),
					
					_ => unreachable!()
				}
			}
			
			// ____1rrrvvvvvvvv
			4 => {
				let r = Reg::from((instr >> 8) & 0b_111);
				let v = (instr & 0xFF) as u8;
				
				Ok(Op::Immediate(r, v))
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
					
					_ => Err(Error::InvalidInstr(instr))
				}
			}
			
			// _______1orrrvvvv
			7 => {
				let o = ((instr >> 3 + 4) & 1) == 1;
				let r = Reg::from((instr >> 4) & 0b_111);
				let v = (instr & 0b_1111) as u8;
				
				if o { Ok(Op::RotRightImm(r, v)) }
				else { Ok(Op::RotLeftImm(r, v)) }
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
					
					_ => unreachable!()
				}
			}
			
			// ____________1rrr
			12 => Ok(Op::Not(Reg::from(instr & 0b_111))),
			
			// _______________0
			16 => Ok(Op::Halt),
			
			_ => Err(Error::InvalidInstr(instr)),
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
			
			Op::CCNot(rc0, rc1, rn) => write!(f, "ccn {} {} {}", rc0, rc1, rn),
			Op::CSwap(rc, rs0, rs1) => write!(f, "cswp {} {} {}", rc, rs0, rs1),
			
			Op::Immediate(r, v)    => write!(f, "xori {} {}", r, v),
			
			Op::BranchParity(r, ref v) => write!(f, "jp {} {}", r, v),
			Op::AssertParity(r, ref v) => write!(f, "ap {} {}", r, v),
			Op::BranchSign(r, ref v)   => write!(f, "js {} {}", r, v),
			Op::AssertSign(r, ref v)   => write!(f, "as {} {}", r, v),
			
			Op::GoTo(ref off)     => write!(f, "jmp {}", off),
			Op::ComeFrom(ref off) => write!(f, "pmj {}", off),
		}
	}
}

impl FromStr for Op {
	type Err = Error;
	
	fn from_str(s: &str) -> Result<Self> {
		
		let mut tokens = s.split_whitespace();
		
		// Parses a register literal. Returns early if an error is found.
		macro_rules! reg(() => {
			tokens.next()
			.ok_or(Error::NoRegister)
			.and_then(|s| s.parse::<Reg>()
				.map_err(Error::Register)
			)?
		});
		
		// Parses a token as type $t with max value $max.
		// Returns early if an error is found.
		macro_rules! val(($t:ty, $max:expr) => {
			tokens.next()
			.ok_or(Error::NoArgument)
			.and_then(|token| match token.parse::<$t>() {
				Ok(value) if value <= $max => Ok(value),

				Ok(_)  => Err(Error::ValueTooLarge),
				Err(e) => Err(Error::Value(e)),
			})?
		});
		
		macro_rules! addr(($max:expr) => {
			tokens.next()
			.ok_or(Error::NoAddress)
			.and_then(|tok| {
				let first = tok.chars().nth(0).unwrap();
				
				if (first.is_alphabetic() || first == '_') && tok.chars().skip(1).all(|c| c.is_alphanumeric() || c == '_') {
					Ok(Addr::Label(tok.to_string()))
				}
				else if tok.chars().all(|c| c >= '0' && c <= '9') {
					match tok.parse::<usize>() {
						Ok(value) if value <= $max =>
							Ok(Addr::Offset(value)),

						Ok(_)  => Err(Error::ValueTooLarge),
						Err(e) => Err(Error::Value(e))
					}
				}
				else {
					Err(Error::NoAddress)
				}
			})?
		});
		
		use std::u8;
		
		let mneu = tokens.next().ok_or(Error::NoMneumonic)?;
		let op = match mneu {
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
			
			"roli" => Op::RotLeftImm(reg!(), val!(u8, 0b_1111)),
			"rori" => Op::RotRightImm(reg!(), val!(u8, 0b_1111)),
			
			"swp"  => Op::Swap(reg!(), reg!()),
			"xor"  => Op::CNot(reg!(), reg!()),
			"add"  => Op::CAdd(reg!(), reg!()),
			"sub"  => Op::CSub(reg!(), reg!()),
			"xchg" => Op::Exchange(reg!(), reg!()),
			"rol"  => Op::RotLeft(reg!(), reg!()),
			"ror"  => Op::RotRight(reg!(), reg!()),
			
			"ccn"  => Op::CCNot(reg!(), reg!(), reg!()),
			"cswp" => Op::CSwap(reg!(), reg!(), reg!()),
			
			"xori" => Op::Immediate(reg!(), val!(u8, u8::MAX)),
			
			"jp" => Op::BranchParity(reg!(), addr!(u8::MAX as usize)),
			"ap" => Op::AssertParity(reg!(), addr!(u8::MAX as usize)),
			"js" => Op::BranchSign(reg!(), addr!(u8::MAX as usize)),
			"as" => Op::AssertSign(reg!(), addr!(u8::MAX as usize)),
			
			"jmp" => Op::GoTo(addr!(0b_11_1111_1111_1111)),
			"pmj" => Op::ComeFrom(addr!(0b_11_1111_1111_1111)),
			
			mneu => return Err(Error::UnknownMneumonic(mneu.to_string()))
		};
		
		// check to make sure all tokens are exhausted.
		let leftovers: Vec<_> = tokens.map(|s| s.to_string()).collect();
		
		if leftovers.is_empty() { Ok(op) }
		else { Err(Error::ExtraArgs(leftovers)) }
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
