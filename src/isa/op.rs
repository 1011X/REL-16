use std::fmt;
use std::num;
use std::result;
use std::convert::From;
use std::error::Error;
use std::str::FromStr;

use super::reg::{self, Reg};

type Result<T> = result::Result<T, ParseOpError>;

/// Tracks address labels to be converted to offsets later.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Addr {
	Label(String),
	Offset(usize),
}

impl fmt::Display for Addr {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Addr::Label(label) => write!(f, "{}", label),
			Addr::Offset(offset)   => write!(f, "{}", offset),
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
	Mneumonic,
	Register,
	Constant,
	Address
}

/// Various things that can go wrong in the process of parsing a string to an
/// `Op` enum.
#[derive(Debug)]
pub enum ParseOpError {
	ExtraToken,
	NoToken(Type),
	BadToken(Type, String),
	ValueOverflow(usize),
	ParseInt(num::ParseIntError),
}

impl fmt::Display for ParseOpError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use self::ParseOpError::*;
		match self {
			ExtraToken =>
				write!(f, "Found extra token."),
			NoToken(t) =>
				write!(f, "Missing token of type {:?}", t),
			BadToken(t, ref tok) =>
				write!(f, "Bad token; expected a {:?}, got {:?}", t, tok),
			ValueOverflow(max) =>
				write!(f, "Value exceeds maximum allowed value of {}", max),
			ParseInt(ref pie) =>
				write!(f, "{}", pie),
		}
	}
}

impl Error for ParseOpError {
	fn description(&self) -> &'static str {
		use self::ParseOpError::*;
		match self {
			ExtraToken       => "extra token",
			NoToken(_)       => "missing token",
			BadToken(..)     => "malformed token",
			ValueOverflow(_) => "value exceeds maximum",
			ParseInt(_)      => "value too big to parse",
		}
	}
	
	fn cause(&self) -> Option<&Error> {
		match self {
			ParseOpError::ParseInt(pie) => Some(pie),
			_ => None
		}
	}
}

impl From<reg::ParseError> for ParseOpError {
	fn from(e: reg::ParseError) -> Self {
		ParseOpError::BadToken(Type::Register, e.0.clone())
	}
}

impl From<num::ParseIntError> for ParseOpError {
	fn from(e: num::ParseIntError) -> Self {
		ParseOpError::ParseInt(e)
	}
}

/**
High-level machine instruction representation

This enum represents all valid instructions used in the REL-16
architecture. It allows for simpler processing compared to string
interpretation.

It's required that each variant here has a reversible equivalent.
That is, if the instruction can't undo its own actions, it must have
a partner instruction that can. By tying each instruction with its
opposite, we can guarantee the reversibility property of the
architecture.

Being a 16-bit architecture, we must organize the bits so all
instructions are representable within 16 bits. There are some
addressing modes:

* Signal (3): nothing
* Register (3): 1-3 registers
  * Single (4-5): 1 register
  * Double (7): 2 registers
  * Triple (2): 3 registers, ah ah ah
* Immediate (6): register, 8-bit value
* Branch (8): register, address
* Jump (2-3): address
*/
// TODO: decide whether to keep branch instructions using I-form or change them
// to R-form and have a constant offset.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
	/// Stops the machine.
	Halt,
	
	/// Does absolutely nothing.
	Nop,
	
	/// Can be used for debugging purposes.
	Debug,
	
	/// Flips every bit in the register.
	Not(Reg),
	
	/// Turns the given register's value into its two's complement.
	Negate(Reg),
	
	#[cfg(feature = "xor-pc")]
	/// Flips bits in PC register based on given register.
	/// 
	/// The destination instruction has no restrictions. The only
	/// restriction is that this is dependent on code/memory
	/// position.
	XorPc(Reg),
	
	/// Swaps the register and the program counter. Used for calling
	/// functions.
	SwapPc(Reg),
	
	/// Flips direction bit, then swaps the register and the program
	/// counter. Used when **un**calling functions.
	RevSwapPc(Reg),
	
	/// Swaps the registers' values.
	Swap(Reg, Reg),
	
	/// Swaps the first register's value with the value pointed to in
	/// memory by the second register.
	Exchange(Reg, Reg),
	
	/// Flips bits in first register based on bits in second register.
	/// Exactly like x86's `xor` instruction.
	Xor(Reg, Reg),
	
	/// Adds/increases first register's value by second register's
	/// value.
	Add(Reg, Reg),
	
	/// Subtracts/decreases the first register's value by the second
	/// register's value.
	Sub(Reg, Reg),
	
	/// Rotates the first register's bits leftwards by the value in
	/// the second register.
	/// 
	/// Only the first 4 bits are necessary/used.
	LRot(Reg, Reg),
	
	/// Rotates the first register's bits rightwards by the value in
	/// the second register.
	/// 
	/// Only the first 4 bits are necessary/used.
	RRot(Reg, Reg),
	
	/// Flips bits in the register's lower half based on the bits of
	/// the immediate byte value.
	/// 
	/// This is usually used to set the register to the given value
	/// or to reset its value to zero.
	XorImm(Reg, u8),
	
	/// Adds/increases first register's value by the value of the
	/// given immediate.
	AddImm(Reg, u8),
	
	/// Subtracts/decreases first register's value by the value of the
	/// given immediate.
	SubImm(Reg, u8),
	
	/// Rotates the register's bits leftwards by the given amount. The
	/// last bit's value is moved to the first bit.
	LRotImm(Reg, u8),
	
	/// Rotates the register's bits rightwards by the given amount.
	/// The first bit's value is moved to the last bit.
	RRotImm(Reg, u8),
	
	/// Toffoli gate; ANDs first and second registers and flips the
	/// bits in the third register based on the result.
	/// 
	/// If a register is in the first or second position, it *should
	/// not* be in the third position.
	CCNot(Reg, Reg, Reg),
	
	/// Fredkin gate; swaps bits in second and third registers based
	/// on bits in the first register.
	/// 
	/// If a register is in the first position, it *should not* be in
	/// the second or third position.
	CSwap(Reg, Reg, Reg),
	
	/// Adds an immediate value to the branch register.
	/// 
	/// This is used to jump unconditionally to another instruction.
	/// To avoid wonky behavior, the destination should be a ComeFrom
	/// instruction to avoid skipping instructions.
	GoTo(Addr),
	
	/// Subtracts an immediate value from the branch register.
	/// 
	/// This is used to jump unconditionally to another instruction.
	/// To avoid wonky behavior, the destination should be a GoTo
	/// instruction to avoid skipping instructions.
	ComeFrom(Addr),
	
	#[cfg(feature = "teleport")]
	/// XORs an immediate value to the PC register.
	/// 
	/// The only restrictions on this instruction are that it be dependent
	/// on code/memory position, and the higher bits of PC can't be reached.
	Teleport(Addr),
	
	/// Adds an offset to the branch register if the register is an
	/// odd number.
	BranchOdd(Reg, Addr),
	
	/// Subtracts an offset from the branch register if the register
	/// is an odd number.
	AssertOdd(Reg, Addr),
	
	/// Adds an offset to the branch register if the register is
	/// negative.
	BranchNeg(Reg, Addr),
	
	/// Subtracts an offset from the branch register if the register
	/// is negative.
	AssertNeg(Reg, Addr),
	
	/// Adds an offset to the branch register if the register is an
	/// even number.
	BranchEven(Reg, Addr),
	
	/// Subtracts an offset from the branch register if the register
	/// is an even number.
	AssertEven(Reg, Addr),
	
	/// Adds an offset to the branch register if the register is not
	/// negative.
	BranchNotNeg(Reg, Addr),
	
	/// Subtracts an offset from the branch register if the register
	/// is not negative.
	AssertNotNeg(Reg, Addr),
	
	/// Swaps data at given register to the device at the given port.
	///
	/// Devices can often be used as a way of getting rid of unwanted
	/// garbage, so swapping becomes the preferred method. If one
	/// wishes to keep a copy of the data sent, `xor` it first.
	IO(Reg, u8),
}

impl Op {
	// To guarantee VM is reversible, every operation must have an
	// inverse. Involutory instructions (which are their own inverse)
	// return self.
	pub fn invert(self) -> Op {
		match self {
			Op::Halt          => self,
			Op::Nop           => self,
			Op::Debug         => self,
			
			Op::Not(_)        => self,
			Op::Negate(_)     => self,
			#[cfg(feature = "xor-pc")]
			Op::XorPc(_)      => self,
			Op::SwapPc(r)     => Op::RevSwapPc(r),
			Op::RevSwapPc(r)  => Op::SwapPc(r),
			
			Op::Swap(..)      => self,
			Op::Exchange(..)  => self,
			Op::Xor(..)       => self,
			Op::Add(ra, rc)   => Op::Sub(ra, rc),
			Op::Sub(rs, rc)   => Op::Add(rs, rc),
			Op::LRot(rr, rv)  => Op::RRot(rr, rv),
			Op::RRot(rr, rv)  => Op::LRot(rr, rv),
			
			Op::XorImm(..)    => self,
			Op::AddImm(rc, i) => Op::SubImm(rc, i),
			Op::SubImm(rc, i) => Op::AddImm(rc, i),
			Op::LRotImm(r, v) => Op::RRotImm(r, v),
			Op::RRotImm(r, v) => Op::LRotImm(r, v),
			
			Op::CCNot(..)     => self,
			Op::CSwap(..)     => self,
			
			Op::BranchOdd(r, a)    => Op::AssertOdd(r, a),
			Op::BranchEven(r, a)   => Op::AssertEven(r, a),
			Op::AssertOdd(r, a)    => Op::BranchOdd(r, a),
			Op::AssertEven(r, a)   => Op::BranchEven(r, a),
			Op::BranchNeg(r, a)    => Op::AssertNeg(r, a),
			Op::BranchNotNeg(r, a) => Op::AssertNotNeg(r, a),
			Op::AssertNeg(r, a)    => Op::BranchNeg(r, a),
			Op::AssertNotNeg(r, a) => Op::BranchNotNeg(r, a),
			
			#[cfg(feature = "teleport")]
			Op::Teleport(_)    => self,
			
			Op::GoTo(addr)     => Op::ComeFrom(addr),
			Op::ComeFrom(addr) => Op::GoTo(addr),
			Op::IO(r, p)       => Op::IO(r, p),
		}
	}
	
	fn mneu_usage(s: &str) -> &'static str {
		match s {
			"hlt" | "nop" | "dbg" =>
				"no arguments taken",
			
			"not" | "neg" | "spc" | "rspc" =>
				"<register>",
			
			#[cfg(feature = "xor-pc")]
			"xpc" =>
				"<register>",
			
			"swp" | "xchg" | "xor" | "add" | "sub" | "rol" | "ror" =>
				"<register> <register>",
			
			"ccn" | "cswp" =>
				"<register> <register> <register>",
			
			"xori" | "addi" | "subi" | "roli" | "rori" =>
				"<register> <8-bit unsigned int>",
			
			"jpo" | "apo" | "js" | "as" | "jpe" | "ape" | "jns"
			| "ans" =>
				"<register> <label or 10-bit address>",
			
			"jmp" | "pmj" =>
				"<label or 14-bit address>",
			
			#[cfg(feature = "teleport")]
			"tp" =>
				"<label or 14-bit address>",
			
			"io" =>
			    "<register> <port number>",
			
			_ => unreachable!()
		}
	}
}

impl fmt::Display for Op {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Op::Halt  => f.write_str("hlt"),
			Op::Nop   => f.write_str("nop"),
			Op::Debug => f.write_str("dbg"),
			
			Op::Not(r)       => write!(f, "not {}", r),
			Op::Negate(r)    => write!(f, "neg {}", r),
			#[cfg(feature = "xor-pc")]
			Op::XorPc(r)     => write!(f, "xpc {}", r),
			Op::SwapPc(r)    => write!(f, "spc {}", r),
			Op::RevSwapPc(r) => write!(f, "rspc {}", r),
			
			Op::Swap(rl, rr)     => write!(f, "swp {} {}", rl, rr),
			Op::Exchange(rr, ra) => write!(f, "xchg {} {}", rr, ra),
			Op::Xor(rn, rc)      => write!(f, "xor {} {}", rn, rc),
			Op::Add(ra, rc)      => write!(f, "add {} {}", ra, rc),
			Op::Sub(rs, rc)      => write!(f, "sub {} {}", rs, rc),
			Op::LRot(rr, ro)     => write!(f, "rol {} {}", rr, ro),
			Op::RRot(rr, ro)     => write!(f, "ror {} {}", rr, ro),
			
			Op::XorImm(r, v)  => write!(f, "xori {} {}", r, v),
			Op::AddImm(r, v)  => write!(f, "addi {} {}", r, v),
			Op::SubImm(r, v)  => write!(f, "subi {} {}", r, v),
			Op::LRotImm(r, v) => write!(f, "roli {} {}", r, v),
			Op::RRotImm(r, v) => write!(f, "rori {} {}", r, v),
			Op::IO(r, v)      => write!(f, "io {} {}", r, v),
			
			Op::CCNot(rc0, rc1, rn) => write!(f, "ccn {} {} {}", rc0, rc1, rn),
			Op::CSwap(rc, rs0, rs1) => write!(f, "cswp {} {} {}", rc, rs0, rs1),
			
			Op::BranchOdd(r, ref a)    => write!(f, "jpo {} {}", r, a),
			Op::AssertOdd(r, ref a)    => write!(f, "apo {} {}", r, a),
			Op::BranchNeg(r, ref a)    => write!(f, "js {} {}", r, a),
			Op::AssertNeg(r, ref a)    => write!(f, "as {} {}", r, a),
			Op::BranchEven(r, ref a)   => write!(f, "jpe {} {}", r, a),
			Op::AssertEven(r, ref a)   => write!(f, "ape {} {}", r, a),
			Op::BranchNotNeg(r, ref a) => write!(f, "jns {} {}", r, a),
			Op::AssertNotNeg(r, ref a) => write!(f, "ans {} {}", r, a),
			
			#[cfg(feature = "teleport")]
			Op::Teleport(addr) => write!(f, "tp {}", addr),
			Op::GoTo(addr)     => write!(f, "jmp {}", addr),
			Op::ComeFrom(addr) => write!(f, "pmj {}", addr),
		}
	}
}

impl FromStr for Op {
	type Err = ParseOpError;
	
	fn from_str(s: &str) -> Result<Self> {
		use self::ParseOpError as OpError;
		
		let mut tokens = s.split_whitespace();
		
		// Parses a register literal. Returns early if an error is
		// found.
		macro_rules! reg(() => {
			tokens.next()
			.ok_or(OpError::NoToken(Type::Register))?
			.parse::<Reg>()?
		});
		
		// Parses a token as type $t with max value $max.
		macro_rules! val(($t:ty, $max:expr) => {{
			let val = tokens.next()
			.ok_or(OpError::NoToken(Type::Constant))?
			.parse::<$t>()?;
			
			if val as usize <= $max {
				val
			} else {
				return Err(OpError::ValueOverflow($max));
			}
		}});
		
		macro_rules! addr(($max:expr) => {{
			let addr_tok = tokens.next()
			.ok_or(OpError::NoToken(Type::Address))?;
			
			let first = addr_tok.chars().nth(0).unwrap();
			let is_alpha = |c: char| c.is_alphabetic() || c == '_';
			let is_alphanum = |c| is_alpha(c) || c.is_digit(10);
			
			if is_alpha(first) && addr_tok.chars().skip(1).all(is_alphanum) {
				Addr::Label(addr_tok.to_string())
			}
			// only decimal numbers currently supported
			else if addr_tok.chars().all(|c| c.is_digit(10)) {
				let value = addr_tok.parse::<usize>()?;
				
				if value <= $max {
					Addr::Offset(value)
				} else {
					return Err(OpError::ValueOverflow($max));
				}
			}
			else {
				return Err(OpError::BadToken(Type::Address, addr_tok.to_owned()));
			}
		}});
		
		let mneu = tokens.next()
		.ok_or(OpError::NoToken(Type::Mneumonic))?;
		
		let op = match mneu {
			"hlt" => Op::Halt,
			"nop" => Op::Nop,
			"dbg" => Op::Debug,
			
			"not"   => Op::Not(reg!()),
			"neg"   => Op::Negate(reg!()),
			#[cfg(feature = "xor-pc")]
			"xpc"   => Op::XorPc(reg!()),
			"spc"   => Op::SwapPc(reg!()),
			"rspc"  => Op::RevSwapPc(reg!()),
						
			"swp"  => Op::Swap(reg!(), reg!()),
			"xchg" => Op::Exchange(reg!(), reg!()),
			"xor"  => Op::Xor(reg!(), reg!()),
			"add"  => Op::Add(reg!(), reg!()),
			"sub"  => Op::Sub(reg!(), reg!()),
			"rol"  => Op::LRot(reg!(), reg!()),
			"ror"  => Op::RRot(reg!(), reg!()),
			
			"xori" => Op::XorImm(reg!(), val!(u8, 0xFF)),
			"addi" => Op::AddImm(reg!(), val!(u8, 0xFF)),
			"subi" => Op::SubImm(reg!(), val!(u8, 0xFF)),
			"roli" => Op::LRotImm(reg!(), val!(u8, 0xFF)),
			"rori" => Op::RRotImm(reg!(), val!(u8, 0xFF)),
			"io"   => Op::IO(reg!(), val!(u8, 0xFF)),
			
			"ccn"  => Op::CCNot(reg!(), reg!(), reg!()),
			"cswp" => Op::CSwap(reg!(), reg!(), reg!()),
			
			"jpo" => Op::BranchOdd(reg!(), addr!(0x01FF)),
			"apo" => Op::AssertOdd(reg!(), addr!(0x01FF)),
			"js"  => Op::BranchNeg(reg!(), addr!(0x01FF)),
			"as"  => Op::AssertNeg(reg!(), addr!(0x01FF)),
			"jpe" => Op::BranchEven(reg!(), addr!(0x01FF)),
			"ape" => Op::AssertEven(reg!(), addr!(0x01FF)),
			"jns" => Op::BranchNotNeg(reg!(), addr!(0x01FF)),
			"ans" => Op::AssertNotNeg(reg!(), addr!(0x01FF)),
			
			#[cfg(feature = "teleport")]
			"tp"  => Op::Teleport(addr!(0x1FFF)),
			"jmp" => Op::GoTo(addr!(0x1FFF)),
			"pmj" => Op::ComeFrom(addr!(0x1FFF)),
			
			_ => return Err(OpError::BadToken(Type::Mneumonic, mneu.to_owned()))
		};
		
		// check to make sure all tokens are exhausted.
		if tokens.count() == 0 {
			Ok(op)
		} else {
			Err(OpError::ExtraToken)
		}
	}
}

#[cfg(test)]
mod tests {
	use std::str::FromStr;
	use super::{Op, Addr};
	use super::super::reg::Reg;
	
	#[test]
	fn instruction_encoding() {
		// Make a vector with all parameters initialized
		let ops = vec![
			Op::Halt,
			Op::Nop,
			Op::Debug,
			Op::Not(Reg::R6),
			Op::Negate(Reg::R6),
			Op::SwapPc(Reg::R6),
			Op::RevSwapPc(Reg::R6),
			Op::Swap(Reg::R6, Reg::R6),
			Op::Exchange(Reg::R6, Reg::R6),
			Op::Xor(Reg::R6, Reg::R6),
			Op::Add(Reg::R6, Reg::R6),
			Op::Sub(Reg::R6, Reg::R6),
			Op::LRot(Reg::R6, Reg::R6),
			Op::RRot(Reg::R6, Reg::R6),
			Op::XorImm(Reg::R6, 0xFF),
			Op::AddImm(Reg::R6, 0xFF),
			Op::SubImm(Reg::R6, 0xFF),
			Op::IO(Reg::R6, 0xFF),
			Op::LRotImm(Reg::R6, 0xF),
			Op::RRotImm(Reg::R6, 0xF),
			Op::CCNot(Reg::R6, Reg::R6, Reg::R6),
			Op::CSwap(Reg::R6, Reg::R6, Reg::R6),
			Op::BranchOdd(Reg::R6, Addr::Label("hey".to_string())),
			Op::AssertOdd(Reg::R6, Addr::Label("hi".to_string())),
			Op::BranchNeg(Reg::R6, Addr::Label("hello".to_string())),
			Op::AssertNeg(Reg::R6, Addr::Label("hiya".to_string())),
			Op::BranchEven(Reg::R6, Addr::Label("hey".to_string())),
			Op::AssertEven(Reg::R6, Addr::Label("hi".to_string())),
			Op::BranchNotNeg(Reg::R6, Addr::Label("hello".to_string())),
			Op::AssertNotNeg(Reg::R6, Addr::Label("hiya".to_string())),
			Op::GoTo(Addr::Label("howdy".to_string())),
			Op::ComeFrom(Addr::Label("sup".to_string())),
		];
		
		for op in ops {
			// String conversion shouldn't error when decoding
			// a valid instruction, so just unwrap it.
			assert_eq!(op, Op::from_str(&op.to_string()).unwrap());
		}
	}
}
