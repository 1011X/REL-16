use std::fmt;
use std::num;
use std::str::FromStr;
use std::error;
use std::result;

use super::reg::{self, Reg};

type Result<T> = result::Result<T, Error>;

/// Tracks address labels to be converted to offsets later.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Addr {
	Label(String),
	Offset(usize),
}

impl fmt::Display for Addr {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Addr::Label(ref label) => write!(f, "{}", label),
			Addr::Offset(offset)   => write!(f, "{}", offset),
		}
	}
}

/// Various things that can go wrong in the process of parsing a string to an
/// `Op` enum.
#[derive(Debug)]
pub enum Error {
	NoMneumonic,
	NoArgument,
	NoAddress,
	ValueTooLarge,
	NoRegister,
	UnknownMneumonic(String),
	ExtraArgs(Vec<String>),
	Value(num::ParseIntError, String),
	Offset(num::ParseIntError, String),
	Register(reg::ParseError),
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Error::NoMneumonic =>
				write!(f, "No mneumonic found."),
			Error::NoArgument =>
				write!(f, "Missing argument for this mneumonic."),
			Error::NoAddress =>
				write!(f, "No label or offset found."),
			Error::ValueTooLarge =>
				write!(f, "Value is bigger than the maximum value."),
			Error::NoRegister =>
				write!(f, "Expected a register literal."),
			
			Error::UnknownMneumonic(ref m) =>
				write!(f, "Did not recognize mneumonic: {}", m),
			Error::ExtraArgs(ref v) => 
				write!(f, "Found these extra tokens at the end: {:?}", v),
			Error::Value(ref e, ref s) =>
				write!(f, "Error parsing integer, got: \"{}\", because {}", s, e),
			Error::Offset(ref e, ref s) =>
				write!(f, "Error parsing offset, got: \"{}\", because {}", s, e),
			Error::Register(ref e) =>
				write!(f, "Error parsing register literal: {}", e),
		}
	}
}

impl error::Error for Error {
	fn description(&self) -> &str {
		match *self {
			Error::NoMneumonic          => "missing mneumonic",
			Error::NoArgument           => "missing argument",
			Error::NoAddress            => "missing address",
			Error::ValueTooLarge        => "argument value is too big",
			Error::NoRegister           => "missing register literal",
			Error::UnknownMneumonic(..) => "unknown mneumonic",
			Error::ExtraArgs(..)        => "found extra tokens",
			Error::Value(ref e, _)      => e.description(),
			Error::Offset(ref e, _)     => e.description(),
			Error::Register(ref e)      => e.description(),
		}
	}
	
	fn cause(&self) -> Option<&error::Error> {
		match *self {
			Error::Value(ref e, _)  => Some(e),
			Error::Offset(ref e, _) => Some(e),
			Error::Register(ref e)  => Some(e),
			_ => None
		}
	}
}

/**
High-level machine instruction representation

This enum represents all valid instructions used in the REL-16 architecture. It
allows for simpler processing compared to string interpretation.

It's required that each variant here has a reversible equivalent. That is, if
the instruction can't undo its own actions, it must have a partner instruction
that can. By tying each instruction with its opposite, we can guarantee the
reversibility property of the architecture.

Being a 16-bit architecture, we must organize the bits so all instructions are
representable within 16 bits. To ensure this, each instruction is given a
16-character wide field where each character represents how a specified bit is
used. The bit-field key is as follows:

* `_`: leading zero; used to organize instructions
* `0`/`1`: bit literal
* `o`: sub-opcode value field; its size is important to know how many
  instructions can fit
* `r`/`R`: register index field
* `v`: immediate value field

*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
	/// Stops the machine.
	///
	/// Format: `0000 0000 0000 0000`
	Halt,
	
	/// Does absolutely nothing.
	///
	/// Format: `0000 0000 0000 0001`
	Nop,
	
	/// Flips every bit in the register.
	///
	/// Format: `____ ____ 1oooorrr`
	Not(Reg),
	
	/// Turns the given register's value into its two's complement.
	///
	/// Format: `____ ____ 1oooorrr`
	Negate(Reg),
	
	/// Adds 1 to the register's value, wrapping around on overflow.
	///
	/// Format: `____ ____ 1oooorrr`
	Increment(Reg),
	
	/// Subtracts 1 from the register's value, wrapping around on underflow.
	///
	/// Format: `____ ____ 1oooorrr`
	Decrement(Reg),
	
	/// Decrements the stack pointer, then swaps the register's value with the
	/// value pointed to in memory by the stack pointer.
	/// 
	/// The register's new value should be zero, assuming no values were leaked
	/// into memory.
	///
	/// Format: `____ ____ 1oooorrr`
	Push(Reg),
	
	/// Swaps the register's value with the value at the stack pointer, then
	/// increments the stack pointer.
	/// 
	/// Register value should be zero before this operation is performed, so
	/// that the memory stays cleared.
	///
	/// Format: `____ ____ 1oooorrr`
	Pop(Reg),
	
	/// Swaps the register and the program counter. Used for calling functions.
	///
	/// Format: `____ ____ 1oooorrr`
	SwapPc(Reg),
	
	/// Flips direction bit, then swaps the register and the program counter.
	/// Used when **un**calling functions.
	///
	/// Format: `____ ____ 1oooorrr`
	RevSwapPc(Reg),
	
	/// Signed multiplication by 2.
	/// 
	/// If the register's value *r* is less than -16384, you'll get: 2*r* - MIN
	/// + 1
	/// 
	/// If the value is greater than 16383, you'll get: 2*r* - MAX
	/// 
	/// Otherwise, it's multiplied by 2 as expected.
	/// 
	/// Format: `____ ____ 1oooorrr`
	Mul2(Reg),
	
	/// Signed division by 2.
	/// 
	/// If the register's value *r* is odd and positive (like me), you'll get:
	/// (*r* + MAX) / 2
	/// 
	/// If it's odd and negative, you'll get: (*r* + MIN - 1) / 2
	/// 
	/// Otherwise (when it's even), it's divided by 2 as expected.
	///
	/// Format: `____ ____ 1oooorrr`
	Div2(Reg),
	
	/// Moves ("rotates") the register's bits to the left by the given amount,
	/// moving the last bit's value to the first bit.
	///
	/// Format: `____ ___1 orrrvvvv`
	RotLeftImm(Reg, u8),
	
	/// Moves ("rotates") the register's bits to the right by the given amount,
	/// moving the first bit's value to the last bit.
	///
	/// Format: `____ ___1 orrrvvvv`
	RotRightImm(Reg, u8),
	
	/// Swaps the registers' values.
	///
	/// Format: `____ __1oooRRRrrr`
	Swap(Reg, Reg),
	
	/// Flips bits in the first register based on bits in the second register.
	/// Exactly like 8086's `xor` instruction.
	///
	/// Format: `____ __1ooossssss`
	CNot(u8),
	
	/// Adds/increases first register's value by second register's value.
	///
	/// Format: `____ __1ooossssss`
	CAdd(u8),
	
	/// Subtracts/decreases first register's value by second register's value.
	///
	/// Format: `____ __1ooossssss`
	CSub(u8),
	
	/// Rotates the first register's bits to the left by the value in the
	/// second register.
	///
	/// Format: `____ __1oooRRRrrr`
	RotLeft(Reg, Reg),
	
	/// Rotates the first register's bits to the right by the value in the
	/// second register.
	///
	/// Format: `____ __1oooRRRrrr`
	RotRight(Reg, Reg),
	
	/// Swaps the first register's value with the value pointed to in memory by
	/// the second register.
	///
	/// Format: `____ __1oooRRRrrr`
	Exchange(Reg, Reg),
	
	/// Toffoli gate; ANDs first and second registers and flips the bits in the
	/// third register based on the result.
	/// 
	/// If a register is in the first or second position here, it *should not*
	/// be in the third position, and vice-versa.
	///
	/// Format: `____ _1orrrRRRrrr`
	CCNot(Reg, Reg, Reg),
	
	/// Fredkin gate; swaps bits in second and third registers based on bits in
	/// the first register.
	/// 
	/// If a register is in the first position, it *should not* be in the
	/// second or third position, and vice-versa.
	///
	/// Format: `____ _1orrrRRRrrr`
	CSwap(Reg, Reg, Reg),
	
	/// Flips bits in the register's lower half based on the bits of the
	/// immediate byte value.
	/// 
	/// This is usually used to set the register to the given value or to reset
	/// its value to zero.
	///
	/// Format: `____ 1rrr vvvvvvvv`
	Immediate(Reg, u8),
	
	/// Adds an immediate 14-bit value to the branch register. It is used to
	/// teleport unconditionally to another instruction. To avoid wonky
	/// behavior, it's recommended the destination instruction be a ComeFrom
	/// instruction, so that it goes back to processing the next immediate
	/// instruction, rather than executing every nth instruction.
	///
	/// Format: `_1vvvvvvvvvvvvvv`
	GoTo(Addr),
	
	/// Subtracts an immediate 14-bit value from the branch register. It is
	/// used to teleport backwards in the code to another instruction. To avoid
	/// wonky behavior, it's recommended the destination instruction be a GoTo
	/// instruction, so that it goes back to processing the next immediate
	/// instruction, rather than executing every nth instruction. 
	///
	/// Format: `_1vvvvvvvvvvvvvv`
	ComeFrom(Addr),
	
	/// Adds an offset to the branch register if the register is an odd number.
	///
	/// Format: `1oorrrvvvvvvvvvv`
	BranchParityOdd(Reg, Addr),
	
	/// Subtracts an offset from the branch register if the the register is an
	/// odd number.
	///
	/// Format: `1oorrrvvvvvvvvvv`
	AssertParityOdd(Reg, Addr),
	
	/// Adds an offset to the branch register if the register is negative.
	///
	/// Format: `1oorrrvvvvvvvvvv`
	BranchSignNegative(Reg, Addr),
	
	/// Subtracts an offset from the branch register if the register is
	/// negative.
	///
	/// Format: `1oorrrvvvvvvvvvv`
	AssertSignNegative(Reg, Addr),
	
	/// Adds an offset to the branch register if the register is an even
	/// number.
	///
	/// Format: `1oorrrvvvvvvvvvv`
	BranchParityEven(Reg, Addr),
	
	/// Subtracts an offset from the branch register if the register is an even
	/// number.
	///
	/// Format: `1oorrrvvvvvvvvvv`
	AssertParityEven(Reg, Addr),
	
	/// Adds an offset to the branch register if the register is not negative.
	///
	/// Format: `1oorrrvvvvvvvvvv`
	BranchSignNonneg(Reg, Addr),
	
	/// Subtracts an offset from the branch register if the register is not
	/// negative.
	///
	/// Format: `1oorrrvvvvvvvvvv`
	AssertSignNonneg(Reg, Addr),
}

impl Op {
	// To guarantee VM is reversible, every operation must have an inverse.
	// Involutory instructions (which are their own inverse) return self.
	pub fn invert(self) -> Op {
		match self {
			Op::Halt               => self,
			Op::Nop                => self,
			Op::Not(..)            => self,
			Op::Negate(..)         => self,
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
			
			Op::BranchParityOdd(r, a)    => Op::AssertParityOdd(r, a),
			Op::BranchSignNegative(r, a) => Op::AssertSignNegative(r, a),
			Op::AssertParityOdd(r, a)    => Op::BranchParityOdd(r, a),
			Op::AssertSignNegative(r, a) => Op::BranchSignNegative(r, a),
			Op::BranchParityEven(r, a)   => Op::AssertParityEven(r, a),
			Op::BranchSignNonneg(r, a)   => Op::AssertSignNonneg(r, a),
			Op::AssertParityEven(r, a)   => Op::BranchParityEven(r, a),
			Op::AssertSignNonneg(r, a)   => Op::BranchSignNonneg(r, a),
			Op::GoTo(addr)               => Op::ComeFrom(addr),
			Op::ComeFrom(addr)           => Op::GoTo(addr),
		}
	}
}

impl fmt::Display for Op {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Op::Halt => write!(f, "hlt"),
			Op::Nop => write!(f, "nop"),
			
			Op::Not(r)       => write!(f, "not {}", r),
			Op::Negate(r)    => write!(f, "neg {}", r),
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
			
			Op::BranchParityOdd(r, ref a) => write!(f, "jpo {} {}", r, a),
			Op::AssertParityOdd(r, ref a) => write!(f, "apo {} {}", r, a),
			Op::BranchSignNegative(r, ref a)   => write!(f, "js {} {}", r, a),
			Op::AssertSignNegative(r, ref a)   => write!(f, "as {} {}", r, a),
			
			Op::BranchParityEven(r, ref a) => write!(f, "jpe {} {}", r, a),
			Op::AssertParityEven(r, ref a) => write!(f, "ape {} {}", r, a),
			Op::BranchSignNonneg(r, ref a)   => write!(f, "jns {} {}", r, a),
			Op::AssertSignNonneg(r, ref a)   => write!(f, "ans {} {}", r, a),
			
			Op::GoTo(ref addr)     => write!(f, "jmp {}", addr),
			Op::ComeFrom(ref addr) => write!(f, "pmj {}", addr),
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
				Err(e) => Err(Error::Value(e, token.to_string())),
			})?
		});
		
		macro_rules! addr(($max:expr) => {
			tokens.next()
			.ok_or(Error::NoAddress)
			.and_then(|tok| {
				// TODO: handle unwrap
				/*
				let first = tok.chars().nth(0).unwrap();
				let is_alpha = |c: char| c.is_alphabetic() || c == '_';
				let is_alphanum = |c| is_alpha(c) || c.is_digit(10);
				
				if is_alpha(first) && tok.chars().skip(1).all(is_alphanum) {
					Ok(Addr::Label(tok.to_string()))
				}
				else if tok.chars().all(|c| c.is_digit(10)) {
					match tok.parse::<usize>() {
						Ok(value) if value <= $max =>
							Ok(Addr::Offset(value)),

						Ok(_)  => Err(Error::ValueTooLarge),
						Err(e) => Err(Error::Value(e)),
					}
				}
				else {
					Err(Error::NoAddress)
				}*/
				match tok.parse::<usize>() {
					Ok(value) if value <= $max =>
						Ok(Addr::Offset(value)),

					Ok(_)  => Err(Error::ValueTooLarge),
					Err(e) => Err(Error::Offset(e, tok.to_string())),
				}
			})?
		});
		
		use std::u8;
		
		let mneu = tokens.next().ok_or(Error::NoMneumonic)?;
		let op = match mneu {
			"hlt" => Op::Halt,
			"nop" => Op::Nop,
			
			"not"  => Op::Not(reg!()),
			"neg"  => Op::Negate(reg!()),
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
			
			"jpo" => Op::BranchParityOdd(reg!(), addr!(u8::MAX as usize)),
			"apo" => Op::AssertParityOdd(reg!(), addr!(u8::MAX as usize)),
			"js" => Op::BranchSignNegative(reg!(), addr!(u8::MAX as usize)),
			"as" => Op::AssertSignNegative(reg!(), addr!(u8::MAX as usize)),
			
			"jpe" => Op::BranchParityEven(reg!(), addr!(u8::MAX as usize)),
			"ape" => Op::AssertParityEven(reg!(), addr!(u8::MAX as usize)),
			"jns" => Op::BranchSignNonneg(reg!(), addr!(u8::MAX as usize)),
			"ans" => Op::AssertSignNonneg(reg!(), addr!(u8::MAX as usize)),
			
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
	use std::str::FromStr;
	use super::{Op, Addr};
	use super::super::reg::Reg;
	
	#[test]
	fn instruction_encoding() {
		// Make a vector with all parameters initialized to 1s
		let ops = vec![
			Op::Halt,
			Op::Nop,
			Op::Not(Reg::BP),
			Op::Negate(Reg::BP),
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
			Op::CCNot(Reg::BP, Reg::BP, Reg::BP),
			Op::CSwap(Reg::BP, Reg::BP, Reg::BP),
			Op::Immediate(Reg::BP, 0xFF),
			Op::BranchParityOdd(Reg::BP, Addr::Label("hey".to_string())),
			Op::AssertParityOdd(Reg::BP, Addr::Label("hi".to_string())),
			Op::BranchSignNegative(Reg::BP, Addr::Label("hello".to_string())),
			Op::AssertSignNegative(Reg::BP, Addr::Label("hiya".to_string())),
			Op::BranchParityEven(Reg::BP, Addr::Label("hey".to_string())),
			Op::AssertParityEven(Reg::BP, Addr::Label("hi".to_string())),
			Op::BranchSignNonneg(Reg::BP, Addr::Label("hello".to_string())),
			Op::AssertSignNonneg(Reg::BP, Addr::Label("hiya".to_string())),
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
