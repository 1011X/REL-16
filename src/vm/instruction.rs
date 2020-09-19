use crate::isa::reg::{self, Reg};

pub type Addr = usize;
type RegMut = Reg;

pub struct EncodeError;

/** High-level machine instruction representation

This enum can represent all valid instructions used in the REL-16 architecture. It allows
for simpler processing compared to string interpretation.

It's required that each variant here has a reversible equivalent. That is, if the
instruction can't undo its own actions, it must have a partner instruction that can. By
tying each instruction with its opposite, we can guarantee the reversibility property of
the architecture.

Being a 16-bit architecture, we must organize the bits so all instructions are
representable within 16 bits. There are some addressing modes:
* Immediate: register, 8-bit value
  * low immediate (op = 0x00)
  * high immediate (op = 0x01)
  * jump-odd (op = 0x02)
  * jump-negative (op = 0x03)
  * I/O ports (op = 0x04)
* Double registers (op = 0x05): 2 registers
  * Xor: func = 0x00
  * Swap: func = 0x01
  * Add: func = 0x02
  * Sub: func = 0x03
  * LRot: func = 0x04
  * RRot: func = 0x05
  * Exchange: func = 0x06
  * Not: func = 0x07
  * Swap-PC: func = 0x08
  * Rev-Swap-PC: func = 0x09
* Triple registers (op = 0x06): 3 registers
  * CCNot: func = 0x0
  * CSwap: func = 0x1
* Jump: 11-bit value
  * jump (op = 0x07)
  * halt (op = 0x1F)
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
	/// Stops the machine. Contains an optional error code.
	Halt(u16),
	
	/// Flips every bit in the register.
	Not(RegMut),
	
	/// Swaps the registers' values.
	Swap(RegMut, RegMut),
	
	/// Flips bits in first register based on bits in second register.
	Xor(RegMut, Reg),
	
	/// Adds/increases first register's value by second register's value.
	Add(RegMut, Reg),
	
	/// Subtracts/decreases the first register's value by the second register's value.
	Sub(RegMut, Reg),
	
	/// Rotates the first register's bits leftwards by the value in the second register.
	/// 
	/// Only the first 4 bits of the argument are used.
	LRot(RegMut, Reg),
	
	/// Rotates the first register's bits rightwards by the value in the second register.
	/// 
	/// Only the first 4 bits of the argument are used.
	RRot(RegMut, Reg),
	
	/// XORs the lower half of the given register with an immediate value.
	ImmLow(RegMut, u8),
	
	/// XORs the higher half of the given register with an immediate value.
	ImmHigh(RegMut, u8),
	
	/// Toffoli gate; ANDs first and second registers and flips the bits in the third
	/// register based on the result.
	/// 
	/// If a register is in the first or second position, it *should not* be in the third
	/// position.
	CCNot(Reg, Reg, RegMut),
	
	/// Fredkin gate; swaps bits in second and third registers based on bits in the first
	/// register.
	/// 
	/// If a register is in the first position, it *should not* be in the second or third
	/// position.
	CSwap(Reg, RegMut, RegMut),
	
	/// Swaps the first register's value with the value pointed to in memory by the second
	/// register.
	Exchange(RegMut, Reg),
	
	/// XORs an immediate value to the branch register.
	Jump(Addr),
	
	/// XORs an offset to the branch register if the given register has an odd number.
	JumpOdd(Reg, Addr),
	
	/// XORs an offset to the branch register if the given register is negative.
	JumpNeg(Reg, Addr),
	
	/// Swaps the given register and the program counter. Used for calling functions.
	SwapPc(RegMut),
	
	/// Flips direction bit, then swaps the given register and the program counter. Used
	/// when **un**calling functions.
	RevSwapPc(RegMut),
	
	/// Swaps data at given register to the device at the given port.
	///
	/// Devices can often be used as a way of getting rid of unwanted garbage, so swapping
	/// becomes the preferred method. If one wishes to keep a copy of the data sent, `xor`
	/// it first.
	IO(RegMut, u8),
}


impl Instr {
	// To guarantee VM is reversible, every operation must have an inverse. Involutory
	// instructions (i.e. instructions which are their own inverse) return self.
	pub fn invert(self) -> Instr {
		match self {
			Instr::Not(..) => self,
			
			Instr::Swap(..)     => self,
			Instr::Xor(..)      => self,
			Instr::Add(ra, rc)  => Instr::Sub(ra, rc),
			Instr::Sub(rs, rc)  => Instr::Add(rs, rc),
			Instr::LRot(rr, rv) => Instr::RRot(rr, rv),
			Instr::RRot(rr, rv) => Instr::LRot(rr, rv),
			
			Instr::ImmLow(..)  => self,
			Instr::ImmHigh(..) => self,
			
			Instr::CCNot(..) => self,
			Instr::CSwap(..) => self,
			
			Instr::Exchange(..) => self,
			
			Instr::Jump(..) => self,
			Instr::Halt(..) => self,
			
			Instr::JumpOdd(..) => self,
			Instr::JumpNeg(..) => self,
			
			Instr::SwapPc(r)    => Instr::RevSwapPc(r),
			Instr::RevSwapPc(r) => Instr::SwapPc(r),
			
			Instr::IO(..) => self,
		}
	}
	
	pub fn encode(self) -> Result<u16, EncodeError> {
		match self {
			Instr::Halt(val) if val > 0x7FF => todo!(),
			Instr::Jump(val) if val > 0x7FF => todo!(),
			
			Instr::Halt(code) => Ok((code as u16) << 5 | 0x1F),
			Instr::Jump(addr) => Ok((addr as u16) << 5 | 0x08),
			
			Instr::ImmLow(reg, imm) => Ok(
				(imm as u16) << 8
				| (reg as u16) << 5
				| 0x00
			),
			Instr::ImmHigh(reg, imm) => Ok(
				(imm as u16) << 8
				| (reg as u16) << 5
				| 0x01
			),
			Instr::JumpOdd(reg, imm) => Ok(
				(imm as u16) << 8
				| (reg as u16) << 5
				| 0x02
			),
			Instr::JumpNeg(reg, imm) => Ok(
				(imm as u16) << 8
				| (reg as u16) << 5
				| 0x03
			),
			Instr::IO(reg, imm) => Ok(
				(imm as u16) << 8
				| (reg as u16) << 5
				| 0x04
			),
			
			_ => todo!(),
		}
	}
}

#[cfg(test)]
mod tests {
	use std::str::FromStr;
	use super::{Instr, Addr};
	use super::super::reg::Reg;
	
	#[test]
	fn instruction_encoding() {
		// Make a vector with all parameters initialized
		let ops = vec![
			Instr::Halt,
			Instr::Nop,
			Instr::Not(Reg::R6),
			Instr::Negate(Reg::R6),
			Instr::SwapPc(Reg::R6),
			Instr::RevSwapPc(Reg::R6),
			Instr::Swap(Reg::R6, Reg::R6),
			Instr::Exchange(Reg::R6, Reg::R6),
			Instr::Xor(Reg::R6, Reg::R6),
			Instr::Add(Reg::R6, Reg::R6),
			Instr::Sub(Reg::R6, Reg::R6),
			Instr::LRot(Reg::R6, Reg::R6),
			Instr::RRot(Reg::R6, Reg::R6),
			Instr::XorImm(Reg::R6, 0xFF),
			Instr::AddImm(Reg::R6, 0xFF),
			Instr::SubImm(Reg::R6, 0xFF),
			Instr::IO(Reg::R6, 0xFF),
			Instr::LRotImm(Reg::R6, 0xF),
			Instr::RRotImm(Reg::R6, 0xF),
			Instr::CCNot(Reg::R6, Reg::R6, Reg::R6),
			Instr::CSwap(Reg::R6, Reg::R6, Reg::R6),
			Instr::BranchOdd(Reg::R6, 0xFFFF),
			Instr::AssertOdd(Reg::R6, 0xFFFF),
			Instr::BranchNeg(Reg::R6, 0xFFFF),
			Instr::AssertNeg(Reg::R6, 0xFFFF),
			Instr::BranchEven(Reg::R6, 0xFFFF),
			Instr::AssertEven(Reg::R6, 0xFFFF),
			Instr::BranchNotNeg(Reg::R6, 0xFFFF),
			Instr::AssertNotNeg(Reg::R6, 0xFFFF),
			Instr::GoTo(0xFFFF),
			Instr::ComeFrom(0xFFFF),
		];
		
		for op in ops {
			// String conversion shouldn't error when decoding
			// a valid instruction, so just unwrap it.
			assert_eq!(op, Instr::from_str(&op.to_string()).unwrap());
		}
	}
	
	#[test]
	#[should_panic]
	fn invalid_instructions() {
		//todo!();
	}
}
