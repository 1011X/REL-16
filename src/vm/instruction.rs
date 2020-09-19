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
* Register (3): 1-3 registers
  * Single (3): 1 register
  * Double (7): 2 registers
    * Swap: 2 mut regs
    * Controlled-op (6): 1 mut reg, 1 const reg
  * Triple (2): 3 registers
    * CCNot: 2 const regs, 1 mut reg
    * CSwap: 1 const reg, 2 mut regs
* Immediate (5): register, 8-bit value
  * low immediate
  * high immediate
  * jump-odd
  * jump-negative
  * I/O ports
* Jump (2): 11-bit value
  * jump
  * halt / jump-void / jump-eternal / bail / see-ya
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
			Instr::Jump(addr) => Ok((addr as u16) << 5 | 0x1E),
			
			Instr::ImmLow(reg, imm) => Ok(
				(imm as u16) << 8
				| (reg as u16) << 5
				| 0x02
			),
			Instr::ImmHigh(reg, imm) => Ok(
				(imm as u16) << 8
				| (reg as u16) << 5
				| 0x03
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
