use crate::isa::reg::{self, Reg};

pub type Addr = usize;
type RegMut = Reg;
type RegUnused = Reg;

#[derive(Debug)]
pub struct EncodeError;

#[derive(Debug)]
pub struct DecodeError;

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
  * Not: func = 0x00
  * Xor: func = 0x01
  * Swap: func = 0x02
  * Exchange: func = 0x03
  * Add: func = 0x04
  * Sub: func = 0x05
  * LRot: func = 0x06
  * RRot: func = 0x07
  * Swap-PC: func = 0x08
  * Rev-Swap-PC: func = 0x09
* Triple registers (op = 0x06): 3 registers
  * CCNot: func = 0x0
  * CSwap: func = 0x1
* Jump: 11-bit value
  * jump (op = 0x07)
  * halt (op = 0x1F)
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instr {
	/// Stops the machine. Contains an optional error code.
	Halt(u16),
	
	/// Flips every bit in the register.
	Not(RegMut, RegUnused),
	
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
	SwapPc(RegMut, RegUnused),
	
	/// Flips direction bit, then swaps the given register and the program counter. Used
	/// when **un**calling functions.
	RevSwapPc(RegMut, RegUnused),
	
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
			
			Instr::SwapPc(r, x)    => Instr::RevSwapPc(r, x),
			Instr::RevSwapPc(r, x) => Instr::SwapPc(r, x),
			
			Instr::IO(..) => self,
		}
	}
	
	pub fn encode(self) -> Result<u16, EncodeError> {
		match self {
			// jump format
			Instr::Halt(val) if val > 0x7FF => Err(EncodeError),
			Instr::Jump(val) if val > 0x7FF => Err(EncodeError),
			
			Instr::Halt(code) => Ok((code as u16) << 5 | 0x1F),
			Instr::Jump(addr) => Ok((addr as u16) << 5 | 0x08),
			
			// immediate format
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
			
			// double register format
			Instr::Not(reg, _) => Ok(
				0 << 11
				| (reg as u16) << 8
				| 0 << 5
				| 0x05
			),
			Instr::Xor(reg_a, reg_b) => Ok(
				1 << 11
				| (reg_a as u16) << 8
				| (reg_b as u16) << 5
				| 0x05
			),
			Instr::Swap(reg_a, reg_b) => Ok(
				2 << 11
				| (reg_a as u16) << 8
				| (reg_b as u16) << 5
				| 0x05
			),
			Instr::Exchange(reg_a, reg_b) => Ok(
				3 << 11
				| (reg_a as u16) << 8
				| (reg_b as u16) << 5
				| 0x05
			),
			Instr::Add(reg_a, reg_b) => Ok(
				4 << 11
				| (reg_a as u16) << 8
				| (reg_b as u16) << 5
				| 0x05
			),
			Instr::Sub(reg_a, reg_b) => Ok(
				5 << 11
				| (reg_a as u16) << 8
				| (reg_b as u16) << 5
				| 0x05
			),
			Instr::LRot(reg_a, reg_b) => Ok(
				6 << 11
				| (reg_a as u16) << 8
				| (reg_b as u16) << 5
				| 0x05
			),
			Instr::RRot(reg_a, reg_b) => Ok(
				7 << 11
				| (reg_a as u16) << 8
				| (reg_b as u16) << 5
				| 0x05
			),
			Instr::SwapPc(reg, _) => Ok(
				8 << 11
				| (reg as u16) << 8
				| 0 << 5
				| 0x05
			),
			Instr::RevSwapPc(reg, _) => Ok(
				9 << 11
				| (reg as u16) << 8
				| 0 << 5
				| 0x05
			),
			
			// triple register format
			Instr::CCNot(rr, rb, rw) => Ok(
				0 << 14
				| (rr as u16) << 11
				| (rw as u16) << 8
				| (rb as u16) << 5
				| 0x06
			),
			Instr::CSwap(rr, rb, rw) => Ok(
				1 << 14
				| (rr as u16) << 11
				| (rw as u16) << 8
				| (rb as u16) << 5
				| 0x06
			),
		}
	}
	
	pub fn decode(instr: u16) -> Result<Self, DecodeError> {
		match instr & 0x1F {
			// halt
			0x1F => {
				let value = instr >> 5;
				Ok(Instr::Halt(value))
			}
			
			// direct jump
			0x07 => {
				let offset = instr >> 5;
				Ok(Instr::Jump(offset as usize))
			}
			
			// low immediate
			0x00 => {
				let reg = ((instr >> 5) & 0x7) as usize;
				let imm = instr >> 8;
				Ok(Instr::ImmLow(reg::ALL_REGISTERS[reg], imm as u8))
			}
			
			// high immediate
			0x01 => {
				let reg = ((instr >> 5) & 0x7) as usize;
				let imm = instr >> 8;
				Ok(Instr::ImmHigh(reg::ALL_REGISTERS[reg], imm as u8))
			}
			
			// jump if odd
			0x02 => {
				let reg = ((instr >> 5) & 0x7) as usize;
				let imm = instr >> 8;
				Ok(Instr::JumpOdd(reg::ALL_REGISTERS[reg], imm as usize))
			}
			
			// jump if negative
			0x03 => {
				let reg = ((instr >> 5) & 0x7) as usize;
				let imm = instr >> 8;
				Ok(Instr::JumpNeg(reg::ALL_REGISTERS[reg], imm as usize))
			}
			
			// io port exchange
			0x04 => {
				let reg = ((instr >> 5) & 0x7) as usize;
				let imm = (instr >> 8) as u8;
				Ok(Instr::IO(reg::ALL_REGISTERS[reg], imm))
			}
			
			// double register format
			0x05 => {
				let rb = reg::ALL_REGISTERS[((instr >> 5) & 0x7) as usize];
				let rw = reg::ALL_REGISTERS[((instr >> 8) & 0x7) as usize];
				
				match instr >> 11 {
					0x00 => Ok(Instr::Not(rb, rw)),
					0x01 => Ok(Instr::Xor(rw, rb)),
					0x02 => Ok(Instr::Swap(rw, rb)),
					0x03 => Ok(Instr::Exchange(rw, rb)),
					0x04 => Ok(Instr::Add(rw, rb)),
					0x05 => Ok(Instr::Sub(rw, rb)),
					0x06 => Ok(Instr::LRot(rw, rb)),
					0x07 => Ok(Instr::RRot(rw, rb)),
					0x08 => Ok(Instr::SwapPc(rw, rb)),
					0x09 => Ok(Instr::RevSwapPc(rw, rb)),
					
					0x00 ..= 0x1F => unimplemented!(),
					_ => unreachable!()
				}
			}
			
			// triple register format
			0x06 => {
				let rb = reg::ALL_REGISTERS[((instr >> 5) & 0x7) as usize];
				let rw = reg::ALL_REGISTERS[((instr >> 8) & 0x7) as usize];
				let rr = reg::ALL_REGISTERS[((instr >> 11) & 0x7) as usize];
				
				match instr >> 14 {
					0x0 => Ok(Instr::CCNot(rr, rb, rw)),
					0x1 => Ok(Instr::CSwap(rr, rb, rw)),
					
					0x0 ..= 0x3 => unimplemented!(),
					_ => unreachable!()
				}
			}
			
			0x00 ..= 0x1F => unimplemented!(),
			_ => unreachable!(),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::Instr;
	
	#[test]
	fn instruction_encoding() {
		// Make a vector with all parameters initialized
		use super::Reg::*;
		let ops = &[
			Instr::Not(R6, R6),
			Instr::Xor(R6, R6),
			Instr::Swap(R6, R6),
			Instr::Exchange(R6, R6),
			Instr::Add(R6, R6),
			Instr::Sub(R6, R6),
			Instr::LRot(R6, R6),
			Instr::RRot(R6, R6),
			Instr::SwapPc(R6, R6),
			Instr::RevSwapPc(R6, R6),
			
			Instr::CCNot(R6, R6, R6),
			Instr::CSwap(R6, R6, R6),
			
			Instr::ImmLow(R6, 0xFF),
			Instr::ImmHigh(R6, 0xFF),
			Instr::JumpOdd(R6, 0xFF),
			Instr::JumpNeg(R6, 0xFF),
			Instr::IO(R6, 0xFF),
			
			Instr::Jump(0x7FF),
			Instr::Halt(0x7FF),
		];
		
		for op in ops {
			assert_eq!(op, &Instr::decode(op.encode().unwrap()).unwrap());
		}
	}
	
	#[test]
	#[should_panic]
	fn invalid_instructions() {
		//todo!();
	}
}
