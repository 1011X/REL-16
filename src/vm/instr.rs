use std::convert::From;
use std::error::Error;

use crate::isa::Reg;

// for semantic purposes.
type RegMut = Reg;

/**
High-level machine instruction representation

This enum represents all valid instructions used in the REL-16 architecture. It
allows for simpler processing compared to string interpretation.

It's required that each variant here has a reversible equivalent. That is, if
the instruction can't undo its own actions, it must have a partner instruction
that can. By tying each instruction with its opposite, we can guarantee the
reversibility property of the architecture.

Being a 16-bit architecture, we must organize the bits so all instructions are
representable within 16 bits. There are some addressing modes:

* Signal (2): nothing
* Register (3): 1-3 registers
  * Single (4-5): 1 register
  * Double (7): 2 registers
  * Triple (2): 3 registers, ah ah ah
* Immediate (6): register, 8-bit value
* Branch (4): register, 8-bit address
* Jump (1): 11-bit address
*/
// TODO: decide whether to keep branch instructions using I-form or change them
// to R-form and have a constant offset.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    /// Stops the machine.
    Halt,
    
    /// Can be used for debugging purposes.
    Debug,
    
    /// Flips every bit in the register.
    Not(RegMut),
    
    /// Swaps the register and the program counter. Used for calling
    /// functions.
    SwapPc(RegMut),
    
    /// Flips direction bit, then swaps the register and the program
    /// counter. Used when **un**calling functions.
    RevSwapPc(RegMut),
    
    /// Swaps the registers' values.
    Swap(RegMut, RegMut),
    
    /// Swaps the first register's value with the value pointed to in
    /// memory by the second register.
    Exchange(RegMut, Reg),
    
    /// Flips bits in first register based on bits in second register.
    /// Exactly like x86's `xor` instruction.
    Xor(RegMut, Reg),
    
    /// Adds/increases first register's value by second register's
    /// value.
    Add(RegMut, Reg),
    
    /// Subtracts/decreases the first register's value by the second
    /// register's value.
    Sub(RegMut, Reg),
    
    /// Rotates the first register's bits leftwards by the value in
    /// the second register.
    /// 
    /// Only the lower 4 bits are used.
    LRot(RegMut, Reg),
    
    /// Rotates the first register's bits rightwards by the value in
    /// the second register.
    /// 
    /// Only the lower 4 bits are used.
    RRot(RegMut, Reg),
    
    /// Flips bits in the register's lower half based on the bits of
    /// the immediate byte value.
    /// 
    /// This is usually used to set the register to the given value
    /// or to reset its value to zero.
    XorImm(RegMut, u8),
    
    /// Adds/increases first register's value by the value of the
    /// given immediate.
    AddImm(RegMut, u8),
    
    /// Subtracts/decreases first register's value by the value of the
    /// given immediate.
    SubImm(RegMut, u8),
    
    /// XORs the upper 4 bits of the value onto the register, then rotates the
    /// register left by the amount in the lower 4 bits.
    XorRot(RegMut, u8),
    
    /// Rotates the register right by the amount in the lower 4 bits of the
    /// value, then XORs the upper 4 bits onto the register.
    RotXor(RegMut, u8),
    
    /// Toffoli gate; ANDs first and second registers and flips the
    /// bits in the third register based on the result.
    /// 
    /// If a register is in the first or second position, it *should
    /// not* be in the third position.
    CCNot(Reg, Reg, RegMut),
    
    /// Fredkin gate; swaps bits in second and third registers based
    /// on bits in the first register.
    /// 
    /// If a register is in the first position, it *should not* be in
    /// the second or third position.
    CSwap(Reg, RegMut, RegMut),
    
    /// XORs an immediate value to the branch register.
    Jump(u16),
    
    /// XORs the immediate value onto the branch register if the given register
    /// is odd.
    BranchOdd(Reg, u8),
    
    /// XORs the immediate value onto the branch register if the given register
    /// is even.
    BranchEven(Reg, u8),
    
    /// XORs the immediate value onto the branch register if the given register
    /// has the sign bit (uppermost bit) set.
    BranchSign(Reg, u8),
    
    /// XORs the immediate value onto the branch register if the given register
    /// has the sign bit (uppermost bit) unset.
    BranchUnsign(Reg, u8),
    
    /// Swaps data at given register to the device at the given port.
    ///
    /// Devices can often be used as a way of getting rid of unwanted
    /// garbage, so swapping becomes the preferred method. If one
    /// wishes to keep a copy of the data sent, `xor` it first.
    IO(Reg, u8),
}

impl Instr {
    // To guarantee VM is reversible, every operation must have an
    // inverse. Involutory instructions (which are their own inverse)
    // return self.
    pub fn invert(self) -> Instr {
        match self {
            Instr::Halt          => self,
            Instr::Debug         => self,
            
            Instr::Not(_)        => self,
            Instr::SwapPc(r)     => Instr::RevSwapPc(r),
            Instr::RevSwapPc(r)  => Instr::SwapPc(r),
            
            Instr::Swap(..)      => self,
            Instr::Exchange(..)  => self,
            Instr::Xor(..)       => self,
            Instr::Add(ra, rc)   => Instr::Sub(ra, rc),
            Instr::Sub(rs, rc)   => Instr::Add(rs, rc),
            Instr::LRot(rr, rv)  => Instr::RRot(rr, rv),
            Instr::RRot(rr, rv)  => Instr::LRot(rr, rv),
            
            Instr::XorImm(..)    => self,
            Instr::AddImm(rc, i) => Instr::SubImm(rc, i),
            Instr::SubImm(rc, i) => Instr::AddImm(rc, i),
            
            Instr::XorRot(r, v) => Instr::RotXor(r, v),
            Instr::RotXor(r, v) => Instr::XorRot(r, v),
            
            Instr::CCNot(..) => self,
            Instr::CSwap(..) => self,
            
            Instr::BranchOdd(..)    => self,
            Instr::BranchEven(..)   => self,
            Instr::BranchSign(..)   => self,
            Instr::BranchUnsign(..) => self,
            
            Instr::Jump(..) => self,
            
            Instr::IO(..) => self,
        }
    }
}


#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use super::{Op, Offset};
    use super::super::reg::Reg;
    
    #[test]
    fn instruction_encoding() {
        // Make a vector with all parameters initialized
        let ops = vec![
            Op::Halt,
            Op::Nop,
            Op::Debug,
            Op::Not(Reg::R6),
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
            Op::BranchParity(true, Reg::R6, Offset::Forward(0xFF)),
            Op::BranchParity(true, Reg::R6, Offset::Backward(0xFF)),
            Op::BranchParity(false, Reg::R6, Offset::Forward(0xFF)),
            Op::BranchParity(false, Reg::R6, Offset::Backward(0xFF)),
            Op::BranchSign(true, Reg::R6, Offset::Forward(0xFF)),
            Op::BranchSign(true, Reg::R6, Offset::Backward(0xFF)),
            Op::BranchSign(false, Reg::R6, Offset::Forward(0xFF)),
            Op::BranchSign(false, Reg::R6, Offset::Backward(0xFF)),
            Op::Jump(Offset::Forward(0xFF)),
            Op::Jump(Offset::Backward(0xFF)),
        ];
        
        for op in ops {
            // String conversion shouldn't error when decoding
            // a valid instruction, so just unwrap it.
            assert_eq!(op, Op::from_str(&op.to_string()).unwrap());
        }
    }
}
