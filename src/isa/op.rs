use std::fmt;
use std::num;
use std::convert::From;
use std::error::Error;
use std::str::FromStr;

use super::reg::{self, Reg};


/// Various things that can go wrong in the process of parsing a string to an
/// `Op` enum.
#[derive(Debug)]
pub enum ParseOpError {
    ExtraToken,
    NoToken,
    BadToken(String),
    ValueOverflow(usize),
    ParseInt(num::ParseIntError),
}

impl fmt::Display for ParseOpError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseOpError::ExtraToken =>
                write!(f, "Found extra token."),
            ParseOpError::NoToken =>
                write!(f, "Missing token"),
            ParseOpError::BadToken(tok) =>
                write!(f, "Bad token: {}", tok),
            ParseOpError::ValueOverflow(max) =>
                write!(f, "Value exceeds maximum of {}", max),
            ParseOpError::ParseInt(pie) =>
                write!(f, "{}", pie),
        }
    }
}

impl Error for ParseOpError {
    fn cause(&self) -> Option<&dyn Error> {
        match self {
            ParseOpError::ParseInt(pie) => Some(pie),
            _ => None
        }
    }
}

impl From<reg::ParseError> for ParseOpError {
    fn from(e: reg::ParseError) -> Self {
        ParseOpError::BadToken(e.0.clone())
    }
}

impl From<num::ParseIntError> for ParseOpError {
    fn from(e: num::ParseIntError) -> Self {
        ParseOpError::ParseInt(e)
    }
}


/// Offset values, and offset labels that can be converted to values
/// later.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Offset {
    #[cfg(feature = "labels")]
    Label(String),
    Forward(usize),
    Backward(usize),
}

impl Offset {
    pub fn invert(self) -> Self {
        match self {
            Offset::Forward(val) => Offset::Backward(val),
            Offset::Backward(val) => Offset::Forward(val),
            #[cfg(feature = "labels")]
            Offset::Label(..) => self,
        }
    }
}

impl fmt::Display for Offset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Offset::Forward(val)  => write!(f, "+{}", val),
            Offset::Backward(val) => write!(f, "-{}", val),
            #[cfg(feature = "labels")]
            Offset::Label(label)  => write!(f, ":{}", label),
        }
    }
}

impl FromStr for Offset {
    type Err = ParseOpError;
    
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match &s[..1] {
            #[cfg(feature = "labels")]
            ":" => {
                let label = &s[1..];
                let head = |c: char| c.is_ascii_alphabetic() || c == '_';
                let tail = |c| head(c) || c.is_digit(10);
                // check that label matches this regex:
                // [A-Za-z][A-Za-z0-9]*
                if label.starts_with(head)
                && label[1..].chars().all(tail) {
                    Ok(Offset::Label(label.to_string()))
                }
                else {
                    Err(ParseOpError::BadToken(s.to_string()))
                }
            }
            
            "+" => Ok(Offset::Forward(s[1..].parse::<u16>()? as usize)),
            "-" => Ok(Offset::Backward(s[1..].parse::<u16>()? as usize)),
            
            _ => Err(ParseOpError::BadToken(s.to_string()))
        }
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
    /// To avoid wonky behavior, the destination should be another
    /// jump instruction with the same offset value but opposite
    /// direction, to avoid skipping instructions.
    Jump(Offset),
    
    /// XORs an immediate value to the PC register.
    /// 
    /// The only restrictions on this instruction are that it be dependent
    /// on code/memory position, and the higher bits of PC can't be reached.
    #[cfg(feature = "teleport")]
    Teleport(u16),
    
    /// Offsets the branch register if the given register has the
    /// given parity value.
    BranchParity(bool, Reg, Offset),
    
    /// Offsets the branch register if the given register has the
    /// given sign value.
    BranchSign(bool, Reg, Offset),
    
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
            
            Op::BranchParity(val, reg, offset) =>
                Op::BranchParity(val, reg, offset.invert()),
            Op::BranchSign(val, reg, offset) =>
                Op::BranchSign(val, reg, offset.invert()),
            
            Op::Jump(offset) => Op::Jump(offset.invert()),
            #[cfg(feature = "teleport")]
            Op::Teleport(_) => self,
            
            Op::IO(..) => self,
        }
    }
    
    pub fn get_label(&self) -> Option<&String> {
        match self {
            #[cfg(feature = "labels")]
            Op::BranchParity(_, _, Offset::Label(label))
            | Op::BranchSign(_, _, Offset::Label(label))
            | Op::Jump(Offset::Label(label)) =>
                Some(label),
            
            _ => None,
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Op::Halt  => f.write_str("halt"),
            Op::Nop   => f.write_str("nop"),
            Op::Debug => f.write_str("dbg"),
            
            Op::Not(r)       => write!(f, "not {}", r),
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
            
            Op::CCNot(rc0, rc1, rn) =>
                write!(f, "ccn {} {} {}", rc0, rc1, rn),
            Op::CSwap(rc, rs0, rs1) =>
                write!(f, "cswp {} {} {}", rc, rs0, rs1),
            
            Op::BranchParity(true, reg, offset) =>
                write!(f, "bo {} {}", reg, offset),
            Op::BranchParity(false, reg, offset) =>
                write!(f, "be {} {}", reg, offset),
            Op::BranchSign(true, reg, offset) =>
                write!(f, "bs {} {}", reg, offset),
            Op::BranchSign(false, reg, offset) =>
                write!(f, "bn {} {}", reg, offset),
            
            #[cfg(feature = "teleport")]
            Op::Teleport(val) => write!(f, "tp {}", val),
            Op::Jump(offset)  => write!(f, "jmp {}", offset),
        }
    }
}

impl FromStr for Op {
    type Err = ParseOpError;
    
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use self::ParseOpError as OpError;
        /*
        enum RegOrImm {
            Reg(Reg),
            Imm(u16),
        }
        
        impl FromStr for RegOrImm {
            type Err = num::ParseIntError;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                Reg::from_str(s)
                .map(RegOrImm::Reg)
                .or_else(|_|
                    u16::from_str(s)
                    .map(RegOrImm::Imm)
                )
            }
        }
        */
        let mut tokens = s.split_whitespace();
        
        macro_rules! token(() => {
            tokens.next().ok_or(OpError::NoToken)?
        });
        
        // Parses a register literal. Returns early if an error is
        // found.
        macro_rules! reg(() => {
            token!().parse::<Reg>()?
        });
        
        // Parses an immediate value with max value $max.
        macro_rules! imm(($max:expr) => {{
            let val = token!().parse::<u16>()?;
            
            if val as usize <= $max {
                val as u8
            } else {
                return Err(OpError::ValueOverflow($max));
            }
        }});
        
        macro_rules! offset(($max:expr) => {{
            let offset = Offset::from_str(token!())?;
            
            match offset {
                #[cfg(feature = "labels")]
                Offset::Label(_) => offset,
                
                Offset::Forward(val) | Offset::Backward(val) => {
                    if val <= $max { offset }
                    else { return Err(OpError::ValueOverflow($max)); }
                }
            }
        }});
        
        let token = token!();
        /*
        let op = if let Ok(ra) = Reg::from_str(token) {
            match token!() {
                "<>" => Op::Swap(ra, reg!()),
                ":=" => match RegOrImm::from_str(token!())? {
                    RegOrImm::Reg(rb)  => Op::Xor(ra, rb),
                    RegOrImm::Imm(imm) => Op::XorImm(ra, imm as u8),
                }
                "+=" => match RegOrImm::from_str(token!())? {
                    RegOrImm::Reg(rb)  => Op::Add(ra, rb),
                    RegOrImm::Imm(imm) => Op::AddImm(ra, imm as u8),
                }
                "-=" => match RegOrImm::from_str(token!())? {
                    RegOrImm::Reg(rb)  => Op::Sub(ra, rb),
                    RegOrImm::Imm(imm) => Op::SubImm(ra, imm as u8),
                }
                _ => return Err(OpError::BadToken(token.to_owned()))
            }
        }
        else {
        let op = match token!() {
        */
        let op = match token {
            "halt" => Op::Halt,
            "nop" => Op::Nop,
            "dbg" => Op::Debug,
            
            "not"  => Op::Not(reg!()),
            "spc"  => Op::SwapPc(reg!()),
            "rspc" => Op::RevSwapPc(reg!()),
                        
            "swp"  => Op::Swap(reg!(), reg!()),
            "xchg" => Op::Exchange(reg!(), reg!()),
            "xor"  => Op::Xor(reg!(), reg!()),
            "add"  => Op::Add(reg!(), reg!()),
            "sub"  => Op::Sub(reg!(), reg!()),
            "rol"  => Op::LRot(reg!(), reg!()),
            "ror"  => Op::RRot(reg!(), reg!()),
            
            "xori" => Op::XorImm(reg!(), imm!(0xFF)),
            "addi" => Op::AddImm(reg!(), imm!(0xFF)),
            "subi" => Op::SubImm(reg!(), imm!(0xFF)),
            "roli" => Op::LRotImm(reg!(), imm!(0xFF)),
            "rori" => Op::RRotImm(reg!(), imm!(0xFF)),
            "io"   => Op::IO(reg!(), imm!(0xFF)),
            
            "ccn"  => Op::CCNot(reg!(), reg!(), reg!()),
            "cswp" => Op::CSwap(reg!(), reg!(), reg!()),
            
            "bo" => Op::BranchParity(true, reg!(), offset!(0xFF)),
            "be" => Op::BranchParity(false, reg!(), offset!(0xFF)),
            "bs" => Op::BranchSign(true, reg!(), offset!(0xFF)),
            "bn" => Op::BranchSign(false, reg!(), offset!(0xFF)),
            
            "jmp" => Op::Jump(offset!(0x1FFF)),
            
            #[cfg(feature = "teleport")]
            "tp"  => Op::Teleport(imm!(0x1FFF)),
            
            token => return Err(OpError::BadToken(token.to_string()))
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
