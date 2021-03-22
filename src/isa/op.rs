use std::fmt;
use std::num;
use std::convert::From;
use std::error::Error;
use std::str::FromStr;

use super::reg::{self, Reg};


// for semantic purposes
type RegMut = Reg;

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
                f.write_str("Found extra token."),
            ParseOpError::NoToken =>
                f.write_str("Missing token"),
            ParseOpError::BadToken(tok) =>
                write!(f, "Bad token: {}", tok),
            ParseOpError::ValueOverflow(max) =>
                write!(f, "Value exceeds maximum of {}", max),
            ParseOpError::ParseInt(pie) =>
            	pie.fmt(f),
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


#[derive(Debug, Clone)]
enum Value {
	Reg(Reg),
	Imm(u16),
}

#[derive(Debug, Clone)]
enum Address {
	Label(String),
	Imm(u16),
}


/**
An assembly instruction.

This enum is a more flexible version of the REL-16 machine instruction
equivalent, as assembly is more likely to be seen and used by a human. For
example, if you provide a register or immediate value to an arithmetic
instruction, the assembler will pick the right machine instruction for you.
*/
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    /// Stops the machine.
    Halt,
    
    /// Does absolutely nothing.
    Nop,
    
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
    Exchange(RegMut, Address),
    
    /// Flips bits in first register based on bits in second register.
    /// Exactly like x86's `xor` instruction.
    Xor(RegMut, Value),
    
    /// Adds/increases first register's value by second register's
    /// value.
    Add(RegMut, Value),
    
    /// Subtracts/decreases the first register's value by the second
    /// register's value.
    Sub(RegMut, Value),
    
    /// Rotates the first register's bits leftwards by the value in
    /// the second register.
    /// 
    /// Only the first 4 bits are necessary/used.
    LRot(RegMut, Value),
    
    /// Rotates the first register's bits rightwards by the value in
    /// the second register.
    /// 
    /// Only the first 4 bits are necessary/used.
    RRot(RegMut, Value),
    
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
    
    /// Adds an immediate value to the branch register.
    Jump(Address),
    
    /// XORs the immediate value onto the branch register if the given register
    /// is odd.
    BranchOdd(Reg, Address),
    
    /// XORs the immediate value onto the branch register if the given register
    /// is even.
    BranchEven(Reg, Address),
    
    /// XORs the immediate value onto the branch register if the given register
    /// has the sign bit (uppermost bit) set.
    BranchSign(Reg, Address),
    
    /// XORs the immediate value onto the branch register if the given register
    /// has the sign bit (uppermost bit) unset.
    BranchUnsign(Reg, Address),
    
    /// Swaps data at given register to the device at the given port.
    ///
    /// Devices can often be used as a way of getting rid of unwanted
    /// garbage, so swapping becomes the preferred method. If one
    /// wishes to keep a copy of the data sent, `xor` it first.
    IO(Reg, u8),
}

impl Op {
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
/*
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
            
            Op::Jump(offset)  => write!(f, "jmp {}", offset),
        }
    }
}
*/
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
    	
    }
}
