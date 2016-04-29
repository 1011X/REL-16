use std::fmt;
use std::str;

pub type Reg = usize; // always in range [0-7]

/*
pub enum OpType {
	Signal,
	Single,
	Double,
	Triple,
	Data,
	Pair,
	
	Unused,
}
*/

/// Types of instructions:
///
/// * *Pair*: Has 1 register and an 8-bit value.
///   `1ooo orrr vvvv vvvv` where
///   `o`es represent the operation field
///   `r`es represent the register used
///   `v`es represent the value used
///
/// * *Data*: Has 12-bit value.
///   `01oo vvvv vvvv vvvv` where
///   `o`es represent the operation field
///   `v`es represent the offset value used
///
/// * *Triple*: Has 3 registers.
///   `0000 1ooa aabb bccc` where
///   `o`es represent the operation field
///   `a`es represent the first register
///   `b`es represent the second register
///   `c`es represent the third register
/// 
/// * *Double*: Has 2 registers.
///   `0000 01oo ooaa abbb` where
///   `o`es represent the operation field
///   `a`es represent the first register
///   `b`es represent the second register
///
/// * *Single*: Has 1 register.
///   `0000 001o oooo orrr` where
///   `o`es represent the operation field
///   `r`es represent the register used
/// 
/// * *Signal*: Tells the machine to do something. Has a 4-bit value.
///   `0000 0000 00ss ssss` where
///   `s`es represent the signal field
/// 
/// * *Reserved*: Unused instructions.
///   `001x xxxx xxxx xxxx`
///   `0001 xxxx xxxx xxxx`
///   `0000 0001 xxxx xxxx`
///   `0000 0000 1xxx xxxx`
///   `0000 0000 01xx xxxx`

pub enum Op {
	Halt,
	Not(Reg),
	RotateLeft(Reg),
	RotateRight(Reg),
	Increment(Reg),
	Decrement(Reg),
	Push(Reg),
	Pop(Reg),
	//Read(Reg),
	//Write(Reg),
	Swap(Reg, Reg),
	CNot(Reg, Reg),
	CAdd(Reg, Reg),
	CSub(Reg, Reg),
	Exchange(Reg, Reg),
	Immediate(Reg, u8),
	CCNot(Reg, Reg, Reg),
	CSwap(Reg, Reg, Reg),
	GoTo(u16),
	ComeFrom(u16),
	/*
	BranchGEZ(Reg, u8),
	BranchLZ(Reg, u8),
	SwapBr(Reg),
	RevSwapBr(Reg),
	*/
}

impl Op {
	
	// To guarantee VM is reversible, every operation must have an inverse.
	// Involutory instructions (instructions that are their own inverse) return self.
	pub fn invert(self) -> Op {
		match self {
			Op::Halt           => self,
			Op::Not(..)        => self,
			Op::RotateLeft(r)  => Op::RotateRight(r),
			Op::RotateRight(r) => Op::RotateLeft(r),
			Op::Increment(r)   => Op::Decrement(r),
			Op::Decrement(r)   => Op::Increment(r),
			Op::Push(r)        => Op::Pop(r),
			Op::Pop(r)         => Op::Push(r),
			Op::Swap(..)       => self,
			Op::CNot(..)       => self,
			Op::CAdd(rc, ra)   => Op::CSub(rc, ra),
			Op::CSub(rc, rs)   => Op::CAdd(rc, rs),
			Op::Exchange(..)   => self,
			Op::Immediate(..)  => self,
			Op::CCNot(..)      => self,
			Op::CSwap(..)      => self,
			
			// control flow
			Op::GoTo(off)        => Op::ComeFrom(off),
			Op::ComeFrom(off)    => Op::GoTo(off),
			/*
			Op::BranchLZ(reg, off) => 
			Op::BranchGEZ(reg, off) => 
			Op::SwapBr(reg) => Op::RevSwapBr(reg),
			Op::RevSwapBr(reg) => Op::SwapBr(reg),
			*/
		}
	}
	/*
	pub fn op_type(&self) -> OpType {
		match *self {
			Op::Halt => OpType::Signal,
			
			Op::Not(..)
			| Op::RotateLeft(..)
			| Op::RotateRight(..)
			| Op::Increment(..)
			| Op::Decrement(..)
			| Op::Push(..)
			| Op::Pop(..) =>
				OpType::Single,
			
			Op::Swap(..)
			| Op::CNot(..)
			| Op::CAdd(..)
			| Op::CSub(..)
			| Op::Exchange(..) =>
				OpType::Double,
			
			Op::CCNot(..)
			| Op::CSwap(..) =>
				OpType::Triple,
			
			Op::GoTo(..)
			| Op::ComeFrom(..) =>
				OpType::Data,
			
			Op::Immediate(..) =>
				OpType::Pair,
		}
	}
	*/

	pub fn encode(&self) -> u16 {
		// TODO: decide if I want to have assertions for values.
		match *self {
			Op::Halt => 0,
		
			Op::Not(reg) => 0b_0000001_000000_000
				| reg as u16,
		
			Op::RotateLeft(reg) => 0b_0000001_000001_000
				| reg as u16,
		
			Op::RotateRight(reg) => 0b_0000001_000010_000
				| reg as u16,
		
			Op::Increment(reg) => 0b_0000001_000011_000
				| reg as u16,
		
			Op::Decrement(reg) => 0b_0000001_000100_000
				| reg as u16,
		
			Op::Push(reg) => 0b_0000001_000101_000
				| reg as u16,
		
			Op::Pop(reg) => 0b_0000001_000110_000
				| reg as u16,
		
			Op::Swap(rl, rr) => 0b_000001_0000_000_000
				| (rl as u16) << 3
				| rr as u16,
		
			Op::CNot(rc, rn) => 0b_000001_0001_000_000
				| (rc as u16) << 3
				| rn as u16,
		
			Op::CAdd(rc, ra) => 0b_000001_0010_000_000
				| (rc as u16) << 3
				| ra as u16,
		
			Op::CSub(rc, rs) => 0b_000001_0011_000_000
				| (rc as u16) << 3
				| rs as u16,
		
			Op::Exchange(r, ra) => 0b_000001_0100_000_000
				| (r as u16) << 3
				| ra as u16,
		
			Op::CCNot(rc0, rc1, rn) => 0b_00001_00_000_000_000
				| (rc0 as u16) << 6
				| (rc1 as u16) << 3
				| rn as u16,
		
			Op::CSwap(rc, rs0, rs1) => 0b_00001_01_000_000_000
				| (rc as u16) << 6
				| (rs0 as u16) << 3
				| rs1 as u16,
		
			Op::Immediate(reg, val) => 0b_1_0000_000_00000000
				| (reg as u16) << 8
				| val as u16,
		
			// control flow
		
			Op::GoTo(off) => 0b_01_00_000000000000
				| off as u16,
		
			Op::ComeFrom(off) => 0b_01_01_000000000000
				| off as u16,
		
			/*
			Op::BranchLZ(reg, off) => 0b_1_0001_000_00000000
				| (reg as u16) << 8
				| off as u16,
		
			Op::BranchGEZ(reg, off) => 0b_1_0010_000_00000000
				| (reg as u16) << 8
				| off as u16,
		
			Op::SwapBr(reg) => 0b_0000001_000111_000
				| reg as u16,
		
			Op::RevSwapBr(reg) => 0b_0000001_001000_000
				| reg as u16,
			*/
		}
	}
	
	pub fn decode(instr: u16) -> Op {
	
		match instr.leading_zeros() {
			// pair type
			0 => {
				let o = (0b_0_1111_000_00000000 & instr) >> 3 + 8;
				let r = (0b_0_0000_111_00000000 & instr) >> 8;
				let v =  0b_0_0000_000_11111111 & instr;
			
				match o {
					0b_0000 => Op::Immediate(r as usize, v as u8),
				
					o if o <= 0b_1111 =>
						panic!("Invalid Pair-type instruction: o={:04b} r{} 0x{:02x}", o, r, v),
				
					_ => unreachable!(),
				}
			}
		
			// data type
			1 => {
				let o = (0b_00_11_000000000000 & instr) >> 12;
				let v = 0b_00_00_111111111111 & instr;
			
				match o {
					0b_00 => Op::GoTo(v as u16),
					0b_01 => Op::ComeFrom(v as u16),
				
					o if o <= 0b_11 =>
						panic!("Invalid Data-type instruction: o={:02b} 0x{:03x}", o, v),
				
					_ => unreachable!(),
				}
			}
		
			// unused
			2...3 => panic!("Reserved/unused instruction: 0x{:x}", instr),
		
			// triple type
			4 => {
				let o  = (0b_00000_11_000_000_000 & instr) >> 3 + 3 + 3;
				let ra = (0b_00000_00_111_000_000 & instr) >> 3 + 3;
				let rb = (0b_00000_00_000_111_000 & instr) >> 3;
				let rc =  0b_00000_00_000_000_111 & instr;
			
				match o {
					0b_00 => Op::CCNot(ra as usize, rb as usize, rc as usize),
					0b_01 => Op::CSwap(ra as usize, rb as usize, rc as usize),
				
					o if o <= 0b_11 =>
						panic!("Invalid Triple-type instruction: o={:02b} r{} r{} r{}", o, ra, rb, rc),
				
					_ => unreachable!(),
				}
			}
		
			// double type
			5 => {
				let o  = (0b_000000_1111_000_000 & instr) >> 3 + 3;
				let ra = (0b_000000_0000_111_000 & instr) >> 3;
				let rb =  0b_000000_0000_000_111 & instr;
			
				match o {
					0b_0000 => Op::Swap(ra as usize, rb as usize),
					0b_0001 => Op::CNot(ra as usize, rb as usize),
					0b_0010 => Op::CAdd(ra as usize, rb as usize),
					0b_0011 => Op::CSub(ra as usize, rb as usize),
					0b_0100 => Op::Exchange(ra as usize, rb as usize),
				
					o if o <= 0b_1111 =>
						panic!("Invalid Double-type instruction: o={:04b} r{} r{}", o, ra, rb),
				
					_ => unreachable!(),
				}
			}
		
			// single type
			6 => {
				let o = (0b_0000000_111111_000 & instr) >> 3;
				let r =  0b_0000000_000000_111 & instr;
			
				match o {
				
					0b_000000 => Op::Not(r as usize),
			
					0b_000001 => Op::RotateLeft(r as usize),
					0b_000010 => Op::RotateRight(r as usize),
			
					0b_000011 => Op::Increment(r as usize),
					0b_000100 => Op::Decrement(r as usize),
	
					0b_000101 => Op::Push(r as usize),
					0b_000110 => Op::Pop(r as usize),
				
					o if o <= 0b_111111 =>
						panic!("Invalid Single-type instruction: o={:06b} r{}", o, r),
				
					_ => unreachable!(),
				}
			}
		
			// reserved
			7...9 => panic!("Reserved/unused instruction: {:x}", instr),
		
			// signal type
			10...16 => match instr {
				0b_000000 => Op::Halt,
			
				s if s <= 0b111111 =>
					panic!("Invalid Signal-type instruction: {}", s),
			
				_ => unreachable!(),
			},
		
			_ => unreachable!(),
		}
	}
}

impl fmt::Display for Op {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Op::Halt                => f.write_str("halt"),
			Op::Not(r)              => f.write_fmt(format_args!("not r{}", r)),
			Op::RotateLeft(r)       => f.write_fmt(format_args!("rotl r{}", r)),
			Op::RotateRight(r)      => f.write_fmt(format_args!("rotr r{}", r)),
			Op::Increment(r)        => f.write_fmt(format_args!("inc r{}", r)),
			Op::Decrement(r)        => f.write_fmt(format_args!("dec r{}", r)),
			Op::Push(r)             => f.write_fmt(format_args!("push r{}", r)),
			Op::Pop(r)              => f.write_fmt(format_args!("pop r{}", r)),
			Op::Swap(rl, rr)        => f.write_fmt(format_args!("swp r{} r{}", rl, rr)),
			Op::CNot(rc, rn)        => f.write_fmt(format_args!("cnot r{} r{}", rc, rn)),
			Op::CAdd(rc, ra)        => f.write_fmt(format_args!("cadd r{} r{}", rc, ra)),
			Op::CSub(rc, rs)        => f.write_fmt(format_args!("csub r{} r{}", rc, rs)),
			Op::Exchange(rr, ra)    => f.write_fmt(format_args!("exch r{} r{}", rr, ra)),
			Op::Immediate(r, v)     => f.write_fmt(format_args!("imm r{} {}", r, v)),
			Op::CCNot(rc0, rc1, rn) => f.write_fmt(format_args!("ccn r{} r{} r{}", rc0, rc1, rn)),
			Op::CSwap(rc, rs0, rs1) => f.write_fmt(format_args!("cswp r{} r{} r{}", rc, rs0, rs1)),
			Op::GoTo(off)           => f.write_fmt(format_args!("goto {}", off)),
			Op::ComeFrom(off)       => f.write_fmt(format_args!("cmfr {}", off)),
		}
	}
}
/*
impl str::FromStr for Op {
	type Err = String;
	
	fn from_str(s: &str) -> Result<Self, Self::Err> {
		
	}
}
*/
