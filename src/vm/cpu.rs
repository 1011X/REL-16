use isa::op::{Op, Addr};
use isa::reg::Reg;

use super::register_file::RegisterFile;

const SP: Reg = Reg::SP;
const BP: Reg = Reg::BP;
const MAX_MEM: usize = 65536;

pub struct Cpu<'mem> {
	logging_enabled: bool,
	
	// when dir is true, it is in reverse mode
	dir: bool,
	br: u16,
	pc: u16,
	ir: Op,
	
	// Some conventions on registers:
	// r0 can be treated like an accumulator register
	// r6 is the stack top pointer (sp)
	// r7 is the stack base pointer (bp)
	reg: RegisterFile,
	
	prog_mem: &'mem [Op], // ROM
	data_mem: [u16; MAX_MEM], // RAM
}

impl<'mem> Cpu<'mem> {
	pub fn new(prog: &[Op], log: bool) -> Cpu {
		Cpu {
			logging_enabled: log,
			
			dir: false,
			br: 1,
			pc: 0,
			ir: Op::Halt,
			reg: RegisterFile([0; 8]),
			prog_mem: prog,
			data_mem: [0; MAX_MEM],
		}
	}
	
	pub fn run(&mut self) {
		loop {
			if self.step() { break; }
		}
	}
	
	pub fn step(&mut self) -> bool {
		if self.logging_enabled {
			println!("pc = {:04x}  br = {:04x}  dir = {}", self.pc, self.br, self.dir);
		
			// print contents of registers
			print!("registers: [");
		
			for &val in &self.reg.0[..BP as usize] {
				print!("{:04x}, ", val);
			}
		
			print!("{:04x}]\n", self.reg[BP]);
		
		
			// print contents of stack
			print!("stack: ");
		
			use std::cmp::Ordering;
		
			match self.reg[BP].cmp(&self.reg[SP]) {
				Ordering::Equal   => print!("nil\n"),
				Ordering::Less    => print!("invalid\n"),
				Ordering::Greater => {
					let bp = self.reg[BP] as usize;
					let sp = self.reg[SP] as usize;
				
					print!("<");
				
					// log whole stack except for last value
					for &val in &self.data_mem[sp..bp - 1] {
						print!("{:04x}, ", val);
					}
				
					// log last value
					print!("{:04x}]\n", self.data_mem[bp - 1]);
				}
			}
		
			print!("\n");
		}
	
		/* FETCH */
		self.ir = match self.prog_mem.get(self.pc as usize) {
			Some(val) => val.clone(),
			None => {
				println!("UNEXPECTED HALT");
				println!("Encountered halt instruction that wasn't part of the program.");
				return true;
			}
		};
		
		/* EXECUTE */
		// get instruction and invert if in reverse mode
		if self.dir {
			self.ir = self.ir.clone().invert();
		}
		
		// macro to log only when self.logging_enabled is true
		macro_rules! log(($($t:tt)*) => {
			if self.logging_enabled {
				print!($($t)*);
			}
		});
		
		
		// show which instruction is being executed
		log!("ir: {:?}\n\n", self.ir);
	
		match self.ir {
			Op::Halt => return true,
			Op::Nop => {}
			
			Op::Not(a) =>
				self.reg[a] = !self.reg[a],
			
			Op::Negate(a) =>
				self.reg[a] = self.reg[a].wrapping_neg(),
		
			Op::Increment(a) =>
				self.reg[a] = self.reg[a].wrapping_add(1),
		
			Op::Decrement(a) =>
				self.reg[a] = self.reg[a].wrapping_sub(1),
		
			Op::Push(rr) => {
				let mut r = 0;
				let mut sp = 0;
				swap!(r, self.reg[rr]);
				swap!(sp, self.reg[SP]);
			
				sp = sp.wrapping_sub(1);
				swap!(r, self.data_mem[sp as usize]);
			
				swap!(sp, self.reg[SP]);
				swap!(r, self.reg[rr]);
				debug_assert!(sp == 0);
				debug_assert!(r == 0);
			}
		
			Op::Pop(rr) => {
				let mut r = 0;
				let mut sp = 0;
				swap!(r, self.reg[rr]);
				swap!(sp, self.reg[SP]);
			
				swap!(r, self.data_mem[sp as usize]);
				sp = sp.wrapping_add(1);
			
				swap!(sp, self.reg[SP]);
				swap!(r, self.reg[rr]);
				debug_assert!(sp == 0);
				debug_assert!(r == 0);
			}
		
			Op::SwapPc(r) =>
				swap!(self.pc, self.reg[r]),
		
			Op::RevSwapPc(r) => {
				swap!(self.pc, self.reg[r]);
				self.dir = !self.dir;
			}
		
			Op::Mul2(ri) => {
				use std::i16;
				
				let r = self.reg[ri] as i16;
				
				self.reg[ri] = match r {
					-16384...16383    => r * 2,
					16384...i16::MAX  => r - (i16::MAX - r),
					i16::MIN...-16385 => r + (r - i16::MIN + 1),
					
					_ => unreachable!()
				} as u16;
			}
			
			Op::Div2(ri) => {
				use std::i16;
				
				let r = self.reg[ri] as i16;
				let odd = |r| r & 1 == 1;
				
				self.reg[ri] = match r {
					0...i16::MAX if odd(r) => i16::MAX - (i16::MAX - r) / 2,
					i16::MIN...0 if odd(r) => i16::MIN + (r + i16::MAX) / 2,
					_ /* even */           => r / 2
				} as u16;
			}
			
			Op::RotLeftImm(r, v) =>
				self.reg[r] = self.reg[r].rotate_left(v as u32),
			
			Op::RotRightImm(r, v) =>
				self.reg[r] = self.reg[r].rotate_right(v as u32),
			
			Op::Swap(a, b) =>
				self.reg.0.swap(a as usize, b as usize),
			
			Op::CNot(rn, rc) => {
				let mut n = 0;
				swap!(n, self.reg[rn]);
				
				n ^= self.reg[rc];
				
				swap!(n, self.reg[rn]);
				debug_assert!(n == 0);
			}
			
			Op::CAdd(ra, rc) => {
				let mut a = 0;
				swap!(a, self.reg[ra]);
				
				a = a.wrapping_add(self.reg[rc]);
				
				swap!(a, self.reg[ra]);
				debug_assert!(a == 0);
			}
			
			Op::CSub(rs, rc) => {
				let mut s = 0;
				swap!(s, self.reg[rs]);
				
				s = s.wrapping_sub(self.reg[rc]);
				
				swap!(s, self.reg[rs]);
				debug_assert!(s == 0);
			}
			
			Op::Exchange(rd, ra) => {
				let mut d = 0;
				swap!(d, self.reg[rd]);
				
				let addr = self.reg[ra] as usize;
				swap!(d, self.data_mem[addr]);
				
				swap!(d, self.reg[rd]);
				debug_assert!(d == 0);
			}
			
			Op::RotLeft(rr, ro) => {
				let mut r = 0;
				swap!(r, self.reg[rr]);
				
				r = r.rotate_left(self.reg[ro] as u32);
				
				swap!(r, self.reg[rr]);
				debug_assert!(r == 0);
			}
			
			Op::RotRight(rr, ro) => {
				let mut r = 0;
				swap!(r, self.reg[rr]);
				
				r = r.rotate_right(self.reg[ro] as u32);
				
				swap!(r, self.reg[rr]);
				debug_assert!(r == 0);
			}
			
			Op::CCNot(rc0, rc1, rn) => {
				let mut n = 0;
				swap!(n, self.reg[rn]);
				
				n ^= self.reg[rc0] & self.reg[rc1];
				
				swap!(n, self.reg[rn]);
				debug_assert!(n == 0);
			}
			
			Op::CSwap(rc, rs0, rs1) => {
				let mut s0 = 0;
				let mut s1 = 0;
				swap!(s0, self.reg[rs0]);
				swap!(s1, self.reg[rs1]);
				
				let t = (s0 ^ s1) & self.reg[rc];
				s0 ^= t;
				s1 ^= t;
				
				swap!(s1, self.reg[rs1]);
				swap!(s0, self.reg[rs0]);
				debug_assert!(s1 == 0);
				debug_assert!(s0 == 0);
			}
			
			Op::BranchParityOdd(r, Addr::Offset(off)) =>
			if (self.reg[r] & 1) == 1 {
				self.br = self.br.wrapping_add(off as u16);
			},
			
			Op::AssertParityOdd(r, Addr::Offset(off)) =>
			if (self.reg[r] & 1) == 1 {
				self.br = self.br.wrapping_sub(off as u16);
			},
			
			Op::BranchSignNegative(r, Addr::Offset(off)) =>
			if (self.reg[r] as i16) < 0 {
				self.br = self.br.wrapping_add(off as u16);
			},
			
			Op::AssertSignNegative(r, Addr::Offset(off)) =>
			if (self.reg[r] as i16) < 0 {
				self.br = self.br.wrapping_sub(off as u16);
			},
			
			Op::BranchParityEven(r, Addr::Offset(off)) =>
			if (self.reg[r] & 1) == 0 {
				self.br = self.br.wrapping_add(off as u16);
			},
			
			Op::AssertParityEven(r, Addr::Offset(off)) =>
			if (self.reg[r] & 1) == 0 {
				self.br = self.br.wrapping_sub(off as u16);
			},
			
			Op::BranchSignNonneg(r, Addr::Offset(off)) =>
			if (self.reg[r] as i16) >= 0 {
				self.br = self.br.wrapping_add(off as u16);
			},
			
			Op::AssertSignNonneg(r, Addr::Offset(off)) =>
			if (self.reg[r] as i16) >= 0 {
				self.br = self.br.wrapping_sub(off as u16);
			},
			
			Op::Immediate(r, v) =>
				self.reg[r] ^= v as u16,
		
			Op::GoTo(Addr::Offset(off)) =>
				self.br = self.br.wrapping_add(off as u16),
		
			Op::ComeFrom(Addr::Offset(off)) =>
				self.br = self.br.wrapping_sub(off as u16),
			
			// for when branch instrs use labels (which shouldn't happen)
			Op::BranchParityOdd(..)
			| Op::BranchParityEven(..)
			| Op::BranchSignNegative(..)
			| Op::BranchSignNonneg(..)
			| Op::AssertParityOdd(..)
			| Op::AssertParityEven(..)
			| Op::AssertSignNegative(..)
			| Op::AssertSignNonneg(..)
			| Op::GoTo(..)
			| Op::ComeFrom(..) =>
				unreachable!(),
		}
		
		// decide next instruction based on offset and dir bit
		if self.dir {
			self.pc = self.pc.wrapping_sub(self.br);
		} else {
			self.pc = self.pc.wrapping_add(self.br);
		}
		
		false
	}
}
