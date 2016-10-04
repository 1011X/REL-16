use isa::{Op, Reg};

use super::register_file::RegisterFile;
use super::rev::Rev;
use super::MAX_MEM_LEN;

pub struct Cpu<'mem> {
	// when dir is true, it is in reverse mode
	dir: bool,
	br: Rev,
	pc: Rev,
	ir: u16,
	
	// Some conventions on registers:
	// r0 can be treated like an accumulator register
	// r6 is the stack top pointer (sp)
	// r7 is the stack base pointer (bp)
	reg: RegisterFile,
	
	prog_mem: &'mem [u16; MAX_MEM_LEN], //MemoryROM<'a>,
	data_mem: &'mem mut [u16; MAX_MEM_LEN], //MemoryRAM<'a>,
}

impl<'mem> Cpu<'mem> {
	pub fn new(prog: &'mem [u16; MAX_MEM_LEN], data: &'mem mut [u16; MAX_MEM_LEN]) -> Cpu<'mem> {
		Cpu {
			dir: false,
			br: Rev(1),
			pc: Rev(0),
			ir: 0,
			reg: RegisterFile([Rev(0); 8]),
			prog_mem: prog,
			data_mem: data,
		}
	}
	
	//pub fn get_reg(&self, r: Reg) -> u16 { self.reg[r as usize] }
	//pub fn set_reg(&mut self, r: Reg, v: u16) { self.reg[r as usize] = v }
	
	pub fn log_state(&self) {
		// address in pc and the instruction it's pointing to
		println!("pc = {:04x}  br = {:04x}  dir = {}", self.pc.0, self.br.0, self.dir);
		
		
		// print contents of registers
		print!("registers: [");
	
		for &Rev(val) in &self.reg.0[..7] {
			print!("{:04x}, ", val);
		}
		
		println!("{:04x}]", self.reg[Reg::BP].0);
		
		
		// print contents of stack
		print!("stack: ");
		
		// bp == sp
		if self.reg[Reg::BP] == self.reg[Reg::SP] {
			println!("nil");
		}
		// bp < sp
		else if self.reg[Reg::BP] < self.reg[Reg::SP] {
			println!("invalid");
		}
		else {
			let bp = self.reg[Reg::BP].0 as usize;
			let sp = self.reg[Reg::SP].0 as usize;
			
			print!("<");
			
			// log whole stack except for last value
			for &val in &self.data_mem[sp..bp - 1] {
				print!("{:04x}, ", val);
			}
			
			// log last value
			println!("{:04x}]", self.data_mem[bp - 1]);
		}
		
		print!("\n");
	}
	
	pub fn log_current_instr(&self) {
		// show which instruction is being executed
		print!("ir = {:04x}: ", self.ir);
		// if invalid, log it and skip it.
		match Op::decode(self.ir) {
			Ok(ref instr) => println!("{}\n", instr),
			Err(_)        => println!("INVALID\n"),
		}
	}
	
	pub fn fetch(&mut self) -> bool {
		self.ir = match self.prog_mem.get(self.pc.0 as usize) {
			Some(&val) => val,
			None => {
				println!("UNEXPECTED HALT");
				println!("Encountered halt instruction that wasn't part of the program");
				return true;
			}
		};
		
		false
	}
	
	pub fn execute(&mut self) -> bool {
		// get instruction and invert if in reverse mode
		let instr = {
			let res = match Op::decode(self.ir) {
				Ok(instr) => instr,
				Err(e) => {
					println_err!("Error parsing instruction {:#06x}", self.ir);
					println_err!("{}", e);
					return true;
				}
			};
			
			if self.dir { res.invert() }
			else { res }
		};
		
		match instr {
			Op::Halt => return true,
			
			
			Op::Not(a) =>
				self.reg[a].0 = !self.reg[a].0,
			
			Op::Increment(a) =>
				self.reg[a] += 1,
			
			Op::Decrement(a) =>
				self.reg[a] -= 1,
			
			Op::Push(r) => {
				self.reg[Reg::SP] -= 1;
				let sp = self.reg[Reg::SP].0 as usize;
				swap!(self.reg[r].0, self.data_mem[sp]);
			}
			
			Op::Pop(r) => {
				let sp = self.reg[Reg::SP].0 as usize;
				swap!(self.reg[r].0, self.data_mem[sp]);
				self.reg[Reg::SP] += 1;
			}
			
			Op::SwapPc(r) =>
				swap!(self.pc, self.reg[r]),
			
			Op::RevSwapPc(r) => {
				swap!(self.pc, self.reg[r]);
				self.dir = !self.dir;
			}
			
			Op::Mul2(ri) => {
				use std::i16;
				
				let r = self.reg[ri].0 as i16;
				
				self.reg[ri].0 = match r {
					-16384...16383    => r * 2,
					16384...i16::MAX  => r - (i16::MAX - r),
					i16::MIN...-16385 => r + (r - i16::MIN + 1),
					
					_ => unreachable!()
				} as u16;
			}
			
			Op::Div2(ri) => {
				use std::i16;
				
				let r = self.reg[ri].0 as i16;
				let odd = |r| r & 1 == 1;
				
				self.reg[ri].0 = match r {
					0...i16::MAX if odd(r) => i16::MAX - (i16::MAX - r) / 2,
					i16::MIN...0 if odd(r) => i16::MIN + (r + i16::MAX) / 2,
					_ /* even */           => r / 2
				} as u16;
			}
			
		
			Op::RotLeftImm(r, v) =>
				self.reg[r] <<= v as u16,
			
			Op::RotRightImm(r, v) =>
				self.reg[r] >>= v as u16,
			
			
			Op::Swap(a, b) =>
				self.reg.0.swap(a as usize, b as usize),
			
			Op::CNot(rn, rc) =>
				self.reg[rn] ^= self.reg[rc],
			
			Op::CAdd(ra, rc) =>
				self.reg[ra] += self.reg[rc],
			
			Op::CSub(rs, rc) =>
				self.reg[rs] -= self.reg[rc],
			
			Op::Exchange(r, ra) => {
				let raddr = self.reg[ra].0 as usize;
				swap!(self.reg[r].0, self.data_mem[raddr]);
			}
			
			Op::RotLeft(rr, ro) =>
				self.reg[rr] <<= self.reg[ro],
			
			Op::RotRight(rr, ro) =>
				self.reg[rr] >>= self.reg[ro],
			
			Op::IO(rd, rp) =>
				unimplemented!(),
			
			Op::CCNot(rc0, rc1, rn) =>
				self.reg[rn] ^= self.reg[rc0].0 & self.reg[rc1].0,
			
			Op::CSwap(rc, rs0, rs1) => {
				let c = self.reg[rc].0;
				let mut s0 = self.reg[rs0].0;
				let mut s1 = self.reg[rs1].0;
				
				let t = (s0 ^ s1) & c;
				s0 ^= t;
				s1 ^= t;
				
				self.reg[rs0].0 = s0;
				self.reg[rs1].0 = s1;
			}
			
			
			Op::BranchParity(r, off) =>
				if self.reg[r].0 % 2 == 1 {
					self.br += off as u16;
				},
			
			Op::AssertParity(r, off) =>
				if self.reg[r].0 % 2 == 1 {
					self.br -= off as u16;
				},
			
			Op::BranchSign(r, off) =>
				if (self.reg[r].0 as i16) < 0 {
					self.br += off as u16;
				},
			
			Op::AssertSign(r, off) =>
				if (self.reg[r].0 as i16) < 0 {
					self.br -= off as u16;
				},
			
			Op::Immediate(r, v) =>
				self.reg[r] ^= v as u16,
			
			
			Op::GoTo(off) =>
				self.br += off,
			
			Op::ComeFrom(off) =>
				self.br -= off,
		}
		
		// next instruction
		if self.dir {
			self.pc -= self.br;
		} else {
			self.pc += self.br;
		}
		
		false
	}
}
