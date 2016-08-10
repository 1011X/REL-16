use std::io::{Read, BufReader};
use std::ops::{Index, IndexMut};
use std::num::Wrapping;

use isa::{Op, Reg};


const MAX_MEM_LEN: usize = 65536;

/*
struct MemoryROM<'a>(&'a [u16; MAX_MEM_LEN]);
struct MemoryRAM<'a>(&'a mut [u16; MAX_MEM_LEN]);
*/

struct RegisterFile([Wrapping<u16>; 8]);

impl Index<Reg> for RegisterFile {
	type Output = Wrapping<u16>;
	fn index(&self, index: Reg) -> &Self::Output {
		&self.0[index as usize]
	}
}

impl IndexMut<Reg> for RegisterFile {
	fn index_mut(&mut self, index: Reg) -> &mut Self::Output {
		&mut self.0[index as usize]
	}
}

pub struct Rel16<'mem> {
	// when dir is true, it is in reverse mode
	dir: bool,
	br: Wrapping<u16>,
	pc: Wrapping<u16>,
	ir: u16,
	
	// Some conventions on registers:
	// r0 can be treated like an accumulator register
	// r6 is the stack top pointer (sp)
	// r7 is the stack base pointer (bp)
	reg: RegisterFile,
	
	prog_mem: &'mem [u16; MAX_MEM_LEN], //MemoryROM<'a>,
	data_mem: &'mem mut [u16; MAX_MEM_LEN], //MemoryRAM<'a>,
}

impl<'mem> Rel16<'mem> {
	pub fn new(prog: &'mem [u16; MAX_MEM_LEN], data: &'mem mut [u16; MAX_MEM_LEN]) -> Rel16<'mem> {
		Rel16 {
			dir: false,
			br: Wrapping(1),
			pc: Wrapping(0),
			ir: 0,
			reg: RegisterFile([Wrapping(0); 8]),
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
	
		for &r in self.reg.0[..7].iter() {
			print!("{:04x}, ", r.0);
		}
		
		println!("{:04x}]", self.reg.0[7].0);
		
		
		// print contents of stack
		print!("stack: ");
		
		// bp == sp
		if self.reg.0[7] == self.reg.0[6] {
			println!("nil");
		}
		// bp < sp
		else if self.reg.0[7] < self.reg.0[6] {
			println!("invalid");
		}
		else {
			let bp = self.reg.0[7].0 as usize;
			let sp = self.reg.0[6].0 as usize;
			
			print!("<");
			
			// log whole stack except for last value
			for &val in self.data_mem[sp..bp - 1].iter() {
				print!("{:04x}, ", val);
			}
			
			// log last value
			println!("{:04x}]", self.data_mem[bp - 1]);
		}
		
		print!("\n");
	}
	
	pub fn log_current_instr(&self) {
		// show which instruction is being executed
		// if invalid, log it and skip it.
		if let Some(ref instr) = Op::decode(self.ir).ok() {
			println!("ir = {:04x}: {}\n", self.ir, instr);
		} else {
			println!("ir = {:04x}: INVALID\n", self.ir);
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
					println_err!("Error parsing instruction 0x{:04x}", self.ir);
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
				self.reg[a] = !self.reg[a],
			
			Op::Increment(a) =>
				self.reg[a] += Wrapping(1),
			
			Op::Decrement(a) =>
				self.reg[a] -= Wrapping(1),
			
			Op::Push(r) => {
				self.reg[Reg::R6] -= Wrapping(1);
				let sp = self.reg[Reg::R6].0 as usize;
				swap!(self.reg[r].0, self.data_mem[sp]);
			}
			
			Op::Pop(r) => {
				let sp = self.reg[Reg::R6].0 as usize;
				swap!(self.reg[r].0, self.data_mem[sp]);
				self.reg[Reg::R6] += Wrapping(1);
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
					i16::MIN...-16383 => r + (r - i16::MIN + 1),
					
					_ => unreachable!()
				} as u16;
			}
			
			Op::Div2(ri) => {
				use std::i16;
				
				let r = self.reg[ri].0 as i16;
				
				self.reg[ri].0 = match r {
					0...i16::MAX if r & 1 == 1 => i16::MAX - (i16::MAX - r) / 2,
					i16::MIN...0 if r & 1 == 1 => i16::MIN + (r - (i16::MIN + 1)) / 2,
					_ /* even */               => r / 2,
				} as u16;
			}
			
		
			Op::RotLeftImm(r, v) =>
				self.reg[r].0 = self.reg[r].0.rotate_left(v as u32),
			
			Op::RotRightImm(r, v) =>
				self.reg[r].0 = self.reg[r].0.rotate_right(v as u32),
			
			
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
			
			Op::RotLeft(rr, ro) => {
				let bits = self.reg[ro].0;
				self.reg[rr].0 = self.reg[rr].0.rotate_left(bits as u32);
			}
			
			Op::RotRight(rr, ro) => {
				let bits = self.reg[ro].0;
				self.reg[rr].0 = self.reg[rr].0.rotate_right(bits as u32);
			}
			
			Op::IO(rd, rp) =>
				unimplemented!(),
			
			Op::CCNot(rc0, rc1, rn) =>
				self.reg[rn] ^= self.reg[rc0] & self.reg[rc1],
			
			Op::CSwap(rc, rs0, rs1) => {
				let c = self.reg[rc];
				let mut s0 = self.reg[rs0];
				let mut s1 = self.reg[rs1];
				
				let t = (s0 ^ s1) & c;
				s0 ^= t;
				s1 ^= t;
				
				self.reg[rs0] = s0;
				self.reg[rs1] = s1;
			}
			
			
			Op::BranchParity(r, off) =>
				if self.reg[r].0 % 2 == 1 {
					self.br += Wrapping(off as u16);
				},
			
			Op::AssertParity(r, off) =>
				if self.reg[r].0 % 2 == 1 {
					self.br -= Wrapping(off as u16);
				},
			
			Op::BranchSign(r, off) =>
				if (self.reg[r].0 as i16) < 0 {
					self.br += Wrapping(off as u16);
				},
			
			Op::AssertSign(r, off) =>
				if (self.reg[r].0 as i16) < 0 {
					self.br -= Wrapping(off as u16);
				},
			
			Op::Immediate(r, v) =>
				self.reg[r].0 ^= v as u16,
			
			
			Op::GoTo(off) =>
				self.br += Wrapping(off),
			
			Op::ComeFrom(off) =>
				self.br -= Wrapping(off),
		}
		
		// next instruction
		if self.dir { self.pc -= self.br }
		else { self.pc += self.br }
		
		false
	}
}


pub fn run<I: Read>(src: I, logging_enabled: bool) {
	let mut input = BufReader::new(src);
	
	let mut prog_mem = [0; MAX_MEM_LEN];
	let mut data_mem = [0; MAX_MEM_LEN];
	
	// read file contents into program memory
	for i in 0.. {
		let buffer = &mut [0, 0];
		
		match try_err!(input.read(buffer)) {
			0 => break,
			
			1 => panic!("Error: Got incomplete instruction."),
			
			2 => if i < MAX_MEM_LEN {
				let instr = (buffer[0] as u16) << 8 | buffer[1] as u16;
				prog_mem[i] = instr;
			} else {
				panic!("Error: Binary is too big for memory!");
			},
			
			_ => unreachable!(),
		}
	}
	
	let mut cpu = Rel16::new(&prog_mem, &mut data_mem);
	
	loop {
		if logging_enabled { cpu.log_state() }
		
		if cpu.fetch() { break }
		
		if logging_enabled { cpu.log_current_instr() }
		
		if cpu.execute() { break }
	}
}
