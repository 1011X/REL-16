use super::reg::Reg;
use super::register_file::RegisterFile;
use super::device::DeviceManager;
use super::instruction::Instr;

const SP: Reg = Reg::SP;
const BP: Reg = Reg::R6;

pub const MAX_MEM: usize = 65536;

macro_rules! swap(
	($left: expr, $right: expr) => {
		std::mem::swap(&mut $left, &mut $right);
	}
);

pub struct Cpu<'pm, 'dev> {
	logging_enabled: bool,
	
	// when dir is true, it is in reverse mode
	dir: bool,
	br: u16,
	pc: u16,
	ir: Instr,
	
	// Some conventions on registers:
	// r0 can be treated like an accumulator register
	// r6 is the stack top pointer (sp)
	// r7 is the stack base pointer (bp)
	reg: RegisterFile,
	
	prog_mem: &'pm [Instr],      // ROM
	data_mem: [u16; MAX_MEM], // RAM
	devices: &'dev mut DeviceManager,
}

impl Cpu<'_, '_> {
	pub fn new<'pm, 'dev>(prog: &'pm [Instr], dev: &'dev mut DeviceManager, log: bool) -> Cpu<'pm, 'dev> {
		Cpu {
			logging_enabled: log,
			
			dir: false,
			br: 1,
			pc: 0,
			ir: Instr::Halt(0),
			reg: RegisterFile([0; 8]),
			
			prog_mem: prog,
			data_mem: [0; MAX_MEM],
			devices: dev,
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
			
			for &val in &self.reg.0[..SP as usize] {
				print!("{:04x}, ", val);
			}
			
			print!("{:04x}]\n", self.reg[SP]);
			
			
			// print contents of stack
			print!("stack: ");
			
			use std::cmp::Ordering;
			
			match self.reg[BP].cmp(&self.reg[SP]) {
				Ordering::Equal   => println!("nil"),
				Ordering::Less    => println!("invalid"),
				Ordering::Greater => {
					let bp = self.reg[BP] as usize;
					let sp = self.reg[SP] as usize;
					
					print!("<");
					
					// log whole stack except for last value
					for val in &self.data_mem[sp..bp - 1] {
						print!("{:04x}, ", val);
					}
					
					// log last value
					println!("{:04x}]", self.data_mem[bp - 1]);
				}
			}
			
			println!();
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
		
		
		// show which instruction is being executed
		if self.logging_enabled {
			println!("ir: {:?}\n", self.ir);
		}
	
		match self.ir {
			Instr::Halt(_) => return true,
			
			Instr::Not(a, _) =>
				self.reg[a] = !self.reg[a],
			
			Instr::SwapPc(r, _) =>
				swap!(self.pc, self.reg[r]),
		
			Instr::RevSwapPc(r, _) => {
				swap!(self.pc, self.reg[r]);
				self.dir = !self.dir;
			}
		
			Instr::Swap(a, b) =>
				self.reg.0.swap(a as usize, b as usize),
			
			Instr::Exchange(rd, raddr) => {
				let mut d = 0;
				swap!(d, self.reg[rd]);
				
				let addr = self.reg[raddr] as usize;
				swap!(d, self.data_mem[addr]);
				
				swap!(d, self.reg[rd]);
				debug_assert!(d == 0);
			}
			
			Instr::Xor(rn, rc) => {
				let mut n = 0;
				swap!(n, self.reg[rn]);
				
				n ^= self.reg[rc];
				
				swap!(n, self.reg[rn]);
				debug_assert!(n == 0);
			}
			
			Instr::Add(ra, rc) => {
				let mut a = 0;
				swap!(a, self.reg[ra]);
				
				a = a.wrapping_add(self.reg[rc]);
				
				swap!(a, self.reg[ra]);
				debug_assert!(a == 0);
			}
			
			Instr::Sub(rs, rc) => {
				let mut s = 0;
				swap!(s, self.reg[rs]);
				
				s = s.wrapping_sub(self.reg[rc]);
				
				swap!(s, self.reg[rs]);
				debug_assert!(s == 0);
			}
			
			Instr::LRot(rr, ro) => {
				let mut r = 0;
				swap!(r, self.reg[rr]);
				
				r = r.rotate_left(self.reg[ro] as u32);
				
				swap!(r, self.reg[rr]);
				debug_assert!(r == 0);
			}
			
			Instr::RRot(rr, ro) => {
				let mut r = 0;
				swap!(r, self.reg[rr]);
				
				r = r.rotate_right(self.reg[ro] as u32);
				
				swap!(r, self.reg[rr]);
				debug_assert!(r == 0);
			}
			
			
			Instr::ImmLow(r, v) =>
				self.reg[r] ^= v as u16,
			
			Instr::ImmHigh(r, v) =>
				self.reg[r] ^= (v as u16) << 8,
			
			Instr::IO(r, p) => {
			    if self.devices.call(self.dir, &mut self.reg[r], p) {
			        eprintln!("Error while trying to call device.");
			        eprintln!("reg = {}  data = {}  port = {}", r, self.reg[r], p);
			        return true;
			    }
			    todo!()
			}
			
			// Swap the modified register's value to a zeroed
			// location, so that in case it's also used as a control
			// register, then this instruction just becomes a no-op.
			Instr::CCNot(rc0, rc1, rn) => {
				let mut n = 0;
				swap!(n, self.reg[rn]);
				
				n ^= self.reg[rc0] & self.reg[rc1];
				
				swap!(n, self.reg[rn]);
				debug_assert!(n == 0);
			}
			
			// Swap the modified registers' values to zeroed
			// locations, just like the CCNot instruction.
			Instr::CSwap(rc, rs0, rs1) => {
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
			
			Instr::JumpOdd(r, off) =>
				if (self.reg[r] & 1) == 1 {
					self.br ^= off as u16;
				}
			
			Instr::JumpNeg(r, off) =>
				if (self.reg[r] as i16) < 0 {
					self.br ^= off as u16;
				}
			
			Instr::Jump(off) =>
				self.br ^= off,
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
