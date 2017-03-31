use std::collections::HashMap;

mod register_file;
//mod devices;

const MAX_MEM: usize = 65536;

use self::register_file::RegisterFile;
use super::isa::op::Op;
use super::isa::reg::Reg;

const SP: Reg = Reg::SP;
const BP: Reg = Reg::BP;

pub struct Cpu<'mem> {
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
	
	symtab: &'mem HashMap<String, usize>,
}

pub fn run(code: Vec<Op>, symtab: HashMap<String, usize>, logging_enabled: bool) {
	let mut cpu = Cpu {
			symtab: &symtab,
			dir: false,
			br: 1,
			pc: 0,
			ir: Op::Halt,
			reg: RegisterFile([0; 8]),
			prog_mem: &code[..],
			data_mem: [0; MAX_MEM],
	};
	
	loop {
		if logging_enabled {
			println!("pc = {:04x}  br = {:04x}  dir = {}", cpu.pc, cpu.br, cpu.dir);
			
			// print contents of registers
			print!("registers: [");
			
			for &val in &cpu.reg.0[..BP as usize] {
				print!("{:04x}, ", val);
			}
			
			println!("{:04x}]", cpu.reg[BP]);
			
			
			// print contents of stack
			print!("stack: ");
			
			use std::cmp::Ordering;
			
			match cpu.reg[BP].cmp(&cpu.reg[SP]) {
				Ordering::Equal   => println!("nil"),
				Ordering::Less    => println!("invalid"),
				Ordering::Greater => {
					let bp = cpu.reg[BP] as usize;
					let sp = cpu.reg[SP] as usize;
					
					print!("<");
					
					// log whole stack except for last value
					for &val in &cpu.data_mem[sp..bp - 1] {
						print!("{:04x}, ", val);
					}
					
					// log last value
					println!("{:04x}]", cpu.data_mem[bp - 1]);
				}
			}
			
			print!("\n");
		}
		
		/* FETCH */
		cpu.ir = match cpu.prog_mem.get(cpu.pc as usize) {
			Some(val) => val.clone(),
			None => {
				println!("UNEXPECTED HALT");
				println!("Encountered halt instruction that wasn't part of the program.");
				break;
			}
		};
		
		
		if logging_enabled {
			// show which instruction is being executed
			println!("ir: {:?}\n", cpu.ir);
		}
		
		
		/* EXECUTE */
		// get instruction and invert if in reverse mode
		if cpu.dir {
			cpu.ir = cpu.ir.clone().invert();
		}
		
		match cpu.ir {
			Op::Halt => break,
			
			Op::Not(a) =>
				cpu.reg[a] = !cpu.reg[a],
			
			Op::Increment(a) =>
				cpu.reg[a] = cpu.reg[a].wrapping_add(1),
			
			Op::Decrement(a) =>
				cpu.reg[a] = cpu.reg[a].wrapping_sub(1),
			
			Op::Push(r) => {
				cpu.reg[SP] = cpu.reg[SP].wrapping_sub(1);
				let sp = cpu.reg[SP] as usize;
				swap!(cpu.reg[r], cpu.data_mem[sp]);
			}
			
			Op::Pop(r) => {
				let sp = cpu.reg[SP] as usize;
				swap!(cpu.reg[r], cpu.data_mem[sp]);
				cpu.reg[SP] = cpu.reg[SP].wrapping_add(1);
			}
			
			Op::SwapPc(r) =>
				swap!(cpu.pc, cpu.reg[r]),
			
			Op::RevSwapPc(r) => {
				swap!(cpu.pc, cpu.reg[r]);
				cpu.dir = !cpu.dir;
			}
			
			Op::Mul2(ri) => {
				use std::i16;
				
				let r = cpu.reg[ri] as i16;
				
				cpu.reg[ri] = match r {
					-16384...16383    => r * 2,
					16384...i16::MAX  => r - (i16::MAX - r),
					i16::MIN...-16385 => r + (r - i16::MIN + 1),
					
					_ => unreachable!()
				} as u16;
			}
			
			Op::Div2(ri) => {
				use std::i16;
				
				let r = cpu.reg[ri] as i16;
				let odd = |r| r & 1 == 1;
				
				cpu.reg[ri] = match r {
					0...i16::MAX if odd(r) => i16::MAX - (i16::MAX - r) / 2,
					i16::MIN...0 if odd(r) => i16::MIN + (r + i16::MAX) / 2,
					_ /* even */           => r / 2
				} as u16;
			}
			
			Op::RotLeftImm(r, v) =>
				cpu.reg[r] = cpu.reg[r].rotate_left(v as u32),
			
			Op::RotRightImm(r, v) =>
				cpu.reg[r] = cpu.reg[r].rotate_right(v as u32),
			
			Op::Swap(a, b) =>
				cpu.reg.0.swap(a as usize, b as usize),
			
			Op::CNot(rn, rc) =>
				cpu.reg[rn] ^= cpu.reg[rc],
			
			Op::CAdd(ra, rc) =>
				cpu.reg[ra] = cpu.reg[ra].wrapping_add(cpu.reg[rc]),
			
			Op::CSub(rs, rc) =>
				cpu.reg[rs] = cpu.reg[rs].wrapping_sub(cpu.reg[rc]),
			
			Op::Exchange(r, ra) => {
				let raddr = cpu.reg[ra] as usize;
				swap!(cpu.reg[r], cpu.data_mem[raddr]);
			}
			
			Op::RotLeft(rr, ro) =>
				cpu.reg[rr] = cpu.reg[rr].rotate_left(cpu.reg[ro] as u32),
			
			Op::RotRight(rr, ro) =>
				cpu.reg[rr] = cpu.reg[rr].rotate_right(cpu.reg[ro] as u32),
			
			Op::CCNot(rc0, rc1, rn) =>
				cpu.reg[rn] ^= cpu.reg[rc0] & cpu.reg[rc1],
			
			Op::CSwap(rc, rs0, rs1) => {
				let c = cpu.reg[rc];
				let mut s0 = cpu.reg[rs0];
				let mut s1 = cpu.reg[rs1];
				
				let t = (s0 ^ s1) & c;
				s0 ^= t;
				s1 ^= t;
				
				cpu.reg[rs0] = s0;
				cpu.reg[rs1] = s1;
			}
			
			Op::BranchParity(r, ref label) =>
				if (cpu.reg[r] & 1) == 1 {
					let off = cpu.symtab[label];
					cpu.br = cpu.br.wrapping_add(off as u16);
				},
			
			Op::AssertParity(r, ref label) =>
				if (cpu.reg[r] & 1) == 1 {
					let off = cpu.symtab[label];
					cpu.br = cpu.br.wrapping_sub(off as u16);
				},
			
			Op::BranchSign(r, ref label) =>
				if (cpu.reg[r] as i16) < 0 {
					let off = cpu.symtab[label];
					cpu.br = cpu.br.wrapping_add(off as u16);
				},
			
			Op::AssertSign(r, ref label) =>
				if (cpu.reg[r] as i16) < 0 {
					let off = cpu.symtab[label];
					cpu.br = cpu.br.wrapping_sub(off as u16);
				},
			
			Op::Immediate(r, v) =>
				cpu.reg[r] ^= v as u16,
			
			Op::GoTo(ref label) => {
				let off = cpu.symtab[label];
				cpu.br = cpu.br.wrapping_add(off as u16);
			}
			
			Op::ComeFrom(ref label) => {
				let off = cpu.symtab[label];
				cpu.br = cpu.br.wrapping_sub(off as u16);
			}
		}
		
		// decide next instruction based on offset and dir bit
		if cpu.dir {
			cpu.pc = cpu.pc.wrapping_sub(cpu.br);
		} else {
			cpu.pc = cpu.pc.wrapping_add(cpu.br);
		}
	}
}
