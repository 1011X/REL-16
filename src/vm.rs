use instr::Op;

use std::mem::swap;
use std::fs::File;
use std::io::Read;
use std::path::Path;

const DMEM_LEN: usize = 65536;
const PMEM_LEN: usize = 65536;

pub fn vm(file_path: &Path) {
	let mut input = try_err!(File::open(file_path));
	
	// when dir is true, it is in backwards mode
	let mut dir = false;
	let mut br: u16 = 1;
	let mut pc: u16 = 0;
	
	let mut ir: u16;
	let mut reg = [0_u16; 8];
	let mut data_mem = [0_u16; DMEM_LEN];
	let mut program_mem = Vec::with_capacity(PMEM_LEN);
	
	reg[7] = 0xFFFF; // use r7 as stack pointer
	
	// read file contents into mem
	let mut buffer = [0_u8; 2];
	for i in 0.. {
		match try_err!(input.read(&mut buffer)) {
			0 => {
				program_mem.shrink_to_fit();
				break;
			}
			
			1 => {
				println_err!("Got incomplete instruction.");
				return;
			}
			
			2 => if i < program_mem.capacity() {
				let instr = (buffer[0] as u16) << 8 | buffer[1] as u16;
				
				program_mem.push(instr);
			}
			else {
				println_err!("Binary is too big for memory!");
				return;
			},
			
			_ => unreachable!(),
		}
	}
	
	// redeclare so program memory is read-only.
	let program_mem = program_mem;
	
	loop {
		// fetch
		ir = program_mem.get(pc as usize).cloned().unwrap_or(0);
		
		let mut op = Op::decode(ir);
		
		if dir { op = op.invert() }
		
		// execute
		match op {
			Op::Immediate(r, v) => reg[r] ^= v as u16,
			
			Op::Exchange(r, ra) => {
				let raddr = reg[ra];
				swap(&mut reg[r], &mut data_mem[raddr as usize]);
			}
			
			Op::Halt => break,
			
			
			Op::Not(a) => reg[a] = !reg[a],
			
			Op::RotateLeft(a)  => reg[a] = reg[a].rotate_left(1),
			Op::RotateRight(a) => reg[a] = reg[a].rotate_right(1),
			
			Op::Increment(a) => reg[a] = reg[a].wrapping_add(1),
			Op::Decrement(a) => reg[a] = reg[a].wrapping_sub(1),
			
			Op::Push(r) => {
				let mut sp = reg[7] as usize;
				sp -= 1;
				swap(&mut reg[r], &mut data_mem[sp]);
				reg[7] = sp as u16;
			}
			
			Op::Pop(r) => {
				let mut sp = reg[7] as usize;
				swap(&mut reg[r], &mut data_mem[sp]);
				sp += 1;
				reg[7] = sp as u16;
			}
			/*
			Op::SwapBr(r) => swap(&mut br, &mut reg[r]),
			
			Op::RevSwapBr(r) => {
				swap(&mut br, &mut reg[r]);
				dir ^= true;
			}
			*/
			
			
			Op::Swap(a, b) => reg.swap(a, b),
			
			Op::CNot(a, b) => reg[b] ^= reg[a],
			
			Op::CAdd(a, b) => reg[b] = reg[b].wrapping_add(reg[a]),
			Op::CSub(a, b) => reg[b] = reg[b].wrapping_sub(reg[a]),
			
			
			Op::CCNot(a, b, c) => if a != c && b != c {
				reg[c] ^= reg[a] & reg[b];
			} else {
				panic!("Error (line {}): Control register in CCNot instruction used again in last parameter.", pc - 1);
			},
			
			Op::CSwap(a, b, c) => if a != c && a != b {
				let s = (reg[b] ^ reg[c]) & reg[a];
				reg[b] ^= s;
				reg[c] ^= s;
			} else {
				panic!("Error (line {}): Control register in CSwap instruction used again in second or last parameter.", pc - 1);
			},
			
			Op::GoTo(off)     => br = br.wrapping_add(off),
			Op::ComeFrom(off) => br = br.wrapping_sub(off),
			/*
			Op::BrGEZ(r, off) => br +=
				if (reg[r] as i16) >= 0 { off as u16 }
				else { 0 },
			
			Op::BrLZ(r, off) => br +=
				if (reg[r] as i16) < 0 { off as u16 }
				else { 0 },
			
			Read(c) => {
				let mut buf = [0_u8, 0];
				std::io::stdin().read(&mut buf).expect("Couldn't read from stdin.");
				reg[0] = ((buf[0] as u16) << 8) | buf[1] as u16;
			}
			
			Write(c) => print!("{}", (reg[0] as u8) as char),
			*/
		}
		
		// debugging code
		{
			// address in pc and the instruction it's pointing to
			println!("PC = {:04x}: {}", pc, op);
		
			// print contents of registers
			print!("Registers: [");
		
			for (i, &r) in reg[..reg.len() - 1].iter().enumerate() {
				print!("{:04x}", r);
			
				// is not last item
				if i != reg.len() - 2 {
					print!(", ");
				}
			}
		
			println!("]");
		
			// print contents of stack
			print!("SP = {:04x}: ", reg[7]);
		
			if reg[7] as usize == DMEM_LEN - 1 {
				println!("nil");
			}
			else {
				let sp = reg[7] as usize;
			
				print!("<");
			
				for (i, &val) in data_mem[sp..DMEM_LEN - 1].iter().enumerate() {
					print!("{:04x}", val);
				
					// is not last item; excludes zero at bottom of stack
					if sp + i < DMEM_LEN - 2 {
						print!(", ");
					}
				}
			
				println!("]");
			}
			
			print!("\n");
		}
		
		
		// next instruction
		if dir {
			pc -= br;
		} else {
			pc += br;
		}
	}
}
