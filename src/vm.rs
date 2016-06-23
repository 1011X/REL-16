use std::io::{Read, BufReader};

use instr::Op;


const MAX_MEM_LEN: usize = 65536;


pub fn run<I: Read>(src: I) {
	let mut input = BufReader::new(src);
	
	let mut data_mem = [0_u16; MAX_MEM_LEN];
	let mut prog_mem = Vec::with_capacity(MAX_MEM_LEN);
	
	// read file contents into program memory
	for i in 0.. {
		let mut buffer = [0_u8; 2];
		
		match try_err!(input.read(&mut buffer)) {
			0 => {
				prog_mem.shrink_to_fit();
				break;
			}
			
			1 => {
				println_err!("Error: Got incomplete instruction.");
				return;
			}
			
			2 => if i < MAX_MEM_LEN {
				let instr = (buffer[0] as u16) << 8 | buffer[1] as u16;
				prog_mem.push(instr);
			}
			else {
				println_err!("Error: Binary is too big for memory!");
				return;
			},
			
			_ => unreachable!(),
		}
	}
	
	// when dir is true, it is in reverse mode
	let mut dir = false;
	let mut br: u16 = 1;
	let mut pc: u16 = 0;
	let mut ir: u16;
	// Some conventions on registers:
	// r0 can be treated like an accumulator register
	// r6 is the stack top pointer (sp)
	// r7 is the stack base pointer (bp)
	let mut reg = [0_u16; 8];
	
	loop {
		// debugging code
		{
			// address in pc and the instruction it's pointing to
			println!("pc = {:04x}  br = {:04x}  dir = {}", pc, br, dir);
			
			
			// print contents of registers
			print!("registers: [");
		
			for (i, &r) in reg.iter().enumerate() {
				print!("{:04x}", r);
			
				// is not last item
				if i != reg.len() - 1 {
					print!(", ");
				}
			}
			
			println!("]");
			
			
			// print contents of stack
			print!("stack: ");
			
			// bp == sp
			if reg[7] == reg[6] {
				println!("nil");
			}
			// bp < sp
			else if reg[7] < reg[6] {
				println!("invalid");
			}
			else {
				let bp = reg[7] as usize;
				let sp = reg[6] as usize;
				
				print!("<");
				
				for (i, &val) in data_mem[sp..bp].iter().enumerate() {
					print!("{:04x}", val);
					
					// is not last item; excludes base of stack
					if sp + i != bp - 1 {
						print!(", ");
					}
				}
				
				println!("]");
			}
			
			
			print!("\n");
		}
		
		// fetch
		ir = *prog_mem.get(pc as usize).unwrap_or(&0x0000);
		
		// get instruction and invert if in reverse mode
		let instr = {
			let instr = try_err!(Op::decode(ir));
			if dir { instr.invert() }
			else { instr }
		};
		
		// show which instruction is being executed
		println!("ir = {:04x}: {}\n", ir, instr);
		
		// execute
		match instr {
			Op::Halt => break,
			Op::Reverse => dir = !dir,
			
			
			Op::Not(a)       => reg[a as usize] = !reg[a as usize],
			Op::Increment(a) => reg[a as usize] = reg[a as usize].wrapping_add(1),
			Op::Decrement(a) => reg[a as usize] = reg[a as usize].wrapping_sub(1),
			Op::Push(r) => {
				let sp = reg[6].wrapping_sub(1);
				swap!(reg[r as usize], data_mem[sp as usize]);
				reg[6] = sp;
			}
			
			Op::Pop(r) => {
				let sp = reg[6];
				swap!(reg[r as usize], data_mem[sp as usize]);
				reg[6] = sp.wrapping_add(1);
			}
			
			Op::SwapPc(r)    => swap!(pc, reg[r as usize]),
			Op::RevSwapPc(r) => {
				swap!(pc, reg[r as usize]);
				dir = !dir;
			}
			
			Op::Exchange(r, ra) => {
				let raddr = reg[ra as usize];
				swap!(reg[r as usize], data_mem[raddr as usize]);
			}
			
			
			Op::RotLeftImm(r, v)  =>
				reg[r as usize] = reg[r as usize].rotate_left(v as u32),
			Op::RotRightImm(r, v) =>
				reg[r as usize] = reg[r as usize].rotate_right(v as u32),
			
			
			Op::Swap(a, b) => reg.swap(a as usize, b as usize),			
			Op::CNot(rn, rc) => {
				let c = reg[rc as usize];
				
				reg[rn as usize] ^= c;
			}
			
			Op::CAdd(ra, rc) => {
				let c = reg[rc as usize];
				let a = reg[ra as usize];
				
				reg[ra as usize] = a.wrapping_add(c);
			}
			
			Op::CSub(rs, rc) => {
				let c = reg[rc as usize];
				let s = reg[rs as usize];
				
				reg[rs as usize] = s.wrapping_sub(c);
			}
			
			Op::RotLeft(rr, ro) => {
				let bits = reg[ro as usize];
				
				reg[rr as usize] = reg[rr as usize].rotate_left(bits as u32);
			}
			
			Op::RotRight(rr, ro) => {
				let bits = reg[ro as usize];
				
				reg[rr as usize] = reg[rr as usize].rotate_right(bits as u32);
			}
			
			
			Op::CCNot(rc0, rc1, rn) => {
				let c0 = reg[rc0 as usize];
				let c1 = reg[rc1 as usize];
				
				reg[rn as usize] ^= c0 & c1;
			}
			
			Op::CSwap(rc, rs0, rs1) => {
				let     c  = reg[rc as usize];
				let mut s0 = reg[rs0 as usize];
				let mut s1 = reg[rs1 as usize];
				
				let t = (s0 ^ s1) & c;
				s0 ^= t;
				s1 ^= t;
				
				reg[rs0 as usize] = s0;
				reg[rs1 as usize] = s1;
			}
			
			
			
			Op::BranchOdd(r, off) => if reg[r as usize] % 2 == 1 {
				br = br.wrapping_add(off as u16);
			},
			
			Op::AssertEven(r, off) => if reg[r as usize] % 2 == 0 {
				br = br.wrapping_sub(off as u16);
			},
			
			Op::BranchEven(r, off) => if reg[r as usize] % 2 == 0 {
				br = br.wrapping_add(off as u16);
			},
			
			Op::AssertOdd(r, off) => if reg[r as usize] % 2 == 1 {
				br = br.wrapping_sub(off as u16);
			},
			
			Op::BranchNeg(r, off) => if (reg[r as usize] as i16) < 0 {
				br = br.wrapping_add(off as u16);
			},
			
			Op::AssertNonneg(r, off) => if (reg[r as usize] as i16) >= 0 {
				br = br.wrapping_sub(off as u16);
			},
			
			Op::BranchNonneg(r, off) => if (reg[r as usize] as i16) >= 0 {
				br = br.wrapping_add(off as u16);
			},
			
			Op::AssertNeg(r, off) => if (reg[r as usize] as i16) < 0 {
				br = br.wrapping_sub(off as u16);
			},
			
			Op::Immediate(r, v) => reg[r as usize] ^= v as u16,
			
			
			
			Op::GoTo(off)     => br = br.wrapping_add(off),
			Op::ComeFrom(off) => br = br.wrapping_sub(off),
		}
		
		
		// next instruction
		pc = if dir { pc.wrapping_sub(br) }
			else { pc.wrapping_add(br) };
	}
}
