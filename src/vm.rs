use std::fs::File;
use std::io::Read;
use std::path::Path;

use instr::Op;


const MAX_MEM_LEN: usize = 65536;


pub fn vm(file_path: &Path) {
	let mut input = try_err!(File::open(file_path));
	
	let mut data_mem = [0_u16; MAX_MEM_LEN];
	let mut prog_mem = Vec::with_capacity(MAX_MEM_LEN);
	
	// read file contents into program memory
	let mut buffer = [0_u8; 2];
	for i in 0.. {
		match try_err!(input.read(&mut buffer)) {
			0 => {
				prog_mem.shrink_to_fit();
				break;
			}
			
			1 => {
				println_err!("Got incomplete instruction.");
				return;
			}
			
			2 => if i < MAX_MEM_LEN {
				let instr = (buffer[0] as u16) << 8 | buffer[1] as u16;
				prog_mem.push(instr);
			}
			else {
				println_err!("Binary is too big for memory!");
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
			Op::Immediate(r, v) => reg[r] ^= v as u16,
			
			Op::Exchange(r, ra) => {
				let raddr = reg[ra];
				swap!(reg[r], data_mem[raddr as usize]);
			}
			
			Op::Halt => break,
			
			
			Op::Not(a) => reg[a] = !reg[a],
			
			Op::RotateLeft(a)  => reg[a] = reg[a].rotate_left(1),
			Op::RotateRight(a) => reg[a] = reg[a].rotate_right(1),
			
			Op::Increment(a) => reg[a] = reg[a].wrapping_add(1),
			Op::Decrement(a) => reg[a] = reg[a].wrapping_sub(1),
			
			Op::Push(r) => {
				let sp = reg[6].wrapping_sub(1);
				swap!(reg[r], data_mem[sp as usize]);
				reg[6] = sp;
			}
			
			Op::Pop(r) => {
				let sp = reg[6];
				swap!(reg[r], data_mem[sp as usize]);
				reg[6] = sp.wrapping_add(1);
			}
			
			Op::SwapBr(r) => swap!(pc, reg[r]),
			
			Op::RevSwapBr(r) => {
				swap!(pc, reg[r]);
				dir = !dir;
			}
			
			
			Op::Swap(a, b) => reg.swap(a, b),
			
			Op::CNot(a, b) => {
				let mut rc = 0;
				let mut rn = 0;
				
				swap!(rc, reg[a]);
				swap!(rn, reg[b]);
				
				rn ^= rc;
				
				swap!(rc, reg[a]);
				swap!(rn, reg[b]);
			}
			
			Op::CAdd(a, b) => {
				use std::num::Wrapping;
				
				let mut rc = Wrapping(0);
				let mut ra = Wrapping(0);
				
				swap!(rc.0, reg[a]);
				swap!(ra.0, reg[b]);
				
				ra += rc;
				
				swap!(rc.0, reg[a]);
				swap!(ra.0, reg[b]);
			}
			
			Op::CSub(a, b) => {
				use std::num::Wrapping;
				
				let mut rc = Wrapping(0);
				let mut rs = Wrapping(0);
				
				swap!(rc.0, reg[a]);
				swap!(rs.0, reg[b]);
				
				rs -= rc;
				
				swap!(rc.0, reg[a]);
				swap!(rs.0, reg[b]);
			}
			
			
			Op::CCNot(a, b, c) => {
				let mut rc0 = 0;
				let mut rc1 = 0;
				let mut rn  = 0;
				
				swap!(rc0, reg[a]);
				swap!(rc1, reg[b]);
				swap!(rn,  reg[c]);
				
				rn ^= rc0 & rc1;
				
				swap!(rc0, reg[a]);
				swap!(rc1, reg[b]);
				swap!(rn,  reg[c]);
			}
			
			Op::CSwap(a, b, c) => {
				let mut rc = 0;
				let mut rl = 0;
				let mut rr = 0;
				
				swap!(rc, reg[a]);
				swap!(rl, reg[b]);
				swap!(rr, reg[c]);
				
				let s = (rl ^ rr) & rc;
				rl ^= s;
				rr ^= s;
				
				swap!(rc, reg[a]);
				swap!(rl, reg[b]);
				swap!(rr, reg[c]);
			}
			
			Op::GoTo(off)     => br = br.wrapping_add(off),
			Op::ComeFrom(off) => br = br.wrapping_sub(off),
			/*
			Op::BranchOdd(r, off) => if reg[r] % 2 == 1 {
				br = br.wrapping_add(off as u16);
			},
			
			Op::BranchSign(r, off) => if (reg[r] as i16) < 0 {
				br = br.wrapping_add(off as u16);
			},
			
			Op::Read(c) => {
				let mut buf = [0_u8, 0];
				std::io::stdin().read(&mut buf).expect("Couldn't read from stdin.");
				reg[0] = ((buf[0] as u16) << 8) | buf[1] as u16;
			}
			
			Op::Write(c) => print!("{}", (reg[0] as u8) as char),
			*/
		}
		
		
		// next instruction
		pc = if dir { pc.wrapping_sub(br) }
			else { pc.wrapping_add(br) };
	}
}
