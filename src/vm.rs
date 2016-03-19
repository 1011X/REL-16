use instr::Op;
use instr::decode;

use std::mem;
use std::fs::File;
use std::io::Read;
use std::path::Path;

pub fn vm(file_path: &Path) {
	let mut input = File::open(file_path).unwrap();
	
	let mut pc: usize = 0;
	//let mut sp: usize = 65535;
	let mut ir: u16;
	let mut reg = [0_u16; 16];
	let mut mem = [0_u16; 65536];
	
	/*
	Sample addition program that takes args a and b:
	
	let program = [
		// addition using bit-ops
		0x1001, // lit 0 r1   ; r1 = 0
		0x1002, // lit 0 r2
		0x0012, // not r2     ; r2 = 0xFFFF
		
		0x1023, // lit 2 r3   ; a = 2
		0x1024, // lit 2 r4   ; b = 2
		0x1005, // lit 0 r5   ; carry = 0
		0x1006, // lit 0 r6   ; result = 0
		
		0x4345, // toff a b carry   ; carry = a & b
		0x0234, // cnot a b         ; b ^= a
		0x4246, // toff r2 b result ; result = b
		0x0234, // cnot a b         ; b ^= a
		0x0105, // swap r0 carry
		
		0x7018, // if r0 == 0, goto 24
		
		0x0105, // swap r0 carry
		0x1007, // lit 0 r7   ; shiftedcarry = 0
		0x1018, // lit 1 r8   ; temp = 1
		0x0018, // not temp   ; temp = ~temp
		0x0025, // rotl carry
		0x4257, // toff temp carry shiftedcarry   ; shiftedcarry = carry & temp
		0x1005, // lit 0 r5                       ; carry = 0
		0x4675, // toff result shiftedcarry carry ; carry = result & shiftedcarry
		0x0276, // cnot shiftedcarry result       ; result ^= shiftedcarry
		0x0105, // swap r0 carry
		0x600C, // jump 12
		
		0x0000  // halt
	];
	*/
	
	// read file contents into mem
	let mut buffer = [0_u8; 2];
	for i in 0.. {
		match input.read(&mut buffer) {
			Ok(0) => break,
			
			Ok(1) => panic!("Got incomplete instruction."),
			
			Ok(2) => if i < mem.len() {
				mem[i] = (buffer[0] as u16) << 8 | buffer[1] as u16;
			} else {
				panic!("Binary too big for memory!");
			},
			
			Ok(_) => unreachable!(),
			
			Err(e) => panic!("{}", e),
		}
	}
	
	loop {
		// fetch
		ir = mem[pc];
		pc += 1;
		
		// execute
		match decode(ir) {
			Op::Lit(r, v) => reg[r] = v as u16,
			
			Op::MemSwap(r, addr) => mem::swap(&mut reg[r], &mut mem[addr]),
			
			Op::Halt => break,
			
			
			Op::Not(a) => reg[a] = !reg[a],
			
			Op::RotateLeft(a) => reg[a] = reg[a].rotate_left(1),
			Op::RotateRight(a) => reg[a] = reg[a].rotate_right(1),
			
			Op::Increment(a) => reg[a] = reg[a].wrapping_add(1),
			Op::Decrement(a) => reg[a] = reg[a].wrapping_sub(1),
			
			
			Op::Swap(a, b) => reg.swap(a, b),
			
			Op::CNot(a, b) => reg[b] ^= reg[a],
			
			//Op::CAdd(a, b) => reg[b] += reg[a],
			
			
			Op::Toffoli(a, b, c) => if a != c && b != c {
				reg[c] ^= reg[a] & reg[b];
			} else {
				panic!("ERROR (line {}): Control register in Toffoli instruction used again in last parameter.", pc - 1);
			},
			
			Op::Fredkin(a, b, c) => if a != c && a != b {
				let s = (reg[b] ^ reg[c]) & reg[a];
				reg[b] ^= s;
				reg[c] ^= s;
			} else {
				panic!("ERROR (line {}): Control register in Fredkin instruction used again in second or last parameter.", pc - 1);
			},
			
			Op::Jump(addr) => pc = addr,
			
			Op::JZero(addr) => if reg[0] == 0 {pc = addr},
			
			/*
			Read(c) => {
				let mut buf = [0_u8, 0];
				std::io::stdin().read(&mut buf).expect("Couldn't read from stdin.");
				reg[0] = ((buf[0] as u16) << 8) | buf[1] as u16;
			}
			
			Write(c) => print!("{}", (reg[0] as u8) as char),
			*/
		}
		
		print!("PC = {:>5}: ", pc);
		
		print!("{:<20}", format!("{:?}", decode(ir)));
		
		print!("[");
		for (i, &r) in reg.iter().enumerate() {
			print!("0x{:04X}", r);
			
			// is not last item
			if i != reg.len() - 1 {
				print!(", ");
			}
		}
		println!("]");
		println!("");
	}
}
