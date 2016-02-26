use std::env;
use std::fs::File;
use std::io::Read;

enum Op {
	Halt,
	Lit(usize, u8),             // aaaavvvvvvvv
	Load(usize),                // aaaaaaaaaaaa
	Store(usize),               // aaaaaaaaaaaa
	
			Not(usize),         // 00000000aaaa
			
			RotateLeft(usize),  // 00000000aaaa
			RotateRight(usize), // 00000000aaaa
			
			Increment(usize),   // 00000000aaaa
			Decrement(usize),   // 00000000aaaa
			
			//Push(usize),
			//Pop(usize),
			
			//Read(usize),      // 00000000dddd
			//Write(usize),     // 00000000dddd
	
		Swap(usize, usize),     // 0000aaaabbbb
		CNot(usize, usize),     // 0000aaaabbbb
	
	Toffoli(usize, usize, usize),  // aaaabbbbcccc
	Fredkin(usize, usize, usize), // aaaabbbbcccc
	
	Jump(usize),                // aaaaaaaaaaaa
	JZero(usize),               // aaaaaaaaaaaa
}

#[allow(unused_variables)]
fn decode(inst: u16) -> Op {
	let opcode = (inst & 0xF000) >> 12;
	let data = inst & 0x0FFF;
	let a = (data & 0xF00) >> 8;
	let b = (data & 0x0F0) >> 4;
	let c = data & 0x00F;
	let ab = (data & 0xFF0) >> 4;
	let bc = data & 0x0FF;
	
	match opcode {
		0 => match a {
			0 => match b {
				// signals
				0 => match c {
					0 => Op::Halt,
					
					c if c < 16 => panic!("Invalid signal ({})", c),
					_ => unreachable!()
				},
				
				1 => Op::Not(c as usize),
				
				2 => Op::RotateLeft(c as usize),
				3 => Op::RotateRight(c as usize),
				
				4 => Op::Increment(c as usize),
				5 => Op::Decrement(c as usize),
				
				//4 => Push(c as usize),
				//5 => Pop(c as usize),
				
				//4 => Read(c as usize),
				//5 => Write(c as usize),
				
				b if b < 16 => panic!("Invalid 1-arg opcode ({})", b),
				_ => unreachable!()
			},
			
			1 => Op::Swap(b as usize, c as usize),
			2 => Op::CNot(b as usize, c as usize),
			
			a if a < 16 => panic!("Invalid 2-arg opcode ({})", a),
			_ => unreachable!()
		},
		
		1 => Op::Lit(a as usize, bc as u8),
		2 => Op::Load(data as usize),
		3 => Op::Store(data as usize),
		
		// CCNot
		4 => Op::Toffoli(a as usize, b as usize, c as usize),
		// CSwap
		5 => Op::Fredkin(a as usize, b as usize, c as usize),
		
		6 => Op::Jump(data as usize),
		7 => Op::JZero(data as usize),
		
		opcode if opcode < 16 => panic!("Invalid opcode ({})! Ahhhh!", opcode),
		_ => unreachable!()
	}
}

fn main() {
	let mut pc: usize = 0;
	//let mut sp : usize = 65535;
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
	
	let mut input = if let Some(s) = env::args().nth(1) {
		match File::open(s) {
			Ok(file) => file,
			Err(e) => {
				println!("{}", e);
				return;
			}
		}
	} else {
		println!("No input given; ending.");
		return;
	};
	
	// read file contents into mem
	let mut buffer = [0_u8; 2];
	for i in 0.. {
		match input.read(&mut buffer) {
			Ok (0) => break,
			
			Ok (1) => {
				println!("Incomplete instruction found.");
				return;
			}
			
			Ok (2) => if i >= mem.len() {
				println!("File too big for memory!");
				return;
			},
			
			Ok (_) => unreachable!(),
			
			Err (e) => {
				println!("{}", e);
				return;
			}
		}
		
		mem[i] = (buffer[0] as u16) << 8 | buffer[1] as u16;
	}
	
	loop {
		// fetch
		ir = mem[pc];
		pc += 1;
		
		// execute
		match decode(ir) {
			Op::Lit(r, v) => reg[r] = v as u16,
			
			Op::Load(addr) => reg[0] = mem[addr],
			Op::Store(addr) => mem[addr] = reg[0],
			
			Op::Halt => break,
			
			
			Op::Not(a) => reg[a] = !reg[a],
			
			Op::RotateLeft(a) => reg[a] = reg[a].rotate_left(1),
			Op::RotateRight(a) => reg[a] = reg[a].rotate_right(1),
			
			Op::Increment(a) => reg[a] = reg[a].wrapping_add(1),
			Op::Decrement(a) => reg[a] = reg[a].wrapping_sub(1),
			
			
			Op::Swap(a, b) => reg.swap(a, b),
			
			Op::CNot(a, b) => reg[b] ^= reg[a],
			
			
			Op::Toffoli(a, b, c) => {
				if a == c || b == c {
					panic!("ERROR (line {}): Control register in Toffoli instruction used again in last parameter.", pc - 1);
				}
				reg[c] ^= reg[a] & reg[b]
			}
			
			Op::Fredkin(a, b, c) => {
				if a == c || a == b {
					panic!("ERROR (line {}): Control register in Fredkin instruction used again in second or last parameter.", pc - 1);
				}
				let s = (reg[b] ^ reg[c]) & reg[a];
				reg[b] ^= s;
				reg[c] ^= s;
			}
			
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
		
		print!("0x{:04X}\t", ir);
		print!("{:?}\t", reg);
		println!("pc = {}", pc);
	}
}
