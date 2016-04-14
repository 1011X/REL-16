#[derive(Debug)]
pub enum Op {
	Halt,
	
	
	Not(usize),
			
	RotateLeft(usize),
	RotateRight(usize),
			
	Increment(usize),
	Decrement(usize),
	
	Push(usize),
	Pop(usize),
	
	//Read(usize),
	//Write(usize),
	
	
	Swap(usize, usize),
	CNot(usize, usize),
	
	CAdd(usize, usize),
	CSub(usize, usize),
	
	
	Immediate(usize, u8),
	Exchange(usize, usize),
	
	CCNot(usize, usize, usize),
	
	CSwap(usize, usize, usize),
	
	Branch(u16),
	BrGEZ(usize, u8),
	BrLZ(usize, u8),
	BrEven(usize, u8),
	BrOdd(usize, u8),
	
	SwapBr(usize),
	RevSwapBr(usize),
}

pub fn encode(op: Op) -> u16 {
	// TODO: decide if I want to have assertions for values.
	match op {
		Op::Halt => 0x0000,
		
		
		
		Op::Not(reg) => 0x0010 | reg as u16,
		
		
		Op::RotateLeft(reg) => 0x0020 | reg as u16,
		
		Op::RotateRight(reg) => 0x0030 | reg as u16,
		
		
		Op::Increment(reg) => 0x0040 | reg as u16,
		
		Op::Decrement(reg) => 0x0050 | reg as u16,
		
		
		Op::Push(reg) => 0x0060 | reg as u16,
		
		Op::Pop(reg) => 0x0070 | reg as u16,
		
		
		
		Op::Swap(r_left, r_right) => 0x0100
			| (r_left as u16) << 4
			| r_right as u16,
		
		Op::CNot(r_ctrl, r_not) => 0x0200
			| (r_ctrl as u16) << 4
			| r_not as u16,
		
		Op::CAdd(r_ctrl, r_add) => 0x0300
			| (r_ctrl as u16) << 4
			| r_add as u16,
		
		Op::CSub(r_ctrl, r_add) => 0x0400
			| (r_ctrl as u16) << 4
			| r_add as u16,
		
		
		Op::Immediate(reg, val) => 0x1000
			| (reg as u16) << 8
			| val as u16,
		
		Op::Exchange(reg, addr) => 0x2000
			| (reg as u16) << 8
			| addr as u16,
		
		
		Op::CCNot(r_ctrl_0, r_ctrl_1, r_not) => 0x3000
			| (r_ctrl_0 as u16) << 8
			| (r_ctrl_1 as u16) << 4
			| r_not as u16,
		
		Op::CSwap(r_ctrl, r_swap_0, r_swap_1) => 0x4000
			| (r_ctrl as u16) << 8
			| (r_swap_0 as u16) << 4
			| r_swap_1 as u16,
		
		
		// control flow
		
		Op::Branch(off) => 0x6000 | off,
		
		Op::BrLZ(reg, off) => 0x7000
			| (reg as u16) << 8
			| off as u16,
		
		Op::BrGEZ(reg, off) => 0x8000
			| (reg as u16) << 8
			| off as u16,
		
		Op::BrEven(reg, off) => 0x9000
			| (reg as u16) << 8
			| off as u16,
		
		Op::BrOdd(reg, off) => 0xA000
			| (reg as u16) << 8
			| off as u16,
		
		Op::SwapBr(reg) => 0x0080 | reg as u16,
		
		Op::RevSwapBr(reg) => 0x0090 | reg as u16,
	}
}

#[allow(unused_variables)]
pub fn decode(instr: u16) -> Op {
	let opcode = (instr & 0xF000) >> 12;
	let data = instr & 0x0FFF;
	let a = (data & 0xF00) >> 8;
	let b = (data & 0x0F0) >> 4;
	let c = data & 0x00F;
	let ab = (data & 0xFF0) >> 4;
	let bc = data & 0x0FF;
	
	match opcode {
		0x0 => match a {
			0x0 => match b {
				// signals
				0x0 => match c {
					0x0 => Op::Halt,
					
					c if c < 0x10 => panic!("Invalid signal ({}): {:04X}", c, instr),
					_ => unreachable!()
				},
				
				0x1 => Op::Not(c as usize),
				
				0x2 => Op::RotateLeft(c as usize),
				0x3 => Op::RotateRight(c as usize),
				
				0x4 => Op::Increment(c as usize),
				0x5 => Op::Decrement(c as usize),
				
				0x6 => Op::Push(c as usize),
				0x7 => Op::Pop(c as usize),
				
				0x8 => Op::SwapBr(c as usize),
				
				0x9 => Op::RevSwapBr(c as usize),
				
				//4 => Read(c as usize),
				//5 => Write(c as usize),
				
				b if b < 0x10 => panic!("Invalid 1-arg opcode ({}): {:04X}", b, instr),
				_ => unreachable!()
			},
			
			0x1 => Op::Swap(b as usize, c as usize),
			0x2 => Op::CNot(b as usize, c as usize),
			
			0x3 => Op::CAdd(b as usize, c as usize),
			0x4 => Op::CSub(b as usize, c as usize),
			
			a if a < 0x10 => panic!("Invalid 2-arg opcode ({}): {:04X}", a, instr),
			_ => unreachable!()
		},
		
		0x1 => Op::Immediate(a as usize, bc as u8),
		
		0x2 => Op::Exchange(a as usize, bc as usize),
		
		// CCNot
		0x3 => Op::CCNot(a as usize, b as usize, c as usize),
		// CSwap
		0x4 => Op::CSwap(a as usize, b as usize, c as usize),
		
		0x6 => Op::Branch(data as u16),
		
		0x7 => Op::BrLZ(a as usize, bc as u8),
		
		0x8 => Op::BrGEZ(a as usize, bc as u8),
		
		0x9 => Op::BrEven(a as usize, bc as u8),
		
		0xA => Op::BrOdd(a as usize, bc as u8),
		
		opcode if opcode > 0xA => panic!("Invalid opcode ({}): 0x{:04X}", opcode, instr),
		_ => unreachable!()
	}
}
