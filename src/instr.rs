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
	
	
	Lit(usize, u8),
	MemSwap(usize, usize),
	
	// CCNot
	Toffoli(usize, usize, usize),
	
	// CSwap
	Fredkin(usize, usize, usize),
	
	Jump(usize),
	JZero(usize),
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
		
		Op::Pop(reg) => 0x0060 | reg as u16,
		
		
		
		Op::Swap(r_left, r_right) =>
			0x0100 | (r_left as u16) << 4 | r_right as u16,
		
		Op::CNot(r_ctrl, r_not) =>
			0x0200 | (r_ctrl as u16) << 4 | r_not as u16,
		
		Op::CAdd(r_ctrl, r_add) =>
			0x0300 | (r_ctrl as u16) << 4 | r_add as u16,
		
		
		
		Op::Lit(reg, val) =>
			0x1000 | (reg as u16) << 8 | val as u16,
		
		Op::MemSwap(reg, addr) =>
			0x2000 | (reg as u16) << 8 | addr as u16,
		
		
		Op::Toffoli(r_ctrl_0, r_ctrl_1, r_not) =>
			0x3000 | (r_ctrl_0 as u16) << 8 | (r_ctrl_1 as u16) << 4 | r_not as u16,
		
		Op::Fredkin(r_ctrl, r_swap_0, r_swap_1) =>
			0x4000 | (r_ctrl as u16) << 8 | (r_swap_0 as u16) << 4 | r_swap_1 as u16,
		
		
		Op::Jump(addr) => 0x5000 | addr as u16,
		
		Op::JZero(addr) => 0x6000 | addr as u16,
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
				
				//4 => Read(c as usize),
				//5 => Write(c as usize),
				
				b if b < 0x10 => panic!("Invalid 1-arg opcode ({}): {:04X}", b, instr),
				_ => unreachable!()
			},
			
			0x1 => Op::Swap(b as usize, c as usize),
			0x2 => Op::CNot(b as usize, c as usize),
			
			0x3 => Op::CAdd(b as usize, c as usize),
			
			a if a < 0x10 => panic!("Invalid 2-arg opcode ({}): {:04X}", a, instr),
			_ => unreachable!()
		},
		
		0x1 => Op::Lit(a as usize, bc as u8),
		
		0x2 => Op::MemSwap(a as usize, bc as usize),
		
		// CCNot
		0x3 => Op::Toffoli(a as usize, b as usize, c as usize),
		// CSwap
		0x4 => Op::Fredkin(a as usize, b as usize, c as usize),
		
		0x5 => Op::Jump(data as usize),
		0x6 => Op::JZero(data as usize),
		
		opcode if opcode < 0x10 => panic!("Invalid opcode ({}): 0x{:04X}", opcode, instr),
		_ => unreachable!()
	}
}
