pub type Reg = usize;
pub type Addr = u16;

#[derive(Debug)]
pub enum Op {
	Halt,
	
	
	Not(Reg),
			
	RotateLeft(Reg),
	RotateRight(Reg),
			
	Increment(Reg),
	Decrement(Reg),
	
	Push(Reg),
	Pop(Reg),
	
	//Read(Reg),
	//Write(Reg),
	
	
	Swap(Reg, Reg),
	
	CNot(Reg, Reg),
	
	CAdd(Reg, Reg),
	CSub(Reg, Reg),
	
	
	Immediate(Reg, u8),
	
	Exchange(Reg, Addr),
	
	CCNot(Reg, Reg, Reg),
	
	CSwap(Reg, Reg, Reg),
	
	GoTo(Addr),
	ComeFrom(Addr),
	/*
	BrGEZ(Reg, u8),
	
	BrLZ(Reg, u8),
	
	SwapBr(Reg),
	
	RevSwapBr(Reg),
	*/
}

impl Op {
	
	// To guarantee VM is reversible, every operation must have an inverse.
	pub fn invert(self) -> Op {
		match self {
			Op::Halt => Op::Halt,
		
		
		
			Op::Not(reg) => Op::Not(reg),
		
		
			Op::RotateLeft(reg) => Op::RotateRight(reg),
		
			Op::RotateRight(reg) => Op::RotateLeft(reg),
		
		
			Op::Increment(reg) => Op::Decrement(reg),
		
			Op::Decrement(reg) => Op::Increment(reg),
		
		
			Op::Push(reg) => Op::Pop(reg),
		
			Op::Pop(reg) => Op::Push(reg),
		
		
		
			Op::Swap(rl, rr) => Op::Swap(rl, rr),
		
			Op::CNot(rc, rn) => Op::CNot(rc, rn),
		
		
			Op::CAdd(rc, ra) => Op::CSub(rc, ra),
		
			Op::CSub(rc, rs) => Op::CAdd(rc, rs),
		
		
		
			Op::Immediate(reg, val) => Op::Immediate(reg, val),
		
			Op::Exchange(reg, addr) => Op::Exchange(reg, addr),
		
		
			Op::CCNot(rc0, rc1, rn) => Op::CCNot(rc0, rc1, rn),
		
			Op::CSwap(rc, rs0, rs1) => Op::CSwap(rc, rs0, rs1),
		
		
			// control flow
		
			Op::GoTo(off) => Op::ComeFrom(off),
		
			Op::ComeFrom(off) => Op::GoTo(off),
		
			/*
			Op::BrLZ(reg, off) => 
		
			Op::BrGEZ(reg, off) => 
		
			Op::SwapBr(reg) => 
		
			Op::RevSwapBr(reg) => 
			*/
		}
	}
}

pub fn encode(op: Op) -> u16 {
	// TODO: decide if I want to have assertions for values.
	match op {
		Op::Halt => 0x0000,
		
		
		
		Op::Not(reg) => 0x0010
			| reg as u16,
		
		
		Op::RotateLeft(reg) => 0x0020
			| reg as u16,
		
		Op::RotateRight(reg) => 0x0030
			| reg as u16,
		
		
		Op::Increment(reg) => 0x0040
			| reg as u16,
		
		Op::Decrement(reg) => 0x0050
			| reg as u16,
		
		
		Op::Push(reg) => 0x0060
			| reg as u16,
		
		Op::Pop(reg) => 0x0070
			| reg as u16,
		
		
		
		Op::Swap(rl, rr) => 0x0100
			| (rl as u16) << 4
			| rr as u16,
		
		Op::CNot(rc, rn) => 0x0200
			| (rc as u16) << 4
			| rn as u16,
		
		Op::CAdd(rc, ra) => 0x0300
			| (rc as u16) << 4
			| ra as u16,
		
		Op::CSub(rc, rs) => 0x0400
			| (rc as u16) << 4
			| rs as u16,
		
		
		
		Op::CCNot(rc0, rc1, rn) => 0x1000
			| (rc0 as u16) << 8
			| (rc1 as u16) << 4
			| rn as u16,
		
		Op::CSwap(rc, rs0, rs1) => 0x2000
			| (rc as u16) << 8
			| (rs0 as u16) << 4
			| rs1 as u16,
		
		
		
		Op::Immediate(reg, val) => 0x3000
			| (reg as u16) << 8
			| val as u16,
		
		Op::Exchange(reg, addr) => 0x4000
			| (reg as u16) << 8
			| addr as u16,
		
		
		// control flow
		
		Op::GoTo(off) => 0x5000
			| off as u16,
		
		Op::ComeFrom(off) => 0x6000
			| off as u16,
		
		/*
		Op::BrLZ(reg, off) => 0x8000
			| (reg as u16) << 8
			| off as u16,
		
		Op::BrGEZ(reg, off) => 0x9000
			| (reg as u16) << 8
			| off as u16,
		
		Op::SwapBr(reg) => 0x0080 | reg as u16,
		
		Op::RevSwapBr(reg) => 0x0090 | reg as u16,
		*/
	}
}

#[allow(unused_variables)]
pub fn decode(instr: u16) -> Op {
	let opcode = (instr & 0xF000) >> 11;
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
				
				/*
				0x8 => Op::SwapBr(c as usize),
				
				0x9 => Op::RevSwapBr(c as usize),
				*/
				
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
		
		0x1 => Op::CCNot(a as usize, b as usize, c as usize),
		
		0x2 => Op::CSwap(a as usize, b as usize, c as usize),
		
		0x3 => Op::Immediate(a as usize, bc as u8),
		
		0x4 => Op::Exchange(a as usize, bc as u16),
		
		0x5 => Op::GoTo(data as u16),
		0x6 => Op::ComeFrom(data as u16),
		/*
		0x8 => Op::BrLZ(a as usize, bc as u8),
		
		0x9 => Op::BrGEZ(a as usize, bc as u8),
		*/
		opcode if opcode < 0x10 => panic!("Invalid opcode ({}): 0x{:04X}", opcode, instr),
		_ => unreachable!()
	}
}
