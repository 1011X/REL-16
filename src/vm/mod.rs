mod register_file;
mod rev;
mod cpu;
//mod devices;

const MAX_MEM: usize = 65536;

use std::io::BufRead;
use self::cpu::Cpu;

pub fn run<I: BufRead>(input: &mut I, logging_enabled: bool) {
	let mut prog_mem = [0; MAX_MEM];
	//let mut ports = [None; MAX_MEM];
	
	//let mut hdd = devices::Hdd::new();
	//ports[0] = Some(&mut hdd);
	
	// read file contents into program memory
	for i in 0.. {
		let buffer = &mut [0, 0];
		
		match try_err!(input.read(buffer)) {
			0 => break,
			1 => panic!("Error: Got incomplete instruction."),
			
			2 => if i < MAX_MEM {
				let instr = (buffer[0] as u16) << 8 | buffer[1] as u16;
				prog_mem[i] = instr;
			} else {
				panic!("Error: Binary is too big for memory!");
			},
			
			_ => unreachable!(),
		}
	}
	
	let mut cpu = Cpu::new(&prog_mem/*, &ports*/);
	
	loop {
		if logging_enabled { cpu.log_state() }
		
		if cpu.fetch() { break }
		
		if logging_enabled { cpu.log_current_instr() }
		
		if cpu.execute() { break }
	}
}
