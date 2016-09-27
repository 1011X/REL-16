use std::str::FromStr;
use std::io::{BufRead, Write};

use isa::Op;


pub fn assemble<I: BufRead>(inp: I) -> Vec<u8> {
	let mut out = Vec::new();
	
	// replace mnemonics with actual instructions
	for (line_number, result) in inp.lines().enumerate() {
		let line = result.unwrap(); // TODO: handle unwrap
		
		// mark end of string as index of first comment marker, or its length if none exist.
		let end = line.find(';').unwrap_or(line.len());
		let l = line[..end].trim();
		
		// skip blank lines
		if l.is_empty() { continue }
		// non-empty line assumed after this
		
		// try encoding line
		match Op::from_str(l) {
			Ok(op) => {
				let instr = op.encode();
				
				out.push((instr >> 8) as u8);
				out.push(instr as u8);
			}
			
			Err(e) => {
				// write line number, mneumonic, and error description
				println_err!("Error encoding instruction at line {}: {}", line_number, line);
				panic!("{}", e);
			}
		}
	}
	
	out
}

pub fn disassemble<I: BufRead>(inp: &mut I) -> Vec<u8> {
	let mut out = Vec::new();
	
	// replace instructions with mnemonics
	for i in 0.. {
		let buf = &mut [0, 0];
		
		// try to read instruction
		match try_err!(inp.read(buf)) {
			0 => break, // reached eof
			1 => panic!("Received incomplete instruction. Exiting"),
			2 => {} // what we expect
			_ => unreachable!()
		}
		
		let instr = (buf[0] as u16) << 8 | buf[1] as u16;
		
		match Op::decode(instr) {
			// write instruction
			Ok(op) => writeln!(out, "{}", op).unwrap(),
			
			Err(e) => {
				// write line number, instruction, and error description
				println_err!("Error decoding instruction {}: 0x{:04x}.", i, instr);
				panic!("{}", e);
			}
		}
	}
	
	out
}
