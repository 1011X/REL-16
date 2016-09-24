use std::str::FromStr;
use std::fs::File;
use std::path::Path;
use std::io::{
	BufReader, BufWriter,
	BufRead, Read, Write
};

use isa::Op;


pub fn assemble(output: &Path, input: &Path) {
	// open input source and create output file, both buffered
	let inp = BufReader::new(try_err!(File::open(input)));
	let mut out = BufWriter::new(try_err!(File::create(output)));
	
	// replace mnemonics with actual instructions
	for (line_number, result) in inp.lines().enumerate() {
		let line = try_err!(result);
		
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
				let data = &[(instr >> 8) as u8, instr as u8];
				
				try_err!(out.write_all(data));
			}
			
			Err(e) => {
				// write line number, mneumonic, and error description
				println_err!("Error encoding instruction at line {}: {}", line_number, line);
				panic!("{}", e);
			}
		}
	}
}

pub fn disassemble(output: &Path, input: &Path) {
	// open input source and create output file, both buffered
	let mut inp = BufReader::new(try_err!(File::open(input)));
	let mut out = BufWriter::new(try_err!(File::create(output)));
	
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
			// try writing instruction and abort if there's an error
			Ok(op) => try_err!(writeln!(out, "{}", op)),
			
			Err(e) => {
				// write line number, instruction, and error description
				println_err!("Error decoding instruction {}: 0x{:04x}.", i, instr);
				panic!("{}", e);
			}
		}
	}
}
