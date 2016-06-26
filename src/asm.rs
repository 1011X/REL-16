use std::str::FromStr;
use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::io::{
	ErrorKind,
	BufReader, BufWriter,
	BufRead, Read, Write
};

use instr::Op;


pub enum Action {
	Assemble,
	Disassemble,
}


pub fn assembler<I: Read>(dir: Action, out_path: &Path, src: I) {
	// create path to temporary file
	let mut tmp_path = PathBuf::from(out_path);
	tmp_path.set_extension("tmp");
	
	// open input source and create output file, both buffered
	let mut input = BufReader::new(src);
	let mut output = BufWriter::new(File::create(&tmp_path).expect("Could not create file."));
	
	// choose assemble or disassemble based on dir
	let result = match dir {
		Action::Assemble    => assemble(&mut output, &mut input),
		Action::Disassemble => disassemble(&mut output, &mut input),
	};
	
	if result.is_ok() {
		// try to drop the .tmp from the file.
		if let Err(e) = fs::rename(tmp_path, out_path) {
			println_err!("Could not remove .tmp extension from file.");
			println_err!("You'll have to replace it with .bin manually.");
			println_err!("{}", e);
		}
	}
	else {
		// delete intermediate file if possible
		if let Err(e) = fs::remove_file(tmp_path) {
			println_err!("Could not delete incomplete file.");
			println_err!("Please delete it if you can.");
			println_err!("{}", e);
		}
	}
}

fn assemble<I, O>(out: &mut BufWriter<O>, inp: &mut BufReader<I>) -> Result<(), ()> where
I: Read, O: Write {
	// replace mnemonics with actual instructions
	for (line_number, result) in inp.lines().enumerate() {
		let line = match result {
			Ok(line) => line,
			Err(e) => {
				println_err!("Error getting line number {}.", line_number);
				println_err!("{}", e);
				return Err(());
			}
		};
		let mut line = line.trim();
		
		// check if line has a comment marker
		for (i, c) in line.char_indices() {
			// found marker; ignore everything after it
			if c == '#' || c == ';' {
				line = line[..i].trim_right();
				break;
			}
		}
		
		// non-empty line assumed after this
		if line.is_empty() { continue }
		
		// try encoding line
		match Op::from_str(line) {
			Ok(op) => {
				let instr = op.encode();
				let data = [(instr >> 8) as u8, instr as u8];
				
				if let Err(e) = out.write_all(&data) {
					// write instruction code and output filename
					println_err!("Error writing instruction `{}` to file.", instr);
					println_err!("{}", e);
					return Err(());
				}
			}
			
			Err(e) => {
				// write line number, mneumonic, and error description
				println_err!("Error encoding instruction at line {}: {}", line_number, line);
				println_err!("{}", e);
				return Err(());
			}
		}
	}
	
	Ok(())
}

fn disassemble<I, O>(out: &mut BufWriter<O>, inp: &mut BufReader<I>) -> Result<(), ()> where
I: Read, O: Write {
	// replace instructions with mnemonics
	for i in 0.. {
		// buf doesn't have to be initialized because
		// input.read_exact() will err if not enough bytes are read,
		// (and we catch it in the following `if`), but Rust can't see
		// implicit semantics.
		let mut buf = [0_u8, 0];
		
		// try to read instruction
		if let Err(e) = inp.read_exact(&mut buf) {
			// reached eof, break
			// XXX: might ignore last byte in files with an odd
			// number of bytes. files with an even number of bytes
			// are expected, though.
			if e.kind() == ErrorKind::UnexpectedEof { break }
			
			// otherwise, return error
			println_err!("Error reading from input source.");
			println_err!("{}", e);
			return Err(());
		}
		
		let instr = (buf[0] as u16) << 8 | buf[1] as u16;
		
		// finally, write instruction if there was no error
		match Op::decode(instr) {
			Ok(op) => {
				// check if write succeeded
				if let Err(e) = writeln!(out, "{}", op) {
					// write mneumonic and output filename
					println_err!("Error writing mneumonic `{}` to file.", op);
					println_err!("{}", e);
					return Err(());
				}
			}
			
			Err(e) => {
				// write line number, instruction, and error description
				println_err!("Error decoding instruction {}: 0x{:04x}.", i, instr);
				println_err!("{}", e);
				return Err(());
			}
		}
	}
	
	Ok(())
}
