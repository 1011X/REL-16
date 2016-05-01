use instr::Op;

use std::str::FromStr;

use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::io::{
	BufReader, BufWriter,
	BufRead, Write
};


pub fn assemble(in_path: &Path) {
	let mut out_path = PathBuf::from(in_path.file_stem().unwrap());
	out_path.set_extension("o");
	
	let input = BufReader::new(try_err!(File::open(in_path)));
	let mut output = BufWriter::new(try_err!(File::create(&out_path)));
	
	// replace mnemonics with actual instructions
	for (line_number, result) in input.lines().enumerate() {
		let line = try_err!(result);
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
		
		// finally, write instruction if there was no error
		match Op::from_str(line) {
			Ok(op) => {
				let instr = op.encode();
				let data = [(instr >> 8) as u8, instr as u8];
				
				match try_err!(output.write(&data)) {
					2 => {}
				
					1 => {
						println_err!("Error: could not write complete instruction");
						return;
					}
				
					0 => {
						println_err!("Error: no more space in file to write");
						return;
					}
				
					_ => unreachable!(),
				}
			}
			
			// if there was an error, write line number, description, and code line
			Err(e) => {
				println_err!("Error (line {}): {}", line_number, e);
				println_err!("{}", line);
				
				if fs::remove_file(out_path).is_err() {
					println_err!("Could not delete incomplete file.");
				}
				
				return;
			}
		}
	}
}
