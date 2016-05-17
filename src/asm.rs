use instr::Op;

use std::str::FromStr;
use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::io::{
	BufReader, BufWriter,
	BufRead, Write
};


pub fn assemble(in_path: &Path) {
	// create path to temporary file in current directory
	let mut out_path = PathBuf::from(in_path.file_stem().unwrap());
	out_path.set_extension("bin.tmp");
	
	// open input file and create output file, both buffered
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
				
				try_err!(output.write_all(&data));
			}
			
			Err(e) => {
				// if there was an error, write line number, description,
				// and code line where error came from
				println_err!("Error (line {}): {}", line_number, e);
				println_err!("{}", line);
				
				// delete intermediate file if possible
				if fs::remove_file(out_path).is_err() {
					println_err!("Could not delete incomplete file.");
				}
				
				return;
			}
		}
	}
	
	// if everything went well, we rename the temp file (with
	// the .tmp extension) to one without the .tmp
	let tmp_path = out_path.clone();
	out_path.set_extension("");
	
	if let Err(e) = fs::rename(tmp_path, out_path) {
		println_err!("Could not rename file: {}", e);
		println_err!("You'll have to remove the .tmp extension manually.");
	}
}
