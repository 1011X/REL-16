use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::io::{
	ErrorKind,
	BufReader, BufWriter,
	Read, Write
};

use instr::Op;


// here, in_path should have a .bin extension
pub fn disassemble(in_path: &Path) {
	// create path to temporary file in current directory
	let mut out_path = PathBuf::from(in_path.file_stem().unwrap());
	out_path.set_extension("dasm.tmp");
	
	// open input file and create output file, both buffered
	let mut input = BufReader::new(try_err!(File::open(in_path)));
	let mut output = BufWriter::new(try_err!(File::create(&out_path)));
	
	// replace instructions with mnemonics
	for i in 0.. {
		// shouldn't have to be initialized, but apparently Rust
		// can't figure that one out
		let ref mut buf = [0; 2];
		
		// try to read instruction
		if let Err(e) = input.read_exact(buf) {
			// reached eof, break
			if e.kind() == ErrorKind::UnexpectedEof { break }
			
			// otherwise, print error and end
			println_err!("Error: {}", e);
			return;
		}
		
		let instr = (buf[0] as u16) << 8 | buf[1] as u16;
		
		// finally, write instruction if there was no error
		match Op::decode(instr) {
			Ok(op) => {
				try_err!(writeln!(output, "{}", op));
			}
			
			Err(e) => {
				// if there was an error, write line number, description,
				// and code line
				println_err!("Error (instruction {}): {}", i, e);
				println_err!("{}", instr);
				
				if fs::remove_file(out_path).is_err() {
					println_err!("Could not delete temporary file.");
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
