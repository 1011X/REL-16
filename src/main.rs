mod vm;
mod assembler;

use std::env;
use std::path::PathBuf;

fn main() {
	let mut args = env::args().skip(1);
	
	let command = args.next().unwrap();
	let file_path = PathBuf::from(args.next().unwrap());
	
	match &*command {
		"run" => {
			vm::vm(&file_path);
		}
		
		"build" => {
			assembler::assemble(&file_path);
		}
		
		_ => unimplemented!(),
	}
}
