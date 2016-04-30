#[macro_use]
mod macros;
mod vm;
mod assembler;
mod instr;

use std::env;
use std::path::PathBuf;

enum Command {
	Run,
	Build
}

fn main() {
	let mut args = env::args().skip(1);
	
	let command = {
		let c = try_err!(args.next().ok_or("missing command argument"));
		
		match &*c {
			"run" => Command::Run,
			"build" => Command::Build,
		
			other => {
				println_err!("No such subcommand: {}", other);
				return;
			}
		}
	};
	
	let file_path = PathBuf::from(try_err!(args.next().ok_or("no file path given")));
	
	match command {
		Command::Run => {
			vm::vm(&file_path);
		}
		
		Command::Build => {
			assembler::assemble(&file_path);
		}
	}
}
