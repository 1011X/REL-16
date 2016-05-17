#[macro_use]
mod macros;
mod instr;

mod vm;
mod asm;
mod dasm;

use std::env;
use std::path::PathBuf;

enum Command {
	Run,
	Assemble,
	Disassemble,
}

fn main() {
	let mut args = env::args().skip(1);
	
	let command = {
		let c = try_err!(args.next().ok_or("missing command argument"));
		
		match &*c {
			"run" => Command::Run,
			"asm" => Command::Assemble,
			"dasm" => Command::Disassemble,
		
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
		
		Command::Assemble => {
			asm::assemble(&file_path);
		}
		
		Command::Disassemble => {
			dasm::disassemble(&file_path);
		}
	}
}
