mod instr;

mod vm;
mod assembler;

use std::env;
use std::path::PathBuf;

macro_rules! println_err(
    ($($arg: tt)*) => {{
    	use std::io::Write;
        let result = writeln!(&mut ::std::io::stderr(), $($arg)*);
        
        if let Err(e) = result {
        	panic!("failed printing to stderr: {}", e);
        }
    }}
);

enum Command {
	Run,
	Build
}

fn main() {
	let mut args = env::args().skip(1);
	
	let command = {
		let c = match args.next() {
			Some(c) => c,
			
			None => {
				println_err!("missing command argument");
				return;
			}
		};
		
		match &*c {
			"run" => Command::Run,
			"build" => Command::Build,
		
			other => {
				println!("No such subcommand: {}", other);
				return;
			}
		}
	};
	
	let file_path = PathBuf::from(match args.next() {
		Some(path) => path,
		
		None => {
			println_err!("no file path given");
			return;
		}
	});
	
	match command {
		Command::Run => {
			vm::vm(&file_path);
		}
		
		Command::Build => {
			assembler::assemble(&file_path);
		}
	}
}
