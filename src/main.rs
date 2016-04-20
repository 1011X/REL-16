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
	
	let command = match &*args.next().expect("missing command argument") {
		"run" => Command::Run,
		"build" => Command::Build,
		
		other => {
			println!("No such subcommand: {}", other);
			return;
		}
	};
	
	let file_path = PathBuf::from(args.next().expect("no file path given"));
	
	match command {
		Command::Run => {
			vm::vm(&file_path);
		}
		
		Command::Build => {
			assembler::assemble(&file_path);
		}
	}
}
