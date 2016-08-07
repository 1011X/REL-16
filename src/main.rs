extern crate getopts;
//extern crate ncurses;
extern crate rel_isa as isa;

#[macro_use] mod macros;
mod vm;
mod asm;

use std::env;
use std::fs::File;
use std::path::Path;
use std::io::{self, Write};
use getopts::Options;


enum Command {
	Run,
	Assembler(asm::Action),
}

enum Source {
	Stdin,
	File(File),
}


fn main() {
	let args = env::args().skip(1);
	
	let mut opts = Options::new();
	opts.optopt("o", "output", "set output to FILENAME", "FILENAME");
	opts.optflag("V", "version", "print program version");
	opts.optflag("h", "help", "print this help menu");
	opts.optflag("v", "verbose", "log each step the VM takes");
	opts.optopt("", "out-dir", "set output to compiler-chosen filename in DIR", "DIR");
	
	let matches = try_err!(opts.parse(args));
	
	if matches.opt_present("version") {
		println!("rel 0.2.4");
		return;
	}
	
	if matches.opt_present("help") {
		println!("{}", opts.usage(&opts.short_usage("rel")));
		return;
	}
	
	let command = match &**matches.free.get(0).expect("Missing subcommand argument.") {
		"run"  => Command::Run,
		"asm"  => Command::Assembler(asm::Action::Assemble),
		"dasm" => Command::Assembler(asm::Action::Disassemble),
		
		other => panic!("No such subcommand: {}", other),
	};
	
	let src = {
		let input = matches.free.get(1).expect("No input source given.");
		
		if input == "-" { Source::Stdin }
		else { Source::File(File::open(input).unwrap()) }
	};
	
	match command {
		Command::Run => {
			let logging_enabled = matches.opt_present("verbose");
			
			match src {
				Source::Stdin   => vm::run(io::stdin(), logging_enabled),
				Source::File(f) => vm::run(f, logging_enabled),
			}
		}
		
		Command::Assembler(dir) => {
			let dest = matches.opt_str("out-dir")
				.or(matches.opt_str("output"));
			
			let cwd = env::current_dir().unwrap();
			
			let out_path = match dest {
				None => Path::new(&cwd),
				Some(ref dir) => Path::new(dir),
			};
			
			match src {
				Source::Stdin   => asm::assembler(dir, out_path, io::stdin()),
				Source::File(f) => asm::assembler(dir, out_path, f),
			}
		}
	}
}
