extern crate getopts;
extern crate rel_isa as isa;

#[macro_use]
mod macros;

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
	opts.optopt("o", "output", "set output file name", "NAME");
	opts.optflag("v", "version", "print program version");
	opts.optflag("h", "help", "print this help menu");
	opts.optflag("", "verbose", "log each step the vm takes");
	
	let matches = try_err!(opts.parse(args));
	
	if matches.opt_present("version") {
		println!("rel16 0.1.1");
		return;
	}
	
	if matches.opt_present("help") {
		println!("{}", opts.usage(&opts.short_usage("rel16")));
		return;
	}
	
	let command = {
		let c = try_err!(matches.free.get(0).ok_or("Missing subcommand argument."));
		
		match &c[..] {
			"run"  => Command::Run,
			"asm"  => Command::Assembler(asm::Action::Assemble),
			"dasm" => Command::Assembler(asm::Action::Disassemble),
			
			other => {
				println_err!("No such subcommand: {}", other);
				return;
			}
		}
	};
	
	let src = {
		let input = try_err!(matches.free.get(1).ok_or("No input source given."));
		
		if input == "-" { Source::Stdin }
		else { Source::File(try_err!(File::open(input))) }
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
			let dest = try_err!(matches.opt_str("output").ok_or("No output file given."));
			let out_path = Path::new(&dest);
			
			match src {
				Source::Stdin   => asm::assembler(dir, out_path, io::stdin()),
				Source::File(f) => asm::assembler(dir, out_path, f),
			}
		}
	}
}
