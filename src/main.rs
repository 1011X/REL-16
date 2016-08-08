extern crate getopts;
//extern crate ncurses;
extern crate rel_isa as isa;

#[macro_use] mod macros;
mod vm;
mod asm;

use std::env;
use std::io;
use std::fs::File;
use std::path::PathBuf;
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
	opts.reqopt("o", "output", "set output to FILENAME", "FILENAME");
	opts.optflag("V", "version", "print program version");
	opts.optflag("h", "help", "print this help menu");
	opts.optflag("v", "verbose", "log each step the VM takes");
	
	let matches = try_err!(opts.parse(args));
	
	if matches.opt_present("version") {
		println!("rel 0.2.5");
		return;
	}
	
	if matches.opt_present("help") {
		println!("{}", opts.usage(&opts.short_usage("rel")));
		return;
	}
	
	let command = match matches.free.get(0).map(|s| s.as_str()) {
		Some("run")  => Command::Run,
		Some("asm")  => Command::Assembler(asm::Action::Assemble),
		Some("dasm") => Command::Assembler(asm::Action::Disassemble),
		
		Some(other) => panic!("Unrecognized subcommand: {}", other),
		
		None => panic!("Missing subcommand."),
	};
	
	let src = match matches.free.get(1).map(|s| s.as_str()) {
		Some("-") => Source::Stdin,
		Some(path) => Source::File(try_err!(File::open(path))),
		
		None => panic!("No input source given."),
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
			let dest = matches.opt_str("output");
			
			match src {
				Source::Stdin   => asm::assembler(dir, &dest, io::stdin()),
				Source::File(f) => asm::assembler(dir, &dest, f),
			}
		}
	}
}
