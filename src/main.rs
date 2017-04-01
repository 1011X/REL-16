extern crate getopts;
//extern crate ncurses;

#[macro_use] mod macros;
mod isa;
mod vm;
mod asm;

use std::env;
use std::io::BufReader;
use std::fs::File;
use getopts::Options;


const USAGE: &'static str = "Usage:
    rel [--version] [--help]
    rel [--verbose] <file>";


fn main() {
	let args = env::args().skip(1);
	
	let mut opts = Options::new();
	opts.optflag("V", "version", "Print program version");
	opts.optflag("h", "help",    "Print this help menu");
	opts.optflag("v", "verbose", "Log each step the VM takes");
	
	let matches = try_err!(opts.parse(args));

	if matches.opt_present("version") {
		println!("rel 0.3.0");
		return;
	}
	
	if matches.opt_present("help") {
		println_err!("{}", opts.usage(USAGE));
		return;
	}
	
	match matches.free.get(0) {
		Some(arg) => {
			let reader = BufReader::new(try_err!(File::open(arg)));
			let (code, symtab) = try_err!(asm::parse(reader));
			let logging_enabled = matches.opt_present("verbose");
			
			vm::run(code, symtab, logging_enabled);
		}
		
		None => {
			println_err!("Error: Missing input file.\n");
			println_err!("{}", opts.usage(USAGE));
		}
	}
}
