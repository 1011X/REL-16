extern crate getopts;
extern crate rel_isa as isa;

mod vm;
mod asm;

use std::env;
use std::io::BufReader;
use std::fs::File;
use getopts::Options;


static USAGE: &str = "\
Usage:
    rel [--version] [--help]
    rel [--verbose] <file>";


fn main() {
	let args = env::args().skip(1);
	
	let mut opts = Options::new();
	opts.optflag("V", "version", "Print program version");
	opts.optflag("h", "help",    "Print this help menu");
	opts.optflag("v", "verbose", "Log each step the VM takes");
	
	let matches = opts.parse(args).unwrap();

	if matches.opt_present("version") {
		println!("rel 0.3.1");
		return;
	}
	
	if matches.opt_present("help") {
		eprintln!("{}", opts.usage(USAGE));
		return;
	}
	
	if let Some(arg) = matches.free.get(0) {
		let reader = BufReader::new(File::open(arg).unwrap());
		let code = asm::parse(reader).unwrap();
		let logging_enabled = matches.opt_present("verbose");
		
		vm::Cpu::new(&code, logging_enabled).run();
	}
	else {
		eprintln!("Error: Missing input file.\n");
		eprintln!("{}", opts.usage(USAGE));
	}
}
