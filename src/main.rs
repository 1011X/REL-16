extern crate getopts;
//extern crate ncurses;

#[macro_use] mod macros;
mod isa;
mod vm;
mod asm;

use std::env;
use std::io::{Write, BufReader, BufWriter};
use std::fs::{self, File};
use getopts::Options;


const USAGE: &'static str = "Usage:
    rel [--version] [--help]
    rel run [--verbose] <infile>
    rel [d]asm [--output <outfile>] <infile>";


fn main() {
	let args = env::args().skip(1);
	
	let mut opts = Options::new();
	// -o not required because `rel run` doesn't need it
	opts.optopt("o", "output",   "Set output destination to <filepath>. Default: \"default.bin\"", "<filepath>");
	opts.optflag("V", "version", "Print program version");
	opts.optflag("h", "help",    "Print this help menu");
	opts.optflag("v", "verbose", "Log each step the VM takes");
	
	let matches = try_err!(opts.parse(args));

	macro_rules! exit_with(
		($($arg: tt)*) => {{
			println_err!($($arg)*);
			println_err!("{}", opts.usage(USAGE));
			return;
		}}
	);

	if matches.opt_present("version") {
		println!("rel 0.2.8");
		return;
	}
	
	if matches.opt_present("help") {
		println_err!("{}", opts.usage(USAGE));
		return;
	}
	
	let mut src = match matches.free.get(1) {
		Some(path) => BufReader::new(try_err!(File::open(path))),
		None => exit_with!("No input source given."),
	};
	
	match matches.free.get(0).map(|s| s.as_str()) {
		Some("run") => {
			let logging_enabled = matches.opt_present("verbose");
			
			vm::run(&mut src, logging_enabled);
		}
		
		Some(subc @ "asm") |
		Some(subc @ "dasm") => {
			let data = match subc {
				"asm"  => asm::assemble(src),
				"dasm" => asm::disassemble(&mut src),
				_      => unreachable!()
			};
			
			let filename = matches.opt_str("output")
				.unwrap_or("default.bin".to_owned());
			
			let result = BufWriter::new(try_err!(File::create(&filename)))
				.write_all(&data);
			
			if let Err(e) = result {
				println_err!("Could not write to file: {}", e);
				
				if let Err(e) = fs::remove_file(&filename) {
					println_err!("Could not delete empty file: {}", e);
					println_err!("Please delete manually.");
				}
			}
		}
		
		Some(subc) => exit_with!("Unrecognized subcommand: {}", subc),
		
		None => exit_with!("Missing subcommand."),
	}
}
