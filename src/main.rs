extern crate getopts;
//extern crate ncurses;
extern crate rel_isa as isa;

#[macro_use] mod macros;
mod vm;
mod asm;

use std::env;
use std::path::PathBuf;
use getopts::Options;


const USAGE: &'static str = "Usage:
    rel [--version] [--help]
    rel run [--verbose] <infile>
    rel [d]asm [--output <outfile>] <infile>";


fn main() {
	let args = env::args().skip(1);
	
	let mut opts = Options::new();
	// shouldn't be required, bc `rel run` doesn't need it
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
		println!("rel 0.2.6");
		return;
	}
	
	if matches.opt_present("help") {
		println_err!("{}", opts.usage(USAGE));
		return;
	}
	
	match matches.free.get(0).map(|s| s.as_str()) {
		Some("run") => {
			let logging_enabled = matches.opt_present("verbose");
			let src = match matches.free.get(1) {
				Some(path) => PathBuf::from(path),
				None => exit_with!("No input source given."),
			};
			
			vm::run(&src, logging_enabled);
		}
		
		Some(sc @ "asm") |
		Some(sc @ "dasm") => {
			let dest = PathBuf::from(matches.opt_str("output")
				.unwrap_or("default.bin".to_owned())
			);
			
			let src = match matches.free.get(1) {
				Some(path) => PathBuf::from(path),
				None => exit_with!("No input source given."),
			};
			
			match sc {
				"asm"  => asm::assemble(&dest, &src),
				"dasm" => asm::disassemble(&dest, &src),
				_      => unreachable!()
			}
		}
		
		Some(other) => exit_with!("Unrecognized subcommand: {}", other),
		
		None => exit_with!("Missing subcommand."),
	}
}
