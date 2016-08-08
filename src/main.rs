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

const USAGE: &'static str = "Usage:
    rel [--version] [--help]
    rel run [--verbose] <input>
    rel [d]asm [--output <filepath>] <input>
    
    The special argument \"-\" can be used as input to specify stdin.";


fn main() {
	let args = env::args().skip(1);
	
	let mut opts = Options::new();
	// shouldn't be required, bc `rel run` doesn't need it
	opts.optopt("o", "output",   "Set output destination to <filepath>. Default: \"default.bin\"", "<filepath>");
	opts.optflag("V", "version", "Print program version");
	opts.optflag("h", "help",    "Print this help menu");
	opts.optflag("v", "verbose", "Log each step the VM takes");
	
	let matches = try_err!(opts.parse(args));
	
	let print_help = || println_err!("{}", opts.usage(USAGE));
	
	if matches.opt_present("version") {
		println!("rel 0.2.5");
		return;
	}
	
	if matches.opt_present("help") {
		print_help();
		return;
	}
	
	let command = match matches.free.get(0).map(|s| s.as_str()) {
		Some("run")  => Command::Run,
		Some("asm")  => Command::Assembler(asm::Action::Assemble),
		Some("dasm") => Command::Assembler(asm::Action::Disassemble),
		
		Some(other) => {
			println_err!("Unrecognized subcommand: {}", other);
			print_help();
			return;
		}
		
		None => {
			println_err!("Missing subcommand.");
			print_help();
			return;
		}
	};
	
	let src = match matches.free.get(1).map(|s| s.as_str()) {
		Some("-") => Source::Stdin,
		Some(path) => Source::File(try_err!(File::open(path))),
		
		None => {
			println_err!("No input source given.");
			print_help();
			return;
		}
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
			let dest = matches.opt_str("output")
				.map(PathBuf::from)
				.unwrap_or_else(|| match src {
					Source::Stdin => PathBuf::from("default.bin"),
					Source::File(_) => {
						let mut path = PathBuf::from(&matches.free[1]);
						path.set_extension("bin");
						path
					}
				});
			
			match src {
				Source::Stdin   => asm::assembler(dir, &dest, io::stdin()),
				Source::File(f) => asm::assembler(dir, &dest, f),
			}
		}
	}
}
