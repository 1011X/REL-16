use instr::Op;

use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::io::{
	BufReader, BufWriter,
	BufRead, Write
};

macro_rules! println_err(
    ($($arg: tt)*) => {{
    	use std::io::Write;
        let result = writeln!(&mut ::std::io::stderr(), $($arg)*);
        
        if let Err(e) = result {
        	panic!("failed printing to stderr: {}", e);
        }
    }}
);

macro_rules! try_err(
	($e: expr) => {{
		match $e {
			Ok(val) => val,
			
			Err(e) => {
				println_err!("Error: {}", e);
				return;
			}
		}
	}}
);


fn parse_byte(s: &str) -> Result<u8, String> {
	s.parse::<u8>()
		.map_err(|e| format!("{}", e))
}

fn parse_reglit(s: &str) -> Result<usize, String> {
	if s.starts_with('r') {
		match parse_byte(&s[1..]) {
			Ok(byte) if byte < 8 =>
				Ok(byte as usize),
			
			Ok(_) =>
				Err("index for register too big!".to_owned()),
			
			Err(e) =>
				Err(e),
		}
	}
	else {
		Err(format!("expected register literal, found {}", s))
	}
}

pub fn assemble(in_path: &Path) {
	let mut out_path = PathBuf::from(in_path.file_stem().unwrap());
	out_path.set_extension("o");
	
	let input = BufReader::new(try_err!(File::open(in_path)));
	let mut output = BufWriter::new(try_err!(File::create(&out_path)));
	
	// replace mnemonics with actual instructions
	for (line_number, result) in input.lines().enumerate() {
		let line = try_err!(result);
		let mut line = line.trim();
		
		// check if line has a comment marker
		for (i, c) in line.char_indices() {
			// found marker; ignore everything after it
			if c == '#' || c == ';' {
				line = line[..i].trim_right();
				break;
			}
		}
		
		// non-empty line assumed after this
		if line.is_empty() { continue }
		
		let mut tokens = line.split_whitespace();
		
		// Can't just include `tokens.next()` here because
		// then there would be a mutable reference to `tokens`
		// in `get_register()` *and* in match block below that
		// determines instruction used.
		let get_register = |token: Option<&str>| token
			.ok_or("register argument not found".to_owned())
			.and_then(parse_reglit);
		
		// we can unwrap once because line should not be empty
		let result = match tokens.next().unwrap() {
			"halt" => Ok(Op::Halt),
			
			"not" => get_register(tokens.next())
				.map(Op::Not),
			
			"rotl" => get_register(tokens.next())
				.map(Op::RotateLeft),
			
			"rotr" => get_register(tokens.next())
				.map(Op::RotateRight),
			
			"inc" => get_register(tokens.next())
				.map(Op::Increment),
			
			"dec" => get_register(tokens.next())
				.map(Op::Decrement),
			
			"push" => get_register(tokens.next())
				.map(Op::Push),
			
			"pop" => get_register(tokens.next())
				.map(Op::Pop),
			
			"swp" => {
				let regl = get_register(tokens.next());
				let regr = get_register(tokens.next());
				
				match (regl, regr) {
					(Ok(regl), Ok(regr)) =>
						Ok(Op::Swap(regl, regr)),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(e),
				}
			}
			
			"cnot" => {
				let regc = get_register(tokens.next());
				let regn = get_register(tokens.next());
				
				match (regc, regn) {
					(Ok(regc), Ok(regn)) if regn != regc =>
						Ok(Op::CNot(regc, regn)),
					
					(Ok(_), Ok(_)) =>
						Err("can't use the same register in cnot".to_owned()),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(e),
				}
			}
			
			"cadd" => {
				let rctrl = get_register(tokens.next());
				let radd = get_register(tokens.next());
				
				match (rctrl, radd) {
					(Ok(rctrl), Ok(radd)) if radd != rctrl =>
						Ok(Op::CAdd(rctrl, radd)),
					
					(Ok(_), Ok(_)) =>
						Err("can't use the same register in cnot".to_owned()),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(e),
				}
			}
			
			"csub" => {
				let rctrl = get_register(tokens.next());
				let radd = get_register(tokens.next());
				
				match (rctrl, radd) {
					(Ok(rctrl), Ok(rsub)) if rsub != rctrl =>
						Ok(Op::CSub(rctrl, rsub)),
					
					(Ok(_), Ok(_)) =>
						Err("can't use the same register in cnot".to_owned()),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(e),
				}
			}
			
			"imm" => {
				let reg = get_register(tokens.next());
				
				let value = tokens.next()
					.ok_or("no value for imm instruction given".to_owned())
					.and_then(parse_byte);
				
				match (reg, value) {
					(Ok(reg), Ok(value)) =>
						Ok(Op::Immediate(reg, value)),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(e)
				}
			}
			
			"exch" => {
				let reg = get_register(tokens.next());
				let raddr = get_register(tokens.next());
				
				match (reg, raddr) {
					(Ok(reg), Ok(raddr)) =>
						Ok(Op::Exchange(reg, raddr)),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(e),
				}
			}
			
			"ccn" => {
				let rega = get_register(tokens.next());
				let regb = get_register(tokens.next());
				let regc = get_register(tokens.next());
				
				match (rega, regb, regc) {
					(Ok(a), Ok(b), Ok(c)) if c != a && c != b =>
						Ok(Op::CCNot(a, b, c)),
					
					(Ok(_), Ok(_), Ok(_)) =>
						Err("controlled argument used in mutable argument".to_owned()),
					
					(Err(e), _, _) | (_, Err(e), _) | (_, _, Err(e)) =>
						Err(e),
				}
			}
			
			"cswp" => {
				let rega = get_register(tokens.next());
				let regb = get_register(tokens.next());
				let regc = get_register(tokens.next());
				
				match (rega, regb, regc) {
					(Ok(a), Ok(b), Ok(c)) if b != a && c != a =>
						Ok(Op::CSwap(a, b, c)),
					
					(Ok(_), Ok(_), Ok(_)) =>
						Err("controlled argument used in mutable argument".to_owned()),
					
					(Err(e), _, _) | (_, Err(e), _) | (_, _, Err(e)) =>
						Err(e),
				}
			}
			
			"goto" => tokens.next()
				.ok_or("address argument not found".to_owned())
				.and_then(|s| match s.parse::<u16>() {
					Ok(v) if v < 0x1000 => Ok(v),
					Ok(_) => Err("value for argument too big!".to_owned()),
					Err(e) => Err(format!("{}", e)),
				})
				.map(Op::GoTo),
			
			"cmfr" => tokens.next()
				.ok_or("address argument not found".to_owned())
				.and_then(|s| match s.parse::<u16>() {
					Ok(v) if v < 0x1000 => Ok(v),
					Ok(_) => Err("value for argument too big!".to_owned()),
					Err(e) => Err(format!("{}", e)),
				})
				.map(Op::ComeFrom),
			/*
			"blz" => {
				let reg = get_register(tokens.next());
				
				let off = if let Some(s) = tokens.next() {
					s.parse::<u8>()
						.map_err(|e| e.description().to_owned())
				} else {
					Err("address argument not found".to_owned())
				};
				
				match (reg, off) {
					(Ok(reg), Ok(off)) =>
						Ok(Op::BrLZ(reg, off)),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(e),
				}
			}
			
			"bgez" => {
				let reg = get_register(tokens.next());
				
				let off = if let Some(s) = tokens.next() {
					s.parse::<u8>()
						.map_err(|e| e.description().to_owned())
				} else {
					Err("address argument not found".to_owned())
				};
				
				match (reg, off) {
					(Ok(reg), Ok(off)) =>
						Ok(Op::BrGEZ(reg, off)),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(e),
				}
			}
			
			"swb" => {
				let reg = get_register(tokens.next());
				
				reg.map(Op::SwapBr)
			}
			
			"rswb" => {
				let reg = get_register(tokens.next());
				
				reg.map(Op::RevSwapBr)
			}
			*/
			
			other => Err(format!("unknown opcode mneumonic: {}", other)),
		};
		
		match result {
			// finally, write instruction if there was no error
			Ok(op) => {
				let instr = op.encode();
				let data = [(instr >> 8) as u8, instr as u8];
				
				match try_err!(output.write(&data)) {
					2 => {}
				
					1 => {
						println_err!("Error: could not write complete instruction");
						return;
					}
				
					0 => {
						println_err!("Error: no more space in file to write");
						return;
					}
				
					_ => unreachable!(),
				}
			}
			
			// if there was an error, write line number, description, and code line
			Err(e) => {
				println_err!("Error (line {}): {}", line_number, e);
				println_err!("{}", line);
				
				if fs::remove_file(out_path).is_err() {
					println_err!("Could not delete incomplete file.");
				}
				
				return;
			}
		}
	}
}
