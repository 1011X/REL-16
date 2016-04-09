use instr::Op;
use instr::encode;

use std::fs;
use std::fs::File;
use std::error::Error;
use std::path::Path;
use std::path::PathBuf;
use std::io::{
	BufReader, BufWriter,
	BufRead, Write
};


macro_rules! println_err(
    ($($arg: tt)*) => {{
        let result = writeln!(&mut ::std::io::stderr(), $($arg)*);
        
        if let Err(e) = result {
        	panic!("failed printing to stderr: {}", e);
        }
    }}
);


fn parse_byte(s: &str) -> Result<u8, String> {
	s.parse::<u8>()
		.map_err(|e| e.description().to_string())
}

fn parse_reglit(s: &str) -> Result<usize, String> {
	if s.starts_with('r') {
		match parse_byte(&s[1..]) {
			Ok(byte) if byte < 16 =>
				Ok(byte as usize),
			
			Ok(_) =>
				Err("index for register too big!".to_string()),
			
			Err(e) => Err(e),
		}
	} else {
		Err(format!("expected register literal, found {}", s))
	}
}

pub fn assemble(in_path: &Path) {
	let mut out_path = PathBuf::from(in_path.file_stem().unwrap());
	out_path.set_extension("o");
	
	let input = BufReader::new(match File::open(in_path) {
		Ok(file) => file,
		Err(e) => {
			println_err!("Error: {}", e.description());
			return;
		}
	});
	let mut output = BufWriter::new(File::create(&out_path).unwrap());
	
	// replace mnemonics with actual instructions
	for (line_number, result) in input.lines().enumerate() {
		let line = result.unwrap();
		let mut tokens = line.split_whitespace();
		
		let get_register = |token| match token {
			Some(t) => parse_reglit(t),
			_ => Err("register argument not found".to_string()),
		};
		
		let opcode = match tokens.next() {
			None | Some("#") | Some(";") => continue,
			Some(op) => op,
		};
		
		let instr = match opcode {
			"halt" => Ok(Op::Halt),
			
			"not" => {
				let reg = get_register(tokens.next());
				
				reg.map(Op::Not)
			}
			
			"rotl" => {
				let reg = get_register(tokens.next());
				
				reg.map(Op::RotateLeft)
			}
			
			"rotr" => {
				let reg = get_register(tokens.next());
				
				reg.map(Op::RotateRight)
			}
			
			"incr" => {
				let reg = get_register(tokens.next());
				
				reg.map(Op::Increment)
			}
			
			"decr" => {
				let reg = get_register(tokens.next());
				
				reg.map(Op::Decrement)
			}
			
			"push" => {
				let reg = get_register(tokens.next());
				
				reg.map(Op::Push)
			}
			
			"pop" => {
				let reg = get_register(tokens.next());
				
				reg.map(Op::Pop)
			}
			
			"swap" => {
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
						Err("can't use the same register in cnot".to_string()),
					
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
						Err("can't use the same register in cnot".to_string()),
					
					(Err(e), _) | (_, Err(e)) => Err(e),
				}
			}
			
			"csub" => {
				let rctrl = get_register(tokens.next());
				let radd = get_register(tokens.next());
				
				match (rctrl, radd) {
					(Ok(rctrl), Ok(rsub)) if rsub != rctrl =>
						Ok(Op::CSub(rctrl, rsub)),
					
					(Ok(_), Ok(_)) =>
						Err("can't use the same register in cnot".to_string()),
					
					(Err(e), _) | (_, Err(e)) => Err(e),
				}
			}
			
			"lit" => {
				let reg = get_register(tokens.next());
				
				let value = match tokens.next() {
					Some(t) => parse_byte(t),
					_ => Err("no value for lit instruction given".to_string()),
				};
				
				match (reg, value) {
					(Ok(reg), Ok(value)) =>
						Ok(Op::Lit(reg, value)),
					
					(Err(e), _) | (_, Err(e)) =>
						Err(e)
				}
			}
			
			"memswap" => {
				let reg = get_register(tokens.next());
				
				let addr = match tokens.next() {
					Some(s) => parse_byte(s),
					_ => Err("address argument not found".to_string())
				};
				
				match (reg, addr) {
					(Ok(reg), Ok(addr)) =>
						Ok(Op::MemSwap(reg, addr as usize)),
					
					(Err(e), _) | (_, Err(e)) => Err(e),
				}
			}
			
			"toffoli" | "toff" | "ccnot" => {
				let rega = get_register(tokens.next());
				let regb = get_register(tokens.next());
				let regc = get_register(tokens.next());
				
				match (rega, regb, regc) {
					(Ok(a), Ok(b), Ok(c)) if c != a && c != b =>
						Ok(Op::CCNot(a, b, c)),
					
					(Ok(_), Ok(_), Ok(_)) =>
						Err("controlled argument used in mutable argument".to_string()),
					
					(Err(e), _, _) | (_, Err(e), _) | (_, _, Err(e)) =>
						Err(e),
				}
			}
			
			"fredkin" | "fredk" | "cswap" => {
				let rega = get_register(tokens.next());
				let regb = get_register(tokens.next());
				let regc = get_register(tokens.next());
				
				match (rega, regb, regc) {
					(Ok(a), Ok(b), Ok(c)) if b != a && c != a =>
						Ok(Op::CSwap(a, b, c)),
					
					(Ok(_), Ok(_), Ok(_)) =>
						Err("controlled argument used in mutable argument".to_string()),
					
					(Err(e), _, _) | (_, Err(e), _) | (_, _, Err(e)) =>
						Err(e),
				}
			}
			
			"jump" | "jmp" => {
				let addr = if let Some(s) = tokens.next() {
					match s.parse::<u16>() {
						Ok(v) if v < 0x1000 => Ok(v as usize),
						Ok(_) => Err("value for argument too big!".to_string()),
						Err(e) => Err(e.description().to_string()),
					}
				} else {
					Err("address argument not found".to_string())
				};
				
				addr.map(Op::Jump)
			}
			
			"jpz" => {
				let addr = if let Some(s) = tokens.next() {
					match s.parse::<u16>() {
						Ok(v) if v < 0x1000 => Ok(v as usize),
						Ok(_) => Err("value for argument too big!".to_string()),
						Err(e) => Err(e.description().to_string()),
					}
				} else {
					Err("address argument not found".to_string())
				};
				
				addr.map(Op::JZero)
			}
			
			other => Err(format!("expected opcode mneumonic, found {}", other)),
		};
		
		// handle comments
		match tokens.next() {
			// no comment, or some other comment marker.
			// don't do anything, because we'll just go to the next line directly
			None | Some("#") | Some(";") => {}
			
			// something that's not a comment marker
			Some(t) =>
				println_err!("Error (line {}): expected comment or line break, found '{}'", line_number, t),
		}
		
		match instr {
			// finally, write instruction if there was no error
			Ok(instr) => {
				let op = encode(instr);
				let data = [(op >> 8) as u8, op as u8];
				
				match output.write(&data) {
					Ok(2) => {}
				
					Ok(1) => {
						println_err!("Error: could not write complete instruction");
						return;
					}
				
					Ok(0) => {
						println_err!("Error: no more space in file to write");
						return;
					}
				
					Ok(_) => unreachable!(),
				
					Err(e) => {
						println_err!("Error: {}", e.description());
						return;
					}
				}
			}
			
			// if there was an error, write line number, description, and code line
			Err(e) => {
				println_err!("Error (line {}): {}\n{}", line_number, e, line);
				
				if fs::remove_file(out_path).is_err() {
					println_err!("Could not delete incomplete file.");
				}
				
				return;
			}
		}
	}
}
