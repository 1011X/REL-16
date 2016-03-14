use std::fs::File;
use std::error::Error;
use std::path::Path;
use std::path::PathBuf;
use std::io::{
	BufReader, BufWriter,
	BufRead, Write
};

fn parse_byte(s: &str) -> Result<u8, String> {
	s.parse::<u8>()
		.map_err(|e| e.description().to_string())
}

fn parse_reglit(s: &str) -> Result<u8, String> {
	if s.starts_with('r') {
		match parse_byte(&s[1..]) {
			Ok(byte) if byte < 16 =>
				Ok(byte),
			
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
	out_path.set_extension(".o");
	
	let input = BufReader::new(File::open(in_path).unwrap());
	let mut output = BufWriter::new(File::create(out_path).unwrap());
	
	/*
	let mut code: Vec<String> = input.lines()
		.map(|r| r.unwrap())
		.collect();
	
	
	// macro rewriting
	let expand_macro = |op| match op {
		"neg" => {
			
		}
	};
	
	for i in 0..code.len() {
		let mut tokens = code[i].split_whitespace();
		
		match tokens.nth(0).unwrap() {
			"neg" => {
				let ra = get_register(
				code.push(
			}
		}
	}
	*/
	
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
			"halt" => Ok([0x00, 0x00]),
			
			"not" => {
				let reg = get_register(tokens.next());
				
				reg.map(|reg| [0x00, 0x10 | reg])
			}
			
			"rotl" => {
				let reg = get_register(tokens.next());
				
				reg.map(|reg| [0x00, 0x20 | reg])
			}
			
			"rotr" => {
				let reg = get_register(tokens.next());
				
				reg.map(|reg| [0x00, 0x30 | reg])
			}
			
			"incr" => {
				let reg = get_register(tokens.next());
				
				reg.map(|reg| [0x00, 0x40 | reg])
			}
			
			"decr" => {
				let reg = get_register(tokens.next());
				
				reg.map(|reg| [0x00, 0x50 | reg])
			}
			
			"swap" => {
				let regl = get_register(tokens.next());
				let regr = get_register(tokens.next());
				
				match (regl, regr) {
					(Ok(regl), Ok(regr)) =>
						Ok([0x01, regl << 4 | regr]),
					
					(Err(e), _) | (_, Err(e)) => Err(e),
				}
			}
			
			"cnot" => {
				let regc = get_register(tokens.next());
				let regn = get_register(tokens.next());
				
				match (regc, regn) {
					(Ok(regc), Ok(regn)) if regn != regc =>
						Ok([0x02, regc << 4 | regn]),
					
					(Ok(_), Ok(_)) =>
						Err("can't use the same register in cnot".to_string()),
					
					(Err(e), _) | (_, Err(e)) => Err(e),
				}
			}
			/*
			"cadd" => {
				let rctrl = get_register(tokens.next());
				let radd = get_register(tokens.next());
				
				match (rctrl, radd) {
					(Ok(rctrl), Ok(radd)) if radd != rctrl =>
						Ok([0x03, rctrl << 4 | radd]),
					
					(Ok(_), Ok(_)) =>
						Err("can't use the same register in cnot".to_string()),
					
					(Err(e), _) | (_, Err(e)) => Err(e),
				}
			}
			*/
			"lit" => {
				let reg = get_register(tokens.next());
				
				let value = match tokens.next() {
					Some(t) => parse_byte(t),
					_ => Err("no value for lit instruction given".to_string()),
				};
				
				match (reg, value) {
					(Ok(reg), Ok(value)) =>
						Ok([0x10 | reg, value]),
					
					(Err(e), _) | (_, Err(e)) => Err(e)
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
						Ok([0x20 | reg, addr]),
					
					(Err(e), _) | (_, Err(e)) => Err(e),
				}
			}
			
			"toffoli" | "toff" | "ccnot" => {
				let rega = get_register(tokens.next());
				let regb = get_register(tokens.next());
				let regc = get_register(tokens.next());
				
				match (rega, regb, regc) {
					(Ok(a), Ok(b), Ok(c)) if c != a && c != b =>
						Ok([0x30 | a, b << 4 | c]),
					
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
						Ok([0x40 | a, b << 4 | c]),
					
					(Ok(_), Ok(_), Ok(_)) =>
						Err("controlled argument used in mutable argument".to_string()),
					
					(Err(e), _, _) | (_, Err(e), _) | (_, _, Err(e)) =>
						Err(e),
				}
			}
			
			"jump" | "jmp" => {
				let addr = if let Some(s) = tokens.next() {
					match s.parse::<u16>() {
						Ok(v) if v < 0x1000 => Ok(v),
						Ok(_) => Err("value for argument too big!".to_string()),
						Err(e) => Err(e.description().to_string()),
					}
				} else {
					Err("address argument not found".to_string())
				};
				
				addr.map(|addr| [
					0x50 | (addr >> 8) as u8,
					addr as u8
				])
			}
			
			"jpz" => {
				let addr = if let Some(s) = tokens.next() {
					match s.parse::<u16>() {
						Ok(v) if v < 0x1000 => Ok(v),
						Ok(_) => Err("value for argument too big!".to_string()),
						Err(e) => Err(e.description().to_string()),
					}
				} else {
					Err("address argument not found".to_string())
				};
				
				addr.map(|addr| [
					0x60 | (addr >> 8) as u8,
					addr as u8
				])
			}
			
			other => Err(format!("expected opcode mneumonic, found {}", other)),
		};
		
		// handle comments
		match tokens.next() {
			// no comment, or some other comment marker.
			// don't do anything, because we'll just go to the next line directly
			None | Some("#") | Some(";") => {}
			
			// something that's not a comment marker
			Some(t) => {
				println!("Line {}: expected comment or line break, found '{}'", line_number, t);
				return;
			}
		}
		
		match instr {
			// finally, write instruction if there was no error
			Ok(instr) => match output.write(&instr) {
				Ok(2) => {}
				
				Ok(1) => panic!("Error: could not write complete instruction"),
				
				Ok(0) => panic!("Error: no more space in file to write"),
				
				Ok(_) => unreachable!(),
				
				Err(e) => panic!("Error: {}", e.description()),
			},
			
			// if there was an error, write line number, description, and code line
			Err(e) => {
				println!("Line {}: {}\n{}", line_number, e, line);
				// TODO: delete file
				return;
			}
		}
	}
}
