mod parser;

//use std::collections::HashMap;
use std::io::{self, BufRead};
use std::str::FromStr;

use crate::isa::{Op, Offset};

// XXX Notes:
// * maybe register and immediate literals (and variables, in the future) should
//   have their own symbols at the start? maybe $ for immediates, @ for
//   registers, and # for variables.
// * should there be assembler directives? if so, they could use % at the start.

pub fn parse(inp: impl BufRead) -> Result<Vec<Op>, String> {
	let lines = inp.lines()
		.collect::<io::Result<Vec<_>>>()
		.map_err(|e| e.to_string())?;
	
	// Pass 1: turn text into internal representation.
	let code = lines.iter()
		// get everything before the comment marker (semicolon) and trim the
		// remaining whitespace
		.map(|line| line.split(';').nth(0).unwrap().trim())
		// keep track of line numbers. MUST go before .filter()
		.enumerate()
		// keep non-empty lines
		.filter(|(_, line)| !line.is_empty())
		// try encoding line, report any error with line number
		.map(|(n, line)| match Op::from_str(line) {
		    Ok(op) => Ok((n, op)),
		    Err(e) => Err(format!("failed to parse line {}, {}", n + 1, e)),
		})
		// simplify into a result and try! it
		.collect::<Result<Vec<_>, String>>()?;
	
    // Pass 2: resolve labels.
	if cfg!(feature = "labels") {
	    // find instructions with labels
	    let mut jumps: Vec<_> = code.iter()
	        // filter `Op`s by those that have labels
	        .filter(|(_, op)| op.get_label().is_some() )
	        .map(|(l, op)| (l, op.clone()) )
		    // keep track of instruction addresses
	        .enumerate()
	        .collect();
        
        jumps.sort_by_key(|(_, (_, op))| op.get_label().unwrap().clone() );
	    
	    // TODO: figure this out. consult the compiler messages.
	    //let mut label_table = HashMap::new();
	    for chunk in jumps.chunks(2) {
	        if chunk.len() < 2
	        || (chunk[0].1).1.get_label() != (chunk[1].1).1.get_label() {
	            return Err(format!(
	                "unpaired label ':{}' (line {})",
	                (chunk[0].1).1.get_label().unwrap(),
	                (chunk[0].1).0
                ));
            }
            
            let _offset = chunk[1].0 - chunk[0].0 - 1;
	    }
    }
	
	Ok(code.into_iter().map(|(_, op)| op).collect())
}
