use std::collections::HashMap;
use std::io::{self, BufRead};
use std::str::FromStr;
use std::result;

use isa::op::Op;
use isa::op::Addr;

type Result<T> = result::Result<T, String>;
type SymTable = HashMap<String, usize>;

pub fn parse<I: BufRead>(inp: I) -> Result<(Vec<Op>, SymTable)> {
	let mut label_indices = HashMap::new();
	
	let lines = inp.lines()
		.collect::<io::Result<Vec<_>>>()
		.map_err(|e| e.to_string())?;
	
	let code = lines.iter()
		// mark end of string as index of first semicolon
		// (comment marker), or its length if none exist, and
		// then trim.
		.map(|l| {
			let end = l.find(';').unwrap_or(l.len());
			l[..end].trim()
		})
		// keep track of line numbers. MUST go before .filter()
		.enumerate()
		// keep non-empty lines
		.filter(|&(_, l)| !l.is_empty())
		// try encoding line, report any error with line number
		.map(|(n, l)| Op::from_str(l)
			.map_err(|e| format!("Error at line {}: {}", n + 1, e))
		)
		// simplify into a result and try! it
		.collect::<Result<Vec<_>>>()?
		// we're not done!
		.into_iter()
		// keep track of instruction addresses
		.enumerate()
		// write addresses where labels appears in table
		.inspect(|&(i, ref op)| match *op {
			Op::BranchParity(_, Addr::Label(ref label))
			| Op::BranchSign(_, Addr::Label(ref label))
			| Op::AssertParity(_, Addr::Label(ref label))
			| Op::AssertSign(_, Addr::Label(ref label))
			| Op::GoTo(Addr::Label(ref label))
			| Op::ComeFrom(Addr::Label(ref label)) =>
				label_indices.entry(label.clone())
				.or_insert(Vec::with_capacity(2))
				.push(i),
			
			_ => {}
		})
		// don't need the address anymore
		.map(|(_, op)| op)
		// collect the finished product!
		.collect();
	
	// used to store calculated address offsets
	let mut ltab = HashMap::with_capacity(label_indices.len());
	
	for (k, v) in label_indices {
		// ensure label is used exactly twice
		if v.len() != 2 {
			return Err(format!("Label \"{}\" appears {} times. Ensure it only appears 2 times.", k, v.len()));
		}
		// calculate offset (addresses are already ordered!)
		ltab.insert(k, v[1] - v[0] - 1);
	}
	
	Ok((code, ltab))
}
