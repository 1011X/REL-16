use std::collections::HashMap;
use std::io::{self, BufRead};
use std::str::FromStr;

use crate::isa::{Op, Addr};

pub fn parse(inp: impl BufRead) -> Result<Vec<Op>, String> {
	let mut label_indices = HashMap::new();
	
	let lines = inp.lines()
		.collect::<io::Result<Vec<_>>>()
		.map_err(|e| e.to_string())?;
	
	let code = lines.iter()
		// get everything before the comment marker (semicolon) and trim the
		// remaining whitespace
		.map(|l| l.split(';').nth(0).unwrap().trim())
		// keep track of line numbers. MUST go before .filter()
		.enumerate()
		// keep non-empty lines
		.filter(|&(_, l)| !l.is_empty())
		// try encoding line, report any error with line number
		.map(|(n, l)|
			Op::from_str(l)
			.map_err(|e| format!("Error at line {}: {}", n + 1, e))
		)
		// simplify into a result and try! it
		.collect::<Result<Vec<_>, String>>()?
		// we're not done!
		.into_iter()
		// keep track of instruction addresses
		.enumerate()
		// write addresses where labels appears in table
		.inspect(|&(i, ref op)| match op {
			Op::BranchOdd(_, Addr::Label(label))
			| Op::BranchNeg(_, Addr::Label(label))
			| Op::AssertOdd(_, Addr::Label(label))
			| Op::AssertNeg(_, Addr::Label(label))
			| Op::BranchEven(_, Addr::Label(label))
			| Op::BranchNotNeg(_, Addr::Label(label))
			| Op::AssertEven(_, Addr::Label(label))
			| Op::AssertNotNeg(_, Addr::Label(label)) =>
				label_indices.entry(label.clone())
				.or_insert_with(|| Vec::with_capacity(2))
				.push(i),
			
			Op::GoTo(Addr::Label(label))
			| Op::ComeFrom(Addr::Label(label)) =>
				label_indices.entry(label.clone())
				.or_insert_with(|| Vec::with_capacity(2))
				.push(i),
			
			#[cfg(feature = "teleport")]
			Op::Teleport(Addr::Label(label)) =>
				label_indices.entry(label.clone())
				.or_insert_with(|| Vec::with_capacity(2))
				.push(i),
			
			_ => {}
		})
		// don't need the address anymore
		.map(|(_, op)| op)
		// collect the finished product!
		.collect::<Vec<_>>();
	
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
	
	// turn all labels into offsets
	Ok(code.into_iter()
		.map(|op| match op {
			Op::BranchOdd(r, Addr::Label(ref label)) =>
				Op::BranchOdd(r, Addr::Offset(ltab[label])),
			Op::BranchNeg(r, Addr::Label(ref label)) =>
				Op::BranchNeg(r, Addr::Offset(ltab[label])),
			Op::AssertOdd(r, Addr::Label(ref label)) =>
				Op::AssertOdd(r, Addr::Offset(ltab[label])),
			Op::AssertNeg(r, Addr::Label(ref label)) =>
				Op::AssertNeg(r, Addr::Offset(ltab[label])),
			Op::BranchEven(r, Addr::Label(ref label)) =>
				Op::BranchEven(r, Addr::Offset(ltab[label])),
			Op::BranchNotNeg(r, Addr::Label(ref label)) =>
				Op::BranchNotNeg(r, Addr::Offset(ltab[label])),
			Op::AssertEven(r, Addr::Label(ref label)) =>
				Op::AssertEven(r, Addr::Offset(ltab[label])),
			Op::AssertNotNeg(r, Addr::Label(ref label)) =>
				Op::AssertNotNeg(r, Addr::Offset(ltab[label])),
			
			#[cfg(feature = "teleport")]
			Op::Teleport(Addr::Label(ref label)) =>
				Op::Teleport(Addr::Offset(ltab[label])),
			Op::GoTo(Addr::Label(ref label)) =>
				Op::GoTo(Addr::Offset(ltab[label])),
			Op::ComeFrom(Addr::Label(ref label)) =>
				Op::ComeFrom(Addr::Offset(ltab[label])),
			_ => op
		})
		.collect()
	)
}
