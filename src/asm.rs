use std::collections::HashMap;
use std::io::{self, BufRead};
use std::str::FromStr;
use std::result;

use isa::{Op, Addr};

type Result<T> = result::Result<T, String>;

pub fn parse<I: BufRead>(inp: I) -> Result<Vec<Op>> {
	let mut label_indices = HashMap::new();
	
	let lines = inp.lines()
		.collect::<io::Result<Vec<_>>>()
		.map_err(|e| e.to_string())?;
	
	let code = lines.iter()
		// get everything before the comment marker (semicolon)
		.map(|l| l.split(';').nth(0).unwrap())
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
			Op::BranchParityOdd(_, Addr::Label(ref label))
			| Op::BranchSignNegative(_, Addr::Label(ref label))
			| Op::AssertParityOdd(_, Addr::Label(ref label))
			| Op::AssertSignNegative(_, Addr::Label(ref label))
			| Op::BranchParityEven(_, Addr::Label(ref label))
			| Op::BranchSignNonneg(_, Addr::Label(ref label))
			| Op::AssertParityEven(_, Addr::Label(ref label))
			| Op::AssertSignNonneg(_, Addr::Label(ref label))
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
			Op::BranchParityOdd(r, Addr::Label(ref label)) =>
				Op::BranchParityOdd(r, Addr::Offset(ltab[label])),
			Op::BranchSignNegative(r, Addr::Label(ref label)) =>
				Op::BranchSignNegative(r, Addr::Offset(ltab[label])),
			Op::AssertParityOdd(r, Addr::Label(ref label)) =>
				Op::AssertParityOdd(r, Addr::Offset(ltab[label])),
			Op::AssertSignNegative(r, Addr::Label(ref label)) =>
				Op::AssertSignNegative(r, Addr::Offset(ltab[label])),
			Op::BranchParityEven(r, Addr::Label(ref label)) =>
				Op::BranchParityEven(r, Addr::Offset(ltab[label])),
			Op::BranchSignNonneg(r, Addr::Label(ref label)) =>
				Op::BranchSignNonneg(r, Addr::Offset(ltab[label])),
			Op::AssertParityEven(r, Addr::Label(ref label)) =>
				Op::AssertParityEven(r, Addr::Offset(ltab[label])),
			Op::AssertSignNonneg(r, Addr::Label(ref label)) =>
				Op::AssertSignNonneg(r, Addr::Offset(ltab[label])),
			Op::GoTo(Addr::Label(ref label)) =>
				Op::GoTo(Addr::Offset(ltab[label])),
			Op::ComeFrom(Addr::Label(ref label)) =>
				Op::ComeFrom(Addr::Offset(ltab[label])),
			_ => op
		})
		.collect()
	)
}
