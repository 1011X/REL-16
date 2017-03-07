use std::collections::HashMap;
use std::convert::From;
use std::io::{self, BufRead, Write};
use std::str::FromStr;
use std::error;
use std::fmt;

use isa::op::{self, Op, Addr};

#[derive(Debug)]
pub enum Error {
	MismatchedLabels,
	IncompleteInstr,
	Line(io::Error),
	Encode(usize, op::Error),
	Decode(usize, op::Error),
}

impl From<io::Error> for Error {
	fn from(e: io::Error) -> Self { Error::Line(e) }
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Error::MismatchedLabels =>
				write!(f, "Mismatched labels in source. Ensure they only come in pairs."),
			Error::IncompleteInstr =>
				write!(f, "Received an incomplete instruction while reading."),
			Error::Line(ref e) =>
				write!(f, "Error reading line: {}", e),
			Error::Encode(n, ref e) =>
				write!(f, "Error encoding instruction at line {}: {}", n, e),
			Error::Decode(n, ref e) =>
				write!(f, "Error decoding instruction at word {}: {}", n, e),
		}
	}
}

impl error::Error for Error {
	fn description(&self) -> &str {
		match *self {
			Error::MismatchedLabels => "mismatched labels",
			Error::IncompleteInstr  => "incomplete instruction",
			Error::Line(..)         => "could not read line",
			Error::Encode(..)       => "failed to encode instruction",
			Error::Decode(..)       => "failed to decode instruction",
		}
	}
	
	fn cause(&self) -> Option<&error::Error> {
		match *self {
			Error::Line(ref e)      => Some(e),
			Error::Encode(_, ref e) => Some(e),
			Error::Decode(_, ref e) => Some(e),
			_                       => None
		}
	}
}

pub fn assemble<I: BufRead>(inp: I) -> Result<Vec<u8>, Error> {
	let mut labtab = HashMap::new();
	
	let lines = inp.lines()
		.collect::<Result<Vec<_>, _>>()?;
	
	let ops = lines.into_iter()
		// mark end of string as index of first comment marker, or its length
		// if none exist, and then trim.
		.map(|l| {
			let end = l.find(';').unwrap_or(l.len());
			l[..end].trim().to_string()
		})
		// keep track of line numbers. MUST go before .filter()
		.enumerate()
		// keep non-empty lines
		.filter(|&(_, ref l)| !l.is_empty())
		// try encoding the line and move the line number into the result if
		// there's an error
		.map(|(n, l)| Op::from_str(&l).map_err(|e| Error::Encode(n + 1, e)))
		// simplify into a result and try! it
		.collect::<Result<Vec<_>, _>>()?;
	
	// collect all labels and their locations
	let mut branches: Vec<(String, usize)> = ops.iter()
		.enumerate()
		.filter_map(|(i, op)| match *op {
			Op::BranchParity(_, Addr::Label(ref l))
			| Op::BranchSign(_, Addr::Label(ref l))
			| Op::AssertParity(_, Addr::Label(ref l))
			| Op::AssertSign(_, Addr::Label(ref l))
			| Op::GoTo(Addr::Label(ref l))
			| Op::ComeFrom(Addr::Label(ref l)) => Some((l.clone(), i)),
			_ => None
		})
		.collect();

	// cluster labels together so it's easier to process them
	branches.sort();

	for pair in branches.chunks(2) {
		// check if chunk is
		if pair.len() == 1                  // uneven,
		|| pair[0].0 != pair[1].0           // or labels mismatch,
		|| labtab.contains_key(&pair[0].0) { // or has been used before.
			return Err(Error::MismatchedLabels);
		}
	
		// create entry for label mapped to offset-1
		labtab.insert(pair[0].0.clone(), pair[1].1 - pair[0].1 - 1);
	}
	
	let instrs = ops.into_iter()
		// change all labels to offsets
		.map(|op| match op {
			Op::BranchParity(r, Addr::Label(ref l)) =>
				Op::BranchParity(r, Addr::Offset(labtab[l])),
			Op::BranchSign(r, Addr::Label(ref l)) =>
				Op::BranchSign(r, Addr::Offset(labtab[l])),
			Op::AssertParity(r, Addr::Label(ref l)) =>
				Op::AssertParity(r, Addr::Offset(labtab[l])),
			Op::AssertSign(r, Addr::Label(ref l)) =>
				Op::AssertSign(r, Addr::Offset(labtab[l])),
			Op::GoTo(Addr::Label(ref l)) =>
				Op::GoTo(Addr::Offset(labtab[l])),
			Op::ComeFrom(Addr::Label(ref l)) =>
				Op::ComeFrom(Addr::Offset(labtab[l])),
				
			_ => op
		})
		.enumerate()
		// encode the instruction
		.map(|(i, op)| op.encode().map_err(|e| Error::Decode(i, e)))
		// try! out any errors if any
		.collect::<Result<Vec<_>, _>>()?;
	
	Ok(instrs.into_iter()
		// transform it into a vector of bytes that can be written.
		.flat_map(|instr| vec![(instr >> 8) as u8, instr as u8])
		.collect()
	)
}

pub fn disassemble<I: BufRead>(inp: &mut I) -> Result<Vec<u8>, Error> {
	let mut out = Vec::new();
	let buf = &mut [0, 0];
	
	// replace instructions with mnemonics
	for i in 0.. {
		// try to read instruction
		match inp.read(buf)? {
			0 => break, // reached eof
			1 => return Err(Error::IncompleteInstr),
			2 => {} // what we expect
			_ => unreachable!()
		}
		
		let instr = (buf[0] as u16) << 8 | buf[1] as u16;
		
		writeln!(out, "{}", Op::decode(instr)
			.map_err(|e| Error::Decode(i, e))?
		)?;
	}
	
	Ok(out)
}
