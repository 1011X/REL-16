use std::fmt;
use std::str;
use std::error::Error;

/// Specifies a machine register.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg {
	/// treated as accumulator in some instructions
	R0 = 0,
	R1, R2, R3, R4, R5,
	/// can be used as a stack base register
	R6,
	/// stack pointer
	SP
}

/// Error when parsing register string literal
#[derive(Debug, PartialEq, Eq)]
pub struct ParseError(String);

impl fmt::Display for Reg {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Reg::R0 => write!(f, "r0"),
			Reg::R1 => write!(f, "r1"),
			Reg::R2 => write!(f, "r2"),
			Reg::R3 => write!(f, "r3"),
			Reg::R4 => write!(f, "r4"),
			Reg::R5 => write!(f, "r5"),
			Reg::R6 => write!(f, "r6"),
			
			Reg::SP => write!(f, "sp"),
		}
	}
}


impl str::FromStr for Reg {
	type Err = ParseError;
	fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s {
			"r0" => Ok(Reg::R0),
			"r1" => Ok(Reg::R1),
			"r2" => Ok(Reg::R2),
			"r3" => Ok(Reg::R3),
			"r4" => Ok(Reg::R4),
			"r5" => Ok(Reg::R5),
			"r6" | "bp" => Ok(Reg::R6),
			"r7" | "sp" => Ok(Reg::SP),
			_ => Err(ParseError(s.to_string()))
		}
	}
}

impl From<usize> for Reg {
	fn from(val: usize) -> Self {
		match val {
			0 => Reg::R0,
			1 => Reg::R1,
			2 => Reg::R2,
			3 => Reg::R3,
			4 => Reg::R4,
			5 => Reg::R5,
			6 => Reg::R6,
			7 => Reg::SP,
			_ => unreachable!()
		}
	}
}

impl fmt::Display for ParseError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "No such register: {}", self.0)
	}
}

impl Error for ParseError {
	fn description(&self) -> &'static str { "invalid register literal" }
}
