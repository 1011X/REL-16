use std::fmt;
use std::str;
use std::error::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg { R0 = 0, R1, R2, R3, R4, R5, SP, BP }

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
			
			Reg::SP => write!(f, "sp"),
			Reg::BP => write!(f, "bp"),
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
			"r6" | "sp" => Ok(Reg::SP),
			"r7" | "bp" => Ok(Reg::BP),
			_ => Err(ParseError(s.to_string()))
		}
	}
}

impl fmt::Display for ParseError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "No such register: {}", self.0)
	}
}

impl Error for ParseError {
	fn description(&self) -> &str {
		"invalid register literal"
	}
}
