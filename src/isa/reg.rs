use std::fmt;
use std::str;
use std::error::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg { R0 = 0, R1, R2, R3, R4, R5, SP, BP }

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
	NoPrefix,
	//InvalidIndex(&'a str),
}

impl fmt::Display for Reg {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Reg::R0
			| Reg::R1
			| Reg::R2
			| Reg::R3
			| Reg::R4
			| Reg::R5 =>
				write!(f, "r{}", *self as u8),
			
			Reg::SP => f.write_str("sp"),
			Reg::BP => f.write_str("bp"),
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
			//s if s.starts_with('r') && s.parse::<u8>().is_ok() => 
			_ => Err(ParseError::NoPrefix)
		}
	}
}

impl fmt::Display for ParseError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			ParseError::NoPrefix => self.description().fmt(f),
		}
	}
}

impl Error for ParseError {
	fn description(&self) -> &str {
		match *self {
			ParseError::NoPrefix => "invalid register literal",
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
			6 => Reg::SP,
			7 => Reg::BP,
			_ => panic!("Invalid register value given: {}", val)
		}
	}
}

impl From<u16> for Reg {
	fn from(val: u16) -> Self { Reg::from(val as usize) }
}
