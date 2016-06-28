use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg { R0 = 0, R1, R2, R3, R4, R5, R6, R7 }

impl fmt::Display for Reg {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "r{}", *self as u8)
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
			7 => Reg::R7,
			v => panic!("Invalid register value given: {}", v)
		}
	}
}

impl From<u8> for Reg {
	fn from(val: u8) -> Self { Reg::from(val as usize) }
}

impl From<u16> for Reg {
	fn from(val: u16) -> Self { Reg::from(val as usize) }
}
