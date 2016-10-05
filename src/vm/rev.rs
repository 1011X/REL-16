use std::ops::{
	AddAssign,
	SubAssign,
	BitXorAssign,
	ShlAssign,
	ShrAssign,
	//Not,
	//Neg
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Rev(pub u16);

impl AddAssign for Rev {
	fn add_assign(&mut self, rhs: Rev) {
		self.0 = self.0.wrapping_add(rhs.0);
	}
}

impl AddAssign<u16> for Rev {
	fn add_assign(&mut self, rhs: u16) {
		self.0 = self.0.wrapping_add(rhs);
	}
}

impl SubAssign for Rev {
	fn sub_assign(&mut self, rhs: Rev) {
		self.0 = self.0.wrapping_sub(rhs.0);
	}
}

impl SubAssign<u16> for Rev {
	fn sub_assign(&mut self, rhs: u16) {
		self.0 = self.0.wrapping_sub(rhs);
	}
}

impl BitXorAssign for Rev {
	fn bitxor_assign(&mut self, rhs: Rev) {
		self.0 ^= rhs.0;
	}
}

impl BitXorAssign<u16> for Rev {
	fn bitxor_assign(&mut self, rhs: u16) {
		self.0 ^= rhs;
	}
}

impl ShlAssign<Rev> for Rev {
	fn shl_assign(&mut self, rhs: Rev) {
		self.0 = self.0.rotate_left(rhs.0 as u32);
	}
}

impl ShlAssign<u16> for Rev {
	fn shl_assign(&mut self, rhs: u16) {
		self.0 = self.0.rotate_left(rhs as u32);
	}
}

impl ShrAssign<Rev> for Rev {
	fn shr_assign(&mut self, rhs: Rev) {
		self.0 = self.0.rotate_right(rhs.0 as u32);
	}
}

impl ShrAssign<u16> for Rev {
	fn shr_assign(&mut self, rhs: u16) {
		self.0 = self.0.rotate_right(rhs as u32);
	}
}
