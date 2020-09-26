use std::ops::{Index, IndexMut};
use super::reg::Reg;

pub struct RegisterFile(pub [u16; 8]);

impl Index<Reg> for RegisterFile {
	type Output = u16;
	fn index(&self, index: Reg) -> &Self::Output {
		&self.0[index as usize]
	}
}

impl IndexMut<Reg> for RegisterFile {
	fn index_mut(&mut self, index: Reg) -> &mut Self::Output {
		&mut self.0[index as usize]
	}
}
