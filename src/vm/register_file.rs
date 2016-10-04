use std::ops::{Index, IndexMut};

use isa::Reg;
use super::rev::Rev;

pub struct RegisterFile(pub [Rev; 8]);

impl Index<Reg> for RegisterFile {
	type Output = Rev;
	fn index(&self, index: Reg) -> &Self::Output {
		&self.0[index as usize]
	}
}

impl IndexMut<Reg> for RegisterFile {
	fn index_mut(&mut self, index: Reg) -> &mut Self::Output {
		&mut self.0[index as usize]
	}
}
