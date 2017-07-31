macro_rules! swap(
	($left: expr, $right: expr) => {
		::std::mem::swap(&mut $left, &mut $right);
	}
);
