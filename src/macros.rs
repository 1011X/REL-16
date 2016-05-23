macro_rules! print_err(
    ($($arg: tt)*) => {{
    	use std::io::Write;
        let result = write!(&mut ::std::io::stderr(), $($arg)*);
        
        if let Err(e) = result {
        	panic!("failed printing to stderr: {}", e);
        }
    }}
);

macro_rules! println_err(
    ($($arg: tt)*) => {{
    	use std::io::Write;
        let result = writeln!(&mut ::std::io::stderr(), $($arg)*);
        
        if let Err(e) = result {
        	panic!("failed printing to stderr: {}", e);
        }
    }}
);

macro_rules! try_err(
	($e: expr) => {{
		match $e {
			Ok(val) => val,
			
			Err(e) => {
				println_err!("{}", e);
				return;
			}
		}
	}}
);

macro_rules! swap(
	($left: expr, $right: expr) => {{
		use std::mem::swap;
		
		swap(&mut $left, &mut $right);
	}}
);
