# REL-16

REL-16 stands for Reversible/Entropy-Less 16-bit, and is an instruction set designed to be able to run both forwards and backwards. This virtual machine and assembler allow for emulation of such an architecture as an example of [reversible computing](https://en.wikipedia.org/wiki/Reversible_computing "Wikipedia - Reversible computing"), and is intended as an example of what a reversible CPU would be like.

## Install

Before installing, you'll first need to install [the Rust compiler, along with Cargo](https://www.rust-lang.org/ "Rust Homepage"). Because this crate isn't on the registry yet, you'll need to download it to install it. Download the project however you want, extracting it if necessary. Then, `cd` into the extracted folder and run the following:

	cargo install

To get a standalone executable, run

	cargo build --release

You can find the executable in `target/release/`.

## Usage

You can compile an assembly file from the terminal using

	rel asm FILE -o OUTPUT_FILE

To run a compiled file, just type

	rel run FILE --verbose

in the terminal, where FILE is the path to the input file.

When using the `--verbose` option with the `run` command, the VM will show the raw and pretty-printed instruction being executed, and the contents of the registers and stack at each step of the program.

You can also disassemble built files similarly using:

	rel dasm FILE -o OUTPUT_FILE

## License

The default license is GPLv3, but if desired, an MIT license is available for commercial uses at a price. Contact this repository's owner if you're interested.
