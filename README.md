# REL-16

REL-16 stands for Reversible/Entropy-Less 16-bit, and is an instruction set architecture (ISA) designed for reversibility of operations. This virtual machine and assembler allow for emulation of such an architecture as an example of [reversible computing](https://en.wikipedia.org/wiki/Reversible_computing "Wikipedia - Reversible computing"), and is intended as an example of what a reversible CPU would be like.

<!--
## Instruction Set

The details of this ISA can be found in the ISA-Spec.md file on this directory.
-->

## Install

Before installing, you'll first need to install the [Rust compiler and Cargo](https://www.rust-lang.org/ "Rust Homepage"). Because this crate isn't on the registry yet, you'll need to download it to install it. Download the project however you want (e.g. GitHub's "Download ZIP" button, `git clone`, etc.) and extract it if necessary. Then, type the following into the terminal:

	cargo install --path CRATE_PATH

...where CRATE_PATH is the path to the crate you've just downloaded.

To get a standalone executable, run

	cargo build

You can add the `--release` flag at the end to optimize it.

## Usage

You can compile an assembly file from the terminal using

	rel16 build FILE

To run a compiled file, just type

	rel16 run FILE

in the terminal, where FILE is the path to the input file. The program will generate the output file on the current directory with the same name as the input file.

When using the `run` command, the VM will show the raw instruction being executed, contents of the registers at each step of the program, and the value of the program counter (pc) register.

## License

The default license is GPLv3, but if desired, an MIT license is available for commercial uses at a price. Contact this repository's owner if you're interested.

**Note regarding derivations:** When making derivatives, to avoid confusing others on which tool builds for what, it's strongly suggested to change the name to something different from REL-16, and recommended to keep it consistent between tools.
