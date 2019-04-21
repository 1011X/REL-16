# REL-16

**REL-16** is an instruction set designed to run both forwards and backwards. This virtual machine and assembler allow for emulation of such an architecture as an example of [reversible computing](https://en.wikipedia.org/wiki/Reversible_computing "Wikipedia - Reversible computing"), and is intended as an example of what a reversible CPU and toolchain would be like.

REL stands for **R**eversible and **E**ntropy-**L**ess.

## Install

Before installing, you'll first need to install [the Rust compiler, along with Cargo](https://www.rust-lang.org/ "Rust Homepage"). Because this crate isn't on the registry yet, you'll need to clone this repository and install it. Once you've cloned it, `cd` into the project and run the following:

	cargo install --path .

## Usage

⚠️ **Note**: the project is currently in flux, so this may not be accurate.

You can compile an assembly file from the terminal using

	rel asm FILE -o OUTPUT_FILE

To run a compiled file, just type

	rel run FILE --verbose

in the terminal, where FILE is the path to the input file.

When using the `--verbose` option with the `run` command, the VM will show the raw and pretty-printed instruction being executed, and the contents of the registers and stack at each step of the program.

You can also disassemble built files similarly using:

	rel dasm FILE -o OUTPUT_FILE

## Manual

Components:
+ General purpose registers: `r0 - r5`, `r6 = bp`, `r7 = sp`.
+ Program counter register (PC)
+ Branch register (BR)
+ Direction bit (DIR)

Upon initialization, everything is set to zero, except BR which is set to 1. When an instruction is done executing, the value of BR will be added to PC, and the instruction at that location will execute.

Addresses to memory point to word-sized (16-bit) locations. This means the largest address is still `0xFFFF`, but the maximum size of the memory is 128 KiB.

When the DIR bit is active, the CPU is running in reverse mode. This means that instructions are inverted before being executed, and BR is *subtracted* from PC when fetching.

Instruction     | Special syntax | Description
----------------|----------------|----------------------
`halt`          | N/A            | Halts the machine
`nop`           | N/A            | Does nothing at all
`dbg`           | N/A            | For debugging purposes (unimplemented)
`not r0`        | None           | NOTs all bits in `r0`
`swp r0 r1`     | `r0 <> r1`     | Swaps values of `r0` and `r1`
`xor r0 r1`     | `r0 := r1`     | XORs `r0` with `r1`
`add r0 r1`     | `r0 += r1`     | Adds `r1` to `r0`
`sub r0 r1`     | `r0 -= r1`     | Subtracts `r1` from `r0`
`rol r0 r1`     | None           | Rotates `r0` to the left by `r1` bits
`ror r0 r1`     | None           | Rotates `r0` to the right by `r1` bits
`xori r0 37`    | `r0 := 37`     | XORs `r0` with 37
`addi r0 37`    | `r0 += 37`     | Adds 37 to `r0`
`subi r0 37`    | `r0 -= 37`     | Subtracts 37 from `r0`
`roli r0 5`     | None           | Rotates `r0` to the left by 5 bits
`rori r0 5`     | None           | Rotates `r0` to the right by 5 bits
`ccn r0 r1 r2`  | None           | XORs `r0` with result of `r1 AND r2`
`cswp r0 r1 r2` | None           | Swaps bits of `r1` and `r2` based on `r0`
`xchg r0 r1`    | None           | Exchanges value in `r0` and value pointed to in memory by `r1`
`io r0 PORT`    | None           | Exchanges value in `r0` and value at `PORT` in IO buffer (unimplemented)
`spc r0`        | None           | Swaps values of `r0` and PC
`rspc r0`       | None           | Swaps values of `r0` and PC, and flips DIR
`jmp ADD`       | None           | Increases BR by `ADD`
`pmj SUB`       | None           | Decreases BR by `SUB`
`tp MASK`       | None           | XORs BR with `MASK` (experimental)
`jpo r0 OFFSET` | None           | Increases BR by `OFFSET` if `r0` is odd
`jpe r0 OFFSET` | None           | Increases BR by `OFFSET` if `r0` is even
`js r0 OFFSET`  | None           | Increases BR by `OFFSET` if `r0` is less than zero
`jns r0 OFFSET` | None           | Increases BR by `OFFSET` if `r0` is greater than or equal to zero
`apo r0 OFFSET` | None           | Decreases BR by `OFFSET` if `r0` is odd
`ape r0 OFFSET` | None           | Decreases BR by `OFFSET` if `r0` is even
`as r0 OFFSET`  | None           | Decreases BR by `OFFSET` if `r0` is less than zero
`ans r0 OFFSET` | None           | Decreases BR by `OFFSET` if `r0` is greater than or equal to zero

## TODO

- [ ] Add special syntax for more instructions
- [ ] Improve label resolution
- [ ] Implement register allocation
- [ ] Implement register spillage?

## License

This project is licensed under the [BSD Zero Clause License](https://choosealicense.com/licenses/0bsd/). In general it means you can do whatever you want with it, but there are no warranties and I'm not held liable for whatever happens.
