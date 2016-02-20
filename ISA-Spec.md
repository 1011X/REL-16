# REL-16

Instructions in the architecture are split into nybbles (4 bits). The order of these, in decreasing significance of bits, are the opcode, and nybbles a, b, and c. This allows for easier reading of machine code in hex format when debugging.

To maximize space use, instructions that don't require all 16 bits can use 0 in the opcode and subsequent nybbles to use less space.

The machine must have 16 general-use registers, and a special register PC for keeping track of which instruction to execute.

## Halt

This intruction takes no arguments.

Immediately stops the machine.

## Literal

**Note: This instruction is irreversible, and will therefore be removed in the future.**

Takes a register and an unsigned value that fits in 8 bits.

Sets value at register to the unsigned value directly.

## Load

**Note: Because this instruction is irreversible, it will be removed in the future.**

Takes an absolute, 12-bit address pointing to main memory.

Loads contents at the given address into a predetermined register.

## Store

**Note: Because this instruction is irreversible, it will be removed in the future.**

Takes an absolute, 12-bit address pointing to main memory.

Stores contents at a predetermined register into the given address.

## Not

Takes a register.

Flips all bits of value at the given register.

## Left Rotation

Takes a register.

Moves all bits to their next most significant position, "wrapping" the most significant bit to the least significant position.

## Right Rotation

Takes a register.

Moves all bits to their next least significant position, "wrapping" the least significant bit to the most significant position.

## Swap

Takes 2 registers. Order doesn't matter. Using the same register does nothing.

Swaps the values stored in the given registers.

## Controlled Not

Takes 2 registers. Second register must not be the same as the first.

Takes the XOR of the first and second registers, and assigns the result to the second register.

## Toffoli

Takes 3 registers. Third register must not be the same as either the first or the second.

The Toffoli gate takes 3 bits, *a*, *b*, and *c*. If *a* and *b* are 1, it flips the value of *c*. For each bit position in a register, the gate takes bit *a* from the first register, *b* from the second, and *c* from the third.

## Fredkin

Takes 3 registers. First register must not be the same as either the second or the third.

The Fredkin gate takes 3 bits, *a*, *b*, and *c*. If *a* is 1, it swaps the values of *b* and *c*. For each bit position in a register, the gate takes bit *a* from the first register, *b* from the second, and *c* from the third.

## Jump

**Note: This instruction is irreversible, but it's functionality will be revised in the future.**

Takes an absolute, 12-bit address pointing to main memory.

Sets the value of PC register to the given address.

## Conditional Branch

**Note: This instruction is irreversible, but it's functionality will be revised in the future.**

Takes an absolute, 12-bit address pointing to main memory.

If a predetermined register has a value of zero, the PC register is set to the given address.

