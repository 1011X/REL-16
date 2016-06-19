imm r0 3; value that will be tested

jpo r0 4

# here, r0 is even; we have to ensure
# it's still even when jumping to the
# assertion. let's add 2 to it.
imm r1 2
add r0 r1
imm r1 2

jmp 4
pmj 4

# here, r0 is odd. to ensure it stays
# odd, we add 4 to it.
imm r1 4
add r0 r1
imm r1 4

ape r0 4

hlt
