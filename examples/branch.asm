xori r0 3; value that will be tested

jp r0 3

# here, r0 is even; we have to ensure
# it's still even when jumping to the
# assertion. let's add 2 to it.
xori r1 2
add r0 r1
xori r1 2

ap r0 3

xori r0 1; flip parity bit
# if number was odd, it's now even, and vice versa

# upon flipping, the following jump will only happen
# if the number was even, and not if it was odd.
jp r0 3

# here, r0 was odd. if we want the original value back,
# we can flip the parity bit again for this section, but
# we have to make sure to flip it again by the end.

# in this case we don't need the original value, since
# we'll be adding 4, and that won't affect the parity bit
# (because (parity of x) + even = (parity of x)).

xori r1 4
add r0 r1
xori r1 4

ap r0 3
xori r0 1; setting number back to original value

hlt
