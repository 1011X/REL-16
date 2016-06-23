xori r0 3; value that will be tested

jp r0 3

# here, r0 is even; we have to ensure
# it's still even when jumping to the
# assertion. let's add 2 to it.
xori r1 2
add r0 r1
xori r1 2

ap 3
xori r0 1
jp 3

# here, r0 is odd. to ensure it stays
# odd, we add 4 to it.
xori r1 4
add r0 r1
xori r1 4

ap r0 3
xori r0 1; setting number back to original value

hlt
