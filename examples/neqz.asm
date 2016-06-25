# Check if r0 != 0
# Result is in r1's parity bit

# prereqs:
# r0 is input
# mut r1 = 0
# mut r2 = 0

xori r0 15

xor r1 r0; copy r0 to r1

# sum r1's ones in r2
not r1

jp r1 1; bit 0
inc r2
ap r1 1
rori r1 1

jp r1 1; bit 1
inc r2
ap r1 1
rori r1 1

jp r1 1
inc r2
ap r1 1
rori r1 1

jp r1 1; bit 3
inc r2
ap r1 1
rori r1 1

jp r1 1
inc r2
ap r1 1
rori r1 1

jp r1 1
inc r2
ap r1 1
rori r1 1

jp r1 1
inc r2
ap r1 1
rori r1 1

jp r1 1; bit 7
inc r2
ap r1 1
rori r1 1

jp r1 1
inc r2
ap r1 1
rori r1 1

jp r1 1
inc r2
ap r1 1
rori r1 1

jp r1 1
inc r2
ap r1 1
rori r1 1

jp r1 1
inc r2
ap r1 1
rori r1 1

jp r1 1
inc r2
ap r1 1
rori r1 1

jp r1 1
inc r2
ap r1 1
rori r1 1

jp r1 1
inc r2
ap r1 1
rori r1 1

jp r1 1; bit 15
inc r2
ap r1 1
rori r1 1; complete rotation

not r1; back to original value
xor r1 r0; back to zero

swp r1 r2

# sum zeros again
# because arch is 16-bits, there are max 16 1's,
# so only bits [0, 4] are used, with max number
# of 1's being 3 (to represent 8).
not r1

jp r1 1; bit 0
inc r2
ap r1 1
rori r1 1

jp r1 1; bit 1
inc r2
ap r1 1
rori r1 1

jp r1 1
inc r2
ap r1 1
rori r1 1

jp r1 1; bit 3
inc r2
ap r1 1
rori r1 1

jp r1 1
inc r2
ap r1 1
rori r1 1


roli r1 5
not r1


roli r1 3
xori r3 7
cswp r3 r1 r2
xori r3 7
not r1

jp r1 1; bit 0
inc r2
ap r1 1
rori r1 1

jp r1 1; bit 1
inc r2
ap r1 1
rori r1 1

jp r1 1; bit 2
inc r2
ap r1 1
rori r1 1


roli r1 3
not r1


roli r1 2
xori r3 3
cswp r3 r1 r2
xori r3 3
not r1


jp r1 1; bit 0
inc r2
ap r1 1
rori r1 1

jp r1 1; bit 1
inc r2
ap r1 1
rori r1 1


roli r1 2
not r1


roli r1 1
xori r3 1
cswp r3 r1 r2
xori r3 1


# postreqs:
# r1's parity bit is the result
# r2 = 0
