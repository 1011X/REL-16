; initilization
not r6; init sp
not r7; init bp

dec r6; make space to store result

xori r0 15; value to be checked
push r0

; Check if r0 != 0
; Result is in r1's parity bit

; prereqs:
; stack[0] is input
; stack[1] is result
; mut r1 = 0

; TODO: ensure r3 is clean when used

xor r4 r7


xchg r0 r6; input
xchg r1 r; result

xor r1 r0; copy r0 to r1
push r2; local variable; ensure r2 is 0

; sum r1's ones in r2
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

; sum zeros again
; because arch is 16-bits, there are max 16 1's,
; so only first 5 bits are used, with max number
; of 1's being 4 (to represent 15).
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

; restore r1 to the first sum
roli r1 5
not r1

; roll r1 to the left to store second sum
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

; restore r1 to first and second sum
roli r1 3
not r1

; roll r1 to the left to store third sum
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

; restore r1 to first, second, and third sum
roli r1 2
not r1

; roll r1 to the left to store fourth and last sum
roli r1 1
xori r3 1
cswp r3 r1 r2
xori r3 1

; all bits swapped into r2 were zero, so it can be
; safely returned to the stack
pop r2
hlt

; postreqs:
; r1's parity bit is the result
; r2 = 0
