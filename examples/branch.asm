xori r0 3; value that will be tested

jpo r0 A

; here, r0 is even; we have to ensure
; it's still even when jumping to the
; assertion. let's add 2 to it.
xori r1 2
add r0 r1
xori r1 2

apo r0 A

xori r0 1; flip parity bit
; if number was odd, it's now even, and vice versa

; upon flipping, the following jump will only happen
; if the number was even, and not if it was odd.
jpo r0 B

; here, r0 was odd. if we want the original value back,
; we can flip the parity bit again for this section, but
; we have to make sure to flip it again by the end.

; in this case we don't need the original value, since
; we'll be adding 4, and that won't affect the parity bit
; (because (parity of x) + even = (parity of x)).

xori r1 4
add r0 r1
xori r1 4

apo r0 B
xori r0 1; setting number back to original value

hlt
