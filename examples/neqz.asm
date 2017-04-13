; initialization
not sp; sp = 0xFFFF
not bp; bp = 0xFFFF

xori r0 15; value to be checked
push r0
; stack <15]
; r0 = 0

dec sp; make space to store result
; stack = <0, 15]


; proc neqz
; Checks if `input` != 0
; Result is in `result`'s parity bit

; prereqs:
; general-purpose registers are all 0
; stack = <0, input]

; TODO:
; * verify r3 is clean when used.
; * make result be only the parity bit
;   by undoing intermediate calculations.
; * clear registers when done.

pop r1; r1 = 0, <input]
pop r0; r0 = input, <]

xor r1 r0; r1 = r0

; start summing r1's ones in r2

jpe r1 1; bit 0
	inc r2
ape r1 1
rori r1 1

jpe r1 1; bit 1
	inc r2
ape r1 1
rori r1 1

jpe r1 1
	inc r2
ape r1 1
rori r1 1

jpe r1 1; bit 3
	inc r2
ape r1 1
rori r1 1

jpe r1 1
	inc r2
ape r1 1
rori r1 1

jpe r1 1
	inc r2
ape r1 1
rori r1 1

jpe r1 1
	inc r2
ape r1 1
rori r1 1

jpe r1 1; bit 7
	inc r2
ape r1 1
rori r1 1

jpe r1 1
	inc r2
ape r1 1
rori r1 1

jpe r1 1
	inc r2
ape r1 1
rori r1 1

jpe r1 1
	inc r2
ape r1 1
rori r1 1

jpe r1 1
	inc r2
ape r1 1
rori r1 1

jpe r1 1
	inc r2
ape r1 1
rori r1 1

jpe r1 1
	inc r2
ape r1 1
rori r1 1

jpe r1 1
	inc r2
ape r1 1
rori r1 1

jpe r1 1; bit 15
	inc r2
ape r1 1
rori r1 1; complete rotation

not r1; back to original value
xor r1 r0; back to zero

swp r1 r2

; start calculating sum of 1's in first sum
; because arch is 16-bits, there are max 16 1's,
; so only first 5 bits are used, with max number
; of 1's being 4 (to represent 15).
not r1

jpe r1 1; bit 0
	inc r2
ape r1 1
rori r1 1

jpe r1 1; bit 1
	inc r2
ape r1 1
rori r1 1

jpe r1 1
	inc r2
ape r1 1
rori r1 1

jpe r1 1; bit 3
	inc r2
ape r1 1
rori r1 1

jpe r1 1
	inc r2
ape r1 1
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

jpe r1 1; bit 0
	inc r2
ape r1 1
rori r1 1

jpe r1 1; bit 1
	inc r2
ape r1 1
rori r1 1

jpe r1 1; bit 2
	inc r2
ape r1 1
rori r1 1

; restore r1 to first and second sum
roli r1 3
not r1

; roll r1 to the left to store third sum
roli r1 2

; store third sum in r1
xori r3 3
cswp r3 r1 r2
xori r3 3

; start calculating fourth sum
not r1

jpe r1 1; bit 0
	inc r2
ape r1 1
rori r1 1

jpe r1 1; bit 1
	inc r2
ape r1 1
rori r1 1

; restore r1 to first, second, and third sum
roli r1 2
not r1

; roll r1 to the left to store fourth and last sum
roli r1 1
xori r3 1
cswp r3 r1 r2
xori r3 1

push r0
push r1

; postreqs:
; general-purpose registers are all back to 0
; stack = <result, input]

hlt
