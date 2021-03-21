; initialization
not sp; sp = 0xFFFF
not x6; bp = 0xFFFF

xor x0 15; value to be checked
push x0
; stack <15]
; x0 = 0

dec sp; make space to store result
; stack = <0, 15]


; proc neqz
; Checks if `input` != 0
; Result is in `result`'s parity bit

; prereqs:
; general-purpose registers are all 0
; stack = <0, input]

; TODO:
; * verify x3 is clean when used.
; * make result be only the parity bit
;   by undoing intermediate calculations.
; * clear registers when done.

pop x1; x1 = 0, <input]
pop x0; x0 = input, <]

xor x1 x0; x1 = x0

; start summing x1's ones in x2

br.even x1 @1; bit 0
	inc x2
br.even x1 @1
ror x1 1

br.even x1 1; bit 1
	inc x2
br.even x1 1
ror x1 1

br.even x1 1
	inc x2
br.even x1 1
ror x1 1

jpe x1 1; bit 3
	inc x2
ape x1 1
rori x1 1

jpe x1 1
	inc x2
ape x1 1
rori x1 1

jpe x1 1
	inc x2
ape x1 1
rori x1 1

jpe x1 1
	inc x2
ape x1 1
rori x1 1

jpe x1 1; bit 7
	inc x2
ape x1 1
rori x1 1

jpe x1 1
	inc x2
ape x1 1
rori x1 1

jpe x1 1
	inc x2
ape x1 1
rori x1 1

jpe x1 1
	inc x2
ape x1 1
rori x1 1

jpe x1 1
	inc x2
ape x1 1
rori x1 1

jpe x1 1
	inc x2
ape x1 1
rori x1 1

jpe x1 1
	inc x2
ape x1 1
rori x1 1

jpe x1 1
	inc x2
ape x1 1
rori x1 1

jpe x1 1; bit 15
	inc x2
ape x1 1
rori x1 1; complete rotation

not x1; back to original value
xor x1 x0; back to zero

swp x1 x2

; start calculating sum of 1's in first sum
; because arch is 16-bits, there are max 16 1's,
; so only first 5 bits are used, with max number
; of 1's being 4 (to represent 15).
not x1

jpe x1 1; bit 0
	inc x2
ape x1 1
rori x1 1

jpe x1 1; bit 1
	inc x2
ape x1 1
rori x1 1

jpe x1 1
	inc x2
ape x1 1
rori x1 1

jpe x1 1; bit 3
	inc x2
ape x1 1
rori x1 1

jpe x1 1
	inc x2
ape x1 1
rori x1 1

; restore x1 to the first sum
roli x1 5
not x1

; roll x1 to the left to store second sum
roli x1 3
xori x3 7
cswp x3 x1 x2
xori x3 7
not x1

jpe x1 1; bit 0
	inc x2
ape x1 1
rori x1 1

jpe x1 1; bit 1
	inc x2
ape x1 1
rori x1 1

jpe x1 1; bit 2
	inc x2
ape x1 1
rori x1 1

; restore x1 to first and second sum
roli x1 3
not x1

; roll x1 to the left to store third sum
roli x1 2

; store third sum in x1
xori x3 3
cswp x3 x1 x2
xori x3 3

; start calculating fourth sum
not x1

jpe x1 1; bit 0
	inc x2
ape x1 1
rori x1 1

jpe x1 1; bit 1
	inc x2
ape x1 1
ror x1 1

; restore x1 to first, second, and third sum
rol x1 2
not x1

; roll x1 to the left to store fourth and last sum
rol x1 1
xor x3 1
cswp x3 x1 x2
xor x3 1

push x0
push x1

; postreqs:
; general-purpose registers are all back to 0
; stack = <result, input]

halt
