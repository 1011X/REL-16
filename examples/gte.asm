; x >= a where a: i16, x: i16


;	if a >= 0
;		!(x - a < 0) && !(x < 0)
;	else
;		!((x - a < 0) && (x < 0))

xori r1 127
roli r1 8
xori r1 255; x = 32767

xori r2 5
neg r2; a = -5

; expect r0 = (32767 >= -5) = true
; NOT (-32764 >= 0) = false

xor r3 r1
sub r3 r2; r3 = x - a

js r2 6; if a >= 0 {
	not r1
	not r3
	ccn r3 r1 r0; r0 = ~r3 & ~x
	not r3
	not r1
jmp 3; }
pmj 6; else {
	ccn r3 r1 r0
	not r0; r0 = ~(r3 & x)
ans r2 3; }

; clean-up
add r3 r2
xor r3 r1; r3 = 0

hlt
