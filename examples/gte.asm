; x >= a where a: i16, x: i16


;	if a >= 0
;		!(x - a < 0) && !(x < 0)
;	else
;		!((x - a < 0) && (x < 0))

xor x1 127
rol x1 8
xor x1 255; x = 32767

xor x2 5
neg x2; a = -5

; expect x0 = (32767 >= -5) = true
; NOT (-32764 >= 0) = false

xor x3 x1
sub x3 x2; x3 = x - a

js x2 6; if a >= 0 {
	not x1
	not x3
	ccn x3 x1 x0; x0 = ~x3 & ~x
	not x3
	not x1
jmp 3; }
pmj 6; else {
	ccn x3 x1 x0
	not x0; x0 = ~(x3 & x)
ans x2 3; }

; clean-up
add x3 x2
xor x3 x1; x3 = 0

halt
