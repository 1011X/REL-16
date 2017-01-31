;x >= a
;	if a >= 0
;		!(x - a < 0) && !(x < 0)
;	else
;		!((x - a < 0) && (x < 0))

xori r1 1
not r1
rori r1 1; x = 32767

xori r2 5
not r2
inc r2; a = -5
; expect r0 = (32767 >= -5) = true
; OR (-32764 >= 0) = false

xor r3 r1
sub r3 r2
js r2 5
	not r1
	not r3
	ccn r3 r1 r0
	not r3
	not r1
as r2 5
roli r2 1
xori r2 1
rori r2 1
js r2 2
	ccn r3 r1 r0
	not r0
as r2 2
roli r2 1
xori r2 1
rori r2 1

hlt
