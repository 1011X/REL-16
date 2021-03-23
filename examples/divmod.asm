; this assembly file is a rough equivalent of the following Janus code:
; procedure divmod(int total, int div, int mod)
;    from div = 0 loop
;        total -= mod
;        div += 1
;    until total < mod
;
; procedure main()
;    int x = 11
;    int div = 0
;    int mod = 3
;    call divmod(x, div, mod)

; proc main()
xor a0 11
xor a1 0
xor s0 3

xor ra 0x08
spc ra
sub ra 13
xor ra 0x08

halt

; proc divmod(total = a0, div = a1, mod = s0)
; pc = 0x05
spc ra

; from div = 0
bro a1 @loop
; rember: ur NOT supposed to take this branch. it's only for looping.
; if it is taken, it breaks the div = 0 assertion and anything can happen.
; and yes, we're only testing if it's odd for simplicity's sake.

; empty do block

xor t0 a0; t0 = total
sub t0 s0; t0 = total - mod
brn t0 @break ; until total - mod < 0
add t0 s0
xor t0 a0

; loop block
sub a0 s0 ; total -= mod
add a1 1  ; div += 1
br @loop

br @break
add t0 s0
xor t0 a0

spc ra
