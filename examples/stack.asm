; uses built-in stack to push 2 numbers, then
; pops them and adds them

; initialization
not sp      ; sp = 0xffff
not x6      ; bp = 0xffff

; summon 2 values from the void
; and send them to The Stack
xor x0 5    ; x0 = 5
push x0     ; push(x0, sp)

xor x0 3    ; x0 = 3
push x0     ; push(x0, sp)

; summon values from The Stack
pop x0      ; pop(x0, sp)  // x0 = 3
pop x1      ; pop(x1, sp)  // x1 = 5

; sum them
add x0 x1   ; x0 = 8

; result is in x0
halt
