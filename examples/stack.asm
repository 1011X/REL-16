; uses built-in stack to push 2 numbers, then
; pops them and adds them

; initialization
not r6      ; sp = 0xffff
not r7      ; bp = 0xffff

; summon 2 values from the void
; and send them to The Stack
xori r0 5   ; r0 = 5
push r0     ; push(r0, sp)

xori r0 3   ; r0 = 3
push r0     ; push(r0, sp)

; summon values from The Stack
pop r0      ; pop(r0, sp)  // r0 = 3
pop r1      ; pop(r1, sp)  // r1 = 5

; sum them
add r0 r1   ; r0 = 8

; result is in r0
hlt
