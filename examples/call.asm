; initialize stack registers
not sp
not bp

jmp main

; push(sp, val)
spc r2
pop r1; val
pop r0; sp
inc r0
xchg r1 r0
push r0
push r1
spc r2
; pop(sp, val)

pmj main

xori r0 255; r0 = 255
xori r1  10; r1 = 10

push r0
push r1

; the actual address of push is 0x0004, but
; we must consider that the pc takes an extra
; step of its own, so it's 0x0003 (+ 1)
xori r2 3

spc r2; push(r0, r1)
; r2 = 0x0004, address where push() ends

rspc r2; pop(r0, r1)
; perfectly undoes the previous call

hlt
