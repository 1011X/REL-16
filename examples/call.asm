; example of how calling a function works.

; initialize stack registers
not r7; sp = 0xffff
not r6; bp = 0xffff

; fn push(r0): addr = 3, len = 2
jmp 4
spc r5
    subi r7 1  ; sp -= 1
    xchg r0 r7 ; r0 <=> mem[sp]
spc r5
pmj 4
; fn pop(r0): addr = 6

; values to be pushed
xori r0 255; r0 = 255
xori r1  10; r1 = 10

; call setup.
; the actual address of push is 4, but remember
; that PC auto-increments, so we use 3 (+ 1)
xori r5 3; r5 = &push

; push values
spc r5   ; push(255)
swp r0 r1; r0 <> r1
spc r5   ; push(10)

spc r5; push(r0, r1)

; the address left at r5 after the call is equal to:
; addr(push) + len(push) + 1
xori r5 6; reset call register

; when above instruction is commented out, this will
; perfectly undo the last function call.
;rspc r5

hlt
