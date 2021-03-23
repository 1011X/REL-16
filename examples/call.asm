; example of how calling a function works.

; fn push(r0): addr = 2, len = 2
jmp +4
spc r5; push() starts here
    sub sp 1
    xchg x0 sp ; r0 <> mem[sp]
spc r5; push() ends here
jmp -4
; fn pop(r0): addr = 5

; values to be pushed
xor x0 17

; call setup.
; push actually starts at 3, but remember
; that PC auto-increments, so we use 2 (+ 1)
xor x5 2; r5 = addr(push)

; push values
spc x5   ; call push

; the address left at r5 after the call is equal to:
; addr(push) + len(push) + 1
xori x5 5; reset call register

; again, but in reverse
xor x5 5  ; r5 = addr(pop)
rspc x5    ; call pop
xor x0 16
xor x5 2  ; reset call register

; end result should be: r0 = 1

halt
