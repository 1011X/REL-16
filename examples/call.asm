; example of how calling a function works.

; fn push(r0): addr = 2, len = 2
jmp 4
spc r5; push() starts here
    sp -= 1
    xchg r0 r7 ; r0 <=> mem[sp]
spc r5; push() ends here
pmj 4
; fn pop(r0): addr = 5

; values to be pushed
r0 := 17

; call setup.
; push actually starts at 3, but remember
; that PC auto-increments, so we use 2 (+ 1)
r5 := 2; r5 = addr(push)

; push values
spc r5   ; call push

; the address left at r5 after the call is equal to:
; addr(push) + len(push) + 1
r5 := 5; reset call register

; again, but in reverse
r5 := 5  ; r5 = addr(pop)
rspc r5  ; call pop
r0 := 16
r5 := 2  ; reset call register

; end result should be: r0 = 1

hlt
