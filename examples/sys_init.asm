; simple system initialization program

; the memory is split in half. the lower half is for the heap,
; and the higher half for the stack.

; initialize stack and base pointer to last address.
; it's necessary to keep these values in range.
not r7; sp = 0xffff
not r6; bp = 0xffff

; make stack sentinel value unique so it's easily identifiable.
xori r0 222   ; 0x00DE
roli r0 8     ; 0xDE00
xori r0 173   ; 0xDEAD 0x1a57
xchg r0 r7    ; r0 <> mem[sp]

; push and pop the stack. enforces stack boundary.
; fn push(r0): addr = 0x0007, len = 10
jmp 12
spc r5
    ; boundary check. check that next address is in range.
    ; can't check SP directly bc then the assertion afterwards won't hold.
    ; instead, copy value to r1 and check that.
    ; r1 is decremented bc we're checking the *next* address is valid.
    xor r1 r7      ; r1 ^= sp
    subi r1 1      ; r1 -= 1

    js r1 2        ; if r1 < 0x8000
        hlt        ;     halt
    jns r1 3
    as r1 2        ; else
        subi r7 1  ;     sp -= 1
        xchg r0 r7 ;     r0 <=> mem[sp]
    ans r1 3       ; fi r1 < 0x8000

    xor r1 r7      ; r1 ^= sp
spc r5
pmj 12
; fn pop(r0): addr = 0x0012


; now we need to set up the free list information for the heap

hlt
