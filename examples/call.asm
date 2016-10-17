jmp 4

; push r0 (sp) r1 (val)
spc r2
inc r0
xchg r1 r0
spc r2
; pop r0 (sp) r1 (reg)

pmj 4
xori r0 255; sp = 255
xori r1 3  ; val = 3
xori r2 1  ; push = 0x0001 + 1
spc r2
rspc r2
hlt
