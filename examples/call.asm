jmp 4

# push r0 (sp) r1 (val)
swb r2
inc r0
xchg r1 r0
swb r2
# pop r0 (sp) r1 (reg)

pmj 4
imm r0 255; sp = 255
imm r1 3  ; val = 3
imm r2 1  ; push = 0x0001 + 1
swb r2
rswb r2
hlt
