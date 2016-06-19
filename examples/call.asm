jmp 4

# push r0 (sp) r1 (val)
sp r2
inc r0
xchg r1 r0
sp r2
# pop r0 (sp) r1 (reg)

pmj 4
imm r0 255; sp = 255
imm r1 3  ; val = 3
imm r2 1  ; push = 0x0001 + 1
sp r2
rsp r2
hlt
