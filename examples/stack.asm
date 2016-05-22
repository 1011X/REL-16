# uses built-in stack to push 2 numbers, then
# pops them and adds them

# sp = r7
inc r0      # ++r0  // r0 = 1
push r0     # push(r0, sp)
inc r0      # ++r0
inc r0      # ++r0  // r0 = 2
push r0     # push(r0, sp)
pop r0      # pop(r0, sp)  // r0 = 2
pop r1      # pop(r1, sp)  // r1 = 1
add r0 r1   # r0 += r1
hlt
