; Stack device must start at port 0.
; Use `--garbage-stack` option in terminal as the first device option.

xori r0 1 ; PUSH
xori r1 3 ; data
xori r2 0 ; NOP

io r1 1   ; port[1] <- 3
io r0 0   ; port[0] <- PUSH
io r0 0   ; port[0] <- NOP; // Stops stack from auto-pushing afterwards.
          ;                 // Otherwise, the IO instruction is invoked, the
          ;                 // device still sees PUSH in the command port, and
          ;                 // would push again.

xori r1 5 ; data

io r1 1   ; port[1] <- 5
io r0 0   ; port[0] <- PUSH
io r0 0   ; port[0] <- NOP

hlt

; Stack device data should look like this:
; <5, 3]
