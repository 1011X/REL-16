; simple system initialization program

; the memory is split in half. the lower half is for the heap,
; and the higher half for the stack.

; initialize stack and base pointer to last address.
; it's necessary to keep these values in range.
not sp; sp = 0xffff
not x6; bp = 0xffff

; make stack sentinel value unique so it's easy to identify.
;xor x0 0xDE   ; 0x00DE
;rol x0 8      ; 0xDE00
;xor x0 0xAD   ; 0xDEAD
xor x0 0xDEAD
xchg x0 sp    ; x0 <> mem[sp]

; push and pop the stack. enforces stack boundary.
; fn push(x0): addr = 0x0007, len = 10
jmp @push
spc x5
    ; boundary check. check that next address is in range.
    ; can't check SP directly bc then the assertion afterwards won't hold.
    ; instead, copy value to x1 and check that.
    xor x1 sp      ; x1 ^= sp
    
    ; x1 is decremented bc we're checking the *next* address is valid.
    dec x1         ; x1 -= 1

    br.neg x1 +2   ; if x1 < 0x8000 // stack overflow
        halt       ;     halt
    jmp +3
    jmp -2         ; else
        sub sp 1   ;     sp -= 1
        xchg x0 sp ;     x0 <=> mem[sp]
    br.neg x1 -3   ; fi x1 < 0x8000

    xor x1 sp      ; x1 ^= sp
spc x5
pmj @push
; fn pop(x0): addr = 0x0012


; TODO finish heap section
; now we need to set up the free list information for the heap.
; we're using the reversible buddy memory allocator.
; heap information can be stored in 2 different places:
; * the stack (used here)
; * the memory reserved for the heap
; when using the second option, keep in mind to disable, or "pre-reserve", the
; blocks that the heap data will take up when initializing it. apart from that,
; everything else here should stay the same, if not mostly similar.

; the smallest unit that can be allocated is a 256-byte "basic" block.
; a bit-array of all basic block entries are kept.
; there's also an array for "meta"-blocks which starts with 1 32K-block, 2 16K-
; blocks, 4 8K-blocks, and so on. it does not include basic blocks.
; the number of basic blocks is:
;     128 blocks = 32768 words ÷ 256 words/block
; the number of meta blocks is:
;     127 blocks
; an allocation will calculate the smallest block size that can contain the
; given size, and then find a block to use. we have 2 options here:
; * traverse the meta block array from the top like a binary tree until we reach
;   an available block with the wanted size.
; * linearly search all blocks of the same wanted size.

; allocate 16 words on the stack to store the heap information.
sub sp 16

; TODO describe how to derive address and size from index of bit.
; TODO the rest of this.

; might be useful:

; approx. algorithm for finding free block using a binary tree.
; 
; def find(node n, size s):
;   if n.full():
;     return false
;   else:
;     if n.size = round_up_power_of_two(s):
;       if n.left.empty() && n.right.empty():
;         n.set_full()
;         return true
;       else:
;         return false
;     else:
;       return find(n.left, s) || find(n.right, s)

; this table gives the memory level for the allocation size requested.
;
;   [2^15 + 1, 2^16) => error, null ptr
;   [2^14 + 1, 2^15 + 1) => level 0
;   [2^13 + 1, 2^14 + 1) => level 1
;   [2^12 + 1, 2^13 + 1) => level 2
;   [2^11 + 1, 2^12 + 1) => level 3
;   [2^10 + 1, 2^11 + 1) => level 4
;   [2^9  + 1, 2^10 + 1) => level 5
;   [2^8  + 1, 2^9  + 1) => level 6
;   [0       , 2^8  + 1) => level 7
;
; notice what happens when we use `size - 1` instead.
;
;   [2^15, 2^16 - 1) => error, null ptr, "level 8"
;   [2^14, 2^15) => level 0
;   [2^13, 2^14) => level 1
;   [2^12, 2^13) => level 2
;   [2^11, 2^12) => level 3
;   [2^10, 2^11) => level 4
;   [2^9 , 2^10) => level 5
;   [2^8 , 2^9 ) => level 6
;   [0   , 2^8 ) => level 7
;   2^16 - 1     => error, null ptr, "level 8"
;
; note how an allocation of zero bytes maps to `2^16 - 1`, which will give us an
; error. This is considered acceptable behavior.
; this allows us to sequentially check each bit from highest to lowest to get
; the level of the allocation.

; pub fn malloc(size s, *void ptr):
    ; let r = s
    ; r -= 1
    ; if r >= 0x8000:
    ;   ptr ^= 0x8000
    ;   // how do we handle this?
    ; else:
    ;   if r >= 0x4000:
    ;     lvl ^= 0
    ;   elif r >= 0x2000:
    ;     lvl ^= 1
    ;   elif r >= 0x1000:
    ;     lvl ^= 2
    ;   elif r >= 0x0800:
    ;     lvl ^= 3
    ;   elif r >= 0x0400:
    ;     lvl ^= 4
    ;   elif r >= 0x0200:
    ;     lvl ^= 5
    ;   elif r >= 0x0100:
    ;     lvl ^= 6
    ;   else:
    ;     lvl ^= 7
    ;   
    ;   let block = 0
    ;   let i = 1 << lvl
    ;   from i = 1 << lvl:
    ;     if !taken[i]:
    ;       taken[i] = false
    ;       block ^= i
    ;     fi block = i
    ;   until block = i:
    ;     i += 1
    ;   drop i = 1 << (lvl + 1)
    ; r += 1
    ; r ^= s

; fn level(size: x1, lvl: x0)
jmp @func_level
spc x5
    sub x1 1; size -= 1
    bn x1 @here   ; if size[15] = 1:
        xor x0 8
    bn x1 @here   ; fi size[15] = 1
    add x1 1
spc x5
pmj @func_level

halt
