; Reimplementation of stack operations (without using `pop` and
; `push` instructinos).
; This stack grows to the right.

xor x0 0xff ; stack starts at, say, 0x00FF
xor x1 3    ; value to be pushed


;; push ;;
inc! x0 ; increment stack pointer
; stack base will always be 0

xchg x1 x0 ; exchange value of r1 and value of [r0]
; currently: r1 = 0, [r0] = 3


;; pop ;;
; A clean hand must be used when exchanging, lest we receive
; unexpected quantities next time around.

; r2 = 0, [r0] = 3
xchg x2 x0
; r2 = 3, [r0] = 0

dec x0 ; have stack pointer point to previous value
; in this case, it'll just be base (zero).


; clear memory whose values we know of.
;xori r2 3
;xori r0 255
halt
