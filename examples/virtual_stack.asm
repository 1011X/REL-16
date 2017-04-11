; Reimplementation of stack operations (without using `pop` and
; `push` instructinos).
; This stack grows to the right.

xori r0 255; stack starts at, say, 0x00FF
xori r1 3; value to be pushed


;; push ;;
inc r0; increment stack pointer
;stack base will always be 0

xchg r1 r0; exchange value of r1 and value of [r0]
; currently: r1 = 0, [r0] = 3


;; pop ;;
; A clean hand must be used when exchanging, lest we receive
; unexpected quantities next time around.

; r2 = 0, [r0] = 3
xchg r2 r0
; r2 = 3, [r0] = 0

dec r0; have stack pointer point to previous value
; in this case, it'll just be base (zero).


; clear memory whose values we know of.
;xori r2 3
;xori r0 255
hlt
