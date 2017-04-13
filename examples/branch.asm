xori r0 3; value to be judged

jpo r0 4
	; here, r0 is even
	; we must ensure this property holds
	; before reaching the assertion
	xori r1 2
	add r0 r1
	xori r1 2
jmp 4
pmj 4
	; here, r0 is odd
	; same rules of the former case apply
	xori r1 4
	add r0 r1
	xori r1 4

; failure to adhere to the rules above
; will result in the below assertion being
; false, resulting in the skipping of
; instructinos
ape r0 4

hlt
