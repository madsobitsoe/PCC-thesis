(set-logic BV)
(set-option :produce-proofs true)
(set-option :proof-format-mode lfsc)
(set-option :dump-proofs true)
(set-option :produce-assignments true)

; We work with the following program, which should clearly be verifiable
; 1: mov r0, 2048
; 2: jeq r1, 0, SKIPDIV ; assume r1 is readable at beginning of program
; 3: div64 r0, r1
; SKIPDIV: ; "fake" label so we can jump over the division
; 4: exit ; r0 = 2048 \/ r0 = 2048 / r1

; VC:
; \A r0 \A r1 .
;         r0 = 2048 =>
;         (r1 = 0)
;         \/
;         (r1 != 0 => r0 = r0 / r1)
;         => true


; Hacky way of doing "implies true" at the end of implications
(declare-const post (_ BitVec 1))

; "Registers" used by the program
(declare-fun r1 () (_ BitVec 64))   ; We have some value in r1 at the beginning of the program
(declare-fun r0_1 () (_ BitVec 64)) ; We assign to register r0 in line 1
(declare-fun r0_3 () (_ BitVec 64)) ; we (maybe)update register r0 in line 3


; To check for validity, we need the negation of our assert(s), i.e:
; VC = \A r0 ... \A r10 => VC_0
; The program is valid if `not VC` is unsatisfiable

(assert
    (not
        (=>
            (= r0_1 #x0000000000000800)
            (=>
	        (or
		    (= r1 (_ bv0 64))
		    (=> (not (= r1 (_ bv0 64))) (= r0_3 (bvudiv r0_1 r1)))
		)
		(= post post)
            )
        )
    )
)


(check-sat)
(exit)
