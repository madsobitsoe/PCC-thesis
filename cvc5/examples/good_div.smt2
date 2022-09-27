(set-logic BV)
(set-option :produce-proofs true)
(set-option :proof-format-mode lfsc)
(set-option :dump-proofs true)
(set-option :produce-assignments true)

; We work with the following program, which should clearly be verifiable
; 1: mov64 r0, 10
; 2: mov64 r1, 2
; 3: div64 r0, r1
; 4: exit

; "Registers" used by the program
(declare-fun r0_1 () (_ BitVec 64)) ; We assign to register r0 in line 1
(declare-fun r1_2 () (_ BitVec 64)) ; we assign to register r1 in line 2
(declare-fun r0_3 () (_ BitVec 64)) ; We update r0 in line 3 (by division)

; To check for validity, we need the negation of our assert(s), i.e:
; VC = \A r0 ... \A r10 => VC_0
; The program is valid if `not VC` is unsatisfiable

(assert
    (not
        (=>
            (= r0_1 #x000000000000000a)
	    (=>
                (= r1_2 #x0000000000000002)
                ; Assigning a non-zero value to r1 should imply it is not zero
		; at least when we want to use it for division
	        (not (= r1_2 (_ bv0 64)))
            )
        )
    )
)


(check-sat)
(exit)
