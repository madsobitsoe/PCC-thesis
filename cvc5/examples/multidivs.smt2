(set-logic BV)
(set-option :produce-proofs true)
(set-option :proof-format-mode lfsc)
(set-option :dump-proofs true)
(set-option :produce-assignments true)

; We work with the following program, which should clearly be verifiable
; 1: mov64 r0, 32
; 2: mov64 r1, 4
; 3: mov64 r2, 2
; 4: div64 r1, r2
; 5: div64 r0, r1
; 6: exit

; VC:
; \A r0 \A r1 \A r2 .
;         r0 = 32 =>
;         r1 = 4 =>
;         r2 = 2 =>
;         r2 != 0 =>
;         r1 = r1 div r2 =>
;         r1 != 0 =>
;         r0 = r0 div r1 =>
;         true

; Hacky way of doing "implies true" at the end of implications
(declare-const post (_ BitVec 1))

; "Registers" used by the program
(declare-fun r0_1 () (_ BitVec 64)) ; We assign to register r0 in line 1
(declare-fun r1_2 () (_ BitVec 64)) ; We assign to register r1 in line 2
(declare-fun r2_3 () (_ BitVec 64)) ; We assign to register r2 in line 3
(declare-fun r1_4 () (_ BitVec 64)) ; we update register r1 in line 4
(declare-fun r0_5 () (_ BitVec 64)) ; we update register r0 in line 5


; To check for validity, we need the negation of our assert(s), i.e:
; VC = \A r0 ... \A r10 => VC_0
; The program is valid if `not VC` is unsatisfiable

(assert
    (not
        (=>
            (= r0_1 #x0000000000000020)
            (=>
                (= r1_2 #x0000000000000004)
                (=>
                    (= r2_3 #x0000000000000002)
                    (=>
                        (not (= r1_2 (_ bv0 64)))
                        (=>
                            (= r1_4 (bvudiv r1_2 r2_3))
                            (=>
                                (not (= r1_4 (_ bv0 64)))
                                (=>
                                    (= r0_5 (bvudiv r0_1 r1_4))
                                    (= post post)
                                )
                            )
			)
                    )
                )
            )
        )
    )
)


(check-sat)
(exit)
