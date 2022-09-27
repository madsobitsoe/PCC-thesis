(set-logic BV)
(set-option :produce-proofs true)
(set-option :proof-format-mode lfsc)
(set-option :dump-proofs true)
(set-option :produce-assignments true)

; We work with the following program, which should clearly be verifiable
; 1: mov64 r1, 0
; 2: add64 r1, 1
; 3: add64 r1, 3
; 4: add64 r1, 2
; 5: mov64 r0, 12
; 6: div64 r0, r1
; 7: exit


(declare-const post (_ BitVec 1))

; "Registers" used by the program
(declare-fun r1_1 () (_ BitVec 64)) ; We assign to register r1 in line 1
(declare-fun r1_2 () (_ BitVec 64)) ; we update register r1 in line 2
(declare-fun r1_3 () (_ BitVec 64)) ; we update register r1 in line 3
(declare-fun r1_4 () (_ BitVec 64)) ; we update register r1 in line 4
(declare-fun r0_5 () (_ BitVec 64)) ; We assign to register r0 in line 5
(declare-fun r0_6 () (_ BitVec 64)) ; We update register r0 in line 6 (by division)

; To check for validity, we need the negation of our assert(s), i.e:
; VC = \A r0 ... \A r10 => VC_0
; The program is valid if `not VC` is unsatisfiable

(assert
    (not
        (=>
            (= r1_1 #x0000000000000000)
            (=>
                (= r1_2 (bvadd r1_1 #x0000000000000001))
                (=>
                    (= r1_3 (bvadd r1_2 #x0000000000000003))
                    (=>
                        (= r1_4 (bvadd r1_3 #x0000000000000002))
                        (=>
                            (= r0_5 #x000000000000000c)
			    (=>
                            (and
                               (not (= r1_4 (_ bv0 64)))
                               (= #x0000000000000002 (bvudiv r0_5 r1_4))
                            )
			    (= post post)
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
