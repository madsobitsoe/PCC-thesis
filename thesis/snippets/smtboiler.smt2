(set-logic BV)
(set-option :produce-proofs true)
(set-option :proof-format-mode lfsc)
(set-option :dump-proofs true)

(assert (not
  (forall ((n (_ BitVec 64)))
    (=> 
      (bvuge n #x0000000000000008)
      ... rest of verification condition goes here
    )
  )
))
(check-sat)
(exit)
