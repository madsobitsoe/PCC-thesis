(set-logic BV)
(set-option :produce-proofs true)
(set-option :proof-format-mode lfsc)
(set-option :dump-proofs true)

(assert (not
(forall ((v6 (_ BitVec 64)))
    (=>
        (= v6 #x0000000000000008)
        (forall ((v5 (_ BitVec 64)))
            (=>
                (= v5 #x0000000000000002)
                (forall ((v4 (_ BitVec 64)))
                    (and
                        (not (= v5 #x0000000000000000))
                        (=>
                            (= v4 (bvudiv v6 v5))
                            (forall ((v3 (_ BitVec 64)))
                                (=>
                                    (= v3 #x0000000000000004)
                                    (forall ((v2 (_ BitVec 64)))
                                        (and
                                            (not (= v3 #x0000000000000000))
                                            (=>
                                                (= v2 (bvudiv v4 v3))
                                                (forall ((v1 (_ BitVec 64)))
                                                    (=>
                                                        (= v1 #x000000000000002a)
                                                        (forall ((v0 (_ BitVec 64)))
                                                            (and
                                                                (not (= v2 #x0000000000000000))
                                                                (=>
                                                                    (= v0 (bvudiv v1 v2))
                                                                    true
                                                                )
                                                            ))
                                                    ))
                                            )
                                        ))
                                ))
                        )
                    ))
            ))
    ))))
(check-sat)
(exit)