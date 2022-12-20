wp_inst :: FWProgram -> Index -> Predicate -> Predicate
wp_inst prog idx q =
  case prog V.! idx of
... -- exit and mov reg, reg left out
  Assign x (EDiv p1 p2) ->
      let q' = wp_inst prog (idx+1) q
          v = freshVar q'
          q'sub = substitute_in_predicate (PVar x) (PVar v) q'
          ep = (PEP (EPEq (PVar v) (EDiv p1 p2)))
          notZeroAssert = PEP (EPNeq p2 (EPrim (PImm 0)))
      in PAll v (PAnd notZeroAssert (PImplies ep q'sub))
... -- EMod left out, similar to EDiv
    Assign x (ELoad (Mem _ (Just sz)) p) ->
      let q' = wp_inst prog (idx+1) q
          v = freshVar q'
          v' = freshVar (PAll v q')
          q'sub = substitute_in_predicate (PVar x) (PVar v) q'
          inboundsAssert = PAnd (PEP (EPGTE p (EPrim (PImm 0)))) (PEP (EPLTE p (ESub sz (PImm 8))))
          alignedAssert = PEP (EPEq (PImm 0) (EMod p (PImm 8)))
          newAss = PEP (EPEq (PVar v) (EPrim (PVar v')))
      in PAll v (PAll v' (PAnd (PAnd inboundsAssert alignedAssert) (PImplies newAss q'sub)))
    Assign x e ->
      let  q' = wp_inst prog (idx+1) q
           v = freshVar q'
           q'sub = substitute_in_predicate (PVar x) (PVar v) q'
           ep = PEP (EPEq (PVar v) e)
      in PAll v (PImplies ep q'sub)
    Cond ep offset ->
      PITE ep (wp_inst prog (idx + 1 + offset) q) (wp_inst prog (idx+1) q)
... -- Store left out, almost similar to ELoad
