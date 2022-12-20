-- Generate VC for a program and prepend the initial guarantees `Pre`
withInitialPre :: A.Program -> Either String Predicate
withInitialPre prog =
  case wp_prog prog of
    Left err -> Left err
    Right pred ->
      Right $ PAll "n" (PImplies (PEP (EPGTE (PVar "n") (EPrim (PImm 8)))) (substitute_in_predicate (PVar "r2") (PVar "n") pred ))

wp_prog :: A.Program -> Either String Predicate
wp_prog prog =
  case typeCheck (toFWProg prog) of
    Left err -> Left err
    Right prog' -> Right $ wp_inst prog' 0 (PEP EPTrue)
