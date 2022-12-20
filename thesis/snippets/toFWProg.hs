toFWProg :: A.Program -> FWProgram
toFWProg = V.fromList . map toFWProg'
toFWProg' :: A.Instruction -> Instr
toFWProg' A.Exit = Exit
toFWProg' (A.Binary A.B64 A.Mov rd (Left rs)) =
  let (PVar v) = reg2var rd  
      rs' = EPrim $ reg2var rs
  in Assign v rs'
...  -- similar ALU translations left out 
toFWProg' (A.JCond A.Jeq rd (Right imm) off) =
  let v = reg2var rd
      imm' = EPrim $ PImm $ fromIntegral imm
      ep = EPEq v imm'
  in Cond ep (fromIntegral off)
... -- similar conditionals left out
toFWProg' (A.Load A.B64 rd rs off) =
  let (PVar v) = reg2var rd
      (PVar src) = reg2var rs
      rs' =
        case src of
          "m" -> Mem src (Just (PVar "n"))
          "fp" -> Mem src (Just (PImm $ fromIntegral 512))
          _ -> Mem src Nothing
      off' =
        case off of
          Nothing -> PImm $ fromIntegral 0
          Just x ->  PImm $ fromIntegral x
  in Assign v (ELoad rs' off')
... -- store instructions left out for brevity, but are similar to load
