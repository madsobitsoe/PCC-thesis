comparisonSetup :: CInt -> CInt -> A.Program -> A.Program
comparisonSetup sizemap ctxmap prog =
  [
    A.LoadMapFd (A.Reg 1) (fromIntegral sizemap)
  , A.Store A.B32 (A.Reg 10) (Just (-4)) (Right 0)
    -- Store the key on the stack
  , A.Store A.B32 (A.Reg 10) (Just (-4)) (Right 0)
    -- Load pointer to key into R2
  , A.Binary A.B64 A.Mov (A.Reg 2) (Left (A.Reg 10))
  , A.Binary A.B64 A.Add (A.Reg 2) (Right (-4))
  -- Call Lookup element for first value, getting a pointer in R0 in return
  , A.Call 1
  -- Check if pointer is NULL
  , A.JCond A.Jne (A.Reg 0) (Right 0) 2
  -- Exit with 1 if pointer was NULL
  , A.Binary A.B64 A.Mov (A.Reg 0) (Right 1)
  , A.Exit
  -- Else, load the value into R9 temporarily
  , A.Load A.B64 (A.Reg 9) (A.Reg 0) (Nothing)
  -- Now setup the ctxmap st. r1 is a pointer to the beginning of memory
  , A.LoadMapFd (A.Reg 1) (fromIntegral ctxmap)
  , A.Store A.B32 (A.Reg 10) (Just (-4)) (Right 0)
    -- Store the key on the stack
  , A.Store A.B32 (A.Reg 10) (Just (-4)) (Right 0)
    -- Load pointer to key into R2
  , A.Binary A.B64 A.Mov (A.Reg 2) (Left (A.Reg 10))
  , A.Binary A.B64 A.Add (A.Reg 2) (Right (-4))
  -- Call Lookup element for first value, getting a pointer in R0 in return
  , A.Call 1
  -- Check if pointer is NULL
  , A.JCond A.Jne (A.Reg 0) (Right 0) 2
  -- Exit with 1 if pointer was NULL
  , A.Binary A.B64 A.Mov (A.Reg 0) (Right 1)
  , A.Exit
  , A.Binary A.B64 A.Mov (A.Reg 1) (Left (A.Reg 0))
  , A.Binary A.B64 A.Mov (A.Reg 2) (Left (A.Reg 9))
  ] ++ prog
