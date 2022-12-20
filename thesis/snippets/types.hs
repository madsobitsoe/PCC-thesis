data FWType = TInt64 | TMem Primitive | TUnknown
  deriving (Eq, Show)
type FWTypeEnv = M.Map VName FWType

initialTypeEnvironment :: FWTypeEnv
initialTypeEnvironment =
  M.fromList [("r0",  TUnknown)
             ,("m",  TMem (PVar "n"))
             ,("n",  TInt64)
             ,("r3",  TUnknown)
             ,("r4",  TUnknown)
             ,("r5",  TUnknown)
             ,("r6",  TUnknown)
             ,("r7",  TUnknown)
             ,("r8",  TUnknown)
             ,("r9",  TUnknown)
             ,("r10", TMem (PImm $ fromIntegral 512))
             ]
