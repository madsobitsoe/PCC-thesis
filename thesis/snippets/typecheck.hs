getType :: Primitive -> FWTypeEnv -> Either String FWType
getExpType :: Expression -> FWTypeEnv -> Either String FWType
getExpPredType :: ExpressionPredicate -> FWTypeEnv -> Either String FWType
typeCheck :: FWProgram -> Either String FWProgram
typeCheck' :: FWProgram -> FWTypeEnv -> Index -> Either String FWProgram
