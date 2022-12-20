freshVar :: Predicate -> VName
varsInPrimitive :: Primitive -> [VName]
varsInExpression :: Expression -> [VName]
varsInExpressionPredicate :: ExpressionPredicate -> [VName]
varsInPredicate :: Predicate -> [VName]
substitute_in_primitive :: Primitive -> Primitive -> Primitive -> Primitive
substitute_in_expression :: Primitive -> Primitive -> Expression -> Expression
substitute_in_expression_predicate :: Primitive -> Primitive ->  ExpressionPredicate -> ExpressionPredicate
substitute_in_predicate :: Primitive -> Primitive -> Predicate -> Predicate
withInitialPre :: A.Program -> Either String Predicate
wp_inst :: FWProgram -> Index -> Predicate -> Predicate
wp_prog :: A.Program -> Either String Predicate
