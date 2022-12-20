data Predicate =
    PEP ExpressionPredicate
  | PNot Predicate
  | PAll VName Predicate
  | PAnd Predicate Predicate
  | PImplies Predicate Predicate
  | PITE ExpressionPredicate Predicate Predicate
  deriving (Eq, Show)
