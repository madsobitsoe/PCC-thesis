-- Given a predicate, return a fresh variable not used in the predicate
freshVar :: Predicate -> VName
freshVar predicate =
  let used = varsInPredicate predicate
      allFresh = map (\n -> "v" ++ show n) [0..]
      unused = filter (\v -> not $ elem v used) allFresh
  in
    head unused
