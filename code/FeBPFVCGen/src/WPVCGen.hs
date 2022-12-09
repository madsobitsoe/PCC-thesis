module WPVCGen
(pp, pp_smt, with_smt_lib_wrapping, typeCheck, toFWProg, wp_prog, wp_inst, withInitialPre)
where

import qualified Ebpf.Asm as A
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Util
import Definitions

-- Pretty printing of primitives
ppPrim :: Primitive -> String
ppPrim (PVar s) = s
ppPrim (PImm imm) = show imm

-- Pretty printing of expressions
ppE :: Expression -> String
ppE (EPrim prim) = ppPrim prim
ppE (EAdd p1 p2) = ppPrim p1 ++ " + " ++ ppPrim p2
ppE (EMul p1 p2) = ppPrim p1 ++ " * " ++ ppPrim p2
ppE (EDiv p1 p2) = ppPrim p1 ++ " / " ++ ppPrim p2
ppE (EXor p1 p2) = ppPrim p1 ++ " ^ " ++ ppPrim p2

-- Pretty printing of expression predicates
ppEP :: ExpressionPredicate -> String
ppEP EPTrue = "true"
ppEP (EPEq  p1 e2) = ppPrim p1 ++ " = "  ++ ppE e2
ppEP (EPNeq p1 e2) = ppPrim p1 ++ " != " ++ ppE e2
ppEP (EPGTE p1 e2) = ppPrim p1 ++ " >= " ++ ppE e2
-- Pretty printing of predicates
pp :: Predicate -> String
pp (PEP ep) = ppEP ep
pp (PNot p) = [toEnum 172] ++ pp p
pp (PAll v p) = [toEnum 8704] ++ v ++ ". " ++ pp p
pp (PAnd p1 p2) = "(" ++ pp p1 ++ ") " ++ [toEnum 8743] ++ " (" ++ pp p2 ++ ")"
pp (PImplies p1 p2) = pp p1 ++ " " ++ [toEnum 8658] ++ " (" ++ pp p2 ++ ")"
pp (PITE ep p1 p2) = "if " ++ ppEP ep ++ " then " ++ pp p1 ++ " else " ++ pp p2

-- indent when pretty printing
ppIndent :: Int -> String
ppIndent d = replicate (d*2) ' '

-- wrap smtlib-pretty printed predicate in the "boiler plate" needed to have a valid smtlib2 program for cvc5
with_smt_lib_wrapping :: String -> String
with_smt_lib_wrapping s =
  "(set-logic BV)\n(set-option :produce-proofs true)\n(set-option :proof-format-mode lfsc)\n(set-option :dump-proofs true)\n\n" ++
  "(assert (not\n" ++
  s ++
  "\n))\n(check-sat)\n(exit)\n"

-- Pretty print primitive in smtlib2 format
ppPrim_smt :: Primitive -> String
ppPrim_smt (PImm imm) = toHex imm
ppPrim_smt (PVar s) = s
-- Pretty print expression to smtlib2 format
ppE_smt :: Expression -> String
ppE_smt (EPrim prim) = ppPrim_smt prim
-- ppE_smt (EVar s) = s
-- ppE_smt (EImm imm) = toHex imm
ppE_smt (EAdd p1 p2) = "(bvadd "  ++ ppPrim_smt p1 ++ " " ++ ppPrim_smt p2 ++ ")"
ppE_smt (EMul p1 p2) = "(bvmul "  ++ ppPrim_smt p1 ++ " " ++ ppPrim_smt p2 ++ ")"
ppE_smt (EDiv p1 p2) = "(bvudiv " ++ ppPrim_smt p1 ++ " " ++ ppPrim_smt p2 ++ ")"
ppE_smt (EXor p1 p2) = "(bvxor "  ++ ppPrim_smt p1 ++ " " ++ ppPrim_smt p2 ++ ")"

-- Pretty print expression predicate to smtlib2 format
ppEP_smt :: ExpressionPredicate -> String
ppEP_smt EPTrue = "true"
ppEP_smt (EPEq  p1 e2) = "(= "     ++ ppPrim_smt p1 ++ " " ++ ppE_smt e2 ++ ")"
ppEP_smt (EPNeq p1 e2) = "(not "   ++ ppEP_smt (EPEq p1 e2) ++ ")"
ppEP_smt (EPGTE p1 e2) = "(bvuge " ++ ppPrim_smt p1 ++ " " ++ ppE_smt e2 ++ ")"
-- Pretty print predicate to smtlib2 format
ppP_smt :: Int -> Predicate -> String
ppP_smt d (PEP ep) = ppIndent d ++ ppEP_smt ep
ppP_smt d (PNot p) = ppIndent d ++ "(not " ++ ppP_smt d p ++ ")"
ppP_smt d (PAnd p1 p2) = "\n" ++ ppIndent d ++ "(and \n" ++ ppP_smt (d+1) p1 ++ ppP_smt (d+1) p2 ++ "\n" ++ ppIndent d ++ ")"
ppP_smt d (PImplies p1 p2) = "\n" ++ ppIndent d ++ "(=> \n" ++ ppP_smt (d+1) p1 ++ "\n" ++ ppP_smt (d+1) p2 ++ "\n" ++ ppIndent d ++ ")"
ppP_smt d (PAll v p) = ppIndent d ++ "(forall ((" ++ v ++ " (_ BitVec 64))) " ++ ppP_smt (d+1) p ++ "\n" ++ ppIndent d ++ ")"
ppP_smt d (PITE ep p1 p2) = ppIndent d ++ "(ite " ++ ppEP_smt ep ++ "\n" ++ ppP_smt (d+1) p1 ++ "\n" ++ ppP_smt (d+1) p2 ++ ")"

-- Pretty print predicate to smtlib2 format
pp_smt :: Predicate -> String
pp_smt predicate = ppP_smt 1 predicate

-- Given a predicate, return a fresh variable not used in the predicate
freshVar :: Predicate -> VName
freshVar predicate =
  let used = varsInPredicate predicate
      allFresh = map (\n -> "v" ++ show n) [0..]
      unused = filter (\v -> not $ elem v used) allFresh
  in
    head unused


varsInPrimitive :: Primitive -> [VName]
varsInPrimitive (PVar v) = [v]
varsInPrimitive _  = []
-- Extract used variables from expression
varsInExpression :: Expression -> [VName]
varsInExpression (EPrim prim) = varsInPrimitive prim
-- varsInExpression (EVar v) = [EVar v]
varsInExpression (EAdd p1 p2) = varsInPrimitive p1 ++ varsInPrimitive p2
varsInExpression (EMul p1 p2) = varsInPrimitive p1 ++ varsInPrimitive p2
varsInExpression (EDiv p1 p2) = varsInPrimitive p1 ++ varsInPrimitive p2
varsInExpression (EXor p1 p2) = varsInPrimitive p1 ++ varsInPrimitive p2

-- Extract used variables from expression predicate
varsInExpressionPredicate :: ExpressionPredicate -> [VName]
varsInExpressionPredicate EPTrue = []
varsInExpressionPredicate (EPEq  p1 e2) = varsInPrimitive p1 ++ varsInExpression e2
varsInExpressionPredicate (EPNeq p1 e2) = varsInPrimitive p1 ++ varsInExpression e2
varsInExpressionPredicate (EPGTE p1 e2) = varsInPrimitive p1 ++ varsInExpression e2
-- Extract used variables from predicate
varsInPredicate :: Predicate -> [VName]
varsInPredicate (PNot predicate) = varsInPredicate predicate
varsInPredicate (PAll v p) = v : varsInPredicate p
varsInPredicate (PAnd p1 p2) = varsInPredicate p1 ++ varsInPredicate p2
varsInPredicate (PImplies p1 p2) = varsInPredicate p1 ++ varsInPredicate p2
varsInPredicate (PEP ep) = varsInExpressionPredicate ep
varsInPredicate (PITE ep p1 p2) = varsInExpressionPredicate ep ++ varsInPredicate p1 ++ varsInPredicate p2


substitute_in_primitive :: Primitive -> Primitive -> Primitive -> Primitive
substitute_in_primitive _ _ (PImm imm) = (PImm imm)
substitute_in_primitive old new (PVar vname) =
  if old == (PVar vname) then new else (PVar vname)
  
substitute_in_expression :: Primitive -> Primitive -> Expression -> Expression
substitute_in_expression old new (EPrim (PVar prim)) = EPrim $ substitute_in_primitive old new (PVar prim)
substitute_in_expression old new (EAdd p1 p2) = EAdd (substitute_in_primitive old new p1) (substitute_in_primitive old new p2)
substitute_in_expression old new (EMul p1 p2) = EMul (substitute_in_primitive old new p1) (substitute_in_primitive old new p2)
substitute_in_expression old new (EDiv p1 p2) = EDiv (substitute_in_primitive old new p1) (substitute_in_primitive old new p2)
substitute_in_expression old new (EXor p1 p2) = EXor (substitute_in_primitive old new p1) (substitute_in_primitive old new p2)
substitute_in_expression _ _ e = e

substitute_in_expression_predicate :: Primitive -> Primitive ->  ExpressionPredicate -> ExpressionPredicate
substitute_in_expression_predicate _ _ EPTrue = EPTrue
substitute_in_expression_predicate old new (EPEq  p1 e2) = EPEq  (substitute_in_primitive old new p1) (substitute_in_expression old new e2)
substitute_in_expression_predicate old new (EPNeq p1 e2) = EPNeq (substitute_in_primitive old new p1) (substitute_in_expression old new e2)
substitute_in_expression_predicate old new (EPGTE p1 e2) = EPGTE (substitute_in_primitive old new p1) (substitute_in_expression old new e2)

substitute_in_predicate :: Primitive -> Primitive -> Predicate -> Predicate
substitute_in_predicate _ _ (PEP EPTrue) = PEP EPTrue
substitute_in_predicate old new (PAnd p1 p2) = PAnd (substitute_in_predicate old new p1) (substitute_in_predicate old new p2)
substitute_in_predicate old new (PImplies p1 p2) =
  PImplies (substitute_in_predicate old new p1) (substitute_in_predicate old new p2)
substitute_in_predicate old new (PNot p) = PNot (substitute_in_predicate old new p)
substitute_in_predicate old new (PEP ep) = PEP $ substitute_in_expression_predicate old new ep
substitute_in_predicate old new (PAll v p) =
  let p' = substitute_in_predicate old new p
  in PAll v p'

substitute_in_predicate old new (PITE ep p1 p2) =
  let ep' = substitute_in_expression_predicate old new ep
      p1' = substitute_in_predicate old new p1
      p2' = substitute_in_predicate old new p2
  in (PITE ep' p1' p2')

-- Generate VC for a program and prepend the initial guarantees `Pre`
withInitialPre :: A.Program -> Predicate
withInitialPre prog = PAll "n" (PImplies (PEP (EPGTE (PVar "n") (EPrim (PImm 1)))) (substitute_in_predicate (PVar "r2") (PVar "n") $ wp_prog prog))


wp_inst :: FWProgram -> Index -> Predicate -> Predicate
wp_inst prog idx q =
  case prog V.! idx of
    Exit -> PEP EPTrue
    Assign x (EDiv p1 p2) ->
      let q' = wp_inst prog (idx+1) q
          v = freshVar q'
          q'sub = substitute_in_predicate (PVar x) (PVar v) q'
          ep = (PEP (EPEq (PVar v) (EDiv p1 p2)))
          notZeroAssert = PEP (EPNeq p2 (EPrim (PImm 0)))
      in PAll v (PAnd notZeroAssert (PImplies ep q'sub))
          
    Assign x e ->
      let  q' = wp_inst prog (idx+1) q
           v = freshVar q'
           q'sub = substitute_in_predicate (PVar x) (PVar v) q'
           ep = PEP (EPEq (PVar v) e)
      in PAll v (PImplies ep q'sub)
    Cond ep offset ->
      PITE ep (wp_inst prog (idx + 1 + offset) q) (wp_inst prog (idx+1) q)


wp_prog :: A.Program -> Predicate
wp_prog prog = wp_inst (toFWProg prog) 0 (PEP EPTrue)

toFWProg :: A.Program -> FWProgram
toFWProg = V.fromList . map toFWProg'

toFWProg' :: A.Instruction -> Instr
toFWProg' A.Exit = Exit
toFWProg' (A.Binary A.B64 A.Mov rd (Left rs)) =
  let (PVar v) = reg2var rd  
      rs' = EPrim $ reg2var rs
  in Assign v rs'

toFWProg' (A.Binary A.B64 A.Mov rd (Right imm)) =
  let (PVar v) = reg2var rd
      imm' = EPrim $ PImm (fromIntegral imm)
  in Assign v imm'

-- Arithmetic
toFWProg' (A.Binary A.B64 A.Add rd (Right imm)) =
  let (PVar v) = reg2var rd 
      imm' = PImm (fromIntegral imm)
  in Assign v (EAdd (PVar v) imm')

toFWProg' (A.Binary A.B64 A.Add rd (Left rs)) =
  let (PVar v) = reg2var rd 
      rs' = reg2var rs
  in Assign v (EAdd (PVar v) rs')

toFWProg' (A.Binary A.B64 A.Mul rd (Right imm)) =
  let (PVar v) = reg2var rd 
      imm' = PImm (fromIntegral imm)
  in Assign v (EMul (PVar v) imm')

toFWProg' (A.Binary A.B64 A.Mul rd (Left rs)) =
  let (PVar v) = reg2var rd 
      rs' = reg2var rs
  in Assign v (EMul (PVar v) rs')

toFWProg' (A.Binary A.B64 A.Xor rd (Right imm)) =
  let (PVar v) = reg2var rd 
      imm' = PImm $ fromIntegral imm
  in Assign v (EXor (PVar v) imm')

toFWProg' (A.Binary A.B64 A.Xor rd (Left rs)) =
  let (PVar v) = reg2var rd 
      rs' = reg2var rs
  in Assign v (EXor (PVar v) rs')


toFWProg' (A.Binary A.B64 A.Div rd (Right imm)) =
  let (PVar v) = reg2var rd 
      imm' = PImm (fromIntegral imm)
  in Assign v (EDiv (PVar v) imm')

toFWProg' (A.Binary A.B64 A.Div rd (Left rs)) =
  let (PVar v) = reg2var rd 
      rs' = reg2var rs
  in Assign v (EDiv (PVar v) rs')

-- Conditionals
toFWProg' (A.JCond A.Jeq rd (Right imm) off) =
  let v = reg2var rd
      imm' = EPrim $ PImm $ fromIntegral imm
      ep = EPEq v imm'
  in Cond ep (fromIntegral off)

toFWProg' (A.JCond A.Jeq rd (Left rs) off) =
  let v = reg2var rd
      rs' = EPrim $ reg2var rs
      ep = EPEq v rs'
      off' = fromIntegral off
  in Cond ep off'

toFWProg' (A.JCond A.Jgt rd (Right imm) off) =
  let v = reg2var rd
      imm' = EPrim $ PImm $ fromIntegral imm
      ep = EPGTE v imm'
  in Cond ep (fromIntegral off)

toFWProg' (A.JCond A.Jgt rd (Left rs) off) =
  let v = reg2var rd
      rs' = EPrim $ reg2var rs
      ep = EPGTE v rs'
      off' = fromIntegral off
  in Cond ep off'
  
toFWProg' (A.JCond A.Jne rd (Right imm) off) =
  let v = reg2var rd
      imm' = EPrim $ PImm $ fromIntegral imm
      ep = EPNeq v imm'
  in Cond ep (fromIntegral off)

toFWProg' (A.JCond A.Jne rd (Left rs) off) =
  let v = reg2var rd
      rs' = EPrim $ reg2var rs
      ep = EPNeq v rs'
      off' = fromIntegral off
  in Cond ep off'

toFWProg' (A.Load A.B64 rd rs off) =
  -- TODO: Can this be done in a cleaner/better/prettier way than hardcoding?
  let (PVar v) = reg2var rd
      (PVar src) = reg2var rs
      rs' =
        case src of
          "m" -> Mem src (PVar "r2")
          "fp" -> Mem src (PImm $ fromIntegral 512)
          -- For now, make it a mem of size 0
          _ -> Mem src (PImm $ fromIntegral 0)
      off' =
        case off of
          Nothing -> PImm $ fromIntegral 0
          Just x ->  PImm $ fromIntegral x
  in Assign v (ELoad rs' off')

toFWProg' _ = undefined


getType :: Primitive -> FWTypeEnv -> Either String FWType
getType prim env =
  case prim of
    PImm _ -> Right TInt64
    PVar x -> case M.lookup x env of
      Nothing -> Left $ show prim ++ " was not in type environment"
      Just t -> Right t


getExpType :: Expression -> FWTypeEnv -> Either String FWType
getExpType e env =
  case e of
    EPrim prim -> getType prim env
    -- ELoad (Mem _ (PImm 0)) _ -> Left "Memory cannot be size 0"
    ELoad (Mem _ _) p ->
      case getType p env of
        Left err -> Left err
        Right t ->
          case t of
            TInt64 -> Right t
            _ -> Left $ show t ++ " not allowed in load operation"
    EAdd p1 p2 -> doIt p1 p2
    EMul p1 p2 -> doIt p1 p2
    EDiv p1 p2 -> doIt p1 p2
    EXor p1 p2 -> doIt p1 p2
  where
    doIt p1 p2 =
      case getType p1 env of
        Left err -> Left err
        Right t1 ->
          case getType p2 env of
            Left err -> Left err
            Right t2 ->
              if t1 == TInt64 && t1 == t2 then Right t1
              else
                Left $ "types " ++ show t1 ++ " and " ++ show t2 ++ " do not match or are not allowed in arithmetic"

getExpPredType :: ExpressionPredicate -> FWTypeEnv -> Either String FWType
getExpPredType ep env =
  case ep of
    EPTrue -> Left "No boolean type in FWeBPF, impossible to check EPTrue"
    EPEq p e -> doIt p e
    EPNeq p e -> doIt p e
    EPGTE p e -> doIt p e
  where
    doIt p e =
      case getType p env of
        Left err -> Left err
        Right t1 ->
          case getExpType e env of
            Left err -> Left err
            Right t2 ->
              if t1 == TInt64 && t1 == t2 then Right t1
              else Left $ "types " ++ show t1 ++ " and " ++ show t2 ++ " do not match or are not allowed in comparisons"

typeCheck :: FWProgram -> Either String FWProgram
typeCheck prog = typeCheck' prog initialTypeEnvironment 0

typeCheck' :: FWProgram -> FWTypeEnv -> Index -> Either String FWProgram
typeCheck' prog env idx =
  case prog V.! idx of
    Exit -> Right prog
    Assign x e ->    
      case getExpType e env of
        Left err -> Left err
        Right t ->
          let env' = M.insert x t env
          in typeCheck' prog env' (idx+1)
    Cond ep offset ->
      case getExpPredType ep env of
        Left err -> Left err
        Right _ ->
            case typeCheck' prog env (idx+1+offset) of
              Left err -> Left err
              Right _ -> typeCheck' prog env (idx+1)

