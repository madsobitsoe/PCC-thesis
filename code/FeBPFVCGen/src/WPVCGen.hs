module WPVCGen

where

import qualified Ebpf.Asm as A
import Data.Word
import qualified Data.Vector as V
import Util
import Definitions

-- Pretty printing of expressions
ppE :: Expression -> String
ppE (EVar s) = s
ppE (EImm imm) = show imm
ppE (EAdd e1 e2) = ppE e1 ++ " + " ++ ppE e2
ppE (EMul e1 e2) = ppE e1 ++ " * " ++ ppE e2
ppE (EDiv rd src) = ppE rd ++ " / " ++ ppE src
ppE (EXor e1 e2) = ppE e1 ++ " ^ " ++ ppE e2

-- Pretty printing of expression predicates
ppEP :: ExpressionPredicate -> String
ppEP EPTrue = "true"
ppEP (EPEq e1 e2) = ppE e1 ++ " = " ++ ppE e2
ppEP (EPNeq e1 e2) = ppE e1 ++ " != " ++ ppE e2
ppEP (EPGTE e1 e2) = ppE e1 ++ " >= " ++ ppE e2
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


-- Pretty print expression to smtlib2 format
ppE_smt :: Expression -> String
ppE_smt (EVar s) = s
ppE_smt (EImm imm) = toHex imm
ppE_smt (EAdd e1 e2) = "(bvadd " ++ ppE_smt e1 ++ " " ++ ppE_smt e2 ++ ")"
ppE_smt (EMul e1 e2) = "(bvmul " ++ ppE_smt e1 ++ " " ++ ppE_smt e2 ++ ")"
ppE_smt (EDiv rd src) = "(bvudiv " ++ ppE_smt rd ++ " " ++ ppE_smt src ++ ")"
ppE_smt (EXor e1 e2) = "(bvxor " ++ ppE_smt e1 ++ " " ++ ppE_smt e2 ++ ")"

-- Pretty print expression predicate to smtlib2 format
ppEP_smt :: ExpressionPredicate -> String
ppEP_smt EPTrue = "true"
ppEP_smt (EPEq e1 e2) = "(= " ++ ppE_smt e1 ++ " " ++ ppE_smt e2 ++ ")"
ppEP_smt (EPNeq e1 e2) = "(not " ++ ppEP_smt (EPEq e1 e2) ++ ")"
ppEP_smt (EPGTE e1 e2) = "(bvuge " ++ ppE_smt e1 ++ " " ++ ppE_smt e2 ++ ")"
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

-- Extract used variables from expression
varsInExpression :: Expression -> [Expression]
varsInExpression (EVar v) = [EVar v]
varsInExpression (EAdd e1 e2) = varsInExpression e1 ++ varsInExpression e2
varsInExpression (EMul e1 e2) = varsInExpression e1 ++ varsInExpression e2
varsInExpression (EDiv e1 e2) = varsInExpression e1 ++ varsInExpression e2
varsInExpression (EXor e1 e2) = varsInExpression e1 ++ varsInExpression e2

varsInExpression _ = []
-- Extract used variables from expression predicate
varsInExpressionPredicate :: ExpressionPredicate -> [Expression]
varsInExpressionPredicate EPTrue = []
varsInExpressionPredicate (EPEq e1 e2) = varsInExpression e1 ++ varsInExpression e2
varsInExpressionPredicate (EPNeq e1 e2) = varsInExpression e1 ++ varsInExpression e2
varsInExpressionPredicate (EPGTE e1 e2) = varsInExpression e1 ++ varsInExpression e2
-- Extract used variables from predicate
varsInPredicate :: Predicate -> [Expression]
varsInPredicate (PNot predicate) = varsInPredicate predicate
varsInPredicate (PAll v p) = varsInExpression (EVar v)  ++ varsInPredicate p
varsInPredicate (PAnd p1 p2) = varsInPredicate p1 ++ varsInPredicate p2
varsInPredicate (PImplies p1 p2) = varsInPredicate p1 ++ varsInPredicate p2
varsInPredicate (PEP ep) = varsInExpressionPredicate ep
varsInPredicate (PITE ep p1 p2) = varsInExpressionPredicate ep ++ varsInPredicate p1 ++ varsInPredicate p2

-- Given a predicate, return a fresh variable not used in the predicate
freshVar :: Predicate -> Expression
freshVar predicate =
  let used = varsInPredicate predicate
      allFresh = map (\n -> EVar $ "v" ++ show n) [0..]
      unused = filter (\v -> not $ elem v used) allFresh
  in
    head unused

substitute_in_expression :: Expression -> Expression -> Expression -> Expression
substitute_in_expression old new (EAdd rd src) = EAdd (substitute_in_expression old new rd) (substitute_in_expression old new src)
substitute_in_expression old new (EMul rd src) = EMul (substitute_in_expression old new rd) (substitute_in_expression old new src)
substitute_in_expression old new (EDiv rd src) = EDiv (substitute_in_expression old new rd) (substitute_in_expression old new src)
substitute_in_expression old new (EXor rd src) = EXor (substitute_in_expression old new rd) (substitute_in_expression old new src)
substitute_in_expression old new e =
  if old == e then new
  else e

substitute_in_expression_predicate :: Expression -> Expression ->  ExpressionPredicate -> ExpressionPredicate
substitute_in_expression_predicate _ _ EPTrue = EPTrue
substitute_in_expression_predicate old new (EPEq e1 e2) = EPEq (substitute_in_expression old new e1) (substitute_in_expression old new e2)
substitute_in_expression_predicate old new (EPNeq e1 e2) = EPNeq (substitute_in_expression old new e1) (substitute_in_expression old new e2)
substitute_in_expression_predicate old new (EPGTE e1 e2) = EPGTE (substitute_in_expression old new e1) (substitute_in_expression old new e2)



substitute_in_predicate :: Expression -> Expression -> Predicate -> Predicate
substitute_in_predicate _ _ (PEP EPTrue) = PEP EPTrue
substitute_in_predicate old new (PAnd p1 p2) = PAnd (substitute_in_predicate old new p1) (substitute_in_predicate old new p2)
substitute_in_predicate old new (PImplies p1 p2) =
  PImplies (substitute_in_predicate old new p1) (substitute_in_predicate old new p2)
substitute_in_predicate old new (PNot p) = PNot (substitute_in_predicate old new p)
substitute_in_predicate old new (PEP (EPEq e1 e2)) =
  let e1' = if e1 == old then new else (substitute_in_expression old new e1)
      e2' = if e2 == old then new else (substitute_in_expression old new e2)
  in (PEP (EPEq e1' e2'))

substitute_in_predicate old new (PEP (EPNeq e1 e2)) =
  let e1' = if e1 == old then new else (substitute_in_expression old new e1)
      e2' = if e2 == old then new else (substitute_in_expression old new e2)
  in (PEP (EPNeq e1' e2'))
substitute_in_predicate old new (PEP (EPGTE e1 e2)) =
  let e1' = if e1 == old then new else (substitute_in_expression old new e1)
      e2' = if e2 == old then new else (substitute_in_expression old new e2)
  in (PEP (EPGTE e1' e2'))

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
withInitialPre prog = PAll "n" (PImplies (PEP (EPGTE (EVar "n") (EImm 1))) (substitute_in_predicate (EVar "r2") (EVar "n") $ wp_prog prog))


-- From meeting
wp_inst :: FWProgram -> Index -> Predicate -> Predicate
wp_inst prog idx q =
  case prog V.! idx of
    Exit -> PEP EPTrue
    Assign x (EDiv e1 e2) ->
      let q' = wp_inst prog (idx+1) q
          (EVar v) = freshVar q'
          q'sub = substitute_in_predicate (EVar x) (EVar v) q'
          ep = (PEP (EPEq (EVar v) (EDiv e1 e2)))
          notZeroAssert = PEP (EPNeq e2 (EImm 0))
      in PAll v (PAnd notZeroAssert (PImplies ep q'sub))
          
    Assign x e ->
      let  q' = wp_inst prog (idx+1) q
           (EVar v) = freshVar q'
           q'sub = substitute_in_predicate (EVar x) (EVar v) q'
           ep = PEP (EPEq (EVar v) e)
      in PAll v (PImplies ep q'sub) --(substitute_in_predicate (EVar x) (EVar v) (wp_inst2 prog (idx+1) q)))
    Cond ep offset ->
      PITE ep (wp_inst prog (idx + 1 + offset) q) (wp_inst prog (idx+1) q)
    _ -> undefined

wp_prog :: A.Program -> Predicate
wp_prog prog = wp_inst (toFWProg prog) 0 (PEP EPTrue)

toFWProg :: A.Program -> FWProgram
toFWProg = V.fromList . map toFWProg'

toFWProg' :: A.Instruction -> Instr
toFWProg' A.Exit = Exit
toFWProg' (A.Binary A.B64 A.Mov rd (Left rs)) =
  let (EVar vname) = reg2var rd  
      rs' = reg2var rs
  in Assign vname rs'

toFWProg' (A.Binary A.B64 A.Mov rd (Right imm)) =
  let (EVar vname) = reg2var rd
      imm' = EImm (fromIntegral imm)
  in Assign vname imm'


-- Arithmetic
toFWProg' (A.Binary A.B64 A.Add rd (Right imm)) =
  let (EVar vname) = reg2var rd 
      imm' = EImm (fromIntegral imm)
  in Assign vname (EAdd (EVar vname) imm')

toFWProg' (A.Binary A.B64 A.Add rd (Left rs)) =
  let (EVar vname) = reg2var rd 
      rs' = reg2var rs
  in Assign vname (EAdd (EVar vname) rs')

toFWProg' (A.Binary A.B64 A.Mul rd (Right imm)) =
  let (EVar vname) = reg2var rd 
      imm' = EImm (fromIntegral imm)
  in Assign vname (EMul (EVar vname) imm')

toFWProg' (A.Binary A.B64 A.Mul rd (Left rs)) =
  let (EVar vname) = reg2var rd 
      rs' = reg2var rs
  in Assign vname (EMul (EVar vname) rs')

toFWProg' (A.Binary A.B64 A.Xor rd (Right imm)) =
  let (EVar vname) = reg2var rd 
      imm' = EImm (fromIntegral imm)
  in Assign vname (EXor (EVar vname) imm')

toFWProg' (A.Binary A.B64 A.Xor rd (Left rs)) =
  let (EVar vname) = reg2var rd 
      rs' = reg2var rs
  in Assign vname (EXor (EVar vname) rs')


toFWProg' (A.Binary A.B64 A.Div rd (Right imm)) =
  let (EVar vname) = reg2var rd 
      imm' = EImm (fromIntegral imm)
  in Assign vname (EDiv (EVar vname) imm')

toFWProg' (A.Binary A.B64 A.Div rd (Left rs)) =
  let (EVar vname) = reg2var rd 
      rs' = reg2var rs
  in Assign vname (EDiv (EVar vname) rs')



-- Conditionals
toFWProg' (A.JCond A.Jeq rd (Right imm) off) =
  let (EVar vname) = reg2var rd
      imm' = EImm $ fromIntegral imm
      ep = EPEq (EVar vname) imm'
  in Cond ep (fromIntegral off)

toFWProg' (A.JCond A.Jeq rd (Left rs) off) =
  let (EVar vname) = reg2var rd
      rs' = reg2var rs
      ep = EPEq (EVar vname) rs'
      off' = fromIntegral off
  in Cond ep off'

toFWProg' (A.JCond A.Jgt rd (Right imm) off) =
  let (EVar vname) = reg2var rd
      imm' = EImm $ fromIntegral imm
      ep = EPGTE (EVar vname) imm'
  in Cond ep (fromIntegral off)

toFWProg' (A.JCond A.Jgt rd (Left rs) off) =
  let (EVar vname) = reg2var rd
      rs' = reg2var rs
      ep = EPGTE (EVar vname) rs'
      off' = fromIntegral off
  in Cond ep off'
  
toFWProg' (A.JCond A.Jne rd (Right imm) off) =
  let (EVar vname) = reg2var rd
      imm' = EImm $ fromIntegral imm
      ep = EPNeq (EVar vname) imm'
  in Cond ep (fromIntegral off)

toFWProg' (A.JCond A.Jne rd (Left rs) off) =
  let (EVar vname) = reg2var rd
      rs' = reg2var rs
      ep = EPNeq (EVar vname) rs'
      off' = fromIntegral off
  in Cond ep off'


  
toFWProg' _ = undefined



