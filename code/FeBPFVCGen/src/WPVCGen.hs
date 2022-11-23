module WPVCGen

where

import Ebpf.Asm as A
import Data.Word
import Text.Printf
import Util
import Definitions


-- Pretty printing of expressions
ppE :: Expression -> String
ppE (EVar s) = s
ppE (EImm imm) = show imm
ppE (EReg reg) = show reg
ppE (EDivReg rd rs) = ppE rd ++ " / " ++ ppE rs

-- Pretty printing of expression predicates
ppEP :: ExpressionPredicate -> String
ppEP EPTrue = "true"
ppEP (EPEq e1 e2) = ppE e1 ++ " = " ++ ppE e2
ppEP (EPNeq e1 e2) = ppE e1 ++ " != " ++ ppE e2

-- Pretty printing of predicates
pp :: Predicate -> String
pp (PEP ep) = ppEP ep
pp (PNot p) = [toEnum 172] ++ pp p
pp (PAll e p) = [toEnum 8704] ++ ppE e ++ ". " ++ pp p
pp (PAnd p1 p2) = "(" ++ pp p1 ++ ") " ++ [toEnum 8743] ++ " (" ++ pp p2 ++ "))"
pp (PImplies p1 p2) = pp p1 ++ " " ++ [toEnum 8658] ++ " (" ++ pp p2 ++ ")"



toHex :: Word32 -> String
toHex x = printf "#x%016x" x

ppIndent :: Int -> String
ppIndent d = replicate (d*4) ' '

with_smt_lib_wrapping :: String -> String
with_smt_lib_wrapping s =
  "(set-logic BV)\n(set-option :produce-proofs true)\n(set-option :proof-format-mode lfsc)\n(set-option :dump-proofs true)\n\n" ++
  "(assert (not\n" ++
  s ++
  "))\n(check-sat)\n(exit)\n"


ppE_smt :: Expression -> String
ppE_smt (EVar s) = s
ppE_smt (EImm imm) = toHex imm
ppE_smt (EReg reg) = show reg
ppE_smt (EDivReg rd rs) = "(bvudiv " ++ ppE_smt rd ++ " " ++ ppE_smt rs ++ ")"

ppEP_smt :: ExpressionPredicate -> String
ppEP_smt EPTrue = "true"
ppEP_smt (EPEq e1 e2) = "(= " ++ ppE_smt e1 ++ " " ++ ppE_smt e2 ++ ")"
ppEP_smt (EPNeq e1 e2) = "(not " ++ ppEP_smt (EPEq e1 e2) ++ ")"

-- smt-lib printing of predicates
ppP_smt :: Int -> Predicate -> String
ppP_smt d (PEP ep) = ppIndent d ++ ppEP_smt ep
ppP_smt d (PNot p) = ppIndent d ++ "(not " ++ ppP_smt d p ++ ")"
ppP_smt d (PAnd p1 p2) = "\n" ++ ppIndent d ++ "(and \n" ++ ppP_smt (d+1) p1 ++ ppP_smt (d+1) p2 ++ "\n" ++ ppIndent d ++ ")"
ppP_smt d (PImplies p1 p2) = "\n" ++ ppIndent d ++ "(=> \n" ++ ppP_smt (d+1) p1 ++ "\n" ++ ppP_smt (d+1) p2 ++ "\n" ++ ppIndent d ++ ")"
ppP_smt d (PAll e p) = ppIndent d ++ "(forall ((" ++ ppE_smt e ++ " (_ BitVec 64))) " ++ ppP_smt (d+1) p ++ ")"

pp_smt :: Predicate -> String
pp_smt predicate = ppP_smt 0 predicate

-- ebpf-tools Asm.Program to FeatherWeightEBPF program
ebpfToFWeBPF :: A.Program -> FWProgram
ebpfToFWeBPF [] = []
ebpfToFWeBPF prog = map ebpfToWeBPF' prog

ebpfToWeBPF' :: A.Instruction -> Statement
ebpfToWeBPF' (A.Exit) = SExit
ebpfToWeBPF' (A.Binary A.B64 A.Mov rd (Left rs)) = 
  let rd' = reg2reg rd
      rs' = reg2reg rs
  in SAssign rd' (EReg rs')
ebpfToWeBPF' (A.Binary A.B64 A.Mov rd (Right imm)) =
  let rd' = reg2reg rd
  in SAssign rd' (EImm (fromIntegral imm))


ebpfToWeBPF' (A.Binary A.B64 A.Div rd (Left rs)) =
  let rd' = reg2reg rd
      rs' = EReg $ reg2reg rs
  in SAssign rd' (EDivReg (EReg rd') rs')

-- So many instructions not handled
ebpfToWeBPF' _ = undefined

-- Extract used variables from expression
varsInExpression :: Expression -> [Expression]
varsInExpression (EVar v) = [EVar v]
varsInExpression _ = []
-- Extract used variables from expression predicate
varsInExpressionPredicate :: ExpressionPredicate -> [Expression]
varsInExpressionPredicate EPTrue = []
varsInExpressionPredicate (EPEq e1 e2) = varsInExpression e1 ++ varsInExpression e2
varsInExpressionPredicate (EPNeq e1 e2) = varsInExpression e1 ++ varsInExpression e2
-- Extract used variables from predicate
varsInPredicate :: Predicate -> [Expression]
varsInPredicate (PNot predicate) = varsInPredicate predicate
varsInPredicate (PAll e p) = varsInExpression e  ++ varsInPredicate p
varsInPredicate (PAnd p1 p2) = varsInPredicate p1 ++ varsInPredicate p2
varsInPredicate (PImplies p1 p2) = varsInPredicate p1 ++ varsInPredicate p2
varsInPredicate (PEP ep) = varsInExpressionPredicate ep

-- Given a predicate, return a fresh variable not used in the predicate
freshVar :: Predicate -> Expression
freshVar predicate =
  let used = varsInPredicate predicate
      allFresh = map (\n -> EVar $ "v" ++ show n) [0..]
      unused = filter (\v -> not $ elem v used) allFresh
  in
    head unused



substitute_in_expression :: Expression -> Expression -> Expression -> Expression
substitute_in_expression old new (EDivReg rd rs) = EDivReg (substitute_in_expression old new rd) (substitute_in_expression old new rs)
substitute_in_expression old new e =
  if old == e then new
  else e

substitute_in_expression_predicate :: Expression -> ExpressionPredicate -> ExpressionPredicate
substitute_in_expression_predicate _ EPTrue = EPTrue
substitute_in_expression_predicate e (EPEq e1 e2) = EPEq e1 e2
substitute_in_expression_predicate e (EPNeq e1 e2) = EPNeq e1 e2

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

substitute_in_predicate old new (PAll e p) =
  -- let ep' = substitute_in_expression_predicate e ep
  let p' = substitute_in_predicate old new p
  in PAll e p'


wp_inst :: Statement -> Predicate -> Predicate
wp_inst (SExit) _ = PEP EPTrue
wp_inst (SAssign x (EDivReg rd rs)) q =
  let v = freshVar q
      e2 = rs
      p1 = PEP (EPNeq e2 (EImm 0))
      p2 = PImplies (PEP (EPEq v (EDivReg rd rs))) (substitute_in_predicate (EReg x) v q)
      p = PAnd p1 p2
  in
    PAll v p

wp_inst (SAssign rd e2) q =
  let v = freshVar q
      ep = PEP (EPEq v e2)
      x = EReg rd
  in
    PAll v (PImplies ep (substitute_in_predicate x v q))


wp_prog' :: FWProgram -> Predicate -> Predicate
wp_prog' [] q = q
wp_prog' (inst:prog) q = wp_inst inst $ wp_prog' prog q
-- wp_prog _ _ = undefined
wp_prog :: A.Program -> Predicate -> Predicate
wp_prog = wp_prog' . ebpfToFWeBPF
wp :: A.Program -> Predicate
wp prog = wp_prog prog (PEP EPTrue)
