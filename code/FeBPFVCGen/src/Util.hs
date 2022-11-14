module Util
  where

import Ebpf.Asm as A
import Definitions
import qualified Data.Map.Strict as M


-- Add linenumbers to each instruction in a program
numberInstrs :: A.Program -> LineNoProgram
numberInstrs = M.fromList . zip [1..]

-- Helper function to convert between ebpf-tools registers and VCGen registers
reg2reg :: A.Reg -> Register
reg2reg (A.Reg 0) = R0
reg2reg (A.Reg 1) = R1
reg2reg (A.Reg 2) = R2
reg2reg (A.Reg 3) = R3
reg2reg (A.Reg 4) = R4
reg2reg (A.Reg 5) = R5
reg2reg (A.Reg 6) = R6
reg2reg (A.Reg 7) = R7
reg2reg (A.Reg 8) = R8
reg2reg (A.Reg 9) = R9
reg2reg (A.Reg 10) = R10
reg2reg _ = undefined


-- Given two predicates where the first is specifically an implication,
-- Chain them such that the conclusion of the implication implies the new term
addImpl :: Pred -> Pred -> Pred
-- If we have true => P as the first term, and Q as second term, make it P => Q
addImpl (Impl (PS PTrue) p) q = Impl p q
-- If we have P => true as the first term and Q as the second term, make it P => Q
addImpl (Impl p (PS PTrue)) q = Impl p q
-- If we have P => (Q => R) and S, make it P => (Q => (R => S))
-- addImpl (Impl p (Impl q r)) s = Impl p (Impl q (addImpl r s))
-- If we have P -> Q as first term and R as second, make it P -> (Q -> R) by calling recursively
addImpl (Impl p q) r = Impl p (addImpl q r)
addImpl p q = Impl p q
addImpl _ _ = undefined
