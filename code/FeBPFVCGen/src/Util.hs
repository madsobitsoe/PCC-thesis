module Util
  where

import Ebpf.Asm as A
import Definitions
import Data.Word
import Text.Printf
-- Helper function to convert between ebpf-tools registers and VCGen Variables
reg2var :: A.Reg -> Expression
reg2var (A.Reg 0) = EPrim (PVar "r0")
reg2var (A.Reg 1) = EPrim (PVar "r1")
reg2var (A.Reg 2) = EPrim (PVar "n")
reg2var (A.Reg 3) = EPrim (PVar "r3")
reg2var (A.Reg 4) = EPrim (PVar "r4")
reg2var (A.Reg 5) = EPrim (PVar "r5")
reg2var (A.Reg 6) = EPrim (PVar "r6")
reg2var (A.Reg 7) = EPrim (PVar "r7")
reg2var (A.Reg 8) = EPrim (PVar "r8")
reg2var (A.Reg 9) = EPrim (PVar "r9")
reg2var (A.Reg 10) = EPrim (PVar "r10")

-- Convert a Word32 to a smtlib2 compatible bit-vector constant
toHex :: Word32 -> String
toHex x = printf "#x%016x" x

