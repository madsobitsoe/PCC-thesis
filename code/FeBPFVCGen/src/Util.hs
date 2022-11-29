module Util
  where

import Ebpf.Asm as A
import Definitions
import Data.Word
import Text.Printf
-- Helper function to convert between ebpf-tools registers and VCGen Variables
reg2var :: A.Reg -> Expression
reg2var (A.Reg 0) = EVar "r0"
reg2var (A.Reg 1) = EVar "r1"
reg2var (A.Reg 2) = EVar "n"
reg2var (A.Reg 3) = EVar "r3"
reg2var (A.Reg 4) = EVar "r4"
reg2var (A.Reg 5) = EVar "r5"
reg2var (A.Reg 6) = EVar "r6"
reg2var (A.Reg 7) = EVar "r7"
reg2var (A.Reg 8) = EVar "r8"
reg2var (A.Reg 9) = EVar "r9"
reg2var (A.Reg 10) = EVar "r10"

-- Convert a Word32 to a smtlib2 compatible bit-vector constant
toHex :: Word32 -> String
toHex x = printf "#x%016x" x

