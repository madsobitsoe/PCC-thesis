module Util
  where

import Ebpf.Asm as A
import Definitions
import Data.Word
import Text.Printf
-- Helper function to convert between ebpf-tools registers and VCGen Variables
reg2var :: A.Reg -> Primitive
reg2var (A.Reg 0) = PVar "r0"
reg2var (A.Reg 1) = PVar "m"
reg2var (A.Reg 2) = PVar "n"
reg2var (A.Reg 3) = PVar "r3"
reg2var (A.Reg 4) = PVar "r4"
reg2var (A.Reg 5) = PVar "r5"
reg2var (A.Reg 6) = PVar "r6"
reg2var (A.Reg 7) = PVar "r7"
reg2var (A.Reg 8) = PVar "r8"
reg2var (A.Reg 9) = PVar "r9"
reg2var (A.Reg 10) = PVar "fp"
reg2var _ = undefined

-- Convert a Word32 to a smtlib2 compatible bit-vector constant
toHex :: Word32 -> String
toHex x = printf "#x%016x" x

