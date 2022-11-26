module Util
  where

import Ebpf.Asm as A
import Definitions
import Data.Word
import Text.Printf
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


-- Convert a Word32 to a smtlib2 compatible bit-vector constant
toHex :: Word32 -> String
toHex x = printf "#x%016x" x

