module VCGen
    ( genVC
    ) where

import Ebpf.Asm as A
import Ebpf.Decode as D


type VC = [Bool]

genVC :: A.Program -> VC
genVC _ = []
