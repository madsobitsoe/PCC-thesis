module Main (main) where

import VCGen
import AbstractMachine
import Ebpf.Asm

testProg = [Binary B64 Mov (Reg 0) (Right 0)
           ,Binary B64 Mov (Reg 1) (Right 0)
           ,Binary B64 Mov (Reg 2) (Right 0)
           ,Binary B64 Mov (Reg 3) (Right 0)
           ,Binary B64 Add (Reg 0) (Right 19)
           ,Exit]

main :: IO ()
main =
  do
    let res = genVC []
        progRes = eval []
    -- print $ t
    print $ getConstants testProg
    -- putStrLn "Woop woop"
