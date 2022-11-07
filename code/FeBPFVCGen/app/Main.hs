module Main (main) where

import VCGen
-- import AbstractMachine
import Ebpf.Asm
import SMTLib2 as S
testProg = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
           , Binary B64 Mov (Reg 3) (Left (Reg 2))
           , Binary B64 Mov (Reg 4) (Left (Reg 2))
           , Binary B64 Mov (Reg 5) (Left (Reg 2))
           , Binary B64 Mov (Reg 6) (Left (Reg 2))
           , Binary B64 Mov (Reg 7) (Left (Reg 2))
           , Binary B64 Mov (Reg 8) (Left (Reg 2))
                                                       
           -- ,Binary B64 Mov (Reg 1) (Right 0)
           -- ,Binary B64 Mov (Reg 2) (Right 0)
           -- ,Binary B64 Mov (Reg 3) (Right 0)
           -- ,Binary B64 Add (Reg 0) (Right 19)
           ,Exit]

testProgOnlyExit = [ Exit ]


main :: IO ()
main =
  do
    case genVC testProgOnlyExit of
      Left err -> print err
      Right res -> print $ S.pp res
    case genVC testProg of
      Left err -> print err
      Right res -> print $ S.pp res
        -- progRes = eval []
    -- print $ t
    -- print $ getConstants testProg
    -- putStrLn "Woop woop"
