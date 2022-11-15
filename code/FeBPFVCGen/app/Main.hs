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
           , Exit]

testProgOnlyExit = [ Exit ]


testProgOneRegMov = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
                 , Exit ]

testProgOneImmMov = [ Binary B64 Mov (Reg 0) (Right 42)
                 , Exit ]

testProgTwoMov = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
                 , Binary B64 Mov (Reg 3) (Right 42)
                 , Exit ]
testProgMovAndAdd = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
                    , Binary B64 Add (Reg 0) (Right 42)
                    , Exit ]

testProgRegAdd = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
                 , Binary B64 Add (Reg 0) (Left (Reg 2))
                 , Exit ]


testProgRegDiv = [ Binary B64 Mov (Reg 0) (Right 10)
                 , Binary B64 Mov (Reg 1) (Left (Reg 2))
                 , Binary B64 Div (Reg 0) (Left (Reg 1))
                 , Exit ]


run :: Program -> IO ()
run p =
  do
    case execute p of
      Left e -> putStrLn("*** Err: " ++ show e)
      Right (Judgment pred ms cs) ->
        do
          putStrLn "Program:"
          print p
          putStrLn "Preds:"
          print pred
          putStrLn "Machine State:"
          print ms
          putStrLn "Constants:"
          print cs

main :: IO ()
main =
  do
    run testProgOneImmMov
    run testProgOneRegMov
    run testProgTwoMov
