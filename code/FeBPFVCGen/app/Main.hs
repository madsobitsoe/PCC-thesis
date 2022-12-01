module Main (main) where

import Ebpf.Asm as A
-- import qualified SMTLib2 as S
import WPVCGen

testProg :: A.Program
testProg = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
           , Binary B64 Mov (Reg 3) (Left (Reg 2))
           , Binary B64 Mov (Reg 4) (Left (Reg 2))
           , Binary B64 Mov (Reg 5) (Left (Reg 2))
           , Binary B64 Mov (Reg 6) (Left (Reg 2))
           , Binary B64 Mov (Reg 7) (Left (Reg 2))
           , Binary B64 Mov (Reg 8) (Left (Reg 2))
           , Exit]

testProgOnlyExit :: A.Program
testProgOnlyExit = [ Exit ]

testProgOneRegMov :: A.Program
testProgOneRegMov = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
                 , Exit ]

testProgOneImmMov :: A.Program
testProgOneImmMov = [ Binary B64 Mov (Reg 0) (Right 42)
                 , Exit ]

testProgTwoMov :: A.Program
testProgTwoMov = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
                 , Binary B64 Mov (Reg 3) (Right 42)
                 , Exit ]

testProgMovAndAdd :: A.Program
testProgMovAndAdd = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
                    , Binary B64 Add (Reg 0) (Right 42)
                    , Exit ]

testProgRegAdd :: A.Program
testProgRegAdd = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
                 , Binary B64 Add (Reg 0) (Left (Reg 2))
                 , Exit ]

-- The test program from the report
testProgRegDiv :: A.Program
testProgRegDiv = [ Binary B64 Mov (Reg 0) (Right 10)
                 , Binary B64 Mov (Reg 1) (Left (Reg 2))
                 , Binary B64 Div (Reg 0) (Left (Reg 1))
                 , Exit ]

testProgOverWriteMovMultiple :: A.Program
testProgOverWriteMovMultiple =
  [
    Binary B64 Mov (Reg 0) (Right 0)
  , Binary B64 Mov (Reg 0) (Right 1)
  , Binary B64 Mov (Reg 0) (Right 2)
  , Binary B64 Mov (Reg 0) (Right 3)
  , Binary B64 Mov (Reg 0) (Right 4)
  , Binary B64 Mov (Reg 0) (Right 5)
  , Exit
  ]

testProgOverWriteMovAfterDiv :: A.Program
testProgOverWriteMovAfterDiv =
  [
    Binary B64 Mov (Reg 0) (Right 10)
  , Binary B64 Div (Reg 0) (Left (Reg 2))
  , Binary B64 Mov (Reg 0) (Right 2)
  , Binary B64 Mov (Reg 1) (Right 42)
  , Binary B64 Div (Reg 1) (Left (Reg 0))  
  , Exit
  ]
testProgDivSeries :: A.Program
testProgDivSeries =
  [
    Binary B64 Mov (Reg 0) (Right 8) -- r0 = 8
  , Binary B64 Mov (Reg 1) (Right 2) -- r1 = 2
  , Binary B64 Div (Reg 0) (Left (Reg 1)) -- r0 = 4
  , Binary B64 Mov (Reg 2) (Right 4)     -- r2 = 4
  , Binary B64 Div (Reg 0) (Left (Reg 2)) -- r0 = 1
  , Binary B64 Mov (Reg 1) (Right 42)     -- r1 = 42
  , Binary B64 Div (Reg 1) (Left (Reg 0)) -- r1 = 42  
  , Exit
  ]


testProgDivImm :: A.Program
testProgDivImm =
  [
    Binary B64 Mov (Reg 0) (Right 8) -- r0 = 8
  , Binary B64 Div (Reg 0) (Right 1) -- r0 = r0 / 1
  , Binary B64 Div (Reg 0) (Right 42)     -- r0 = r0 / 42
  , Exit
  ]


testProgRegDivR1Noninit :: A.Program
testProgRegDivR1Noninit = [ Binary B64 Mov (Reg 0) (Right 10)
                 , Binary B64 Div (Reg 0) (Left (Reg 1))
                 , Exit ]

testProgJeq :: A.Program
testProgJeq = [ Binary B64 Mov (Reg 0) (Right 10)
              , JCond Jeq (Reg 2) (Right 0) 1
              , Binary B64 Div (Reg 0) (Left (Reg 2))
              , Exit ]

testProgAddMulDiv :: A.Program
testProgAddMulDiv =
  [ Binary B64 Mov (Reg 0) (Right 10)
  , Binary B64 Mov (Reg 1) (Right 0)
  , Binary B64 Add (Reg 0) (Right 10)
  , Binary B64 Add (Reg 1) (Left (Reg 0))
  , Binary B64 Mul (Reg 1) (Left (Reg 0))  
  , Binary B64 Div (Reg 0) (Left (Reg 1))
  , Exit ]

testProgXorDiv :: A.Program
testProgXorDiv =
  [ Binary B64 Mov (Reg 0) (Right 10)
  , Binary B64 Mov (Reg 1) (Right 0)
  , Binary B64 Xor (Reg 1) (Left (Reg 0))
  , Binary B64 Div (Reg 0) (Left (Reg 1))
  , Exit ]

testProgXorInitDiv :: A.Program
testProgXorInitDiv =
  [ Binary B64 Mov (Reg 0) (Right 42)
  , Binary B64 Mov (Reg 1) (Left (Reg 2))
  , Binary B64 Xor (Reg 1) (Left (Reg 2))
  , Binary B64 Add (Reg 1) (Right 1)
  , Binary B64 Div (Reg 0) (Left (Reg 1))
  , Exit ]




run :: Program -> IO ()
run p =
  do
    let predicate = withInitialPre p
    putStrLn $ "Program: " ++ show p
    putStrLn $ "Predicate: " ++ show predicate
    putStrLn $ "prettified predicate: " ++ pp predicate
    putStrLn "As smtlib2:"
    putStrLn $ with_smt_lib_wrapping $ pp_smt predicate

main :: IO ()
main =
  do
    run testProgRegDiv
    run testProgOnlyExit
    run testProgTwoMov
    run testProgOverWriteMovMultiple
    run testProgOverWriteMovAfterDiv
    run testProgDivSeries
    run testProgRegDivR1Noninit
    run testProgJeq
    run testProgDivImm
    run testProgAddMulDiv
    run testProgXorDiv
    run testProgXorInitDiv
