-- import AbstractMachine (eval)
import WPVCGen
import Definitions
import qualified Ebpf.Asm as A

import Test.Tasty
import Test.Tasty.HUnit

import Data.Word (Word64)
import qualified Data.Map.Strict as M

import qualified Data.Vector as V
main :: IO ()
main = defaultMain $ localOption (mkTimeout 100000) allTests

allTests :: TestTree
allTests = testGroup "All tests" [ ebpfToFWeBPFTests, wp_vcgen_tests, typecheck_tests ]


ebpfToFWeBPFTests = testGroup "ebpfToFWeBPFTests" [
  testCase "Empty program gives empty Vector" $    
    let prog = ([] :: A.Program)
    in
      toFWProg prog @?= V.empty

  ,  testCase "Program with Exit gives Vector with Exit" $    
    let prog = ([A.Exit] :: A.Program)
    in
      toFWProg prog @?= V.fromList [Exit]
      
  ]
  
wp_vcgen_tests = testGroup "WPVCGen tests" [
    testCase "Instr exit gives true" $
    let progA = [A.Exit]
        progFW = toFWProg progA
        initial = PEP EPTrue
    in
      wp_inst progFW 0 initial @?= initial

    , testCase "mov reg reg substitutes without forall" $
    let progA = [A.Binary A.B64 A.Mov (A.Reg 0) (Left (A.Reg 2))
                ,A.Binary A.B64 A.Mov (A.Reg 3) (Left (A.Reg 0))
                ,A.Binary A.B64 A.Mov (A.Reg 4) (Left (A.Reg 3))
                ,A.Binary A.B64 A.Mov (A.Reg 5) (Left (A.Reg 4))
                ,A.Binary A.B64 A.Mov (A.Reg 6) (Left (A.Reg 5))
                ,A.Binary A.B64 A.Mov (A.Reg 7) (Left (A.Reg 6))
                ,A.Binary A.B64 A.Div (A.Reg 7) (Left (A.Reg 2))                
                ,A.Exit]
        progFW = toFWProg progA
        initial = PEP EPTrue
        expected = (PAll "v0" (PAnd (PEP (EPNeq (PVar "n") (EPrim (PImm 0)))) (PImplies (PEP (EPEq (PVar "v0") (EDiv (PVar "n") (PVar "n")))) (PEP EPTrue))))
    in
      wp_inst progFW 0 initial @?= expected

  ]

typecheck_tests = testGroup "Typecheck tests" [
    testCase "Exit only typechecks" $
    let prog = toFWProg [A.Exit]
    in
      typeCheck prog  @?= Right prog

    , testCase "Mov,Exit typechecks" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0), A.Exit]
          fwprog = toFWProg prog
      in
        typeCheck fwprog  @?= Right fwprog

    , testCase "plus of two int64 regs typechecks" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Mov (A.Reg 1) (Right 1)
                 , A.Binary A.B64 A.Add (A.Reg 0) (Left (A.Reg 1))
                 , A.Exit]
          fwprog = toFWProg prog
      in
        typeCheck fwprog  @?= Right fwprog
    , testCase "mul of two int64 regs typechecks" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Mov (A.Reg 1) (Right 1)
                 , A.Binary A.B64 A.Mul (A.Reg 0) (Left (A.Reg 1))
                 , A.Exit]
          fwprog = toFWProg prog
      in
        typeCheck fwprog  @?= Right fwprog
    , testCase "Div of two int64 regs typechecks" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Mov (A.Reg 1) (Right 1)
                 , A.Binary A.B64 A.Div (A.Reg 0) (Left (A.Reg 1))
                 , A.Exit]
          fwprog = toFWProg prog
      in
        typeCheck fwprog  @?= Right fwprog
    , testCase "xor of two int64 regs typechecks" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Mov (A.Reg 1) (Right 1)
                 , A.Binary A.B64 A.Xor (A.Reg 0) (Left (A.Reg 1))
                 , A.Exit]
          fwprog = toFWProg prog
      in
        typeCheck fwprog  @?= Right fwprog
    , testCase "mul with mem-reg does not typecheck" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Mul (A.Reg 0) (Left (A.Reg 1))
                 , A.Exit]
          fwprog = toFWProg prog
          res =
            case typeCheck fwprog of
              Left _ -> True
              Right _ -> False
      in
        assertBool "Arithmetic with pointer should not type check" res

    , testCase "add with mem-reg does not typecheck" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Add (A.Reg 0) (Left (A.Reg 1))
                 , A.Exit]
          fwprog = toFWProg prog
          res =
            case typeCheck fwprog of
              Left _ -> True
              Right _ -> False
      in
        assertBool "Arithmetic with pointer should not type check" res
    , testCase "Div with mem-reg does not typecheck" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Div (A.Reg 0) (Left (A.Reg 1))
                 , A.Exit]
          fwprog = toFWProg prog
          res =
            case typeCheck fwprog of
              Left _ -> True
              Right _ -> False
      in
        assertBool "Arithmetic with pointer should not type check" res
    , testCase "xor with mem-reg does not typecheck" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Xor (A.Reg 0) (Left (A.Reg 1))
                 , A.Exit]
          fwprog = toFWProg prog
          res =
            case typeCheck fwprog of
              Left _ -> True
              Right _ -> False
      in
        assertBool "Arithmetic with pointer should not type check" res

    , testCase "add with unknown-reg does not typecheck" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Add (A.Reg 0) (Left (A.Reg 3))
                 , A.Exit]
          fwprog = toFWProg prog
          res =
            case typeCheck fwprog of
              Left _ -> True
              Right _ -> False
      in
        assertBool "Arithmetic with pointer should not type check" res
        
    , testCase "mul with unknown-reg does not typecheck" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Mul (A.Reg 0) (Left (A.Reg 3))
                 , A.Exit]
          fwprog = toFWProg prog
          res =
            case typeCheck fwprog of
              Left _ -> True
              Right _ -> False
      in
        assertBool "Arithmetic with pointer should not type check" res
    
    , testCase "div with unknown-reg does not typecheck" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Div (A.Reg 0) (Left (A.Reg 3))
                 , A.Exit]
          fwprog = toFWProg prog
          res =
            case typeCheck fwprog of
              Left _ -> True
              Right _ -> False
      in
        assertBool "Arithmetic with pointer should not type check" res
    , testCase "xor with unknown-reg does not typecheck" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Xor (A.Reg 0) (Left (A.Reg 3))
                 , A.Exit]
          fwprog = toFWProg prog
          res =
            case typeCheck fwprog of
              Left _ -> True
              Right _ -> False
      in
        assertBool "Arithmetic with pointer should not type check" res

    , testCase "add with mem from mov does not typecheck" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Mov (A.Reg 3) (Left (A.Reg 1))
                 , A.Binary A.B64 A.Add (A.Reg 0) (Left (A.Reg 3))
                 , A.Exit]
          fwprog = toFWProg prog
          res =
            case typeCheck fwprog of
              Left _ -> True
              Right _ -> False
      in
        assertBool "Arithmetic with pointer should not type check" res
    , testCase "Mul with mem from mov does not typecheck" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Mov (A.Reg 3) (Left (A.Reg 1))
                 , A.Binary A.B64 A.Mul (A.Reg 0) (Left (A.Reg 3))
                 , A.Exit]
          fwprog = toFWProg prog
          res =
            case typeCheck fwprog of
              Left _ -> True
              Right _ -> False
      in
        assertBool "Arithmetic with pointer should not type check" res
    , testCase "Div with mem from mov does not typecheck" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Mov (A.Reg 3) (Left (A.Reg 1))
                 , A.Binary A.B64 A.Div (A.Reg 0) (Left (A.Reg 3))
                 , A.Exit]
          fwprog = toFWProg prog
          res =
            case typeCheck fwprog of
              Left _ -> True
              Right _ -> False
      in
        assertBool "Arithmetic with pointer should not type check" res
    , testCase "Xor with mem from mov does not typecheck" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 0) (Right 0)
                 , A.Binary A.B64 A.Mov (A.Reg 3) (Left (A.Reg 1))
                 , A.Binary A.B64 A.Xor (A.Reg 0) (Left (A.Reg 3))
                 , A.Exit]
          fwprog = toFWProg prog
          res =
            case typeCheck fwprog of
              Left _ -> True
              Right _ -> False
      in
        assertBool "Arithmetic with pointer should not type check" res
    , testCase "(type)-Valid load typechecks" $
      let prog = [A.Load A.B64 (A.Reg 0) (A.Reg 1) Nothing
                 , A.Exit]
          fwprog = toFWProg prog
      in
        typeCheck fwprog @?= Right fwprog
    , testCase "(type)-Valid load from mov typechecks" $
      let prog = [A.Binary A.B64 A.Mov (A.Reg 3) (Left (A.Reg 1))
                 ,A.Load A.B64 (A.Reg 0) (A.Reg 3) Nothing
                 , A.Exit]
          fwprog = toFWProg prog
          res = case typeCheck fwprog of
            Left _ -> False
            Right _ -> True
      in
        assertBool "Moving a mem around did not typecheck as expected" res

            
  ]

