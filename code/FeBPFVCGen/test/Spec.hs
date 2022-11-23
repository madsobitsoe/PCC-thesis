-- import AbstractMachine (eval)
import WPVCGen
import Definitions
import Ebpf.Asm as A

import Test.Tasty
import Test.Tasty.HUnit

import Data.Word (Word64)
import qualified Data.Map.Strict as M
main :: IO ()
main = defaultMain $ localOption (mkTimeout 100000) allTests

allTests :: TestTree
allTests = testGroup "All tests" [ wp_vcgen_tests ]


wp_vcgen_tests = testGroup "WPVCGen tests" [
    testCase "Empty program gives Q" $
    let prog = []
        q = PEP EPTrue
    in
      wp_prog prog q @?= q

  , testCase "program with only exit gives PTrue" $
  let prog = [A.Exit]
      q = PEP EPTrue
  in
    wp_prog prog q @?= q

  , testCase "program with mov x,imm then exit gives forall v . v = imm => Q[x <- v]" $
  let prog = [ A.Binary A.B64 A.Mov (Reg 0) (Right 42)
             , A.Exit]
      v = EVar "v0"
      e = EImm 42
      ep = PEP (EPEq v e)
      
      q = PAll v (PImplies ep (PEP EPTrue))
      initial = PEP EPTrue
  in
    wp_prog prog initial @?= q

  , testCase "program with mov imm, mov reg, exit gives PTrue" $
  let prog = [ A.Binary A.B64 A.Mov (Reg 0) (Right 42)
             , A.Binary A.B64 A.Mov (Reg 1) (Left (Reg 0))
             , A.Exit]
      q = PEP EPTrue
  in
    wp_prog prog q @?= q

  , testCase "program with multiple mov reg, reg substitutes correctly" $
  let prog = [ A.Binary A.B64 A.Mov (Reg 0) (Right 42)     -- mov r0, 42 : forall v4 . v4 = 42 => 
             , A.Binary A.B64 A.Mov (Reg 1) (Left (Reg 0)) -- mov r1, r0 : forall v3 . v3 = v4 =>
             , A.Binary A.B64 A.Mov (Reg 2) (Left (Reg 1)) -- mov r2, r1 : forall v2 . v2 = v3 =>
             , A.Binary A.B64 A.Mov (Reg 3) (Left (Reg 2)) -- mov r3, r2 : forall v1 . v1 = v2 =>
             , A.Binary A.B64 A.Mov (Reg 4) (Left (Reg 3)) -- mov r4, r3 : forall v0 . v0 = v1 =>
             , A.Exit]                                     -- exit       : true            
      initial = PEP EPTrue
      v4 = EVar "v4"
      v3 = EVar "v3"
      v2 = EVar "v2"
      v1 = EVar "v1"
      v0 = EVar "v0"
      all = \v p -> PAll v p
      impl = \p1 p2 -> PImplies p1 p2
      eq = \e1 e2 -> PEP (EPEq e1 e2)
      -- q  = PAll v4 (PImplies (PEP (EPEq v4 (EImm 42)))
      --                        (PAll v3 (PImplies (PEP (EPEq v3 v4))
      --                                           (PAll v2 (PImplies (PEP (EPEq v2 v3))
      --                                                              (PAll v1 (PImplies (PEP (EPEq v1 v2))
      --                                                                                 (PAll v0 (PImplies (PEP (EPEq v0 v1))
      --                                                                                                    (PEP EPTrue))))))))))
      q' = all v4 (impl (eq v4 (EImm 42)) (all v3 (impl (eq v3 v4) (all v2 (impl (eq v2 v3) (all v1 (impl (eq v1 v2) (all v0 (impl (eq v0 v1) initial)))))))))
  in
    wp_prog prog initial @?= q'
  
  , testCase "program with div does something idk" $
  let prog = [ A.Binary A.B64 A.Mov (Reg 0) (Right 10)
             , A.Binary A.B64 A.Mov (Reg 1) (Left (Reg 2))
             , A.Binary A.B64 A.Div (Reg 0) (Left (Reg 1))
             , A.Exit]
      q = PEP EPTrue
  in
    wp_prog prog q @?= q

  , testCase "program with div r0, r1, then exit" $
  let prog = [ A.Binary A.B64 A.Div (Reg 0) (Left (Reg 1))
             , A.Exit]
      initial = PEP EPTrue
      e1 = EReg R0
      e2 = EReg R1
      v  = EVar "v0"
      ep1 = PEP (EPNeq e2 (EImm 0))
      ep2 = PEP (EPEq v (EDivReg R0 R1))
      q = PAll v (PAnd ep1 (PImplies ep2 initial))
  in
    wp_prog prog initial @?= q
  

  ]

-- arithTests = testGroup "Arithmetic tests" [
--   testCase "Initialize R0" $
--     let prog = (initRegister (A.Reg 0) 0)
--         expected = Right (M.insert (A.Reg 0) 0 M.empty)
--     in
--       eval prog @?= expected

--   ]

-- initRegister :: A.Reg -> Word64 -> A.Program
-- initRegister reg value =
--   [
--     LoadImm reg (fromIntegral value)
--   , Exit
--   ]
