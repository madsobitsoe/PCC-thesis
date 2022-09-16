import AbstractMachine (eval)
import Ebpf.Asm as A

import Test.Tasty
import Test.Tasty.HUnit

import Data.Word (Word64)
import qualified Data.Map.Strict as M
main :: IO ()
main = defaultMain $ localOption (mkTimeout 100000) allTests

allTests :: TestTree
allTests = testGroup "All tests" [ arithTests ]

arithTests = testGroup "Arithmetic tests" [
  testCase "Initialize R0" $
    let prog = (initRegister (A.Reg 0) 0)
        expected = Right (M.insert (A.Reg 0) 0 M.empty)
    in
      eval prog @?= expected

  ]

initRegister :: A.Reg -> Word64 -> A.Program
initRegister reg value =
  [
    LoadImm reg (fromIntegral value)
  , Exit
  ]
