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
allTests = testGroup "All tests" [ ebpfToFWeBPFTests, wp_vcgen_tests ]


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

  ]

