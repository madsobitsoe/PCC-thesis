module VCGen
    ( genVC, t, withDefaultOptions, getConstants
    ) where

import Ebpf.Asm as A
import Ebpf.Decode as D
import SMTLib2 as S
import SMTLib2.Core as SC
import SMTLib2.BitVector as SBV
import Data.Maybe

type VC = [Bool]

genVC :: A.Program -> VC
genVC _ = []


getConstDecl :: (Int,A.Instruction)  -> [S.Command] -> [S.Command]
getConstDecl (line, Binary B32 _  (Reg regname) _) acc =  (declFun32 $ "r" ++ show regname ++ "_" ++ show line) : acc
getConstDecl (line, Binary B64 _  (Reg regname) _) acc =  (declFun64 $ "r" ++ show regname ++ "_" ++ show line) : acc
getConstDecl (line, Unary B32 _  (Reg regname)) acc =  (declFun32 $ "r" ++ show regname ++ "_" ++ show line) : acc
getConstDecl (line, Unary B64 _  (Reg regname)) acc =  (declFun64 $ "r" ++ show regname ++ "_" ++ show line) : acc

getConstDecl _ acc = acc

getConstDecls :: A.Program -> Script
getConstDecls = Script . foldr getConstDecl [] . numberInstrs


-- Adds the default header of options to the smtlib2 output
withDefaultOptions (Script cmds) =
  Script $ [ CmdSetLogic (N "BV")
           , CmdSetOption $ OptProduceProofs True
           , CmdSetOption $ OptAttr (Attr (N "proof-format-mode lfsc") Nothing)
           , CmdSetOption $ OptAttr (Attr (N "dump-proofs true") Nothing)] ++ cmds

declFun32 name = CmdDeclareFun (N name) [] (tBitVec 32)
declFun64 name = CmdDeclareFun (N name) [] (tBitVec 64)
funRef name = App (I (N name) []) Nothing []

-- "good_div.smt2" program
t = pp $ withDefaultOptions $ Script [ declFun64 "r0_1", declFun64 "r1_2", declFun64 "r0_3"
                          -- Actual assertion below
                        ,CmdAssert (SC.not $ (SC.==>) ((SC.===) (funRef "r0_1") (bv 10 64) ) ((SC.==>) ((SC.===) (funRef "r1_2") (bv 2 64)) (SC.not ((SC.===) (funRef "r1_2") (bv 0 64 )))))
                        ,CmdCheckSat
                        ,CmdExit
                        ]


numberInstrs = zip [1..]

-- getConstants :: A.Program -> Doc
getConstants prog = pp $ withDefaultOptions $ getConstDecls prog
