module VCGen
    ( genVC, withDefaultOptions
    ) where

import Ebpf.Asm as A
import Ebpf.Decode as D
import SMTLib2 as S
import SMTLib2.Core as SC
import SMTLib2.BitVector as SBV
import Data.Maybe
import Data.Word
import qualified Data.Map.Strict as M


type VName = String

data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
  deriving (Ord, Show, Eq)
data Uninit = U0 | U3 | U4 | U5 | U6 | U7 | U8 | U9
  deriving (Ord, Show, Eq)
  
data SExpr =
    Imm Word64
  | Var VName
  | UnVar Uninit
  | Add SExpr SExpr
  | Sub SExpr SExpr
  | Div SExpr SExpr
  | Xor SExpr SExpr
  deriving (Show)

data PSExpr =
    PTrue
  | PFalse
  | Eq SExpr SExpr
  | Neq SExpr SExpr
  | Geq SExpr SExpr
  | LT SExpr SExpr
  -- | Init SExpr
  deriving (Show)

data Pred =
    PSExpr
  | LAnd Pred Pred
  | Impl Pred Pred
  deriving (Show)
  -- | FAll 



pred2VC :: Pred -> VC
-- pred2VC (Eq (Var n) (Imm i)) =  S.Script [ (SC.=== (funRef n) (bv (fromIntegral i) 64)) ]
pred2VC _ = undefined

type VC = S.Script

type VCGenError = String
type MachineState = M.Map Register SExpr

initialMachineState = M.fromList [ (R0, UnVar U0)
                                 , (R1, Var "m")
                                 , (R2, Var "n")
                                 , (R3, UnVar U3)
                                 , (R4, UnVar U4)
                                 , (R5, UnVar U5)
                                 , (R6, UnVar U6)
                                 , (R7, UnVar U7)
                                 , (R8, UnVar U8)
                                 , (R9, UnVar U9)
                                 , (R10, Var "fp")
                                 ]

reg2reg (A.Reg 0) = R0
reg2reg (A.Reg 1) = R1
reg2reg (A.Reg 2) = R2
reg2reg (A.Reg 3) = R3
reg2reg (A.Reg 4) = R4
reg2reg (A.Reg 5) = R5
reg2reg (A.Reg 6) = R6
reg2reg (A.Reg 7) = R7
reg2reg (A.Reg 8) = R8
reg2reg (A.Reg 9) = R9
reg2reg (A.Reg 10) = R10
reg2reg _ = undefined

data Judgment = Judgment VC MachineState

genVC :: A.Program -> Either VCGenError VC
genVC prog =
  case innerGenVC ((Judgment (S.Script []) initialMachineState)) prog of
    Left err -> Left err
    Right (Judgment vc ms) -> Right $ withDefaultOptions $ withDefaultPreconditions $ withDefaultPostamble vc

innerGenVC :: Judgment -> A.Program -> Either VCGenError Judgment
innerGenVC judgment [] = Left "Invalid program: Program was empty before exit"
innerGenVC judgment [A.Exit] = genVC' judgment A.Exit
innerGenVC judgment (i:is) =
  case genVC' judgment i of
    Left err -> Left err
    Right judgment' -> innerGenVC judgment' is
  

initialJudgment = Judgment (S.Script []) initialMachineState

updateVC :: S.Script -> PSExpr -> S.Script
updateVC _ _ = undefined


genVC' :: Judgment -> A.Instruction -> Either VCGenError Judgment
genVC' (Judgment vc ms) (A.Exit) =
  case M.lookup R0 ms of
    Nothing -> Left "INTERNAL ERROR: R0 not in map"
    Just (UnVar _) -> Left "Invalid Program: R0 not initialized at exit"
    Just _ -> Right (Judgment vc ms)
      
genVC' (Judgment vc ms) (A.Binary A.B64 A.Mov (A.Reg d) (Left (A.Reg s)))  =
                         let rd = reg2reg (A.Reg d)
                             rs = reg2reg (A.Reg s)
                         in case M.lookup rs ms of
                           Nothing -> Left "Key not in map"
                           Just sexp -> Right $ Judgment vc (M.insert rd sexp ms) 

genVC' _ _ = Left "Not implemented yet"



-- Adds the default header of options to the smtlib2 output
withDefaultOptions (S.Script cmds) =
  S.Script $ [ CmdSetLogic (N "BV")
           , CmdSetOption $ OptProduceProofs True
           , CmdSetOption $ OptAttr (Attr (N "proof-format-mode lfsc") Nothing)
           , CmdSetOption $ OptAttr (Attr (N "dump-proofs true") Nothing)] ++ cmds

withDefaultPreconditions (S.Script cmds) =
  S.Script $ [ declFun64 "m", declFun64 "n" ] ++ cmds

withDefaultPostamble (S.Script cmds) =
  S.Script $ cmds ++ [ CmdCheckSat, CmdExit ]
-- getConstDecl :: (Int,A.Instruction)  -> [S.Command] -> [S.Command]
-- getConstDecl (line, Binary B32 _  (Reg regname) _) acc =  (declFun32 $ "r" ++ show regname ++ "_" ++ show line) : acc
-- getConstDecl (line, Binary B64 _  (Reg regname) _) acc =  (declFun64 $ "r" ++ show regname ++ "_" ++ show line) : acc
-- getConstDecl (line, Unary B32 _  (Reg regname)) acc =  (declFun32 $ "r" ++ show regname ++ "_" ++ show line) : acc
-- getConstDecl (line, Unary B64 _  (Reg regname)) acc =  (declFun64 $ "r" ++ show regname ++ "_" ++ show line) : acc

-- getConstDecl _ acc = acc

-- getConstDecls :: A.Program -> S.Script
-- getConstDecls = S.Script . foldr getConstDecl [] . numberInstrs




-- -- withDefaultPreconditions (Script cmds) =
-- --   Script $ [ 

-- declFun32 name = CmdDeclareFun (N name) [] (tBitVec 32)
declFun64 name = CmdDeclareFun (N name) [] (tBitVec 64)
funRef name = App (I (N name) []) Nothing []

-- -- "good_div.smt2" program
-- t = pp $ withDefaultOptions $ Script [ declFun64 "r0_1", declFun64 "r1_2", declFun64 "r0_3"
--                           -- Actual assertion below
--                         ,CmdAssert (SC.not $ (SC.==>) ((SC.===) (funRef "r0_1") (bv 10 64) ) ((SC.==>) ((SC.===) (funRef "r1_2") (bv 2 64)) (SC.not ((SC.===) (funRef "r1_2") (bv 0 64 )))))
--                         ,CmdCheckSat
--                         ,CmdExit
--                         ]


-- numberInstrs = zip [1..]

-- -- getConstants :: A.Program -> Doc
-- getConstants prog = pp $ withDefaultOptions $ getConstDecls prog
