module Definitions
  where

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
--import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad.Trans


import Ebpf.Asm as A
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Word


type VName = String

data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
  deriving (Ord, Show, Eq)

data Expression =
    EVar String
  | EImm Word32
  | EReg Register
--  | EDivReg Register Register
  | EDivReg Expression Expression
  deriving (Eq, Show)

data ExpressionPredicate =
    EPTrue
  | EPEq Expression Expression
  | EPNeq Expression Expression
  deriving (Eq, Show)

data Statement =
    SExit
    -- Mov rd, e
  | SAssign Register Expression
  -- Necessary? 
  -- | SSeq Statement Statement
  deriving (Eq, Show)    

type FWProgram = [Statement]

data Predicate =
    PEP ExpressionPredicate
  | PNot Predicate
  -- | PEq ExpressionPredicate ExpressionPredicate
    -- \all v . v = e => Q[x <- v]
  | PAll Expression Predicate
  | PAnd Predicate Predicate
  | PImplies Predicate Predicate
  deriving (Eq, Show)


-- -- Corresponds to (Expressions E) in report
-- data SExpr =
-- --    SImm Word64
--     SImm Word32  
--   | SVar VName
--   -- | SUnVar Uninit
--   -- | SAdd SExpr SExpr
--   -- | SSub SExpr SExpr
--   | SDiv SExpr SExpr
--   -- | SXor SExpr SExpr
--   deriving (Show)

-- -- Corresponds to (Expression Predicates EP) in report
-- data PSExpr =
--     PTrue
--   -- | PFalse
--   | Eq SExpr SExpr
--   | Neq SExpr SExpr
--   | Geq SExpr SExpr
--   -- | Lt SExpr SExpr
--   deriving (Show)

-- -- Corresponds to (Predicates P) in report
-- data Pred =
--     PS PSExpr
--   -- | Not Pred 
--   | LAnd Pred Pred
--   | Impl Pred Pred
--   deriving (Show)

-- type VCGenError = String
-- type MachineState = M.Map Register SExpr

-- type Constants = M.Map Register [VName]

-- initialConstants = M.fromList [ (R0, [])
--                               , (R1, ["m"])
--                               , (R2, ["n"])
--                               , (R3, [])
--                               , (R4, [])
--                               , (R5, [])
--                               , (R6, [])
--                               , (R7, [])
--                               , (R8, [])
--                               , (R9, [])
--                               , (R10,["fp"])
--                               ]


-- initialMachineState = M.fromList [ (R1, SVar "m")
--                                  , (R2, SVar "n")
--                                  , (R10, SVar "fp")
--                                  ]


-- -- initialJudgment = Judgment (Impl (PS (Geq (SVar "n") (SImm 1))) (PS PTrue)) initialMachineState initialConstants
-- initialJudgment = Judgment (PS (Geq (SVar "n") (SImm 1))) initialMachineState initialConstants

-- data Judgment = Judgment Pred MachineState Constants
--   deriving (Show)

-- type LineNoInstruction = (Int, A.Instruction)
-- type LineNoProgram = M.Map Int A.Instruction


-- -- Monad definitions
-- --type VCGenComp a = ReaderT LineNoProgram (StateT Judgment (Except VCGenError)) a
-- type VCGenComp a = ReaderT LineNoProgram (StateT Judgment (Except VCGenError)) a

