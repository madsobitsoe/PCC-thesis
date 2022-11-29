module Definitions
  where

-- import Control.Monad
-- import Control.Monad.Trans.State.Lazy
-- import Control.Monad.Trans.Reader
-- --import Control.Monad.Trans.Except
-- import Control.Monad.Except
-- import Control.Monad.Trans


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
  | EPGTE Expression Expression
  deriving (Eq, Show)

data Statement =
    SExit
  | SAssign Register Expression
  | SSeq Statement Statement
  | SITE ExpressionPredicate Statement Statement
  deriving (Eq, Show)    

type FWProgram = Statement

data Predicate =
    PEP ExpressionPredicate
  | PNot Predicate
    -- \all v . v = e => Q[x <- v]
  | PAll Expression Predicate
  | PAnd Predicate Predicate
  | PImplies Predicate Predicate
  | PITE ExpressionPredicate Predicate Predicate
  deriving (Eq, Show)

-- Data.Vector 
-- data Program = [Instr2] -- Brug Data.Vector 
-- data Instr2 = --Seq Cmd Instr2
--              SAssign Register Expression
--            | Cond Prim RelOp Prim Index
--            | Jmp Index 
--            | Exit
--   deriving (Eq, Show, Generic)


-- data Instr = Seq Cmd Instr
--            | Cond Prim RelOp Prim Instr Instr
--            | Exit
--   deriving (Eq, Show, Generic)
