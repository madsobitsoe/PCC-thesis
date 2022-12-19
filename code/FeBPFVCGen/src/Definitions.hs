module Definitions
  where

import Data.Word (Word32)
import Data.Vector as V
import Data.Map.Strict as M 

type VName = String

data Primitive =
    PVar VName
  | PImm Word32
  deriving (Eq, Show)

-- data Mem = Mem VName Primitive
--   deriving (Eq, Show)

-- data Mem2 p = Mem2 VName p
--   deriving (Eq, Show)


data Mem = Mem VName (Maybe Primitive)
  deriving (Eq, Show)

data Expression =
    EPrim Primitive
  | EAdd Primitive Primitive
  | EMul Primitive Primitive
  | EDiv Primitive Primitive
  | EXor Primitive Primitive
  | EMod Primitive Primitive
  | ELoad Mem Primitive
  deriving (Eq, Show)

data ExpressionPredicate =
    EPTrue
  | EPEq  Primitive Expression
  | EPNeq Primitive Expression
  | EPGTE Primitive Expression
  | EPLTE Primitive Expression
  | EPLT Primitive Expression
  deriving (Eq, Show)

data Predicate =
    PEP ExpressionPredicate
  | PNot Predicate
    -- \all v . v = e => Q[x <- v]
  | PAll VName Predicate
  | PAnd Predicate Predicate
  | PImplies Predicate Predicate
  | PITE ExpressionPredicate Predicate Predicate
  deriving (Eq, Show)

type Index = Int

type FWProgram = Vector Instr
data Instr =
    Assign VName Expression
  | Cond ExpressionPredicate Index
  -- | Jmp Index -- Currently no uncond jumps, but hopefully later
  | Store Mem Primitive Primitive
  | Exit
  deriving (Eq, Show)

data FWType = TInt64 | TMem Primitive | TUnknown
  deriving (Eq, Show)
type FWTypeEnv = M.Map VName FWType

initialTypeEnvironment :: FWTypeEnv
initialTypeEnvironment =
  M.fromList [("r0",  TUnknown)
             ,("m",  TMem (PVar "n"))
             ,("n",  TInt64)
             ,("r3",  TUnknown)
             ,("r4",  TUnknown)
             ,("r5",  TUnknown)
             ,("r6",  TUnknown)
             ,("r7",  TUnknown)
             ,("r8",  TUnknown)
             ,("r9",  TUnknown)
             ,("r10", TMem (PImm $ fromIntegral 512))
             ]
