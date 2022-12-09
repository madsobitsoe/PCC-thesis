module Definitions
  where


import Ebpf.Asm as A
import Data.Maybe
import Data.Word
import Data.Vector as V
import Data.Map.Strict as M 

type VName = String

data Primitive =
    PVar VName
  | PImm Word32
  deriving (Eq, Show)

data Expression =
    EPrim Primitive
  | EAdd Primitive Primitive
  | EMul Primitive Primitive
  | EDiv Primitive Primitive
  | EXor Primitive Primitive
  deriving (Eq, Show)

data ExpressionPredicate =
    EPTrue
  | EPEq  Primitive Expression
  | EPNeq Primitive Expression
  | EPGTE Primitive Expression
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
  | Exit
  deriving (Eq, Show)

data FWType = Int64 | Mem Primitive | Unknown
  deriving (Eq, Show)
type FWTypeEnv = M.Map VName FWType

initialTypeEnvironment :: FWTypeEnv
initialTypeEnvironment =
  M.fromList [("r0", Unknown)
             ,("r1", Mem (PVar "r2"))
             ,("r2", Int64)
             ,("r3", Unknown)
             ,("r4", Unknown)
             ,("r5", Unknown)
             ,("r6", Unknown)
             ,("r7", Unknown)
             ,("r8", Unknown)
             ,("r9", Unknown)
             ,("r10", Mem (PImm $ fromIntegral 512))
             ]
