module Definitions
  where


import Ebpf.Asm as A
import Data.Maybe
import Data.Word
import Data.Vector as V

type VName = String

data Primitive =
    PVar VName
  | PImm Word32
  deriving (Eq, Show)

data Expression =
    EPrim Primitive
  --   EVar VName
  -- | EImm Word32
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

