type VName = String

data Primitive =
    PVar VName
  | PImm Word32
  deriving (Eq, Show)

data Mem = Mem VName (Maybe Primitive)
  deriving (Eq, Show)

data Expression =
    EPrim Primitive
  | EAdd  Primitive Primitive
  | ESub  Primitive Primitive
  | EMul  Primitive Primitive
  | EDiv  Primitive Primitive
  | EXor  Primitive Primitive
  | EMod  Primitive Primitive
  | ELoad Mem Primitive
  deriving (Eq, Show)

data ExpressionPredicate =
    EPTrue
  | EPEq  Primitive Expression
  | EPNeq Primitive Expression
  | EPGTE Primitive Expression
  | EPLTE Primitive Expression
  | EPLT  Primitive Expression
  deriving (Eq, Show)

type FWProgram = Vector Instr
data Instr =
    Assign VName Expression
  | Cond ExpressionPredicate Index
  | Store Mem Primitive Primitive
  | Exit
  deriving (Eq, Show)
