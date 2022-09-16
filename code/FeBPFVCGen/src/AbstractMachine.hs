module AbstractMachine
    ( eval
    ) where

import Ebpf.Asm as A
import Ebpf.Decode as D
import qualified Data.Map.Strict as M
import Data.Map (Map)
import Data.Word (Word64)

type Data = Word64
type VC = [Bool]

-- data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
type Registers = Map A.Reg Data
data EvalError = NonTerminate | RegNotInit
  deriving (Eq, Show)

data EvalProg = EvalProg Program Registers
initial :: Registers
initial = M.empty


binOp :: Instruction -> Registers -> Either EvalError Registers
binOp (Binary B64 Add reg (Right imm)) regs =
  case M.lookup reg regs of
    Just val -> Right $ M.insert reg (val + (fromIntegral imm)) regs
    Nothing -> Left RegNotInit
binOp _ _ = undefined

evalPC :: EvalProg -> Either EvalError EvalProg
evalPC (EvalProg (LoadMapFd _ _:_) _) = undefined
evalPC (EvalProg (Unary _ _ _:_) _) = undefined
evalPC (EvalProg (Load _ _ _ _:_) _ ) = undefined
evalPC (EvalProg (Call _:_) _ ) = undefined
evalPC (EvalProg (Jmp _:_) _) = undefined
evalPC (EvalProg (JCond _ _ _ _:_) _) = undefined
evalPC (EvalProg (Store _ _ _ _:_) _) = undefined
evalPC (EvalProg (Load _ _ _ _:_) _) = undefined
evalPC (EvalProg (LoadImm reg imm:is) regs) =
        let imm' = fromIntegral imm
            regs' = M.insert reg imm' regs
        in Right (EvalProg is regs')
-- Anything else will be a Binary Op
evalPC (EvalProg (i:is) regs) =
  case binOp i regs of
    Right regs' -> Right (EvalProg is regs')
    Left err -> Left err
        
evalPC _ = undefined

eval' :: EvalProg -> Either EvalError Registers
eval' (EvalProg [] _) = Left NonTerminate
eval' (EvalProg (A.Exit:_) regs) = Right regs
eval' evalProg =
  case evalPC evalProg of
    Right ep -> eval' ep
    Left err -> Left err
  -- let (EvalProg rest regs') = evalPC evalProg
  -- in eval' (EvalProg rest regs')
      
eval :: A.Program -> Either EvalError Registers
eval prog = eval' (EvalProg prog initial)
