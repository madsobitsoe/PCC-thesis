module VCGen
where

--    ( execute, withDefaultOptions, Judgment(..), judgmentToConstantDeclarations, judgmentToAssertion, withDefaults)
-- where

-- import Ebpf.Asm as A
-- import Ebpf.Decode as D
-- import SMTLib2 as S
-- import SMTLib2.Core as SC
-- import qualified SMTLib2.BitVector as SBV
-- import Data.Maybe
-- import Data.Word
-- import qualified Data.Map.Strict as M

-- import Definitions
-- import Util



-- import Control.Monad
-- import Control.Monad.Trans.State.Lazy
-- import Control.Monad.Trans.Reader
-- import Control.Monad.Except
-- import Control.Monad.Trans



-- -- Adds the default header of options to the smtlib2 output
-- withDefaultOptions (S.Script cmds) =
--   S.Script $ [ CmdSetLogic (N "BV")
--            , CmdSetOption $ OptProduceProofs True
--            , CmdSetOption $ OptAttr (Attr (N "proof-format-mode lfsc") Nothing)
--            , CmdSetOption $ OptAttr (Attr (N "dump-proofs true") Nothing)] ++ cmds



-- withDefaultPostamble (S.Script cmds) =
--   S.Script $ cmds ++ [ CmdCheckSat, CmdExit ]

-- withDefaults = withDefaultOptions . withDefaultPostamble




-- --declFunR0Init = CmdDeclareFun (N "r0_init") [] (SBV.tBitVec 64)
-- declFun64 name = CmdDeclareFun (N name) [] (SBV.tBitVec 64)
-- funRef name = App (I (N name) []) Nothing []


-- judgmentToConstantDeclarations :: Judgment -> S.Script
-- judgmentToConstantDeclarations (Judgment _ _ cs) =
--   S.Script $ reverse . (map declFun64) $ concat $ M.elems cs


-- sexprToScript :: SExpr -> S.Expr
-- sexprToScript (SImm w) = SBV.bv (fromIntegral w) 64
-- sexprToScript (SVar name) = funRef name
-- -- sexprToScript (SAdd s1 s2) =
-- --   let s1' = sexprToScript s1
-- --       s2' = sexprToScript s2
-- --   in
-- --     SBV.bvadd s1' s2'

-- -- sexprToScript (SSub s1 s2) =
-- --   let s1' = sexprToScript s1
-- --       s2' = sexprToScript s2
-- --   in
-- --     SBV.bvsub s1' s2'

-- sexprToScript (SDiv s1 s2) =
--   let s1' = sexprToScript s1
--       s2' = sexprToScript s2
--   in
--     SBV.bvudiv s1' s2'
  
-- -- sexprToScript (SXor s1 s2) =
-- --   let s1' = sexprToScript s1
-- --       s2' = sexprToScript s2
-- --   in
-- --     SBV.bvxor s1' s2'

-- psExprToScript :: PSExpr -> S.Expr
-- psExprToScript PTrue = SC.true
-- -- psExprToScript PFalse = SC.false
-- psExprToScript (Eq s1 s2) =
--   let s1' = sexprToScript s1
--       s2' = sexprToScript s2
--   in
--     (SC.===) s1' s2'

-- psExprToScript (Neq s1 s2) =
--   SC.not $ psExprToScript (Eq s1 s2)

-- psExprToScript (Geq s1 s2) =
--   let s1' = sexprToScript s1
--       s2' = sexprToScript s2
--   in
--     SBV.bvuge s1' s2'
-- -- psExprToScript (Lt s1 s2) =
-- --   let s1' = sexprToScript s1
-- --       s2' = sexprToScript s2
-- --   in
-- --     SBV.bvult s1' s2'

-- predToExpr :: Pred -> S.Expr
-- predToExpr (PS psexpr) = psExprToScript psexpr
-- -- predToExpr (Not pred) = SC.not $ predToExpr pred
-- predToExpr (LAnd p1 p2) =
--   let p1' = predToExpr p1
--       p2' = predToExpr p2
--   in
--     SC.and p1' p2'
-- predToExpr (Impl p1 p2) =
--   let p1' = predToExpr p1
--       p2' = predToExpr p2
--   in
--     (SC.==>) p1' p2'

-- judgmentToAssertion :: Judgment -> S.Script
-- judgmentToAssertion (Judgment p _ _) = S.Script [S.CmdAssert (SC.not $ predToExpr p)] 




-- -- Monad stuff

-- runVCGenComp :: VCGenComp a -> LineNoProgram -> Judgment -> Either VCGenError (a, Judgment)
-- runVCGenComp x p e = runExcept $ flip runStateT e $ runReaderT x p



-- abort :: VCGenError -> VCGenComp a
-- abort err = lift . lift $ throwError err


-- lookRegister :: Register -> VCGenComp SExpr
-- lookRegister reg = do
--   (Judgment _ ms _) <- lift get
--   case M.lookup reg ms of
--     Nothing -> abort $ "Reg " ++ show reg ++ " not in machine state"
--     Just x -> return x

-- lookConstants :: Register -> VCGenComp [VName]
-- lookConstants reg = do
--   (Judgment _ _ cs) <- lift get
--   case M.lookup reg cs of
--     Nothing -> abort "INTERNAL ERROR: Register not present in constants"
--     Just consts -> return consts


-- lookPred :: VCGenComp Pred
-- lookPred = do
--   (Judgment pred _ _) <- lift get
--   return pred

-- newConstant :: Int -> Register -> VName
-- newConstant lineno reg = show reg ++ "_" ++ show lineno

-- symbolicEvalInstruction :: A.Instruction -> VCGenComp SExpr
-- symbolicEvalInstruction (A.Binary A.B64 A.Mov _ (Right imm)) =
--   return $ SImm $ fromIntegral imm

-- symbolicEvalInstruction (A.Binary A.B64 A.Mov _ (Left srcReg)) = do
--   sourceVal <- lookRegister (reg2reg srcReg)
--   return $ sourceVal

-- symbolicEvalInstruction _ = abort $ "Not yet implemented"


-- withBinding :: Register -> SExpr -> VCGenComp a -> VCGenComp a
-- withBinding reg sexp comp = do
--   lift $ modify (\(Judgment pred ms cs) -> Judgment pred (M.insert reg sexp ms) cs)
--   comp

-- -- withNewConstant :: Int -> Register -> VCGenComp a -> VCGenComp a
-- withNewConstant :: VName -> Register -> VCGenComp a -> VCGenComp a
-- withNewConstant constant reg comp = do
--   constants <- lookConstants reg
--   let newConstants = constant : constants
--   lift $ modify (\(Judgment pred ms cs) -> Judgment pred ms (M.insert reg newConstants cs))
--   comp

-- withNewPredicate :: Pred -> VCGenComp a -> VCGenComp a
-- withNewPredicate pred' comp = do
--   lift $ modify (\(Judgment pred ms cs) -> Judgment (Impl pred pred') ms cs)
--   comp
  

-- getInstruction :: Int -> VCGenComp A.Instruction
-- getInstruction n = do
--   instrs <- ask
--   case M.lookup n instrs of
--     Nothing -> abort $ "Instruction at pc=" ++ show n ++ " not in program"
--     Just instr -> return instr
    
-- -- Symbolic execution of a program starting at instruction n
-- exec :: Int -> VCGenComp ()
-- exec n = do
--   (Judgment pred ms cs) <- lift get
--   instr <- getInstruction n
--   case instr of
--     -- ProgExit
--     A.Exit -> do
--       withNewPredicate (PS PTrue) $ return ()
--     -- ProgSeqDiv
--     (Binary B64 Div rd (Left rs)) -> do
--       abort $ "Div not implemented yet"
--       -- rs' <- symbolicEvalInstruction instr
      
--     -- ProgSeq
--     (Binary B64 Mov rd (Right imm)) -> do
--       imm' <- symbolicEvalInstruction instr
--       let rd' = reg2reg rd
--           newConst = newConstant n rd'
--           newPred = PS (Eq (SVar newConst) imm')
--         in withBinding rd' imm' $ withNewConstant newConst rd' $ withNewPredicate newPred $ exec (n+1)
--     (Binary B64 Mov rd (Left rs)) -> do
--       rs' <- symbolicEvalInstruction instr
--       let rd' = reg2reg rd
--           newConst = newConstant n rd'
--           newPred = PS (Eq (SVar newConst) rs') 
--         in withBinding rd' rs' $ withNewConstant newConst rd' $ withNewPredicate newPred $ exec (n+1)
      
--     _ -> do
--       abort $ show instr ++ " not implemented."
--       -- sexpr <- symbolicEvalInstruction instr
--       -- withBinding R0 sexpr $ exec (n+1)

-- execute :: A.Program -> Either VCGenError Judgment
-- execute prog =  do
--   (_,judgment) <- runVCGenComp (exec 1) (numberInstrs prog) initialJudgment
--   return judgment
