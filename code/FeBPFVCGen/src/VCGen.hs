module VCGen
   ( execute, withDefaultOptions, Judgment(..), judgmentToConstantDeclarations, judgmentToAssertion, withDefaults)
where

import Ebpf.Asm as A
import Ebpf.Decode as D
import SMTLib2 as S
import SMTLib2.Core as SC
import qualified SMTLib2.BitVector as SBV
import Data.Maybe
import Data.Word
import qualified Data.Map.Strict as M

import Definitions
import Util



import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Except
import Control.Monad.Trans

-- type VCGenComp a = ReaderT LineNoProgram (StateT Judgment (
-- >>>>>>> Stashed changes


-- genVC :: A.Program -> Either VCGenError Judgment
-- genVC prog = innerGenVC initialJudgment (Util.numberInstrs prog)




-- innerGenVC :: Judgment -> LineNoProgram -> Either VCGenError Judgment
-- innerGenVC _ _ = Left "Not implemented yet"
-- -- innerGenVC judgment [] = Left "Invalid program: Program was empty before exit"
-- -- innerGenVC judgment [(_,A.Exit)] = genVC' judgment (0, A.Exit)
-- -- innerGenVC judgment (i:is) =
-- --   case genVC' judgment i of
-- --     Left err -> Left err
-- --     Right judgment' -> innerGenVC judgment' is
  

-- <<<<<<< Updated upstream
-- =======




-- >>>>>>> Stashed changes

-- genVC' :: Judgment -> LineNoInstruction -> Either VCGenError Judgment
-- -- genVC' (Judgment vc ms cs) (lineno, A.Exit) =
-- --   case M.lookup R0 ms of
-- --     Nothing -> Left "INTERNAL ERROR: R0 not in map"
-- <<<<<<< Updated upstream
-- =======
-- --     -- Just (SUnVar _) ->  -- Left "Invalid Program: R0 not initialized at exit"      
-- >>>>>>> Stashed changes
-- --     Just _ ->
-- --       let vc' = addImpl vc (PS (Geq (SVar "R0_init") (SImm 1)))
-- --       in Right (Judgment vc' ms cs)
      
-- -- genVC' (Judgment vc ms cs) (lineno, A.Binary A.B64 A.Mov (A.Reg d) (Left (A.Reg s)))  =
-- --                          let rd = reg2reg (A.Reg d)
-- --                              rs = reg2reg (A.Reg s)
-- --                          in case M.lookup rs ms of
-- --                            Nothing -> Left "Key not in map"
-- --                            -- Source is not initialized, so "illegal" read
-- <<<<<<< Updated upstream
-- =======
-- --                            -- Just (SUnVar _) ->
                             
-- >>>>>>> Stashed changes
-- --                            Just sexp ->
-- --                              case M.lookup rd cs of
-- --                                Nothing -> Left "INTERNAL ERROR"
-- --                                Just consts ->
-- --                                  let newConst = (show rd ++ "_" ++ show lineno)
-- --                                      newConsts = M.insert rd (newConst:consts) cs
-- --                                      -- We need to ensure source has been initialised before we read it
-- --                                      initImpl1 = PS (Eq (SVar ("R" ++ show s ++ "_init")) (SImm 1))
-- --                                      -- If we initialise r_d, we add an implication that rd_init = 1,
-- --                                      -- before the "real" implication we want to add
-- --                                      initImpl2 = PS (Eq (SVar ("R" ++ show d ++ "_init")) (SImm 1))
-- --                                  in
-- --                                    case M.lookup rd ms of
-- --                                      Nothing -> Left "Internal error"
-- --                                      -- Just (SUnVar _) -> Right $ Judgment (addImpl (addImpl (addImpl vc initImpl1) initImpl2) (PS (Eq (SVar newConst) sexp))) (M.insert rd sexp ms) newConsts
-- --                                      Just _ ->  Right $ Judgment (addImpl vc (PS (Eq (SVar newConst) sexp))) (M.insert rd sexp ms) newConsts

-- -- genVC' (Judgment vc ms cs) (lineno, A.Binary A.B64 A.Mov (A.Reg d) (Right n))  =
-- --   let rd = reg2reg (A.Reg d)
-- --   in
-- --     case M.lookup rd cs of
-- --       Nothing -> Left "INTERNAL ERROR"
-- --       Just consts ->
-- --         let newConst = (show rd ++ "_" ++ show lineno)
-- --             newConsts = M.insert rd (newConst:consts) cs 
-- --             -- If we initialise r_d, we add an implication that rd_init = 1,
-- --             -- before the "real" implication we want to add
-- --             initImpl = PS (Eq (SVar ("R" ++ show d ++ "_init")) (SImm 1))
-- --         in
-- --           case M.lookup rd ms of
-- --             Nothing -> Left "Internal error"
-- --             -- Just (SUnVar _) -> Right $ Judgment (addImpl (addImpl vc initImpl) (PS (Eq (SVar newConst) (SImm (fromIntegral n))))) (M.insert (reg2reg (A.Reg d)) (SImm (fromIntegral n)) ms) newConsts 
-- --             Just _ -> Right $ Judgment (addImpl vc (PS (Eq (SVar newConst) (SImm (fromIntegral n))))) (M.insert (reg2reg (A.Reg d)) (SImm (fromIntegral n)) ms) newConsts 

-- -- genVC' (Judgment vc ms cs) (lineno, A.Binary A.B64 A.Add (A.Reg d) (Right n))  =
-- --   let rd = reg2reg (A.Reg d)
-- --   in
-- --     case M.lookup rd cs of
-- --       Nothing -> Left "INTERNAL ERROR"
-- --       Just consts ->
-- --         let newConst = (show rd ++ "_" ++ show lineno)
-- --             newConsts = M.insert rd (newConst:consts) cs
-- --         in Right $ Judgment (addImpl vc (PS (Eq (SVar newConst) (SAdd (SVar (head consts)) (SImm (fromIntegral n)))))) (M.insert (reg2reg (A.Reg d)) (SAdd (SVar newConst) (SImm (fromIntegral n))) ms) newConsts

-- -- genVC' (Judgment vc ms cs) (lineno, A.Binary A.B64 A.Add (A.Reg d) (Left (Reg s)))  =
-- --   let rd = reg2reg (A.Reg d)
-- --       rs = reg2reg (A.Reg s)
-- --   in case M.lookup rs ms of
-- --     Nothing -> Left "Key not in map"
-- --     Just sexp ->
-- --       case M.lookup rs cs of
-- --         Nothing -> Left "INTERNAL ERROR"
-- --         Just srcConsts ->
-- --           case M.lookup rd cs of
-- --             Nothing -> Left "INTERNAL ERROR"
-- --             Just dstConsts ->
-- --               let newConst = (show rd ++ "_" ++ show lineno)
-- --                   newConsts = M.insert rd (newConst:dstConsts) cs
-- --               in Right $ Judgment (addImpl vc (PS (Eq (SVar newConst) (SAdd (SVar (head dstConsts)) (SVar (head srcConsts)))))) (M.insert (reg2reg (A.Reg d)) (SAdd (SVar newConst) (SVar (head srcConsts))) ms) newConsts


-- -- genVC' (Judgment vc ms cs) (lineno, A.Binary A.B64 A.Div (A.Reg d) (Left (Reg s)))  =
-- --   let rd = reg2reg (A.Reg d)
-- --       rs = reg2reg (A.Reg s)
-- --   in case M.lookup rs ms of
-- --     Nothing -> Left "Key not in map"
-- --     Just sexp ->
-- --       case M.lookup rs cs of
-- --         Nothing -> Left "INTERNAL ERROR"
-- --         Just srcConsts ->
-- --           case M.lookup rd cs of
-- --             Nothing -> Left "INTERNAL ERROR"
-- --             Just dstConsts ->
-- --               let newConst = (show rd ++ "_" ++ show lineno)
-- --                   newConsts = M.insert rd (newConst:dstConsts) cs
-- --                   notZeroPred = PS (Neq (SVar (head srcConsts)) (SImm 0))
-- --                   postDivPred = PS (Eq (SVar newConst) (SDiv (SVar (head dstConsts)) (SVar (head srcConsts))))
-- --               in
-- --                 Right $ Judgment (addImpl vc (addImpl notZeroPred postDivPred)) (M.insert rd (SDiv (SVar (head dstConsts)) (SVar (head srcConsts))) ms) newConsts
                
--genVC' _ _ = Left "Not implemented yet"



-- Adds the default header of options to the smtlib2 output
withDefaultOptions (S.Script cmds) =
  S.Script $ [ CmdSetLogic (N "BV")
           , CmdSetOption $ OptProduceProofs True
           , CmdSetOption $ OptAttr (Attr (N "proof-format-mode lfsc") Nothing)
           , CmdSetOption $ OptAttr (Attr (N "dump-proofs true") Nothing)] ++ cmds



withDefaultPostamble (S.Script cmds) =
  S.Script $ cmds ++ [ CmdCheckSat, CmdExit ]

withDefaults = withDefaultOptions . withDefaultPostamble
-- getConstDecl :: (Int,A.Instruction)  -> [S.Command] -> [S.Command]
-- getConstDecl (line, Binary B32 _  (Reg regname) _) acc =  (declFun32 $ "r" ++ show regname ++ "_" ++ show line) : acc
-- getConstDecl (line, Binary B64 _  (Reg regname) _) acc =  (declFun64 $ "r" ++ show regname ++ "_" ++ show line) : acc
-- getConstDecl (line, Unary B32 _  (Reg regname)) acc =  (declFun32 $ "r" ++ show regname ++ "_" ++ show line) : acc
-- getConstDecl (line, Unary B64 _  (Reg regname)) acc =  (declFun64 $ "r" ++ show regname ++ "_" ++ show line) : acc

-- getConstDecl _ acc = acc

-- getConstDecls :: A.Program -> S.Script
-- getConstDecls = S.Script . foldr getConstDecl [] . numberInstrs





declFunR0Init = CmdDeclareFun (N "r0_init") [] (SBV.tBitVec 64)
-- declFun32 name = CmdDeclareFun (N name) [] (tBitVec 32)
declFun64 name = CmdDeclareFun (N name) [] (SBV.tBitVec 64)
funRef name = App (I (N name) []) Nothing []

-- -- "good_div.smt2" program
-- t = pp $ withDefaultOptions $ Script [ declFun64 "r0_1", declFun64 "r1_2", declFun64 "r0_3"
--                           -- Actual assertion below
--                         ,CmdAssert (SC.not $ (SC.==>) ((SC.===) (funRef "r0_1") (bv 10 64) ) ((SC.==>) ((SC.===) (funRef "r1_2") (bv 2 64)) (SC.not ((SC.===) (funRef "r1_2") (bv 0 64 )))))
--                         ,CmdCheckSat
--                         ,CmdExit
--                         ]



judgmentToConstantDeclarations :: Judgment -> S.Script
judgmentToConstantDeclarations (Judgment _ _ cs) =
  S.Script $ reverse . (map declFun64) $ concat $ M.elems cs


sexprToScript :: SExpr -> S.Expr
sexprToScript (SImm w) = SBV.bv (fromIntegral w) 64
sexprToScript (SVar name) = funRef name
-- sexprToScript (SUnVar uninitReg) = undefined
sexprToScript (SAdd s1 s2) =
  let s1' = sexprToScript s1
      s2' = sexprToScript s2
  in
    SBV.bvadd s1' s2'

sexprToScript (SSub s1 s2) =
  let s1' = sexprToScript s1
      s2' = sexprToScript s2
  in
    SBV.bvsub s1' s2'

sexprToScript (SDiv s1 s2) =
  let s1' = sexprToScript s1
      s2' = sexprToScript s2
  in
    SBV.bvudiv s1' s2'
  
sexprToScript (SXor s1 s2) =
  let s1' = sexprToScript s1
      s2' = sexprToScript s2
  in
    SBV.bvxor s1' s2'

psExprToScript :: PSExpr -> S.Expr
psExprToScript PTrue = SC.true
psExprToScript PFalse = SC.false
psExprToScript (Eq s1 s2) =
  let s1' = sexprToScript s1
      s2' = sexprToScript s2
  in
    (SC.===) s1' s2'

psExprToScript (Neq s1 s2) =
  SC.not $ psExprToScript (Eq s1 s2)

psExprToScript (Geq s1 s2) =
  let s1' = sexprToScript s1
      s2' = sexprToScript s2
  in
    SBV.bvuge s1' s2'
psExprToScript (Lt s1 s2) =
  let s1' = sexprToScript s1
      s2' = sexprToScript s2
  in
    SBV.bvult s1' s2'

predToExpr :: Pred -> S.Expr
predToExpr (PS psexpr) = psExprToScript psexpr
predToExpr (Not pred) = SC.not $ predToExpr pred
predToExpr (LAnd p1 p2) =
  let p1' = predToExpr p1
      p2' = predToExpr p2
  in
    SC.and p1' p2'
predToExpr (Impl p1 p2) =
  let p1' = predToExpr p1
      p2' = predToExpr p2
  in
    (SC.==>) p1' p2'

judgmentToAssertion :: Judgment -> S.Script
judgmentToAssertion (Judgment p _ _) = S.Script [S.CmdAssert (SC.not $ predToExpr p)] 




-- Monad stuff

runVCGenComp :: VCGenComp a -> LineNoProgram -> Judgment -> Either VCGenError (a, Judgment)
runVCGenComp x p e = runExcept $ flip runStateT e $ runReaderT x p



abort :: VCGenError -> VCGenComp a
abort err = lift . lift $ throwError err


lookRegister :: Register -> VCGenComp SExpr
lookRegister reg = do
  (Judgment _ ms _) <- lift get
  case M.lookup reg ms of
    Nothing -> abort $ "Reg " ++ show reg ++ " not in machine state"
    Just x -> return x

lookConstants :: Register -> VCGenComp [VName]
lookConstants reg = do
  (Judgment _ _ cs) <- lift get
  case M.lookup reg cs of
    Nothing -> abort "INTERNAL ERROR: Register not present in constants"
    Just consts -> return consts


lookPred :: VCGenComp Pred
lookPred = do
  (Judgment pred _ _) <- lift get
  return pred

symbolicEvalInstruction :: A.Instruction -> VCGenComp SExpr
symbolicEvalInstruction (A.Binary A.B64 A.Mov (A.Reg d) (Right imm)) =
  return $ SImm $ fromIntegral imm

symbolicEvalInstruction (A.Binary A.B64 A.Mov (A.Reg d) (Left srcReg)) =
  do
    sourceVal <- lookRegister (reg2reg srcReg)
    return $ sourceVal

symbolicEvalInstruction _ = abort $ "Not yet implemented"


withBinding :: Register -> SExpr -> VCGenComp a -> VCGenComp a
withBinding reg sexp comp = do
  lift $ modify (\(Judgment pred ms cs) -> Judgment pred (M.insert reg sexp ms) cs)
  comp

withNewConstant :: Int -> Register -> VCGenComp a -> VCGenComp a
withNewConstant lineno reg comp = do
  consts <- lookConstants reg
  let newConsts = (show reg ++ "_" ++ show lineno) : consts
  lift $ modify (\(Judgment pred ms cs) -> Judgment pred ms (M.insert reg newConsts cs))
  comp


getInstruction :: Int -> VCGenComp A.Instruction
getInstruction n = do
  instrs <- ask
  case M.lookup n instrs of
    Nothing -> abort $ "Instruction at pc=" ++ show n ++ " not in program"
    Just instr -> return instr
    
-- Symbolic execution of a program starting at instruction n
exec :: Int -> VCGenComp ()
exec n = do
  (Judgment pred ms cs) <- lift get
  instr <- getInstruction n
  case instr of
    A.Exit -> return ()
    _ -> do
      sexpr <- symbolicEvalInstruction instr
      withBinding R0 sexpr $ exec (n+1)
  -- instrs <- ask
  -- case M.lookup n instrs of
    -- Nothing -> abort $ "Instruction at pc=" ++ show n ++ " not in program"
    -- Just A.Exit -> return ()
    -- Just instr ->
    --   do
    --     sexpr <- symbolicEvalInstructions instr
    --     withBinding R0 sexpr $ exec (n+1) 
      -- abort $ "Instruction found: " ++ show instr

execute :: A.Program -> Either VCGenError Judgment
execute prog =  do
  (_,judgment) <- runVCGenComp (exec 1) (numberInstrs prog) initialJudgment
  return judgment
