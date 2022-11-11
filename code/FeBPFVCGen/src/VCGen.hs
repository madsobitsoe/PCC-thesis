module VCGen
    ( genVC, withDefaultOptions, Judgment(..), judgmentToConstantDeclarations, judgmentToAssertion, withDefaults
    ) where

import Ebpf.Asm as A
import Ebpf.Decode as D
import SMTLib2 as S
import SMTLib2.Core as SC
import qualified SMTLib2.BitVector as SBV
import Data.Maybe
import Data.Word
import qualified Data.Map.Strict as M


type VName = String

data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
  deriving (Ord, Show, Eq)
data Uninit = U0 | U3 | U4 | U5 | U6 | U7 | U8 | U9
  deriving (Ord, Show, Eq)
  
data SExpr =
    SImm Word64
  | SVar VName
  | SUnVar Uninit
  | SAdd SExpr SExpr
  | SSub SExpr SExpr
  | SDiv SExpr SExpr
  | SXor SExpr SExpr
  deriving (Show)

data PSExpr =
    PTrue
  | PFalse
  | Eq SExpr SExpr
  | Neq SExpr SExpr
  | Geq SExpr SExpr
  | Lt SExpr SExpr
  -- | Init SExpr
  deriving (Show)

data Pred =
    PS PSExpr
  | Not Pred 
  | LAnd Pred Pred
  | Impl Pred Pred
  deriving (Show)
  -- | FAll 


-- Add linenumbers to each instruction in a program
numberInstrs = zip [1..]



type VC = S.Script

type VCGenError = String
type MachineState = M.Map Register SExpr

type Constants = M.Map Register [VName]

-- initialConstants = M.fromList [ (R0, ["r0_0"])
--                               , (R1, ["m"])
--                               , (R2, ["n"])
--                               , (R3, ["r3_0"])
--                               , (R4, ["r4_0"])
--                               , (R5, ["r5_0"])
--                               , (R6, ["r6_0"])
--                               , (R7, ["r7_0"])
--                               , (R8, ["r8_0"])
--                               , (R9, ["r9_0"])
--                               , (R10,["fp"])
--                               ]

initialConstants = M.fromList [ (R0, ["R0_init"])
                              , (R1, ["m"])
                              , (R2, ["n"])
                              , (R3, ["R3_init"])
                              , (R4, ["R4_init"])
                              , (R5, ["R5_init"])
                              , (R6, ["R6_init"])
                              , (R7, ["R7_init"])
                              , (R8, ["R8_init"])
                              , (R9, ["R9_init"])
                              , (R10,["fp"])
                              ]


initialMachineState = M.fromList [ (R0, SUnVar U0)
                                 , (R1, SVar "m")
                                 , (R2, SVar "n")
                                 , (R3, SUnVar U3)
                                 , (R4, SUnVar U4)
                                 , (R5, SUnVar U5)
                                 , (R6, SUnVar U6)
                                 , (R7, SUnVar U7)
                                 , (R8, SUnVar U8)
                                 , (R9, SUnVar U9)
                                 , (R10, SVar "fp")
                                 ]

-- Helper function to convert between ebpf-tools registers and VCGen registers
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

data Judgment = Judgment Pred MachineState Constants
  deriving (Show)

genVC :: A.Program -> Either VCGenError Judgment
genVC prog = innerGenVC initialJudgment (numberInstrs prog)



-- Given two predicates where the first is specifically an implication,
-- Chain them such that the conclusion of the implication implies the new term
addImpl :: Pred -> Pred -> Pred
-- If we have true => P as the first term, and Q as second term, make it P => Q
addImpl (Impl (PS PTrue) p) q = Impl p q
-- If we have P => true as the first term and Q as the second term, make it P => Q
addImpl (Impl p (PS PTrue)) q = Impl p q
-- If we have P => (Q => R) and S, make it P => (Q => (R => S))
-- addImpl (Impl p (Impl q r)) s = Impl p (Impl q (addImpl r s))
-- If we have P -> Q as first term and R as second, make it P -> (Q -> R) by calling recursively
addImpl (Impl p q) r = Impl p (addImpl q r)
addImpl p q = Impl p q
addImpl _ _ = undefined

innerGenVC :: Judgment -> [LineNoInstruction] -> Either VCGenError Judgment
innerGenVC judgment [] = Left "Invalid program: Program was empty before exit"
innerGenVC judgment [(_,A.Exit)] = genVC' judgment (0, A.Exit)
innerGenVC judgment (i:is) =
  case genVC' judgment i of
    Left err -> Left err
    Right judgment' -> innerGenVC judgment' is
  

initialJudgment = Judgment (Impl (PS (Geq (SVar "n") (SImm 1))) (PS PTrue)) initialMachineState initialConstants


type LineNoInstruction = (Int, A.Instruction)


genVC' :: Judgment -> LineNoInstruction -> Either VCGenError Judgment
genVC' (Judgment vc ms cs) (lineno, A.Exit) =
  case M.lookup R0 ms of
    Nothing -> Left "INTERNAL ERROR: R0 not in map"
    -- Just (SUnVar _) ->  -- Left "Invalid Program: R0 not initialized at exit"      
    Just _ ->
      let vc' = addImpl vc (PS (Geq (SVar "R0_init") (SImm 1)))
      in Right (Judgment vc' ms cs)
      
genVC' (Judgment vc ms cs) (lineno, A.Binary A.B64 A.Mov (A.Reg d) (Left (A.Reg s)))  =
                         let rd = reg2reg (A.Reg d)
                             rs = reg2reg (A.Reg s)
                         in case M.lookup rs ms of
                           Nothing -> Left "Key not in map"
                           -- Source is not initialized, so "illegal" read
                           -- Just (SUnVar _) ->
                             
                           Just sexp ->
                             case M.lookup rd cs of
                               Nothing -> Left "INTERNAL ERROR"
                               Just consts ->
                                 let newConst = (show rd ++ "_" ++ show lineno)
                                     newConsts = M.insert rd (newConst:consts) cs
                                     -- We need to ensure source has been initialised before we read it
                                     initImpl1 = PS (Eq (SVar ("R" ++ show s ++ "_init")) (SImm 1))
                                     -- If we initialise r_d, we add an implication that rd_init = 1,
                                     -- before the "real" implication we want to add
                                     initImpl2 = PS (Eq (SVar ("R" ++ show d ++ "_init")) (SImm 1))
                                 in
                                   case M.lookup rd ms of
                                     Nothing -> Left "Internal error"
                                     Just (SUnVar _) -> Right $ Judgment (addImpl (addImpl (addImpl vc initImpl1) initImpl2) (PS (Eq (SVar newConst) sexp))) (M.insert rd sexp ms) newConsts
                                     Just _ ->  Right $ Judgment (addImpl vc (PS (Eq (SVar newConst) sexp))) (M.insert rd sexp ms) newConsts

genVC' (Judgment vc ms cs) (lineno, A.Binary A.B64 A.Mov (A.Reg d) (Right n))  =
  let rd = reg2reg (A.Reg d)
  in
    case M.lookup rd cs of
      Nothing -> Left "INTERNAL ERROR"
      Just consts ->
        let newConst = (show rd ++ "_" ++ show lineno)
            newConsts = M.insert rd (newConst:consts) cs 
            -- If we initialise r_d, we add an implication that rd_init = 1,
            -- before the "real" implication we want to add
            initImpl = PS (Eq (SVar ("R" ++ show d ++ "_init")) (SImm 1))
        in
          case M.lookup rd ms of
            Nothing -> Left "Internal error"
            Just (SUnVar _) -> Right $ Judgment (addImpl (addImpl vc initImpl) (PS (Eq (SVar newConst) (SImm (fromIntegral n))))) (M.insert (reg2reg (A.Reg d)) (SImm (fromIntegral n)) ms) newConsts 
            Just _ -> Right $ Judgment (addImpl vc (PS (Eq (SVar newConst) (SImm (fromIntegral n))))) (M.insert (reg2reg (A.Reg d)) (SImm (fromIntegral n)) ms) newConsts 

genVC' (Judgment vc ms cs) (lineno, A.Binary A.B64 A.Add (A.Reg d) (Right n))  =
  let rd = reg2reg (A.Reg d)
  in
    case M.lookup rd cs of
      Nothing -> Left "INTERNAL ERROR"
      Just consts ->
        let newConst = (show rd ++ "_" ++ show lineno)
            newConsts = M.insert rd (newConst:consts) cs
        in Right $ Judgment (addImpl vc (PS (Eq (SVar newConst) (SAdd (SVar (head consts)) (SImm (fromIntegral n)))))) (M.insert (reg2reg (A.Reg d)) (SAdd (SVar newConst) (SImm (fromIntegral n))) ms) newConsts

genVC' (Judgment vc ms cs) (lineno, A.Binary A.B64 A.Add (A.Reg d) (Left (Reg s)))  =
  let rd = reg2reg (A.Reg d)
      rs = reg2reg (A.Reg s)
  in case M.lookup rs ms of
    Nothing -> Left "Key not in map"
    Just sexp ->
      case M.lookup rs cs of
        Nothing -> Left "INTERNAL ERROR"
        Just srcConsts ->
          case M.lookup rd cs of
            Nothing -> Left "INTERNAL ERROR"
            Just dstConsts ->
              let newConst = (show rd ++ "_" ++ show lineno)
                  newConsts = M.insert rd (newConst:dstConsts) cs
              in Right $ Judgment (addImpl vc (PS (Eq (SVar newConst) (SAdd (SVar (head dstConsts)) (SVar (head srcConsts)))))) (M.insert (reg2reg (A.Reg d)) (SAdd (SVar newConst) (SVar (head srcConsts))) ms) newConsts


genVC' (Judgment vc ms cs) (lineno, A.Binary A.B64 A.Div (A.Reg d) (Left (Reg s)))  =
  let rd = reg2reg (A.Reg d)
      rs = reg2reg (A.Reg s)
  in case M.lookup rs ms of
    Nothing -> Left "Key not in map"
    Just sexp ->
      case M.lookup rs cs of
        Nothing -> Left "INTERNAL ERROR"
        Just srcConsts ->
          case M.lookup rd cs of
            Nothing -> Left "INTERNAL ERROR"
            Just dstConsts ->
              let newConst = (show rd ++ "_" ++ show lineno)
                  newConsts = M.insert rd (newConst:dstConsts) cs
                  notZeroPred = PS (Neq (SVar (head srcConsts)) (SImm 0))
                  postDivPred = PS (Eq (SVar newConst) (SDiv (SVar (head dstConsts)) (SVar (head srcConsts))))
              in
                Right $ Judgment (addImpl vc (addImpl notZeroPred postDivPred)) (M.insert rd (SDiv (SVar (head dstConsts)) (SVar (head srcConsts))) ms) newConsts
                
genVC' _ _ = Left "Not implemented yet"



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
sexprToScript (SUnVar uninitReg) = undefined
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
psExprToScript PTrue = SC.true --undefined
psExprToScript PFalse = SC.false -- undefined
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
