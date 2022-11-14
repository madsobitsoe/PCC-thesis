module Main (main) where

import VCGen
-- import AbstractMachine
import Ebpf.Asm
import SMTLib2 as S
testProg = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
           , Binary B64 Mov (Reg 3) (Left (Reg 2))
           , Binary B64 Mov (Reg 4) (Left (Reg 2))
           , Binary B64 Mov (Reg 5) (Left (Reg 2))
           , Binary B64 Mov (Reg 6) (Left (Reg 2))
           , Binary B64 Mov (Reg 7) (Left (Reg 2))
           , Binary B64 Mov (Reg 8) (Left (Reg 2))
                                                       
           -- ,Binary B64 Mov (Reg 1) (Right 0)
           -- ,Binary B64 Mov (Reg 2) (Right 0)
           -- ,Binary B64 Mov (Reg 3) (Right 0)
           -- ,Binary B64 Add (Reg 0) (Right 19)
           ,Exit]

testProgOnlyExit = [ Exit ]


testProgOneRegMov = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
                 , Exit ]

testProgOneImmMov = [ Binary B64 Mov (Reg 0) (Right 42)
                 , Exit ]

testProgTwoMov = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
                 , Binary B64 Mov (Reg 3) (Right 42)
                 , Exit ]
testProgMovAndAdd = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
                    , Binary B64 Add (Reg 0) (Right 42)
                    , Exit ]

testProgRegAdd = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
                 , Binary B64 Add (Reg 0) (Left (Reg 2))
                 , Exit ]


testProgRegDiv = [ Binary B64 Mov (Reg 0) (Right 10)
                 , Binary B64 Mov (Reg 1) (Left (Reg 2))
                 , Binary B64 Div (Reg 0) (Left (Reg 1))
                 , Exit ]



-- run :: Program -> IO ()
-- run p = do
--   res <- execute p
--   case res of
--     Left e -> putStrLn("*** Err: " ++ show e)
--     Right (Judgment pc pred ms cs) ->
--       putStrLn "Sweet baby jesus"


main :: IO ()
main =
  do
    -- res <- execute testProgOnlyExit
    case execute testProgOneImmMov of
      Left e -> putStrLn("*** Err: " ++ show e)
      Right (Judgment pred ms cs) ->
        do
          putStrLn "Program:"
          print testProgOneImmMov
          putStrLn "Preds:"
          print pred
          putStrLn "Machine State:"
          print ms
          putStrLn "Constants:"
          print cs
        

    -- case genVC testProgRegDiv of
    --   Left err -> print err
    --   Right vc -> do
    --       let (Script cs) = judgmentToConstantDeclarations vc
    --           (Script vc') = judgmentToAssertion vc 
    --         in print $ pp $ withDefaults (Script $ cs ++ vc')

    -- case genVC testProgOnlyExit of
    --   Left err -> print err
    --   Right vc -> do
    --       let (Script cs) = judgmentToConstantDeclarations vc
    --           (Script vc') = judgmentToAssertion vc 
    --         in print $ pp $ withDefaults (Script $ cs ++ vc')




