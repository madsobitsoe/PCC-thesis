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

testProgOneMov = [ Binary B64 Mov (Reg 0) (Left (Reg 2))
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


main :: IO ()
main =
  do
    -- case genVC testProgOnlyExit of
    --   Left err -> print err
    --   Right res -> print res
    -- case genVC testProg of
    --   Left err -> print err
    --   Right res -> print res
    -- putStrLn "One mov case:"
    -- case genVC testProgOneMov of
    --   Left err -> print err
    --   Right res -> print res
    -- putStrLn "Two mov case:"
    -- case genVC testProgTwoMov of
    --   Left err -> print err
    --   Right res -> print res
    -- putStrLn "Mov and Add case:"      
    -- case genVC testProgMovAndAdd of
    --   Left err -> print err
    --   Right (Judgment vc ms cs) -> do
    --     putStrLn "Constants:"
    --     print cs
    --     putStrLn "VC:"
    --     print vc

    case genVC testProgRegDiv of
      Left err -> print err
      Right vc -> do
          let (Script cs) = judgmentToConstantDeclarations vc
              (Script vc') = judgmentToAssertion vc 
            in print $ pp $ withDefaults (Script $ cs ++ vc')

    case genVC testProgOnlyExit of
      Left err -> print err
      Right vc -> do
          let (Script cs) = judgmentToConstantDeclarations vc
              (Script vc') = judgmentToAssertion vc 
            in print $ pp $ withDefaults (Script $ cs ++ vc')




