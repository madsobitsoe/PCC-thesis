module Main (main) where

import VCGen
import AbstractMachine
main :: IO ()
main =
  do
    let res = genVC []
        progRes = eval []
    putStrLn "Woop woop"
