module Main (main) where

import Ebpf.Asm as A
import Ebpf.AsmParser
import qualified Ebpf.Encode as E

import WPVCGen
import ExamplePrograms
import Options.Applicative
import System.Process

data Tool = VCGen
          | Default
          | GetProof
  deriving Show
          
data Options = Options { tool :: Tool
                       , infile :: Maybe FilePath
                       , outfile :: Maybe FilePath
                       } deriving Show

options :: ParserInfo Options
options = info (opts <**> helper)
          (fullDesc
           <> progDesc "Verification Condition Generator for (FeatherWeight)-eBPF bytecode")
  where
    opts = Options
      <$> tool
      <*> infile --argument str (metavar "INFILE")
      <*> outfile
    tool = flag' VCGen (long "vcgen"
                        <> short 'g'
                        <> help "parse asm file, generate VC and write to output")
           <|>
           flag' Default (long "default"
                          <> short 'd'
                          <> help "generate VC for the default programs" )
           <|>
           flag' GetProof (long "get-proof"
                           <> short 'p'
                           <> help "Parse asm file, generate VC and call cvc5-Linux to obtain a proof in lfsc format. Requires cvc5-Linux to be installed.")
    infile = optional $ strOption (long "input"
                                   <> short 'i'
                                   <> metavar "INFILE"
                                   <> help "The input assembly file to read" )
    outfile = optional $ strOption (long "output"
                                   <> short 'o'
                                   <> metavar "OUTFILE"
                                   <> help "Write output to OUTFILE (writes to stdout if not given)" )
             

run :: Program -> IO ()
run p =
  do
    let predicate = withInitialPre p
    putStrLn $ "Program: " ++ show p
    putStrLn $ "Predicate: " ++ show predicate
    putStrLn $ "prettified predicate: " ++ pp predicate
    putStrLn "As smtlib2:"
    putStrLn $ with_smt_lib_wrapping $ pp_smt predicate

main :: IO ()
main =
  do
    Options tool infile outfile <- execParser options
    case tool of
      VCGen -> do
        case infile of
          Nothing -> error "No inputfile to parse"
          Just file -> do
            res <- parseFromFile file
            case res of
              Left err -> print err
              Right prog ->
                let predicate = withInitialPre prog
                    spred = with_smt_lib_wrapping $ pp_smt predicate
                in
                  case outfile of
                    Nothing -> putStrLn spred
                    Just out -> writeFile out spred

      -- This is a bad and very hacky way to call cvc5
      -- Basically just a wrapper for a bash command that will only work on linux
      -- and only if cvc5-Linux is in PATH
      GetProof -> do
        case infile of
          Nothing -> error "No inputfile to parse"
          Just file -> do
            res <- parseFromFile file
            case res of
              Left err -> print err
              Right prog ->
                let predicate = withInitialPre prog
                    spred = with_smt_lib_wrapping $ pp_smt predicate
                in
                  case outfile of
                    Nothing -> error "No output filename"
                    Just out -> do
                      writeFile out spred
                      callCommand $ "cvc5-Linux " ++ out ++ " | tail +2 > " ++ (out ++ ".plf")

      Default -> do
        run testProgRegDiv
        run testProgOnlyExit
        run testProgTwoMov
        run testProgOverWriteMovMultiple
        run testProgOverWriteMovAfterDiv
        run testProgDivSeries
        run testProgRegDivR1Noninit
        run testProgJeq
        run testProgDivImm
        run testProgAddMulDiv
        run testProgXorDiv
        run testProgXorInitDiv
      _ -> error "Not implemented yet"
