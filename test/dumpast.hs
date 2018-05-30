{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           Text.Show.Pretty

import           Language.VHDL.Parser
import           Language.VHDL.Pretty
import           Language.VHDL.Syntax

data ArgFlag = PrintAst
             | PrintPretty
             | Verbose
             | Help
  deriving (Eq, Ord, Show)

options :: [OptDescr ArgFlag]
options = [ Option "a" ["print-ast"] (NoArg PrintAst)
             "Print AST"
          , Option "p" ["print-pretty"] (NoArg PrintPretty)
            "Print pretty printed code"
          , Option "v" ["verbose"] (NoArg Verbose)
            "Be verbose"
          , Option "h" ["help"] (NoArg Help)
            "Prints this message"
          ]

parseOptions :: [String] -> IO ([ArgFlag], [String])
parseOptions argv = case getOpt Permute options argv of
  (o, n, []) -> return (o, n)
  (_,_,errs) -> ioError $ argFail errs

usage :: String
usage = usageInfo header options
  where header = "Usage: dumpast [-apvh] FILE"

argFail :: [String] -> IOError
argFail errs = userError (concat errs ++ usage)

doParse :: FilePath -> IO (DesignFile, T.Text)
doParse f = do
  ast <- parseFile f >>= \case
    Right ast -> pure ast
    Left e -> putStrLn e >> exitFailure
  return (ast, pprrText ast)

printHelp :: IO ()
printHelp = putStrLn usage

main :: IO ()
main = do
  (args, files) <- getArgs >>= parseOptions
  when (Help `elem` args) (printHelp >> exitFailure)

  [file] <- if length files /= 1
            then ioError $ argFail ["FILE argument missing\n\n"]
            else pure files

  doesFileExist file >>= flip unless (fail $ "File not found " ++ file)

  (res, str) <- doParse file

  let verbose = Verbose `elem` args

  when (PrintAst `elem` args) $ do
    when verbose $ putStrLn "AST tree dump"
    putStrLn $ ppShow res
    when verbose $ putStrLn ""

  when (PrintPretty `elem` args) $ do
    when verbose $ putStrLn "Pretty printed source"
    TIO.putStrLn str
