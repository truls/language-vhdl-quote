{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import System.Exit
import System.Directory

import Control.Monad

import Data.String.Here
import Text.PrettyPrint
import Text.Show.Pretty

import Language.VHDL.Parser
import Language.VHDL.Pretty
import Language.VHDL.Syntax
import Text.Parsec


doParse :: FilePath -> IO (DesignFile, String)
doParse f = do
  res <- parseFile f
  ast <- case res of
          Right ast -> pure ast
          Left e -> print e >> exitFailure
  return (ast, render $ pp ast)

main :: IO ()
main = do
  arg <- getArgs >>= \case
    [a] -> pure a
    _ -> putStrLn "Usage: dumpast <VHDL file>" >> exitFailure
  doesFileExist arg >>= flip unless (fail $ "File not found " ++ arg)

  (res, str) <- doParse arg

  putStrLn "AST tree dump"
  putStrLn $ ppShow res
  putStrLn ""
  putStrLn "Pretty printed source"
  putStrLn str
