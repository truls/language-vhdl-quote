{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import           Control.Monad.Extra  (allM, concatMapM, unless)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           System.Directory     (copyFile, createDirectoryIfMissing,
                                       doesDirectoryExist)
import           System.Exit          (ExitCode (..), exitFailure, exitWith)
import           System.FilePath.Find
import           System.Process       (system)

import           Language.VHDL.Parser
import           Language.VHDL.Pretty


replaceDir :: FilePath -> FilePath
replaceDir =
  let f = T.pack "vhdl-93"
      t = T.pack "vhdl-93-transformed"
  in T.unpack . T.replace f t . T.pack

transform :: FilePath -> IO ()
transform fp = do
  ast <-
    parseFile fp >>= \case
      Right ast -> pure ast
      Left e -> putStrLn e >> exitFailure
  putStrLn $ "Transforming " ++ fp
  T.writeFile (replaceDir fp) (pprrText ast)

basePath :: FilePath
basePath = "test/vests"

testDirs :: [FilePath]
testDirs = map ((basePath ++ "/") ++) dirs
  where
    dirs =
      [ "vhdl-93/ashenden/compliant"
      , "vhdl-93/billowitch/compliant"
      , "vhdl-93/clifton-labs/compliant"
      ]

recurse :: FilterPredicate -> FilePath -> IO [FilePath]
recurse = find always

mkTree :: IO ()
mkTree = do
  dirs <-
    map replaceDir <$> concatMapM (recurse (fileType ==? Directory)) testDirs
  mapM_ (createDirectoryIfMissing True) dirs

main :: IO ()
main = do
  res <- system "make -C test/vests installghdl vhdl-93"
  unless (res == ExitSuccess) $ exitWith res

  exists <- allM doesDirectoryExist testDirs
  unless exists $ do
    putStrLn "Vests test suite not found"
    exitFailure

  files <-
    concatMapM
      (recurse (extension ==? ".vhd" ||? extension ==? ".vhdl"))
      testDirs
  expFiles <-
    concatMapM (recurse (extension ==? ".exp")) testDirs
  mkTree
  mapM_ (\f -> copyFile f (replaceDir f)) expFiles
  mapM_ transform files

  system "make -C test/vests test" >>= exitWith
