-- {-# LANGUAGE unsafe #-}
module Main where

import System.FilePath.Posix
import System.IO
--import System.IO.Temp
import System.IO.Unsafe
import System.Directory
import Control.Monad

--import Data.IORef
--import Data.IORef.MonadIO

import Debug.Trace

import Text.Parsec.Error
import Test.Tasty
-- import Test.Tasty.Hspec
import Test.Tasty.Program
import Text.PrettyPrint

-- import Language.VHDL.Syntax
import Language.VHDL.Parser
import Language.VHDL.Pretty

data TestType = Build | Run | Check

type Entity = String

main :: IO ()
main = defaultMain tests

transformFile :: FilePath -> IO FilePath
transformFile f = do
  pwd <- getCurrentDirectory
  let fp = pwd </> f
  exists <- doesFileExist $ fp
  unless exists $ fail $ "file jnot found " ++ fp
  let basename = takeBaseName fp
  res <- parseFile $ fp

  ast <- case res of
    Right ast -> pure ast
    Left err -> do
      putStr $ show err
      fail $ "foo" ++ (concat $ map messageString $ errorMessages err)

  tmp <- getTemporaryDirectory
  (nfp, handle) <- openTempFile tmp basename

  let prettyast = render $ pp ast
  putStr $ "Pretty AST: " ++ prettyast
  hPutStr handle $ prettyast
  hClose handle
  return nfp

compCase :: FilePath -> (IO FilePath -> TestTree) -> TestTree
compCase fp tt = withResource (transformFile fp) removeFile tt

{-# NOINLINE defTest #-}
defTest :: Maybe Entity -> IO FilePath -> TestTree
defTest e iofp =
  let
    fp = unsafePerformIO iofp
    analyze = testProgram "Test file analyze" "ghdl" ["-a",  "--std=93c", fp ] Nothing
    elabrun ent = testProgram "Test file elaborate" "ghdl"
                  ["--elab-run", ent, "--assert-level=error"] Nothing
  in
    case e of
      Just ent -> testGroup "Elaboration" $  [ analyze, elabrun ent ]
      Nothing -> analyze

defBuildTest :: FilePath -> Maybe Entity -> TestTree
defBuildTest fp s = compCase ("test" </> "vests" </> "vhdl-93"
                              </> "ashenden" </> "compliant" </> fp) $ defTest s

tests :: TestTree
tests =  testGroup "tests" $
         [ defBuildTest "ch_01_fg_01_07.vhd" Nothing
         , defBuildTest "ch_01_fg_01_08.vhd" Nothing
         , defBuildTest "ch_01_fg_01_10.vhd" Nothing
         , defBuildTest "ch_01_fg_01_11.vhd" Nothing
         , defBuildTest "ch_01_fg_01_13.vhd" Nothing
         , defBuildTest "ch_01_tb_01_01.vhd" Nothing
         , defBuildTest "ch_01_tb_01_02.vhd" Nothing
         , defBuildTest "ch_01_tb_01_03.vhd" Nothing
         ]
