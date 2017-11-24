module Main
  ( main
  ) where

import           System.Exit    (exitWith)
import           System.Process (system)

main :: IO ()
main = system "make -C test/vests test" >>= exitWith
