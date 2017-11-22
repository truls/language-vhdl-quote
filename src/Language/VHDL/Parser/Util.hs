module Language.VHDL.Parser.Util
  ( toQQString
  ) where

import           Data.Char  (toLower)
import           Data.Data  (Data, toConstr)
import           Data.List  (stripPrefix)
import           Data.Maybe (fromJust)

toQQString
  :: (Data a)
  => a -> String
toQQString s = fromJust $ stripPrefix "anti" $ map toLower $ show . toConstr $ s
