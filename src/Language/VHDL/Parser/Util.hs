module Language.VHDL.Parser.Util
  ( antiQ'
  , toQQString
  ) where

import           Control.Monad              (unless)
import           Data.Char                  (toLower)
import           Data.Data                  (Data, toConstr)
import           Data.List                  (stripPrefix)
import           Data.Maybe                 (fromJust)
import           Language.VHDL.Parser.Monad (Parser, quotesEnabled)
import           Language.VHDL.Syntax
import           Text.Parsec

toQQString
  :: (Data a)
  => a -> String
toQQString s = fromJust $ stripPrefix "anti" $ map toLower $ show . toConstr $ s

antiQ'
  :: (Data a)
  => Parser Identifier -> (String -> a) -> Parser a -> Parser a
antiQ' a q p = try parseQ <|> p
  where
    parseQ = do
      _ <- char '$'
      let qn = toQQString $ q ""
      qs <- string qn
      _ <- char ':'
      (Ident i) <- a
      qe <- quotesEnabled
      unless qe $ unexpected "QuasiQuotation syntax not emabled"
      unless (qs == qn) $
        unexpected $ "Wrong QuasiQuoter " ++ qn ++ " used in context"
      return $ q i
