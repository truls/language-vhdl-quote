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

parseAntiExpr :: Parser String
parseAntiExpr = first
  where
    first = do
      c <- char '('
      cs <- rest 1
      return (c : cs)
    rest :: Int -> Parser String
    rest 0 = return []
    rest nest = do
      c <- anyChar
      case c of
        -- FIXME: Make this more roboust
        '\\' -> do
          c2 <- anyChar
          cs <- rest nest
          return (c : c2 : cs)
        '(' -> do
          cs <- rest (nest + 1)
          return (c : cs)
        ')' -> do
          cs <- rest (nest - 1)
          return (c : cs)
        _ -> do
          cs <- rest nest
          return (c : cs)

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
      identOrExpr <- optionMaybe $ lookAhead (char '(')
      i <-
        case identOrExpr of
          Just _ -> parseAntiExpr
          Nothing -> do
            (Ident i') <- a
            return i'
      qe <- quotesEnabled
      unless qe $ unexpected "QuasiQuotation syntax not emabled"
      unless (qs == qn) $
        unexpected $ "Wrong QuasiQuoter " ++ qn ++ " used in context"
      return $ q i
