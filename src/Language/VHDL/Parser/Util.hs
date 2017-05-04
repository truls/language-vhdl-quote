module Language.VHDL.Parser.Util
where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Data.Maybe (isJust)

import Language.VHDL.Syntax
import Language.VHDL.Lexer

isReserved :: String -> Parser Bool
isReserved a = isJust <$> optionMaybe (reserved a)

block :: String -> Parser a -> Parser a
block s p = reserved s >> p <* (reserved "end" *> optional (reserved s))
            <* optional simpleName <* semi


stmLabel :: (Maybe Label -> Parser a) -> Parser a
stmLabel f = do
  label <- optionMaybe (label <* colon)
  f label
