module Language.VHDL.Parser
  ( parseFile
  , parseDesignFile
  , parseExp
  )
where

import Text.Parsec

import Language.VHDL.Syntax hiding (expression)
import Language.VHDL.Parser.Internal
import Language.VHDL.Parser.Monad

withStateParse :: ParseState -> Parser a -> SourceName -> String -> Either ParseError a
withStateParse u p sn s = runP p u sn s

stateParse :: Parser a -> SourceName -> String -> Either ParseError a
stateParse = withStateParse newParseState

parseFile :: FilePath -> IO (Either ParseError DesignFile)
parseFile fp =  do
  contents <- readFile fp
  return $ parseDesignFile fp contents

parseDesignFile :: FilePath -> String -> Either ParseError DesignFile
parseDesignFile fp s = stateParse designFile fp s

parseExp :: String -> Either ParseError Expression
parseExp s = stateParse expression "expression" s
