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
withStateParse u p = runP p u

stateParse :: Parser a -> SourceName -> String -> Either ParseError a
stateParse = withStateParse newParseState

parseFile :: FilePath -> IO (Either ParseError DesignFile)
parseFile fp =  do
  contents <- readFile fp
  return $ parseDesignFile fp contents

parseDesignFile :: FilePath -> String -> Either ParseError DesignFile
parseDesignFile = stateParse designFile

parseExp :: String -> Either ParseError Expression
parseExp = stateParse expression "expression"
