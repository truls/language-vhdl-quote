module Language.VHDL.Parser
  ( parseFile
  , parseDesignFile
  , quoteParse
  , parseExpr
  , parseSeqStm
  , parseSeqStms
  , parseConStm
  , parseSeqStm
  , parseName
  , Result
  )
where

import           Text.Parsec

import           Language.VHDL.Parser.Internal
import           Language.VHDL.Parser.Monad
import           Language.VHDL.Syntax

type Result a = Either ParseError a

withStateParse :: ParseState -> Parser a -> SourceName -> String -> Result a
withStateParse u p = runP p u

stateParse :: Parser a -> SourceName -> String -> Result a
stateParse = withStateParse $ newParseState False

quoteParse :: Parser a -> (String, Int, Int) -> String -> Result a
quoteParse p (f, r, c) = withStateParse (newParseState True) (updatePosition f r c >> p) ""

parseFile :: FilePath -> IO (Result DesignFile)
parseFile fp = do
  contents <- readFile fp
  return $ parseDesignFile fp contents

parseDesignFile :: FilePath -> String -> Result DesignFile
parseDesignFile = stateParse designFile

parseExpr :: (String, Int, Int) -> String -> Result Expression
parseExpr = quoteParse expression

parseSeqStm :: (String, Int, Int) -> String -> Result SequentialStatement
parseSeqStm = quoteParse sequentialStatement

parseSeqStms :: (String, Int, Int) -> String -> Result [SequentialStatement]
parseSeqStms = quoteParse sequenceOfStatements

parseConStm :: (String, Int, Int) -> String -> Result ConcurrentStatement
parseConStm = quoteParse concurrentStatement

parseConStms :: (String, Int, Int) -> String -> Result [ConcurrentStatement]
parseConStms = quoteParse concurrentStatements

parseName :: (String, Int, Int) -> String -> Result Name
parseName = quoteParse name

updatePosition :: String -> Int -> Int -> Parser ()
updatePosition file line col = do
   pos <- getPosition
   setPosition $
     flip setSourceName file $
     flip setSourceLine line $
     setSourceColumn pos col
