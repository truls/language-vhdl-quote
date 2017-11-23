module Language.VHDL.Parser
  ( parseFile
  , parseDesignFile
  , parseDesignFileQ
  , parseDesignUnit
  , parseLibraryUnit
  , parsePrimaryUnit
  , parseContextItem
  , parseContextItems
  , quoteParse
  , parseExpr
  , parseSeqStm
  , parseSeqStms
  , parseConStm
  , parseConStms
  , parseName
  , parseBlockDeclIt
  , parseBlockDeclIts
  , parseWaveform
  , parseAssociationEl
  , parseAssociationEls
  , parseProcDecl
  , parseProcDecls
  , parseStringLit
  , parseCharLit
  , stateParse
  , Result
  )
where

import           Text.Parsec

import           Language.VHDL.Lexer
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

parseDesignFileQ :: (String, Int, Int) -> String -> Result DesignFile
parseDesignFileQ = quoteParse designFile

parseDesignUnit :: (String, Int, Int) -> String -> Result DesignUnit
parseDesignUnit = quoteParse designUnit

parseLibraryUnit :: (String, Int, Int) -> String -> Result LibraryUnit
parseLibraryUnit = quoteParse libraryUnit

parsePrimaryUnit :: (String, Int, Int) -> String -> Result PrimaryUnit
parsePrimaryUnit = quoteParse primaryUnit

parseContextItem :: (String, Int, Int) -> String -> Result ContextItem
parseContextItem = quoteParse contextItem

parseContextItems :: (String, Int, Int) -> String -> Result ContextClause
parseContextItems = quoteParse contextClause

parseExpr :: (String, Int, Int) -> String -> Result Expression
parseExpr = quoteParse expression

parseSeqStm :: (String, Int, Int) -> String -> Result SequentialStatement
parseSeqStm = quoteParse sequentialStatement

parseSeqStms :: (String, Int, Int) -> String -> Result SequenceOfStatements
parseSeqStms = quoteParse sequenceOfStatements

parseConStm :: (String, Int, Int) -> String -> Result ConcurrentStatement
parseConStm = quoteParse concurrentStatement

parseConStms :: (String, Int, Int) -> String -> Result [ConcurrentStatement]
parseConStms = quoteParse concurrentStatements

parseWaveform :: (String, Int, Int) -> String -> Result Waveform
parseWaveform = quoteParse waveform

parseName :: (String, Int, Int) -> String -> Result Name
parseName = quoteParse name

parseBlockDeclIt :: (String, Int, Int) -> String -> Result BlockDeclarativeItem
parseBlockDeclIt = quoteParse blockDeclarativeItem

parseBlockDeclIts :: (String, Int, Int) -> String -> Result BlockDeclarativePart
parseBlockDeclIts = quoteParse blockDeclarativePart

parseAssociationEl :: (String, Int, Int) -> String -> Result AssociationElement
parseAssociationEl = quoteParse associationElement

parseAssociationEls :: (String, Int, Int) -> String -> Result AssociationList
parseAssociationEls = quoteParse associationList

parseProcDecl :: (String, Int, Int) -> String -> Result ProcessDeclarativeItem
parseProcDecl = quoteParse processDeclarativeItem

parseProcDecls :: (String, Int, Int) -> String -> Result ProcessDeclarativePart
parseProcDecls = quoteParse processDeclarativePart

parseStringLit :: (String, Int, Int) -> String -> Result StringLiteral
parseStringLit = quoteParse stringLiteral

parseCharLit :: (String, Int, Int) -> String -> Result CharacterLiteral
parseCharLit = quoteParse charLiteral

updatePosition :: String -> Int -> Int -> Parser ()
updatePosition file line col = do
   pos <- getPosition
   setPosition $
     flip setSourceName file $
     flip setSourceLine line $
     setSourceColumn pos col
