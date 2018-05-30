module Language.VHDL.Parser
  (
    -- Utility functions
    parseFile
  , stateParse
  , quoteParse
  , Result
  , parseDesignFile

    -- Quasiquoter parsing functions
  , parseDesignFileQ
  , parseDesignUnit
  , parseLibraryUnit
  , parsePrimaryUnit
  , parseContextItem
  , parseContextItems
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
  , parseLiteral
  , parseElAssoc
  , parseCaseAlt
  , parseIfaceDecl
  , parseSubtyInd
  , parsePackDecl
  )
where

import           Control.Monad.State           (evalStateT)
import           Data.List.NonEmpty            as N
import           Data.Text
import qualified Data.Text.IO                  as T
import           Text.Megaparsec

import           Language.VHDL.Lexer
import           Language.VHDL.Parser.Internal
import           Language.VHDL.Parser.Monad
import           Language.VHDL.Syntax

type Result a = Either String a

withStateParse :: ParseState -> Parser a -> String -> Text -> Result a
withStateParse u p f c =
  case runParser (evalStateT p u) f c of
    Left err -> Left $ parseErrorPretty err
    Right r  -> Right r

stateParse :: Parser a -> String -> Text -> Result a
stateParse = withStateParse $ newParseState False

quoteParse :: Parser a -> (String, Int, Int) -> Text -> Result a
quoteParse p (f, r, c) =
  withStateParse (newParseState True) (updatePosition f r c >> p) ""

parseFile :: FilePath -> IO (Result DesignFile)
parseFile fp = do
  contents <- T.readFile fp
  return $ parseDesignFile fp contents

parseDesignFile :: FilePath -> Text -> Result DesignFile
parseDesignFile = stateParse designFile

parseDesignFileQ :: (String, Int, Int) -> Text -> Result DesignFile
parseDesignFileQ = quoteParse designFile

parseDesignUnit :: (String, Int, Int) -> Text -> Result DesignUnit
parseDesignUnit = quoteParse designUnit

parseLibraryUnit :: (String, Int, Int) -> Text -> Result LibraryUnit
parseLibraryUnit = quoteParse libraryUnit

parsePrimaryUnit :: (String, Int, Int) -> Text -> Result PrimaryUnit
parsePrimaryUnit = quoteParse primaryUnit

parseContextItem :: (String, Int, Int) -> Text -> Result ContextItem
parseContextItem = quoteParse contextItem

parseContextItems :: (String, Int, Int) -> Text -> Result [ContextItem]
parseContextItems = quoteParse (many contextItem)

parseExpr :: (String, Int, Int) -> Text -> Result Expression
parseExpr = quoteParse expression

parseSeqStm :: (String, Int, Int) -> Text -> Result SequentialStatement
parseSeqStm = quoteParse sequentialStatement

parseSeqStms :: (String, Int, Int) -> Text -> Result SequenceOfStatements
parseSeqStms = quoteParse sequenceOfStatements

parseConStm :: (String, Int, Int) -> Text -> Result ConcurrentStatement
parseConStm = quoteParse concurrentStatement

parseConStms :: (String, Int, Int) -> Text -> Result [ConcurrentStatement]
parseConStms = quoteParse concurrentStatements

parseWaveform :: (String, Int, Int) -> Text -> Result Waveform
parseWaveform = quoteParse waveform

parseName :: (String, Int, Int) -> Text -> Result Name
parseName = quoteParse name

parseBlockDeclIt :: (String, Int, Int) -> Text -> Result BlockDeclarativeItem
parseBlockDeclIt = quoteParse blockDeclarativeItem

parseBlockDeclIts :: (String, Int, Int) -> Text -> Result BlockDeclarativePart
parseBlockDeclIts = quoteParse blockDeclarativePart

parseAssociationEl :: (String, Int, Int) -> Text -> Result AssociationElement
parseAssociationEl = quoteParse associationElement

parseAssociationEls :: (String, Int, Int) -> Text -> Result AssociationList
parseAssociationEls = quoteParse associationList

parseProcDecl :: (String, Int, Int) -> Text -> Result ProcessDeclarativeItem
parseProcDecl = quoteParse processDeclarativeItem

parseProcDecls :: (String, Int, Int) -> Text -> Result ProcessDeclarativePart
parseProcDecls = quoteParse processDeclarativePart

parseStringLit :: (String, Int, Int) -> Text -> Result StringLiteral
parseStringLit = quoteParse stringLiteral

parseCharLit :: (String, Int, Int) -> Text -> Result CharacterLiteral
parseCharLit = quoteParse charLiteral

parseLiteral :: (String, Int, Int) -> Text -> Result Literal
parseLiteral = quoteParse literal

parseElAssoc :: (String, Int, Int) -> Text -> Result ElementAssociation
parseElAssoc = quoteParse elementAssociation

parseCaseAlt :: (String, Int, Int) -> Text -> Result CaseStatementAlternative
parseCaseAlt = quoteParse caseStatementAlternative

parseIfaceDecl :: (String, Int, Int) -> Text -> Result InterfaceDeclaration
parseIfaceDecl = quoteParse interfaceElement

parseSubtyInd :: (String, Int, Int) -> Text -> Result SubtypeIndication
parseSubtyInd = quoteParse subtypeIndication

parsePackDecl :: (String, Int, Int) -> Text -> Result PackageDeclarativeItem
parsePackDecl = quoteParse packageDeclarativeItem

updatePosition :: String -> Int -> Int -> Parser ()
updatePosition file line col =
  let pos = SourcePos file (mkPos line) (mkPos col)
  in updateParserState (\s -> s {statePos = N.fromList [pos]})
