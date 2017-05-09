module Language.VHDL.Parser.Monad
  ( Parser
  , ParseState
  , pushBlockName
  , popBlockName
  , newParseState
  , quotesEnabled
  ) where

import           Language.VHDL.Syntax
import           Text.Parsec

trace :: t -> a -> a
trace _ = id

type Parser = Parsec String ParseState

data ParseState = ParseState
  { blockNames  :: [Identifier]
  , parseQuotes :: Bool
  }

newParseState :: Bool -> ParseState
newParseState q = ParseState {blockNames = [], parseQuotes = q}

pushBlockName :: Identifier -> Parser Identifier
pushBlockName s = do
  updateState (\st -> st {blockNames = s : blockNames st})
  return $ trace ("Push: " ++ show s) s

popBlockName :: Parser Identifier
popBlockName = do
  st <- getState
  putState $ st {blockNames = tail $ blockNames st}
  let res = head $ blockNames st
  return $ trace ("Pop " ++ show res) res

quotesEnabled :: Parser Bool
quotesEnabled = parseQuotes <$> getState
