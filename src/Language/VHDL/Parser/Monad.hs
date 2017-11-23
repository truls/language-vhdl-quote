module Language.VHDL.Parser.Monad
  ( Parser
  , ParseState
  , pushBlockName
  , popBlockName
  , newParseState
  , quotesEnabled
  , identToLower
  ) where

import           Data.Char            (toLower)
import           Language.VHDL.Syntax
import           Text.Parsec

trace :: t -> a -> a
trace _ = id

type Parser = Parsec String ParseState

data ParseState = ParseState
  { blockNames  :: [Identifier]
  , parseQuotes :: Bool
  }

-- Make Identifier a Functor instance?
identToLower :: Identifier -> Identifier
identToLower = go
  where
    go (Ident s)         = Ident (map toLower s)
    go (ExtendedIdent s) = ExtendedIdent (map toLower s)
    go a                 = a

newParseState :: Bool -> ParseState
newParseState q = ParseState {blockNames = [], parseQuotes = q}

pushBlockName :: Identifier -> Parser Identifier
pushBlockName s = do
  updateState (\st -> st {blockNames = identToLower s : blockNames st})
  return $ trace ("Push: " ++ show s) s

popBlockName :: Parser Identifier
popBlockName = do
  st <- getState
  putState $ st {blockNames = tail $ blockNames st}
  let res = head $ blockNames st
  return $ trace ("Pop " ++ show res) res

quotesEnabled :: Parser Bool
quotesEnabled = parseQuotes <$> getState
