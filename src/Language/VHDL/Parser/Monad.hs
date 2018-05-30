module Language.VHDL.Parser.Monad
  ( Parser
  , ParseState
  , pushBlockName
  , popBlockName
  , newParseState
  , quotesEnabled
  , identToLower
  ) where

import           Control.Monad.State
import           Data.Char            (toLower)
--import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Void
import           Language.VHDL.Syntax
import           Text.Megaparsec

trace :: t -> a -> a
trace _ = id

type Parser = StateT ParseState (Parsec Void T.Text)

data ParseState = ParseState
  { blockNames  :: [Identifier]
  , parseQuotes :: Bool
  }

-- Make Identifier a Functor instance?
identToLower :: Identifier -> Identifier
identToLower = go
  where
    go (Ident s)         = Ident (T.map toLower s)
    go (ExtendedIdent s) = ExtendedIdent (T.map toLower s)
    go a                 = a

newParseState :: Bool -> ParseState
newParseState q = ParseState {blockNames = [], parseQuotes = q}

pushBlockName :: Identifier -> Parser Identifier
pushBlockName s = do
  modify (\st -> st {blockNames = identToLower s : blockNames st})
  return $ trace ("Push: " ++ show s) s

popBlockName :: Parser Identifier
popBlockName = do
  st <- get
  put $ st {blockNames = tail $ blockNames st}
  let res = head $ blockNames st
  return $ trace ("Pop " ++ show res) res

quotesEnabled :: Parser Bool
quotesEnabled = parseQuotes <$> get
