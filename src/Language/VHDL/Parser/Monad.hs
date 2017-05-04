{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.VHDL.Parser.Monad
  ( Parser
  , ParseState
  , pushBlockName
  , popBlockName
  , newParseState
--  , unParser
  )
where

import Text.Parsec

import Debug.Trace

import qualified Control.Monad.State as CM
import Control.Monad.Identity

import Language.VHDL.Syntax

-- newtype Parser a = Parser { unParser :: ParsecT String () (CM.StateT ParseState Identity) a }
--   deriving ( Functor, Applicative, Monad
--            , CM.MonadState ParseState
--            )

type Parser = Parsec String ParseState

data ParseState = ParseState
  { blockNames :: [Identifier]
  }

newParseState :: ParseState
newParseState = ParseState {
  blockNames = []
  }

-- runParser :: Parser a -> String -> String -> ParseState -> Either ParseError a
-- runParser p sn name ps = CM.runStateT (runPT (unParser p) () sn name) ps

pushBlockName :: Identifier -> Parser Identifier
pushBlockName s = do
  updateState $ (\st -> st { blockNames = s : blockNames st })

  return $ trace ("Push: " ++ show s) s

popBlockName :: Parser Identifier
popBlockName = do
  st <- getState
  putState $ st { blockNames = tail $ blockNames st }
  let res = head $ blockNames st
  return $ trace ("Pop " ++ show res) res
