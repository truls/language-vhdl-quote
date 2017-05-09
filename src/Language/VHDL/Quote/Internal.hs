{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Inspired by C/Base.hs from language-c-quote
module Language.VHDL.Quote.Internal
  ( quasiquote
  ) where

import           Control.Monad             ((>=>))

import           Data.Data                 (Data (..))
import           Data.Generics             (extQ)
import           Data.List                 (stripPrefix)
import           Data.Maybe                (fromJust)
import           Data.Typeable             (Typeable)

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote (QuasiQuoter (..), dataToExpQ,
                                            dataToPatQ)

--import Language.Haskell.TH.Syntax
import           Language.Haskell.Meta     (parseExp, parsePat)

import           Language.VHDL.Parser      (Result)
import qualified Language.VHDL.Syntax      as V

class ToExpr a where
  toExpr :: a -> V.Expression

instance ToExpr String where
  toExpr a = V.PrimName $ V.NSimple (V.Ident a)

instance ToExpr Int where
  toExpr = V.PrimLit . toLit

instance ToExpr Integer where
  toExpr = V.PrimLit . toLit

instance ToExpr Float where
  toExpr = V.PrimLit . toLit

instance ToExpr Double where
  toExpr = V.PrimLit . toLit

-- instance ToExpr Int where
--   toExpr a = V.PrimLit $ V.LitNum $ V.NLitAbstract
class ToLit a where
  toLit :: a -> V.Literal

instance ToLit V.Literal where
  toLit l = l

instance ToLit V.DecimalLiteral where
  toLit a = V.LitNum . V.NLitAbstract . V.ALitDecimal $ a

instance ToLit String where
  toLit = V.LitString . V.SLit

instance ToLit Float where
  toLit a = toLit $ toDecLit a

instance ToLit Double where
  toLit a = toLit $ toDecLit a

instance ToLit Integer where
  toLit a = toLit $ toDecLit a

instance ToLit Int where
  toLit a = toLit $ toDecLit a

class (Num a) =>
      ToDecLit a where
  toDecLit :: a -> V.DecimalLiteral

-- FIXME: Get rid of stripPrefix. show may be locale dependent?
fromFloating
  :: (RealFrac a, Show a)
  => a -> V.DecimalLiteral
fromFloating f =
  let (n, m) = properFraction f
      fract = (read ((fromJust . stripPrefix "0.") (show m)) :: Integer)
  in V.DecimalLiteral n (Just fract) Nothing

instance ToDecLit Double where
  toDecLit = fromFloating

instance ToDecLit Float where
  toDecLit = fromFloating

instance ToDecLit Integer where
  toDecLit n = V.DecimalLiteral n Nothing Nothing

instance ToDecLit Int where
  toDecLit n = V.DecimalLiteral (fromIntegral n) Nothing Nothing

antiVarE :: String -> ExpQ
antiVarE = either fail return . parseExp

qqNameE :: V.Name -> Maybe (Q Exp)
qqNameE (V.AntiName n) = Just [|$(antiVarE n)|]
qqNameE _              = Nothing

qqExprE :: V.Expression -> Maybe (Q Exp)
qqExprE (V.AntiExpr v) = Just [|toExpr $(antiVarE v)|]
qqExprE _              = Nothing

--qqIdentE :: V.Ident -> Maybe (Q Exp)
qqExp
  :: Typeable a
  => a -> Maybe (Q Exp)
qqExp = const Nothing `extQ` qqNameE `extQ` qqExprE

qqPat
  :: Typeable a
  => a -> Maybe (Q Pat)
qqPat = undefined

parse :: ((String, Int, Int) -> String -> Result a) -> String -> Q a
parse p s = do
  pos <- getPosition
  case p pos s of
    Left e  -> fail (show e)
    Right a -> return a
  where
    getPosition :: Q (String, Int, Int)
    getPosition = transPos <$> location
    transPos loc = (loc_filename loc, fst (loc_start loc), snd (loc_start loc))

quasiquote
  :: (Data a)
  => ((String, Int, Int) -> String -> Result a) -> QuasiQuoter
quasiquote p =
  QuasiQuoter
  { quoteExp  = parse p >=> dataToExpQ qqExp
  , quotePat  = parse p >=> dataToPatQ qqPat
  , quoteType = fail "VHDL type quasiquoter undefined"
  , quoteDec  = fail "VHDL declaration quasiquoter undefined"
  }
