{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Inspired by C/Base.hs from language-c-quote
module Language.VHDL.Quote.Internal
  ( quasiquote
  ) where

import           Control.Monad              ((>=>))

import           Data.Data                  (Data (..))
import           Data.Generics              (extQ)
import           Data.List                  (stripPrefix)
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)

import           Language.Haskell.Meta      (parseExp)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote  (QuasiQuoter (..), dataToExpQ)
import           Language.Haskell.TH.Syntax (lift)

import           Language.VHDL.Parser       (Result)
import qualified Language.VHDL.Syntax       as V

class ToExpr a where
  toExpr :: a -> V.Expression

instance ToExpr String where
  toExpr a = V.PrimName $ V.NSimple (V.Ident (T.pack a))

instance ToExpr Int where
  toExpr = V.PrimLit . toLit

instance ToExpr Integer where
  toExpr = V.PrimLit . toLit

instance ToExpr Float where
  toExpr = V.PrimLit . toLit

instance ToExpr Double where
  toExpr = V.PrimLit . toLit

instance ToExpr V.Expression where
  toExpr v = v

-- instance ToExpr Int where
--   toExpr a = V.PrimLit $ V.LitNum $ V.NLitAbstract
class ToLit a where
  toLit :: a -> V.Literal

instance ToLit V.Literal where
  toLit l = l

instance ToLit V.DecimalLiteral where
  toLit = V.LitNum . V.NLitAbstract . V.ALitDecimal

instance ToLit String where
   toLit = V.LitString . V.SLit . T.pack

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

-- class ToName a where
--   toName :: a -> Either String V.Name

-- instance ToName V.Name where
--   toName v = Right v

-- instance ToName String where
--   toName a = case parseName ("", 0, 0) a of
--     Right r -> Right r
--     Left f  -> Left (show f)

class ToIdent a where
  toIdent :: a -> V.Identifier

instance ToIdent String where
  toIdent = V.Ident . T.pack

instance ToIdent V.Identifier where
  toIdent s = s

class ToSlit a where
  toSlit :: a -> V.StringLiteral

instance ToSlit T.Text where
  toSlit = V.SLit

instance ToSlit String where
  toSlit = V.SLit . T.pack

instance ToSlit V.StringLiteral where
  toSlit a = a

class ToClit a where
  toClit :: a -> V.CharacterLiteral

instance ToClit Char where
  toClit = V.CLit

instance ToClit V.CharacterLiteral where
  toClit a = a

-- FIXME: Get rid of stripPrefix. show may be locale dependent?
fromFloating
  :: (RealFrac a, Show a)
  => a -> V.DecimalLiteral
fromFloating f =
  let (n, m) = properFraction f
      fract = (fromJust . stripPrefix "0.") (show m)
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

qqIdentE :: V.Identifier -> Maybe (Q Exp)
qqIdentE (V.AntiIdent i) = Just [|toIdent $(antiVarE i)|]
qqIdentE _               = Nothing

qqNameE :: V.Name -> Maybe (Q Exp)
qqNameE (V.AntiName n) = Just $ antiVarE n
qqNameE _              = Nothing

qqExprE :: V.Expression -> Maybe (Q Exp)
qqExprE (V.AntiExpr v) = Just [|toExpr $(antiVarE v)|]
qqExprE _              = Nothing

qqSeqStmE :: V.SequentialStatement -> Maybe (Q Exp)
qqSeqStmE (V.AntiSeqStm v) = Just $ antiVarE v
qqSeqStmE _                = Nothing

qqSeqStmListE :: V.SequenceOfStatements -> Maybe (Q Exp)
qqSeqStmListE [] = Just [|[]|]
qqSeqStmListE (V.AntiSeqStms v:stms) =
  Just [|$(antiVarE v) ++ $(dataToExpQ qqExp stms)|]
qqSeqStmListE (stm:stms) =
  Just [|$(dataToExpQ qqExp stm) : $(dataToExpQ qqExp stms)|]

qqConStmE :: V.ConcurrentStatement -> Maybe (Q Exp)
qqConStmE (V.AntiConStm v) = Just $ antiVarE v
qqConStmE _                = Nothing

qqConStmListE :: [V.ConcurrentStatement] -> Maybe (Q Exp)
qqConStmListE [] = Just [|[]|]
qqConStmListE (V.AntiConStms v:stms) =
  Just [|$(antiVarE v) ++ $(dataToExpQ qqExp stms)|]
qqConStmListE (stm:stms) =
  Just [|$(dataToExpQ qqExp stm) : $(dataToExpQ qqExp stms)|]

-- FIXME: This may not be needed
qqDesignUnitE :: V.DesignUnit -> Maybe (Q Exp)
qqDesignUnitE (V.AntiDesignUnit v) = Just $ antiVarE v
qqDesignUnitE _                    = Nothing

qqLibraryUnitE :: V.LibraryUnit -> Maybe (Q Exp)
qqLibraryUnitE (V.AntiLibraryUnit v) = Just $ antiVarE v
qqLibraryUnitE _                     = Nothing

qqPrimaryUnitE :: V.PrimaryUnit -> Maybe (Q Exp)
qqPrimaryUnitE (V.AntiPrimaryUnit v) = Just $ antiVarE v
qqPrimaryUnitE _                     = Nothing

qqContextItemE :: V.ContextItem -> Maybe (Q Exp)
qqContextItemE (V.AntiContextItem v) = Just $ antiVarE v
qqContextItemE _                     = Nothing

qqContextItemListE :: V.ContextClause -> Maybe (Q Exp)
qqContextItemListE (V.ContextClause cis) = Just [|V.ContextClause $(go cis)|]
  where
    go [] = [|[]|]
    go (V.AntiContextItems v:items) =
      [|$(antiVarE v) ++ $(dataToExpQ qqExp items)|]
    go (item:items) = [|$(dataToExpQ qqExp item) : $(dataToExpQ qqExp items)|]

qqWaveformE :: V.Waveform -> Maybe (Q Exp)
qqWaveformE (V.AntiWave v) = Just $ antiVarE v
qqWaveformE _              = Nothing

qqAssocElE :: V.AssociationElement -> Maybe (Q Exp)
qqAssocElE (V.AntiAssocEl v) = Just $ antiVarE v
qqAssocElE _                 = Nothing

qqAssocElListE :: V.AssociationList -> Maybe (Q Exp)
qqAssocElListE (V.AssociationList l) = Just [|V.AssociationList $(go l)|]
  where
    go [] = [|[]|]
    go (V.AntiAssocEls v:els) = [|$(antiVarE v) ++ $(dataToExpQ qqExp els)|]
    go (el:els) = [|$(dataToExpQ qqExp el) : $(dataToExpQ qqExp els)|]

qqBlockDeclE :: V.BlockDeclarativeItem -> Maybe (Q Exp)
qqBlockDeclE (V.AntiBlockDecl v) = Just $ antiVarE v
qqBlockDeclE _                   = Nothing

qqBlockDeclListE :: [V.BlockDeclarativeItem] -> Maybe (Q Exp)
qqBlockDeclListE [] = Just [|[]|]
qqBlockDeclListE (V.AntiBlockDecls d:decls) =
  Just [|$(antiVarE d) ++ $(dataToExpQ qqExp decls)|]
qqBlockDeclListE (decl:decls) =
  Just [|$(dataToExpQ qqExp decl) : $(dataToExpQ qqExp decls)|]

qqProcDeclE :: V.ProcessDeclarativeItem -> Maybe (Q Exp)
qqProcDeclE (V.AntiProcDecl v) = Just $ antiVarE v
qqProcDeclE _                  = Nothing

qqProcDeclListE :: [V.ProcessDeclarativeItem] -> Maybe (Q Exp)
qqProcDeclListE [] = Just [|[]|]
qqProcDeclListE (V.AntiProcDecls d:decls) =
  Just [|$(antiVarE d) ++ $(dataToExpQ qqExp decls)|]
qqProcDeclListE (decl:decls) =
  Just [|$(dataToExpQ qqExp decl) : $(dataToExpQ qqExp decls)|]

qqStringLit :: V.StringLiteral -> Maybe (Q Exp)
qqStringLit (V.AntiSlit v) = Just $ [|toSlit $(antiVarE v)|]
qqStringLit _              = Nothing

qqCharLit :: V.CharacterLiteral -> Maybe (Q Exp)
qqCharLit (V.AntiClit v) = Just $ [|toClit $(antiVarE v)|]
qqCharLit _              = Nothing

qqLit :: V.Literal -> Maybe (Q Exp)
qqLit (V.AntiLit v) = Just $ [|toLit $(antiVarE v)|]
qqLit _             = Nothing

qqText :: T.Text -> Maybe (Q Exp)
qqText t = Just $ AppE (VarE 'T.pack) <$> lift (T.unpack t)

qqElAssocE :: V.ElementAssociation -> Maybe (Q Exp)
qqElAssocE (V.AntiElAssoc v) = Just $ antiVarE v
qqElAssocE _                 = Nothing

qqElAssocsE :: [V.ElementAssociation] -> Maybe (Q Exp)
qqElAssocsE [] = Just [|[]|]
qqElAssocsE (V.AntiElAssocs d:decls) =
  Just [|$(antiVarE d) ++ $(dataToExpQ qqExp decls)|]
qqElAssocsE (decl:decls) =
  Just [|$(dataToExpQ qqExp decl) : $(dataToExpQ qqExp decls)|]

qqExp
  :: Typeable a
  => a -> Maybe (Q Exp)
qqExp =
  const Nothing `extQ` qqIdentE `extQ` qqNameE `extQ` qqExprE `extQ`
  qqSeqStmListE `extQ`
  qqSeqStmE `extQ`
  qqConStmE `extQ`
  qqConStmListE `extQ`
  qqDesignUnitE `extQ`
  qqLibraryUnitE `extQ`
  qqPrimaryUnitE `extQ`
  qqWaveformE `extQ`
  qqContextItemE `extQ`
  qqContextItemListE `extQ`
  qqAssocElE `extQ`
  qqAssocElListE `extQ`
  qqBlockDeclE `extQ`
  qqBlockDeclListE `extQ`
  qqProcDeclE `extQ`
  qqProcDeclListE `extQ`
  qqStringLit `extQ`
  qqCharLit `extQ`
  qqLit `extQ`
  qqElAssocE `extQ`
  qqElAssocsE `extQ`
  qqText

parse :: ((String, Int, Int) -> T.Text -> Result a) -> String -> Q a
parse p s = do
  pos <- getPosition
  case p pos (T.pack s) of
    Left e  -> fail (show e)
    Right a -> return a
  where
    getPosition :: Q (String, Int, Int)
    getPosition = transPos <$> location
    transPos loc = (loc_filename loc, fst (loc_start loc), snd (loc_start loc))

quasiquote
  :: (Data a)
  => ((String, Int, Int) -> T.Text -> Result a) -> QuasiQuoter
quasiquote p =
  QuasiQuoter
  { quoteExp  = parse p >=> dataToExpQ qqExp
  , quotePat  = fail "VHDL pattern quasiquoter undefined"
  , quoteType = fail "VHDL type quasiquoter undefined"
  , quoteDec  = fail "VHDL declaration quasiquoter undefined"
  }
