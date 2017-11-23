module Language.VHDL.Quote.Quoters
  ( ident
  , name
  , expr
  , seqstm
  , seqstms
  , constm
  , constms
  , designfile
  , designunit
  , libraryunit
  , primaryunit
  , contextitem
  , contextitems
  , blockdecl
  , blockdecls
  , assocel
  , assocels
  , wave
  , procdecl
  , procdecls
  , slit
  , clit
  ) where

import           Language.Haskell.TH.Quote    (QuasiQuoter)

import           Language.VHDL.Parser
import           Language.VHDL.Quote.Internal


ident, name, expr, seqstm, seqstms, constm, constms, designfile, designunit, libraryunit, primaryunit, contextitem, contextitems, blockdecl, blockdecls, assocel, assocels, wave, procdecl, procdecls, slit, clit :: QuasiQuoter
ident = quasiquote parseName
name = quasiquote parseName
expr = quasiquote parseExpr
seqstm = quasiquote parseSeqStm
seqstms = quasiquote parseSeqStms
constm = quasiquote parseConStm
constms = quasiquote parseConStms
designfile = quasiquote parseDesignFileQ
designunit = quasiquote parseDesignUnit
libraryunit = quasiquote parseLibraryUnit
primaryunit = quasiquote parsePrimaryUnit
contextitem = quasiquote parseContextItem
contextitems = quasiquote parseContextItems
blockdecl = quasiquote parseBlockDeclIt
blockdecls = quasiquote parseBlockDeclIts
assocel = quasiquote parseAssociationEl
assocels = quasiquote parseAssociationEls
wave = quasiquote parseWaveform
procdecl = quasiquote parseProcDecl
procdecls = quasiquote parseProcDecls
slit = quasiquote parseStringLit
clit = quasiquote parseCharLit
