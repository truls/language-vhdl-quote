module Language.VHDL.Quote.Quoters
  ( name
  , expr
  , seqstm
  , seqstms
  )
  where

import Language.Haskell.TH.Quote (QuasiQuoter)

import Language.VHDL.Parser
import Language.VHDL.Quote.Internal


name, expr, seqstm, seqstms :: QuasiQuoter
name = quasiquote parseName
expr = quasiquote parseExpr
seqstm = quasiquote parseSeqStm
seqstms = quasiquote parseSeqStms
constm = quasiquote parseConStm
constms = quasiquote parseConStms

--interfaces = quasu
