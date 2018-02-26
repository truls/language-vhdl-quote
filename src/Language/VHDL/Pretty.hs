{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.VHDL.Pretty
  ( pprr
  , pprrText
  ) where

import           Data.Char                       (intToDigit)
import           Data.Data                       (Data)
import qualified Data.Text                       as T
import           Data.Text.Lazy                  (toStrict)
import           Numeric                         (showIntAtBase)
import           Text.PrettyPrint.Mainland
import           Text.PrettyPrint.Mainland.Class

import           Language.VHDL.Parser.Util
import           Language.VHDL.Syntax

--------------------------------------------------------------------------------
-- ** Pretty printing instances
instance Pretty AbstractLiteral where
  ppr (ALitDecimal d) = ppr d
  ppr (ALitBased b)   = ppr b

instance Pretty AccessTypeDefinition where
  ppr (AccessTypeDefinition s) = text "access" <+> ppr s

instance Pretty ActualDesignator where
  ppr (ADExpression e) = ppr e
  ppr (ADSignal n)     = ppr n
  ppr (ADVariable n)   = ppr n
  ppr (ADFile n)       = ppr n
  ppr ADOpen           = text "open"

instance Pretty ActualPart where
  ppr (APDesignator a) = ppr a
  ppr (APFunction f a) = ppr f <+> parens (ppr a)
  ppr (APType t a)     = ppr t <+> parens (ppr a)

instance Pretty Aggregate where
  ppr (Aggregate es) = parens (commasep $ map ppr es)

instance Pretty AliasDeclaration where
  ppr (AliasDeclaration a sub n sig) =
    text "alias" <+>
    ppr a <+> cond (colon <+>) sub <+> text "is" <+> ppr n <> ppr' sig <> semi

instance Pretty AliasDesignator where
  ppr (ADIdentifier i) = ppr i
  ppr (ADCharacter c)  = ppr c
  ppr (ADOperator o)   = ppr o

instance Pretty Allocator where
  ppr (AllocSub s)  = text "new" <+> ppr s
  ppr (AllocQual q) = text "new" <+> ppr q

instance Pretty ArchitectureBody where
  ppr (ArchitectureBody i n d s) =
    stack [header, indent' (vppr d), text "begin", indent' (vppr s), footer]
    where
      header = text "architecture" <+> ppr i <+> text "of" <+> ppr n <+> text "is"
      footer = text "end architecture" <+> ppr i <> semi

instance Pretty ArrayTypeDefinition where
  ppr (ArrU u) = ppr u
  ppr (ArrC c) = ppr c

instance Pretty Assertion where
  ppr (Assertion c r s) = stack [text "assert" <+> ppr c, report, severity]
    where
      report = indent' $ cond (text "report" <+>) r
      severity = indent' $ cond (text "severity" <+>) s

instance Pretty AssertionStatement where
  ppr (AssertionStatement l a) = label l <+> ppr a <> semi

instance Pretty AssociationElement where
  ppr (AssociationElement f a) = condR (text "=>") f <+> ppr a
  ppr e@(AntiAssocEl s)        = pprAnti e s
  ppr e@(AntiAssocEls s)       = pprAnti e s

instance Pretty AssociationList where
  ppr (AssociationList as) = commasep $ map ppr as

instance Pretty AttributeDeclaration where
  ppr (AttributeDeclaration i t) =
    text "attribute" <+> ppr i <> colon <+> ppr t <> semi

instance Pretty AttributeName where
  ppr (AttributeName p s d e) =
    ppr p <> ppr' s <> char '\'' <> ppr d <> ppr (appL parens e)

instance Pretty AttributeSpecification where
  ppr (AttributeSpecification d s e) =
    text "attribute" <+>
    ppr d <+> text "of" <+> ppr s <+> text "is" <+> ppr e <> semi

instance Pretty BaseSpecifier where
  ppr BinaryBase         = text "B"
  ppr OctalBase          = text "O"
  ppr HexBase            = text "X"
  ppr UnsignedBinaryBase = text "UB"
  ppr UnsignedOctalBase  = text "UO"
  ppr UnsignedHexBase    = text "UX"
  ppr SignedBinaryBase   = text "SB"
  ppr SignedHoctalBase   = text "SO"
  ppr SignedHexBase      = text "SX"
  ppr Decimal            = text "D"

instance Pretty BasedLiteral where
  ppr (BasedLiteral b i f e) =
    ppr b <> char '#' <> text (intToBase b i) <> condL' (char '.') (text <$> f) <>
    char '#' <>
    ppr e

instance Pretty BindingIndication where
  ppr (BindingIndication e g p) =
    stack [condL (text "use") e, ppr g, ppr p]

instance Pretty BitStringLiteral where
  ppr (BitStringLiteral l b v) = ppr l <> ppr b <> ppr v

instance Pretty BitValue where
  ppr (BitValue s) = ppr s

instance Pretty BlockConfiguration where
  ppr (BlockConfiguration s u c) =
    stack
      [ text "for" <+> ppr s
      , indent' $ pprlStack u
      , indent' $ pprlStack c
      , text "end for" <> semi
      ]

instance Pretty BlockDeclarativeItem where
  ppr (BDISubprogDecl d)   = ppr d
  ppr (BDISubprogBody b)   = ppr b
  ppr (BDIType t)          = ppr t
  ppr (BDISubtype s)       = ppr s
  ppr (BDIConstant c)      = ppr c
  ppr (BDISignal s)        = ppr s
  ppr (BDIShared v)        = ppr v
  ppr (BDIFile f)          = ppr f
  ppr (BDIAlias a)         = ppr a
  ppr (BDIComp c)          = ppr c
  ppr (BDIAttrDecl a)      = ppr a
  ppr (BDIAttrSepc a)      = ppr a
  ppr (BDIConfigSepc c)    = ppr c
  ppr (BDIDisconSpec d)    = ppr d
  ppr (BDIUseClause u)     = ppr u
  ppr (BDIGroupTemp g)     = ppr g
  ppr (BDIGroup g)         = ppr g
  ppr e@(AntiBlockDecl s)  = pprAnti e s
  ppr e@(AntiBlockDecls s) = pprAnti e s

instance Pretty BlockHeader where
  ppr (BlockHeader p g) = stack [go p, go g]
    where
      go
        :: (Pretty a, Pretty b)
        => Maybe (a, Maybe b) -> Doc
      go Nothing        = empty
      go (Just (a, mb)) = ppr a </> cond (indent' . (<> semi)) mb

instance Pretty BlockSpecification where
  ppr (BSArch n)  = ppr n
  ppr (BSBlock l) = ppr l
  ppr (BSGen l i) = ppr l <+> cond parens i

instance Pretty BlockStatement where
  ppr (BlockStatement l g h d s) =
    ppr l <> colon <+> stack [header, body, footer]
    where
      header =
        text "block" <+>
        cond parens g <+> text "is" `hangs` stack (ppr h : line : map ppr d)
      body = text "begin" `hangs` stack (map ppr s)
      footer = text "end block" <+> ppr l <> semi

instance Pretty CaseStatement where
  ppr (CaseStatement l e cs) = labels l $ stack [header, body, footer]
    where
      header = text "case" <+> ppr e <+> text "is"
      body = indent' $ stack $ map ppr cs
      footer = text "end case" <> ppr' l <> semi

instance Pretty CaseStatementAlternative where
  ppr (CaseStatementAlternative c ss) =
    stack [text "when" <+> ppr c <+> text "=>", indent' $ stack $ map ppr ss]

instance Pretty CharacterLiteral where
  ppr (CLit c)       = char '\'' <> char c <> char '\''
  ppr a@(AntiClit c) = pprAnti a c

instance Pretty Choice where
  ppr (ChoiceSimple s) = ppr s
  ppr (ChoiceRange r)  = ppr r
  ppr (ChoiceName n)   = ppr n
  ppr ChoiceOthers     = text "others"

instance Pretty Choices where
  ppr (Choices cs) = pipeSep $ map ppr cs

instance Pretty ComponentConfiguration where
  ppr (ComponentConfiguration s i c) =
    stack
      [ text "for" <+> ppr s
      , indent' $ stack [condR' semi i, ppr c]
      , text "end for" <> semi
      ]

instance Pretty ComponentDeclaration where
  ppr (ComponentDeclaration i g p) =
    stack
      [ text "component" <+> ppr i <+> text "is"
      , indent' $ stack [ppr g, ppr p]
      , text "end component" <+> ppr i <> semi
      ]

instance Pretty ComponentInstantiationStatement where
  ppr (ComponentInstantiationStatement l u g p) =
    ppr l <> colon `hangs` (ppr u `hangs` stack [ppr g, ppr p]) <> semi

instance Pretty ComponentSpecification where
  ppr (ComponentSpecification ls n) = ppr ls <> colon <+> ppr n

instance Pretty CompositeTypeDefinition where
  ppr (CTDArray at)  = ppr at
  ppr (CTDRecord rt) = ppr rt

instance Pretty ConcurrentAssertionStatement where
  ppr (ConcurrentAssertionStatement l p a) = postponed l p a <> semi

instance Pretty ConcurrentProcedureCallStatement where
  ppr (ConcurrentProcedureCallStatement l p a) = postponed l p a <> semi

instance Pretty ConcurrentSignalAssignmentStatement where
  ppr (CSASCond l p a)   = postponed l p a
  ppr (CSASSelect l p a) = postponed l p a

instance Pretty ConcurrentStatement where
  ppr (ConBlock b)      = ppr b
  ppr (ConProcess p)    = ppr p
  ppr (ConProcCall c)   = ppr c
  ppr (ConAssertion a)  = ppr a
  ppr (ConSignalAss s)  = ppr s
  ppr (ConComponent c)  = ppr c
  ppr (ConGenerate g)   = ppr g
  ppr a@(AntiConStm s)  = pprAnti a s
  ppr a@(AntiConStms s) = pprAnti a s

instance Pretty ConditionClause where
  ppr (ConditionClause e) = text "until" <+> ppr e

instance Pretty ConditionalSignalAssignment where
  ppr (ConditionalSignalAssignment t o w) =
    ppr t <+> text "<=" <+> ppr o <+> ppr w <> semi

instance Pretty ConditionalWaveforms where
  ppr (ConditionalWaveforms ws (w, c)) =
    stack ws' <+> ppr w <+> condL (text "when") c
    where
      ws' =
        map (\(w', c') -> ppr w' <+> text "when" <+> ppr c' <+> text "else") ws

instance Pretty ConfigurationDeclaration where
  ppr (ConfigurationDeclaration i n d b) =
    stack
      [ text "configuration" <+> ppr i <+> text "of" <+> ppr n <+> text "is"
      , indent' $ stack (map ppr d ++ [ppr b])
      , text "end configuration" <+> ppr i <> semi
      ]

instance Pretty ConfigurationDeclarativeItem where
  ppr (CDIUse u)      = ppr u
  ppr (CDIAttrSpec a) = ppr a
  ppr (CDIGroup g)    = ppr g

instance Pretty ConfigurationItem where
  ppr (CIBlock b) = ppr b
  ppr (CIComp c)  = ppr c

instance Pretty ConfigurationSpecification where
  ppr (ConfigurationSpecification s i) = text "for" <+> ppr s <+> ppr i <> semi

instance Pretty ConstantDeclaration where
  ppr (ConstantDeclaration is s e) =
    text "constant" <+>
    commasep (fmap ppr is) <> colon <+> ppr s <+> condL (text ":=") e <> semi

instance Pretty ConstrainedArrayDefinition where
  ppr (ConstrainedArrayDefinition i s) =
    text "array" <+> ppr i <+> text "of" <+> ppr s

instance Pretty Constraint where
  ppr (CRange r) = ppr r
  ppr (CIndex i) = ppr i

instance Pretty ContextClause where
  ppr (ContextClause items) = stack $ fmap ppr items

instance Pretty ContextItem where
  ppr (ContextLibrary l)     = ppr l
  ppr (ContextUse u)         = ppr u
  ppr e@(AntiContextItem i)  = pprAnti e i
  ppr e@(AntiContextItems i) = pprAnti e i

instance Pretty DecimalLiteral where
  ppr (DecimalLiteral i f e) = ppr i <> condL' (char '.') (text <$> f) <> ppr e

instance Pretty Declaration where
  ppr (DType t)          = ppr t
  ppr (DSubtype s)       = ppr s
  ppr (DObject o)        = ppr o
  ppr (DAlias a)         = ppr a
  ppr (DComponent c)     = ppr c
  ppr (DAttribute a)     = ppr a
  ppr (DGroupTemplate g) = ppr g
  ppr (DGroup g)         = ppr g
  ppr (DEntity e)        = ppr e
  ppr (DConfiguration c) = ppr c
  ppr (DSubprogram s)    = ppr s
  ppr (DPackage p)       = ppr p

instance Pretty DelayMechanism where
  ppr DMechTransport    = text "transport"
  ppr (DMechInertial e) = condL (text "reject") e <+> text "inertial"

instance Pretty DesignFile where
  ppr (DesignFile units) = stack $ fmap ppr units

instance Pretty DesignUnit where
  ppr (DesignUnit ctxt lib) = stack [ppr ctxt, ppr lib]
  ppr a@(AntiDesignUnit s)  = pprAnti a s
  ppr a@(AntiDesignUnits s) = pprAnti a s

instance Pretty Designator where
  ppr (DId i) = ppr i
  ppr (DOp o) = ppr o

instance Pretty Direction where
  ppr To     = text "to"
  ppr DownTo = text "downto"

instance Pretty DisconnectionSpecification where
  ppr (DisconnectionSpecification g e) =
    text "disconnect" <+> ppr g <+> text "after" <+> ppr e <> semi

instance Pretty DiscreteRange where
  ppr (DRSub s)   = ppr s
  ppr (DRRange r) = ppr r

instance Pretty ElementAssociation where
  ppr (ElementAssociation c e) = condR (text "=>") c <+> ppr e

instance Pretty ElementDeclaration where
  ppr (ElementDeclaration is s) = commasep (map ppr is) <> colon <+> ppr s <> semi

instance Pretty EntityAspect where
  ppr (EAEntity n i) = text "entity" <+> ppr n <+> cond parens i
  ppr (EAConfig n)   = text "configuration" <+> ppr n
  ppr EAOpen         = text "open"

instance Pretty EntityClass where
  ppr ENTITY        = text "entity"
  ppr ARCHITECTURE  = text "architecture"
  ppr CONFIGURATION = text "configuration"
  ppr PROCEDURE     = text "procedure"
  ppr FUNCTION      = text "function"
  ppr PACKAGE       = text "package"
  ppr TYPE          = text "type"
  ppr SUBTYPE       = text "subtype"
  ppr CONSTANT      = text "constant"
  ppr SIGNAL        = text "signal"
  ppr VARIABLE      = text "variable"
  ppr COMPONENT     = text "Component"
  ppr LABEL         = text "Label"
  ppr LITERAL       = text "literal"
  ppr UNITS         = text "units"
  ppr GROUP         = text "group"
  ppr FILE          = text "file"

instance Pretty EntityClassEntry where
  ppr (EntityClassEntry c m) = ppr c <+> when m (text "<>")

instance Pretty EntityDeclaration where
  ppr (EntityDeclaration i h d s) =
    stack
      [ line
      , text "entity" <+> ppr i <+> text "is"
      , indent' $ stack (ppr h : map ppr d)
      , hang' $ ppr (catL (text "begin" <> line) stack <$> (map ppr <$> s))
      , text "end entity" <+> ppr i <> semi </> text ""
      ]

instance Pretty EntityDeclarativeItem where
  ppr (EDISubprogDecl s) = ppr s
  ppr (EDISubprogBody b) = ppr b
  ppr (EDIType t)        = ppr t
  ppr (EDISubtype s)     = ppr s
  ppr (EDIConstant c)    = ppr c
  ppr (EDISignal s)      = ppr s
  ppr (EDIShared s)      = ppr s
  ppr (EDIFile f)        = ppr f
  ppr (EDIAlias a)       = ppr a
  ppr (EDIAttrDecl a)    = ppr a
  ppr (EDIAttrSpec a)    = ppr a
  ppr (EDIDiscSpec d)    = ppr d
  ppr (EDIUseClause u)   = ppr u
  ppr (EDIGroupTemp g)   = ppr g
  ppr (EDIGroup g)       = ppr g

instance Pretty EntityDesignator where
  ppr (EntityDesignator t s) = ppr t <> ppr' s

instance Pretty EntityHeader where
  ppr (EntityHeader g p) = stack [cond indent' g, cond indent' p]

instance Pretty EntityNameList where
  ppr (ENLDesignators es) = commasep $ fmap ppr es
  ppr ENLOthers           = text "others"
  ppr ENLAll              = text "all"

instance Pretty EntitySpecification where
  ppr (EntitySpecification ns c) = ppr ns <> colon <+> ppr c

instance Pretty EntityStatement where
  ppr (ESConcAssert a)  = ppr a
  ppr (ESPassiveConc p) = ppr p
  ppr (ESPassiveProc p) = ppr p

instance Pretty EntityTag where
  ppr (ETName n) = ppr n
  ppr (ETChar c) = ppr c
  ppr (ETOp o)   = ppr o

instance Pretty EnumerationLiteral where
  ppr (EId i)   = ppr i
  ppr (EChar c) = ppr c

instance Pretty EnumerationTypeDefinition where
  ppr (EnumerationTypeDefinition es) = parens $ commasep $ fmap ppr es

instance Pretty ExitStatement where
  ppr (ExitStatement l b c) =
    label l <+> text "exit" <> ppr' b <> condL (space <> text "when") c <> semi

instance Pretty Exponent where
  ppr (ExponentPos i) = char 'e' <> ppr i
  ppr (ExponentNeg i) = char 'e' <> char '-' <> ppr i

instance Pretty Expression where
  ppr (Unary op@Identity e) = ppr op <> ppr e
  ppr (Unary op@Negation e) = ppr op <> ppr e
  ppr (Unary op e)          = ppr op <+> ppr e
  ppr (Binary op e1 e2)     = ppr e1 <+> ppr op <+> ppr e2
  ppr (PrimName n)          = ppr n
  ppr (PrimLit l)           = ppr l
  ppr (PrimAgg a)           = ppr a
  ppr (PrimFun f)           = ppr f
  ppr (PrimQual q)          = ppr q
  ppr (PrimTCon t)          = ppr t
  ppr (PrimAlloc a)         = ppr a
  ppr (PrimExp e)           = text "(" <> ppr e <> text ")"
  ppr a@(AntiExpr e)        = pprAnti a e

instance Pretty BinOp where
  ppr And    = text "and"
  ppr Or     = text "or"
  ppr Nand   = text "nand"
  ppr Nor    = text "nor"
  ppr Xor    = text "xor"
  ppr Xnor   = text "xnor"
  ppr Eq     = text "="
  ppr Neq    = text "/="
  ppr Lt     = text "<"
  ppr Lte    = text "<="
  ppr Gt     = text ">"
  ppr Gte    = text ">="
  ppr Sll    = text "sll"
  ppr Srl    = text "srl"
  ppr Sla    = text "sla"
  ppr Sra    = text "sra"
  ppr Rol    = text "rol"
  ppr Ror    = text "ror"
  ppr Times  = text "*"
  ppr Div    = text "/"
  ppr Mod    = text "mod"
  ppr Rem    = text "rem"
  ppr Plus   = text "+"
  ppr Minus  = text "-"
  ppr Concat = text "&"
  ppr Exp    = text "**"

instance Pretty UnOp where
  ppr Identity = text "+"
  ppr Negation = text "-"
  ppr Abs      = text "abs"
  ppr Not      = text "not"

instance Pretty FileDeclaration where
  ppr (FileDeclaration is s o) =
    text "file" <+>
    commasep (fmap ppr is) <> colon <+> ppr s <> ppr' o <> semi

instance Pretty FileOpenInformation where
  ppr (FileOpenInformation e n) = condL (text "open") e <+> text "is" <+> ppr n

instance Pretty FileTypeDefinition where
  ppr (FileTypeDefinition t) = text "file of" <+> ppr t

instance Pretty FormalDesignator where
  ppr (FDGeneric n)   = ppr n
  ppr (FDPort n)      = ppr n
  ppr (FDParameter n) = ppr n

instance Pretty FormalPart where
  ppr (FPDesignator d) = ppr d
  ppr (FPFunction n d) = ppr n <+> parens (ppr d)
  ppr (FPType t d)     = ppr t <+> parens (ppr d)

instance Pretty FullTypeDeclaration where
  ppr (FullTypeDeclaration i t) =
    text "type" <+> ppr i <+> text "is" <+> ppr t <> semi

instance Pretty FunctionCall where
  ppr (FunctionCall n p) = ppr n <> ppr (parens . ppr <$> p)

instance Pretty FunctionName where
  ppr (FNSelected n) = ppr n
  ppr (FNSimple n)   = ppr n
  ppr (FNOp n)       = ppr n

instance Pretty GenerateStatement where
  ppr (GenerateStatement l g d s) =
    ppr l <> colon `hangs`
    stack
      [ ppr g <+> text "generate"
      , cond indent' (pprlStack <$> d)
      , cond (const $ text "begin") d
      , indent' $ stack $ fmap ppr s
      , text "end generate" <+> ppr l <> semi
      ]

instance Pretty GenerationScheme where
  ppr (GSFor p) = text "for" <+> ppr p
  ppr (GSIf c)  = text "if"  <+> ppr c

instance Pretty GenericClause where
  ppr (GenericClause ls) = text "generic" <+> parens (ppr ls) <> semi

instance Pretty GenericMapAspect where
  ppr (GenericMapAspect as) = text "generic map" <+> parens (ppr as)

instance Pretty GroupConstituent where
  ppr (GCName n) = ppr n
  ppr (GCChar c) = ppr c

instance Pretty GroupTemplateDeclaration where
  ppr (GroupTemplateDeclaration i cs) =
    text "group" <+> ppr i <+> text "is" <+> parens (commasep (map ppr cs)) <> semi

instance Pretty GroupDeclaration where
  ppr (GroupDeclaration i n cs) =
    text "group" <+> ppr i <> colon <+> ppr n <+> parens (commasep (map ppr cs)) <> semi

instance Pretty GuardedSignalSpecification where
  ppr (GuardedSignalSpecification ss t) = ppr ss <> colon <+> ppr t

instance Pretty Identifier where
  ppr (Ident i)         = ppr i
  ppr (ExtendedIdent i) = char '\\' <> ppr i <> char '\\'
  ppr a@(AntiIdent i)   = pprAnti a i

instance Pretty IfStatement where
  ppr (IfStatement l (tc, ts) a e) =
    labels l $
    stack
      [ (text "if" <+> ppr tc <+> text "then") `hangs` vppr ts
      , elseIf' a
      , else' e
      , text "end if" <> ppr' l <> semi
      ]
    where
      elseIf' :: [(Condition, SequenceOfStatements)] -> Doc
      elseIf' =
        stack .
        fmap
          (\(c, ss) -> (text "elsif" <+> ppr c <+> text "then") `hangs` vppr ss)
      else' :: Maybe SequenceOfStatements -> Doc
      else' Nothing   = empty
      else' (Just ss) = text "else" `hangs` vppr ss

instance Pretty IncompleteTypeDeclaration where
  ppr (IncompleteTypeDeclaration i) = text "type" <+> ppr i <> semi

instance Pretty IndexConstraint where
  ppr (IndexConstraint rs) = parens (commasep $ map ppr rs)

instance Pretty IndexSpecification where
  ppr (ISRange r) = ppr r
  ppr (ISExp e)   = ppr e

instance Pretty IndexSubtypeDefinition where
  ppr (IndexSubtypeDefinition t) = ppr t <+> text "range" <+> text "<>"

instance Pretty IndexedName where
  ppr (IndexedName p es) = ppr p <> enclosesep lparen rparen comma (map ppr es)

instance Pretty InstantiatedUnit where
  ppr (IUComponent n) = text "component" <+> ppr n
  ppr (IUEntity n i)  = text "entity" <+> ppr n <+> cond parens i
  ppr (IUConfig n)    = text "configuration" <+> ppr n

instance Pretty InstantiationList where
  ppr (ILLabels ls) = commasep $ map ppr ls
  ppr ILOthers      = text "others"
  ppr ILAll         = text "all"

instance Pretty InterfaceDeclaration where
  ppr (InterfaceVariableDeclaration is m s e) =
    text "variable" <+>
    commasep (fmap ppr is) <> colon <>
    ppr' m <+> ppr s <+> condL (text ":=") e
  ppr (InterfaceSignalDeclaration is m s b e) =
    text "signal" <+>
    commasep (fmap ppr is) <> colon <>
    ppr' m <+> ppr s <+> when b (text "bus") <+> condL (text ":=") e
  ppr (InterfaceConstantDeclaration is s e) =
    text "constant" <+>
    commasep (fmap ppr is) <> colon <+>
    text "in" <+> ppr s <+> condL (text ":=") e
  ppr (InterfaceFileDeclaration is s) =
    text "file" <+> commasep (fmap ppr is) <> colon <+> ppr s

instance Pretty InterfaceList where
  ppr (InterfaceList es) = stack $ punctuate semi $ map ppr es

instance Pretty IterationScheme where
  ppr (IterWhile c) = text "while" <+> ppr c
  ppr (IterFor p)   = text "for" <+> ppr p

instance Pretty LibraryClause where
  ppr (LibraryClause ns) = text "library" <+> ppr ns <> semi

instance Pretty LibraryUnit where
  ppr (LibraryPrimary p)    = ppr p
  ppr (LibrarySecondary s)  = ppr s
  ppr a@(AntiLibraryUnit s) = pprAnti a s

instance Pretty Literal where
  ppr (LitNum n)       = ppr n
  ppr (LitEnum e)      = ppr e
  ppr (LitString s)    = ppr s
  ppr (LitBitString b) = ppr b
  ppr LitNull          = text "null"

instance Pretty LogicalNameList where
  ppr (LogicalNameList ns) = commasep $ fmap ppr ns

instance Pretty LoopStatement where
  ppr (LoopStatement l i ss) =
    labels l $
    stack
      [ (ppr i <+> text "loop") `hangs` vppr ss
      , text "end loop" <> ppr' l <> semi
      ]

instance Pretty Mode where
  ppr In      = text "in"
  ppr Out     = text "out"
  ppr InOut   = text "inout"
  ppr Buffer  = text "buffer"
  ppr Linkage = text "linkage"

instance Pretty Name where
  ppr (NSimple n)    = ppr n
  ppr (NOp o)        = ppr o
  ppr (NSelect s)    = ppr s
  ppr (NIndex i)     = ppr i
  ppr (NSlice s)     = ppr s
  ppr (NAttr a)      = ppr a
  ppr a@(AntiName n) = pprAnti a n

instance Pretty NextStatement where
  ppr (NextStatement l b c) =
    label l <+> text "next" <> ppr' b <> condL (space <> text "when") c <> semi

instance Pretty NullStatement where
  ppr (NullStatement l) = label l <+> text "null" <> semi

instance Pretty NumericLiteral where
  ppr (NLitAbstract a) = ppr a
  ppr (NLitPhysical p) = ppr p
  ppr a@(AntiLitnum n) = pprAnti a n

instance Pretty ObjectDeclaration where
  ppr (ObjConst c) = ppr c
  ppr (ObjSig s)   = ppr s
  ppr (ObjVar v)   = ppr v
  ppr (ObjFile f)  = ppr f

instance Pretty Options where
  ppr (Options g d) = when g (text "guarded") <> ppr' d

instance Pretty PackageBody where
  ppr (PackageBody n d) =
    stack
      [ text "package body" <+> ppr n <+> text "is"
      , indent' $ block d
      , text "end package body" <+> ppr n <> semi
      ]

instance Pretty PackageBodyDeclarativeItem where
  ppr (PBDISubprogDecl s) = ppr s
  ppr (PBDISubprogBody b) = ppr b
  ppr (PBDIType t)        = ppr t
  ppr (PBDISubtype s)     = ppr s
  ppr (PBDIConstant c)    = ppr c
  ppr (PBDIShared s)      = ppr s
  ppr (PBDIFile f)        = ppr f
  ppr (PBDIAlias a)       = ppr a
  ppr (PBDIUseClause u)   = ppr u
  ppr (PBDIGroupTemp g)   = ppr g
  ppr (PBDIGroup g)       = ppr g

instance Pretty PackageDeclaration where
  ppr (PackageDeclaration i d) =
    stack
      [ text "package" <+> ppr i <+> text "is"
      , indent' $ block d
      , text "end package" <+> ppr i <> semi
      ]

instance Pretty PackageDeclarativeItem where
  ppr (PHDISubprogDecl s) = ppr s
  ppr (PHDISubprogBody b) = ppr b
  ppr (PHDIType t)        = ppr t
  ppr (PHDISubtype s)     = ppr s
  ppr (PHDIConstant c)    = ppr c
  ppr (PHDISignal s)      = ppr s
  ppr (PHDIShared v)      = ppr v
  ppr (PHDIFile f)        = ppr f
  ppr (PHDIAlias a)       = ppr a
  ppr (PHDIComp c)        = ppr c
  ppr (PHDIAttrDecl a)    = ppr a
  ppr (PHDIAttrSpec a)    = ppr a
  ppr (PHDIDiscSpec d)    = ppr d
  ppr (PHDIUseClause u)   = ppr u
  ppr (PHDIGroupTemp g)   = ppr g
  ppr (PHDIGroup g)       = ppr g

instance Pretty ParameterSpecification where
  ppr (ParameterSpecification i r) = ppr i <+> text "in" <+> ppr r

instance Pretty PhysicalLiteral where
  ppr (PhysicalLiteral a n) = ppr a <+> ppr n

instance Pretty PhysicalTypeDefinition where
  ppr (PhysicalTypeDefinition c p s n) =
    ppr c `hangs`
    stack
      [ text "units"
      , indent' $ stack [ppr p <> semi, stack $ map (\x -> ppr x <> semi) s]
      , text "end units" <> ppr' n
      ]

instance Pretty PortClause where
  ppr (PortClause ls) = text "port" <+> parens (ppr ls) <> semi

instance Pretty PortMapAspect where
  ppr (PortMapAspect as) = text "port map" <+> parens (ppr as)

instance Pretty Prefix where
  ppr (PName n) = ppr n
  ppr (PFun f)  = ppr f

instance Pretty PrimaryUnit where
  ppr (PrimaryEntity e)     = ppr e
  ppr (PrimaryConfig c)     = ppr c
  ppr (PrimaryPackage p)    = ppr p
  ppr a@(AntiPrimaryUnit p) = pprAnti a p

instance Pretty ProcedureCall where
  ppr (ProcedureCall n ap) = ppr n <> cond parens ap

instance Pretty ProcedureCallStatement where
  ppr (ProcedureCallStatement l p) = label l <+> ppr p <> semi

instance Pretty ProcessDeclarativeItem where
  ppr (PDISubprogDecl s)  = ppr s
  ppr (PDISubprogBody b)  = ppr b
  ppr (PDIType t)         = ppr t
  ppr (PDISubtype s)      = ppr s
  ppr (PDIConstant c)     = ppr c
  ppr (PDIVariable v)     = ppr v
  ppr (PDIFile f)         = ppr f
  ppr (PDIAlias a)        = ppr a
  ppr (PDIAttrDecl a)     = ppr a
  ppr (PDIAttrSpec a)     = ppr a
  ppr (PDIUseClause u)    = ppr u
  ppr a@(AntiProcDecl s)  = pprAnti a s
  ppr a@(AntiProcDecls s) = pprAnti a s

instance Pretty ProcessStatement where
  ppr (ProcessStatement l p ss d s) =
    labels l $
    stack
      [ (post <+> cond parens ss <+> text "is") `hangs` vppr d
      , text "begin" `hangs` vppr s
      , text "end" <+> post <> ppr' l <> semi
      ]
    where
      post = when p (text "postponed") <+> text "process"

instance Pretty QualifiedExpression where
  ppr (QualExp t e) = ppr t <> char '\'' <> parens (ppr e)
  ppr (QualAgg t a) = ppr t <> char '\'' <> ppr a

instance Pretty Range where
  ppr (RAttr a)       = ppr a
  ppr (RSimple l d u) = ppr l <+> ppr d <+> ppr u

instance Pretty RangeConstraint where
  ppr (RangeConstraint r) = text "range" <+> ppr r

instance Pretty RecordTypeDefinition where
  ppr (RecordTypeDefinition es n) =
    stack [text "record", stack $ map ppr es, text "end record" <> ppr' n]

instance Pretty ReportStatement where
  ppr (ReportStatement l e s) =
    labels l (text "report" <+> ppr e <> condL (indent' $ text "severity") s) <> semi

instance Pretty ReturnStatement where
  ppr (ReturnStatement l e) = label l <+> text "return" <> ppr' e <> semi

instance Pretty ScalarTypeDefinition where
  ppr (ScalarEnum e)  = ppr e
  ppr (ScalarInt i)   = ppr i
  ppr (ScalarFloat f) = ppr f
  ppr (ScalarPhys p)  = ppr p

instance Pretty SecondaryUnit where
  ppr (SecondaryArchitecture a) = ppr a
  ppr (SecondaryPackage p)      = ppr p
  ppr a@(AntiSecondaryUnit v)   = pprAnti a v

instance Pretty SecondaryUnitDeclaration where
  ppr (SecondaryUnitDeclaration i p) = ppr i <+> equals <+> ppr p

instance Pretty SelectedName where
  ppr (SelectedName p s) = ppr p <> char '.' <> ppr s

instance Pretty SelectedSignalAssignment where
  ppr (SelectedSignalAssignment e t o w) =
    text "with" <+>
    ppr e <+> text "select" `hangs` ppr t <+> text "<=" <+> ppr o <+> ppr w <> semi

instance Pretty SelectedWaveforms where
  ppr (SelectedWaveforms ws) = stack (punctuate comma sws)
    where
      sws = map f ws
      f (w, c) = ppr w <+> text "when" <+> ppr c

instance Pretty SensitivityClause where
  ppr (SensitivityClause ss) = text "on" <+> ppr ss

instance Pretty SensitivityList where
  ppr (SensitivityList ns) = commasep $ map ppr ns

instance Pretty SequentialStatement where
  ppr (SWait w)         = ppr w
  ppr (SAssert a)       = ppr a
  ppr (SReport r)       = ppr r
  ppr (SSignalAss s)    = ppr s
  ppr (SVarAss v)       = ppr v
  ppr (SProc p)         = ppr p
  ppr (SIf i)           = ppr i
  ppr (SCase c)         = ppr c
  ppr (SLoop l)         = ppr l
  ppr (SNext n)         = ppr n
  ppr (SExit e)         = ppr e
  ppr (SReturn r)       = ppr r
  ppr (SNull n)         = ppr n
  ppr a@(AntiSeqStm s)  = pprAnti a s
  ppr a@(AntiSeqStms s) = pprAnti a s

instance Pretty SignalAssignmentStatement where
  ppr (SignalAssignmentStatement l t d w) =
    label l <+> ppr t <+> text "<=" <> ppr' d <> space <> ppr w <> semi

instance Pretty SignalDeclaration where
  ppr (SignalDeclaration is s k e) =
    text "signal" <+>
    commasep (fmap ppr is) <> colon <+>
    ppr s <> ppr' k <> condL ( space <> text ":=") e <> semi

instance Pretty SignalKind where
  ppr Register = text "register"
  ppr Bus      = text "bus"

instance Pretty SignalList where
  ppr (SLName ns) = commasep $ map ppr ns
  ppr SLOthers    = text "others"
  ppr SLAll       = text "all"

instance Pretty Signature where
  ppr (Signature (ts, t)) = brackets $ initial <+> condL (text "return") t
    where
      initial = commasep $ maybe [] (map ppr) ts

instance Pretty SliceName where
  ppr (SliceName p r) = ppr p <> parens (ppr r)

instance Pretty StringLiteral where
  ppr (SLit s) = char '\"' <> text (fixQuotes (T.unpack s)) <> char '\"'
    where
      fixQuote xs '"' = xs ++ "\"\""
      fixQuote xs x   = xs ++ [x]
      fixQuotes = foldl fixQuote []
  ppr a@(AntiSlit s) = pprAnti a s

instance Pretty SubprogramBody where
  ppr (SubprogramBody s d st k de) =
    stack
      [ ppr s <+> text "is"
      , indent' $ block d
      , text "begin"
      , indent' $ block st
      , text "end" <> ppr' k <> ppr' de <> semi <> line
      ]

instance Pretty SubprogramDeclarativeItem where
  ppr (SDISubprogDecl d) = ppr d
  ppr (SDISubprogBody b) = ppr b
  ppr (SDIType t)        = ppr t
  ppr (SDISubtype s)     = ppr s
  ppr (SDIConstant c)    = ppr c
  ppr (SDIVariable v)    = ppr v
  ppr (SDIFile f)        = ppr f
  ppr (SDIAlias a)       = ppr a
  ppr (SDIAttrDecl a)    = ppr a
  ppr (SDIAttrSpec a)    = ppr a
  ppr (SDIUseClause u)   = ppr u
  ppr (SDIGroupTemp g)   = ppr g
  ppr (SDIGroup g)       = ppr g

instance Pretty SubprogramKind where
  ppr Procedure = text "procedure"
  ppr Function  = text "function"

instance Pretty SubprogramDeclaration where
  ppr (SubprogramDeclaration s) = ppr s <> semi

instance Pretty SubprogramSpecification where
  ppr (SubprogramProcedure d fs) = text "procedure" <+> ppr d <+> cond parens fs
  ppr (SubprogramFunction p d fs t) =
    purity <+>
    text "function" <+> ppr d <+> cond parens fs <+> text "return" <+> ppr t
    where
      purity =
        case p of
          Nothing    -> empty
          Just True  -> text "pure"
          Just False -> text "impure"

instance Pretty SubtypeDeclaration where
  ppr (SubtypeDeclaration i s) =
    text "subtype" <+> ppr i <+> text "is" <+> ppr s <> semi

instance Pretty SubtypeIndication where
  ppr (SubtypeIndication n t c) = ppr' n <+> ppr t <> ppr' c

instance Pretty Suffix where
  ppr (SSimple n) = ppr n
  ppr (SChar c)   = ppr c
  ppr (SOp o)     = ppr o
  ppr SAll        = text "all"

instance Pretty Target where
  ppr (TargetName n) = ppr n
  ppr (TargetAgg a)  = ppr a

instance Pretty TimeoutClause where
  ppr (TimeoutClause e) = text "for" <+> ppr e

instance Pretty TypeConversion where
  ppr (TypeConversion t e) = ppr t <+> parens (ppr e)

instance Pretty TypeDeclaration where
  ppr (TDFull ft)    = ppr ft
  ppr (TDPartial pt) = ppr pt

instance Pretty TypeDefinition where
  ppr (TDScalar s)    = ppr s
  ppr (TDComposite c) = ppr c
  ppr (TDAccess a)    = ppr a
  ppr (TDFile f)      = ppr f

instance Pretty TypeMark where
  ppr (TMType n)    = ppr n
  ppr (TMSubtype n) = ppr n

instance Pretty UnconstrainedArrayDefinition where
  ppr (UnconstrainedArrayDefinition is s) =
    text "array" <+> parens (commasep $ map ppr is) <+> text "of" <+> ppr s

instance Pretty UseClause where
  ppr (UseClause ns) = text "use" <+> commasep (map ppr ns) <> semi

instance Pretty VariableAssignmentStatement where
  ppr (VariableAssignmentStatement l t e) =
    label l <+> ppr t <+> text ":=" <+> ppr e <> semi

instance Pretty VariableDeclaration where
  ppr (VariableDeclaration s is sub e) =
    when s (text "shared") <+>
    text "variable" <+>
    commasep (fmap ppr is) <> colon <+> ppr sub <> condL (space <> text ":=") e <> semi

instance Pretty WaitStatement where
  ppr (WaitStatement l sc cc tc) =
    label l <+> text "wait" <> ppr' sc <> ppr' cc <> ppr' tc <> semi

instance Pretty Waveform where
  ppr (WaveElem es)  = commasep $ map ppr es
  ppr WaveUnaffected = text "unaffected"
  ppr a@(AntiWave v) = pprAnti a v

instance Pretty WaveformElement where
  ppr (WaveEExp e te) = ppr e <+> condL (text "after") te
  ppr (WaveENull te)  = text "null" <+> condL (text "after") te

--------------------------------------------------------------------------------
-- * Some helpers
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- text sep.

pipeSep :: [Doc] -> Doc
pipeSep = stack . punctuate (char '|')

--------------------------------------------------------------------------------
-- indentation

il :: Int
il = 4

indent' :: Doc -> Doc
indent' = indent il

hang' :: Doc -> Doc
hang' = hang il

hangs :: Doc -> Doc -> Doc
hangs d1 d2 = hang' (d1 </> d2)

labels
  :: Pretty a
  => Maybe a -> Doc -> Doc
labels Nothing doc  = doc
labels (Just a) doc = ppr a <> colon <+> doc

block
  :: Pretty a
  => [a] -> Doc
block = stack . map ppr

--------------------------------------------------------------------------------
-- conditional print
cond
  :: Pretty a
  => (Doc -> Doc) -> Maybe a -> Doc
cond f = maybe empty (f . ppr)

condR
  :: Pretty a
  => Doc -> Maybe a -> Doc
condR s = cond (<+> s)

-- No space variant of condR
condR'
  :: Pretty a
  => Doc -> Maybe a -> Doc
condR' s = cond (<> s)

condL
  :: Pretty a
  => Doc -> Maybe a -> Doc
condL s = cond (s <+>)

condL'
  :: Pretty a
  => Doc -> Maybe a -> Doc
condL' s = cond (s <>)

label
  :: Pretty a
  => Maybe a -> Doc
label = cond (<> colon)

ppr'
  :: Pretty a
  => Maybe a -> Doc
ppr' d = ppr $ catL space d

when :: Bool -> Doc -> Doc
when b a =
  if b
    then a
    else empty

catL :: (Pretty a, Functor f) => Doc -> f a -> f Doc
catL d e = catL' d <$> e

catL' :: (Pretty a) => Doc -> a -> Doc
catL' d e = d <> ppr e

appL :: (Pretty a, Functor f) => (Doc -> Doc) -> f a -> f Doc
appL f e = appL' f <$> e

appL' :: (Pretty a) => (Doc -> Doc) -> a -> Doc
appL' f e = f $ ppr e

--------------------------------------------------------------------------------
-- some common things
vppr
  :: Pretty a
  => [a] -> Doc
vppr = foldr ((</>) . ppr) empty

postponed
  :: Pretty a
  => Maybe Label -> Bool -> a -> Doc
postponed l b a = label l <+> when b (text "postponed") <+> ppr a

pprr :: (Pretty a) => a -> String
pprr = pretty 80 . ppr

pprrText :: (Pretty a) => a -> T.Text
pprrText d = toStrict $ prettyLazyText 80 (ppr d)

pprAnti
  :: (Data a)
  => a -> String -> Doc
pprAnti s t = char '$' <> text (toQQString s) <> char ':' <> text t

intToBase :: Integer -> Integer -> String
intToBase b n = showIntAtBase b intToDigit n ""

pprlStack :: (Pretty a) => [a] -> Doc
pprlStack d = stack $ map ppr d
--------------------------------------------------------------------------------
