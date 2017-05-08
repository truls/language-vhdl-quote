{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}

module Language.VHDL.Pretty (Pretty (..), ppr) where

import Language.VHDL.Syntax
import Text.PrettyPrint     hiding (Mode)

--------------------------------------------------------------------------------
-- * Pretty printing class
--------------------------------------------------------------------------------

class Pretty a
  where
    pp :: a -> Doc

instance Pretty a => Pretty [a]
  where
    pp = hsep . map pp

instance Pretty a => Pretty (Maybe a)
  where
    pp = maybe empty pp

--------------------------------------------------------------------------------
-- ** Pretty printing instances

instance Pretty AbstractLiteral where
  pp (ALitDecimal d) = pp d
  pp (ALitBased   b) = pp b

instance Pretty AccessTypeDefinition where
  pp (AccessTypeDefinition s) = text "access" <+> pp s

instance Pretty ActualDesignator where
  pp (ADExpression e) = pp e
  pp (ADSignal n)     = pp n
  pp (ADVariable n)   = pp n
  pp (ADFile n)       = pp n
  pp (ADOpen)         = text "open"

--instance Pretty ActualParameterPart where pp = undefined

instance Pretty ActualPart where
  pp (APDesignator a) = pp a
  pp (APFunction f a) = pp f <+> parens (pp a)
  pp (APType t a)     = pp t <+> parens (pp a)

-- instance Pretty AddingOperator where
--   pp (Plus)   = char '+'
--   pp (Minus)  = char '-'
--   pp (Concat) = char '&'

instance Pretty Aggregate where
  pp (Aggregate es) = parens (commaSep $ map pp es)

instance Pretty AliasDeclaration where
  pp (AliasDeclaration a sub n sig) =
        text "alias" <+> pp a
    <+> cond (colon <+>) sub
    <+> text "is" <+> pp n
    <+> cond id sig <> semi

instance Pretty AliasDesignator where
  pp (ADIdentifier i) = pp i
  pp (ADCharacter c)  = pp c
  pp (ADOperator o)   = pp o

instance Pretty Allocator where
  pp (AllocSub s)  = text "new" <+> pp s
  pp (AllocQual q) = text "new" <+> pp q

instance Pretty ArchitectureBody where
  pp (ArchitectureBody i n d s) =
      vcat [ header
           , indent (vpp d)
           , text "begin"
           , indent (vpp s)
           , footer
           ]
    where
      header = text "architecture" <+> pp i
           <+> text "of" <+> pp n
           <+> text "is"
      footer = text "end architecture" <+> pp i <> semi

--instance Pretty ArchitectureDeclarativePart where pp = undefined

--instance Pretty ArchitectureStatementPart where pp = undefined

instance Pretty ArrayTypeDefinition where
  pp (ArrU u) = pp u
  pp (ArrC c) = pp c

instance Pretty Assertion where
  pp (Assertion c r s) = vcat [text "assert" <+> pp c, report, severity]
    where
      report   = indent $ cond (text "report" <+>) r
      severity = indent $ cond (text "severity" <+>) s

instance Pretty AssertionStatement where
  pp (AssertionStatement l a) = label l <+> pp a <> semi

instance Pretty AssociationElement where
  pp (AssociationElement f a) = condR (text "=>") f <+> pp a

instance Pretty AssociationList where
  pp (AssociationList as) = vcat $ punctuate comma $ map pp as

instance Pretty AttributeDeclaration where
  pp (AttributeDeclaration i t) = text "attribute" <+> pp i <> colon <+> pp t <> semi

--instance Pretty AttributeDesignator where pp = undefined

instance Pretty AttributeName where
  pp (AttributeName p s d e) = pp p <+> cond id s <> char '\'' <> pp d <+> cond parens e

instance Pretty AttributeSpecification where
  pp (AttributeSpecification d s e) =
        text "attribute" <+> pp d
    <+> text "of" <+> pp s
    <+> text "is" <+> pp e <> semi

instance Pretty BaseSpecifier where pp = error "missing: BaseSpecifier" -- todo

instance Pretty BaseUnitDeclaration where pp = error "missing: BaseUnitDeclaration" -- todo

instance Pretty BasedLiteral where
  pp (BasedLiteral b i f e) = pp b <+> char '#' <+> pp i <+> condL (char '.') f <+> char '#' <+> cond id e

instance Pretty BasicCharacter where pp = error "missing: BasicCharacter" -- todo

instance Pretty BasicGraphicCharacter where pp = error "missing: BasicGraphicCharacter" -- todo

instance Pretty BasicIdentifier where pp = error "missing: BasicIdentifier" -- todo

instance Pretty BindingIndication where
  pp (BindingIndication e g p) =
    vcat [condR (text "USE") e, cond id g, cond id p]

instance Pretty BitStringLiteral where pp = error "missing: BitStringLiteral" -- todo

instance Pretty BitValue where pp = error "missing: BitValue" -- todo

instance Pretty BlockConfiguration where
  pp (BlockConfiguration s u c) =
    vcat [ text "for" <+> pp s
         , indent (pp u)
         , indent (pp c)
         , text "end for" <> semi]

instance Pretty BlockDeclarativeItem where
  pp (BDISubprogDecl d) = pp d
  pp (BDISubprogBody b) = pp b
  pp (BDIType t)        = pp t
  pp (BDISubtype s)     = pp s
  pp (BDIConstant c)    = pp c
  pp (BDISignal s)      = pp s
  pp (BDIShared v)      = pp v
  pp (BDIFile f)        = pp f
  pp (BDIAlias a)       = pp a
  pp (BDIComp c)        = pp c
  pp (BDIAttrDecl a)    = pp a
  pp (BDIAttrSepc a)    = pp a
  pp (BDIConfigSepc c)  = pp c
  pp (BDIDisconSpec d)  = pp d
  pp (BDIUseClause u)   = pp u
  pp (BDIGroupTemp g)   = pp g
  pp (BDIGroup g)       = pp g

--instance Pretty BlockDeclarativePart where pp = undefined

instance Pretty BlockHeader where
  pp (BlockHeader p g) =
      vcat [go p, go g]
    where
      go :: (Pretty a, Pretty b) => Maybe (a, Maybe b) -> Doc
      go (Nothing)      = empty
      go (Just (a, mb)) = pp a $+$ cond indent mb

instance Pretty BlockSpecification where
  pp (BSArch n)  = pp n
  pp (BSBlock l) = pp l
  pp (BSGen l i)   = pp l <+> cond parens i

instance Pretty BlockStatement where
  pp (BlockStatement l g h d s) =
      pp l <> colon `hangs` vcat [header, body, footer]
    where
      header = text "block" <+> cond parens g <+> text "is" `hangs` (pp h $$ pp d)
      body   = text "begin" `hangs` (pp s)
      footer = text "end block" <+> pp l

--instance Pretty BlockStatementPart where pp = undefined

instance Pretty CaseStatement where
  pp (CaseStatement l e cs) =
      labels l $ vcat [header, body, footer]
    where
      header = text "case" <+> pp e <+> text "is"
      body   = indent $ vcat $ map pp cs
      footer = text "end case" <+> cond id l <> semi

instance Pretty CaseStatementAlternative where
  pp (CaseStatementAlternative c ss) =
    vcat [ text "when" <+> pp c <+> text "=>"
         , indent $ vcat $ map pp ss]

instance Pretty CharacterLiteral where
  pp (CLit c) = char '\'' <> char c <> char '\''

instance Pretty Choice where
  pp (ChoiceSimple s) = pp s
  pp (ChoiceRange r)  = pp r
  pp (ChoiceName n)   = pp n
  pp (ChoiceOthers)   = text "others"

instance Pretty Choices where
  pp (Choices cs) = pipeSep $ map pp cs

instance Pretty ComponentConfiguration where
  pp (ComponentConfiguration s i c) =
    vcat [ text "for" <+> pp s
         , indent $ vcat
           [ condR semi i
           , cond  id c
           ]
         , text "end for" <> semi
         ]

instance Pretty ComponentDeclaration where
  pp (ComponentDeclaration i g p) =
    vcat [ text "component" <+> pp i <+> text "is"
         , indent $ vcat
           [ cond id g
           , cond id p
           ]
         , text "end component" <+> pp i <> semi
         ]

instance Pretty ComponentInstantiationStatement where
  pp (ComponentInstantiationStatement l u g p) =
    pp l <> colon `hangs` (pp u `hangs` vcat [cond id g, cond id p])

instance Pretty ComponentSpecification where
  pp (ComponentSpecification ls n) = pp ls <> colon <+> pp n

instance Pretty CompositeTypeDefinition where
  pp (CTDArray at)  = pp at
  pp (CTDRecord rt) = pp rt

instance Pretty ConcurrentAssertionStatement where
  pp (ConcurrentAssertionStatement l p a) = postponed l p a

instance Pretty ConcurrentProcedureCallStatement where
  pp (ConcurrentProcedureCallStatement l p a) = postponed l p a

instance Pretty ConcurrentSignalAssignmentStatement where
  pp (CSASCond l p a)   = postponed l p a
  pp (CSASSelect l p a) = postponed l p a

instance Pretty ConcurrentStatement where
  pp (ConBlock b)     = pp b
  pp (ConProcess p)   = pp p
  pp (ConProcCall c)  = pp c
  pp (ConAssertion a) = pp a
  pp (ConSignalAss s) = pp s
  pp (ConComponent c) = pp c
  pp (ConGenerate g)  = pp g

--instance Pretty Condition where pp = undefined

instance Pretty ConditionClause where
  pp (ConditionClause e) = text "until" <+> pp e

instance Pretty ConditionalSignalAssignment where
  pp (ConditionalSignalAssignment t o w) = pp t <+> text "<=" <+> pp o <+> pp w <> semi

instance Pretty ConditionalWaveforms where
  pp (ConditionalWaveforms ws (w, c)) =
      vcat ws' $$ pp w <+> condL (text "when") c
    where
      ws' = map (\(w', c') -> pp w' <+> text "when" <+> pp c' <+> text "else") ws

instance Pretty ConfigurationDeclaration where
  pp (ConfigurationDeclaration i n d b) =
    vcat [ text "configuration" <+> pp i <+> text "of" <+> pp n <+> text "is"
         , indent $ vcat
           [ pp d
           , pp b
           ]
         , text "end configuration" <+> pp i
         ]

instance Pretty ConfigurationDeclarativeItem where
  pp (CDIUse u)      = pp u
  pp (CDIAttrSpec a) = pp a
  pp (CDIGroup g)    = pp g

--instance Pretty ConfigurationDeclarativePart where pp = undefined

instance Pretty ConfigurationItem where
  pp (CIBlock b) = pp b
  pp (CIComp c)  = pp c

instance Pretty ConfigurationSpecification where
  pp (ConfigurationSpecification s i) = text "for" <+> pp s <+> pp i <> semi

instance Pretty ConstantDeclaration where
  pp (ConstantDeclaration is s e) =
    text "constant" <+> commaSep (fmap pp is) <> colon <+> pp s <+> condL (text ":=") e <> semi

instance Pretty ConstrainedArrayDefinition where
  pp (ConstrainedArrayDefinition i s) = text "array" <+> pp i <+> text "of" <+> pp s

instance Pretty Constraint where
  pp (CRange r) = pp r
  pp (CIndex i) = pp i

instance Pretty ContextClause where
  pp (ContextClause items) = vcat $ fmap pp items

instance Pretty ContextItem where
  pp (ContextLibrary l) = pp l
  pp (ContextUse u)     = pp u

instance Pretty DecimalLiteral where
  pp (DecimalLiteral i f e) = pp i <+> condL (char '.') f <+> cond id e

instance Pretty Declaration where
  pp (DType t)          = pp t
  pp (DSubtype s)       = pp s
  pp (DObject o)        = pp o
  pp (DAlias a)         = pp a
  pp (DComponent c)     = pp c
  pp (DAttribute a)     = pp a
  pp (DGroupTemplate g) = pp g
  pp (DGroup g)         = pp g
  pp (DEntity e)        = pp e
  pp (DConfiguration c) = pp c
  pp (DSubprogram s)    = pp s
  pp (DPackage p)       = pp p

instance Pretty DelayMechanism where
  pp (DMechTransport)  = text "transport"
  pp (DMechInertial e) = condL (text "reject") e <+> text "inertial"

instance Pretty DesignFile where
  pp (DesignFile units) = vcat $ fmap pp units

instance Pretty DesignUnit where
  pp (DesignUnit ctxt lib) = vcat [pp ctxt, pp lib]

instance Pretty Designator where
  pp (DId i) = pp i
  pp (DOp o) = pp o

instance Pretty Direction where
  pp (To)     = text "to"
  pp (DownTo) = text "downto"

instance Pretty DisconnectionSpecification where
  pp (DisconnectionSpecification g e) =
    text "disconnect" <+> pp g <+> text "after" <+> pp e <> semi

instance Pretty DiscreteRange where
  pp (DRSub s)   = pp s
  pp (DRRange r) = pp r

instance Pretty ElementAssociation where
  pp (ElementAssociation c e) = condR (text "=>") c <+> pp e

instance Pretty ElementDeclaration where
  pp (ElementDeclaration is s) = pp is <> colon <+> pp s <> semi

--instance Pretty ElementSubtypeDefinition where pp = undefined

instance Pretty EntityAspect where
  pp (EAEntity n i) = text "entity" <+> pp n <+> cond parens i
  pp (EAConfig n)   = text "configuration" <+> pp n
  pp (EAOpen)       = text "open"

instance Pretty EntityClass where
  pp ENTITY        = text "ENTITY"
  pp ARCHITECTURE  = text "ARCHITECTURE"
  pp CONFIGURATION = text "CONFIGURATION"
  pp PROCEDURE     = text "PROCEDURE"
  pp FUNCTION      = text "FUNCTION"
  pp PACKAGE       = text "PACKAGE"
  pp TYPE          = text "TYPE"
  pp SUBTYPE       = text "SUBTYPE"
  pp CONSTANT      = text "CONSTANT"
  pp SIGNAL        = text "SIGNAL"
  pp VARIABLE      = text "VARIABLE"
  pp COMPONENT     = text "COMPONENT"
  pp LABEL         = text "LABEL"
  pp LITERAL       = text "LITERAL"
  pp UNITS         = text "UNITS"
  pp GROUP         = text "GROUP"
  pp FILE          = text "FILE"
  pp Entity        = text "entity"
  pp Architecture  = text "architecture"
  pp Configuration = text "configuration"
  pp Procedure'     = text "procedure"
  pp Function'      = text "function"
  pp Package       = text "package"
  pp Type          = text "type"
  pp Subtype       = text "subtype"
  pp Constant'      = text "constant"
  pp Signal        = text "signal"
  pp Variable      = text "variable"
  pp Component     = text "component"
  pp Label         = text "label"
  pp Literal       = text "literal"
  pp Units         = text "units"
  pp Group         = text "group"
  pp File          = text "file"
  pp Constant      = text "constant"

instance Pretty EntityClassEntry where
  pp (EntityClassEntry c m) = pp c <+> when m (text "<>")

--instance Pretty EntityClassEntryList where pp = undefined

instance Pretty EntityDeclaration where
  pp (EntityDeclaration i h d s) =
    vcat [ text "entity" <+> pp i <+> text "is"
         , indent $ vcat
           [ pp h
           , pp d
           ]
         , flip cond s $ \ss ->
             text "begin" `hangs` ss
         , text "end entity" <+> pp i <> semi $+$ text ""
         ]

instance Pretty EntityDeclarativeItem where
  pp (EDISubprogDecl s)  = pp s
  pp (EDISubprogBody b)  = pp b
  pp (EDIType t)         = pp t
  pp (EDISubtype s)      = pp s
  pp (EDIConstant c)     = pp c
  pp (EDISignal s)       = pp s
  pp (EDIShared s)       = pp s
  pp (EDIFile f)         = pp f
  pp (EDIAlias a)        = pp a
  pp (EDIAttrDecl a)     = pp a
  pp (EDIAttrSpec a)     = pp a
  pp (EDIDiscSpec d)     = pp d
  pp (EDIUseClause u)    = pp u
  pp (EDIGroupTemp g)    = pp g
  pp (EDIGroup g)        = pp g

--instance Pretty EntityDeclarativePart where pp = undefined

instance Pretty EntityDesignator where
  pp (EntityDesignator t s) = pp t <+> cond id s

instance Pretty EntityHeader where
  pp (EntityHeader g p) = vcat [cond indent g, cond indent p]

instance Pretty EntityNameList where
  pp (ENLDesignators es) = commaSep $ fmap pp es
  pp ENLOthers = text "others"
  pp ENLAll = text "all"

instance Pretty EntitySpecification where
  pp (EntitySpecification ns c) = pp ns <> colon <+> pp c

instance Pretty EntityStatement where
  pp (ESConcAssert a)  = pp a
  pp (ESPassiveConc p) = pp p
  pp (ESPassiveProc p) = pp p

--instance Pretty EntityStatementPart where pp = undefined

instance Pretty EntityTag where
  pp (ETName n) = pp n
  pp (ETChar c) = pp c
  pp (ETOp o)   = pp o

instance Pretty EnumerationLiteral where
  pp (EId i)   = pp i
  pp (EChar c) = pp c

instance Pretty EnumerationTypeDefinition where
  pp (EnumerationTypeDefinition es) = commaSep $ fmap pp es

instance Pretty ExitStatement where
  pp (ExitStatement l b c) =
    label l <+> text "exit" <+> cond id b <+> condL (text "when") c <> semi

instance Pretty Exponent where
  pp (ExponentPos i) = char 'e' <+> pp i
  pp (ExponentNeg i) = char 'e' <+> char '-' <+> pp i

-- instance Pretty Expression where
--   pp (ECondExpr p) = pp p
--   pp (ELogicExpr e) = pp e

-- instance Pretty LogicalExpression where
--   pp (EAnd rs)    = textSep "AND"  $ map pp rs
--   pp (EOr rs)     = textSep "OR"   $ map pp rs
--   pp (EXor rs)    = textSep "XOR"  $ map pp rs
--   pp (ENand r rs) = pp r <+> condL (text "NAND") rs
--   pp (ENor r rs)  = pp r <+> condL (text "NOR")  rs
--   pp (EXnor rs)   = textSep "XNOR" $ map pp rs


instance Pretty Expression where
  pp (Unary op e) = pp op <+> pp e -- <+> (text "foo")
  pp (Binary op e1 e2) = pp e1 <+> pp op <+> pp e2
  pp (PrimName n) = pp n
  pp (PrimLit l) = pp l
  pp (PrimAgg a) = pp a
  pp (PrimFun f) = pp f
  pp (PrimQual q) = pp q
  pp (PrimTCon t) = pp t
  pp (PrimAlloc a) = pp a
  pp (PrimExp e) = text "(" <> pp e <> text ")"

instance Pretty TimeExpression where
  pp (TimeExpression l) = pp l

instance Pretty ExtendedDigit where pp = error "missing: ExtendedDigit" -- todo

instance Pretty ExtendedIdentifier where pp = error "missing: ExtendedIdentifier" -- todo

instance Pretty BinOp where
  pp And = text "and"
  pp Or = text "or"
  pp Nand = text "nand"
  pp Nor = text "nor"
  pp Xor = text "xor"
  pp Xnor = text "xnor"
  pp Eq = text "="
  pp Neq = text "/="
  pp Lt = text "<"
  pp Lte = text "<="
  pp Gt = text ">"
  pp Gte = text ">="
  pp Sll = text "sll"
  pp Srl = text "srl"
  pp Sla = text "sla"
  pp Sra = text "sra"
  pp Rol = text "rol"
  pp Ror = text "ror"
  pp Times = text "*"
  pp Div = text "/"
  pp Mod = text "mor"
  pp Rem = text "rem"
  pp Plus = text "+"
  pp Minus = text "-"
  pp Concat = text "&"
  pp Exp = text "**"

instance Pretty UnOp where
  pp Identity = text "+"
  pp Negation = text "-"
  pp Abs = text "abs"
  pp Not = text "not"

-- instance Pretty Factor where
--   pp (FacPrim p mp) = pp p <+> condL (text "**") mp
--   pp (FacAbs p)     = text "abs" <+> pp p
--   pp (FacNot p)     = text "not" <+> pp p

instance Pretty FileDeclaration where
  pp (FileDeclaration is s o) =
        text "file" <+> commaSep (fmap pp is)
    <> colon <+> pp s <+> cond id o <> semi

--instance Pretty FileLogicalName where pp = undefined

instance Pretty FileOpenInformation where
  pp (FileOpenInformation e n) = condL (text "open") e <+> text "is" <+> pp n

instance Pretty FileTypeDefinition where
  pp (FileTypeDefinition t) = text "file of" <+> pp t

--instance Pretty FloatingTypeDefinition where pp = undefined

instance Pretty FormalDesignator where
  pp (FDGeneric n)   = pp n
  pp (FDPort n)      = pp n
  pp (FDParameter n) = pp n

--instance Pretty FormalParameterList where pp = undefined

instance Pretty FormalPart where
  pp (FPDesignator d) = pp d
  pp (FPFunction n d) = pp n <+> parens (pp d)
  pp (FPType t d)     = pp t <+> parens (pp d)

instance Pretty FullTypeDeclaration where
  pp (FullTypeDeclaration i t) = text "type" <+> pp i <+> text "is" <+> pp t <> semi

instance Pretty FunctionCall where
  pp (FunctionCall n p) = pp n <+> cond parens p

instance Pretty GenerateStatement where
  pp (GenerateStatement l g d s) =
    pp l <> colon `hangs` vcat
      [ pp g <+> text "generate"
      , cond indent d
      , cond (const $ text "begin") d
      , indent $ vcat $ fmap pp s
      , text "end generate" <+> pp l <> semi
      ]

instance Pretty GenerationScheme where
  pp (GSFor p) = pp p
  pp (GSIf c)  = pp c

instance Pretty GenericClause where
  pp (GenericClause ls) = text "generic" <+> parens (pp ls) <> semi

--instance Pretty GenericList where pp = undefined

instance Pretty GenericMapAspect where
  pp (GenericMapAspect as) = text "generic map" <+> parens (pp as) <> semi

instance Pretty GraphicCharacter where pp = error "missing: GraphicCharacter" -- todo

instance Pretty GroupConstituent where
  pp (GCName n) = pp n
  pp (GCChar c) = pp c

--instance Pretty GroupConstituentList where pp = undefined

instance Pretty GroupTemplateDeclaration where
  pp (GroupTemplateDeclaration i cs) = text "group" <+> pp i <+> text "is" <+> parens (pp cs) <> semi

instance Pretty GroupDeclaration where
  pp (GroupDeclaration i n cs) = text "group" <+> pp i <> colon <+> pp n <+> parens (pp cs) <> semi

instance Pretty GuardedSignalSpecification where
  pp (GuardedSignalSpecification ss t) = pp ss <> colon <+> pp t

instance Pretty Identifier where
  pp (Ident i) = text i

--instance Pretty IdentifierList where pp = undefined

instance Pretty IfStatement where
  pp (IfStatement l (tc, ts) a e) =
    labels l $ vcat
      [ (text "if" <+> pp tc <+> text "then") `hangs` vpp ts
      , elseIf' a
      , else'   e
      , text "end if" <+> cond id l <> semi
      ]
    where
      elseIf' :: [(Condition, SequenceOfStatements)] -> Doc
      elseIf' = vcat . fmap (\(c, ss) -> (text "elsif" <+> pp c <+> text "then") `hangs` (vpp ss))

      else'   :: Maybe SequenceOfStatements -> Doc
      else' (Nothing) = empty
      else' (Just ss) = text "else" `hangs` (vpp ss)

instance Pretty IncompleteTypeDeclaration where
  pp (IncompleteTypeDeclaration i) = text "type" <+> pp i <> semi

instance Pretty IndexConstraint where
  pp (IndexConstraint rs) = parens (commaSep $ map pp rs)

instance Pretty IndexSpecification where
  pp (ISRange r) = pp r
  pp (ISExp e)   = pp e

instance Pretty IndexSubtypeDefinition where
  pp (IndexSubtypeDefinition t) = pp t <+> text "range" <> semi

instance Pretty IndexedName where
  pp (IndexedName p es) = pp p <+> parens (commaSep $ map pp es)

instance Pretty InstantiatedUnit where
  pp (IUComponent n) = text "component" <+> pp n
  pp (IUEntity n i)  = text "entity" <+> pp n <+> cond parens i
  pp (IUConfig n)    = text "configuration" <+> pp n

instance Pretty InstantiationList where
  pp (ILLabels ls) = commaSep $ map pp ls
  pp (ILOthers)    = text "others"
  pp (ILAll)       = text "all"

instance Pretty Integer where pp = integer

--instance Pretty IntegerTypeDefinition where pp = undefined

-- instance Pretty InterfaceDeclaration where
--   pp (ICDecl d) = pp d
--   pp (ISDecl d) = pp d
--   pp (IVDecl d) = pp d
--   pp (IFDecl d) = pp d

-- instance Pretty InterfaceConstantDeclaration where
--   pp (InterfaceConstantDeclaration is s e) =
--     text "constant" <+> commaSep (fmap pp is) <> colon <+> text "in" <+> pp s <+> condL (text ":=") e

-- instance Pretty InterfaceSignalDeclaration where
--   pp (InterfaceSignalDeclaration is m s b e) =
--     text "signal" <+> commaSep (fmap pp is) <> colon <+> cond id m <+> pp s <+> when b (text "bus") <+> condL (text ":=") e

-- instance Pretty InterfaceVariableDeclaration where
--   pp (InterfaceVariableDeclaration is m s e) =
--     text "variable" <+> commaSep (fmap pp is) <> colon <+> cond id m <+> pp s <+> condL (text ":=") e

-- instance Pretty InterfaceFileDeclaration where
--   pp (InterfaceFileDeclaration is s) =
--     text "file" <+> commaSep (fmap pp is) <> colon <+> pp s

instance Pretty InterfaceDeclaration where
  pp (InterfaceVariableDeclaration is m s e) =
    text "variable" <+> commaSep (fmap pp is) <> colon <+> cond id m <+> pp s <+> condL (text ":=") e
  pp (InterfaceSignalDeclaration is m s b e) =
    text "signal" <+> commaSep (fmap pp is) <> colon <+> cond id m <+> pp s <+> when b (text "bus") <+> condL (text ":=") e
  pp (InterfaceConstantDeclaration is s e) =
    text "constant" <+> commaSep (fmap pp is) <> colon <+> text "in" <+> pp s <+> condL (text ":=") e
  pp (InterfaceFileDeclaration is s) =
    text "file" <+> commaSep (fmap pp is) <> colon <+> pp s

--instance Pretty InterfaceElement where pp = undefined

instance Pretty InterfaceList where
  pp (InterfaceList es) = vcat $ punctuate semi $ map pp es

instance Pretty IterationScheme where
  pp (IterWhile c) = text "while" <+> pp c
  pp (IterFor p)   = text "for" <+> pp p

--instance Pretty Label where pp = undefined

instance Pretty Letter where pp = error "missing: Letter" -- todo

instance Pretty LetterOrDigit where pp = error "missing: LetterOrDigit" -- todo

instance Pretty LibraryClause where
  pp (LibraryClause ns) = text "library" <+> pp ns <> semi

instance Pretty LibraryUnit where
  pp (LibraryPrimary p)   = pp p
  pp (LibrarySecondary s) = pp s

instance Pretty Literal where
  pp (LitNum n)       = pp n
  pp (LitEnum e)      = pp e
  pp (LitString s)    = pp s
  pp (LitBitString b) = pp b
  pp (LitNull)        = text "null"

instance Pretty LogicalNameList where
  pp (LogicalNameList ns) = commaSep $ fmap pp ns

-- instance Pretty LogicalOperator where
--   pp (And)  = text "and"
--   pp (Or)   = text "or"
--   pp (Nand) = text "nand"
--   pp (Nor)  = text "nor"
--   pp (Xor)  = text "xor"
--   pp (Xnor) = text "xnor"

instance Pretty LoopStatement where
  pp (LoopStatement l i ss) =
    labels l $ vcat
      [ (cond id i <+> text "loop")
        `hangs` vpp ss
      , text "end loop" <+> cond id l <> semi
      ]

-- instance Pretty MiscellaneousOperator where
--   pp (Exp) = text "**"
--   pp (Abs) = text "abs"
--   pp (Not) = text "not"

instance Pretty Mode where
  pp (In)      = text "in"
  pp (Out)     = text "out"
  pp (InOut)   = text "inout"
  pp (Buffer)  = text "buffer"
  pp (Linkage) = text "linkage"

-- instance Pretty MultiplyingOperator where
--   pp (Times) = char '*'
--   pp (Div)   = char '/'
--   pp (Mod)   = text "mod"
--   pp (Rem)   = text "rem"

instance Pretty Name where
  pp (NSimple n) = pp n
  pp (NOp o)     = pp o
  pp (NSelect s) = pp s
  pp (NIndex i)  = pp i
  pp (NSlice s)  = pp s
  pp (NAttr a)   = pp a

instance Pretty NextStatement where
  pp (NextStatement l b c) = label l <+> text "next" <+> cond id b <+> condL (text "when") c <> semi

instance Pretty NullStatement where
  pp (NullStatement l) = label l <+> text "null"

instance Pretty NumericLiteral where
  pp (NLitAbstract a) = pp a
  pp (NLitPhysical p) = pp p

instance Pretty ObjectDeclaration where
  pp (ObjConst c) = pp c
  pp (ObjSig s)   = pp s
  pp (ObjVar v)   = pp v
  pp (ObjFile f)  = pp f

--instance Pretty OperatorSymbol where pp = undefined

instance Pretty Options where
  pp (Options g d) = when g (text "guarded") <+> cond id d

instance Pretty PackageBody where
  pp (PackageBody n d) =
    vcat [ text "package body" <+> pp n <+> text "is"
         , indent $ block d
         , text "end package body" <+> pp n <> semi
         ]

instance Pretty PackageBodyDeclarativeItem where
  pp (PBDISubprogDecl s) = pp s
  pp (PBDISubprogBody b) = pp b
  pp (PBDIType t)        = pp t
  pp (PBDISubtype s)     = pp s
  pp (PBDIConstant c)    = pp c
  pp (PBDIShared s)      = pp s
  pp (PBDIFile f)        = pp f
  pp (PBDIAlias a)       = pp a
  pp (PBDIUseClause u)   = pp u
  pp (PBDIGroupTemp g)   = pp g
  pp (PBDIGroup g)       = pp g

--Instance Pretty PackageBodyDeclarativePart where pp = undefined

instance Pretty PackageDeclaration where
  pp (PackageDeclaration i d) =
    vcat [ text "package" <+> pp i <+> text "is"
         , indent $ blockCat semi d
         , text "end package" <+> pp i <> semi
         ]

instance Pretty PackageDeclarativeItem where
  pp (PHDISubprogDecl s) = pp s
  pp (PHDISubprogBody b) = pp b
  pp (PHDIType t)        = pp t
  pp (PHDISubtype s)     = pp s
  pp (PHDIConstant c)    = pp c
  pp (PHDISignal s)      = pp s
  pp (PHDIShared v)      = pp v
  pp (PHDIFile f)        = pp f
  pp (PHDIAlias a)       = pp a
  pp (PHDIComp c)        = pp c
  pp (PHDIAttrDecl a)    = pp a
  pp (PHDIAttrSpec a)    = pp a
  pp (PHDIDiscSpec d)    = pp d
  pp (PHDIUseClause u)   = pp u
  pp (PHDIGroupTemp g)   = pp g
  pp (PHDIGroup g)       = pp g

--instance Pretty PackageDeclarativePart where pp = undefined

instance Pretty ParameterSpecification where
  pp (ParameterSpecification i r) = pp i <+> text "in" <+> pp r

instance Pretty PhysicalLiteral where
  pp (PhysicalLiteral a n) = cond id a <+> pp n

instance Pretty PhysicalTypeDefinition where
  pp (PhysicalTypeDefinition c p s n) =
    pp c `hangs` vcat
      [ text "units"
      , indent $ vcat
        [ pp p
        , vcat $ map pp s
        ]
      , text "end units" <+> cond id n
      ]

instance Pretty PortClause where
  pp (PortClause ls) = text "port" <+> parens (pp ls) <> semi

--instance Pretty PortList where pp = undefined

instance Pretty PortMapAspect where
  pp (PortMapAspect as) = text "port map" <+> parens (pp as) <> semi

instance Pretty Prefix where
  pp (PName n) = pp n
  pp (PFun f)  = pp f

-- instance Pretty Primary where
--   pp (PrimName n)  = pp n
--   pp (PrimLit l)   = pp l
--   pp (PrimAgg a)   = pp a
--   pp (PrimFun f)   = pp f
--   pp (PrimQual q)  = pp q
--   pp (PrimTCon t)  = pp t
--   pp (PrimAlloc a) = pp a
--   pp (PrimExp e)   = parens (pp e)

instance Pretty PrimaryUnit where
  pp (PrimaryEntity e)  = pp e
  pp (PrimaryConfig c)  = pp c
  pp (PrimaryPackage p) = pp p

instance Pretty ProcedureCall where
  pp (ProcedureCall n ap) = pp n <+> cond parens ap

instance Pretty ProcedureCallStatement where
  pp (ProcedureCallStatement l p) = label l <+> pp p <> semi

instance Pretty ProcessDeclarativeItem where
  pp (PDISubprogDecl s) = pp s
  pp (PDISubprogBody b) = pp b
  pp (PDIType t)        = pp t
  pp (PDISubtype s)     = pp s
  pp (PDIConstant c)    = pp c
  pp (PDIVariable v)    = pp v
  pp (PDIFile f)        = pp f
  pp (PDIAlias a)       = pp a
  pp (PDIAttrDecl a)    = pp a
  pp (PDIAttrSpec a)    = pp a
  pp (PDIUseClause u)   = pp u

--instance Pretty ProcessDeclarativePart where pp = undefined

instance Pretty ProcessStatement where
  pp (ProcessStatement l p ss d s) =
    labels l $ vcat
      [ (post <+> cond parens ss <+> text "is")
        `hangs` vpp d
      , text "begin"
        `hangs` vpp s
      , text "end" <+> post <+> cond id l <> semi
      ]
    where
      post = when p (text "postponed") <+> text "process"

--instance Pretty ProcessStatementPart where pp = undefined

instance Pretty QualifiedExpression where
  pp (QualExp t e) = pp t <> char '\'' <> parens (pp e)
  pp (QualAgg t a) = pp t <> char '\'' <> pp a

instance Pretty Range where
  pp (RAttr a)       = pp a
  pp (RSimple l d u) = pp l <+> pp d <+> pp u

instance Pretty RangeConstraint where
  pp (RangeConstraint r) = text "range" <+> pp r

instance Pretty RecordTypeDefinition where
  pp (RecordTypeDefinition es n) =
    vcat [ text "record"
         , vcat $ map pp es
         , text "end record" <+> cond id n
         ]

-- instance Pretty Relation where
--   pp (Relation e (Nothing))     = pp e
--   pp (Relation e (Just (r, s))) = pp e <+> pp r <+> pp s

-- instance Pretty RelationalOperator where
--   pp (Eq)  = equals
--   pp (Neq) = text "/="
--   pp (Lt)  = char '<'
--   pp (Lte) = text "<="
--   pp (Gt)  = char '>'
--   pp (Gte) = text ">="

instance Pretty ReportStatement where
  pp (ReportStatement l e s) =
    labels l $ (text "report" <+> pp e `hangs` condL (text "severity") s)

instance Pretty ReturnStatement where
  pp (ReturnStatement l e) = label l <+> text "return" <+> condR semi e

instance Pretty ScalarTypeDefinition where
  pp (ScalarEnum e)  = pp e
  pp (ScalarInt i)   = pp i
  pp (ScalarFloat f) = pp f
  pp (ScalarPhys p)  = pp p

instance Pretty SecondaryUnit where
  pp (SecondaryArchitecture a) = pp a
  pp (SecondaryPackage p)      = pp p

instance Pretty SecondaryUnitDeclaration where
  pp (SecondaryUnitDeclaration i p) = pp i <+> equals <+> pp p

instance Pretty SelectedName where
  pp (SelectedName p s) = pp p <> char '.' <> pp s

instance Pretty SelectedSignalAssignment where
  pp (SelectedSignalAssignment e t o w) =
    text "with" <+> pp e <+> text "select"
      `hangs`
    pp t <+> text "<=" <+> pp o <+> pp w <> semi

instance Pretty SelectedWaveforms where
  pp (SelectedWaveforms ws) = vcat $ sws
    where
      sws = map f ws
      f (w, c) = pp w <+> text "when" <+> pp c <+> comma

  -- pp (SelectedWaveforms ws (w, c)) = vcat $ optional ++ [last]
  --   where
  --     optional = maybe [] (map f) ws
  --     last     = pp w <+> text "when" <+> pp c
  --    f (w, c) = pp w <+> text "when" <+> pp c <+> comma

instance Pretty SensitivityClause where
  pp (SensitivityClause ss) = text "on" <+> pp ss

instance Pretty SensitivityList where
  pp (SensitivityList ns) = commaSep $ map pp ns

--instance Pretty SequenceOfStatements where pp = undefined

instance Pretty SequentialStatement where
  pp (SWait w)      = pp w
  pp (SAssert a)    = pp a
  pp (SReport r)    = pp r
  pp (SSignalAss s) = pp s
  pp (SVarAss v)    = pp v
  pp (SProc p)      = pp p
  pp (SIf i)        = pp i
  pp (SCase c)      = pp c
  pp (SLoop l)      = pp l
  pp (SNext n)      = pp n
  pp (SExit e)      = pp e
  pp (SReturn r)    = pp r
  pp (SNull n)      = pp n

-- instance Pretty ShiftExpression where
--   pp (ShiftExpression e (Nothing))     = pp e
--   pp (ShiftExpression e (Just (r, s))) = pp e <+> pp r <+> pp s

-- instance Pretty ShiftOperator where
--   pp Sll = text "sll"
--   pp Srl = text "srl"
--   pp Sla = text "sla"
--   pp Sra = text "sra"
--   pp Rol = text "rol"
--   pp Ror = text "ror"

-- instance Pretty Sign where
--   pp Identity = char '+'
--   pp Negation = char '-'

instance Pretty SignalAssignmentStatement where
  pp (SignalAssignmentStatement l t d w) =
        label l <+> pp t <+> text "<="
    <+> cond  id    d <+> pp w <> semi

instance Pretty SignalDeclaration where
  pp (SignalDeclaration is s k e) =
        text "signal"
    <+> commaSep (fmap pp is)
    <> colon <+> pp s {-<+> cond id k-}
    <+> condL (text ":=") e <> semi

instance Pretty SignalKind where
  pp Register = text "register"
  pp Bus      = text "bus"

instance Pretty SignalList where
  pp (SLName ns) = commaSep $ map pp ns
  pp (SLOthers)  = text "others"
  pp (SLAll)     = text "all"

instance Pretty Signature where
  pp (Signature (Nothing))      = empty
  pp (Signature (Just (ts, t))) = initial <+> condL (text "return") t
    where
      initial = commaSep $ maybe [] (map pp) ts

-- instance Pretty SimpleExpression where
--   pp (SimpleExpression s t as) = cond id s <+> pp t <+> adds
--     where
--       adds = hsep $ map (\(a, t') -> pp a <+> pp t') as

--instance Pretty SimpleName where pp = undefined

instance Pretty SliceName where
  pp (SliceName p r) = pp p <+> parens (pp r)

instance Pretty StringLiteral where
  pp (SLit s) = char '\"' <> text s <> char '\"'

instance Pretty SubprogramBody where
  pp (SubprogramBody s d st k de) =
    vcat [ pp s <+> text "is"
         , indent $ block d
         , text "begin"
         , indent $ block st
         , text "end" <+> pp' k <+> pp' de <> semi
         ]

--instance Pretty SubprogramDeclaration where pp = undefined

instance Pretty SubprogramDeclarativeItem where
  pp (SDISubprogDecl d) = pp d
  pp (SDISubprogBody b) = pp b
  pp (SDIType t)        = pp t
  pp (SDISubtype s)     = pp s
  pp (SDIConstant c)    = pp c
  pp (SDIVariable v)    = pp v
  pp (SDIFile f)        = pp f
  pp (SDIAlias a)       = pp a
  pp (SDIAttrDecl a)    = pp a
  pp (SDIAttrSpec a)    = pp a
  pp (SDIUseClause u)   = pp u
  pp (SDIGroupTemp g)   = pp g
  pp (SDIGroup g)       = pp g

--instance Pretty SubprogramDeclarativePart where pp = undefined

instance Pretty SubprogramKind where
  pp Procedure = text "procedure"
  pp Function  = text "function"

instance Pretty SubprogramSpecification where
  pp (SubprogramProcedure d fs)    = text "procedure" <+> pp d <+> cond parens fs
  pp (SubprogramFunction p d fs t) =
      purity <+> vcat
        [ text "function" <+> pp d <+> cond parens fs
        , text "return"   <+> pp t
        ]
    where
      purity = case p of
        Nothing    -> empty
        Just True  -> text "pure"
        Just False -> text "impure"

--instance Pretty SubprogramStatementPart where pp = undefined

instance Pretty SubtypeDeclaration where
  pp (SubtypeDeclaration i s) = text "subtype" <+> pp i <+> text "is" <+> pp s <> semi

instance Pretty SubtypeIndication where
  pp (SubtypeIndication n t c) = pp' n <+> pp t <+> pp' c

instance Pretty Suffix where
  pp (SSimple n) = pp n
  pp (SChar c)   = pp c
  pp (SOp o)     = pp o
  pp (SAll)      = text "all"

instance Pretty Target where
  pp (TargetName n) = pp n
  pp (TargetAgg a)  = pp a

-- instance Pretty Term where
--   pp (Term f ms) = pp f <+> muls
--     where
--       muls = hsep $ map (\(m, t) -> pp m <+> pp t) ms

instance Pretty TimeoutClause where
  pp (TimeoutClause e) = text "for" <+> pp e

instance Pretty TypeConversion where
  pp (TypeConversion t e) = pp t <+> parens (pp e)

instance Pretty TypeDeclaration where
  pp (TDFull ft)    = pp ft
  pp (TDPartial pt) = pp pt

instance Pretty TypeDefinition where
  pp (TDScalar s)    = pp s
  pp (TDComposite c) = pp c
  pp (TDAccess a)    = pp a
  pp (TDFile f)      = pp f

instance Pretty TypeMark where
  pp (TMType n)    = pp n
  pp (TMSubtype n) = pp n

instance Pretty UnconstrainedArrayDefinition where
  pp (UnconstrainedArrayDefinition is s) =
    text "array" <+> parens (commaSep $ map pp is) <+> text "of" <+> pp s

instance Pretty UseClause where
  pp (UseClause ns) = text "use" <+> commaSep (map pp ns) <> semi

instance Pretty VariableAssignmentStatement where
  pp (VariableAssignmentStatement l t e) = label l <+> pp t <+> text ":=" <+> pp e <> semi

instance Pretty VariableDeclaration where
  pp (VariableDeclaration s is sub e) =
    when s (text "shared") <+> text "variable"
    <+> commaSep (fmap pp is)
    <> colon <+> pp sub <+> condL (text ":=") e <> semi

instance Pretty WaitStatement where
  pp (WaitStatement l sc cc tc) =
    label l <+> text "wait" <+> pp' sc <+> pp' cc <+> pp' tc <> semi

instance Pretty Waveform where
  pp (WaveElem es)    = commaSep $ map pp es
  pp (WaveUnaffected) = text "unaffected"

instance Pretty WaveformElement where
  pp (WaveEExp e te) = pp e <+> condL (text "after") te
  pp (WaveENull te) = text "null" <+> condL (text "after") te

--------------------------------------------------------------------------------
-- * Some helpers
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- text sep.

commaSep  :: [Doc] -> Doc
commaSep  = hsep . punctuate comma

semiSep   :: [Doc] -> Doc
semiSep   = hsep . punctuate semi

pipeSep   :: [Doc] -> Doc
pipeSep   = hsep . punctuate (char '|')

textSep   :: String -> [Doc] -> Doc
textSep s = hsep . punctuate (space <> text s)

--------------------------------------------------------------------------------
-- indentation

indent :: Doc -> Doc
indent = nest 4

hangs  :: Doc -> Doc -> Doc
hangs d1 d2 = d1 $+$ indent d2

labels  :: Pretty a => Maybe a -> Doc -> Doc
labels (Nothing) doc = doc
labels (Just a)  doc = (pp a <> colon) `hangs` doc

block :: Pretty a => [a] -> Doc
block = vcat . map pp

blockCat :: Pretty a => Doc -> [a] -> Doc
blockCat d = vcat . map (\s -> pp s <> d)

--------------------------------------------------------------------------------
-- conditional print

cond :: Pretty a => (Doc -> Doc) -> Maybe a -> Doc
cond f = maybe empty (f . pp)

condR :: Pretty a => Doc -> Maybe a -> Doc
condR s = cond (<+> s)

condL :: Pretty a => Doc -> Maybe a -> Doc
condL s = cond (s <+>)

label :: Pretty a => Maybe a -> Doc
label = cond (<> colon)

pp' :: Pretty a => Maybe a -> Doc
pp' = cond id

parens' :: Pretty a => Maybe a -> Doc
parens' = cond parens

when :: Bool -> Doc -> Doc
when b a = if b then a else empty

--------------------------------------------------------------------------------
-- some common things

vpp :: Pretty a => [a] -> Doc
vpp = foldr ($+$) empty . map pp

postponed :: Pretty a => Maybe Label -> Bool -> a -> Doc
postponed l b a = label l <+> when b (text "postponed") <+> pp a

ppr :: (Pretty a) => a -> String
ppr = render . pp

--------------------------------------------------------------------------------
