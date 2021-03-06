{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Language.VHDL.Parser.Internal
where

import           Prelude                    hiding (exponent)
import           Text.Megaparsec            hiding (label)
import           Text.Megaparsec.Expr

import           Control.Monad              (void, when)
import           Data.Data                  (Data)
import           Data.Maybe                 (fromJust, isJust)
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Language.VHDL.Lexer
import           Language.VHDL.Parser.Monad
import           Language.VHDL.Pretty
import           Language.VHDL.Syntax

---------------------------------------------------------------------------------
-- Util functions
---------------------------------------------------------------------------------
trace :: String -> a -> a
trace _ = id

antiQ2
  :: (Data a)
  => (String -> a) -> (String -> a) -> Parser a -> Parser a
antiQ2 s p q = antiQ s q <|> antiQ p q

isReserved :: Text -> Parser Bool
isReserved a = isJust <$> optional (reserved a)

isReservedOp :: Text -> Parser Bool
isReservedOp a = isJust <$> optional (symbol a)

-- Match block statements with optional label
optionEndNameLabel :: Maybe Label -> Parser ()
optionEndNameLabel l =
  case l of
    Just (Ident s)         -> optionEndName s
    Just (ExtendedIdent s) -> optionEndName s
    Just (AntiIdent _)     -> fail "This shouldn't be an anti quotation"
    Nothing                -> return ()

-- TODO: Is having e.g. package foo ... end foo instead of end package foo valid?
optionEndName :: Text -> Parser ()
optionEndName s = do
  expected <- popBlockName
  -- FIXME: Error message points to end of list
  actual <- optional simpleName
  case actual of
    Just n ->
      when (identToLower n /= expected) $
      fail
        (T.unpack s ++
         " block " ++ pprr expected ++ " cannot be ended by " ++ pprr n)
    Nothing -> return ()

block :: Text -> Parser a -> Parser a
block s p =
  reserved s >>
  p <* (reserved "end" *> optional (reserved s)) <* optionEndName s <* semi

labeledBlock
  :: Text
  -> Maybe Label
  -> (Maybe Label -> Parser a)
  -> Parser a
labeledBlock s l p = do
  labelPush l
  void $ reserved s
  p l <* (reserved "end" *> optional (reserved s)) <* optionEndNameLabel l <*
    semi

-- FIXME: What is going on here?
blockN :: [Text] -> Parser a -> Parser a
blockN s p =
  mapM_ reserved s >>
  p <* (reserved "end" *> optional (mapM_ reserved s)) <*
  optionEndName (mconcat s) <*
  semi

labelPush :: Maybe Label -> Parser ()
labelPush l =
  case l of
    Just lab -> void $ pushBlockName lab
    Nothing  -> return ()

stmLabelPush :: Maybe Label -> (Maybe Label -> Parser a) -> Parser a
stmLabelPush l p = do
  when (isJust l) (void $ pushBlockName (fromJust l))
  p l

stmLabel :: (Maybe Label -> Parser a) -> Parser a
stmLabel = stmLabel' (\_ -> return ())

stmLabel' :: (Maybe Label -> Parser ()) -> (Maybe Label -> Parser a) -> Parser a
stmLabel' f g = do
  lab <- optional (try (label <* colon))
  f lab
  g (trace ("Label: " ++ show lab) lab)

blockName :: Parser Identifier
blockName = simpleName >>= pushBlockName

labelRequired :: Maybe Label -> Parser Label
labelRequired l =
  case l of
    Just l' -> return l'
    Nothing -> fail "A label is required here"

--------------------------------------------------------------------------------
--
--                                   -- 1 --
--
--                      Design entities and configurations
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- * 1.1 Entiity Declarations
--------------------------------------------------------------------------------
{-
    entity_declaration ::=
      ENTITY identifier IS
        entity_header
        entity_declarative_part
      [ BEGIN
        entity_statement_part ]
      END [ ENTITY ] [ entity_simple_name ] ;
-}
entityDeclaration :: Parser EntityDeclaration
entityDeclaration =
  block "entity" $
  EntityDeclaration <$> (blockName <* reserved "is") <*> entityHeader <*>
  entityDeclarativePart <*>
  optional (reserved "begin" *> entityStatementPart)

--------------------------------------------------------------------------------
-- ** 1.1.1 Entity haeder
{-
    entity_header ::=
      [ formal_generic_clause ]
      [ formal_port_clause ]

    generic_clause ::= GENERIC ( generic_list ) ;
    port_clause    ::= PORT ( port_list ) ;
-}
-- TODO: Change AST here so that EntityHeader encodes if its a generic or a port
entityHeader :: Parser EntityHeader
entityHeader =
  EntityHeader <$> optional genericClause <*> optional portClause

genericClause :: Parser GenericClause
genericClause =
  reserved "generic" >> GenericClause <$> parens genericList <* semi

portClause :: Parser PortClause
portClause = reserved "port" >> PortClause <$> parens portList <* semi

--------------------------------------------------------------------------------
-- *** 1.1.1.1 Generics
{-
    generic_list ::= generic_interface_list
-}
genericList :: Parser InterfaceList
genericList = interfaceList' interfaceConstantDeclaration

--------------------------------------------------------------------------------
-- *** 1.1.1.2 Ports
{-
    port_list ::= port_interface_list
-}
portList :: Parser InterfaceList
portList = interfaceList' interfaceSignalDeclaration

--------------------------------------------------------------------------------
-- ** 1.1.2 Entity declarative part
{-
    entity_declarative_part ::=
      { entity_declarative_item }

entity_declarative_item ::=
        subprogram_declaration
      | subprogram_body
      | type_declaration
      | subtype_declaration
      | constant_declaration
      | signal_declaration
      | shared_variable_declaration
      | file_declaration
      | alias_declaration
      | attribute_declaration
      | attribute_specification
      | disconnection_specification
      | use_clause
      | group_template_declaration
      | group_declaration
-}
entityDeclarativePart :: Parser EntityDeclarativePart
entityDeclarativePart =
  many $
  choice
    [ subprogramDeclOrBody EDISubprogDecl EDISubprogBody
    , EDIType <$> typeDeclaration
    , EDISubtype <$> subtypeDeclaration
    , EDIConstant <$> constantDeclaration
    , EDISignal <$> signalDeclaration
    , EDIShared <$> variableDeclaration
    , EDIAttrDecl <$> attributeDeclaration
    , EDIAttrSpec <$> attributeSpecification
    , EDIAlias <$> aliasDeclaration
    , EDIFile <$> fileDeclaration
    , EDIDiscSpec <$> disconnectionSpecification
    , EDIUseClause <$> useClause
    , groupTemplOrDecl EDIGroupTemp EDIGroup
    ]

--------------------------------------------------------------------------------
-- ** 1.1.3 Entity statement part
{-
    entity_statement_part ::=
      { entity_statement }

    entity_statement ::=
        concurrent_assertion_statement
      | passive_concurrent_procedure_call_statement
      | passive_process_statement
-}
entityStatementPart :: Parser EntityStatementPart
entityStatementPart = many entityStatement

entityStatement :: Parser EntityStatement
entityStatement =
  stmLabel
    (\l ->
       choice
         [ ESPassiveProc <$> processStatement l
         , ESConcAssert <$> try (concurrentAssertionStatement l)
         , ESPassiveConc <$> concurrentProcedureCallStatement l
         ])

--------------------------------------------------------------------------------
-- * 1.2 Arcitecture bodies
--------------------------------------------------------------------------------
{-
    architecture_body ::=
      ARCHITECTURE identifier OF entity_name IS
        architecture_declarative_part
      BEGIN
        architecture_statement_part
      END [ ARCHITECTURE ] [ architecture_simple_name ] ;
-}
architectureBody :: Parser ArchitectureBody
architectureBody =
  block
    "architecture"
    (ArchitectureBody <$> blockName <*> (reserved "of" *> name <* reserved "is") <*>
     architectureDeclarativePart <*>
     (reserved "begin" *> architectureStatementPart))

--------------------------------------------------------------------------------
-- ** 1.2.1 Architecture declarative part
{-
    architecture_declarative_part ::=
      { block_declarative_item }

    block_declarative_item ::=
        subprogram_declaration
      | subprogram_body
      | type_declaration
      | subtype_declaration
      | constant_declaration
      | signal_declaration
      | shared_variable_declaration
      | file_declaration
      | alias_declaration
      | component_declaration
      | attribute_declaration
      | attribute_specification
      | configuration_specification
      | disconnection_specification
      | use_clause
      | group_template_declaration
      | group_declaration
-}
architectureDeclarativePart :: Parser ArchitectureDeclarativePart
architectureDeclarativePart = many blockDeclarativeItem

blockDeclarativeItem :: Parser BlockDeclarativeItem
blockDeclarativeItem =
  antiQ2 AntiBlockDecl AntiBlockDecls $
  choice
    [ subprogramDeclOrBody BDISubprogDecl BDISubprogBody
    , BDIType <$> typeDeclaration
    , BDISubtype <$> subtypeDeclaration
    , BDIConstant <$> constantDeclaration
    , BDISignal <$> signalDeclaration
    , BDIShared <$> variableDeclaration
    , BDIFile <$> fileDeclaration
    , BDIConfigSepc <$> configurationSpecification
    , BDIAlias <$> aliasDeclaration
    , BDIComp <$> componentDeclaration
    , BDIAttrDecl <$> attributeDeclaration
    , BDIAttrSepc <$> attributeSpecification
    , BDIDisconSpec <$> disconnectionSpecification
    , BDIUseClause <$> useClause
    , groupTemplOrDecl BDIGroupTemp BDIGroup
    ]
  --------------------------------------------------------------------------------

-- ** 1.2.2 Architecture statement part
{-
    architecture_statement_part ::=
      { concurrent_statement }
-}
architectureStatementPart :: Parser ArchitectureStatementPart
architectureStatementPart = concurrentStatements

--------------------------------------------------------------------------------
-- * 1.3 Configuration declarations
--------------------------------------------------------------------------------
{-
    configuration_declaration ::=
      CONFIGURATION identifier OF entity_name IS
        configuration_declarative_part
        block_configuration
      END [ CONFIGURATION ] [ configuration_simple_name ] ;

    configuration_declarative_part ::=
      { configuration_declarative_item }

    configuration_declarative_item ::=
        use_clause
      | attribute_specification
      | group_declaration
-}
configurationDeclaration :: Parser ConfigurationDeclaration
configurationDeclaration =
  block "configuration" $
  ConfigurationDeclaration <$> (blockName <* reserved "of") <*>
  (name <* reserved "is") <*>
  configurationDeclarativePart <*>
  blockConfiguration

configurationDeclarativePart :: Parser ConfigurationDeclarativePart
configurationDeclarativePart = many configurationDeclarativeItem

configurationDeclarativeItem :: Parser ConfigurationDeclarativeItem
configurationDeclarativeItem =
  choice
    [ CDIUse <$> useClause
    , CDIAttrSpec <$> attributeSpecification
    , CDIGroup <$> (reserved "group" >> groupDeclaration)
    ]

--------------------------------------------------------------------------------
-- ** 1.3.1 Block configuration
{-
    block_configuration ::=
      FOR block_specification
        { use_clause }
        { configuration_item }
      END FOR ;

    block_specification ::=
        architecture_name
      | block_statement_label
      | generate_statement_label [ ( index_specification ) ]

    index_specification ::=
        discrete_range
      | static_expression

    configuration_item ::=
        block_configuration
      | component_configuration
-}
blockConfiguration :: Parser BlockConfiguration
blockConfiguration =
  reserved "for" >>
  BlockConfiguration <$> blockSpecification <*> many useClause <*>
  many configurationItem <*
  reserved "end" <*
  reserved "for" <*
  semi

blockSpecification :: Parser BlockSpecification
-- FIXME: This distinction is probably useless
blockSpecification =
  choice
    [ try $ BSGen <$> label <*> optional (parens indexSpecification)
    , BSArch <$> name
    , BSBlock <$> label
    ]

indexSpecification :: Parser IndexSpecification
indexSpecification =
  choice [ISRange <$> try discreteRange, ISExp <$> expression]

configurationItem :: Parser ConfigurationItem
configurationItem = -- FIXME: Get rid of this try
  choice [try $ CIBlock <$> blockConfiguration, CIComp <$> componentConfiguration]

--------------------------------------------------------------------------------
-- ** 1.3.2 Component configuration
{-
    component_configuration ::=
      FOR component_specification
        [ binding_indication ; ]
          [ block_configuration ]
      END FOR ;
-}
-- XXX: How to distinguish this form the previous?
componentConfiguration :: Parser ComponentConfiguration
componentConfiguration =
  reserved "for" >>
  ComponentConfiguration <$> componentSpecification <*>
  optional (bindingIndication <* semi) <*>
  optional blockConfiguration <*
  (reserved "end" >> reserved "for" >> semi)

--------------------------------------------------------------------------------
--
--                                   -- 2 --
--
--                           Subprograms and packages
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- * 2.1 Subprogram declarations
--------------------------------------------------------------------------------
{-
    subprogram_declaration ::=
      subprogram_specification ;

    subprogram_specification ::=
      PROCEDURE designator [ ( formal_parameter_list ) ]
      | [ PURE | IMPURE ] FUNCTION designator [ ( formal_parameter_list ) ]
        RETURN type_mark

    designator ::= identifier | operator_symbol

    operator_symbol ::= string_literal
-}
subprogramDeclaration :: SubprogramSpecification -> Parser SubprogramDeclaration
subprogramDeclaration = trace "subprogramDeclaration" (pure . SubprogramDeclaration)

subprogramDeclaration' :: Parser SubprogramDeclaration
subprogramDeclaration' =
  trace "subprogramDeclaration'" $
  SubprogramDeclaration <$> subprogramSpecification <* semi

subprogramSpecification :: Parser SubprogramSpecification
subprogramSpecification = subprogramProcedure <|> subprogramFunction

subprogramProcedure :: Parser SubprogramSpecification
subprogramProcedure =
  reserved "procedure" >>
  SubprogramProcedure <$> designator <*>
  optional (parens formalParameterList)

data Purity
  = Pure
  | Impure
  deriving (Eq)

purity :: Parser Purity
purity = pure' <|> impure
  where
    pure' = reserved "pure" *> pure Pure
    impure = reserved "impure" *> pure Impure

ispure :: Parser Bool
ispure = flip (==) Pure <$> purity

subprogramFunction :: Parser SubprogramSpecification
subprogramFunction =
  SubprogramFunction <$> optional ispure <*>
  (reserved "function" >> designator) <*>
  optional (parens formalParameterList) <*>
  (reserved "return" >> typeMark)

designator :: Parser Designator
designator = choice [DId <$> identifier, DOp <$> operatorSymbol]

operatorSymbol :: Parser StringLiteral
operatorSymbol = stringLiteral

subprogramDeclOrBody :: (SubprogramDeclaration -> a)
                     -> (SubprogramBody -> a)
                     -> Parser a
subprogramDeclOrBody t1 t2 =
  subprogramSpecification >>=
  (\s ->
     t1 <$> (semi *> subprogramDeclaration s) <|>
     t2 <$> (reserved "is" *> subprogramBody s))

--------------------------------------------------------------------------------
-- ** 2.1.1 Formal parameters
{-
    formal_parameter_list ::= parameter_interface_list
-}
formalParameterList :: Parser InterfaceList
formalParameterList = interfaceList

--------------------------------------------------------------------------------
-- * 2.2 Subprogram bodies
--------------------------------------------------------------------------------
{-
    subprogram_body ::=
      subprogram_specification IS
        subprogram_declarative_part
      BEGIN
        subprogram_statement_part
      END [ subprogram_kind ] [ designator ] ;

    subprogram_declarative_part ::=
      { subprogram_declarative_item }

    subprogram_declarative_item ::=
        subprogram_declaration
      | subprogram_body
      | type_declaration
      | subtype_declaration
      | constant_declaration
      | variable_declaration
      | file_declaration
      | alias_declaration
      | attribute_declaration
      | attribute_specification
      | use_clause
      | group_template_declaration
      | group_declaration


    subprogram_statement_part ::=
      { sequential_statement }

    subprogram_kind ::= PROCEDURE | FUNCTION
-}
subprogramBody :: SubprogramSpecification -> Parser SubprogramBody
subprogramBody s =
  trace "subProgramBody" $
  SubprogramBody s <$> (subprogramDeclarativePart <* reserved "begin") <*>
  subprogramStatementPart <*>
  (reserved "end" *> optional subprogramKind) <*>
  (optional designator <* semi)
  -- FIXME: Seems odd to put significance to something after the
  -- end statement

subprogramDeclarativePart :: Parser [SubprogramDeclarativeItem]
subprogramDeclarativePart = many subprogramDeclarativeItem

subprogramDeclarativeItem :: Parser SubprogramDeclarativeItem
subprogramDeclarativeItem =
  choice
    [ subprogramDeclOrBody SDISubprogDecl SDISubprogBody
    , SDIType <$> typeDeclaration
    , SDISubtype <$> subtypeDeclaration
    , SDIConstant <$> constantDeclaration
    , SDIVariable <$> variableDeclaration
    , SDIAlias <$> aliasDeclaration
    , SDIAttrDecl <$> attributeDeclaration
    , SDIAttrSpec <$> attributeSpecification
    , SDIFile <$> fileDeclaration
    , SDIUseClause <$> useClause
    , groupTemplOrDecl SDIGroupTemp SDIGroup
    ]

subprogramStatementPart :: Parser [SequentialStatement]
subprogramStatementPart = many sequentialStatement

subprogramKind :: Parser SubprogramKind
subprogramKind =
  choice
    [ reserved "procedure" *> pure Procedure
    , reserved "function" *> pure Function
    ]

--------------------------------------------------------------------------------
-- * 2.3 Subprogram overloading
-- properties ... todo
--------------------------------------------------------------------------------
-- ** 2.3.1 Operator overloading
-- properties ... todo
--------------------------------------------------------------------------------
-- ** 2.3.2 Signatures
{-
    signature ::= [ [ type_mark { , type_mark } ] [ RETURN type_mark ] ]
-}
signature :: Parser Signature
signature =
  Signature <$>
  brackets
    ((,) <$> optional (commaSep typeMark) <*>
     optional (reserved "return" *> typeMark))

--------------------------------------------------------------------------------
-- * 2.5 Package declarations
{-
    package_declaration ::=
      PACKAGE identifier IS
        package_declarative_part
      END [ PACKAGE ] [ package_simple_name ] ;

    package_declarative_part ::=
      { package_declarative_item }

    package_declarative_item ::=
        subprogram_declaration
      | type_declaration
      | subtype_declaration
      | constant_declaration
      | signal_declaration
      | shared_variable_declaration
      | file_declaration
      | alias_declaration
      | component_declaration
      | attribute_declaration
      | attribute_specification
      | disconnection_specification
      | use_clause
      | group_template_declaration
      | group_declaration
-}
-- TODO: Match package identifier/end name
packageDeclaration :: Parser PackageDeclaration
packageDeclaration =
  trace "packageDeclaration" $
  block "package" $
  PackageDeclaration <$> (blockName <* reserved "is") <*> packageDeclarativePart

packageDeclarativePart :: Parser [PackageDeclarativeItem]
packageDeclarativePart = many packageDeclarativeItem

packageDeclarativeItem :: Parser PackageDeclarativeItem
packageDeclarativeItem =
  antiQ2 AntiPackDeclIt AntiPackDeclIts $
  choice
    [ subprogramDeclOrBody PHDISubprogDecl PHDISubprogBody
    , PHDIType <$> typeDeclaration
    , PHDISubtype <$> subtypeDeclaration
    , PHDIConstant <$> constantDeclaration
    , PHDISignal <$> signalDeclaration
    , PHDIShared <$> variableDeclaration
    , PHDIFile <$> fileDeclaration
    , PHDIAttrDecl <$> attributeDeclaration
    , PHDIAttrSpec <$> attributeSpecification
    , PHDIAlias <$> aliasDeclaration
    , PHDIComp <$> componentDeclaration
    , PHDIDiscSpec <$> disconnectionSpecification
    , PHDIUseClause <$> useClause
    , groupTemplOrDecl PHDIGroupTemp PHDIGroup
    ]

-- * 2.6 Package bodies
{-
    package_body ::=
      PACKAGE  package_simple_name IS
        package_body_declarative_part
      END [ PACKAGE BODY ] [ package_simple_name ] ;

    package_body_declarative_part ::=
      { package_body_declarative_item }

    package_body_declarative_item ::=
        subprogram_declaration
      | subprogram_body
      | type_declaration
      | subtype_declaration
      | constant_declaration
      | shared_variable_declaration
      | file_declaration
      | alias_declaration
      | use_clause
      | group_template_declaration
      | group_declaration
-}
packageBody :: Parser PackageBody
packageBody = trace "PackageBody" $
  blockN ["package", "body"] $
  PackageBody <$> (blockName <* reserved "is") <*> packageBodyDeclarativePart

packageBodyDeclarativePart :: Parser PackageBodyDeclarativePart
packageBodyDeclarativePart = many packageBodyDeclarativeItem

packageBodyDeclarativeItem :: Parser PackageBodyDeclarativeItem
packageBodyDeclarativeItem =
  choice
    [ subprogramDeclOrBody PBDISubprogDecl PBDISubprogBody
    , PBDIType <$> typeDeclaration
    , PBDISubtype <$> subtypeDeclaration
    , PBDIConstant <$> constantDeclaration
    , PBDIShared <$> variableDeclaration
    , PBDIFile <$> fileDeclaration
    , PBDIAlias <$> aliasDeclaration
    , PBDIUseClause <$> useClause
    , groupTemplOrDecl PBDIGroupTemp PBDIGroup
    ]

--------------------------------------------------------------------------------
--
--                                   -- 3 --
--
--                                    Types
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- * 3.1 Scalar types
{-
    scalar_type_definition ::=
        enumeration_type_definition
      | integer_type_definition
      | floating_type_definition
      | physical_type_definition

    range_constraint ::= RANGE range

    range ::=
        range_attribute_name
      | simple_expression direction simple_expression

    direction ::= TO | DOWNTO
-}
scalarTypeDefinition :: Parser ScalarTypeDefinition
scalarTypeDefinition =
  choice [ScalarEnum <$> enumerationTypeDefinition, rangeStart]
  where
    rangeStart = do
      r <- rangeConstraint
      -- TODO: Handle ScalarFloat. Check types of numbers in range?
      choice [ScalarPhys <$> physicalTypeDefinition r, ScalarInt <$> pure r]

rangeConstraint :: Parser RangeConstraint
rangeConstraint = reserved "range" >> RangeConstraint <$> range'

-- FIMXE: Replacing simpleExpression by expression
range :: Parser Range
range =
  choice
    [ RSimple <$> expression <*> direction <*> expression
    , RAttr <$> (name >>= attributeName) -- FIXME: This won't work
    ]

-- For parsing definite ranges (i.e. preceded by range kw)
range' :: Parser Range
range' =
  choice
    [ try
    -- We have no way of parsing specifically parsing an attribute name so here
    -- we go...
        ((expression <* notFollowedBy direction) >>= \case
           PrimName (NAttr n@AttributeName {}) -> RAttr <$> pure n
           _ -> fail "")
    , RSimple <$> expression <*> direction <*> expression
    ]

direction :: Parser Direction
direction = choice [reserved "to" *> pure To, reserved "downto" *> pure DownTo]

--------------------------------------------------------------------------------
-- ** 3.1.1 Enumeration types
{-
    enumeration_type_definition ::=
      ( enumeration_literal { , enumeration_literal } )

    enumeration_literal ::= identifier | character_literal
-}
enumerationTypeDefinition :: Parser EnumerationTypeDefinition
enumerationTypeDefinition =
  EnumerationTypeDefinition <$> parens (commaSep1 enumerationLiteral)

enumerationLiteral :: Parser EnumerationLiteral
enumerationLiteral =
  choice
    [ EChar <$> charLiteral
    , EId <$> identifier
    ]

--------------------------------------------------------------------------------
-- *** 3.1.1.1 Predefined enumeration types
-- predefined ... todo
--------------------------------------------------------------------------------
-- ** 3.1.2 Integer types
{-
    integer_type_definition ::= range_constraint
-}
-- No parser explicitly defined

--------------------------------------------------------------------------------
-- *** 3.1.2.1 Predefined integer types
-- predefined ... todo
--------------------------------------------------------------------------------
-- ** 3.1.3 Physical types
{-
    physical_type_definition ::=
      range_constraint
        UNITS
            primary_unit_declaration
           { secondary_unit_declaration }
        END UNITS [ physical_type_simple_name ]

    primary_unit_declaration ::= identifier ;

    secondary_unit_declaration ::= identifier = physical_literal ;

    physical_literal ::= [ abstract_literal ] unit_name
-}
physicalTypeDefinition :: RangeConstraint -> Parser PhysicalTypeDefinition
physicalTypeDefinition r =
  try (reserved "units") >>
  PhysicalTypeDefinition r <$> primaryUnitDeclaration <*>
  many secondaryUnitDeclaration <*>
  (reserved "end" *> reserved "units" *> optional identifier)

primaryUnitDeclaration :: Parser Identifier
primaryUnitDeclaration = identifier <* semi

secondaryUnitDeclaration :: Parser SecondaryUnitDeclaration
secondaryUnitDeclaration =
  SecondaryUnitDeclaration <$> (identifier <* symbol "=") <*>
  physicalLiteral <*
  semi

physicalLiteral :: Parser PhysicalLiteral
-- TODO: Unit names seem be just identifiers rather than names
physicalLiteral = PhysicalLiteral <$> optional abstractLiteral <*> name

--------------------------------------------------------------------------------
-- *** 3.1.3.1 Predefined physical types
-- predefined ... todo
--------------------------------------------------------------------------------
-- ** 3.1.4 Floating point types
{-
    floating_type_definition ::= range_constraint
-}

-- TODO: Undefined

--------------------------------------------------------------------------------
-- *** 3.1.4.1 Predefined floating point types
-- predefined ... todo
--------------------------------------------------------------------------------
-- * 3.2 Composite types
{-
    composite_type_definition ::=
        array_type_definition
      | record_type_definition
-}
compositeTypeDefinition :: Parser CompositeTypeDefinition
compositeTypeDefinition =
  CTDArray <$> arrayTypeDefinition <|> CTDRecord <$> recordTypeDefinition

--------------------------------------------------------------------------------
-- ** 3.2.1 Array types
{-
    array_type_definition ::=
        unconstrained_array_definition
      | constrained_array_definition

    unconstrained_array_definition ::=
      ARRAY ( index_subtype_definition { , index_subtype_definition } )
        OF element_subtype_indication

    constrained_array_definition ::=
      ARRAY index_constraint OF element_subtype_indication

    index_subtype_definition ::= type_mark RANGE <>

    index_constraint ::= ( discrete_range { , discrete_range } )

    discrete_range ::= discrete_subtype_indication | range
-}
arrayTypeDefinition :: Parser ArrayTypeDefinition
arrayTypeDefinition =
  trace "arrayTypedefinition" $
  reserved "array" >>
  choice
    [ ArrU <$> try unconstrainedArrayDefinition
    , ArrC <$> constrainedArrayDefinition
    ]

unconstrainedArrayDefinition :: Parser UnconstrainedArrayDefinition
unconstrainedArrayDefinition =
  UnconstrainedArrayDefinition <$> parens (commaSep1 indexSubtypeDefinition) <*>
  (reserved "of" *> elementSubtypeDefinition)

constrainedArrayDefinition :: Parser ConstrainedArrayDefinition
constrainedArrayDefinition =
  ConstrainedArrayDefinition <$> indexConstraint <*>
  (reserved "of" *> subtypeIndication)

indexSubtypeDefinition :: Parser IndexSubtypeDefinition
indexSubtypeDefinition =
  IndexSubtypeDefinition <$> typeMark <* reserved "range" <* symbol "<>"

indexConstraint :: Parser IndexConstraint
indexConstraint = IndexConstraint <$> parens (commaSep1 discreteRange)

discreteRange :: Parser DiscreteRange
discreteRange = choice [DRRange <$> try range, DRSub <$> subtypeIndication]

--------------------------------------------------------------------------------
-- *** 3.2.1.1 Index constraints and discrete ranges
-- constraints ... todo
--------------------------------------------------------------------------------
-- *** 3.2.1.2 Predefined array types
-- predefined ... todo
--------------------------------------------------------------------------------
-- ** 3.2.2 Record types
{-
    record_type_definition ::=
      RECORD
        element_declaration
      { element_declaration }
      END RECORD [ record_type_simple_name ]

    element_declaration ::=
      identifier_list : element_subtype_definition ;

    identifier_list ::= identifier { , identifier }

    element_subtype_definition ::= subtype_indication
-}
recordTypeDefinition :: Parser RecordTypeDefinition
recordTypeDefinition =
  reserved "record" >>
  RecordTypeDefinition <$> some elementDeclaration <*>
  (reserved "end" *> reserved "record" *> optional simpleName)

elementDeclaration :: Parser ElementDeclaration
elementDeclaration =
  ElementDeclaration <$> identifierList <*>
  (colon *> elementSubtypeDefinition <* semi)

identifierList :: Parser IdentifierList
identifierList = commaSep1 identifier

elementSubtypeDefinition :: Parser SubtypeIndication
elementSubtypeDefinition = subtypeIndication

--------------------------------------------------------------------------------
-- * 3.3 Access types
{-
    access_type_definition ::= ACCESS subtype_indication
-}
accessTypeDefinition :: Parser AccessTypeDefinition
accessTypeDefinition =
  reserved "access" >> AccessTypeDefinition <$> subtypeIndication

--------------------------------------------------------------------------------
-- ** 3.3.1 Incomplete type declarations
{-
    incomplete_type_declaration ::= TYPE identifier ;
-}
-- Type keyword parsed in typeDeclaration
incompleteTypeDeclaration :: Parser IncompleteTypeDeclaration
incompleteTypeDeclaration =
  try $ IncompleteTypeDeclaration <$> identifier <* semi

--------------------------------------------------------------------------------
-- *** 3.3.2 Allocation and deallocation of objects
-- ?
--------------------------------------------------------------------------------
-- * 3.4 File types
{-
    file_type_definition ::= FILE OF type_mark
-}
fileTypeDefinition :: Parser FileTypeDefinition
fileTypeDefinition =
  reserved "file" >> reserved "of" >> FileTypeDefinition <$> typeMark

--------------------------------------------------------------------------------
-- ** 3.4.1 File operations
-- ?
--------------------------------------------------------------------------------
-- * 3.5 Protected types
-- I'll skip these for now..
--------------------------------------------------------------------------------
--
--                                   -- 4 --
--
--                                Declarations
--
--------------------------------------------------------------------------------
{-
    declaration ::=
        type_declaration
      | subtype_declaration
      | object_declaration
      | interface_declaration
      | alias_declaration
      | attribute_declaration
      | component_declaration
      | group_template_declaration
      | group_declaration
      | entity_declaration
      | configuration_declaration
      | subprogram_declaration
      | package_declaration
-}
declaration :: Parser Declaration
declaration =
  choice
    [ DType <$> typeDeclaration
    , DSubtype <$> subtypeDeclaration
    , DObject <$> objectDeclaration
    , DAttribute <$> attributeDeclaration
    , DAlias <$> aliasDeclaration
    , groupTemplOrDecl DGroupTemplate DGroup
    , DGroup <$> groupDeclaration
    , DEntity <$> entityDeclaration
    , DConfiguration <$> configurationDeclaration
    , DSubprogram <$> subprogramDeclaration'
    , DPackage <$> packageDeclaration
    ]

--------------------------------------------------------------------------------
-- * 4.1 Type declarations
{-
    type_declaration ::=
        full_type_declaration
      | incomplete_type_declaration

    full_type_declaration ::=
      TYPE identifier IS type_definition ;

    type_definition ::=
        scalar_type_definition
      | composite_type_definition
      | access_type_definition
      | file_type_definition
      | protected_type_definition  -- missing from ref. manual
-}
typeDeclaration :: Parser TypeDeclaration
typeDeclaration =
  reserved "type" >>
  choice
    [TDPartial <$> incompleteTypeDeclaration, TDFull <$> fullTypeDeclaration]

fullTypeDeclaration :: Parser FullTypeDeclaration
fullTypeDeclaration =
  FullTypeDeclaration <$> identifier <*> (reserved "is" *> typeDefinition) <*
  semi

typeDefinition :: Parser TypeDefinition
typeDefinition =
  choice
    [ TDScalar <$> scalarTypeDefinition
    , TDComposite <$> compositeTypeDefinition
    , TDAccess <$> accessTypeDefinition
    , TDFile <$> fileTypeDefinition
    ]

--------------------------------------------------------------------------------
-- * 4.2 Subtype declarations
{-
    subtype_declaration ::=
      SUBTYPE identifier IS subtype_indication ;

    subtype_indication ::=
      [ resolution_function_name ] type_mark [ constraint ]

    type_mark ::=
        type_name
      | subtype_name

    constraint ::=
        range_constraint
      | index_constraint
-}
subtypeDeclaration :: Parser SubtypeDeclaration
subtypeDeclaration =
  reserved "subtype" >>
  SubtypeDeclaration <$> identifier <*>
  (reserved "is" *> subtypeIndication <* semi)

-- FIXME: Should bit_vector(length - 1 downto 0) be parsed as subtypeIndication
-- a simpleName typeMark followed by a subtypeIndication constraint or a
-- subtypeIndication containing a sliceName>
subtypeIndication :: Parser SubtypeIndication
subtypeIndication = antiQ AntiSubtyInd $ trace "subtypeindication" $ go <*> optional constraint
  where
    go = do
      name1 <- name' constraint
      optional (try typeMark) >>= \case
        Just ty -> return $ SubtypeIndication (Just name1) ty
        Nothing -> return $ SubtypeIndication Nothing (TMType name1)

typeMark :: Parser TypeMark
-- Since a type_mark refers to the names of subtypes and identifiers, we
-- restrict the name to either a simpleName or a selectedName
typeMark = TMType <$> (try (NSelect <$> selectedName) <|> (NSimple <$> simpleName))

constraint :: Parser Constraint
constraint = choice [CRange <$> rangeConstraint, CIndex <$> indexConstraint]

--------------------------------------------------------------------------------
-- * 4.3 Objects
--------------------------------------------------------------------------------
-- ** 4.3.1 Object declarations
{-
    object_declaration ::=
        constant_declaration
      | signal_declaration
      | variable_declaration
      | file_declaration
-}
objectDeclaration :: Parser ObjectDeclaration
objectDeclaration =
  choice
    [ ObjConst <$> constantDeclaration
    , ObjSig <$> signalDeclaration
    , ObjVar <$> variableDeclaration
    , ObjFile <$> fileDeclaration
    ]

--------------------------------------------------------------------------------
-- *** 4.3.1.1 Constant declarations
{-
    constant_declaration ::=
      CONSTANT identifier_list : subtype_indication [ := expression ] ;
-}
constantDeclaration :: Parser ConstantDeclaration
constantDeclaration =
  reserved "constant" >>
  ConstantDeclaration <$> (identifierList <* colon) <*> subtypeIndication <*>
  optional (symbol ":=" *> expression) <*
  semi

--------------------------------------------------------------------------------
-- *** 4.3.1.2 Signal declarations
{-
    signal_declaration ::=
      SIGNAL identifier_list : subtype_indication [ signal_kind ] [ := expression ] ;

    signal_kind ::= REGISTER | BUS
-}
signalDeclaration :: Parser SignalDeclaration
signalDeclaration =
  try $
  reserved "signal" >>
  SignalDeclaration <$> (identifierList <* colon) <*> subtypeIndication <*>
  optional signalKind <*>
  optional (symbol ":=" *> expression) <*
  semi

signalKind :: Parser SignalKind
signalKind =
  choice [reserved "register" *> pure Register, reserved "bus" *> pure Bus]

--------------------------------------------------------------------------------
-- *** 4.3.1.3 Variable declarations
{-
    variable_declaration ::=
      [ SHARED ] VARIABLE identifier_list : subtype_indication [ := expression ] ;
-}
variableDeclaration :: Parser VariableDeclaration
variableDeclaration =
  trace "variableDeclaration" $
  VariableDeclaration <$> try (isReserved "shared" <* reserved "variable") <*>
  (identifierList <* colon) <*>
  subtypeIndication <*>
  optional (symbol ":=" *> expression) <*
  semi
  --------------------------------------------------------------------------------

-- *** 4.3.1.4 File declarations
{-
    file_declaration ::=
      FILE identifier_list : subtype_indication [ file_open_information ] ;

    file_open_information ::=
      [ OPEN file_open_kind_expression ] IS file_logical_name

    file_logical_name ::= string_expression
-}
fileDeclaration :: Parser FileDeclaration
fileDeclaration =
  reserved "file" >>
  FileDeclaration <$> (identifierList <* colon) <*> subtypeIndication <*>
  optional fileOpenInformation <*
  semi

fileOpenInformation :: Parser FileOpenInformation
fileOpenInformation =
  FileOpenInformation <$> optional (reserved "open" *> expression) <*>
  (reserved "is" *> fileLogicalName)

fileLogicalName :: Parser Expression
fileLogicalName = expression

--------------------------------------------------------------------------------
-- ** 4.3.2 Interface declarations
{-
    interface_declaration ::=
        interface_constant_declaration
      | interface_signal_declaration
      | interface_variable_declaration
      | interface_file_declaration

    interface_constant_declaration ::=
      [ CONSTANT ] identifier_list : [ IN ] subtype_indication [ := static_expression ]

    interface_signal_declaration ::=
      [ SIGNAL ] identifier_list : [ mode ] subtype_indication [ BUS ] [ := static_expression ]

    interface_variable_declaration ::=
      [ VARIABLE ] identifier_list : [ mode ] subtype_indication [ := static_expression ]

    interface_file_declaration ::=
       FILE identifier_list : subtype_indication

    mode ::= IN | OUT | INOUT | BUFFER | LINKAGE
-}
-- TODO: This is generally ambigous. Especially InterfaceConstantDeclaration and
-- InterfaceSignalDeclaration
-- See sections 4.2.2.1, 6.5.6.2 and 6.5.6.3 for info about resolving these
-- ambiguities. It seems like object classes (constant, signals, etc...) are
-- only used for function parameter list. Maybe create separate parsers for each
-- context interface_declaration occurs in?
-- TODO: Error messages. Give useful error when e.g. a signal is attempted
-- defined in a generic clause
interfaceConstantDeclaration :: Parser InterfaceDeclaration
interfaceConstantDeclaration =
  try $
  antiQ2 AntiIfaceDecl AntiIfaceDecls $
  optional (reserved "constant") >>
  InterfaceConstantDeclaration <$>
  (identifierList <* colon <* optional (reserved "in")) <*>
  subtypeIndication <*>
  optional (symbol ":=" *> expression)

interfaceSignalDeclaration :: Parser InterfaceDeclaration
interfaceSignalDeclaration =
  try $
  antiQ2 AntiIfaceDecl AntiIfaceDecls $
  optional (reserved "signal") >>
  InterfaceSignalDeclaration <$> (identifierList <* colon) <*>
  optional interfaceMode <*>
  subtypeIndication <*>
  choice [reserved "bus" *> pure True, pure False] <*>
  optional (symbol ":=" *> expression)

interfaceVariableDeclaration :: Parser InterfaceDeclaration
interfaceVariableDeclaration =
  try $
  antiQ2 AntiIfaceDecl AntiIfaceDecls $
  optional (reserved "variable") >>
  InterfaceVariableDeclaration <$> (identifierList <* colon) <*>
  optional interfaceMode <*>
  subtypeIndication <*>
  optional (symbol ":=" *> expression)

interfaceFileDeclaration :: Parser InterfaceDeclaration
interfaceFileDeclaration =
  reserved "file" >>
  InterfaceFileDeclaration <$> (identifierList <* colon) <*> subtypeIndication

interfaceMode :: Parser Mode
interfaceMode =
  choice
    [ reserved "inout" *> pure InOut
    , reserved "in" *> pure In
    , reserved "out" *> pure Out
    , reserved "buffer" *> pure Buffer
    , reserved "linkage" *> pure Linkage
    ]

--------------------------------------------------------------------------------
-- *** 4.3.2.1 Interface lists
{-
    interface_list ::= interface_element { ; interface_element }

    interface_element ::= interface_declaration
-}

interfaceList :: Parser InterfaceList
interfaceList = interfaceList' interfaceElement

interfaceList' :: Parser InterfaceDeclaration -> Parser InterfaceList
interfaceList' p = trace "interfaceList'" $ InterfaceList <$> semiSep1 p

interfaceElement :: Parser InterfaceDeclaration
interfaceElement =
  antiQ2 AntiIfaceDecl AntiIfaceDecls $
  choice
    [ interfaceConstantDeclaration
    , interfaceVariableDeclaration
    , interfaceSignalDeclaration
    , interfaceFileDeclaration
    ]

--------------------------------------------------------------------------------
-- *** 4.3.2.2 Association lists
{-
    association_element ::=
      [ formal_part => ] actual_part

    association_list ::=
      association_element { , association_element }

    formal_designator ::=
        generic_name
      | port_name
      | parameter_name

    formal_part ::=
        formal_designator
      | function_name ( formal_designator )
      | type_mark ( formal_designator )

    actual_designator ::=
        expression
      | signal_name
      | variable_name
      | file_name
      | OPEN

    actual_part ::=
        actual_designator
      | function_name ( actual_designator )
      | type_mark ( actual_designator )
-}
associationElement :: Parser AssociationElement
associationElement =
  trace "associationElement" $
  antiQ2 AntiAssocEl AntiAssocEls $
  AssociationElement <$>
  optional (try (formalPart <* trace "reservedOp" (symbol "=>"))) <*>
  actualPart

associationList :: Parser AssociationList
associationList = AssociationList <$> commaSep1 associationElement

formalDesignator :: Parser FormalDesignator
-- XXX: another in practice impossible distinction
formalDesignator =
  choice [FDGeneric <$> name, FDPort <$> name, FDParameter <$> name]

formalPart :: Parser FormalPart
formalPart =
  choice
    [ try $ FPType <$> typeMark <*> parens formalDesignator
     -- FIXME: This looks ambigous
    , try $ FPFunction <$> name <*> parens formalDesignator
    , FPDesignator <$> formalDesignator
    ]

actualDesignator :: Parser ActualDesignator
-- FIXME: some ambiguities here as well?
actualDesignator =
  trace "actualDesignator" $
  choice
  -- FIXME: Parse as expression and reduce to name if possible
    [ reserved "open" *> pure ADOpen
    --, ADVariable <$> try name
    , ADExpression <$> try expression
    , ADSignal <$> name
    , ADFile <$> name
    ]

actualPart :: Parser ActualPart
actualPart =
  trace "actualPart" $
  choice
  -- FIXME: Fix these
    [-- try $ APType <$> typeMark <*> parens actualDesignator
    -- FIXME: This looks ambigous
    --, try $ APFunction <$> name <*> parens actualDesignator
    APDesignator <$> actualDesignator
    ]

--------------------------------------------------------------------------------
-- ** 4.3.3 Alias declarations
{-
    alias_declaration ::=
      ALIAS alias_designator [ : subtype_indication ] IS name [ signature ] ;

    alias_designator ::= identifier | character_literal | operator_symbol
-}
aliasDeclaration :: Parser AliasDeclaration
aliasDeclaration =
  try (reserved "alias") >>
  AliasDeclaration <$> aliasDesignator <*>
  optional (colon *> subtypeIndication) <*>
  (reserved "is" *> name) <*>
  optional signature <*
  semi

aliasDesignator :: Parser AliasDesignator
aliasDesignator =
  choice
    [ ADOperator <$> operatorSymbol
    , ADCharacter <$> charLiteral
    , ADIdentifier <$> identifier
    ]

--------------------------------------------------------------------------------
-- * 4.4 Attribute declarations
{-
    attribute_declaration ::=
      ATTRIBUTE identifier : type_mark ;
-}
attributeDeclaration :: Parser AttributeDeclaration
attributeDeclaration =
  AttributeDeclaration <$> try (reserved "attribute" *> identifier <* colon) <*>
  typeMark <*
  semi

--------------------------------------------------------------------------------
-- * 4.5 Component declarations
{-
    component_declaration ::=
      COMPONENT identifier [ IS ]
        [ local_generic_clause ]
        [ local_port_clause ]
      END COMPONENT [ component_simple_name ] ;
-}
componentDeclaration :: Parser ComponentDeclaration
componentDeclaration =
  block "component" $
  ComponentDeclaration <$> (blockName <* optional (reserved "is")) <*>
  optional genericClause <*>
  optional portClause

--------------------------------------------------------------------------------
-- * 4.6 Group template declarations
{-
    group_template_declaration ::=
      GROUP identifier IS ( entity_class_entry_list ) ;

    entity_class_entry_list ::=
      entity_class_entry { , entity_class_entry }

    entity_class_entry ::= entity_class [ <> ]
-}
groupTemplateDeclaration :: Parser GroupTemplateDeclaration
groupTemplateDeclaration =
  GroupTemplateDeclaration <$> try (identifier <* reserved "is") <*>
  parens entityClassEntryList <*
  semi <?> "group_template_declcaration"

entityClassEntryList :: Parser EntityClassEntryList
entityClassEntryList = commaSep1 entityClassEntry

entityClassEntry :: Parser EntityClassEntry
entityClassEntry = EntityClassEntry <$> entityClass <*> isReservedOp "<>"

-- * 4.7 Group declarations
{-
    group_declaration ::=
      GROUP identifier : group_template_name ( group_constituent_list ) ;

    group_constituent_list ::= group_constituent { , group_constituent }

    group_constituent ::= name | character_literal
-}
groupDeclaration :: Parser GroupDeclaration
groupDeclaration =
  GroupDeclaration <$> (identifier <* colon) <*> name' conList <*> conList <*
  semi <?> "group_declaration"
  where
    conList = parens groupConstituentList

groupConstituentList :: Parser [GroupConstituent]
groupConstituentList = commaSep1 groupConstituent

groupConstituent :: Parser GroupConstituent
groupConstituent = choice [GCChar <$> charLiteral, GCName <$> name]

groupTemplOrDecl :: (GroupTemplateDeclaration -> a)
                 -> (GroupDeclaration -> a)
                 -> Parser a
groupTemplOrDecl templ decl =
  reserved "group" >>
  choice [templ <$> groupTemplateDeclaration, decl <$> groupDeclaration]

--------------------------------------------------------------------------------
--
--                                   -- 5 --
--
--                               Specifications
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- * 5.1 Attribute specification
{-
    attribute_specification ::=
      ATTRIBUTE attribute_designator OF entity_specification IS expression ;

    entity_specification ::=
      entity_name_list : entity_class

    entity_class ::=
        ENTITY     | ARCHITECTURE  | CONFIGURATION
      | PROCEDURE  | FUNCTION      | PACKAGE
      | TYPE       | SUBTYPE       | CONSTANT
      | SIGNAL     | VARIABLE      | COMPONENT
      | LABEL      | LITERAL       | UNITS
      | GROUP      | FILE

    entity_name_list ::=
        entity_designator { , entity_designator }
      | OTHERS
      | ALL

    entity_designator ::= entity_tag [ signature ]

    entity_tag ::= simple_name | character_literal | operator_symbol
-}
attributeSpecification :: Parser AttributeSpecification
attributeSpecification =
  try (reserved "attribute") >>
  AttributeSpecification <$> (attributeDesignator <* reserved "of") <*>
  (entitySpecification <* reserved "is") <*>
  expression <*
  semi

entitySpecification :: Parser EntitySpecification
entitySpecification =
  EntitySpecification <$> (entityNameList <* colon) <*> entityClass

entityClass :: Parser EntityClass
entityClass =
  choice
    [ reserved "architecture"  *> pure ARCHITECTURE
    , reserved "component"     *> pure COMPONENT
    , reserved "configuration" *> pure CONFIGURATION
    , reserved "constant"      *> pure CONSTANT
    , reserved "entity"        *> pure ENTITY
    , reserved "file"          *> pure FILE
    , reserved "function"      *> pure FUNCTION
    , reserved "group"         *> pure GROUP
    , reserved "label"         *> pure LABEL
    , reserved "literal"       *> pure LITERAL
    , reserved "package"       *> pure PACKAGE
    , reserved "procedure"     *> pure PROCEDURE
    , reserved "signal"        *> pure SIGNAL
    , reserved "subtype"       *> pure SUBTYPE
    , reserved "type"          *> pure TYPE
    , reserved "units"         *> pure UNITS
    , reserved "variable"      *> pure VARIABLE
    ]

entityNameList :: Parser EntityNameList
entityNameList =
  choice
    [ reserved "others" *> pure ENLOthers
    , reserved "all" *> pure ENLAll
    , ENLDesignators <$> commaSep1 entityDesignator
    ]

entityDesignator :: Parser EntityDesignator
entityDesignator = EntityDesignator <$> entityTag <*> optional signature

entityTag :: Parser EntityTag
entityTag =
  choice
    [ETChar <$> charLiteral, ETOp <$> operatorSymbol, ETName <$> simpleName]

--------------------------------------------------------------------------------
-- * 5.2 Configuration specification
{-
    configuration_specification ::=
      FOR component_specification binding_indication ;

    component_specification ::=
      instantiation_list : component_name

    instantiation_list ::=
        instantiation_label { , instantiation_label }
      | OTHERS
      | ALL

-}
configurationSpecification :: Parser ConfigurationSpecification
configurationSpecification =
  reserved "for" >>
  ConfigurationSpecification <$> componentSpecification <*> bindingIndication <*
  semi

componentSpecification :: Parser ComponentSpecification
componentSpecification =
  ComponentSpecification <$> (instantiationList <* colon) <*> name

instantiationList :: Parser InstantiationList
instantiationList =
  choice
    [ reserved "all" *> pure ILAll
    , reserved "others" *> pure ILOthers
    , ILLabels <$> commaSep label
    ]

--------------------------------------------------------------------------------
-- ** 5.2.1 Binding indication
{-
    binding_indication ::=
      [ USE entity_aspect ]
      [ generic_map_aspect ]
      [ port_map_aspect ]
-}
bindingIndication :: Parser BindingIndication
bindingIndication =
  BindingIndication <$> optional (reserved "use" *> entityAspect) <*>
  optional genericMapAspect <*>
  optional portMapAspect

--------------------------------------------------------------------------------
-- *** 5.2.1.1 Entity aspect
{-
    entity_aspect ::=
        ENTITY entity_name [ ( architecture_identifier) ]
      | CONFIGURATION configuration_name
      | OPEN
-}
entityAspect :: Parser EntityAspect
entityAspect =
  choice
    [ reserved "entity" >> EAEntity <$> name <*> optional (parens identifier)
    , reserved "configuration" >> EAConfig <$> name
    , reserved "open" *> pure EAOpen
    ]

--------------------------------------------------------------------------------
-- *** 5.2.1.2 Generic map and port map aspects
{-
    generic_map_aspect ::=
      GENERIC MAP ( generic_association_list )

    port_map_aspect ::=
      PORT MAP ( port_association_list )
-}
genericMapAspect :: Parser GenericMapAspect
genericMapAspect =
  reserved "generic" >> reserved "map" >>
  GenericMapAspect <$> parens associationList

portMapAspect :: Parser PortMapAspect
portMapAspect =
  trace "portMapAspect" $
  reserved "port" >> reserved "map" >> PortMapAspect <$> parens associationList

--------------------------------------------------------------------------------
-- * 5.2.4 Physical types
-- * 5.2.4.1 General
{-
physical_type_definition ::=
   range_constraint
       units
       primary_unit_declaration
       { secondary_unit_declaration }
    end units [ physical_type_simple_name ]
primary_unit_declaration ::= identifier ;
secondary_unit_declaration ::= identifier = physical_literal ;
physical_literal ::= [ abstract_literal ] unit_name
-}

--------------------------------------------------------------------------------
-- * 5.3 Disconnection specification
{-
    disconnection_specification ::=
      DISCONNECT guarded_signal_specification AFTER time_expression ;

    guarded_signal_specification ::=
      guarded_signal_list : type_mark

    signal_list ::=
        signal_name { , signal_name }
      | OTHERS
      | ALL
-}

disconnectionSpecification :: Parser DisconnectionSpecification
disconnectionSpecification =
  reserved "disconnect" >>
  DisconnectionSpecification <$>
  (guardedSignalSpecification <* reserved "after") <*>
  expression <*
  semi

guardedSignalSpecification :: Parser GuardedSignalSpecification
guardedSignalSpecification =
  GuardedSignalSpecification <$> (signalList <* colon) <*> typeMark

signalList :: Parser SignalList
signalList =
  choice
    [ reserved "all" *> pure SLAll
    , reserved "others" *> pure SLOthers
    , SLName <$> commaSep1 name
    ]


--------------------------------------------------------------------------------
--
--                                   -- 6 --
--
--                                    Names
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- * 6.1 Names
{-
    name ::=
        simple_name
      | operator_symbol
      | selected_name
      | indexed_name
      | slice_name
      | attribute_name

    prefix ::=
        name
      | function_call
-}
-- TODO: Maybe explicitly handle base_specifiers (avoid try in expr)
-- FIXME: This wrapping of name' seems to have hurt performance
name :: Parser Name
name = name' (fail "")

-- TODO: Function calls as prefixes is probably poorly handled
name' :: Parser a -> Parser Name
name' p = firstPart >>= rest
  where
    firstPart :: Parser Name
    -- possible hack: a name is most certainly a name if the initial simple
    -- name is not followed by '( (qualified expr) or a string delimiter (bitstring lit)
    firstPart =
      antiQ AntiName ((NSimple <$> simpleName) <|> NOp <$> operatorSymbol) <*
      notFollowedBy ((symbol "'" >> symbol "('") <|> stringDelimiter)
    rest :: Name -> Parser Name
    rest context =
      trace "rest" $
      try (lookAhead p >> pure context) <|>
      choice
        [ dot >> (suffix <?> "selected_name") >>=
          rest . NSelect . SelectedName context
        , try (attributeName context <?> "attribute_name") >>= rest . NAttr
        , try $ (sliceName context <?> "slice_name") >>= rest . NSlice
        , (indexedName context <?> "indexed_name") >>= rest . NIndex
        , pure context
        ]

--------------------------------------------------------------------------------
-- * 6.2 Simple names
{-
    simple_name ::= identifier
-}
simpleName :: Parser SimpleName
simpleName = trace "simpleName" identifier

--------------------------------------------------------------------------------
-- * 6.3 Selected names
{-
    selected_name ::= prefix . suffix

    suffix ::=
        simple_name
      | character_literal
      | operator_symbol
      | ALL
-}

selectedName :: Parser SelectedName
selectedName = NSimple <$> try (simpleName <* dot) >>= rest
  where
    rest :: Name -> Parser SelectedName
    rest n =
      choice
        [ try (suffix <* dot) >>= rest . NSelect . SelectedName n
        , suffix >>= pure . SelectedName n
        ]

suffix :: Parser Suffix
suffix =
  trace "suffix" $
  choice
    [ SSimple <$> simpleName
    , SChar <$> charLiteral
    , SOp <$> operatorSymbol
    , pure SAll <* reserved "all"
    ]

--------------------------------------------------------------------------------
-- * 6.4 Indexed names
{-
    indexed_name ::= prefix ( expression { , expression } )
-}
indexedName :: Name -> Parser IndexedName
indexedName n =
  trace "indexedName" $ IndexedName n <$> parens (commaSep1 expression)

--------------------------------------------------------------------------------
-- * 6.5 Slice names
{-
    slice_name ::= prefix ( discrete_range )
-}
sliceName :: Name -> Parser SliceName
sliceName m = SliceName m <$> parens discreteRange

--------------------------------------------------------------------------------
-- * 6.6 Attribute names
{-
    attribute_name ::=
      prefix [ signature ] ' attribute_designator [ ( expression ) ]

    attribute_designator ::= attribute_simple_name
-}
attributeName :: Name -> Parser AttributeName
attributeName n =
  trace ("attributeName " ++ show n) $
  (AttributeName n <$> optional signature <*
   (symbol "\'" <* try (notFollowedBy (symbol "(")))) <*>
  attributeDesignator <*>
  optional (try (parens expression))

-- LRM08 15.10 explicitly defines range and subtype as reserved words that are
-- also predefined attributes names so we prevent them from reaching the
-- identifier parser (this is a bit of a hack)
attributeDesignator :: Parser SimpleName
attributeDesignator =
  choice
    [ Ident <$> try (reserved "range" *> pure "range")
    , Ident <$> try (reserved "subtype" *> pure "subtype")
    , simpleName
    ]

--------------------------------------------------------------------------------
--
--                                   -- 7 --
--
--                                 Expression
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- * 7.1 Rules for expressions
{-

-- XXX Added this. makes more sense now
expression ::=
condition_operator primary
| logical_expression

    expression ::=
        relation { AND relation }
      | relation { OR relation }
      | relation { XOR relation }
      | relation [ NAND relation ]
      | relation [ NOR relation ]
      | relation { XNOR relation }

    relation ::=
      shift_expression [ relational_operator shift_expression ]

    shift_expression ::=
      simple_expression [ shift_operator simple_expression ]

    simple_expression ::=
      [ sign ] term { adding_operator term }

    term ::=
      factor { multiplying_operator factor }

    factor ::=
        primary [ ** primary ]
      | ABS primary
      | NOT primary

    primary ::=
        name
      | literal
      | aggregate
      | function_call
      | qualified_expression
      | type_conversion
      | allocator
      | ( expression )
-}
expression :: Parser Expression
expression =
  trace "expression" $
  makeExprParser primary table

-- TODO: typeConversion is indistinguishable from functionCalls and
-- indexedNames with a single expression. FunctionCalls with only actualPart
-- parameters are indistinguishable from indexedNames
primary :: Parser Expression
primary =
  antiQ AntiExpr $
  choice
    [ PrimAlloc <$> allocator
    , PrimAgg <$> aggregate
    , PrimExp <$> parens expression
    , PrimQual <$> try qualifiedExpression
    , PrimName <$> try name
    , PrimLit <$> try literal
    , PrimFun <$> try functionCall
    , PrimTCon <$> typeConversion
    ]

table :: [[Operator Parser  Expression]]
table =
  [ [InfixL (Binary <$> binOpPrec1), Prefix (Unary <$> unOpPrec1)]
  , [Prefix (Unary <$> unOpPrec2)]
  , [InfixL (Binary <$> binOpPrec3)]
  , [InfixL (Binary <$> binOpPrec4)]
  , [InfixL (Binary <$> binOpPrec5)]
  , [InfixL (Binary <$> binOpPrec6)]
  , [InfixL (Binary <$> binOpPrec7)]
  ]

--------------------------------------------------------------------------------
-- * 7.2 Operators
{-
    logical_operator ::= AND | OR | NAND | NOR | XOR | XNOR

    relational_operator ::= = | /= | < | <= | > | >=

    shift_operator ::= SLL | SRL | SLA | SRA | ROL | ROR

    adding_operator ::= + | – | &

    sign ::= + | –

    multiplying_operator ::= * | / | MOD | REM

    miscellaneous_operator ::= ** | ABS | NOT
-}

data OpType = ResOp | ResWord

unOpPrec1 :: Parser UnOp
unOpPrec1 = choice [binResWord "abs" Abs, binResWord "not" Not]

binOpPrec1 :: Parser BinOp
binOpPrec1 = choice [binOp "**" Exp]

unOpPrec2 :: Parser UnOp
unOpPrec2 = choice [binOp "+" Identity, binOp "-" Negation]

binOpPrec3 :: Parser BinOp
binOpPrec3 =
  choice
    [ binOp "*" Times
    , binOpGuard "/" "=" Div
    , binResWord "mod" Mod
    , binResWord "rem" Rem
    ]

binOpPrec4 :: Parser BinOp
binOpPrec4 = choice [binOp "+" Plus, binOp "-" Minus, binOp "&" Concat]

binOpPrec5 :: Parser BinOp
binOpPrec5 =
  choice
    [ binResWord "sll" Sll
    , binResWord "srl" Srl
    , binResWord "sla" Sla
    , binResWord "sra" Sra
    , binResWord "rol" Rol
    , binResWord "ror" Ror
    ]

binOpPrec6 :: Parser BinOp
binOpPrec6 =
  choice
    [ binOpGuard "=" ">" Eq
    , binOp "/=" Neq
    , binOpGuard' "<" ["=", ">"] Lt
    , binOp "<=" Lte
    , binOpGuard ">" "=" Gt
    , binOp ">=" Gte
    ]

binOpPrec7 :: Parser BinOp
binOpPrec7 =
  choice
    [ binResWord "and" And
    , binResWord "or" Or
    , binResWord "nand" Nand
    , binResWord "nor" Nor
    , binResWord "xor" Xor
    , binResWord "xnor" Xnor
    ]

binResWord :: Text -> a -> Parser a
binResWord op t = reserved op *> pure t

binOp :: Text ->  a -> Parser a
binOp op t = symbol op *> pure t

binOpGuard :: Text -> Text -> a -> Parser a
binOpGuard op nop = binOpGuard' op [nop]

binOpGuard' :: Text -> [Text] -> a -> Parser a
binOpGuard' op nop t =
  try (symbol op <* notFollowedBy (choice (map symbol nop))) *> pure t

makeOpParser :: [(OpType, Text, a)] -> Parser a
makeOpParser = choice . map oneOp
  where
    oneOp (ResOp, op, t)   = symbol op *> pure t
    oneOp (ResWord, op, t) = reserved op *> pure t

-- makeOpParser :: [(OpType, Text, a)] -> Parser a
-- makeOpParser = choice . map oneOp
--   where
--     oneOp (ResOp, op, t)   = reservedOp op *> pure t
--     oneOp (ResWord, op, t) = reserved op   *> pure t

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** 7.2.1 Logical operators
-- ...
--------------------------------------------------------------------------------
-- ** 7.2.2 Relational operators
-- ...
-- ** 7.2.3 Shift operators
-- ...
--------------------------------------------------------------------------------
-- ** 7.2.4 Adding operators
-- ...
--------------------------------------------------------------------------------
-- ** 7.2.5 Sign operators
-- ...
--------------------------------------------------------------------------------
-- ** 7.2.6 Multiplying operators
-- ...
--------------------------------------------------------------------------------
-- ** 7.2.7 Miscellaneous operators
-- ...
--------------------------------------------------------------------------------
-- * 7.3 Operands
--------------------------------------------------------------------------------
-- ** 7.3.1 Literals
{-
    literal ::=
        numeric_literal
      | enumeration_literal
      | string_literal
      | bit_string_literal
      | NULL

    numeric_literal
        astract_literal
      | physical_literal
-}
-- TODO: We parse enum literals referred to by identifier as name since we
-- cannot distinguish. Document this!
literal :: Parser Literal
literal =
  antiQ AntiLit $
  choice
    [ LitEnum . EChar <$> charLiteral
    -- A String literal followed by an opening parenthesis is a function call
    , LitString <$> (stringLiteral <* notFollowedBy (symbol "("))
    , LitBitString <$> try bitStringLiteral
    , LitNum <$> try numericLiteral
    , reserved "null" *> pure LitNull
    ]

numericLiteral :: Parser NumericLiteral
numericLiteral =
  antiQ AntiLitnum $
  choice [NLitPhysical <$> try physicalLiteral, NLitAbstract <$> abstractLiteral]

--------------------------------------------------------------------------------
-- ** 7.3.2 Aggregates
{-
    aggregate ::=
      ( element_association { , element_association } )

    element_association ::=
      [ choices => ] expression

    choices ::= choice { | choice }

    choice ::=
        simple_expression
      | discrete_range
      | element_simple_name
      | OTHERS
-}

-- FIXME: Combine with parser for expr wrapped in parens and reduce aggregate to
--  ( expr ) when aggregate only contains a single expression.
aggregate :: Parser Aggregate
aggregate =
  trace "aggregate" $ Aggregate <$> parens (commaSep1 elementAssociation)

elementAssociation :: Parser ElementAssociation
elementAssociation =
  antiQ2 AntiElAssoc AntiElAssocs $
  ElementAssociation <$> optional (try (choices <* symbol "=>")) <*>
  expression

-- VHDL allows for obscure character substitutions (| -> !).
choices :: Parser Choices
choices = Choices <$> choice' `sepBy1` (symbol "|" <|> symbol "!")

-- FIXME: Replacing simpleExpression by expression
-- FIXME: expression <|> simpleName will never reach simpleName -> Reduce
-- expressions only containing a simpleName and replace with a parser parsing
-- common subexpression first
-- FIXME: Why does descreteRange have to be before expression for choices' not
-- to fail on "add to load"
choice' :: Parser Choice
choice' =
  choice
    [ ChoiceRange <$> try discreteRange
    , ChoiceSimple <$> try expression
    , ChoiceName <$> simpleName
    , reserved "others" *> pure ChoiceOthers
    ]

--------------------------------------------------------------------------------
-- *** 7.3.2.1 Record aggregates
-- ...
--------------------------------------------------------------------------------
-- *** 7.3.2.2 Array aggregates
-- ...
--------------------------------------------------------------------------------
-- ** 7.3.3 Function calls
{-
    function_call ::=
      function_name [ ( actual_parameter_part ) ]

    actual_parameter_part ::= parameter_association_list
-}
-- TODO: Consider removing this parser and instead parsing functionCalls as
-- names. We have no way of disambiguating
functionCall :: Parser FunctionCall
functionCall =
  try $ FunctionCall <$> functionName <*> optional (parens actualParameterPart)

-- TODO: This may be a too restrictive definition of what a function name can be
functionName :: Parser FunctionName
functionName =
  choice
    [ FNOp <$> operatorSymbol
    , FNSelected <$> try selectedName
    , FNSimple <$> try simpleName
    ]

actualParameterPart :: Parser AssociationList
actualParameterPart = associationList

--------------------------------------------------------------------------------
-- ** 7.3.4 Qualified expressions
{-
    qualified_expression ::=
        type_mark ' ( expression )
      | type_mark ' aggregate
-}
qualifiedExpression :: Parser QualifiedExpression
qualifiedExpression =
  -- TODO: Common parsing of type_mark' with try
  trace "qualifiedExpression" $
  choice
    [ try $ QualAgg <$> typeMark <*> (symbol "'" *> aggregate)
    , QualExp <$> typeMark <*> (symbol "'" *> parens expression)
    ]

--------------------------------------------------------------------------------
-- ** 7.3.5 Type conversions
{-
    type_conversion ::= type_mark ( expression )
-}
typeConversion :: Parser TypeConversion
typeConversion = TypeConversion <$> typeMark <*> parens expression

--------------------------------------------------------------------------------
-- ** 7.3.6 Allocators
{-
    allocator ::=
        NEW subtype_indication
      | NEW qualified_expression
-}
allocator :: Parser Allocator
allocator =
  try (reserved "new") >>
  choice
    [try $ AllocQual <$> qualifiedExpression, AllocSub <$> subtypeIndication]

--------------------------------------------------------------------------------
-- * 7.4 Static expressions
--------------------------------------------------------------------------------
-- ** 7.4.1 Locally static primaries
-- ...
--------------------------------------------------------------------------------
-- ** 7.4.2 Globally static primaries
-- ...
--------------------------------------------------------------------------------
-- * 7.5 Universal expressions
-- ...
--------------------------------------------------------------------------------
--
--                                   -- 8 --
--
--                             Sequential statements
--
--------------------------------------------------------------------------------
{-
    sequence_of_statements ::= { sequential_statement }

    sequential_statement ::=
        wait_statement
      | assertion_statement
      | report_statement
      | signal_assignment_statement
      | variable_assignment_statement
      | procedure_call_statement
      | if_statement
      | case_statement
      | loop_statement
      | next_statement
      | exit_statement
      | return_statement
      | null_statement
-}
sequenceOfStatements :: Parser SequenceOfStatements
sequenceOfStatements = many sequentialStatement

-- TODO: Should we use the try combinator with all reserved words?  TODO: Rework
-- label handling. Parse labels only here and only push labels for block
-- statements
sequentialStatement :: Parser SequentialStatement
sequentialStatement =
  antiQ2 AntiSeqStm AntiSeqStms $
  stmLabel
    (\l ->
       choice
         [ SWait <$> waitStatement l
         , SAssert <$> assertionStatement l
         , SReport <$> reportStatement l
         , SIf <$> ifStatement l
         , SCase <$> caseStatement l
         , SLoop <$> loopStatement l
         , SExit <$> exitStatement l
         , SNext <$> nextStatement l
         , SReturn <$> returnStatement l
         , SNull <$> nullStatement l
         , SSignalAss <$> signalAssignmentStatement l
         , SVarAss <$> variableAssignmentStatement l
         , SProc <$> procedureCallStatement l
         ])

--------------------------------------------------------------------------------
-- * 8.1 Wait statement
{-
    wait_statement ::=
      [ label : ] WAIT [ sensitivity_clause ] [ condition_clause ] [ timeout_clause ] ;

    sensitivity_clause ::= ON sensitivity_list

    sensitivity_list ::= signal_name { , signal_name }

    condition_clause ::= UNTIL conditionn

    condition ::= boolean_expression

    timeout_clause ::= FOR time_expression
-}
waitStatement :: Maybe Label -> Parser WaitStatement
waitStatement l =
  trace "waitStatemen" try $
  reserved "wait" >>
  WaitStatement l <$> optional sensitivityClause <*>
  optional conditionClause <*>
  optional timeoutClause <*
  semi

sensitivityClause :: Parser SensitivityClause
sensitivityClause = SensitivityClause <$> (reserved "on" *> sensitivityList)

sensitivityList :: Parser SensitivityList
sensitivityList = SensitivityList <$> name `sepBy1` comma

conditionClause :: Parser ConditionClause
conditionClause = ConditionClause <$> (reserved "until" *> condition)
  -- FIXME: Check this, grammar says boolean_expression but syntax says expression

condition :: Parser Expression
condition = expression

-- FIXME: Check this, grammar says time_expression but syntax says Expression
timeoutClause :: Parser TimeoutClause
timeoutClause = TimeoutClause <$> (reserved "for" *> expression)

--------------------------------------------------------------------------------
-- * 8.2 Assertion statement
{-
    assertion_statement ::= [ label : ] assertion ;

    assertion ::=
      ASSERT condition
        [ REPORT expression ]
        [ SEVERITY expression ]
-}
assertionStatement :: Maybe Label -> Parser AssertionStatement
assertionStatement l =
  trace
    "assertionStatement"
    (AssertionStatement l <$> try assertion <*
     semi <?> "Assertion statement")

assertion :: Parser Assertion
assertion =
  Assertion <$> try (reserved "assert" *> condition) <*>
  optional (reserved "report" *> expression) <*>
  optional (reserved "severity" *> expression) <?> "Assertion"

--------------------------------------------------------------------------------
-- * 8.3 Report statement
{-
    report_statement ::=
      [ label : ]
        REPORT expression
          [ SEVERITY expression ] ;
-}
reportStatement :: Maybe Label -> Parser ReportStatement
reportStatement l =
  trace "reportStatement" $
  reserved "report" >>
  ReportStatement l <$> expression <*>
  optional (reserved "severity" *> expression) <*
  semi

--------------------------------------------------------------------------------
-- * 8.4 Signal assignment statement
{-
    signal_assignment_statement ::=
      [ label : ] target <= [ delay_mechanism ] waveform ;

    delay_mechanism ::=
        TRANSPORT
      | [ REJECT time_expression ] INERTIAL

    target ::=
        name
      | aggregate

    waveform ::=
        waveform_element { , waveform_element }
      | UNAFFECTED
-}
signalAssignmentStatement :: Maybe Label
                          -> Parser SignalAssignmentStatement
signalAssignmentStatement l =
  trace "signalAssignmentStatement" $
  SignalAssignmentStatement l <$> try (target <* symbol "<=") <*>
  optional delayMechanism <*>
  waveform <*
  semi

delayMechanism :: Parser DelayMechanism
delayMechanism =
  choice
    [ reserved "transport" *> pure DMechTransport
    , DMechInertial <$> optional (reserved "reject" *> expression) <*
      reserved "inertial"
    ]

-- FIXME: Maybe ambigous?
target :: Parser Target
target = trace "target" $ choice [TargetAgg <$> aggregate, TargetName <$> name]

waveform :: Parser Waveform
waveform =
  antiQ AntiWave $
  trace "waveform" $
  choice
    [ WaveElem <$> commaSep1 waveformElement
    , reserved "unaffected" *> pure WaveUnaffected
    ]

--------------------------------------------------------------------------------
-- ** 8.4.1 Updating a projected output waveform
{-
    waveform_element ::=
        value_expression [ AFTER time_expression ]
      | null [ AFTER time_expression ]
-}
waveformElement :: Parser WaveformElement
waveformElement =
  choice
    [ WaveENull <$>
      (reserved "null" *> optional (reserved "after" *> expression))
    , WaveEExp <$> expression <*>
      optional (reserved "after" *> expression)
    ]

--------------------------------------------------------------------------------
-- * 8.5 Variable assignment statement
{-
    variable_assignment_statement ::=
      [ label : ] target := expression ;
-}
variableAssignmentStatement :: Maybe Label -> Parser VariableAssignmentStatement
variableAssignmentStatement l =
  trace "variableAssignmentStatement" $
  VariableAssignmentStatement l <$> try (target <* symbol ":=") <*>
  (expression <* semi)

--------------------------------------------------------------------------------
-- ** 8.5.1 Array variable assignments
-- ...
--------------------------------------------------------------------------------
-- * 8.6 Procedure call statement
{-
    procedure_call_statement ::= [ label : ] procedure_call ;

    procedure_call ::= procedure_name [ ( actual_parameter_part ) ]
-}
-- TODO: Can we disambigously distinguish between variable assignments and
-- procedure calls without knowing what names are defined as
procedureCallStatement :: Maybe Label -> Parser ProcedureCallStatement
procedureCallStatement l =
  trace "procedureCallStatement" $
  ProcedureCallStatement l <$> procedureCall <* semi

-- FIXME: Parse function calls correctly. Grammar allows for all names to be
-- used as function names. We currently restrict to simple names and selected
-- names to avoid ambiguities with sliced names and indexed names
procedureCall :: Parser ProcedureCall
procedureCall =
  ProcedureCall <$> (NSelect <$> try selectedName <|> NSimple <$> simpleName) <*>
  optional (parens actualParameterPart)

-- * 8.7 If statement
{-
    if_statement ::=
      [ if_label : ]
        IF condition THEN
          sequence_of_statements
        { ELSEIF condition THEN
          sequence_of_statements }
        [ ELSE
          sequence_of_statements ]
        END IF [ if_label ] ;
-}
ifStatement :: Maybe Label -> Parser IfStatement
ifStatement lab =
  trace "ifStatement" $
  stmLabelPush
    lab
    (\l ->
       IfStatement l <$>
       ((,) <$> (reserved "if" *> condition <* reserved "then") <*>
        sequenceOfStatements) <*>
       many
         ((,) <$> (reserved "elsif" *> condition <* reserved "then") <*>
          sequenceOfStatements) <*>
       optional (reserved "else" *> sequenceOfStatements) <*
       (reserved "end" >> reserved "if" >> optionEndNameLabel l >> semi))

--------------------------------------------------------------------------------
-- * 8.8 Case statement
{-
    case_statement ::=
      [ case_label : ]
        CASE expression IS
          case_statement_alternative
          { case_statement_alternative }
        END CASE [ case_label ] ;

    case_statement_alternative ::=
      WHEN choices =>
        sequence_of_statements
-}
caseStatement :: Maybe Label -> Parser CaseStatement
caseStatement lab =
  labeledBlock
    "case"
    lab
    (\l ->
       CaseStatement l <$> (expression <* reserved "is") <*>
       some caseStatementAlternative)

caseStatementAlternative :: Parser CaseStatementAlternative
caseStatementAlternative =
  antiQ2 AntiCasealt AntiCasealts $
  reserved "when" >>
  CaseStatementAlternative <$> (choices <* symbol "=>") <*> sequenceOfStatements

--------------------------------------------------------------------------------
-- * 8.9 Loop statement
{-
    loop_statement ::=
      [ loop_label : ]
        [ iteration_scheme ] LOOP
          sequence_of_statements
        END LOOP [ loop_label ] ;

    iteration_scheme ::=
        WHILE condition
      | FOR loop_parameter_specification

    parameter_specification ::=
      identifier IN discrete_range
-}

loopStatement :: Maybe Label -> Parser LoopStatement
loopStatement lab =
  stmLabelPush lab
    (\l ->
       LoopStatement l <$> (optional iterationScheme <* reserved "loop") <*>
       sequenceOfStatements <*
       (reserved "end" >> reserved "loop" >> optionEndNameLabel l >> semi))

iterationScheme :: Parser IterationScheme
iterationScheme =
  choice
    [ reserved "while" >> (IterWhile <$> condition)
    , reserved "for" >> (IterFor <$> parameterSpecification)
    ]

parameterSpecification :: Parser ParameterSpecification
parameterSpecification =
  ParameterSpecification <$> identifier <* reserved "in" <*> discreteRange

--------------------------------------------------------------------------------
-- * 8.10 Next statement
{-
    next_statement ::=
      [ label : ] NEXT [ loop_label ] [ WHEN condition ] ;
-}
-- TODO: Verify that label matches a loop in scope
nextStatement :: Maybe Label -> Parser NextStatement
nextStatement l =
  reserved "next" >>
  NextStatement l <$> optional label <*>
  optional (reserved "when" *> condition) <*
  semi

--------------------------------------------------------------------------------
-- * 8.11 Exit statement
{-
    exit_statement ::=
      [ label : ] EXIT [ loop_label ] [ WHEN condition ] ;
-}
-- TODO: Verify that label refers to a lop in scope
exitStatement :: Maybe Label -> Parser ExitStatement
exitStatement l =
  reserved "exit" >>
  ExitStatement l <$> optional label <*>
  optional (reserved "when" >> condition) <*
  semi

-- * 8.12 Return statement
{-
    return_statement ::=
      [ label : ] RETURN [ expression ] ;
-}
returnStatement :: Maybe Label -> Parser ReturnStatement
returnStatement l =
       reserved "return" >>
       ReturnStatement l <$> optional expression <* semi

--------------------------------------------------------------------------------
-- * 8.13 Null statement
{-
    null_statement ::=
      [ label : ] NULL ;
-}
nullStatement :: Maybe Label -> Parser NullStatement
nullStatement l = reserved "null" *> semi *> pure (NullStatement l)

--------------------------------------------------------------------------------
-- ** 9.6.1 Instantiation of a component
--------------------------------------------------------------------------------
-- ** 9.6.2 Instantiation of a design entity
--------------------------------------------------------------------------------
-- * 9.7 Generate statements
{-
    generate_statement ::=
      generate_label :
        generation_scheme GENERATE
          [ { block_declarative_item }
        BEGIN ]
          { concurrent_statement }
        END GENERATE [ generate_label ] ;

    generation_scheme ::=
        FOR generate_parameter_specification
      | IF condition

    label ::= identifier
-}

generateStatement :: Maybe Label -> Parser GenerateStatement
generateStatement lab =
  stmLabelPush
    lab
    (\l ->
       GenerateStatement <$> labelRequired l <*>
       (generationScheme <* reserved "generate") <*>
       try (optional (many blockDeclarativeItem <* reserved "begin")) <*>
       concurrentStatements <*
       (reserved "end" >> reserved "generate" >> optionEndNameLabel l >> semi))

generationScheme :: Parser GenerationScheme
generationScheme =
  choice
    [ reserved "for" >> GSFor <$> parameterSpecification
    , reserved "if" >> GSIf <$> condition
    ]

label :: Parser Identifier
label = identifier

--------------------------------------------------------------------------------
--
--                                   -- 9 --
--
--                            Concurrent statements
--
--------------------------------------------------------------------------------
{-
    concurrent_statement ::=
        block_statement
      | process_statement
      | concurrent_procedure_call_statement
      | concurrent_assertion_statement
      | concurrent_signal_assignment_statement
      | component_instantiation_statement
      | generate_statement
-}
-- FIXME: component instantiation/procedure calls are ambigous
concurrentStatement :: Parser ConcurrentStatement
concurrentStatement =
  antiQ2 AntiConStm AntiConStms $
  stmLabel
    (\l ->
       choice
         [ ConBlock <$> blockStatement l
         , ConProcess <$> processStatement l
         , ConAssertion <$> concurrentAssertionStatement l
         , ConSignalAss <$> try (concurrentSignalAssignmentStatement l)
         , ConProcCall <$> try (concurrentProcedureCallStatement l)
         , ConComponent <$> componentInstantiationStatement l
         , ConGenerate <$> generateStatement l
         ])

concurrentStatements :: Parser [ConcurrentStatement]
concurrentStatements = many concurrentStatement

--------------------------------------------------------------------------------
-- * 9.1 Block statement
{-
    block_statement ::=
      block_label :
        BLOCK [ ( guard_expression ) ] [ IS ]
          block_header
          block_declarative_part
        BEGIN
          block_statement_part
        END BLOCK [ block_label ] ;

    block_header ::=
      [ generic_clause
        [ generic_map_aspect ; ] ]
      [ port_clause
        [ port_map_aspect ; ] ]

    block_declarative_part ::=
      { block_declarative_item }

    block_statement_part ::=
      { concurrent_statement }
-}

blockStatement :: Maybe Label -> Parser BlockStatement
blockStatement lab =
  labeledBlock
    "block"
    lab
    (\l ->
       BlockStatement <$> labelRequired l <*> optional (parens expression) <*
       optional (reserved "is") <*>
       blockHeader <*>
       blockDeclarativePart <*
       reserved "begin" <*>
       blockStatementPart)

blockHeader :: Parser BlockHeader
blockHeader = do
  genClause <- try (optional genericClause)
  genMap <- try (optional (genericMapAspect <* semi))
  thisPortClause <- try (optional portClause)
  portMap <- try (optional (portMapAspect <* semi))
  let genPart = maybe Nothing (\g -> Just (g, genMap)) genClause
  let portPart = maybe Nothing (\p -> Just (p, portMap)) thisPortClause
  return $ BlockHeader genPart portPart

blockDeclarativePart :: Parser BlockDeclarativePart
blockDeclarativePart = many blockDeclarativeItem

blockStatementPart :: Parser BlockStatementPart
blockStatementPart = many concurrentStatement

--------------------------------------------------------------------------------
-- * 9.2 Process statement
{-
    process_statement ::=
      [ process_label : ]
        [ POSTPONED ] PROCESS [ ( sensitivity_list ) ] [ IS ]
          process_declarative_part
        BEGIN
          process_statement_part
        END [ POSTPONED ] PROCESS [ process_label ] ;

    process_declarative_part ::=
      { process_declarative_item }

    process_declarative_item ::=
        subprogram_declaration
      | subprogram_body
      | type_declaration
      | subtype_declaration
      | constant_declaration
      | variable_decloration
      | file_declaration
      | alias_declaration
      | attribute_declaration
      | attribute_specification
      | use_clause
      | group_type_declaration

    process_statement_part ::=
      { sequential_statement }
-}
processStatement :: Maybe Label -> Parser ProcessStatement
processStatement lab =
  stmLabelPush
    lab
    (\l -> do
       postponed <- try (isReserved "postponed" <* reserved "process")
       ProcessStatement l postponed <$>
         (optional (parens sensitivityList) <* optional (reserved "is")) <*>
         processDeclarativePart <*>
         (reserved "begin" *> processStatementPart) <*
         reserved "end" <*
         when postponed (void $ reserved "postponed") <*
         reserved "process" <*
         optionEndNameLabel l <*
         semi) <?>
  "process"

processDeclarativePart :: Parser [ProcessDeclarativeItem]
processDeclarativePart = many processDeclarativeItem

processDeclarativeItem :: Parser ProcessDeclarativeItem
processDeclarativeItem =
  antiQ2 AntiProcDecl AntiProcDecls $
  trace "processDeclarativeItemm" $
  choice
    [ subprogramDeclOrBody PDISubprogDecl PDISubprogBody
    , PDIType <$> typeDeclaration
    , PDISubtype <$> subtypeDeclaration
    , PDIConstant <$> constantDeclaration
    , PDIVariable <$> variableDeclaration
    , PDIFile <$> fileDeclaration
    , PDIAttrDecl <$> attributeDeclaration
    , PDIAttrSpec <$> attributeSpecification
    , PDIAlias <$> aliasDeclaration
    , PDIUseClause <$> useClause
    ]

processStatementPart :: Parser [SequentialStatement]
processStatementPart = trace "ProcessStatementPart" $ many sequentialStatement

--------------------------------------------------------------------------------
-- * 9.3 Concurrent procedure call statements
{-
    concurrent_procedure_call_statement ::=
      [ label : ] [ POSTPONED ] procedure_call ;
-}
concurrentProcedureCallStatement :: Maybe Label -> Parser ConcurrentProcedureCallStatement
concurrentProcedureCallStatement l =
  ConcurrentProcedureCallStatement l <$> try (isReserved "postponed") <*>
  procedureCall <*
  semi

--------------------------------------------------------------------------------
-- * 9.4 Concurrent assertion statements
{-
    concurrent_assertion_statement ::=
      [ label : ] [ POSTPONED ] assertion ;
-}
concurrentAssertionStatement :: Maybe Label -> Parser ConcurrentAssertionStatement
concurrentAssertionStatement l =
  ConcurrentAssertionStatement l <$> isReserved "postponed" <*> assertion <*
  semi <?> "Concurrent assertion"

--------------------------------------------------------------------------------
-- * 9.5 Concurrent signal assignment statements
{-
    concurrent_signal_assignment_statement ::=
        [ label : ] [ POSTPONED ] conditional_signal_assignment
      | [ label : ] [ POSTPONED ] selected_signal_assignment

    options ::= [ GUARDED ] [ delay_mechanism ]
-}
concurrentSignalAssignmentStatement :: Maybe Label -> Parser ConcurrentSignalAssignmentStatement
concurrentSignalAssignmentStatement l =
  choice
    [ CSASSelect l <$> isReserved "postponed" <*> selectedSignalAssignment
    , CSASCond l <$> isReserved "postponed" <*> conditionalSignalAssignment
    ]

options :: Parser Options
options = Options <$> isReserved "guarded" <*> optional delayMechanism

--------------------------------------------------------------------------------
-- ** 9.5.1 Conditional signal assignments
{-
    conditional_signal_assignment ::=
      target <= options conditional_waveforms ;

    conditional_waveforms ::=
      { waveform WHEN condition ELSE }
      waveform [ WHEN condition ]
-}
conditionalSignalAssignment :: Parser ConditionalSignalAssignment
conditionalSignalAssignment =
  ConditionalSignalAssignment <$> try (target <* symbol "<=") <*> options <*>
  conditionalWaveforms <*
  semi

conditionalWaveforms :: Parser ConditionalWaveforms
conditionalWaveforms = ConditionalWaveforms <$> many (try cwOptional) <*> cwWave
  where
    cwOptional =
      (,) <$> (waveform <* reserved "when") <*> (condition <* reserved "else")
    cwWave = (,) <$> waveform <*> optional (reserved "when" *> condition)

--------------------------------------------------------------------------------
-- ** 9.5.2 Selected signal assignments
{-
    selected_signal_assignment ::=
      WITH expression SELECT
        target <= options selected_waveforms ;

    selected_waveforms ::=
      { waveform WHEN choices , }
      waveform WHEN choices
-}
selectedSignalAssignment :: Parser SelectedSignalAssignment
selectedSignalAssignment =
  try $
  reserved "with" >>
  SelectedSignalAssignment <$> (expression <* reserved "select") <*>
  (target <* symbol "<=") <*>
  options <*>
  selectedWaveforms <*
  semi

selectedWaveforms :: Parser SelectedWaveforms
selectedWaveforms =
  SelectedWaveforms <$>
  ((,) <$> (waveform <* reserved "when") <*> choices) `sepBy1` comma

--------------------------------------------------------------------------------
-- * 9.6 Component instantiation statements
{-
    component_instantiation_statement ::=
      instantiation_label :
        instantiated_unit
          [ generic_map_aspect ]
          [ port_map_aspect ] ;

    instantiated_unit ::=
        [ COMPONENT ] component_name
      | ENTITY entity_name [ ( architecture_identifier ) ]
      | CONFIGURATION configuration_name
-}
componentInstantiationStatement :: Maybe Label -> Parser ComponentInstantiationStatement
componentInstantiationStatement l =
  ComponentInstantiationStatement <$> labelRequired l <*> instantiatedUnit <*>
  optional genericMapAspect <*>
  optional portMapAspect <*
  semi

instantiatedUnit :: Parser InstantiatedUnit
instantiatedUnit =
  choice
    [ IUConfig <$> try (reserved "configuration" *> name)
    , IUEntity <$> try (reserved "entity" *> name) <*>
      optional (parens identifier)
    , optional (reserved "component") >> IUComponent <$> name
    ]

--------------------------------------------------------------------------------
-- ** 10.4 Use clauses
{-
    use_clause ::=
      USE selected_name { , selected_name } ;
-}
useClause :: Parser UseClause
useClause = reserved "use" >> UseClause <$> selectedName `sepBy1` comma <* semi

--------------------------------------------------------------------------------
--
--                                  -- 11 --
--
--                        Design units and their analysis
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- ** 11.1 Design units
{-
    design_file ::= design_unit { design_unit }

    design_unit ::= context_clause library_unit

    library_unit ::=
        primary_unit
      | secondary_unit

    primary_unit ::=
        entity_declaration
      | configuration_declaration
      | package_declaration

    secondary_unit ::=
        architecture_body
      | package_body
-}
designFile :: Parser DesignFile
designFile = DesignFile <$> (spaceConsumer *> some designUnit)

designUnit :: Parser DesignUnit
designUnit = antiQ AntiDesignUnit $ DesignUnit <$> contextClause <*> libraryUnit

libraryUnit :: Parser LibraryUnit
libraryUnit =
  antiQ AntiLibraryUnit $
  choice [LibrarySecondary <$> secondaryUnit, LibraryPrimary <$> primaryUnit]

primaryUnit :: Parser PrimaryUnit
primaryUnit = antiQ AntiPrimaryUnit $
  choice
    [ PrimaryEntity <$> entityDeclaration
    , PrimaryConfig <$> configurationDeclaration
    , PrimaryPackage <$> packageDeclaration
    ]

secondaryUnit :: Parser SecondaryUnit
secondaryUnit =
  antiQ AntiSecondaryUnit $
  trace
    "secondaryUnit"
    choice
    [ try (lookAhead (reserved "package" >> reserved "body")) >>
      SecondaryPackage <$> packageBody
    , SecondaryArchitecture <$> architectureBody
    ]

--------------------------------------------------------------------------------
-- ** 11.2 Design libraries
{-
    library_clause ::= LIBRARY logical_name_list ;

    logical_name_list ::= logical_name { , logical_name }

    logical_name ::= identifier
-}
libraryClause :: Parser LibraryClause
libraryClause = reserved "library" >> LibraryClause <$> logicalNameList <* semi

logicalNameList :: Parser LogicalNameList
logicalNameList = LogicalNameList <$> logicalName `sepBy1` comma

logicalName :: Parser Identifier
logicalName = identifier

--------------------------------------------------------------------------------
-- ** 11.3 Context clauses
{-
    context_clause ::= { context_item }

    context_item ::=
        library_clause
      | use_clause
-}
contextClause :: Parser ContextClause
contextClause = ContextClause <$> many contextItem

contextItem :: Parser ContextItem
contextItem =
  antiQ2 AntiContextItem AntiContextItems $
  choice [ContextLibrary <$> libraryClause, ContextUse <$> useClause]
