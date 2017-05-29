{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Language.VHDL.Parser.Internal
  ( designFile
  , expression
  , sequentialStatement
  , sequenceOfStatements
  , concurrentStatement
  , concurrentStatements
  , name
  ) where

import           Prelude                    hiding (exponent)
import           Text.Parsec                hiding (label)
import           Text.Parsec.Expr

import           Control.Monad              (void, when)
import           Data.Data                  (Data)
import qualified Data.Functor.Identity
import           Data.Maybe                 (isJust)

import           Language.VHDL.Lexer
import           Language.VHDL.Parser.Monad
import           Language.VHDL.Parser.Util
import           Language.VHDL.Pretty
import           Language.VHDL.Syntax

---------------------------------------------------------------------------------
-- Util functions
---------------------------------------------------------------------------------
trace :: t -> a -> a
trace _ = id

-- Hack to avoid circular imports between lexer and util modules
antiQ
  :: (Data a)
  => (String -> a) -> Parser a -> Parser a
antiQ = antiQ' identifier

isReserved :: String -> Parser Bool
isReserved a = isJust <$> optionMaybe (reserved a)

-- Match block statements with optional label
optionEndNameLabel :: Maybe Label -> Parser ()
optionEndNameLabel l =
  case l of
    Just (Ident s)     -> optionEndName s
    Just (AntiIdent _) -> fail "This houldn't be an anti quotation"
    Nothing            -> return ()

-- TODO: Is having e.g. package foo ... end foo instead of end package foo valid?
optionEndName :: String -> Parser ()
optionEndName s = do
  expected <- popBlockName
  -- FIXME: Error message points to end of list
  actual <- optionMaybe simpleName
  case actual of
    Just n ->
      when (n /= expected) $
      unexpected
        (s ++ " block " ++ ppr expected ++ " cannot be ended by " ++ ppr n)
    Nothing -> return ()

block :: String -> Parser a -> Parser a
block s p =
  reserved s >>
  p <* (reserved "end" *> optional (reserved s)) <* optionEndName s <* semi

blockN :: [String] -> Parser a -> Parser a
blockN s p =
  mapM_ reserved s >>
  p <* (reserved "end" *> optional (mapM_ reserved s)) <*
  optionEndName (unwords s) <*
  semi

stmLabelPush :: (Parser (Maybe Label) -> Parser a) -> Parser a
stmLabelPush =
  stmLabel'
    (\s ->
       trace ("PUUUUUUUU " ++ show s) $
       case s of
         Just a  -> void $ pushBlockName a
         Nothing -> return ())

--(\s -> whenIsJust s (\i -> isJust pushBlockName i >> return ()))
stmLabel :: (Parser (Maybe Label) -> Parser a) -> Parser a
stmLabel = stmLabel' (\_ -> return ())

stmLabel' :: (Maybe Label -> Parser ())
          -> (Parser (Maybe Label) -> Parser a)
          -> Parser a
stmLabel' f g
  --lab <- optionMaybe $ (trace "stmLabel") $ try (label <* reservedOp ":")
 = do
  lab <- optionMaybe $ try (label <* reservedOp ":")
  f lab
  g (trace ("Label: " ++ show lab) pure lab)

lookaheadLabel :: String -> Parser a -> Parser a
lookaheadLabel s p =
  try $ lookAhead (optional (label <* reservedOp ":") >> reserved s) >> p

blockName :: Parser Identifier
blockName = simpleName >>= pushBlockName
  -- label <- optionMaybe (label <* colon)
  -- f label

labelRequired :: Parser (Maybe Label) -> Parser Label
labelRequired l = do
  lab <- l
  case lab of
    Just l' -> return l'
    Nothing -> fail "A label is required here"

--runStateParnse p sn imp
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
  optionMaybe (reserved "begin" *> entityStatementPart)

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
  EntityHeader <$> optionMaybe genericClause <*> optionMaybe portClause

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
genericList = genericInterfaceList

--------------------------------------------------------------------------------
-- *** 1.1.1.2 Ports
{-
    port_list ::= port_interface_list
-}
portList :: Parser InterfaceList
portList = portInterfaceList

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
    -- , EDIFile <$> fileDeclaration
    -- , EDIAlias <$> aliasDeclaration
    -- , EDIAttrDecl <$> attributeDeclaration
    -- , EDIAttrSpec <$> attributeSpecification
    -- , EDIDiscSpec <$> disconnectionSpecification
    , EDIUseClause <$> useClause
    -- , EDIGroupTemp <$> groupTempDeclaration
    -- , EDIGroup <$> groupDeclaration
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
entityStatementPart = many1 entityStatement

entityStatement :: Parser EntityStatement
entityStatement =
  choice
    [ ESConcAssert <$> concurrentAssertionStatement
     -- , ESPassiveConc <$> concurrentProcedureCallStatement
     -- , ESPassiveProc <$> processStatement
     -- TODO
    ]

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
  choice
    [ subprogramDeclOrBody BDISubprogDecl BDISubprogBody
    , BDIType <$> typeDeclaration
    , BDISubtype <$> subtypeDeclaration
    , BDIConstant <$> constantDeclaration
    , BDISignal <$> signalDeclaration
    , BDIShared <$> variableDeclaration
    , BDIFile <$> fileDeclaration
    -- , BDIAlias <$> aliasDeclaration
    , BDIComp <$> componentDeclaration
    -- , BDIAttrDecl <$> attributeDeclaration
    -- , BDIAttrSepc <$> attributeSpecification
    -- , BDIConfigSepc <$> configurationSpecification
    -- , BDIDisconSpec <$> disconnectionSpecification
    , BDIUseClause <$> useClause
    -- , BDIGroupTemp <$> groupTemplateDeclaration
    -- , BDIGroup <$> groupDeclaration
    -- TODO
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
  ConfigurationDeclaration <$> (identifier <* reserved "of") <*>
  (name <* reserved "is") <*>
  configurationDeclarativePart <*>
  blockConfiguration

configurationDeclarativePart :: Parser ConfigurationDeclarativePart
configurationDeclarativePart = many configurationDeclarativeItem

configurationDeclarativeItem :: Parser ConfigurationDeclarativeItem
configurationDeclarativeItem =
  choice
    [ CDIUse <$> useClause
     -- , CDIAttrSpec <$> attributeSpecification
     -- , CDIGroup <$> groupDeclaration
     -- TODO:
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
    [ BSGen <$> label <*> pure Nothing -- TODO <*> optionMaybe indexSpeification
    , BSArch <$> name
    , BSBlock <$> label
    ]

indexSpecification :: Parser IndexSpecification
indexSpecification =
  choice [ISRange <$> try discreteRange, ISExp <$> expression]

configurationItem :: Parser ConfigurationItem
configurationItem =
  choice [CIBlock <$> blockConfiguration, CIComp <$> componentConfiguration]

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
-- FIXME: Missing end for semi
componentConfiguration :: Parser ComponentConfiguration
componentConfiguration =
  reserved "for" >>
  ComponentConfiguration <$> componentSpecification <*>
  optionMaybe bindingIndication <*>
  optionMaybe blockConfiguration

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
subprogramDeclaration = trace "subprogramDeclaration" pure

subprogramDeclaration' ::  Parser SubprogramDeclaration
subprogramDeclaration' =
  trace "subprogramDeclaration'" $ subprogramSpecification <* semi

subprogramSpecification :: Parser SubprogramSpecification
subprogramSpecification = subprogramProcedure <|> subprogramFunction

subprogramProcedure :: Parser SubprogramSpecification
subprogramProcedure =
  reserved "procedure" >>
  SubprogramProcedure <$> designator <*>
  optionMaybe (parens formalParameterList)

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
  SubprogramFunction <$> optionMaybe ispure <*>
  (reserved "function" >> designator) <*>
  optionMaybe (parens formalParameterList) <*>
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
  (reserved "end" *> optionMaybe subprogramKind) <*>
  (optionMaybe designator <* semi)
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
      -- TODO
     -- , SDIFile <$> fileDeclaration
     -- , SDIAttrDecl <$> attributeDeclaration
     -- , SDIAttrSepc <$> attributeSpecification
     -- , SDIUseClause <$> useClause
     -- , SDIGroupTemp <$> groupTemplateDeclaration
     -- , SDIGroup <$> groupDeclaration
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
  cleanupNothing . Signature <$>
  optionMaybe
    ((,) <$> optionMaybe (commaSep typeMark) <*>
     optionMaybe (reserved "return" *> typeMark))
    -- Quick hack to make sure that an empty signature will actually parse to
    -- Nothing
    -- TODO: Clean up AST?
  where
    cleanupNothing (Signature (Just (Just [], Nothing))) = Signature Nothing
    cleanupNothing a                                     = a

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
  block "package" $
  PackageDeclaration <$> (blockName <* reserved "is") <*> packageDeclarativePart

packageDeclarativePart :: Parser [PackageDeclarativeItem]
packageDeclarativePart = many packageDeclarativeItem

packageDeclarativeItem :: Parser PackageDeclarativeItem
packageDeclarativeItem =
  choice
    [ subprogramDeclOrBody PHDISubprogDecl PHDISubprogBody
    , PHDIType <$> typeDeclaration
    , PHDISubtype <$> subtypeDeclaration
    , PHDIConstant <$> constantDeclaration
    , PHDISignal <$> signalDeclaration
    , PHDIShared <$> variableDeclaration
    , PHDIFile <$> fileDeclaration
    -- TODO
    --, PHDfIAlias <$> aliasDeclaration
    , PHDIComp <$> componentDeclaration
    -- TODO
    --, PHDIAttrDecl <$> attributeDeclaration
    --, PHDIAttrSpec <$> attributeSpecification
    --, PHDIDiscSpec <$> disconnectionSpecification
    , PHDIUseClause <$> useClause
    -- TODO
    -- , PHDIGroupTemp <$> groupTemplateDeclaration
    -- , PHDIGroup <$> groupDeclaraiton
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
packageBody =
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
    -- TODO
    -- , PBDIAlias aliasDeclaration
    , PBDIUseClause <$> useClause
    -- TODO
    -- , PBDIGroupTemp groupTemplateDeclaration
    -- , PBDIGroup groupDeclaration
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
  choice
    [ ScalarEnum <$> enumerationTypeDefinition
    , ScalarInt <$> integerTypeDefinition
    , ScalarFloat <$> floatingTypeDefinition
    , ScalarPhys <$> physicalTypeDefinition
    ]

rangeConstraint :: Parser RangeConstraint
rangeConstraint = reserved "range" >> RangeConstraint <$> range

-- range :: Parser Range
-- range = choice [ RAttr <$> attributeName
--                , RSimple <$> simpleExpression <*> direction <*> simpleExpression
--                ]
-- FIMXE: Replacing simpleExpression by expression
range :: Parser Range
range =
  choice
    [ RSimple <$> expression <*> direction <*> expression
    , RAttr <$> (name >>= attributeName) -- FIXME: This won't work
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
enumerationLiteral = choice [EChar <$> try charLiteral, EId <$> identifier]

--------------------------------------------------------------------------------
-- *** 3.1.1.1 Predefined enumeration types
-- predefined ... todo
--------------------------------------------------------------------------------
-- ** 3.1.2 Integer types
{-
    integer_type_definition ::= range_constraint
-}
integerTypeDefinition :: Parser RangeConstraint
integerTypeDefinition = rangeConstraint

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
physicalTypeDefinition :: Parser PhysicalTypeDefinition
physicalTypeDefinition =
  PhysicalTypeDefinition <$> rangeConstraint <*>
  (reserved "units" *> primaryUnitDeclaration) <*>
  many secondaryUnitDeclaration <*>
  (reserved "end" *> reserved "units" *> optionMaybe identifier)

primaryUnitDeclaration :: Parser Identifier
primaryUnitDeclaration = identifier

secondaryUnitDeclaration :: Parser SecondaryUnitDeclaration
secondaryUnitDeclaration =
  SecondaryUnitDeclaration <$> identifier <*> physicalLiteral

physicalLiteral :: Parser PhysicalLiteral
-- TODO: Actual unit names instead of just name?
physicalLiteral = PhysicalLiteral <$> optionMaybe abstractLiteral <*> name

--------------------------------------------------------------------------------
-- *** 3.1.3.1 Predefined physical types
-- predefined ... todo
--------------------------------------------------------------------------------
-- ** 3.1.4 Floating point types
{-
    floating_type_definition ::= range_constraint
-}
floatingTypeDefinition :: Parser RangeConstraint
floatingTypeDefinition = rangeConstraint

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
  try $
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
  IndexSubtypeDefinition <$> typeMark <* reserved "range" <* reservedOp "<>"

indexConstraint :: Parser IndexConstraint
indexConstraint = IndexConstraint <$> parens (commaSep1 discreteRange)

discreteRange :: Parser DiscreteRange
discreteRange = choice [DRSub <$> subtypeIndication, DRRange <$> range]

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
  RecordTypeDefinition <$> many1 elementDeclaration <*>
  (reserved "end" *> reserved "record" *> optionMaybe simpleName)

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
incompleteTypeDeclaration :: Parser IncompleteTypeDeclaration
incompleteTypeDeclaration =
  reserved "type" >> IncompleteTypeDeclaration <$> identifier

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
    -- , DAlias <$> aliasDeclaration
    -- , DComponent <$> componentDelaration
    -- , DAttribute <$> attributeDeclaration
    -- , DGroupTemplate <$> groupTemplateDeclaration
    -- , DGroup <$> groupDeclaration
    , DEntity <$> entityDeclaration
    , DConfiguration <$> configurationDeclaration
    , DSubprogram <$> subprogramDeclaration'
    , DPackage <$> packageDeclaration
                     -- TODO
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
  choice
    [TDFull <$> fullTypeDeclaration, TDPartial <$> incompleteTypeDeclaration]

fullTypeDeclaration :: Parser FullTypeDeclaration
fullTypeDeclaration =
  reserved "type" >>
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
  try $
  reserved "subtype" >>
  SubtypeDeclaration <$> identifier <*>
  (reserved "is" *> subtypeIndication <* semi)

-- FIXME: Should bit_vector(length - 1 downto 0) be parsed as subtypeIndication
-- a simpleName typeMark followed by a subtypeIndication constraint or a
-- subtypeIndication containing a sliceName>
subtypeIndication :: Parser SubtypeIndication
subtypeIndication = trace "subtypeindication" $ go <*> optionMaybe constraint
  where
    go = do
      name1 <- name
      optionMaybe (try typeMark) >>= \case
        Just ty -> return $ SubtypeIndication (Just name1) ty
        Nothing -> return $ SubtypeIndication Nothing (TMType name1)

typeMark :: Parser TypeMark
-- Syntax defines subtype names separately, but we have no way of distinguisthing
typeMark = TMType <$> name

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
  optionMaybe (reservedOp ":=" *> expression) <*
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
  optionMaybe signalKind <*>
  optionMaybe (reservedOp ":=" *> expression) <*
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
  VariableDeclaration
   -- <$> (try (reserved "shared" *> reserved "variable" *> pure True)
   -- <|> (reserved "variable" *> pure False))
   <$>
  (reserved "variable" *> pure False) <*>
  (identifierList <* colon) <*>
  subtypeIndication <*>
  optionMaybe (reservedOp ":=" *> expression) <*
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
  optionMaybe fileOpenInformation <*
  semi

fileOpenInformation :: Parser FileOpenInformation
fileOpenInformation =
  FileOpenInformation <$> optionMaybe (reserved "open" *> expression) <*>
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
  optional (reserved "constant") >>
  InterfaceConstantDeclaration <$>
  (identifierList <* colon <* optional (reserved "in")) <*>
  subtypeIndication <*>
  optionMaybe expression

interfaceSignalDeclaration :: Parser InterfaceDeclaration
interfaceSignalDeclaration =
  try $
  optional (reserved "signal") >>
  InterfaceSignalDeclaration <$> (identifierList <* colon) <*>
  optionMaybe interfaceMode <*>
  subtypeIndication <*>
  choice [reserved "bus" *> pure True, pure False] <*>
  optionMaybe expression

interfaceVariableDeclaration :: Parser InterfaceDeclaration
interfaceVariableDeclaration =
  try $
  optional (reserved "variable") >>
  InterfaceVariableDeclaration <$> (identifierList <* colon) <*>
  optionMaybe interfaceMode <*>
  subtypeIndication <*>
  optionMaybe expression

interfaceFileDeclaration :: Parser InterfaceDeclaration
interfaceFileDeclaration =
  reserved "file" >>
  InterfaceFileDeclaration <$> (identifierList <* colon) <*> subtypeIndication

interfaceMode :: Parser Mode
interfaceMode =
  choice
    [ reserved "in" *> pure In
    , reserved "out" *> pure Out
    , reserved "inout" *> pure InOut
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
interfaceList = InterfaceList <$> semiSep1 interfaceElement

-- These were added to disambiguate the many occurences of interface_list int he grammer
genericInterfaceList :: Parser InterfaceList
genericInterfaceList = InterfaceList <$> semiSep1 interfaceConstantDeclaration

portInterfaceList :: Parser InterfaceList
portInterfaceList = InterfaceList <$> semiSep1 interfaceSignalDeclaration

interfaceElement :: Parser InterfaceDeclaration
interfaceElement =
  choice
    [ interfaceConstantDeclaration
    , interfaceSignalDeclaration
    , interfaceVariableDeclaration
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
  AssociationElement <$>
  optionMaybe (try (formalPart <* trace "reservedOp" (reservedOp "=>"))) <*>
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
    [ reserved "open" *> pure ADOpen
    , ADVariable <$> try name
    , ADExpression <$> try expression
    , ADSignal <$> name
    , ADFile <$> name
    ]

actualPart :: Parser ActualPart
actualPart =
  trace "actualPart" $
  choice
    [ try $ APType <$> typeMark <*> parens actualDesignator
    -- FIXME: This looks ambigous
    , try $ APFunction <$> name <*> parens actualDesignator
    , APDesignator <$> actualDesignator

--------------------------------------------------------------------------------
-- ** 4.3.3 Alias declarations
{-
    alias_declaration ::=
      ALIAS alias_designator [ : subtype_indication ] IS name [ signature ] ;

    alias_designator ::= identifier | character_literal | operator_symbol
-}
aliasDeclaration :: Parser AliasDeclaration
aliasDeclaration =
  reserved "alias" >>
  AliasDeclaration <$> aliasDesignator <*>
  optionMaybe (colon *> subtypeIndication) <*>
  (reserved "is" *> name) <*>
  optionMaybe signature <*
  semi

aliasDesignator :: Parser AliasDesignator
aliasDesignator =
  choice
    [ ADOperator <$> operatorSymbol
    , ADCharacter <$> charLiteral
    , ADIdentifier <$> identifier
    ]

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
  optionMaybe genericClause <*>
  optionMaybe portClause
  -- <*> (reserved "end" *> reserved "component"
  --   *> optionMaybe simpleName <* semi)

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
  BindingIndication <$> optionMaybe (reserved "use" *> entityAspect) <*>
  optionMaybe genericMapAspect <*>
  optionMaybe portMapAspect

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
    [ reserved "entity" >> EAEntity <$> name <*> optionMaybe (parens identifier)
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
-- physicalLiteral = Parser PhysicalLiteral
-- physicalLiteral = PhysicalLiteral <$>
--                   optionMaybe abstractLiteral <*>
--                   unitName
-- unitName :: Parser UnitName
-- unitName = choice [ symbol "fs" >> pure Fs
--                   , symbol "ps" >> pure Ps
--                   , symbol "ns" >> pure Ns
--                   , symbol "us" >> pure Us
--                   , symbol "ms" >> pure Ms
--                   , symbol "sec" >> pure Sec
--                   ]
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

name :: Parser Name
name = antiQ AntiName $ NSimple <$> simpleName >>= rest
  where
    rest :: Name -> Parser Name
    rest context =
      trace "rest" $
      choice
        [ dot >> suffix >>= rest . NSelect . SelectedName context
        , attributeName context >>= rest . NAttr
        , sliceName context >>= rest . NSlice
        , indexedName context >>= rest . NIndex
        , pure context
        ]

prefix :: Parser Prefix
prefix =
  trace "prefix" $
  choice [PName  <$> name, PFun <$> functionCall]

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
selectedName = do
  n <- name
  case n of
    (NSelect s) -> return s
    _           -> unexpected "Expected a selectedName"

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
  trace ("attributeName" ++ show n) $
  try (AttributeName n <$> (optionMaybe signature <* char '\'')) <*>
  attributeDesignator <*>
  optionMaybe (parens expression)

attributeDesignator :: Parser SimpleName
attributeDesignator = simpleName

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
expression = antiQ AntiExpr $ buildExpressionParser table primary

primary :: Parser Expression
primary =
  antiQ AntiExpr $
  choice
    [ PrimQual <$> try qualifiedExpression
    , PrimFun <$> functionCall
    , PrimName <$> name
    , PrimLit <$> literal
    , PrimAgg <$> aggregate
    , PrimTCon <$> typeConversion
    , PrimAlloc <$> allocator
    , PrimExp <$> parens expression
    ]

table :: [[Operator String ParseState Data.Functor.Identity.Identity Expression]]
table =
  [ [Infix (Binary <$> binOpPrec1) AssocLeft, Prefix (Unary <$> unOpPrec1)]
  , [Prefix (Unary <$> unOpPrec2)]
  , [Infix (Binary <$> binOpPrec3) AssocLeft]
  , [Infix (Binary <$> binOpPrec4) AssocLeft]
  , [Infix (Binary <$> binOpPrec5) AssocLeft]
  , [Infix (Binary <$> binOpPrec6) AssocLeft]
  , [Infix (Binary <$> binOpPrec7) AssocLeft]
  ]

timeExpression :: Parser TimeExpression
timeExpression = TimeExpression <$> physicalLiteral

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
unOpPrec1 :: Parser UnOp
unOpPrec1 = makeOpParser [("abs", Abs), ("not", Not)]

binOpPrec1 :: Parser BinOp
binOpPrec1 = makeOpParser [("**", Exp)]

unOpPrec2 :: Parser UnOp
unOpPrec2 = makeOpParser [("+", Identity), ("-", Negation)]

binOpPrec3 :: Parser BinOp
binOpPrec3 = makeOpParser [("*", Times), ("/", Div), ("mod", Mod), ("rem", Rem)]

binOpPrec4 :: Parser BinOp
binOpPrec4 = makeOpParser [("+", Plus), ("-", Minus), ("&", Concat)]

binOpPrec5 :: Parser BinOp
binOpPrec5 =
  makeOpParser
    [("sll", Sll), ("srl", Srl), ("sla", Sla), ("rol", Rol), ("ror", Ror)]

binOpPrec6 :: Parser BinOp
binOpPrec6 =
  makeOpParser
    [("=", Eq), ("/=", Neq), ("<", Lt), ("<=", Lte), (">", Gt), (">=", Gte)]

binOpPrec7 :: Parser BinOp
binOpPrec7 =
  makeOpParser
    [ ("and", And)
    , ("or", Or)
    , ("nand", Nand)
    , ("nor", Nor)
    , ("xor", Xor)
    , ("xnor", Xnor)
    ]

makeOpParser :: [(String, a)] -> Parser a
makeOpParser = choice . map oneOp
  where
    oneOp (op, t) = reservedOp op *> pure t

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
-- FIXME: Passing random text gives an IntLit?
literal :: Parser Literal
literal =
  choice
    [ LitEnum <$> enumerationLiteral
    , LitString <$> stringLiteral
    --, LitBitString <$> bitStringLiteral
    , reserved "null" *> pure LitNull
    , LitNum <$> numericLiteral
    ]

numericLiteral :: Parser NumericLiteral
numericLiteral =
  antiQ AntiLitnum $
  choice [NLitAbstract <$> abstractLiteral, NLitPhysical <$> physicalLiteral]

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
aggregate :: Parser Aggregate
aggregate =
  trace "aggregate" $ Aggregate <$> parens (commaSep1 elementAssociation)

-- FIXME: we mayu have a problem here
elementAssociation :: Parser ElementAssociation
elementAssociation = ElementAssociation Nothing <$> expression

--elementAssociation = ElementAssociation <$> optionMaybe (choices <* symbol "=>") <*> expression
choices :: Parser Choices
choices = Choices <$> choice' `sepBy1` symbol "|"

-- FIXME: Replacing simpleExpression by expression
choice' :: Parser Choice
choice' =
  choice
    [ ChoiceSimple <$> expression
    , ChoiceRange <$> discreteRange
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
-- TODO
functionCall :: Parser FunctionCall
functionCall = FunctionCall <$> functionName <*> optionMaybe (parens actualParameterPart)

functionName :: Parser Identifier
functionName = identifier

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
  trace "qualifiedExpression" $
  choice
    [ QualExp <$> typeMark <*> (symbol "'" *> parens expression)
    , QualAgg <$> typeMark <*> (symbol "'" *> aggregate)
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
  try (AllocSub <$> (reserved "new" *> subtypeIndication)) <|>
  (AllocQual <$> (reserved "new" *> qualifiedExpression))

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

sequentialStatement :: Parser SequentialStatement
sequentialStatement =
  stmLabel
    (\l ->
       choice
         [ SWait <$> waitStatement l
         , SAssert <$> assertionStatement l
         , SReport <$> reportStatement
         , SIf <$> ifStatement
         -- , SCase <$> caseStatement
         , SLoop <$> loopStatement
         , SExit <$> exitStatement
         -- , SNext <$> nextStatement
         , SReturn <$> returnStatement
         -- , SNull <$> nullStatement
         , SSignalAss <$> signalAssignmentStatement l
         , SVarAss <$> variableAssignmentStatement l
         , SProc <$> procedureCallStatement l
         ])

--   where
--     lp = label' <* con
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
waitStatement :: Parser (Maybe Label) -> Parser WaitStatement
waitStatement l =
  trace "waitStatemen" try $
  reserved "wait" >>
  WaitStatement <$> l <*> optionMaybe sensitivityClause <*>
  optionMaybe conditionClause <*>
  optionMaybe timeoutClause <*
  semi

sensitivityClause :: Parser SensitivityClause
sensitivityClause = SensitivityClause <$> (reserved "on" *> sensitivityList)

sensitivityList :: Parser SensitivityList
sensitivityList = SensitivityList <$> name `sepBy1` comma

conditionClause :: Parser ConditionClause
conditionClause = ConditionClause <$> (reserved "util" *> condition)
  -- FIXME: Check this, grammar says boolean_expression but syntax says expression

condition :: Parser Expression
condition = expression

-- FIXME: Check this, grammar says time_expression but syntax says Expression
timeoutClause :: Parser TimeoutClause
timeoutClause = TimeoutClause <$> (reserved "for" *> timeExpression)

--------------------------------------------------------------------------------
-- * 8.2 Assertion statement
{-
    assertion_statement ::= [ label : ] assertion ;

    assertion ::=
      ASSERT condition
        [ REPORT expression ]
        [ SEVERITY expression ]
-}
assertionStatement :: Parser (Maybe Label) -> Parser AssertionStatement
assertionStatement l =
  trace
    "assertionStatement"
    (AssertionStatement <$> l <*> try assertion <*
     semi <?> "Assertion statement")

assertion :: Parser Assertion
assertion =
  Assertion <$> try (reserved "assert" *> condition) <*>
  optionMaybe (reserved "report" *> expression) <*>
  optionMaybe (reserved "severity" *> expression) <?> "Assertion"

--------------------------------------------------------------------------------
-- * 8.3 Report statement
{-
    report_statement ::=
      [ label : ]
        REPORT expression
          [ SEVERITY expression ] ;
-}
reportStatement :: Parser ReportStatement
reportStatement =
  trace "reportStatement" $
  stmLabel
    (\l ->
       reserved "report" >>
       ReportStatement <$> l <*> expression <*>
       optionMaybe (reserved "severity" *> expression) <*
       semi)

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
signalAssignmentStatement :: Parser (Maybe Label)
                          -> Parser SignalAssignmentStatement
signalAssignmentStatement l =
  trace "signalAssignmentStatement" $
  SignalAssignmentStatement <$> l <*> try (target <* reservedOp "<=") <*>
  optionMaybe delayMechanism <*>
  waveform <*
  semi

delayMechanism :: Parser DelayMechanism
delayMechanism =
  choice
    [ reserved "transport" *> pure DMechTransport
    , DMechInertial <$> optionMaybe (reserved "reject" *> timeExpression) <*
      reserved "intertial"
    ]

-- FIXME: Maybe ambigous?
target :: Parser Target
target = trace "target" $ choice [TargetAgg <$> aggregate, TargetName <$> name]

waveform :: Parser Waveform
waveform =
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
      (reserved "null" *> optionMaybe (reserved "after" *> timeExpression))
    , WaveEExp <$> expression <*>
      optionMaybe (reserved "after" *> timeExpression)
    ]

--------------------------------------------------------------------------------
-- * 8.5 Variable assignment statement
{-
    variable_assignment_statement ::=
      [ label : ] target := expression ;
-}
variableAssignmentStatement :: Parser (Maybe Label)
                            -> Parser VariableAssignmentStatement
variableAssignmentStatement l =
  trace "variableAssignmentStatement" $
  VariableAssignmentStatement <$> l <*> try (target <* reservedOp ":=") <*>
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
procedureCallStatement :: Parser (Maybe Label) -> Parser ProcedureCallStatement
procedureCallStatement l =
  trace "procedureCallStatement" $
  ProcedureCallStatement <$> l <*> procedureCall <* semi

procedureCall :: Parser ProcedureCall
procedureCall =
  ProcedureCall <$> name <*> optionMaybe (parens actualParameterPart)

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
ifStatement :: Parser IfStatement
ifStatement =
  trace "ifStatement" $
  stmLabelPush
    (\l ->
       IfStatement <$> l <*>
       ((,) <$> (reserved "if" *> condition <* reserved "then") <*>
        sequenceOfStatements) <*>
       many
         ((,) <$> (reserved "elseif" *> condition <* reserved "then") <*>
          sequenceOfStatements) <*>
       optionMaybe (reserved "else" *> sequenceOfStatements) <*
       (reserved "end" >> reserved "if" >> (optionEndNameLabel <$> l) >> semi))

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

loopStatement :: Parser LoopStatement
loopStatement =
  stmLabelPush
    (\l ->
       LoopStatement <$> l <*> (optionMaybe iterationScheme <* reserved "loop") <*>
       sequenceOfStatements <*
       (reserved "end" >> reserved "loop" >> (optionEndNameLabel <$> l) >> semi))

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
-- * 8.11 Exit statement
{-
    exit_statement ::=
      [ label : ] EXIT [ loop_label ] [ WHEN condition ] ;
-}
-- TODO: Verify that label refers to a lop in scope
exitStatement :: Parser ExitStatement
exitStatement =
  stmLabel
    (\l ->
       reserved "exit" >>
       ExitStatement <$> l <*> optionMaybe label <*>
       optionMaybe (reserved "when" >> condition) <*
       semi)

-- * 8.12 Return statement
{-
    return_statement ::=
      [ label : ] RETURN [ expression ] ;
-}
returnStatement :: Parser ReturnStatement
returnStatement =
  stmLabel
    (\l ->
       reserved "return" >>
       ReturnStatement <$> l <*> optionMaybe expression <* semi)

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
label :: Parser Identifier
label = identifier

newLabel :: Parser (Maybe Identifier)
newLabel = optionMaybe $ try (label <* colon)

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
concurrentStatement :: Parser ConcurrentStatement
concurrentStatement =
  choice --ConBlock <$> blockStatement
  -- TODO: Too many try here?
  -- YES! Replace with a lookahead
    [ ConProcess <$> lookaheadLabel "process" processStatement
    -- , ConProcCall <$> concurrentProcedureCallStatement
    -- , ConAssertion <$> concurrentAssertionStatement
    , ConSignalAss <$> try concurrentSignalAssignmentStatement
    , ConComponent <$> componentInstantiationStatement
    -- , ConGenerate <$> generateStatement
    ]

concurrentStatements :: Parser [ConcurrentStatement]
concurrentStatements = many concurrentStatement

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
processStatement :: Parser ProcessStatement
processStatement =
  stmLabelPush
    (\l ->
       ProcessStatement <$> l <*> isReserved "postponed" <*>
       try
         (reserved "process" >>
          optionMaybe (parens sensitivityList) <* optional (reserved "is")) <*>
       processDeclarativePart <*>
       (reserved "begin" *> processStatementPart) <*
       reserved "end" <*
       optional (reserved "postponed") <*
       reserved "process" <*
       optionEndName "process" <*
       semi) <?>
  "process"

processDeclarativePart :: Parser [ProcessDeclarativeItem]
processDeclarativePart = many processDeclarativeItem

processDeclarativeItem :: Parser ProcessDeclarativeItem
processDeclarativeItem =
  trace "processDeclarativeItemm" $
  choice
    [ subprogramDeclOrBody PDISubprogDecl PDISubprogBody
    , PDIType <$> typeDeclaration
    , PDISubtype <$> subtypeDeclaration
    , PDIConstant <$> constantDeclaration
    , PDIVariable <$> variableDeclaration
    -- TODO
    -- , PDIFile <$> fileDeclaration
    -- , PDIAlias <$> aliasDeclaration
    -- , PDIAttrDecl <$> attributeDeclaration
    -- , PDIAttrSpec <$> attributeSpecification
    , PDIUseClause <$> useClause
    ]

processStatementPart :: Parser [SequentialStatement]
processStatementPart = trace "ProcessStatementPart" $ many sequentialStatement

--------------------------------------------------------------------------------
-- * 9.4 Concurrent assertion statements
{-
    concurrent_assertion_statement ::=
      [ label : ] [ POSTPONED ] assertion ;
-}
concurrentAssertionStatement :: Parser ConcurrentAssertionStatement
concurrentAssertionStatement =
  ConcurrentAssertionStatement <$> newLabel <*> isReserved "postponed" <*>
  assertion <?> "Concurrent assertion"

--------------------------------------------------------------------------------
-- * 9.5 Concurrent signal assignment statements
{-
    concurrent_signal_assignment_statement ::=
        [ label : ] [ POSTPONED ] conditional_signal_assignment
      | [ label : ] [ POSTPONED ] selected_signal_assignment

    options ::= [ GUARDED ] [ delay_mechanism ]
-}
concurrentSignalAssignmentStatement :: Parser ConcurrentSignalAssignmentStatement
concurrentSignalAssignmentStatement =
  stmLabel
    (\l ->
       choice
         [ CSASSelect <$> l <*> isReserved "postponed" <*>
           selectedSignalAssignment
         , CSASCond <$> l <*> isReserved "postponed" <*>
           conditionalSignalAssignment
         ])

options :: Parser Options
options = Options <$> isReserved "guarded" <*> optionMaybe delayMechanism

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
  ConditionalSignalAssignment <$> try (target <* reservedOp "<=") <*> options <*>
  conditionalWaveforms <*
  semi

conditionalWaveforms :: Parser ConditionalWaveforms
conditionalWaveforms = ConditionalWaveforms <$> many (try cwOptional) <*> cwWave
  where
    cwOptional =
      (,) <$> (waveform <* reserved "when") <*> (condition <* reserved "else")
    cwWave = (,) <$> waveform <*> optionMaybe (reserved "when" *> condition)

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
  (target <* reservedOp "<=") <*>
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
-- FIXME: label is mandatory here
componentInstantiationStatement :: Parser ComponentInstantiationStatement
componentInstantiationStatement =
  stmLabel
    (\l ->
       ComponentInstantiationStatement <$> labelRequired l <*> instantiatedUnit <*>
       optionMaybe genericMapAspect <*>
       optionMaybe portMapAspect <*
       semi)

instantiatedUnit :: Parser InstantiatedUnit
instantiatedUnit =
  choice
    [ IUConfig <$> try (reserved "configuration" *> name)
    , IUEntity <$> try (reserved "entity" *> name) <*>
      optionMaybe (parens identifier)
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
designFile = DesignFile <$> (whiteSpace *> many1 designUnit)

designUnit :: Parser DesignUnit
designUnit = DesignUnit <$> contextClause <*> libraryUnit

libraryUnit :: Parser LibraryUnit
libraryUnit =
  choice [LibrarySecondary <$> secondaryUnit, LibraryPrimary <$> primaryUnit]

primaryUnit :: Parser PrimaryUnit
primaryUnit =
  choice
    [ PrimaryEntity <$> entityDeclaration
    , PrimaryConfig <$> configurationDeclaration
    , PrimaryPackage <$> packageDeclaration
    ]

secondaryUnit :: Parser SecondaryUnit
secondaryUnit =
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
  choice [ContextLibrary <$> libraryClause, ContextUse <$> useClause]

--------------------------------------------------------------------------------
--
--                                  -- 13 --
--
--                              Lexical elements
--
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- ** 13.4
{-
    abstract_literal ::= decimal_literal | based_literal
-}
abstractLiteral :: Parser AbstractLiteral
abstractLiteral =
  (ALitDecimal <$> decimalLiteral) <|> (ALitBased <$> basedLiteral)

--------------------------------------------------------------------------------
-- *** 13.4.1
--
-- I use Haskell's Integer to represent integers in VHDL. Its syntax seems to be
-- slightly different though (the underline part).
{-
    decimal_literal ::= integer [ . integer ] [ exponent ]

    integer ::= digit { [ underline ] digit }

    exponent ::= E [ + ] integer | E – integer
-}
decimalLiteral :: Parser DecimalLiteral
decimalLiteral =
  DecimalLiteral <$> integer <*> optionMaybe (dot *> integer) <*>
  optionMaybe exponent

exponent :: Parser Exponent
exponent =
  symbol "E" >>
  ((ExponentNeg <$> (symbol "-" *> integer)) <|>
   (ExponentPos <$> (optional (symbol "+") *> integer)))

--------------------------------------------------------------------------------
-- *** 13.4.2
{-
    based_literal ::=
      base # based_integer [ . based_integer ] # [ exponent ]

    base ::= integer

    based_integer ::=
      extended_digit { [ underline ] extended_digit }

    extended_digit ::= digit | letter
-}
basedLiteral :: Parser BasedLiteral
basedLiteral =
  BasedLiteral <$> base <*> (symbol "#" *> basedInteger) <*>
  (dot *> optionMaybe basedInteger) <*>
  optionMaybe (symbol "#" *> exponent)

base
  :: forall u.
     ParsecT String u Data.Functor.Identity.Identity Integer
base = integer

-- TODO: Probably case sensitive
basedInteger :: Parser BasedInteger
basedInteger = SLit . concat <$> many1 alphaNum `sepBy1` char '_'
