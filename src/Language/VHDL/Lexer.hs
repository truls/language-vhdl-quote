module Language.VHDL.Lexer
  ( abstractLiteral
  , angles
  , antiQ
  , bitStringLiteral
  , braces
  , brackets
  , charLiteral
  , colon
  , comma
  , commaSep
  , commaSep1
  , decimal
  , dot
  , float
  , hexadecimal
  , identifier
  , integer
  , lexeme
  , natural
  , naturalOrFloat
  , octal
  , operator
  , parens
  , reserved
  , reservedOp
  , semi
  , semiSep
  , semiSep1
  , squares
  , stringLiteral
  , symbol
  , whiteSpace
  ) where

import           Control.Arrow              (first)
import           Control.Monad              (unless)
import           Data.Char                  (chr, isDigit, toLower)
import           Data.Data                  (Data)
import           Data.Functor.Identity      (Identity)
import           Language.VHDL.Parser.Monad (Parser, quotesEnabled)
import           Language.VHDL.Parser.Util
import           Language.VHDL.Syntax
import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token          as P

vhdlReserved :: [String]
vhdlReserved =
  [ "abs"
  , "access"
  , "after"
  , "alias"
  , "all"
  , "and"
  , "architecture"
  , "array"
  , "assert"
  , "attribute"
  , "begin"
  , "block"
  , "body"
  , "buffer"
  , "bus"
  , "case"
  , "component"
  , "configuration"
  , "constant"
  , "disconnect"
  , "downto"
  , "else"
  , "elsif"
  , "end"
  , "entity"
  , "exit"
  , "file"
  , "for"
  , "function"
  , "generate"
  , "generic"
  , "group"
  , "guarded"
  , "if"
  , "impure"
  , "in"
  , "inertial"
  , "inout"
  , "is"
  , "label"
  , "library"
  , "linkage"
  , "literal"
  , "loop"
  , "map"
  , "mod"
  , "nand"
  , "new"
  , "next"
  , "nor"
  , "not"
  , "null"
  , "of"
  , "on"
  , "open"
  , "or"
  , "others"
  , "out"
  , "package"
  , "port"
  , "postponed"
  , "procedure"
  , "process"
  , "pure"
  , "range"
  , "record"
  , "register"
  , "reject"
  , "rem"
  , "report"
  , "return"
  , "rol"
  , "ror"
  , "select"
  , "severity"
  , "signal"
  , "shared"
  , "sla"
  , "sll"
  , "sra"
  , "srl"
  , "subtype"
  , "then"
  , "to"
  , "transport"
  , "type"
  , "unaffected"
  , "units"
  , "until"
  , "use"
  , "variable"
  , "wait"
  , "when"
  , "while"
  , "with"
  , "xnor"
  , "xor"
  , "access"
  , "after"
  , "alias"
  , "all"
  , "and"
  , "architecture"
  , "array"
  , "assert"
  , "attribute"
  , "begin"
  , "block"
  , "body"
  , "buffer"
  , "bus"
  , "case"
  , "component"
  , "configuration"
  , "constant"
  , "disconnect"
  , "downto"
  , "else"
  , "elsif"
  , "end"
  , "entity"
  , "exit"
  , "file"
  , "for"
  , "function"
  , "generate"
  , "generic"
  , "group"
  , "guarded"
  , "if"
  , "impure"
  , "in"
  , "inertial"
  , "inout"
  , "is"
  , "label"
  , "library"
  , "linkage"
  , "literal"
  , "loop"
  , "map"
  , "mod"
  , "nand"
  , "new"
  , "next"
  , "nor"
  , "not"
  , "null"
  , "of"
  , "on"
  , "open"
  , "or"
  , "others"
  , "out"
  , "package"
  , "port"
  , "postponed"
  , "procedure"
  , "process"
  , "pure"
  , "range"
  , "record"
  , "register"
  , "reject"
  , "rem"
  , "report"
  , "return"
  , "rol"
  , "ror"
  , "select"
  , "severity"
  , "signal"
  , "shared"
  , "sla"
  , "sll"
  , "sra"
  , "srl"
  , "subtype"
  , "then"
  , "to"
  , "transport"
  , "type"
  , "unaffected"
  , "units"
  , "until"
  , "use"
  , "variable"
  , "wait"
  , "when"
  , "while"
  , "with"
  , "xnor"
  , "xor"
  ]

vhdlOps :: [String]
vhdlOps =
  [ "abs"
  , "not"
  , "mod"
  , "rem"
  , "sll"
  , "srl"
  , "sla"
  , "sra"
  , "rol"
  , "ror"
  , "and"
  , "or"
  , "nand"
  , "nor"
  , "xor"
  , "xnor"
  ]

vhdlDef :: LanguageDef st
vhdlDef =
  emptyDef
  { P.commentStart = ""
  , P.commentEnd = ""
  , P.commentLine = "--"
  , P.nestedComments = False
  , P.identStart = letter
  , P.identLetter = alphaNum <|> char '_'
  , P.opLetter = oneOf "*/+-&<=>:"
  , P.opStart = P.opLetter vhdlDef
  , P.reservedNames = vhdlReserved
  , P.reservedOpNames = vhdlOps
  , P.caseSensitive = False
  }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser vhdlDef

reserved, reservedOp :: String -> Parser ()
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

operator, semi, comma, colon, dot :: Parser String
operator = P.operator lexer
semi = P.semi lexer
comma = P.comma lexer
colon = P.colon lexer
dot = P.dot lexer

charLiteral :: Parser CharacterLiteral
charLiteral =
  antiQ AntiClit $
  CLit <$> lexeme (char '\'' *> (char '"' <|> char '\\' <|> graphicalChar) <* char '\'')

stringLiteral :: Parser StringLiteral
stringLiteral = antiQ AntiSlit $ SLit <$> stringLiteral'

natural, decimal, hexadecimal, octal :: Parser Integer
natural = P.natural lexer
decimal = P.decimal lexer
hexadecimal = P.hexadecimal lexer
octal = P.octal lexer

float :: Parser Double
float = P.float lexer

naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = P.naturalOrFloat lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

lexeme, parens, braces, angles, brackets, squares :: Parser a -> Parser a
lexeme = P.lexeme lexer
parens = P.parens lexer
braces = P.braces lexer
angles = P.angles lexer
brackets = P.brackets lexer
squares = P.squares lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

semiSep, semiSep1, commaSep, commaSep1 :: Parser a -> Parser [a]
semiSep = P.semiSep lexer
semiSep1 = P.semiSep1 lexer
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer

------------------------------------------------------------------------------------
-- Antiquote parsing functions

parseAntiExpr :: Parser String
parseAntiExpr = firstPar
  where
    firstPar = do
      c <- char '('
      cs <- rest 1
      return (c : cs)
    rest :: Int -> Parser String
    rest 0 = return []
    rest nest = do
      c <- anyChar
      case c
        -- FIXME: Make this more roboust
            of
        '\\' -> do
          c2 <- anyChar
          cs <- rest nest
          return (c : c2 : cs)
        '(' -> do
          cs <- rest (nest + 1)
          return (c : cs)
        ')' -> do
          cs <- rest (nest - 1)
          return (c : cs)
        _ -> do
          cs <- rest nest
          return (c : cs)

antiQ
  :: (Data a)
  => (String -> a) -> Parser a -> Parser a
antiQ q p = try (lexeme parseQ) <|> p
  where
    parseQ = do
      _ <- char '$'
      let qn = toQQString $ q ""
      qs <- string qn
      _ <- char ':'
      identOrExpr <- optionMaybe $ lookAhead (char '(')
      i <-
        case identOrExpr of
          Just _ -> parseAntiExpr
          Nothing -> do
            (Ident i') <- identifier
            return i'
      qe <- quotesEnabled
      unless qe $ unexpected "QuasiQuotation syntax not emabled"
      unless (qs == qn) $
        unexpected $ "Wrong QuasiQuoter " ++ qn ++ " used in context"
      return $ q i


------------------------------------------------------------------------------------
-- ** 15.4 Identifiers
{-
   identifier ::= basic_identifier | extended_identifier
-}
identifier :: Parser Identifier
identifier =
  lexeme $
  antiQ
    AntiIdent
    (ExtendedIdent <$> extendedIdentifier <|>
     Ident <$> basicIdentifier <?> "identifier")

-- ** 15.4.2 Basic identifiers
{-
   basic_identifier ::= letter { [ underline ] letter_or_digit }

   letter_or_digit ::= letter | digit

   letter ::= upper_case_letter | lower_case_letter
-}

basicIdentifier :: Parser String
basicIdentifier = P.identifier lexer

-- ** 15.4.3 Extended identifiers
{-
   extended_identifier ::=
      \ graphic_character { graphic_character } \
-}
extendedIdentifier :: Parser String
extendedIdentifier =
  between
    (char '\\')
    (char '\\' <?> "end of extended identifier")
    (many1 (escapedBackslash <|> graphicalChar))
  where
    escapedBackslash = do
      _ <- try (symbol "\\\\")
      return '\\'

--------------------------------------------------------------------------------
-- ** 15.5 Abstract-literal
{-
    abstract_literal ::= decimal_literal | based_literal
-}
abstractLiteral :: Parser AbstractLiteral
abstractLiteral =
  (ALitDecimal <$> decimalLiteral) <|> (ALitBased <$> basedLiteral)

------------------------------------------------------------------------------------
-- ***15.5.2 Decimal literals
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
  optionMaybe exponent'

integer :: Parser Integer
integer =
  toInteger . (read :: String -> Int) . concat <$> number `sepBy1` symbol "_"
  where
    number = lexeme $ many1 digit

-- We do this rather convoluted thing to avoid interpreting x = 3 ELSE as the
-- beginning of an exponent
exponent' :: Parser Exponent
exponent' = try $ do
  c1 <- anyChar
  unless (c1 == 'E') (fail "")
  c2 <- lookAhead anyChar
  case c2 of
    '-' -> do
      _ <- anyChar
      ExponentNeg <$> integer
    '+' -> do
      _ <- anyChar
      ExponentPos <$> integer
    a ->
      if isDigit a
      then ExponentPos <$> integer
      else fail ""

--------------------------------------------------------------------------------
-- *** 15.5.3
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
  optionMaybe (symbol "#" *> exponent')

base :: Parser Integer
base = integer

-- TODO: Probably case sensitive
basedInteger :: Parser StringLiteral
basedInteger = SLit . concat <$> many1 alphaNum `sepBy1` char '_'

--------------------------------------------------------------------------------
-- *** 15.8
{-
bit_string_literal ::= [ integer ] base_specifier " [ bit_value ] "

bit_value ::= graphic_character { [ underline ] graphic_character }

base_specifier ::= B | O | X | UB | UO | UX | SB | SO | SX | D
-}
bitStringLiteral :: Parser BitStringLiteral
bitStringLiteral = --BitStringLiteral Nothing <$> try baseSpecifier <*> bitValue
                   BitStringLiteral <$> try (optionMaybe integer) <*> baseSpecifier <*> bitValue

-- FIXME: Should we filter out _'s?
bitValue :: Parser BitValue
bitValue = BitValue <$> (SLit <$> (filter ('_' /=) <$> stringLiteral'))

baseSpecifier :: Parser BaseSpecifier
baseSpecifier = choice $ map (\(l, s) -> symbol l *> pure s) specMap
  where
    specMap = specMap' ++  map (first (toLower <$>)) specMap'
    specMap' =
      [ ("B",  BinaryBase)
      , ("O",  OctalBase)
      , ("X",  HexBase)
      , ("UB", UnsignedBinaryBase)
      , ("UO", UnsignedOctalBase)
      , ("UX", UnsignedHexBase)
      , ("SB", SignedBinaryBase)
      , ("SO", SignedHoctalBase)
      , ("SX", SignedHexBase)
      , ("D",  Decimal)
      ]


-- The following is somewhat inspired by the language module of parsec

-- LRM08 15.7. Parses a string consisting of string segments and escape codes
-- separated by &
-- FIXME: Test escape codes
stringLiteral' :: Parser String
stringLiteral' = lexeme (strSegment >>= rest) <?> "String lit"
  where
    rest ctx =
      choice
        [ try (symbol "&" >> strSegment) >>= (\s -> rest (ctx ++ s))
        , try (symbol "&" >> asciiCode) >>= (\s -> rest (ctx ++ [s]))
        , pure ctx
        ]

-- Parses a segment between " and "
strSegment :: Parser String
strSegment =
  lexeme
    (between (char '"') (char '"' <?> "end of string") (many strChar) <?>
     "literal string")

-- "" in a string becomes literal "
strChar :: Parser Char
strChar = try $ (string "\"\"" *> pure '"') <|> char '\\' <|> graphicalChar

-- escape codes
asciiCode :: Parser Char
asciiCode = lexeme charAscii <?> "escape code"

-- Parses names of unprintable ASCII chars such as ACK
charAscii :: Parser Char
charAscii = choice (map parseAscii asciiMap)
  where
    parseAscii :: (String, Char) -> Parser Char
    parseAscii (asc, code) = try (string asc *> pure code)

-- LRM08 15.2. Values as defined by the CHARACTERS type in the STANDARD package
graphicalChar :: Parser Char
graphicalChar = choice (map char gchars) <?> "graphical character"
  where
    gchars =
      [ ' '
      , '!'
        -- intentionally removed: , '"'
      , '#'
      , '$'
      , '%'
      , '&'
      , '\''
      , '('
      , ')'
      , '*'
      , '+'
      , ','
      , '-'
      , '.'
      , '/'
      , '0'
      , '1'
      , '2'
      , '3'
      , '4'
      , '5'
      , '6'
      , '7'
      , '8'
      , '9'
      , ':'
      , ';'
      , '<'
      , '='
      , '>'
      , '?'
      , '@'
      , 'A'
      , 'B'
      , 'C'
      , 'D'
      , 'E'
      , 'F'
      , 'G'
      , 'H'
      , 'I'
      , 'J'
      , 'K'
      , 'L'
      , 'M'
      , 'N'
      , 'O'
      , 'P'
      , 'Q'
      , 'R'
      , 'S'
      , 'T'
      , 'U'
      , 'V'
      , 'W'
      , 'X'
      , 'Y'
      , 'Z'
      , '['
      -- , '\\'
      , ']'
      , '^'
      , '_'
      , '`'
      , 'a'
      , 'b'
      , 'c'
      , 'd'
      , 'e'
      , 'f'
      , 'g'
      , 'h'
      , 'i'
      , 'j'
      , 'k'
      , 'l'
      , 'm'
      , 'n'
      , 'o'
      , 'p'
      , 'q'
      , 'r'
      , 's'
      , 't'
      , 'u'
      , 'v'
      , 'w'
      , 'x'
      , 'y'
      , 'z'
      , '{'
      , '|'
      , '}'
      , '~'
      , ' '
      , '¡'
      , '¢'
      , '£'
      , '¤'
      , '¥'
      , '¦'
      , '§'
      , '¨'
      , '©'
      , 'ª'
      , '«'
      , '¬'
      , '\173' -- -
      , '®'
      , '¯'
      , '°'
      , '±'
      , '²'
      , '³'
      , '´'
      , 'µ'
      , '¶'
      , '·'
      , '¸'
      , '¹'
      , 'º'
      , '»'
      , '¼'
      , '½'
      , '¾'
      , '¿'
      , 'À'
      , 'Á'
      , 'Â'
      , 'Ã'
      , 'Ä'
      , 'Å'
      , 'Æ'
      , 'Ç'
      , 'È'
      , 'É'
      , 'Ê'
      , 'Ë'
      , 'Ì'
      , 'Í'
      , 'Î'
      , 'Ï'
      , 'Ð'
      , 'Ñ'
      , 'Ò'
      , 'Ó'
      , 'Ô'
      , 'Õ'
      , 'Ö'
      , '×'
      , 'Ø'
      , 'Ù'
      , 'Ú'
      , 'Û'
      , 'Ü'
      , 'Ý'
      , 'Þ'
      , 'ß'
      , 'à'
      , 'á'
      , 'â'
      , 'ã'
      , 'ä'
      , 'å'
      , 'æ'
      , 'ç'
      , 'è'
      , 'é'
      , 'ê'
      , 'ë'
      , 'ì'
      , 'í'
      , 'î'
      , 'ï'
      , 'ð'
      , 'ñ'
      , 'ò'
      , 'ó'
      , 'ô'
      , 'õ'
      , 'ö'
      , '÷'
      , 'ø'
      , 'ù'
      , 'ú'
      , 'û'
      , 'ü'
      , 'ý'
      , 'þ'
      , 'ÿ'
      ]

-- escape code tables
asciiMap :: [(String, Char)]
asciiMap = zip (asciiNames ++ ascii2Names) (asciiCodes ++ ascii2Codes)

asciiNames :: [String]
asciiNames =
  [ "NUL"
  , "SOH"
  , "STX"
  , "ETX"
  , "EOT"
  , "ENQ"
  , "ACK"
  , "BEL"
  , "BS"
  , "HT"
  , "LF"
  , "VT"
  , "FF"
  , "CR"
  , "SO"
  , "SI"
  , "DLE"
  , "DC1"
  , "DC2"
  , "DC3"
  , "DC4"
  , "NAK"
  , "SYN"
  , "ETB"
  , "CAN"
  , "EM"
  , "SUB"
  , "ESC"
  , "FSP"
  , "GSP"
  , "RSP"
  , "USP"
  , "DEL"
  ]

asciiCodes :: String
asciiCodes =
  [ '\NUL'
  , '\SOH'
  , '\STX'
  , '\ETX'
  , '\EOT'
  , '\ENQ'
  , '\ACK'
  , '\BEL'
  , '\BS'
  , '\HT'
  , '\LF'
  , '\VT'
  , '\FF'
  , '\CR'
  , '\SO'
  , '\SI'
  , '\DLE'
  , '\DC1'
  , '\DC2'
  , '\DC3'
  , '\DC4'
  , '\NAK'
  , '\SYN'
  , '\ETB'
  , '\CAN'
  , '\EM'
  , '\SUB'
  , '\ESC'
  , '\FS'
  , '\GS'
  , '\RS'
  , '\US'
  , '\DEL'
  ]

ascii2Names :: [String]
ascii2Names =
  [ "C128"
  , "C129"
  , "C130"
  , "C131"
  , "C132"
  , "C133"
  , "C134"
  , "C135"
  , "C136"
  , "C137"
  , "C138"
  , "C139"
  , "C140"
  , "C141"
  , "C142"
  , "C143"
  , "C144"
  , "C145"
  , "C146"
  , "C147"
  , "C148"
  , "C149"
  , "C150"
  , "C151"
  , "C152"
  , "C153"
  , "C154"
  , "C155"
  , "C156"
  , "C157"
  , "C158"
  , "C159"
  ]

ascii2Codes :: String
ascii2Codes =
  map
    chr
    [ 128
    , 129
    , 130
    , 131
    , 132
    , 133
    , 134
    , 135
    , 136
    , 137
    , 138
    , 139
    , 140
    , 141
    , 142
    , 143
    , 144
    , 145
    , 146
    , 147
    , 148
    , 149
    , 150
    , 151
    , 152
    , 153
    , 154
    , 155
    , 156
    , 157
    , 158
    , 159
    ]
