{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.VHDL.Lexer
  ( abstractLiteral
  , angles
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

import           Control.Arrow
import           Data.Char                 (chr, toLower)
import           Language.VHDL.Parser.Util
import           Language.VHDL.Syntax
import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token         as P

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

lexer = P.makeTokenParser vhdlDef
reserved = P.reserved lexer
operator = P.operator lexer
reservedOp = P.reservedOp lexer
charLiteral =
  antiQ' identifier AntiClit $
  CLit <$> lexeme (char '\'' *> (char '"' <|> graphicalChar) <* char '\'')
stringLiteral = antiQ' identifier AntiSlit $ SLit <$> stringLiteral'
natural = P.natural lexer
float = P.float lexer
naturalOrFloat = P.naturalOrFloat lexer
decimal = P.decimal lexer
hexadecimal = P.hexadecimal lexer
octal = P.octal lexer
symbol = P.symbol lexer
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
parens = P.parens lexer
braces = P.braces lexer
angles = P.angles lexer
brackets = P.brackets lexer
squares = P.squares lexer
semi = P.semi lexer
comma = P.comma lexer
colon = P.colon lexer
dot = P.dot lexer
semiSep = P.semiSep lexer
semiSep1 = P.semiSep1 lexer
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer

------------------------------------------------------------------------------------
-- ** 15.4 Identifiers
{-
   identifier ::= basic_identifier | extended_identifier
-}
identifier =
  antiQ'
    identifier
    AntiIdent
    (ExtendedIdent <$> extendedIdentifier <|>
     Ident <$> basicIdentifier <?> "identifier")

-- ** 15.4.2 Basic identifiers
{-
   basic_identifier ::= letter { [ underline ] letter_or_digit }

   letter_or_digit ::= letter | digit

   letter ::= upper_case_letter | lower_case_letter
-}

basicIdentifier = P.identifier lexer

-- ** 15.4.3 Extended identifiers
{-
   extended_identifier ::=
      \ graphic_character { graphic_character } \
-}
extendedIdentifier =
  lexeme $
  between
    (char '\\')
    (char '\\' <?> "end of extended identifier")
    (many1 graphicalChar)

--------------------------------------------------------------------------------
-- ** 15.5 AbstractLiteral
{-
    abstract_literal ::= decimal_literal | based_literal
-}
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
decimalLiteral =
  DecimalLiteral <$> integer <*> optionMaybe (dot *> integer) <*>
  optionMaybe exponent'

integer =
  toInteger <$> (read :: String -> Int) . concat <$> number `sepBy1` symbol "_"
  where
    number = lexeme $ many1 digit

exponent' =
  symbol "E" >>
  ((ExponentNeg <$> (symbol "-" *> integer)) <|>
   (ExponentPos <$> (optional (symbol "+") *> integer)))

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
basedLiteral =
  BasedLiteral <$> base <*> (symbol "#" *> basedInteger) <*>
  (dot *> optionMaybe basedInteger) <*>
  optionMaybe (symbol "#" *> exponent')

base = integer

-- TODO: Probably case sensitive
basedInteger = SLit . concat <$> many1 alphaNum `sepBy1` char '_'

--------------------------------------------------------------------------------
-- *** 15.8
{-
bit_string_literal ::= [ integer ] base_specifier " [ bit_value ] "

bit_value ::= graphic_character { [ underline ] graphic_character }

base_specifier ::= B | O | X | UB | UO | UX | SB | SO | SX | D
-}
bitStringLiteral = --BitStringLiteral Nothing <$> try baseSpecifier <*> bitValue
                   BitStringLiteral <$> try (optionMaybe integer) <*> baseSpecifier <*> bitValue

-- FIXME: Should we filter out _'s?
bitValue = BitValue <$> (SLit <$> (filter ('_' /=) <$> stringLiteral'))

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
stringLiteral' = lexeme (strSegment >>= rest) <?> "String lit"
  where
    rest ctx =
      choice
        [ try (symbol "&" >> strSegment) >>= (\s -> rest (ctx ++ s))
        , try (symbol "&" >> asciiCode) >>= (\s -> rest (ctx ++ [s]))
        , pure ctx
        ]

-- Parses a segment between " and "
strSegment =
  lexeme
    (between (char '"') (char '"' <?> "end of string") (many strChar) <?>
     "literal string")

-- "" in a string becomes literal "
strChar = try $ (string "\"\"" *> pure '"') <|> graphicalChar

-- escape codes
asciiCode = lexeme charAscii <?> "escape code"

-- Parses names of unprintable ASCII chars such as ACK
charAscii = choice (map parseAscii asciiMap)
  where
    parseAscii (asc, code) = try (string asc *> pure code)

-- LRM08 15.2. Values as defined by the CHARACTERS type in the STANDARD package
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
      , '\''
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
asciiMap = zip (asciiNames ++ ascii2Names) (asciiCodes ++ ascii2Codes)

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
