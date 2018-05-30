{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.VHDL.Lexer
  ( abstractLiteral
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
  , octal
  , parens
  , reserved
  , semi
  , semiSep
  , semiSep1
  , stringLiteral
  , symbol
  , spaceConsumer
  , basedLiteral
  , stringDelimiter
  ) where

import           Control.Arrow              (first)
import           Control.Monad              (unless, when)
import           Data.Char                  (chr, digitToInt, isDigit)
import           Data.Data                  (Data)
import qualified Data.HashSet               as S
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Language.VHDL.Parser.Monad (Parser, quotesEnabled)
import           Language.VHDL.Parser.Util
import           Language.VHDL.Syntax
import           Numeric                    (readInt)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

vhdlReserved :: S.HashSet Text
vhdlReserved = S.fromList
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

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") empty
{-# INLINEABLE spaceConsumer #-}

reserved' :: S.HashSet T.Text -> T.Text -> Parser T.Text
reserved' t w =
  lexeme $
  try $ do
    r <- string' w <* notFollowedBy identChar
    unless (T.toLower r `S.member` t) $
      fail (T.unpack r <> " is not a reserved word")
    return r
{-# INLINEABLE reserved' #-}

reserved :: T.Text -> Parser T.Text
reserved = reserved' vhdlReserved
{-# INLINEABLE reserved #-}

charLiteral :: Parser CharacterLiteral
charLiteral =
  antiQ AntiClit $
  CLit <$>
  lexeme (char '\'' *> (char '"' <|> char '\\' <|> graphicalChar) <* char '\'')
{-# INLINEABLE charLiteral #-}

stringLiteral :: Parser StringLiteral
stringLiteral = antiQ AntiSlit $ SLit <$> stringLiteral'
{-# INLINEABLE stringLiteral #-}

decimal, hexadecimal, octal :: Parser Integer
hexadecimal = try (string' "0x") >> L.hexadecimal
octal = try (string' "0o") >> L.octal
decimal = L.decimal
{-# INLINEABLE decimal #-}
{-# INLINEABLE octal #-}
{-# INLINEABLE hexadecimal #-}

float :: Parser Double
float = lexeme $ L.signed spaceConsumer L.float
{-# INLINEABLE float #-}

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer
{-# INLINE lexeme #-}

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer
{-# INLINEABLE symbol #-}

parens, braces, brackets :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
brackets  = between (symbol "[") (symbol "]")
{-# INLINEABLE parens #-}
{-# INLINEABLE braces #-}
{-# INLINEABLE brackets #-}

semi, comma, colon, dot :: Parser T.Text
semi  = symbol ";"
comma = symbol ","
colon = symbol ":" <* notFollowedBy (symbol "=")
dot   = symbol "."
{-# INLINEABLE semi #-}
{-# INLINEABLE comma #-}
{-# INLINEABLE colon #-}
{-# INLINEABLE dot #-}

semiSep, semiSep1, commaSep, commaSep1 :: Parser a -> Parser [a]
semiSep = flip sepBy semi
semiSep1 = flip sepBy1 semi
commaSep = flip sepBy comma
commaSep1 = flip sepBy1 comma
{-# INLINEABLE semiSep #-}
{-# INLINEABLE commaSep #-}
{-# INLINEABLE semiSep1 #-}
{-# INLINEABLE commaSep1 #-}


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
      case c of
        '"' -> do
          s <- hsString
          cs <- rest nest
          return ((c : s) ++ cs)
        '(' -> do
          cs <- rest (nest + 1)
          return (c : cs)
        ')' -> do
          cs <- rest (nest - 1)
          return (c : cs)
        _ -> do
          cs <- rest nest
          return (c : cs)
    hsString = do
      c <- anyChar
      case c of
        '\\'
          -- FIXME: is this sufficient?
         -> do
          c2 <- anyChar
          cs <- hsString
          return (c : c2 : cs)
        '"' -> return [c]
        _ -> do
          cs <- hsString
          return (c : cs)

antiQ
  :: (Data a)
  => (String -> a) -> Parser a -> Parser a
antiQ q p = try (lexeme parseQ) <|> p
  where
    parseQ = do
      _ <- char '$'
      let qn = toQQString $ q ""
      qs <- T.unpack <$> string (T.pack qn)
      _ <- char ':'
      identOrExpr <- optional $ lookAhead (char '(')
      i <-
        case identOrExpr of
          Just _ -> parseAntiExpr
          Nothing
            -- FIXME: Using VHDL reserved words are fine here
           ->
            identifier >>= \case
              (Ident e) -> pure (T.unpack e)
              (ExtendedIdent e) -> pure (T.unpack e)
              (AntiIdent e) -> pure e
      qe <- quotesEnabled
      unless qe $ fail "QuasiQuotation syntax not emabled"
      unless (qs == qn) $
        fail $ "Wrong QuasiQuoter " <> qn <> " used in context"
      return $ q i
{-# INLINEABLE antiQ #-}


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
{-# INLINE identifier #-}

-- ** 15.4.2 Basic identifiers
{-
   basic_identifier ::= letter { [ underline ] letter_or_digit }

   letter_or_digit ::= letter | digit

   letter ::= upper_case_letter | lower_case_letter
-}

identChar :: Parser Char
identChar = alphaNumChar <|> char '_'
{-# INLINE identChar #-}

basicIdentifier :: Parser Text
basicIdentifier =
  lexeme $ try $ do
    i <- part <|> string "_" <* notFollowedBy part
    when (T.toLower i `S.member` vhdlReserved) $
      fail $ "Keyword " ++ T.unpack i ++ " used as identifier"
    return i
  where
    part = T.pack <$> ((:) <$> letterChar <*> many identChar)
{-# INLINEABLE basicIdentifier #-}

-- ** 15.4.3 Extended identifiers
{-
   extended_identifier ::=
      \ graphic_character { graphic_character } \
-}
extendedIdentifier :: Parser Text
extendedIdentifier =
  T.pack <$> between
    (char '\\')
    (char '\\' <?> "end of extended identifier")
    (some (escapedBackslash <|> graphicalChar))
  where
    escapedBackslash = do
      _ <- try (symbol "\\\\")
      return '\\'
{-# INLINEABLE extendedIdentifier #-}

--------------------------------------------------------------------------------
-- ** 15.5 Abstract-literal
{-
    abstract_literal ::= decimal_literal | based_literal
-}
abstractLiteral :: Parser AbstractLiteral
abstractLiteral =
  try (ALitBased <$> basedLiteral) <|> (ALitDecimal <$> decimalLiteral)
{-# INLINEABLE abstractLiteral #-}

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
  DecimalLiteral <$> integer <*> optional (dot *> number) <*> optional exponent'
{-# INLINEABLE decimalLiteral #-}

integer :: Parser Integer
integer =
  toInteger . (read :: String -> Integer) <$> number
{-# INLINEABLE integer #-}

number, hexNumber :: Parser String
number = number' digitChar
hexNumber = number' hexDigitChar
{-# INLINEABLE number #-}
{-# INLINEABLE hexNumber #-}

number' :: Parser Char -> Parser String
number' p = lexeme $ concat <$> some p `sepBy1` symbol "_"
{-# INLINEABLE number' #-}

-- We do this rather convoluted thing to avoid interpreting x = 3 ELSE as the
-- beginning of an exponent
exponent' :: Parser Exponent
exponent' = try $ do
  _ <- char' 'e'
  lookAhead anyChar >>= \case
    '-' -> do
      _ <- anyChar
      ExponentNeg <$> integer
    '+' -> do
      _ <- anyChar
      ExponentPos <$> integer
    a ->
      if isDigit a
      then ExponentPos <$> integer
      else fail "Exponent not followed by digit"
{-# INLINEABLE exponent' #-}

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
-- TODO: VHDL allows for obscure character substitutions (# -> :). Consider
-- dropping this
basedLiteral :: Parser BasedLiteral
basedLiteral =
  let sepSym = symbol "#" <|> symbol ":"
  in do b <- base <* sepSym
        unless (2 <= b && b <= 16) (fail "Base must be between 2 and 16")
        bi1 <- basedInteger b
        bi2 <- optional (dot *> hexNumber)
        _ <- sepSym
        e <- optional exponent'
        return $ BasedLiteral b bi1 bi2 e
{-# INLINEABLE basedLiteral #-}

base :: Parser Integer
base = integer

-- TODO: Probably case sensitive
basedInteger :: Integer -> Parser BasedInteger
basedInteger b =
  let b' = fromIntegral b
  in fromIntegral .
     fst . head . readInt b' ((< b') . digitToInt) digitToInt <$> hexNumber
{-# INLINEABLE basedInteger #-}

--------------------------------------------------------------------------------
-- *** 15.8
{-
bit_string_literal ::= [ integer ] base_specifier " [ bit_value ] "

bit_value ::= graphic_character { [ underline ] graphic_character }

base_specifier ::= B | O | X | UB | UO | UX | SB | SO | SX | D
-}
bitStringLiteral :: Parser BitStringLiteral
bitStringLiteral =
  BitStringLiteral <$> try (optional integer) <*> baseSpecifier <*> bitValue

-- FIXME: Should we filter out _'s?
bitValue :: Parser BitValue
bitValue = BitValue <$> (SLit <$> (T.filter ('_' /=) <$> stringLiteral'))

baseSpecifier :: Parser BaseSpecifier
baseSpecifier = choice $ map (\(l, s) -> symbol l *> pure s) specMap
  where
    specMap = specMap' ++ map (first T.toLower) specMap'
    specMap' =
      [ ("B", BinaryBase)
      , ("O", OctalBase)
      , ("X", HexBase)
      , ("UB", UnsignedBinaryBase)
      , ("UO", UnsignedOctalBase)
      , ("UX", UnsignedHexBase)
      , ("SB", SignedBinaryBase)
      , ("SO", SignedHoctalBase)
      , ("SX", SignedHexBase)
      , ("D", Decimal)
      ]


-- The following is somewhat inspired by the language module of parsec

-- LRM08 15.7. Parses a string consisting of string segments and escape codes
-- separated by &
-- FIXME: Test escape codes
stringLiteral' :: Parser Text
stringLiteral' = lexeme (strSegment >>= rest) <?> "String lit"
  where
    rest ctx =
      choice
        [ try (symbol "&" >> strSegment) >>= (\s -> rest (ctx <> s))
        , try (symbol "&" >> asciiCode) >>= (\s -> rest (ctx <> T.pack [s]))
        , pure ctx
        ]
{-# INLINEABLE stringLiteral' #-}

stringDelimiter :: Parser Text
stringDelimiter = symbol "\"" <|> symbol "%"
{-# INLINEABLE stringDelimiter #-}

-- Parses a segment between " and " or % and % (obscure VHDL char replacement)
strSegment :: Parser Text
strSegment = strSegment' '%' <|> strSegment' '"'
{-# INLINEABLE strSegment #-}

strSegment' :: Char -> Parser Text
strSegment' delim =
  T.pack <$>
  lexeme
    (between
       (char delim)
       (char delim <?> "end of string")
       (many (strChar delim)) <?>
     "literal string")
{-# INLINEABLE strSegment' #-}

-- "" in a string becomes literal " if string is between ""
-- %% becomes % if string is between %%
strChar :: Char -> Parser Char
strChar delim = try $ (char delim >> char delim) <|> char '\\' <|> graphicalChar
{-# INLINEABLE strChar #-}

-- escape codes
asciiCode :: Parser Char
asciiCode = lexeme charAscii <?> "escape code"
{-# INLINEABLE asciiCode #-}

-- Parses names of unprintable ASCII chars such as ACK

-- FIXME: This might be slow
charAscii :: Parser Char
charAscii = choice (map parseAscii asciiMap)
  where
    parseAscii :: (Text, Char) -> Parser Char
    parseAscii (asc, code) = try (string asc *> pure code)
{-# INLINEABLE charAscii #-}

-- LRM08 15.2. Values as defined by the CHARACTERS type in the STANDARD package
graphicalChar :: Parser Char
graphicalChar = oneOf gchars <?> "graphical character"
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
{-# INLINE graphicalChar #-}

-- escape code tables
asciiMap :: [(Text, Char)]
asciiMap = zip (asciiNames ++ ascii2Names) (asciiCodes ++ ascii2Codes)

asciiNames :: [Text]
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

ascii2Names :: [Text]
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
