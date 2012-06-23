{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fspec-constr -fspec-constr-count=8 #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
#define USE_DEFAULT_SIGNATURES
#endif

#ifdef USE_DEFAULT_SIGNATURES
{-# LANGUAGE DefaultSignatures, TypeFamilies #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parser.Char
-- Copyright   :  (c) Edward Kmett 2011
--                (c) Daan Leijen 1999-2001
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parsers for character streams
--
-----------------------------------------------------------------------------
module Text.Parser.Char
  ( CharParsing(..)
  -- * Character parsers
  , oneOf       -- :: CharParsing m => [Char] -> m Char
  , noneOf      -- :: CharParsing m => [Char] -> m Char
  , oneOfSet    -- :: CharParsing m => CharSet -> m Char
  , noneOfSet   -- :: CharParsing m => CharSet -> m Char
  , spaces      -- :: CharParsing m => m ()
  , space       -- :: CharParsing m => m Char
  , newline     -- :: CharParsing m => m Char
  , tab         -- :: CharParsing m => m Char
  , upper       -- :: CharParsing m => m Char
  , lower       -- :: CharParsing m => m Char
  , alphaNum    -- :: CharParsing m => m Char
  , letter      -- :: CharParsing m => m Char
  , digit       -- :: CharParsing m => m Char
  , hexDigit    -- :: CharParsing m => m Char
  , octDigit    -- :: CharParsing m => m Char
  , decimal     -- :: CharParsing m => m Integer
  , hexadecimal -- :: CharParsing m => m Integer
  , octal       -- :: CharParsing m => m Integer
  -- ** Internal parsers
  , charLiteral'
  , characterChar
  , stringLiteral'
  , natural'
  , integer'
  , double'
  , naturalOrDouble'
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Data.Char
import Data.CharSet (CharSet(..))
import qualified Data.CharSet as CharSet
import Data.Foldable
import qualified Data.IntSet as IntSet
import Data.Monoid
import Text.Parser.Combinators

-- | @oneOf cs@ succeeds if the current character is in the supplied
-- list of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
--
-- >   vowel  = oneOf "aeiou"
oneOf :: CharParsing m => [Char] -> m Char
oneOf xs = oneOfSet (CharSet.fromList xs)
{-# INLINE oneOf #-}

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"
noneOf :: CharParsing m => [Char] -> m Char
noneOf xs = noneOfSet (CharSet.fromList xs)
{-# INLINE noneOf #-}

-- | @oneOfSet cs@ succeeds if the current character is in the supplied
-- set of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
--
-- >   vowel  = oneOf "aeiou"
oneOfSet :: CharParsing m => CharSet -> m Char
oneOfSet (CharSet True _ is)  = satisfy (\c -> IntSet.member (fromEnum c) is)
oneOfSet (CharSet False _ is) = satisfy (\c -> not (IntSet.member (fromEnum c) is))
{-# INLINE oneOfSet #-}

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"
noneOfSet :: CharParsing m => CharSet -> m Char
noneOfSet s = oneOfSet (CharSet.complement s)
{-# INLINE noneOfSet #-}

-- | Skips /zero/ or more white space characters. See also 'skipMany' and
-- 'whiteSpace'.
spaces :: CharParsing m => m ()
spaces = skipMany space <?> "white space"

-- | Parses a white space character (any character which satisfies 'isSpace')
-- Returns the parsed character.
space :: CharParsing m => m Char
space = satisfy isSpace <?> "space"
{-# INLINE space #-}

-- | Parses a newline character (\'\\n\'). Returns a newline character.
newline :: CharParsing m => m Char
newline = char '\n' <?> "new-line"
{-# INLINE newline #-}

-- | Parses a tab character (\'\\t\'). Returns a tab character.
tab :: CharParsing m => m Char
tab = char '\t' <?> "tab"
{-# INLINE tab #-}

-- | Parses an upper case letter (a character between \'A\' and \'Z\').
-- Returns the parsed character.
upper :: CharParsing m => m Char
upper = satisfy isUpper <?> "uppercase letter"
{-# INLINE upper #-}

-- | Parses a lower case character (a character between \'a\' and \'z\').
-- Returns the parsed character.
lower :: CharParsing m => m Char
lower = satisfy isLower <?> "lowercase letter"
{-# INLINE lower #-}

-- | Parses a letter or digit (a character between \'0\' and \'9\').
-- Returns the parsed character.

alphaNum :: CharParsing m => m Char
alphaNum = satisfy isAlphaNum <?> "letter or digit"
{-# INLINE alphaNum #-}

-- | Parses a letter (an upper case or lower case character). Returns the
-- parsed character.

letter :: CharParsing m => m Char
letter = satisfy isAlpha <?> "letter"
{-# INLINE letter #-}

-- | Parses a digit. Returns the parsed character.

digit :: CharParsing m => m Char
digit = satisfy isDigit <?> "digit"
{-# INLINE digit #-}

-- | Parses a hexadecimal digit (a digit or a letter between \'a\' and
-- \'f\' or \'A\' and \'F\'). Returns the parsed character.
hexDigit :: CharParsing m => m Char
hexDigit = satisfy isHexDigit <?> "hexadecimal digit"
{-# INLINE hexDigit #-}

-- | Parses an octal digit (a character between \'0\' and \'7\'). Returns
-- the parsed character.
octDigit :: CharParsing m => m Char
octDigit = satisfy isOctDigit <?> "octal digit"
{-# INLINE octDigit #-}

class Parsing m => CharParsing m where
  -- | Parse a single character of the input, with UTF-8 decoding
  satisfy :: (Char -> Bool) -> m Char
#ifdef USE_DEFAULT_SIGNATURES
  default satisfy :: (MonadTrans t, CharParsing n, m ~ t n) =>
                     (Char -> Bool) ->
                     t n Char
  satisfy = lift . satisfy
#endif
  -- | @char c@ parses a single character @c@. Returns the parsed
  -- character (i.e. @c@).
  --
  -- >  semiColon  = char ';'
  char :: CharParsing m => Char -> m Char
  char c = satisfy (c ==) <?> show [c]

  -- | @notChar c@ parses any single character other than @c@. Returns the parsed
  -- character.
  --
  -- >  semiColon  = char ';'
  notChar :: CharParsing m => Char -> m Char
  notChar c = satisfy (c /=)

  -- | This parser succeeds for any character. Returns the parsed character.
  anyChar :: CharParsing m => m Char
  anyChar = satisfy (const True)

  -- | @string s@ parses a sequence of characters given by @s@. Returns
  -- the parsed string (i.e. @s@).
  --
  -- >  divOrMod    =   string "div"
  -- >              <|> string "mod"
  string :: CharParsing m => String -> m String
  string s = s <$ try (traverse_ char s) <?> show s


instance CharParsing m => CharParsing (Lazy.StateT s m) where
  satisfy = lift . satisfy
  char    = lift . char
  notChar = lift . notChar
  anyChar = lift anyChar
  string  = lift . string

instance CharParsing m => CharParsing (Strict.StateT s m) where
  satisfy = lift . satisfy
  char    = lift . char
  notChar = lift . notChar
  anyChar = lift anyChar
  string  = lift . string

instance CharParsing m => CharParsing (ReaderT e m) where
  satisfy = lift . satisfy
  char    = lift . char
  notChar = lift . notChar
  anyChar = lift anyChar
  string  = lift . string

instance (CharParsing m, Monoid w) => CharParsing (Strict.WriterT w m) where
  satisfy = lift . satisfy
  char    = lift . char
  notChar = lift . notChar
  anyChar = lift anyChar
  string  = lift . string

instance (CharParsing m, Monoid w) => CharParsing (Lazy.WriterT w m) where
  satisfy = lift . satisfy
  char    = lift . char
  notChar = lift . notChar
  anyChar = lift anyChar
  string  = lift . string

instance (CharParsing m, Monoid w) => CharParsing (Lazy.RWST r w s m) where
  satisfy = lift . satisfy
  char    = lift . char
  notChar = lift . notChar
  anyChar = lift anyChar
  string  = lift . string

instance (CharParsing m, Monoid w) => CharParsing (Strict.RWST r w s m) where
  satisfy = lift . satisfy
  char    = lift . char
  notChar = lift . notChar
  anyChar = lift anyChar
  string  = lift . string

instance CharParsing m => CharParsing (IdentityT m) where
  satisfy = lift . satisfy
  char    = lift . char
  notChar = lift . notChar
  anyChar = lift anyChar
  string  = lift . string


-- | This parser parses a single literal character. Returns the
-- literal character value. This parsers deals correctly with escape
-- sequences. The literal character is parsed according to the grammar
-- rules defined in the Haskell report (which matches most programming
-- languages quite closely).
--
-- This parser does NOT swallow trailing whitespace.
charLiteral' :: CharParsing m => m Char
charLiteral' = between (char '\'') (char '\'' <?> "end of character") characterChar
          <?> "character"

characterChar, charEscape, charLetter :: CharParsing m => m Char
characterChar = charLetter <|> charEscape
            <?> "literal character"
charEscape = char '\\' *> escapeCode
charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

-- | This parser parses a literal string. Returns the literal
-- string value. This parsers deals correctly with escape sequences and
-- gaps. The literal string is parsed according to the grammar rules
-- defined in the Haskell report (which matches most programming
-- languages quite closely).
--
-- This parser does NOT swallow trailing whitespace
stringLiteral' :: CharParsing m => m String
stringLiteral' = Prelude.foldr (maybe id (:)) "" <$>
  between (char '"') (char '"' <?> "end of string") (many stringChar) <?> 
  "literal string" where
  stringChar = Just <$> stringLetter
           <|> stringEscape
       <?> "string character"
  stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

  stringEscape = char '\\' *> esc where
    esc = Nothing <$ escapeGap
      <|> Nothing <$ escapeEmpty
      <|> Just <$> escapeCode
  escapeEmpty = char '&'
  escapeGap = do skipSome space
                 char '\\' <?> "end of string gap"

escapeCode :: CharParsing m => m Char
escapeCode = (charEsc <|> charNum <|> charAscii <|> charControl) <?> "escape code"
  where
  charControl = (\c -> toEnum (fromEnum c - fromEnum 'A')) <$> (char '^' *> upper)
  charNum     = toEnum . fromInteger <$> num where
    num = decimal
      <|> (char 'o' *> number 8 octDigit)
      <|> (char 'x' *> number 16 hexDigit)
  charEsc = choice $ parseEsc <$> escMap
  parseEsc (c,code) = code <$ char c
  escMap = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
  charAscii = choice $ parseAscii <$> asciiMap
  parseAscii (asc,code) = try $ code <$ string asc
  asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
  ascii2codes, ascii3codes :: [String]
  ascii2codes = [ "BS","HT","LF","VT","FF","CR","SO"
                , "SI","EM","FS","GS","RS","US","SP"]
  ascii3codes = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK"
                ,"BEL","DLE","DC1","DC2","DC3","DC4","NAK"
                ,"SYN","ETB","CAN","SUB","ESC","DEL"]
  ascii2, ascii3 :: [Char]
  ascii2 = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI'
           ,'\EM','\FS','\GS','\RS','\US','\SP']
  ascii3 = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK'
           ,'\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK'
           ,'\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']

-- | This parser parses a natural number (a positive whole
-- number). Returns the value of the number. The number can be
-- specified in 'decimal', 'hexadecimal' or
-- 'octal'. The number is parsed according to the grammar
-- rules in the Haskell report.
--
-- This parser does NOT swallow trailing whitespace.
natural' :: CharParsing m => m Integer
natural' = nat <?> "natural"

number :: CharParsing m => Integer -> m Char -> m Integer
number base baseDigit = do
  digits <- some baseDigit
  return $! foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 digits

-- | This parser parses an integer (a whole number). This parser
-- is like 'natural' except that it can be prefixed with
-- sign (i.e. \'-\' or \'+\'). Returns the value of the number. The
-- number can be specified in 'decimal', 'hexadecimal'
-- or 'octal'. The number is parsed according
-- to the grammar rules in the Haskell report.
--
-- This parser does NOT swallow trailing whitespace.
--
-- Also, unlike the 'integer' parser, this parser does not admit spaces
-- between the sign and the number.

integer' :: CharParsing m => m Integer
integer' = int <?> "integer"

sign :: CharParsing m => m (Integer -> Integer)
sign = negate <$ char '-'
   <|> id <$ char '+'
   <|> pure id

int :: CharParsing m => m Integer
int = {-lexeme-} sign <*> nat
nat, zeroNumber :: CharParsing m => m Integer
nat = zeroNumber <|> decimal
zeroNumber = char '0' *> (hexadecimal <|> octal <|> decimal <|> return 0) <?> ""

-- | This parser parses a floating point value. Returns the value
-- of the number. The number is parsed according to the grammar rules
-- defined in the Haskell report.
--
-- This parser does NOT swallow trailing whitespace.

double' :: CharParsing m => m Double
double' = floating <?> "double"

floating :: CharParsing m => m Double
floating = decimal >>= fractExponent

fractExponent :: CharParsing m => Integer -> m Double
fractExponent n = (\fract expo -> (fromInteger n + fract) * expo) <$> fraction <*> option 1.0 exponent'
              <|> (fromInteger n *) <$> exponent' where
  fraction = Prelude.foldr op 0.0 <$> (char '.' *> (some digit <?> "fraction"))
  op d f = (f + fromIntegral (digitToInt d))/10.0
  exponent' = do
       _ <- oneOf "eE"
       f <- sign
       e <- decimal <?> "exponent"
       return (power (f e))
    <?> "exponent"
  power e
    | e < 0     = 1.0/power(-e)
    | otherwise = fromInteger (10^e)


-- | This parser parses either 'natural' or a 'double'.
-- Returns the value of the number. This parsers deals with
-- any overlap in the grammar rules for naturals and floats. The number
-- is parsed according to the grammar rules defined in the Haskell report.
--
-- This parser does NOT swallow trailing whitespace.

naturalOrDouble' :: CharParsing m => m (Either Integer Double)
naturalOrDouble' = natDouble <?> "number"

natDouble, zeroNumFloat, decimalFloat :: CharParsing m => m (Either Integer Double)
natDouble
    = char '0' *> zeroNumFloat
  <|> decimalFloat
zeroNumFloat
    = Left <$> (hexadecimal <|> octal)
  <|> decimalFloat
  <|> fractFloat 0
  <|> return (Left 0)
decimalFloat = do
  n <- decimal
  option (Left n) (fractFloat n)

fractFloat :: CharParsing m => Integer -> m (Either Integer Double)
fractFloat n = Right <$> fractExponent n

-- | Parses a positive whole number in the decimal system. Returns the
-- value of the number.

decimal :: CharParsing m => m Integer
decimal = number 10 digit

-- | Parses a positive whole number in the hexadecimal system. The number
-- should be prefixed with \"x\" or \"X\". Returns the value of the
-- number.

hexadecimal :: CharParsing m => m Integer
hexadecimal = oneOf "xX" *> number 16 hexDigit

-- | Parses a positive whole number in the octal system. The number
-- should be prefixed with \"o\" or \"O\". Returns the value of the
-- number.

octal :: CharParsing m => m Integer
octal = oneOf "oO" *> number 8 octDigit

