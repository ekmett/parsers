{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fspec-constr -fspec-constr-count=8 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parser.Token
-- Copyright   :  (c) Edward Kmett 2011
--                (c) Daan Leijen 1999-2001
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parsers that comprehend whitespace and identifier styles
--
-- > idStyle    = haskellIdentifierStyle { styleReserved = ... }
-- > identifier = ident haskellIdentifierStyle
-- > reserved   = reserve haskellIdentifierStyle
--
-----------------------------------------------------------------------------
module Text.Parser.Token
  (
  -- * Token Parsing
    whiteSpace      -- :: TokenParsing m => m ()
  , token           -- :: TokenParsing m => m a -> m a
  , charLiteral     -- :: TokenParsing m => m Char
  , stringLiteral   -- :: TokenParsing m => m String
  , natural         -- :: TokenParsing m => m Integer
  , integer         -- :: TokenParsing m => m Integer
  , double          -- :: TokenParsing m => m Double
  , naturalOrDouble -- :: TokenParsing m => m (Either Integer Double)
  , symbol          -- :: TokenParsing m => String -> m String
  , symbolic        -- :: TokenParsing m => Char -> m Char
  , parens          -- :: TokenParsing m => m a -> m a
  , braces          -- :: TokenParsing m => m a -> m a
  , angles          -- :: TokenParsing m => m a -> m a
  , brackets        -- :: TokenParsing m => m a -> m a
  , comma           -- :: TokenParsing m => m Char
  , colon           -- :: TokenParsing m => m Char
  , dot             -- :: TokenParsing m => m Char
  , semiSep         -- :: TokenParsing m => m a -> m [a]
  , semiSep1        -- :: TokenParsing m => m a -> m [a]
  , commaSep        -- :: TokenParsing m => m a -> m [a]
  , commaSep1       -- :: TokenParsing m => m a -> m [a]
  -- ** Token Parsing Class
  , TokenParsing(..)
  -- ** Token Parsing Transformers
  , Unspaced(..)
  , Unhighlighted(..)
  -- ** /Non-Token/ Parsers
  , decimal       -- :: TokenParsing m => m Integer
  , hexadecimal   -- :: TokenParsing m => m Integer
  , octal         -- :: TokenParsing m => m Integer
  , characterChar -- :: TokenParsing m => m Char
  , integer'      -- :: TokenParsing m => m Integer
  -- * Identifiers
  , IdentifierStyle(..)
  , liftIdentifierStyle -- :: (MonadTrans t, Monad m) =>
                        --    IdentifierStyle m -> IdentifierStyle (t m)
  , ident           -- :: TokenParsing m => IdentifierStyle m -> m String
  , reserve         -- :: TokenParsing m => IdentifierStyle m -> String -> m ()
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(..), when)
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
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.List (foldl')
import Data.Monoid
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token.Highlight

-- | Skip zero or more bytes worth of white space. More complex parsers are
-- free to consider comments as white space.
whiteSpace :: TokenParsing m => m ()
whiteSpace = someSpace <|> pure ()
{-# INLINE whiteSpace #-}

-- | @token p@ first applies parser @p@ and then the 'whiteSpace'
-- parser, returning the value of @p@. Every lexical
-- token (token) is defined using @token@, this way every parse
-- starts at a point without white space. Parsers that use @token@ are
-- called /token/ parsers in this document.
--
-- The only point where the 'whiteSpace' parser should be
-- called explicitly is the start of the main parser in order to skip
-- any leading white space.
--
-- > mainParser  = sum <$ whiteSpace <*> many (token digit) <* eof
token :: TokenParsing m => m a -> m a
token p = p <* whiteSpace
{-# INLINE token #-}

-- | This token parser parses a single literal character. Returns the
-- literal character value. This parsers deals correctly with escape
-- sequences. The literal character is parsed according to the grammar
-- rules defined in the Haskell report (which matches most programming
-- languages quite closely).
charLiteral :: TokenParsing m => m Char
charLiteral = token (highlight CharLiteral lit) where
  lit = between (char '\'') (char '\'' <?> "end of character") characterChar
    <?> "character"
{-# INLINE charLiteral #-}

-- | This token parser parses a literal string. Returns the literal
-- string value. This parsers deals correctly with escape sequences and
-- gaps. The literal string is parsed according to the grammar rules
-- defined in the Haskell report (which matches most programming
-- languages quite closely).
stringLiteral :: TokenParsing m => m String
stringLiteral = token (highlight StringLiteral lit) where
  lit = Prelude.foldr (maybe id (:)) ""
    <$> between (char '"') (char '"' <?> "end of string") (many stringChar)
    <?> "string"
  stringChar = Just <$> stringLetter
           <|> stringEscape
       <?> "string character"
  stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

  stringEscape = highlight EscapeCode $ char '\\' *> esc where
    esc = Nothing <$ escapeGap
      <|> Nothing <$ escapeEmpty
      <|> Just <$> escapeCode
  escapeEmpty = char '&'
  escapeGap = skipSome space *> (char '\\' <?> "end of string gap")
{-# INLINE stringLiteral #-}

-- | This token parser parses a natural number (a positive whole
-- number). Returns the value of the number. The number can be
-- specified in 'decimal', 'hexadecimal' or
-- 'octal'. The number is parsed according to the grammar
-- rules in the Haskell report.
natural :: TokenParsing m => m Integer
natural = token natural'
{-# INLINE natural #-}

-- | This token parser parses an integer (a whole number). This parser
-- is like 'natural' except that it can be prefixed with
-- sign (i.e. \'-\' or \'+\'). Returns the value of the number. The
-- number can be specified in 'decimal', 'hexadecimal'
-- or 'octal'. The number is parsed according
-- to the grammar rules in the Haskell report.
integer :: TokenParsing m => m Integer
integer = token (token (highlight Operator sgn <*> natural')) <?> "integer"
  where
  sgn = negate <$ char '-'
    <|> id <$ char '+'
    <|> pure id
{-# INLINE integer #-}

-- | This token parser parses a floating point value. Returns the value
-- of the number. The number is parsed according to the grammar rules
-- defined in the Haskell report.
double :: TokenParsing m => m Double
double = token (highlight Number floating <?> "double")
{-# INLINE double #-}

-- | This token parser parses either 'natural' or a 'float'.
-- Returns the value of the number. This parsers deals with
-- any overlap in the grammar rules for naturals and floats. The number
-- is parsed according to the grammar rules defined in the Haskell report.
naturalOrDouble :: TokenParsing m => m (Either Integer Double)
naturalOrDouble = token (highlight Number natDouble <?> "number")
{-# INLINE naturalOrDouble #-}

-- | Token parser @symbol s@ parses 'string' @s@ and skips
-- trailing white space.
symbol :: TokenParsing m => String -> m String
symbol name = token (highlight Symbol (string name))
{-# INLINE symbol #-}

-- | Token parser @symbolic s@ parses 'char' @s@ and skips
-- trailing white space.
symbolic :: TokenParsing m => Char -> m Char
symbolic name = token (highlight Symbol (char name))
{-# INLINE symbolic #-}

-- | Token parser @parens p@ parses @p@ enclosed in parenthesis,
-- returning the value of @p@.
parens :: TokenParsing m => m a -> m a
parens = nesting . between (symbolic '(') (symbolic ')')
{-# INLINE parens #-}

-- | Token parser @braces p@ parses @p@ enclosed in braces (\'{\' and
-- \'}\'), returning the value of @p@.
braces :: TokenParsing m => m a -> m a
braces = nesting . between (symbolic '{') (symbolic '}')
{-# INLINE braces #-}

-- | Token parser @angles p@ parses @p@ enclosed in angle brackets (\'\<\'
-- and \'>\'), returning the value of @p@.
angles :: TokenParsing m => m a -> m a
angles = nesting . between (symbolic '<') (symbolic '>')
{-# INLINE angles #-}

-- | Token parser @brackets p@ parses @p@ enclosed in brackets (\'[\'
-- and \']\'), returning the value of @p@.
brackets :: TokenParsing m => m a -> m a
brackets = nesting . between (symbolic '[') (symbolic ']')
{-# INLINE brackets #-}

-- | Token parser @comma@ parses the character \',\' and skips any
-- trailing white space. Returns the string \",\".
comma :: TokenParsing m => m Char
comma = symbolic ','
{-# INLINE comma #-}

-- | Token parser @colon@ parses the character \':\' and skips any
-- trailing white space. Returns the string \":\".
colon :: TokenParsing m => m Char
colon = symbolic ':'
{-# INLINE colon #-}

-- | Token parser @dot@ parses the character \'.\' and skips any
-- trailing white space. Returns the string \".\".
dot :: TokenParsing m => m Char
dot = symbolic '.'
{-# INLINE dot #-}

-- | Token parser @semiSep p@ parses /zero/ or more occurrences of @p@
-- separated by 'semi'. Returns a list of values returned by @p@.
semiSep :: TokenParsing m => m a -> m [a]
semiSep p = sepBy p semi
{-# INLINE semiSep #-}

-- | Token parser @semiSep1 p@ parses /one/ or more occurrences of @p@
-- separated by 'semi'. Returns a list of values returned by @p@.
semiSep1 :: TokenParsing m => m a -> m [a]
semiSep1 p = sepBy1 p semi
{-# INLINE semiSep1 #-}

-- | Token parser @commaSep p@ parses /zero/ or more occurrences of
-- @p@ separated by 'comma'. Returns a list of values returned
-- by @p@.
commaSep :: TokenParsing m => m a -> m [a]
commaSep p = sepBy p comma
{-# INLINE commaSep #-}

-- | Token parser @commaSep1 p@ parses /one/ or more occurrences of
-- @p@ separated by 'comma'. Returns a list of values returned
-- by @p@.
commaSep1 :: TokenParsing m => m a -> m [a]
commaSep1 p = sepBy p comma
{-# INLINE commaSep1 #-}

-- | Additional functionality that is needed to tokenize input while ignoring whitespace.
class CharParsing m => TokenParsing m where
  -- | Usually, someSpace consists of /one/ or more occurrences of a 'space'.
  -- Some parsers may choose to recognize line comments or block (multi line)
  -- comments as white space as well.
  someSpace :: m ()
  someSpace = skipSome (satisfy isSpace)

  -- | Called when we enter a nested pair of symbols.
  -- Overloadable to enable disabling layout
  nesting :: m a -> m a
  nesting = id

  -- | The token parser |semi| parses the character \';\' and skips
  -- any trailing white space. Returns the character \';\'. Overloadable to
  -- permit automatic semicolon insertion or Haskell-style layout.
  semi :: m Char
  semi = (satisfy (';'==) <?> ";") <* (someSpace <|> pure ())

  -- | Tag a region of parsed text with a bit of semantic information.
  -- Most parsers won't use this, but it is indispensible for highlighters.
  highlight :: Highlight -> m a -> m a
  highlight _ a = a

instance (TokenParsing m, MonadPlus m) => TokenParsing (Lazy.StateT s m) where
  nesting (Lazy.StateT m) = Lazy.StateT $ nesting . m
  someSpace = lift someSpace
  semi      = lift semi
  highlight h (Lazy.StateT m) = Lazy.StateT $ highlight h . m

instance (TokenParsing m, MonadPlus m) => TokenParsing (Strict.StateT s m) where
  nesting (Strict.StateT m) = Strict.StateT $ nesting . m
  someSpace = lift someSpace
  semi      = lift semi
  highlight h (Strict.StateT m) = Strict.StateT $ highlight h . m

instance (TokenParsing m, MonadPlus m) => TokenParsing (ReaderT e m) where
  nesting (ReaderT m) = ReaderT $ nesting . m
  someSpace = lift someSpace
  semi      = lift semi
  highlight h (ReaderT m) = ReaderT $ highlight h . m

instance (TokenParsing m, MonadPlus m, Monoid w) => TokenParsing (Strict.WriterT w m) where
  nesting (Strict.WriterT m) = Strict.WriterT $ nesting m
  someSpace = lift someSpace
  semi      = lift semi
  highlight h (Strict.WriterT m) = Strict.WriterT $ highlight h m

instance (TokenParsing m, MonadPlus m, Monoid w) => TokenParsing (Lazy.WriterT w m) where
  nesting (Lazy.WriterT m) = Lazy.WriterT $ nesting m
  someSpace = lift someSpace
  semi      = lift semi
  highlight h (Lazy.WriterT m) = Lazy.WriterT $ highlight h m

instance (TokenParsing m, MonadPlus m, Monoid w) => TokenParsing (Lazy.RWST r w s m) where
  nesting (Lazy.RWST m) = Lazy.RWST $ \r s -> nesting (m r s)
  someSpace = lift someSpace
  semi      = lift semi
  highlight h (Lazy.RWST m) = Lazy.RWST $ \r s -> highlight h (m r s)

instance (TokenParsing m, MonadPlus m, Monoid w) => TokenParsing (Strict.RWST r w s m) where
  nesting (Strict.RWST m) = Strict.RWST $ \r s -> nesting (m r s)
  someSpace = lift someSpace
  semi      = lift semi
  highlight h (Strict.RWST m) = Strict.RWST $ \r s -> highlight h (m r s)

instance (TokenParsing m, MonadPlus m) => TokenParsing (IdentityT m) where
  nesting = IdentityT . nesting . runIdentityT
  someSpace = lift someSpace
  semi      = lift semi
  highlight h = IdentityT . highlight h . runIdentityT

-- | Used to describe an input style for constructors, values, operators, etc.
data IdentifierStyle m = IdentifierStyle
  { styleName              :: String
  , styleStart             :: m Char
  , styleLetter            :: m Char
  , styleReserved          :: HashSet String
  , styleHighlight         :: Highlight
  , styleReservedHighlight :: Highlight
  }

-- | Lift an identifier style into a monad transformer
liftIdentifierStyle :: (MonadTrans t, Monad m) => IdentifierStyle m -> IdentifierStyle (t m)
liftIdentifierStyle s =
  s { styleStart  = lift (styleStart s)
    , styleLetter = lift (styleLetter s)
    }
{-# INLINE liftIdentifierStyle #-}

-- | parse a reserved operator or identifier using a given style
reserve :: (TokenParsing m, Monad m) => IdentifierStyle m -> String -> m ()
reserve s name = token $ try $ do
   _ <- highlight (styleReservedHighlight s) $ string name
   notFollowedBy (styleLetter s) <?> "end of " ++ show name
{-# INLINE reserve #-}

-- | parse an non-reserved identifier or symbol
ident :: (TokenParsing m, Monad m) => IdentifierStyle m -> m String
ident s = token $ try $ do
  name <- highlight (styleHighlight s)
          ((:) <$> styleStart s <*> many (styleLetter s) <?> styleName s)
  when (HashSet.member name (styleReserved s)) $ unexpected $ "reserved " ++ styleName s ++ " " ++ show name
  return name
{-# INLINE ident #-}

-- * Utilities


-- | This parser parses a character literal without the surrounding quotation marks.
--
-- This parser does NOT swallow trailing whitespace

characterChar :: TokenParsing m => m Char

charEscape, charLetter :: TokenParsing m => m Char
characterChar = charLetter <|> charEscape <?> "literal character"
{-# INLINE characterChar #-}

charEscape = highlight EscapeCode $ char '\\' *> escapeCode
{-# INLINE charEscape #-}

charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))
{-# INLINE charLetter #-}

-- | This parser parses a literal string. Returns the literal
-- string value. This parsers deals correctly with escape sequences and
-- gaps. The literal string is parsed according to the grammar rules
-- defined in the Haskell report (which matches most programming
-- languages quite closely).
--
-- This parser does NOT swallow trailing whitespace

escapeCode :: TokenParsing m => m Char
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
natural' :: TokenParsing m => m Integer
natural' = highlight Number nat <?> "natural"

number :: TokenParsing m => Integer -> m Char -> m Integer
number base baseDigit =
  foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 <$> some baseDigit

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

integer' :: TokenParsing m => m Integer
integer' = int <?> "integer"
{-# INLINE integer' #-}

sign :: TokenParsing m => m (Integer -> Integer)
sign = highlight Operator
     $ negate <$ char '-'
   <|> id <$ char '+'
   <|> pure id

int :: TokenParsing m => m Integer
int = {-token-} sign <*> highlight Number nat
nat, zeroNumber :: TokenParsing m => m Integer
nat = zeroNumber <|> decimal
zeroNumber = char '0' *> (hexadecimal <|> octal <|> decimal <|> pure 0) <?> ""

floating :: TokenParsing m => m Double
floating = decimal <**> fractExponent
{-# INLINE floating #-}

fractExponent :: TokenParsing m => m (Integer -> Double)
fractExponent = (\fract expo n -> (fromInteger n + fract) * expo) <$> fraction <*> option 1.0 exponent'
            <|> (\expo n -> fromInteger n * expo) <$> exponent' where
  fraction = Prelude.foldr op 0.0 <$> (char '.' *> (some digit <?> "fraction"))
  op d f = (f + fromIntegral (digitToInt d))/10.0
  exponent' = ((\f e -> power (f e)) <$ oneOf "eE" <*> sign <*> (decimal <?> "exponent")) <?> "exponent"
  power e
    | e < 0     = 1.0/power(-e)
    | otherwise = fromInteger (10^e)

natDouble, zeroNumFloat, decimalFloat :: TokenParsing m => m (Either Integer Double)
natDouble
    = char '0' *> zeroNumFloat
  <|> decimalFloat
zeroNumFloat
    = Left <$> (hexadecimal <|> octal)
  <|> decimalFloat
  <|> pure 0 <**> fractFloat
  <|> pure (Left 0)
decimalFloat = decimal <**> option Left (try fractFloat)

fractFloat :: TokenParsing m => m (Integer -> Either Integer Double)
fractFloat = (Right .) <$> fractExponent
{-# INLINE fractFloat #-}

-- | Parses a positive whole number in the decimal system. Returns the
-- value of the number.
--
-- This parser does NOT swallow trailing whitespace
decimal :: TokenParsing m => m Integer
decimal = number 10 digit
{-# INLINE decimal #-}

-- | Parses a positive whole number in the hexadecimal system. The number
-- should be prefixed with \"x\" or \"X\". Returns the value of the
-- number.
--
-- This parser does NOT swallow trailing whitespace
hexadecimal :: TokenParsing m => m Integer
hexadecimal = oneOf "xX" *> number 16 hexDigit
{-# INLINE hexadecimal #-}

-- | Parses a positive whole number in the octal system. The number
-- should be prefixed with \"o\" or \"O\". Returns the value of the
-- number.
--
-- This parser does NOT swallow trailing whitespace
octal :: TokenParsing m => m Integer
octal = oneOf "oO" *> number 8 octDigit
{-# INLINE octal #-}

-- | This is a parser transformer you can use to disable syntax highlighting
-- over a range of text you are parsing.
newtype Unhighlighted m a = Unhighlighted { runUnhighlighted :: m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,Parsing,CharParsing)

instance MonadTrans Unhighlighted where
  lift = Unhighlighted

instance (TokenParsing m, MonadPlus m) => TokenParsing (Unhighlighted m) where
  nesting (Unhighlighted m) = Unhighlighted (nesting m)
  someSpace = lift someSpace
  semi      = lift semi
  highlight _ m = m

-- | This is a parser transformer you can use to disable the automatic trailing
-- space consumption of a Token parser.
newtype Unspaced m a = Unspaced { runUnspaced :: m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,Parsing,CharParsing)

instance MonadTrans Unspaced where
  lift = Unspaced

instance (TokenParsing m, MonadPlus m) => TokenParsing (Unspaced m) where
  nesting (Unspaced m) = Unspaced (nesting m)
  someSpace = empty
  semi      = lift semi
  highlight h (Unspaced m) = Unspaced (highlight h m)
