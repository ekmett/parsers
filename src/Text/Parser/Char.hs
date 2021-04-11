{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  (
  -- * Combinators
    oneOf        -- :: CharParsing m => [Char] -> m Char
  , noneOf       -- :: CharParsing m => [Char] -> m Char
  , oneOfSet     -- :: CharParsing m => CharSet -> m Char
  , noneOfSet    -- :: CharParsing m => CharSet -> m Char
  , spaces       -- :: CharParsing m => m ()
  , space        -- :: CharParsing m => m Char
  , newline      -- :: CharParsing m => m Char
  , tab          -- :: CharParsing m => m Char
  , upper        -- :: CharParsing m => m Char
  , lower        -- :: CharParsing m => m Char
  , alphaNum     -- :: CharParsing m => m Char
  , letter       -- :: CharParsing m => m Char
  , digit        -- :: CharParsing m => m Char
  , hexDigit     -- :: CharParsing m => m Char
  , octDigit     -- :: CharParsing m => m Char
  , satisfyRange -- :: CharParsing m => Char -> Char -> m Char
  -- * Class
  , CharParsing(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Control.Monad (MonadPlus(..))
import Data.Char
import Data.CharSet (CharSet(..))
import qualified Data.CharSet as CharSet
import Data.Foldable
import qualified Data.IntSet as IntSet
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Parser.Combinators

#ifdef MIN_VERSION_parsec
import qualified Text.Parsec as Parsec
#endif

#ifdef MIN_VERSION_attoparsec
import qualified Data.Attoparsec.Types as Att
import qualified Data.Attoparsec.Combinator as Att
#endif

-- | @oneOf cs@ succeeds if the current character is in the supplied
-- list of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
--
-- >   vowel  = oneOf "aeiou"
oneOf :: CharParsing m => [Char] -> m Char
oneOf xs = oneOfSet (CharSet.fromList xs)
{-# INLINE oneOf #-}
{-# ANN oneOf "HLint: ignore Use String" #-}

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character is /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"
noneOf :: CharParsing m => [Char] -> m Char
noneOf xs = noneOfSet (CharSet.fromList xs)
{-# INLINE noneOf #-}
{-# ANN noneOf "HLint: ignore Use String" #-}

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
-- character is /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"
noneOfSet :: CharParsing m => CharSet -> m Char
noneOfSet s = oneOfSet (CharSet.complement s)
{-# INLINE noneOfSet #-}

-- | Skips /zero/ or more white space characters. See also 'skipMany'.
spaces :: CharParsing m => m ()
spaces = skipMany space <?> "white space"
{-# INLINE spaces #-}

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

-- | Parses an upper case letter. Returns the parsed character.
upper :: CharParsing m => m Char
upper = satisfy isUpper <?> "uppercase letter"
{-# INLINE upper #-}

-- | Parses a lower case character. Returns the parsed character.
lower :: CharParsing m => m Char
lower = satisfy isLower <?> "lowercase letter"
{-# INLINE lower #-}

-- | Parses a letter or digit. Returns the parsed character.
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

satisfyRange :: CharParsing m => Char -> Char -> m Char
satisfyRange a z = satisfy (\c -> c >= a && c <= z)
{-# INLINE satisfyRange #-}

-- | Additional functionality needed to parse character streams.
class Parsing m => CharParsing m where
  -- | Parse a single character of the input, with UTF-8 decoding
  satisfy :: (Char -> Bool) -> m Char
#ifdef USE_DEFAULT_SIGNATURES
  default satisfy :: (MonadTrans t, CharParsing n, Monad n, m ~ t n) =>
                     (Char -> Bool) ->
                     m Char
  satisfy = lift . satisfy
#endif
  -- | @char c@ parses a single character @c@. Returns the parsed
  -- character (i.e. @c@).
  --
  -- /e.g./
  --
  -- @semiColon = 'char' ';'@
  char :: Char -> m Char
  char c = satisfy (c ==) <?> show [c]
  {-# INLINE char #-}

  -- | @notChar c@ parses any single character other than @c@. Returns the parsed
  -- character.
  notChar :: Char -> m Char
  notChar c = satisfy (c /=)
  {-# INLINE notChar #-}

  -- | This parser succeeds for any character. Returns the parsed character.
  anyChar :: m Char
  anyChar = satisfy (const True)
  {-# INLINE anyChar #-}

  -- | @string s@ parses a sequence of characters given by @s@. Returns
  -- the parsed string (i.e. @s@).
  --
  -- >  divOrMod    =   string "div"
  -- >              <|> string "mod"
  string :: String -> m String
  string s = s <$ try (traverse_ char s) <?> show s
  {-# INLINE string #-}

  -- | @text t@ parses a sequence of characters determined by the text @t@ Returns
  -- the parsed text fragment (i.e. @t@).
  --
  -- Using @OverloadedStrings@:
  --
  -- >  divOrMod    =   text "div"
  -- >              <|> text "mod"
  text :: Text -> m Text
  text t = t <$ string (Text.unpack t)
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m) => CharParsing (Lazy.StateT s m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char    = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string  = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m) => CharParsing (Strict.StateT s m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char    = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string  = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m) => CharParsing (ReaderT e m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char    = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string  = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m, Monoid w) => CharParsing (Strict.WriterT w m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char    = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string  = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m, Monoid w) => CharParsing (Lazy.WriterT w m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char    = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string  = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m, Monoid w) => CharParsing (Lazy.RWST r w s m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char    = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string  = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m, Monoid w) => CharParsing (Strict.RWST r w s m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char    = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string  = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

instance (CharParsing m, MonadPlus m) => CharParsing (IdentityT m) where
  satisfy = lift . satisfy
  {-# INLINE satisfy #-}
  char    = lift . char
  {-# INLINE char #-}
  notChar = lift . notChar
  {-# INLINE notChar #-}
  anyChar = lift anyChar
  {-# INLINE anyChar #-}
  string  = lift . string
  {-# INLINE string #-}
  text = lift . text
  {-# INLINE text #-}

#ifdef MIN_VERSION_parsec
instance Parsec.Stream s m Char => CharParsing (Parsec.ParsecT s u m) where
  satisfy   = Parsec.satisfy
  char      = Parsec.char
  notChar c = Parsec.satisfy (/= c)
  anyChar   = Parsec.anyChar
  string    = Parsec.string
#endif

#ifdef MIN_VERSION_attoparsec
instance Att.Chunk t => CharParsing (Att.Parser t) where
  satisfy p = fmap e2c $ Att.satisfyElem $ p . e2c
    where e2c = Att.chunkElemToChar (undefined :: t)
  {-# INLINE satisfy #-}
#endif

instance CharParsing ReadP.ReadP where
  satisfy   = ReadP.satisfy
  char      = ReadP.char
  notChar c = ReadP.satisfy (/= c)
  anyChar   = ReadP.get
  string    = ReadP.string
