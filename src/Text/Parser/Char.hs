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
    oneOf       -- :: CharParsing m => [Char] -> m Char
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
  -- * Class
  , CharParsing(..)
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
import Control.Monad (MonadPlus(..))
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
{-# ANN oneOf "HLint: ignore Use String" #-}

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character /not/ in the supplied list of characters @cs@. Returns the
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

-- | Additional functionality needed to parse character streams.
class Parsing m => CharParsing m where
  -- | Parse a single character of the input, with UTF-8 decoding
  satisfy :: (Char -> Bool) -> m Char
#ifdef USE_DEFAULT_SIGNATURES
  default satisfy :: (MonadTrans t, CharParsing n, Monad n, m ~ t n) =>
                     (Char -> Bool) ->
                     t n Char
  satisfy = lift . satisfy
#endif
  -- | @char c@ parses a single character @c@. Returns the parsed
  -- character (i.e. @c@).
  --
  -- /e.g./
  --
  -- @semiColon = 'char' ';'@
  char :: CharParsing m => Char -> m Char
  char c = satisfy (c ==) <?> show [c]
  {-# INLINE char #-}

  -- | @notChar c@ parses any single character other than @c@. Returns the parsed
  -- character.
  notChar :: CharParsing m => Char -> m Char
  notChar c = satisfy (c /=)
  {-# INLINE notChar #-}

  -- | This parser succeeds for any character. Returns the parsed character.
  anyChar :: CharParsing m => m Char
  anyChar = satisfy (const True)
  {-# INLINE anyChar #-}

  -- | @string s@ parses a sequence of characters given by @s@. Returns
  -- the parsed string (i.e. @s@).
  --
  -- >  divOrMod    =   string "div"
  -- >              <|> string "mod"
  string :: CharParsing m => String -> m String
  string s = s <$ try (traverse_ char s) <?> show s
  {-# INLINE string #-}


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
