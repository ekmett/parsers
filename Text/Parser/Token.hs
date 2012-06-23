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
-----------------------------------------------------------------------------
module Text.Parser.Token
  ( TokenParsing(..)
  -- * Token Parsers
  , whiteSpace
  , token
  , charLiteral
  , stringLiteral
  , natural
  , integer
  , double
  , naturalOrDouble
  , symbol
  , symbolic
  , parens
  , braces
  , angles
  , brackets
  , comma
  , colon
  , dot
  , semiSep
  , semiSep1
  , commaSep
  , commaSep1
  -- * Identifiers
  , IdentifierStyle(..)
  , liftIdentifierStyle
  , ident
  , reserve
  ) where

import Control.Applicative
import Control.Monad (when)
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
import Data.HashSet as HashSet
import Data.Monoid
import Text.Parser.Combinators
import Text.Parser.Char

-- | Skip zero or more bytes worth of white space. More complex parsers are‗
-- free to consider comments as white space.
whiteSpace :: TokenParsing m => m ()
whiteSpace = someSpace <|> return ()
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

-- | This token parser parses a single literal character. Returns the
-- literal character value. This parsers deals correctly with escape
-- sequences. The literal character is parsed according to the grammar
-- rules defined in the Haskell report (which matches most programming
-- languages quite closely).
charLiteral :: TokenParsing m => m Char
charLiteral = token charLiteral'

-- | This token parser parses a literal string. Returns the literal
-- string value. This parsers deals correctly with escape sequences and
-- gaps. The literal string is parsed according to the grammar rules
-- defined in the Haskell report (which matches most programming
-- languages quite closely).

stringLiteral :: TokenParsing m => m String
stringLiteral = token stringLiteral'

-- | This token parser parses a natural number (a positive whole
-- number). Returns the value of the number. The number can be
-- specified in 'decimal', 'hexadecimal' or
-- 'octal'. The number is parsed according to the grammar
-- rules in the Haskell report.

natural :: TokenParsing m => m Integer
natural = token natural'

-- | This token parser parses an integer (a whole number). This parser
-- is like 'natural' except that it can be prefixed with
-- sign (i.e. \'-\' or \'+\'). Returns the value of the number. The
-- number can be specified in 'decimal', 'hexadecimal'
-- or 'octal'. The number is parsed according
-- to the grammar rules in the Haskell report.

integer :: TokenParsing m => m Integer
integer = token int <?> "integer"
  where
  sign = negate <$ char '-'
    <|> id <$ char '+'
    <|> pure id
  int = token sign <*> natural'

-- | This token parser parses a floating point value. Returns the value
-- of the number. The number is parsed according to the grammar rules
-- defined in the Haskell report.

double :: TokenParsing m => m Double
double = token double'

-- | This token parser parses either 'natural' or a 'float'.
-- Returns the value of the number. This parsers deals with
-- any overlap in the grammar rules for naturals and floats. The number
-- is parsed according to the grammar rules defined in the Haskell report.

naturalOrDouble :: TokenParsing m => m (Either Integer Double)
naturalOrDouble = token naturalOrDouble'

-- | Token parser @symbol s@ parses 'string' @s@ and skips
-- trailing white space.

symbol :: TokenParsing m => String -> m String
symbol name = token (string name)

-- | Token parser @symbolic s@ parses 'char' @s@ and skips
-- trailing white space.

symbolic :: TokenParsing m => Char -> m Char
symbolic name = token (char name)

-- | Token parser @parens p@ parses @p@ enclosed in parenthesis,
-- returning the value of @p@.

parens :: TokenParsing m => m a -> m a
parens = nesting . between (symbolic '(') (symbolic ')')

-- | Token parser @braces p@ parses @p@ enclosed in braces (\'{\' and
-- \'}\'), returning the value of @p@.

braces :: TokenParsing m => m a -> m a
braces = nesting . between (symbolic '{') (symbolic '}')

-- | Token parser @angles p@ parses @p@ enclosed in angle brackets (\'\<\'
-- and \'>\'), returning the value of @p@.

angles :: TokenParsing m => m a -> m a
angles = nesting . between (symbolic '<') (symbolic '>')

-- | Token parser @brackets p@ parses @p@ enclosed in brackets (\'[\'
-- and \']\'), returning the value of @p@.

brackets :: TokenParsing m => m a -> m a
brackets = nesting . between (symbolic '[') (symbolic ']')

-- | Token parser @comma@ parses the character \',\' and skips any
-- trailing white space. Returns the string \",\".

comma :: TokenParsing m => m Char
comma = symbolic ','

-- | Token parser @colon@ parses the character \':\' and skips any
-- trailing white space. Returns the string \":\".

colon :: TokenParsing m => m Char
colon = symbolic ':'

-- | Token parser @dot@ parses the character \'.\' and skips any
-- trailing white space. Returns the string \".\".

dot :: TokenParsing m => m Char
dot = symbolic '.'

-- | Token parser @semiSep p@ parses /zero/ or more occurrences of @p@
-- separated by 'semi'. Returns a list of values returned by
-- @p@.

semiSep :: TokenParsing m => m a -> m [a]
semiSep p = sepBy p semi

-- | Token parser @semiSep1 p@ parses /one/ or more occurrences of @p@
-- separated by 'semi'. Returns a list of values returned by @p@.

semiSep1 :: TokenParsing m => m a -> m [a]
semiSep1 p = sepBy1 p semi

-- | Token parser @commaSep p@ parses /zero/ or more occurrences of
-- @p@ separated by 'comma'. Returns a list of values returned
-- by @p@.

commaSep :: TokenParsing m => m a -> m [a]
commaSep p = sepBy p comma

-- | Token parser @commaSep1 p@ parses /one/ or more occurrences of
-- @p@ separated by 'comma'. Returns a list of values returned
-- by @p@.

commaSep1 :: TokenParsing m => m a -> m [a]
commaSep1 p = sepBy p comma

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

instance TokenParsing m => TokenParsing (Lazy.StateT s m) where
  nesting (Lazy.StateT m) = Lazy.StateT $ nesting . m
  someSpace = lift someSpace
  semi      = lift semi


instance TokenParsing m => TokenParsing (Strict.StateT s m) where
  nesting (Strict.StateT m) = Strict.StateT $ nesting . m
  someSpace = lift someSpace
  semi      = lift semi

instance TokenParsing m => TokenParsing (ReaderT e m) where
  nesting (ReaderT m) = ReaderT $ nesting . m
  someSpace = lift someSpace
  semi      = lift semi

instance (TokenParsing m, Monoid w) => TokenParsing (Strict.WriterT w m) where
  nesting (Strict.WriterT m) = Strict.WriterT $ nesting m
  someSpace = lift someSpace
  semi      = lift semi

instance (TokenParsing m, Monoid w) => TokenParsing (Lazy.WriterT w m) where
  nesting (Lazy.WriterT m) = Lazy.WriterT $ nesting m
  someSpace = lift someSpace
  semi      = lift semi

instance (TokenParsing m, Monoid w) => TokenParsing (Lazy.RWST r w s m) where
  nesting (Lazy.RWST m) = Lazy.RWST $ \r s -> nesting (m r s)
  someSpace = lift someSpace
  semi      = lift semi

instance (TokenParsing m, Monoid w) => TokenParsing (Strict.RWST r w s m) where
  nesting (Strict.RWST m) = Strict.RWST $ \r s -> nesting (m r s)
  someSpace = lift someSpace
  semi      = lift semi

instance TokenParsing m => TokenParsing (IdentityT m) where
  nesting = IdentityT . nesting . runIdentityT
  someSpace = lift someSpace
  semi      = lift semi


data IdentifierStyle m = IdentifierStyle
  { styleName              :: String
  , styleStart             :: m Char
  , styleLetter            :: m Char
  , styleReserved          :: HashSet String
  }

-- | Lift an identifier style into a monad transformer
liftIdentifierStyle :: (MonadTrans t, Monad m) => IdentifierStyle m -> IdentifierStyle (t m)
liftIdentifierStyle s =
  s { styleStart  = lift (styleStart s)
    , styleLetter = lift (styleLetter s)
    }

-- | parse a reserved operator or identifier using a given style
reserve :: TokenParsing m => IdentifierStyle m -> String -> m ()
reserve s name = token $ try $ do
   _ <- string name
   notFollowedBy (styleLetter s) <?> "end of " ++ show name

-- | parse an non-reserved identifier or symbol
ident :: TokenParsing m => IdentifierStyle m -> m String
ident s = token $ try $ do
  name <- (:) <$> styleStart s <*> many (styleLetter s) <?> styleName s
  when (member name (styleReserved s)) $ unexpected $ "reserved " ++ styleName s ++ " " ++ show name
  return name
