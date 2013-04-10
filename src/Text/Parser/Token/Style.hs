-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parser.Token.Style
-- Copyright   :  (c) Edward Kmett 2011-2012
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-- A toolbox for specifying comment and identifier styles
--
-- This must be imported directly as it is not re-exported elsewhere
--
-----------------------------------------------------------------------------
module Text.Parser.Token.Style
  (
  -- * Comment and white space styles
    CommentStyle(..)
  -- ** Lenses
  , commentStart
  , commentEnd
  , commentLine
  , commentNesting
  -- ** Common Comment Styles
  , emptyCommentStyle
  , javaCommentStyle
  , haskellCommentStyle
  , buildSomeSpaceParser
  -- * Identifier Styles
  , emptyIdents, haskellIdents, haskell98Idents
  -- * Operator Styles
  , emptyOps, haskellOps, haskell98Ops
  ) where

import Control.Applicative
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.Monoid
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Token.Highlight
import Data.List (nub)

-- | How to deal with comments.
data CommentStyle = CommentStyle
  { _commentStart   :: String -- ^ String that starts a multiline comment
  , _commentEnd     :: String -- ^ String that ends a multiline comment
  , _commentLine    :: String -- ^ String that starts a single line comment
  , _commentNesting :: Bool   -- ^ Can we nest multiline comments?
  }

-- | This is a lens that can edit the string that starts a multiline comment.
--
-- @'commentStart' :: Lens' 'CommentStyle' 'String'@
commentStart :: Functor f => (String -> f String) -> CommentStyle -> f CommentStyle
commentStart f (CommentStyle s e l n) = (\s' -> CommentStyle s' e l n) <$> f s
{-# INLINE commentStart #-}

-- | This is a lens that can edit the string that ends a multiline comment.
--
-- @'commentEnd' :: Lens' 'CommentStyle' 'String'@
commentEnd :: Functor f => (String -> f String) -> CommentStyle -> f CommentStyle
commentEnd f (CommentStyle s e l n) = (\e' -> CommentStyle s e' l n) <$> f e
{-# INLINE commentEnd #-}

-- | This is a lens that can edit the string that starts a single line comment.
--
-- @'commentLine' :: Lens' 'CommentStyle' 'String'@
commentLine :: Functor f => (String -> f String) -> CommentStyle -> f CommentStyle
commentLine f (CommentStyle s e l n) = (\l' -> CommentStyle s e l' n) <$> f l
{-# INLINE commentLine #-}

-- | This is a lens that can edit whether we can nest multiline comments.
--
-- @'commentNesting' :: Lens' 'CommentStyle' 'Bool'@
commentNesting :: Functor f => (Bool -> f Bool) -> CommentStyle -> f CommentStyle
commentNesting f (CommentStyle s e l n) = CommentStyle s e l <$> f n
{-# INLINE commentNesting #-}

-- | No comments at all
emptyCommentStyle :: CommentStyle
emptyCommentStyle   = CommentStyle "" "" "" True

-- | Use java-style comments
javaCommentStyle :: CommentStyle
javaCommentStyle = CommentStyle "/*" "*/" "//" False

-- | Use haskell-style comments
haskellCommentStyle :: CommentStyle
haskellCommentStyle = CommentStyle "{-" "-}" "--" True

-- | Use this to easily build the definition of whiteSpace for your MonadParser
--   given a comment style and an underlying someWhiteSpace parser
buildSomeSpaceParser :: CharParsing m => m () -> CommentStyle -> m ()
buildSomeSpaceParser simpleSpace (CommentStyle startStyle endStyle lineStyle nestingStyle)
  | noLine && noMulti  = skipSome (simpleSpace <?> "")
  | noLine             = skipSome (simpleSpace <|> multiLineComment <?> "")
  | noMulti            = skipSome (simpleSpace <|> oneLineComment <?> "")
  | otherwise          = skipSome (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
  where
    noLine  = null lineStyle
    noMulti = null startStyle
    oneLineComment = try (string lineStyle) *> skipMany (satisfy (/= '\n'))
    multiLineComment = try (string startStyle) *> inComment
    inComment = if nestingStyle then inCommentMulti else inCommentSingle
    inCommentMulti
      =   () <$ try (string endStyle)
      <|> multiLineComment *> inCommentMulti
      <|> skipSome (noneOf startEnd) *> inCommentMulti
      <|> oneOf startEnd *> inCommentMulti
      <?> "end of comment"
    startEnd = nub (endStyle ++ startStyle)
    inCommentSingle
      =   () <$ try (string endStyle)
      <|> skipSome (noneOf startEnd) *> inCommentSingle
      <|> oneOf startEnd *> inCommentSingle
      <?> "end of comment"

set :: [String] -> HashSet String
set = HashSet.fromList

-- | A simple operator style based on haskell with no reserved operators
emptyOps :: TokenParsing m => IdentifierStyle m
emptyOps = IdentifierStyle
  { _styleName     = "operator"
  , _styleStart    = _styleLetter emptyOps
  , _styleLetter   = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , _styleReserved = mempty
  , _styleHighlight = Operator
  , _styleReservedHighlight = ReservedOperator
  }
-- | A simple operator style based on haskell with the operators from Haskell 98.
haskell98Ops, haskellOps :: TokenParsing m => IdentifierStyle m
haskell98Ops = emptyOps
  { _styleReserved = set ["::","..","=","\\","|","<-","->","@","~","=>"]
  }
haskellOps = haskell98Ops

-- | A simple identifier style based on haskell with no reserve words
emptyIdents :: TokenParsing m => IdentifierStyle m
emptyIdents = IdentifierStyle
  { _styleName     = "identifier"
  , _styleStart    = letter <|> char '_'
  , _styleLetter   = alphaNum <|> oneOf "_'"
  , _styleReserved = set []
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

-- | A simple identifier style based on haskell with only the reserved words from Haskell 98.
haskell98Idents :: TokenParsing m => IdentifierStyle m
haskell98Idents = emptyIdents
  { _styleReserved = set haskell98ReservedIdents }

-- | A simple identifier style based on haskell with the reserved words from Haskell 98 and some common extensions.
haskellIdents :: TokenParsing m => IdentifierStyle m
haskellIdents = haskell98Idents
  { _styleLetter   = _styleLetter haskell98Idents <|> char '#'
  , _styleReserved = set $ haskell98ReservedIdents ++
      ["foreign","import","export","primitive","_ccall_","_casm_" ,"forall"]
  }

haskell98ReservedIdents :: [String]
haskell98ReservedIdents =
  ["let","in","case","of","if","then","else","data","type"
  ,"class","default","deriving","do","import","infix"
  ,"infixl","infixr","instance","module","newtype"
  ,"where","primitive" -- "as","qualified","hiding"
  ]
