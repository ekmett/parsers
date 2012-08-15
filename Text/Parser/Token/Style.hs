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
  { commentStart   :: String -- ^ String that starts a multiline comment
  , commentEnd     :: String -- ^ String that ends a multiline comment
  , commentLine    :: String -- ^ String that starts a single line comment
  , commentNesting :: Bool   -- ^ Can we nest multiline comments?
  }

-- | No comments at all
emptyCommentStyle :: CommentStyle
emptyCommentStyle   = CommentStyle "" "" "" True

-- | Use java-style comments
javaCommentStyle :: CommentStyle
javaCommentStyle = CommentStyle "/*" "*/" "//" True

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
  { styleName     = "operator"
  , styleStart    = styleLetter emptyOps
  , styleLetter   = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , styleReserved = mempty
  , styleHighlight = Operator
  , styleReservedHighlight = ReservedOperator
  }
-- | A simple operator style based on haskell with the operators from Haskell 98.
haskell98Ops, haskellOps :: TokenParsing m => IdentifierStyle m
haskell98Ops = emptyOps
  { styleReserved = set ["::","..","=","\\","|","<-","->","@","~","=>"]
  }
haskellOps = haskell98Ops

-- | A simple identifier style based on haskell with no reserve words
emptyIdents :: TokenParsing m => IdentifierStyle m
emptyIdents = IdentifierStyle
  { styleName     = "identifier"
  , styleStart    = letter <|> char '_'
  , styleLetter   = alphaNum <|> oneOf "_'"
  , styleReserved = set []
  , styleHighlight = Identifier
  , styleReservedHighlight = ReservedIdentifier
  }

-- | A simple identifier style based on haskell with only the reserved words from Haskell 98.
haskell98Idents :: TokenParsing m => IdentifierStyle m
haskell98Idents = emptyIdents
  { styleReserved = set haskell98ReservedIdents }

-- | A simple identifier style based on haskell with the reserved words from Haskell 98 and some common extensions.
haskellIdents :: TokenParsing m => IdentifierStyle m
haskellIdents = haskell98Idents
  { styleLetter   = styleLetter haskell98Idents <|> char '#'
  , styleReserved = set $ haskell98ReservedIdents ++
      ["foreign","import","export","primitive","_ccall_","_casm_" ,"forall"]
  }

haskell98ReservedIdents :: [String]
haskell98ReservedIdents =
  ["let","in","case","of","if","then","else","data","type"
  ,"class","default","deriving","do","import","infix"
  ,"infixl","infixr","instance","module","newtype"
  ,"where","primitive" -- "as","qualified","hiding"
  ]
