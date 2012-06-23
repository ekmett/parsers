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
  -- * identifier styles
  , emptyIdents, haskellIdents, haskell98Idents
  -- * operator styles
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

data CommentStyle = CommentStyle
  { commentStart   :: String
  , commentEnd     :: String
  , commentLine    :: String
  , commentNesting :: Bool
  }

emptyCommentStyle, javaCommentStyle, haskellCommentStyle :: CommentStyle
emptyCommentStyle   = CommentStyle "" "" "" True
javaCommentStyle    = CommentStyle "/*" "*/" "//" True
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

emptyOps, haskell98Ops, haskellOps :: TokenParsing m => IdentifierStyle m
emptyOps = IdentifierStyle
  { styleName     = "operator"
  , styleStart    = styleLetter emptyOps
  , styleLetter   = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , styleReserved = mempty
  , styleHighlight = Operator
  , styleReservedHighlight = ReservedOperator
  }
haskell98Ops = emptyOps
  { styleReserved = set ["::","..","=","\\","|","<-","->","@","~","=>"]
  }
haskellOps = haskell98Ops

emptyIdents, haskell98Idents, haskellIdents :: TokenParsing m => IdentifierStyle m
emptyIdents = IdentifierStyle
  { styleName     = "identifier"
  , styleStart    = letter <|> char '_'
  , styleLetter   = alphaNum <|> oneOf "_'"
  , styleReserved = set []
  , styleHighlight = Identifier
  , styleReservedHighlight = ReservedIdentifier
  }

haskell98Idents = emptyIdents
  { styleReserved = set haskell98ReservedIdents }
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
