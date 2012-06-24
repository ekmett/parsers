-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parser.Token.Highlight
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Highlighting isn't strictly a parsing concern, but it makes more sense
-- to annotate a parser with highlighting information than to require
-- someone to completely reimplement all of the combinators to add
-- this functionality later when they need it.
--
----------------------------------------------------------------------------
module Text.Parser.Token.Highlight
  ( Highlight(..)
  ) where

data Highlight
  = EscapeCode
  | Number
  | Comment
  | CharLiteral
  | StringLiteral
  | Constant
  | Statement
  | Special
  | Symbol
  | Identifier
  | ReservedIdentifier
  | Operator
  | ReservedOperator
  | Constructor
  | ReservedConstructor
  | ConstructorOperator
  | ReservedConstructorOperator
  | BadInput
  | Unbound
  | Layout
  | MatchedSymbols
  | LiterateComment
  | LiterateSyntax
  deriving (Eq,Ord,Show,Read,Enum,Bounded)
