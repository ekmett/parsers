{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parser.Expression
-- Copyright   :  (c) Edward Kmett 2011-2012
--                (c) Paolo Martini 2007
--                (c) Daan Leijen 1999-2001,
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A helper module to parse \"expressions\".
-- Builds a parser given a table of operators and associativities.
--
-----------------------------------------------------------------------------

module Text.Parser.Expression
    ( Assoc(..), Operator(..), OperatorTable
    , buildExpressionParser
    ) where

import Control.Applicative
import Text.Parser.Combinators
import Data.Data hiding (Infix, Prefix)
import Data.Ix

-----------------------------------------------------------
-- Assoc and OperatorTable
-----------------------------------------------------------

-- |  This data type specifies the associativity of operators: left, right
-- or none.

data Assoc
  = AssocNone
  | AssocLeft
  | AssocRight
  deriving (Eq,Ord,Show,Read,Ix,Enum,Bounded,Data,Typeable)

-- | This data type specifies operators that work on values of type @a@.
-- An operator is either binary infix or unary prefix or postfix. A
-- binary operator has also an associated associativity.

data Operator m a
  = Infix (m (a -> a -> a)) Assoc
  | Prefix (m (a -> a))
  | Postfix (m (a -> a))

-- | An @OperatorTable m a@ is a list of @Operator m a@
-- lists. The list is ordered in descending
-- precedence. All operators in one list have the same precedence (but
-- may have a different associativity).

type OperatorTable m a = [[Operator m a]]

-----------------------------------------------------------
-- Convert an OperatorTable and basic term parser into
-- a full fledged expression parser
-----------------------------------------------------------

-- | @buildExpressionParser table term@ builds an expression parser for
-- terms @term@ with operators from @table@, taking the associativity
-- and precedence specified in @table@ into account. Prefix and postfix
-- operators of the same precedence can only occur once (i.e. @--2@ is
-- not allowed if @-@ is prefix negate). Prefix and postfix operators
-- of the same precedence associate to the left (i.e. if @++@ is
-- postfix increment, than @-2++@ equals @-1@, not @-3@).
--
-- The @buildExpressionParser@ takes care of all the complexity
-- involved in building expression parser. Here is an example of an
-- expression parser that handles prefix signs, postfix increment and
-- basic arithmetic.
--
-- >  tp = makeTokenParser emptyDef
-- >
-- >  expr    = buildExpressionParser table term
-- >          <?> "expression"
-- >
-- >  term    =  parens tp expr
-- >          <|> natural tp
-- >          <?> "simple expression"
-- >
-- >  table   = [ [prefix "-" negate, prefix "+" id ]
-- >            , [postfix "++" (+1)]
-- >            , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
-- >            , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
-- >            ]
-- >
-- >  binary  name fun assoc = Infix (fun <$ reservedOp tp name) assoc
-- >  prefix  name fun       = Prefix (fun <$ reservedOp tp name)
-- >  postfix name fun       = Postfix (fun <$ reservedOp tp name)

buildExpressionParser :: forall m a. (Parsing m, Applicative m)
                      => OperatorTable m a
                      -> m a
                      -> m a
buildExpressionParser operators simpleExpr
    = foldl makeParser simpleExpr operators
    where
      makeParser term ops
        = let (rassoc,lassoc,nassoc,prefix,postfix) = foldr splitOp ([],[],[],[],[]) ops
              rassocOp   = choice rassoc
              lassocOp   = choice lassoc
              nassocOp   = choice nassoc
              prefixOp   = choice prefix  <?> ""
              postfixOp  = choice postfix <?> ""

              ambiguous assoc op = try $ op *> empty <?> ("ambiguous use of a " ++ assoc ++ "-associative operator")

              ambiguousRight    = ambiguous "right" rassocOp
              ambiguousLeft     = ambiguous "left" lassocOp
              ambiguousNon      = ambiguous "non" nassocOp

              termP      = (prefixP <*> term) <**> postfixP

              postfixP   = postfixOp <|> pure id

              prefixP    = prefixOp <|> pure id

              rassocP, rassocP1, lassocP, lassocP1, nassocP :: m (a -> a)

              rassocP  = (flip <$> rassocOp <*> (termP <**> rassocP1)
                          <|> ambiguousLeft
                          <|> ambiguousNon)

              rassocP1 = rassocP <|> pure id

              lassocP  = ((flip <$> lassocOp <*> termP) <**> ((.) <$> lassocP1)
                          <|> ambiguousRight
                          <|> ambiguousNon)

              lassocP1 = lassocP <|> pure id

              nassocP = (flip <$> nassocOp <*> termP)
                        <**> (ambiguousRight
                              <|> ambiguousLeft
                              <|> ambiguousNon
                              <|> pure id)
           in termP <**> (rassocP <|> lassocP <|> nassocP <|> pure id) <?> "operator"


      splitOp (Infix op assoc) (rassoc,lassoc,nassoc,prefix,postfix)
        = case assoc of
            AssocNone  -> (rassoc,lassoc,op:nassoc,prefix,postfix)
            AssocLeft  -> (rassoc,op:lassoc,nassoc,prefix,postfix)
            AssocRight -> (op:rassoc,lassoc,nassoc,prefix,postfix)

      splitOp (Prefix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,op:prefix,postfix)

      splitOp (Postfix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,prefix,op:postfix)
