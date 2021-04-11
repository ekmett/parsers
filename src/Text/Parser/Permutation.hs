{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parser.Permutation
-- Copyright   :  (c) Edward Kmett 2011-2012
--                (c) Paolo Martini 2007
--                (c) Daan Leijen 1999-2001
-- License     :  BSD-style
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-- This module implements permutation parsers. The algorithm is described in:
--
-- /Parsing Permutation Phrases,/
-- by Arthur Baars, Andres Loh and Doaitse Swierstra.
-- Published as a functional pearl at the Haskell Workshop 2001.
--
-----------------------------------------------------------------------------
module Text.Parser.Permutation
    ( Permutation
    , permute
    , (<||>), (<$$>)
    , (<|?>), (<$?>)
    ) where

import Control.Applicative
import qualified Data.Foldable as F (asum)

infixl 1 <||>, <|?>
infixl 2 <$$>, <$?>

----------------------------------------------------------------
--  Building a permutation parser
----------------------------------------------------------------

-- | The expression @perm \<||> p@ adds parser @p@ to the permutation
-- parser @perm@. The parser @p@ is not allowed to accept empty input -
-- use the optional combinator ('<|?>') instead. Returns a
-- new permutation parser that includes @p@.

(<||>) :: Functor m => Permutation m (a -> b) -> m a -> Permutation m b
(<||>) = add
{-# INLINE (<||>) #-}

-- | The expression @f \<$$> p@ creates a fresh permutation parser
-- consisting of parser @p@. The final result of the permutation
-- parser is the function @f@ applied to the return value of @p@. The
-- parser @p@ is not allowed to accept empty input - use the optional
-- combinator ('<$?>') instead.
--
-- If the function @f@ takes more than one parameter, the type variable
-- @b@ is instantiated to a functional type which combines nicely with
-- the adds parser @p@ to the ('<||>') combinator. This
-- results in stylized code where a permutation parser starts with a
-- combining function @f@ followed by the parsers. The function @f@
-- gets its parameters in the order in which the parsers are specified,
-- but actual input can be in any order.

(<$$>) :: Functor m => (a -> b) -> m a -> Permutation m b
(<$$>) f p = newPermutation f <||> p
{-# INLINE (<$$>) #-}

-- | The expression @perm \<|?> (x,p)@ adds parser @p@ to the
-- permutation parser @perm@. The parser @p@ is optional - if it can
-- not be applied, the default value @x@ will be used instead. Returns
-- a new permutation parser that includes the optional parser @p@.

(<|?>) :: Functor m => Permutation m (a -> b) -> (a, m a) -> Permutation m b
(<|?>) perm (x,p) = addOpt perm x p
{-# INLINE (<|?>) #-}

-- | The expression @f \<$?> (x,p)@ creates a fresh permutation parser
-- consisting of parser @p@. The final result of the permutation
-- parser is the function @f@ applied to the return value of @p@. The
-- parser @p@ is optional - if it can not be applied, the default value
-- @x@ will be used instead.

(<$?>) :: Functor m => (a -> b) -> (a, m a) -> Permutation m b
(<$?>) f (x,p) = newPermutation f <|?> (x,p)
{-# INLINE (<$?>) #-}

----------------------------------------------------------------
-- The permutation tree
----------------------------------------------------------------

-- | The type @Permutation m a@ denotes a permutation parser that,
-- when converted by the 'permute' function, parses
-- using the base parsing monad @m@ and returns a value of
-- type @a@ on success.
--
-- Normally, a permutation parser is first build with special operators
-- like ('<||>') and than transformed into a normal parser
-- using 'permute'.

data Permutation m a = Permutation (Maybe a) [Branch m a]

instance Functor m => Functor (Permutation m) where
  fmap f (Permutation x xs) = Permutation (fmap f x) (fmap f <$> xs)

data Branch m a = forall b. Branch (Permutation m (b -> a)) (m b)

instance Functor m => Functor (Branch m) where
  fmap f (Branch perm p) = Branch (fmap (f.) perm) p

-- | The parser @permute perm@ parses a permutation of parser described
-- by @perm@. For example, suppose we want to parse a permutation of:
-- an optional string of @a@'s, the character @b@ and an optional @c@.
-- This can be described by:
--
-- >  test  = permute (tuple <$?> ("",some (char 'a'))
-- >                         <||> char 'b'
-- >                         <|?> ('_',char 'c'))
-- >        where
-- >          tuple a b c  = (a,b,c)

-- transform a permutation tree into a normal parser
permute :: forall m a. Alternative m => Permutation m a -> m a
permute (Permutation def xs)
  = F.asum (map branch xs ++ e)
  where
    e :: [m a]
    e = maybe [] (pure . pure) def
    branch (Branch perm p) = flip id <$> p <*> permute perm

-- build permutation trees
newPermutation :: (a -> b) -> Permutation m (a -> b)
newPermutation f = Permutation (Just f) []
{-# INLINE newPermutation #-}

add :: Functor m => Permutation m (a -> b) -> m a -> Permutation m b
add perm@(Permutation _mf fs) p
  = Permutation Nothing (first:map insert fs)
  where
    first = Branch perm p
    insert (Branch perm' p')
            = Branch (add (fmap flip perm') p) p'

addOpt :: Functor m => Permutation m (a -> b) -> a -> m a -> Permutation m b
addOpt perm@(Permutation mf fs) x p
  = Permutation (fmap ($ x) mf) (first:map insert fs)
  where
    first = Branch perm p
    insert (Branch perm' p') = Branch (addOpt (fmap flip perm') x p) p'
