{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
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
    , toPermutation
    , permute
    , (<||>), (<$$>)
    , (<|?>), (<$?>)
    , (<|&>)
    ) where

import Control.Applicative

infixl 1 <||>, <|?>
infixl 2 <$$>, <$?>
infixr 5 <|&>

----------------------------------------------------------------
--  Building a permutation parser
----------------------------------------------------------------

-- | @toPermutation p@ lifts a permutation @p@ into a permutation parser.
-- The parser @p@ is not allowed to accept empty input - use the optional
-- combinator '<|&>' instead. Returns a new permutation parser that parses
-- @p@.
toPermutation :: m a -> Permutation m a
toPermutation p = Permutation $ \y -> add (y id) p
{-# INLINE toPermutation #-}

-- | The expression @x \<|&> p@ creates a permutation parser that parses
-- @p@. If it cannot be applied, the default value @x@ will be used.
(<|&>) :: a -> m a -> Permutation m a
(<|&>) x p = Permutation $ \y -> addOpt (y id) x p
{-# INLINE (<|&>) #-}

-- | The expression @perm \<||> p@ adds parser @p@ to the permutation
-- parser @perm@. The parser @p@ is not allowed to accept empty input -
-- use the optional combinator ('<|?>') instead. Returns a
-- new permutation parser that includes @p@.

(<||>) :: Permutation m (a -> b) -> m a -> Permutation m b
(<||>) perm p = perm <*> toPermutation p
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

(<$$>) :: (a -> b) -> m a -> Permutation m b
(<$$>) f p = fmap f (toPermutation p)
{-# INLINE (<$$>) #-}

-- | The expression @perm \<|?> (x,p)@ adds parser @p@ to the
-- permutation parser @perm@. The parser @p@ is optional - if it can
-- not be applied, the default value @x@ will be used instead. Returns
-- a new permutation parser that includes the optional parser @p@.

(<|?>) :: Permutation m (a -> b) -> (a, m a) -> Permutation m b
(<|?>) perm (x,p) = perm <*> x <|&> p
{-# INLINE (<|?>) #-}

-- | The expression @f \<$?> (x,p)@ creates a fresh permutation parser
-- consisting of parser @p@. The final result of the permutation
-- parser is the function @f@ applied to the return value of @p@. The
-- parser @p@ is optional - if it can not be applied, the default value
-- @x@ will be used instead.

(<$?>) :: (a -> b) -> (a, m a) -> Permutation m b
(<$?>) f (x,p) = fmap f (x <|&> p)
{-# INLINE (<$?>) #-}

----------------------------------------------------------------
-- The permutation tree
----------------------------------------------------------------

-- | The type @Permutation m a@ denotes a permutation parser that,
-- when converted by the 'permute' function, parses
-- using the base parsing monad @m@ and returns a value of
-- type @a@ on success.
--
-- Normally, a permutation parser is first built as an 'Applicative'
-- with functions like 'toPermutation' and ('<|&>') and then transformed
-- into a normal parser using 'permute'.

data Permutation m a = Permutation (
    forall r. (forall t. ((a -> r) -> t) -> Perm m t) -> Perm m r
  )

instance Functor (Permutation m) where
  fmap f (Permutation m) = Permutation $ \y -> m $ \u -> y $ \c -> u $ c . f
  {-# INLINE fmap #-}

instance Applicative (Permutation m) where
  pure a = Permutation $ \y -> y ($ a)
  {-# INLINE pure #-}

  liftA2 f (Permutation ma) (Permutation mb) = Permutation
    $ \y -> mb $ \t -> ma $ \u -> y $ \c -> u $ \a -> t $ c . f a
  {-# INLINE liftA2 #-}

  Permutation mf <*> Permutation ma = Permutation
    $ \y -> ma $ \t -> mf $ \u -> y $ \c -> u $ \f -> t $ c . f
  {-# INLINE (<*>) #-}

-- | A 'Perm' is a tree of permutations.
data Perm m a = Perm !(Maybe a) [Branch m a]

instance Functor (Perm m) where
  fmap f (Perm x xs) = Perm (fmap f x) (fmap f <$> xs)

data Branch m a = forall b. Branch (Perm m (b -> a)) (m b)

instance Functor (Branch m) where
  fmap f (Branch perm p) = Branch (fmap ((.) f) perm) p

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

-- unfold the difference list, build and dismantle the tree
permute :: Alternative m => Permutation m a -> m a
permute (Permutation m) = permute' (m (\c -> Perm (Just (c id)) []))

-- transform a permutation tree into a normal parser
permute' :: Alternative m => Perm m a -> m a
permute' (Perm def xs)
  = foldr ((<|>) . branch) (maybe empty pure def) xs
  where
    branch (Branch perm p) = liftA2 (flip id) p (permute' perm)

-- build permutation trees
add :: Perm m (a -> b) -> m a -> Perm m b
add perm@(Perm _mf fs) p
  = Perm Nothing (first:map insert fs)
  where
    first = Branch perm p
    insert (Branch perm' p')
            = Branch (add (fmap flip perm') p) p'

addOpt :: Perm m (a -> b) -> a -> m a -> Perm m b
addOpt perm@(Perm mf fs) x p
  = Perm (fmap ($ x) mf) (first:map insert fs)
  where
    first = Branch perm p
    insert (Branch perm' p') = Branch (addOpt (fmap flip perm') x p) p'
