{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

#ifdef MIN_VERSION_monoid_subclasses
{-# LANGUAGE DefaultSignatures #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parser.Char
-- Copyright   :  (c) Edward Kmett 2020
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parsers for monoidal input streams
--
-----------------------------------------------------------------------------

module Text.Parser.Input where

import Control.Applicative (Applicative ((<*>), pure), Alternative ((<|>)))
import Control.Monad (void)
import Data.Functor ((<$>))
import qualified Data.List as List
import Data.Monoid (Monoid, mappend, mempty)
import Data.String (IsString (fromString))
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

import Text.Parser.Char (CharParsing)
import Text.Parser.Combinators (count, eof, notFollowedBy, try, unexpected)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)
import qualified Text.Parser.Char as Char

#ifdef MIN_VERSION_monoid_subclasses
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Null as Null
import qualified Data.Monoid.Textual as Textual
import qualified Data.Semigroup.Cancellative as Cancellative
import Data.Monoid.Factorial (FactorialMonoid)
import Data.Monoid.Textual (TextualMonoid)
import Data.Semigroup.Cancellative (LeftReductive)
#endif

#ifdef MIN_VERSION_attoparsec
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Text as Text

import qualified Data.Attoparsec.ByteString as Attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec.Char8
import qualified Data.Attoparsec.Text as Attoparsec.Text
#endif

import Prelude hiding (take, takeWhile)

-- | Methods for parsing factorial monoid inputs
class LookAheadParsing m => InputParsing m where
   type ParserInput m
   -- | Always sucessful parser that returns the remaining input without consuming it.
   getInput :: m (ParserInput m)

   -- | A parser that accepts any single atomic prefix of the input stream.
   -- > anyToken == satisfy (const True)
   -- > anyToken == take 1
   anyToken :: m (ParserInput m)
   -- | A parser that accepts exactly the given number of input atoms.
   take :: Int -> m (ParserInput m)
   -- | A parser that accepts an input atom only if it satisfies the given predicate.
   satisfy :: (ParserInput m -> Bool) -> m (ParserInput m)
   -- | A parser that succeeds exactly when satisfy doesn't, equivalent to
   -- 'Text.Parser.Combinators.notFollowedBy' @. satisfy@
   notSatisfy :: (ParserInput m -> Bool) -> m ()

   -- | A stateful scanner. The predicate modifies a state argument, and each transformed state is passed to successive
   -- invocations of the predicate on each token of the input until one returns 'Nothing' or the input ends.
   --
   -- This parser does not fail.  It will return an empty string if the predicate returns 'Nothing' on the first
   -- character.
   --
   -- /Note/: Because this parser does not fail, do not use it with combinators such as 'many', because such parsers
   -- loop until a failure occurs.  Careless use will thus result in an infinite loop.
   scan :: state -> (state -> ParserInput m -> Maybe state) -> m (ParserInput m)
   -- | A parser that consumes and returns the given prefix of the input.
   string :: ParserInput m -> m (ParserInput m)

   -- | A parser accepting the longest sequence of input atoms that match the given predicate; an optimized version of
   -- 'concatMany . satisfy'.
   --
   -- /Note/: Because this parser does not fail, do not use it with combinators such as 'many', because such parsers
   -- loop until a failure occurs.  Careless use will thus result in an infinite loop.
   takeWhile :: (ParserInput m -> Bool) -> m (ParserInput m)
   -- | A parser accepting the longest non-empty sequence of input atoms that match the given predicate; an optimized
   -- version of 'concatSome . satisfy'.
   takeWhile1 :: (ParserInput m -> Bool) -> m (ParserInput m)
   -- | Zero or more argument occurrences like 'many', with concatenated monoidal results.
   concatMany :: Monoid a => m a -> m a

   anyToken = take 1
   notSatisfy predicate = try (void $ satisfy $ not . predicate) <|> eof
   concatMany p = go
      where go = mappend <$> try p <*> go <|> pure mempty

#ifdef MIN_VERSION_monoid_subclasses
   default string :: (Monad m, LeftReductive (ParserInput m), FactorialMonoid (ParserInput m), Show (ParserInput m))
                  => ParserInput m -> m (ParserInput m)
   string s = do i <- getInput
                 if s `Cancellative.isPrefixOf` i
                    then take (Factorial.length s)
                    else unexpected ("string " <> show s)
   default scan :: (Monad m, FactorialMonoid (ParserInput m)) =>
                   state -> (state -> ParserInput m -> Maybe state) -> m (ParserInput m)
   scan state f = do i <- getInput
                     let (prefix, _suffix, _state) = Factorial.spanMaybe' state f i
                     take (Factorial.length prefix)
   default takeWhile :: (Monad m, FactorialMonoid (ParserInput m)) => (ParserInput m -> Bool) -> m (ParserInput m)
   takeWhile predicate = do i <- getInput
                            take (Factorial.length $ Factorial.takeWhile predicate i)
   default takeWhile1 :: (Monad m, FactorialMonoid (ParserInput m)) => (ParserInput m -> Bool) -> m (ParserInput m)
   takeWhile1 predicate = do x <- takeWhile predicate
                             if Null.null x then unexpected "takeWhile1" else pure x
#endif
   {-# INLINE concatMany #-}


-- | Methods for parsing textual monoid inputs
class (CharParsing m, InputParsing m) => InputCharParsing m where
   -- | Specialization of 'satisfy' on textual inputs, accepting an input character only if it satisfies the given
   -- predicate, and returning the input atom that represents the character. Equivalent to @fmap singleton
   -- . Char.satisfy@
   satisfyCharInput :: (Char -> Bool) -> m (ParserInput m)
   -- | A parser that succeeds exactly when satisfy doesn't, equivalent to @notFollowedBy . Char.satisfy@
   notSatisfyChar :: (Char -> Bool) -> m ()

   -- | Stateful scanner like `scan`, but specialized for 'TextualMonoid' inputs.
   scanChars :: state -> (state -> Char -> Maybe state) -> m (ParserInput m)

   -- | Specialization of 'takeWhile' on 'TextualMonoid' inputs, accepting the longest sequence of input characters that
   -- match the given predicate; an optimized version of @fmap fromString  . many . Char.satisfy@.
   --
   -- /Note/: Because this parser does not fail, do not use it with combinators such as 'many', because such parsers
   -- loop until a failure occurs.  Careless use will thus result in an infinite loop.
   takeCharsWhile :: (Char -> Bool) -> m (ParserInput m)
   -- | Specialization of 'takeWhile1' on 'TextualMonoid' inputs, accepting the longest sequence of input characters
   -- that match the given predicate; an optimized version of @fmap fromString  . some . Char.satisfy@.
   takeCharsWhile1 :: (Char -> Bool) -> m (ParserInput m)

   notSatisfyChar = notFollowedBy . Char.satisfy

#ifdef MIN_VERSION_monoid_subclasses
   default scanChars :: (Monad m, TextualMonoid (ParserInput m)) =>
                        state -> (state -> Char -> Maybe state) -> m (ParserInput m)
   scanChars state f = do i <- getInput
                          let (prefix, _suffix, _state) = Textual.spanMaybe' state (const $ const Nothing) f i
                          take (Factorial.length prefix)
   default takeCharsWhile :: (Monad m, TextualMonoid (ParserInput m)) => (Char -> Bool) -> m (ParserInput m)
   takeCharsWhile predicate = do i <- getInput
                                 take (Factorial.length $ Textual.takeWhile_ False predicate i)
   default takeCharsWhile1 :: (Monad m, TextualMonoid (ParserInput m)) => (Char -> Bool) -> m (ParserInput m)
   takeCharsWhile1 predicate = do x <- takeCharsWhile predicate
                                  if Null.null x then unexpected "takeCharsWhile1" else pure x
#endif

instance InputParsing ReadP where
   type ParserInput ReadP = String
   getInput = ReadP.look
   take n = count n ReadP.get
   anyToken = pure <$> ReadP.get
   satisfy predicate = pure <$> ReadP.satisfy (predicate . pure)
   string = ReadP.string

#ifndef MIN_VERSION_monoid_subclasses
   scan state f = ReadP.readS_to_P scanList
      where scanList l = [(prefix' [], suffix' [])]
               where (prefix', suffix', _, _) = List.foldl' g (id, id, state, True) l
                     g (prefix, suffix, s1, live) x
                        | live, Just s2 <- f s1 [x] = seq s2 (prefix . (x:), id, s2, True)
                        | otherwise = (prefix, suffix . (x:), s1, False)
   takeWhile predicate = ReadP.readS_to_P ((:[]) . List.span (predicate . (:[])))
   takeWhile1 predicate = do x <- takeWhile predicate
                             if List.null x then unexpected "takeWhile1" else pure x
#endif

instance InputCharParsing ReadP where
   satisfyCharInput predicate = pure <$> ReadP.satisfy predicate

#ifndef MIN_VERSION_monoid_subclasses
   scanChars state f = ReadP.readS_to_P scanList
      where scanList l = [(prefix' [], suffix' [])]
               where (prefix', suffix', _, _) = List.foldl' g (id, id, state, True) l
                     g (prefix, suffix, s1, live) c
                        | live, Just s2 <- f s1 c = seq s2 (prefix . (c:), id, s2, True)
                        | otherwise = (prefix, suffix . (c:), s1, False)
   takeCharsWhile predicate = ReadP.readS_to_P ((:[]) . List.span predicate)
   takeCharsWhile1 predicate = do x <- takeCharsWhile predicate
                                  if List.null x then unexpected "takeCharsWhile1" else pure x
#endif

#ifdef MIN_VERSION_attoparsec
instance InputParsing Attoparsec.Parser where
   type ParserInput Attoparsec.Parser = ByteString
   getInput = lookAhead Attoparsec.takeByteString
   anyToken = Attoparsec.take 1
   take = Attoparsec.take
   satisfy predicate = Attoparsec.satisfyWith ByteString.singleton predicate
   string = Attoparsec.string
   takeWhile predicate = Attoparsec.takeWhile (predicate . ByteString.singleton)
   takeWhile1 predicate = Attoparsec.takeWhile1 (predicate . ByteString.singleton)
   scan state f = Attoparsec.scan state f'
      where f' s byte = f s (ByteString.singleton byte)

instance InputCharParsing Attoparsec.Parser where
   satisfyCharInput predicate = ByteString.Char8.singleton <$> Attoparsec.Char8.satisfy predicate
   scanChars = Attoparsec.Char8.scan
   takeCharsWhile = Attoparsec.Char8.takeWhile
   takeCharsWhile1 = Attoparsec.Char8.takeWhile1

instance InputParsing Attoparsec.Text.Parser where
   type ParserInput Attoparsec.Text.Parser = Text
   getInput = lookAhead Attoparsec.Text.takeText
   anyToken = Attoparsec.Text.take 1
   take = Attoparsec.Text.take
   satisfy predicate = Attoparsec.Text.satisfyWith Text.singleton predicate
   string = Attoparsec.Text.string
   takeWhile predicate = Attoparsec.Text.takeWhile (predicate . Text.singleton)
   takeWhile1 predicate = Attoparsec.Text.takeWhile1 (predicate . Text.singleton)
   scan state f = Attoparsec.Text.scan state f'
      where f' s c = f s (Text.singleton c)

instance InputCharParsing Attoparsec.Text.Parser where
   satisfyCharInput predicate = Text.singleton <$> Attoparsec.Text.satisfy predicate
   scanChars = Attoparsec.Text.scan
   takeCharsWhile = Attoparsec.Text.takeWhile
   takeCharsWhile1 = Attoparsec.Text.takeWhile1
#endif
