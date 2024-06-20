{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Main
( main
) where

import Control.Applicative

#ifdef MIN_VERSION_attoparsec
import Data.Attoparsec.Text (parseOnly)
#endif
import Data.String

#if MIN_VERSION_base(4,7,0)
import Data.Either
#endif

import Test.QuickCheck
import Test.QuickCheck.Instances ()

#ifdef MIN_VERSION_parsec
import Text.Parsec.Prim as P (parse)
#endif
import Text.Parser.Char
import Text.Parser.Combinators
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.ParserCombinators.ReadPrec (readPrec_to_S)

import System.Exit

-- -------------------------------------------------------------------------- --
-- Run tests with different parser frameworks

-- Instead of letting quick check pick the parser framework as a test parameter
-- it may be better to just run all tests for each parser framework.

newtype P a = P (forall m. (Monad m, CharParsing m) => m a)

data TestParser a = TestParser String (P a -> String -> Either String a)

instance Show (TestParser a) where show (TestParser n _) = n

#ifdef MIN_VERSION_attoparsec
pAtto :: TestParser a
pAtto = TestParser "attoparsec" $ \(P p) -> parseOnly p . fromString
#endif

#ifdef MIN_VERSION_parsec
pParsec :: TestParser a
pParsec = TestParser "parsec" $ \(P p) -> either (Left . show) Right . parse p "test input"
#endif

pReadP :: TestParser a
pReadP = TestParser "ReadP" $ \(P p) s -> case readP_to_S p s of
  [] -> Left "parseFailed"
  (a,_):_ -> Right a

pReadPrec :: TestParser a
pReadPrec = TestParser "ReadPrec" $ \(P p) s -> case readPrec_to_S p 0 s of
  [] -> Left "parseFailed"
  (a,_):_ -> Right a

instance Arbitrary (TestParser a) where
    arbitrary = elements ps
        where
            ps = [pReadP, pReadPrec]
#ifdef MIN_VERSION_attoparsec
              ++ [pAtto]
#endif
#ifdef MIN_VERSION_parsec
              ++ [pParsec]
#endif

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = mapM quickCheckResult tests >>= \x -> case filter (not . passed) x of
    [] -> exitSuccess
    _ -> exitFailure
  where
    passed Success{} = True
    passed _ = False

-- -------------------------------------------------------------------------- --
-- Tests

tests :: [Property]
tests =
    [ property prop_notFollowedBy0
    , property prop_notFollowedBy1
    , property prop_notFollowedBy2
    , property prop_notFollowedBy3
    , property prop_spaces
    , property prop_spaces2
    ]

-- -------------------------------------------------------------------------- --
-- Properties

prop_notFollowedBy0 :: TestParser Char -> Char -> Char -> Bool
prop_notFollowedBy0 (TestParser _ p) x y = either (\_ -> x == y) (/= y)
    $ p (P (notFollowedBy (char y) *> anyChar)) [x]

prop_notFollowedBy1 :: TestParser Char -> Char -> Bool
prop_notFollowedBy1 (TestParser _ p) x = either (\_ -> x == x) (/= x)
    $ p (P (notFollowedBy (char x) *> anyChar)) [x]

prop_notFollowedBy2 :: TestParser Char -> String -> Char -> Bool
prop_notFollowedBy2 (TestParser _ p) x y = isLeft
    $ p (P (anyChar *> notFollowedBy (char y) *> char y)) x

prop_notFollowedBy3 :: TestParser () -> Char -> Bool
prop_notFollowedBy3 (TestParser _ p) x = isRight
    $ p (P (notFollowedBy (char x) <|> char x *> pure ())) [x]

prop_spaces :: TestParser () -> Int -> Bool
prop_spaces (TestParser _ p) nspaces = isRight
    $ p (P (spaces *> eof)) (replicate nspaces ' ')

prop_spaces2 :: TestParser () -> Int -> Bool
prop_spaces2 (TestParser _ p) nspaces = isRight
    $ p (P (spaces *> char 'p' *> pure ())) (replicate nspaces ' ' ++ "p")

-- -------------------------------------------------------------------------- --
-- Utils

#if !MIN_VERSION_base(4,7,0)
isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

isRight :: Either a b -> Bool
isRight = either (const False) (const True)
#endif
