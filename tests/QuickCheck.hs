{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Main
( main
) where

import Control.Applicative

import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString.Char8 as B8

#if MIN_VERSION_base(4,7,0)
import Data.Either
#endif

import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Text.Parsec.Prim as P (parse)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.ParserCombinators.ReadP (readP_to_S)

import System.Exit

-- -------------------------------------------------------------------------- --
-- Run tests with different parser frameworks

-- Instead of letting quick check pick the parser framework as a test parameter
-- it may be better to just run all tests for each parser framework.

newtype P a = P (forall m. (Monad m, CharParsing m) => m a)

data TestParser a = TestParser String (P a -> String -> Either String a)

instance Show (TestParser a) where show (TestParser n _) = n

pAtto, pParsec, pReadP :: TestParser a
pAtto = TestParser "attoparsec" $ \(P p) -> parseOnly p . B8.pack
pParsec = TestParser "parsec" $ \(P p) -> either (Left . show) Right . parse p "test input"
pReadP = TestParser "ReadP" $ \(P p) s -> case readP_to_S p s of
  [] -> Left "parseFailed"
  (a,_):_ -> Right a

instance Arbitrary (TestParser a) where
    arbitrary = elements [pReadP, pAtto, pParsec]

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

-- -------------------------------------------------------------------------- --
-- Utils

#if !MIN_VERSION_base(4,7,0)
isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

isRight :: Either a b -> Bool
isRight = either (const False) (const True)
#endif
