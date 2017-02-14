{-# LANGUAGE InstanceSigs              #-} -- allow type signatures in instances
{-# LANGUAGE GADTs                     #-} -- nice GADT syntax
{-# LANGUAGE ExistentialQuantification #-} -- Allow SomeParser
{-# LANGUAGE RankNTypes                #-} -- Allow the use of SomeParser
{-# LANGUAGE TemplateHaskell           #-} -- Allows us to collect all tests at the end
{-# LANGUAGE FlexibleInstances         #-}
{-|
Module      : Test.Tutorial
Description : A comprehensive test suite for the parser tutorial
Copyright   : (c) Michael Klein, 2017
License     : BSD3

Motivation:

So you learned all about parsing, but want to be sure it all works?
Welcome to a less beginner-friendly tutorial on testing in Haskell.
-}


module Test.Tutorial where

import Control.Applicative (Alternative(..))
import Control.Monad
import Control.Monad (liftM2)
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Either
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Tutorial
import Data.Maybe


data SomeParser = forall a. (EqProp a, Show a, Arbitrary a, CoArbitrary a) => SomeParser (String, Gen (Parse a))

instance Show SomeParser where
  show (SomeParser (s, _)) = s

instance Testable Char where
  property _ = property True

someReturn :: (EqProp a, Show a, Arbitrary a, CoArbitrary a) => a -> SomeParser
someReturn x = SomeParser ("return: " ++ show x, return <$> arbitrary `asTypeOf` (return x))

rGen :: a -> Gen a
rGen = return

testSomeParser :: (forall a. (EqProp a, Show a, Arbitrary a, CoArbitrary a) => Parse a -> TestBatch) -> SomeParser -> Gen TestBatch
testSomeParser f (SomeParser (s, x)) = f <$> x

testSomeParser2 :: (forall a b. (EqProp a, Show a, Arbitrary a, CoArbitrary a, EqProp b, Show b, Arbitrary b, CoArbitrary b) => Parse (a, b) -> TestBatch) -> SomeParser -> SomeParser -> Gen TestBatch
testSomeParser2 f (SomeParser (s1, x1)) (SomeParser (s2, x2)) = f <$> liftM2 (liftM2 (,)) x1 x2

testSomeParser3 :: (forall a b c. (EqProp a, Show a, Arbitrary a, CoArbitrary a, EqProp b, Show b, Arbitrary b, CoArbitrary b, EqProp c, Show c, Arbitrary c, CoArbitrary c) => Parse (a, b, c) -> TestBatch) -> SomeParser -> SomeParser -> SomeParser -> Gen TestBatch
testSomeParser3 f (SomeParser (s1, x1)) (SomeParser (s2, x2)) (SomeParser (s3, x3)) = f <$> liftM3 (liftM3 (,,)) x1 x2 x3

prop_parseIsFunctor :: SomeParser -> SomeParser -> SomeParser -> Gen TestBatch
prop_parseIsFunctor = testSomeParser3 functor

prop_parseIsApplicative :: SomeParser -> SomeParser -> SomeParser -> Gen TestBatch
prop_parseIsApplicative = testSomeParser3 applicative

prop_parseIsMonad :: SomeParser -> SomeParser -> SomeParser -> Gen TestBatch
prop_parseIsMonad = testSomeParser3 monad

prop_parseIsMonadFunctor :: SomeParser -> SomeParser -> Gen TestBatch
prop_parseIsMonadFunctor = testSomeParser2 monadFunctor

prop_parseIsAlternative :: SomeParser -> Gen TestBatch
prop_parseIsAlternative = testSomeParser alternative


consumes :: Int -> Parse a -> String -> Bool
consumes n (Parser parser) str = case parser str of
  Left rest -> length rest == length str
  Right (remaining, _) -> length str - length remaining == n

-- | Test that a char parser consumes one char if the predicate is true on the first character of the string and 0 otherwise
testCharParser :: (Char -> Bool) -> Parse a -> String -> Bool
testCharParser f p (c:cs) | f c       = consumes 1 p (c:cs)
                          | otherwise = consumes 0 p (c:cs)
testCharParser _ p _                  = consumes 0 p []

prop_charP :: (Char -> Bool) -> String -> Bool
prop_charP p = testCharParser p (charP p)

prop_char :: Char -> String -> Bool
prop_char c = testCharParser (== c) (char c)

prop_digit :: String -> Bool
prop_digit = testCharParser isDigit digit

prop_anyChar :: String -> Bool
prop_anyChar = testCharParser (const True) anyChar

prop_string :: String -> String -> Bool
prop_string str1 str2 | str1 == take (length str1) str2 = consumes (length str1) (string str1) str2
                      | otherwise                      = consumes 0 (string str1) str2

prop_notP :: (Arbitrary a, Show a) => Parse a -> String -> Bool
prop_notP p str | isRight $ parse p str = isLeft  (parse (notP p) str) && consumes 0 (notP p) str
                | otherwise             = isRight (parse (notP p) str) && consumes 0 (notP p) str

prop_elemP :: String -> String -> Bool
prop_elemP str = testCharParser (`elem` str) (elemP str)

prop_space :: String -> Bool
prop_space = testCharParser isSpace space

instance Integral a => Show (Expr a) where
  show (Lit x) = show (toInteger x)
  show (Sum x) = unwords ("(+" : fmap show x) ++ ")"

instance Arbitrary SomeParser where
  arbitrary = aParser


aParser :: Gen SomeParser
aParser = oneof $ [ (\x -> SomeParser ("charP", return $ (charP :: (Char ->Bool) ->Parse Char) x)) <$> arbitrary
                  , (\x -> SomeParser ("char: " ++ show x, return $ (char :: Char ->Parse Char) x)) <$> arbitrary
                  , (\x -> SomeParser ("string: " ++ show x, return $ (string :: String -> Parse String) x)) <$> arbitrary
                  , (\x -> SomeParser ("elemP: " ++ show x, return $ (elemP :: String -> Parse Char) x)) <$> arbitrary
                  ] ++ fmap rGen [ SomeParser ("digit", rGen (digit :: Parse Char))
                  , SomeParser ("anyChar", rGen (anyChar :: Parse Char))
                  , SomeParser ("word", rGen (word :: Parse String))
                  , SomeParser ("name", rGen (name :: Parse String))
                  , SomeParser ("space", rGen (space :: Parse Char))
                  , SomeParser ("classType", rGen (classType :: Parse (String, String)))
                  , SomeParser ("parseLit", rGen (parseLit :: Parse (Expr Int)))
                  , SomeParser ("parseSum", rGen (parseSum :: Parse (Expr Int)))
                  , SomeParser ("parseExpr", rGen (parseExpr :: Parse (Expr Int)))
                  , SomeParser ("someParser", (toParser <$> arbitrary :: Gen (Parse Int)))
                  , SomeParser ("someParser", (toParser <$> arbitrary :: Gen (Parse ())))
                  ]

return []
runTests :: IO Bool
runTests = $quickCheckAll
