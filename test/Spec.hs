module Spec where

{-# LANGUAGE InstanceSigs #-} -- allow type signatures in instances
{-# LANGUAGE GADTs        #-} -- nice GADT syntax

{-|
Module      : Tutorial
Description : A
Copyright   : (c) Michael Klein, 2017
License     : BSD3
Maintainer  : maintainer

Motivation:
-}

module Spec where

import Control.Applicative (Alternative(..))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Either (isLeft)
import Control.Monad (liftM2)

-- newtype Parse a = Parser { runParser :: String -> Either String (String, a) }

-- -- | An Expression with type @a@ can be:
-- data Expr a where
--   Lit ::       a  -> Expr a
--   Sum :: [Expr a] -> Expr a

-- use inverses for this one

-- parseLit :: Num a => Parse (Expr a)
-- parseLit = do
--   some space
--   digits <- some digit
--   return . Lit . toNum . read $ digits
--     where
--       toNum :: Num a => Int -> a
--       toNum = fromInteger . toEnum

-- parseSum :: Num a => Parse (Expr a)
-- parseSum = do
--   char '('
--   char '+'
--   subExprs <- some (parseLit <|> parseSum)
--   char ')'
--   return $ Sum subExprs

-- parseExpr :: Num a => Parse (Expr a)
-- parseExpr = parseLit <|> parseSum

-- eval :: Num a => Expr a -> a
-- eval (Lit x ) = x
-- eval (Sum xs) = sum (map eval xs)

tests = [Test]
tests = []
