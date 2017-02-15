{-# LANGUAGE InstanceSigs #-} -- allow type signatures in instances
{-# LANGUAGE GADTs        #-} -- nice GADT syntax

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Tutorial
Description : A
Copyright   : (c) Michael Klein, 2017
License     : BSD3
Maintainer  : maintainer

Motivation:
Ok so we want to parse stuff. First of all, what does it mean to parse something?
According to Google, "Parse (verb): analyze (a sentence) into its parts and describe their syntactic roles"
So we want to break down, in our case, a string into parts with descriptions.
What are some things we want to cover with our parser?
 - We want to be able to parse small pieces with descriptions
 - We want to be able to combine small parsers into larger parsers
 - If a parser fails, we should be able to try something else

Ok, these are pretty strightforward. Here's a type that models this:

@
 `Parse` object :: `String` -> `Either` `String` (`String`, object)
@

In words, something that parses an @object@ is a function that takes a `String`
and either fails (returns the input) or succeeds and returns the leftover `String` and the @object@.

Quick note: In Haskell, a `String` is a list of `Char`'s, @[`Char`]@, and @`Either` left right = `Left` left | `Right` right@.

A couple examples:

@
"abc" = ['a', 'b', 'c']
`Left` "Hi, I'm 'left'"
`Right` "Hi, I'm 'right'"
@


In Haskell, we have the following definitions:

@
type String = [Char]

data Either l r = Left l | Right r
@

Ok, now that we know what we want, how do we implement it? I use a "newtype" which is just a wrapper type:

-}

module Tutorial (
  Parse(..),
  -- * Classes
  -- $dog_example
  Dog(..),
  myPet,
  Animal(..),
  -- ** Class applications
  -- $class_applications

  -- @@ Combinator classes
  -- $combinator_classes

  -- * Class instances
  -- ** Functor instance
  -- $functor_instance

  -- ** Applicative instance
  -- $applicative_instance
  -- $applicative_parse

  -- ** Monad instance
  -- $monad_instance
  -- $io_is_monad

  -- ** Alternative instance
  -- $alternative_instance
  parse,
  -- $writing_actual_parsers
  charP,
  char,
  digit,
  anyChar,
  notP,
  string,
  elemP,
  (<<),
  space,
  inParens,
  word,
  name,
  classType,
  -- ** Writing a recursive parser
  -- $parse_lisp
  Expr(..),
  parseLit,
  parseSum,
  parseExpr,
  -- $that_was_easy
  eval,
  -- $conclusions
  toParser
                )where

import Control.Applicative (Alternative(..))
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Either ()
import Control.Monad ()

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Control.Monad
import Test.QuickCheck.Modifiers ()

-- | What does this mean? It means "Parse a" has the constructor "Parser",
-- takes a function of that long type, and you can pull out the internal function
-- with the function `runParser`. So if we have a parsing function:
--
-- @
--  iParse :: String -> Either String (String, Stuff)
-- @
--
-- we can do @`Parser` iParse :: `Parse` Stuff@ (the fancy notation "::" can be translated as "has the type").
newtype Parse a = Parser { runParser :: String -> Either String (String, a) }

-- | Dummy instance for testing
instance Show a => Show (Parse a) where
  show (Parser x) = show (x [])

-- | Use `toParser` with an arbitrary function
instance Arbitrary a => Arbitrary (Parse a) where
  arbitrary = toParser <$> arbitrary

-- | Just unwrap the `Parser`
instance EqProp a => EqProp (Parse a) where
  Parser p =-= Parser q = p =-= q

-- | Just use the derived `Eq` instance
instance Eq a => EqProp (Expr a) where
  (=-=) = eq

newtype ShortList a = ShortList { getShortList :: [a] } deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (ShortList a) where
  arbitrary = do
    n <- choose (1, 3)
    ShortList <$> vectorOf n arbitrary
  -- shrink (ShortList xs) = ShortList . getNonEmpty <$> shrink (NonEmpty xs)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Expr a) where
  arbitrary = oneof [ Lit . getNonNegative <$> arbitrary, Sum <$> (fmap getShortList arbitrary) ]

  -- shrink (Lit x ) = Lit <$> shrink x
  -- shrink (Sum xs) = Sum <$> shrink xs

instance (CoArbitrary a) => CoArbitrary (Expr a) where
  coarbitrary (Lit x) = coarbitrary x
  coarbitrary (Sum x) = coarbitrary x

-- | Convert a
--
-- @
--  `String -> (`Bool`, `Int`, a)
-- @
--
-- function into a parser by:
--
--  * If the `Bool` is `False`, consume no input
--  * If the `Bool` is `True`, consume the `Int` characters of input if available, else fail
toParser :: (String -> (Bool, Int, a)) -> Parse a
toParser p = Parser (\s -> case p s of
                            (False, _, _) -> Left s
                            (_    , n, x) -> if 0 <= n && n <= length s
                                                then Right (drop n s, x)
                                                else Left s
                    )




-- $dog_example
-- Ok, now let's make some combinators for our type. In Haskell, type classes are basically facts.
-- Given a fact about an object, you can derive other things.

-- | `Dog`s have names
data Dog = Dog { dogName :: String }

-- | For example, if all animals have a name and Fido is an animal, then Fido has a name
--
-- @
-- myPet = Dog \"Fido\"
-- @
myPet :: Dog
myPet = Dog "Fido"

-- | This is equivalent to defining that all `Animal`s have names
class Animal animal where
  -- | The full type is:
  --
  -- @
  -- getName :: Animal animal => animal -> String
  -- @
  --
  -- Which can be read as: "if @animal@ is an `Animal`, then we can get it's name, which is a `String`"
  getName :: animal -> String

-- | Here, @`getName` = `dogName`@
instance Animal Dog where
  getName = dogName

-- $class_applications
-- Now suppose we want to make a "myFavoriteAnimal" function. Well, we could do the following:
--
-- @
-- myFavoriteDog :: `Dog` -> `String`
-- myFavoriteDog (`Dog` name) = "My favorite animal is named: " `++` name
--
-- myFavoriteBird :: Bird -> `String`
-- ..
-- @
--
-- Or, we can make a "classy" function:
--
-- @
-- myFavoriteAnimal :: `Animal` animal => animal -> `String`
-- myFavoriteAnimal namedAnimal = "My favorite animal is named: " `++` `getName` namedAnimal
-- @
--
-- Whoh, what's that fancy "`Animal` animal => animal" stuff? It means that "animal" must have an instance of the
-- `Animal` class for it to work with the function. In other words, it works for _every_ animal of the `Animal` class.

-- $combinator_classes
-- Now onto combinator classes. One important class is `Functor`, (minimally) defined as so:
--
-- @
-- class `Functor` f where
--   `fmap` :: (a -> b) -> f a -> f b
-- @
--
-- It also has the requirements (Not enforced by Haskell):
--
-- @
--  `fmap` `id` `==` `id`
--  `fmap` (f `.` g) `==` `fmap` f `.` `fmap` g
-- @
--
-- What does this mean? Well `Functor` takes a type @t@ and converts an @a -> b@ function into a @t a -> t b@ one.
--
-- A common example is the `map` function:
--
-- @
--  `map` :: (a -> b) -> [a] -> [b]
-- @
--
-- `map` takes a function from @a@ to @b@ and a list of elements with type @a@. It then applies the function to every element
-- so the resulting elements are of type @b@.
--
-- That's basically what `fmap` does; If you have a way to get from @Type1@ to @Type2@, you can get from a "container" of @Type1@'s to a container of @Type2@'s.
--
-- So what are other "containers"? @(`Either` left)@ is a `Functor`, with
--
-- @
--  `fmap` :: (a -> b) -> `Either` left a -> `Either` left b
-- @
--
-- A fun example: if we have a function
--
-- @
--  infuriateCoder :: Coder -> AngryCoder
-- @
--
-- then @`fmap` infuriateCoder@ can do several things:
--
-- @
--  `fmap` infuriateCoder :: `Either` NonCoder Coder -> `Either` NonCoder AngryCoder
--  -- Only infuriate coders, we'd need another function for NonCoder's
--
--  `fmap` infuriateCoder :: RoomFullOf Coder -> RoomFullOf AngryCoder
--  -- Infuriate everyone in the room, who happen to all be Coders
-- @
--
-- Here's where it starts to get really neat. What if we have a @RoomFullOf (`Either` NonCoder Coder)@ and we want to infuriate all the coders in the room?
-- That's just:
--
-- @
--  `fmap` (`fmap` infuriateCoder) :: RoomFullOf (`Either` NonCoder Coder) -> RoomFullOf (`Either` NonCoder AngryCoder)
-- @
--
-- Whoh! What happened there? Well let's look at the type of `fmap` for @RoomFullOf a@:
--
-- @
--  `fmap` :: (a -> b) -> RoomFullOf a -> RoomFullOf b
-- @
--
-- Ah, so since @`fmap` infuriateCoder@ works on @`Either` NonCoder Coder@, `fmap` can also apply it to everyone in the room.
--
--
-- What about those requirements? What do those mean?
--
-- @
--  `fmap` `id` `==` `id`                   -- means that fmapping the identity to all the objects in "t object" should do nothing
--  `fmap` (f `.` g) `==` `fmap` f `.` `fmap` g -- means that fmapping one function then another is the same as fmapping "both at once"
-- @
--
-- So `Functor` is really useful, it allows us to get inside some type and modify what it contains.
--
-- How can we apply this to a parser and what can it help with?
--
-- Well the type of `fmap` for `Parse` is:
--
-- @
--  `fmap` :: (a -> b) -> `Parse` a -> `Parse` b
-- @
--
-- Here's an example:
--
-- @
--  translateInsult :: EnglishInsult -> GermanInsult
--  parseEnglishInsult :: `Parse` EnglishInsult
--  `fmap` translateInsult parseEnglishInsult :: `Parse` GermanInsult
-- @
--
-- This means that we can take something that works with the results of a parser and inject it into that parser.
--
-- This allows us to easily package the post-processing into our parser. Here's a slightly more practical example:
--
-- @
--  toJSON :: Account -> JSON Account
--
--  parseAccount :: `Parse` Account
--
--  parseAccountJSON :: `Parse` (JSON Account)
--  parseAccountJSON = `fmap` toJSON parseAccount
-- @
--
-- Now we can write @parseAccount@ without having to worry about JSON conversion and convert to JSON without worrying about parsing.
--
-- All good stuff.

instance Functor Parse where
  fmap :: (a -> b) -> Parse a -> Parse b
  fmap f (Parser p) = Parser (fmap (fmap f) . p)

-- $functor_instance
-- Now the actual Functor instance:
--
-- @
-- instance `Functor` `Parse` where
--   `fmap` :: (a -> b) -> `Parse` a -> `Parse` b
--   `fmap` f (`Parser` p) = `Parser` (`fmap` (`fmap` f) . p)
-- @
--
-- To begin to make sense of this, here's what's going on:
--
-- @
--  `Parser` (`fmap` (`fmap` f) . p) :: `Parse` b
--          `fmap` (`fmap` f) . p  :: `String` -> `Either` `String` (`String`, b)
--          `fmap` (`fmap` f)      :: `Either` `String` (`String`, a) -> `Either` `String` (`String`, b)
--                `fmap` f       :: (`String`, a) -> (`String`, b)
--                     f       :: a -> b
-- @
--
--  Now, we want to get to the "a" and apply "f" to it so we:
--
-- @
--    Compose with _: "_ . p", where _ :: Either String (String, a) -> Either String (String, b)
--    fmap _: "fmap _ . p", where _ :: (String, a) -> (String, b)
--    fmap _: "fmap (fmap _) . p", where _ :: a -> b
--    Now we just need to replace '_' with 'f', which has type "a -> b"!
-- @
--
-- Note that `fmap` automatically changes instances twice:
--
-- @
--  `fmap` :: ((`String`, a) -> (`String`, b)) -> Either `String` (`String`, a) -> Either `String` (`String`, b)
--  `fmap` :: (a -> b) -> (`String`, a) -> (`String`, b)
-- @
--
-- It's neat, but all this bouncing around with abstractions can be a bit intense. Don't be afraid to take a break here to breathe.

-- $applicative_instance
-- Now you might be thinking: "ok, ok, but this isn't anything too crazy. I could do that manually without the Functor instance and still be O.K."
-- You're right, and I'm not going to pretend that you should use Haskell or monadic parsers just because you can map over different things.
-- Here's where it starts to get really fun :)
--
-- First a use case: What if you had a list of functions ([a -> b]) and a list of elements's ([a]).
-- Can you apply all the functions to all the elements?
-- Of course you can! Just do [function1 element1, function1 element2, .., function2 element1..]
--
-- Ok so I'm going a lot less in depth for this one. This is called the applicative class and it has two necessary functions:
--
-- @
-- pure :: Applicative f => a -> f a
-- (<*>) Applicative f => f (a -> b) -> f a -> f b
-- @
--
-- Here's another application: you have several parsers and a combinator
--
-- @
-- \f x1          -> f `<$>` x1                      :: `Applicative` f => (                  a1 -> b)                            f a1 -> f b
-- \f x1 x2       -> f `<$>` x1 `<*>` x2               :: `Applicative` f => (            a2 -> a1 -> b)                 -> f a2 -> f a1 -> f b
-- \f x1 x2 x3    -> f `<$>` x1 `<*>` x2 `<*>` x3        :: `Applicative` f => (      a3 -> a2 -> a1 -> b)         -> f a3 -> f a2 -> f a1 -> f b
-- \f x1 x2 x3 x4 -> f `<$>` x1 `<*>` x2 `<*>` x3 `<*>` x4 :: `Applicative` f => (a4 -> a3 -> a2 -> a1 -> b) -> f a4 -> f a3 -> f a2 -> f a1 -> f b
-- @
--
-- Do you see the pattern? `Applicative` allows arbitrary extensions of `Functor`.
--
-- Here's an example use case:
--
-- @
--  (`+`) `<$>` parseInt `<*>` parseInt
-- @
--
-- parses two integers and returns their sum.
--
-- Or
--
-- @
--  (,) `<$>` parseA `<*>` parseB = parseAB
-- @
--
-- parses a tuple of @(A, B)@
--
--
-- If they have the above types and follow these rules, it's a valid instance
--
-- @
-- identity:     pure id <*> v = v
--                           = [id] <*> [1,2,3] = [id 1, id 2, id 3] = [1,2,3]
--
-- homomorphism: pure f <*> pure x = pure (f x)
--   = pure f <*> pure x = [f] <*> [x] = [f x] = pure (f x)
--
-- composition:  pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--
-- interchange:  u <*> pure y = pure ($ y) <*> u
--   = u <*> [y] = [u y] = [($ y)] <*> u = pure ($ y) <*> u
-- @
--
-- @
-- instance Applicative [] where
--     pure :: a -> [a]
--     pure x    = [x]
--
--     (<*>) :: [a -> b] -> [a] -> [b]
--     fs <*> xs = [f x | f <- fs, x <- xs]
-- @

instance Applicative Parse where
  -- (pure x) is a parser that consumes no input and always returns x
  pure :: a -> Parse a
  pure = Parser . (Right .) . flip (,)
  -- pure x = Parser $ \s -> Right (s, x)

  -- If the first parser passes, apply its result to the result of the second parser
  -- E.g. Parser (Word -> Definition) -> Parser Word -> Parser Definition
  -- If the first parser reads a dictionary to get "Word -> Definition" mappings
  --  and the second reads a word, they may be combined to make a parser that
  --  reads a word *and* looks up its definition
  --
  -- That means that if either one fails, you will not get a result
  -- One bug I found made it so when looking up the word failed,
  -- the parser forgot to let you know it wasn't able to use the given word
  --
  -- All the code says is:
  --  Given parser1 that parses a function and parser2 that parses an input:
  --  if parser1 succeeds
  --    then run parser2 and if it succeeds
  --      then return parser1_result_function( parser2_result )
  --      else fail
  --    else fail
  --
  (<*>) :: Parse (a -> b) -> Parse a -> Parse b
  Parser f <*> Parser x = Parser $ combine . f
    where
      combine (Left   s1     ) = Left      s1
      combine (Right (s2, f')) = fmap (fmap f') (x s2)


-- $applicative_parse
-- Now for the actual instance for `Parse`:
--
-- @
-- instance `Applicative` `Parse` where
--   -- @`pure` x@ is a parser that consumes no input and always returns @x@
--   `pure` :: a -> `Parse` a
--   `pure` = `Parser` `.` (`Right` `.`) `.` `flip` (,)
--   -- here's an alternative implementation:
--   -- `pure` x = `Parser` $ \s -> `Right` (s, x)
--
--   -- If the first parser passes, apply its result to the result of the second parser
--   -- E.g. @`Parse` (Word -> Definition) -> `Parse` Word -> `Parse` Definition@
--
--   -- If the first parser reads a dictionary to get @Word -> Definition@ mappings
--   --  and the second reads a word, they may be combined to make a parser that
--   --  reads a word *and* looks up its definition.
--   --
--   -- That means that if either one fails, you will not get a result.
--   -- (One bug I found made it so when looking up the word failed,
--   -- the parser forgot to let you know it wasn't able to use the given word.)
--   --
--   -- All the code says is:
--   --  Given parser1 that parses a function and parser2 that parses an input:
--   --  if parser1 succeeds
--   --    then run parser2 and if it succeeds
--   --      then return parser1_result_function( parser2_result )
--   --      else fail
--   --    else fail
--   --
--   (<*>) :: `Parse` (a -> b) -> `Parse` a -> `Parse` b
--   `Parser` f `<*>` `Parser` x = `Parser` `$` combine `.` f
--     where
--       combine (`Left`   s1     ) = `Left`      s1
--       combine (`Right` (s2, f')) = `fmap` (`fmap` f') (x s2)
-- @



instance Monad Parse where
  -- Still just a parser that returns its argument, no matter what
  return = pure

  -- This is very similar to (<*>)
  -- What it does is runs parser1, then passes the result to the function to get a new parser
  -- E.g. Parser Password -> (Password -> Parser Secret) -> Parser Secret
  -- The first parser reads the password and the second can read a secret
  --  if it has the password. These can be combined to make something that
  --  parses the password and the secret, returning the secret.
  --
  --  This again means that if either fails, you will not get the result.
  --
  --  All the code says is:
  --    Given a parser and a function that can make a new parser out of its result:
  --    If the parser succeeds
  --      then
  --        make a new parser with its result and the function
  --        run the new parser
  --        return the result
  --      else
  --        fail
  --
  (>>=) :: Parse a -> (a -> Parse b) -> Parse b
  Parser x >>= f = Parser $ \s -> x s >>= \(s', y) -> either (const $ Left s) Right (runParser (f y) s')


-- $monad_instance
-- Here's one way to understand the bind function (`>>=`):
--
-- @
-- (`>>=`)    :: t a -> (a -> t b) -> t b
-- passedTo ::   a -> (a ->   b) ->   b
-- x \`passedTo\` f = f x
-- @
--
-- That is, bind is just 'passedTo' where the function result is somehow encapsulated
-- You may not be able to break the encapsulation, but you can often still apply a function.
--
-- Here's an example:
--
-- @
--  (`>>=`) :: `Either` Cat Dog -> (Dog -> `Either` Cat Dog) -> `Either` Cat Dog
-- @
--
-- What happens if we have the following?
--
-- @
-- (`>>=`) (`Left` someCat) :: (Dog -> `Either` Cat Dog) -> `Either` Cat Dog
-- @
--
-- Well, there's no obvious way to convert a Cat into a Dog, so a reasonable default
-- is to skip the @Dog -> Either Cat Dog@ and return @`Left` someCat@.
--
-- In other words, there's likely no good function with the type @`Either` Cat Dog -> Dog@,
-- since we could have a Cat.
--
-- Thus a sane implementation (actually the default definition for Haskell) for (`>>=`) on `Either`
-- is the following:
--
-- @
-- (`>>=`) (`Left`  l) f = `Left`     x
-- (`>>=`) (`Right` r) f = `Right` (f x)
-- @
--
-- Luckily, this is almost all we need to make a `Monad` instance for `Parse`:
--
-- @
-- instance `Monad` `Parse` where
--   -- Still just a parser that returns its argument, no matter what
--   `return` :: a -> `Parse` a
--   `return` = `pure`
--
--   -- This is somewhat similar to (`<*>`)
--   --
--   -- What it does is runs parser1, then passes the result to the function to get a new parser
--   --   E.g. `Parser` Password -> (Password -> `Parser` Secret) -> `Parser` Secret
--   -- The first parser reads the password and the second can read a secret
--   --  if it has the password. These can be combined to make something that
--   --  parses the password and the secret, returning the secret.
--   --
--   --  This again means that if either fails, you will not get the result.
--   --
--   --  All the code says is:
--   --    Given a parser and a function that can make a new parser out of its result:
--   --    If the parser succeeds
--   --      then
--   --        make a new parser with its result and the function
--   --        run the new parser
--   --        return the result
--   --      else
--   --        fail
--   --
--   (`>>=`) :: `Parse` a -> (a -> `Parse` b) -> `Parse` b
--   `Parser` x >>= f = Parser $ \s -> x s >>= \(s', y) -> either (const $ Left s) Right (runParser (f y) s')
-- @


-- $io_is_monad
-- You might now be scratching your head, remembering that the weird `IO` type in Haskell is a monad.
--
-- Without getting too off track, here's a brief explanation:
--
-- The `Monad` instance for functions like @(a -> b)@ has a bind with type:
--
-- @
-- (Think that "t" equals "function that returns b" here)
-- (`>>=`) :: (a -> b) -> (a1 -> a -> b) -> a1 -> b
-- @
--
-- So then a simple implementation is
--
-- @
-- (`>>=`) f g = \x -> g x (f x)
-- @
--
-- This also turns out to be the default implementaion in Haskell.
--
-- Where am I going with this? Well, part of the idea behind `IO` in Haskell is that
-- `IO` actions may be order sensitive, so you need to make sure they're strung up in a row.
--
-- How do you string up a bunch of effects? Why, might as well have them pass something along, to make sure they're sequenced.
-- That's exactly how `IO` is implemented behind the scenes (with slightly different names):
--
-- @
-- data IO a = State RealWorld -> (State RealWorld, a)
-- @
--
-- Some people have said that this isn't pretty or should be hidden from users (it's pretty hard to find in the source).
-- However, I think that with it, `IO` makes perfect sense. You use monads for `IO` because they're an easy way to sequence these
-- sorts of functions. So look at the type of bind for `IO` then translated:
--
-- @
-- (`>>=`) :: `IO` a -> (a -> `IO` b) -> `IO` b
-- (`>>=`) :: (State RealWorld -> (State RealWorld, a)) -> (a -> State RealWorld -> (State RealWorld, b)) -> State RealWorld -> (State RealWorld, b)
-- @
--
-- It's pretty verbose, but compare this to:
--
-- @
-- (`>>=`) :: (s               ->                   a ) -> (a -> s               ->                   b ) -> s               ->                   b
-- @
--
-- The parallels are clear!


-- $alternative_instance
-- Don't forget an alternative instance + explanation!
instance Alternative Parse where
  empty = Parser Left
  Parser x <|> Parser y = Parser (\s -> x s <|> y s)


-- | All you do to run a parser is pull the function out of
-- the 'Parse' type, run it on the string, and drop the leftovers
-- It would be nice to have it return an error, but it would get
-- more messy and this is supposed to be a pretty clean introduction.
--
-- Here's a verbose implementation:
--
-- @
--  `parse` (`Parser` p) s = mapEither onlySecond (p s)
-- @
--
-- or my actual Haskell implementaion:
--
-- @
--  `parse` = `fmap` (`fmap` `snd`) `.` `runParser`
-- @
parse :: Parse a ->String ->Either String a
parse = fmap (fmap snd) . runParser


-- $writing_actual_parsers
-- Ok, so we have a nice background on Functors, Applicatives, Monads, and how they apply to parsing.
-- Now what? I'll begin by defining a single parser "by hand" (without all these nice classes) and show
-- what can be done with it.


-- | Accept any `Char` the predicate passes for
-- Here's how this works: `charP` takes a test or "predicate".
--
-- When the parser is run, if the test passes for the first character in the string
-- then return the character and the rest of the string as leftover.
-- If the test fails or there is no first character, then it fails.
--
-- @
--  `charP` p = `Parser` go
--    where
--      go [] = `Left` []
--      go (c:cs) | p c       = `Right` (cs, c )
--                | `otherwise` = `Left`  (c : cs)
-- @
charP :: (Char ->Bool) ->Parse Char
charP p = Parser go
  where
    go [] = Left []
    go (c:cs) | p c       = Right (cs, c )
              | otherwise = Left  (c : cs)

-- | Match the specified `Char`
--
-- (This is just @`charP` (`==` givenCharacter)@)
--
-- @
--  `char` = `charP` `.` (`==`)
-- @
char :: Char ->Parse Char
char = charP . (==)

-- | Match any digit `Char
--
-- @
--  `digit` = `charP` `isDigit`
-- @
digit :: Parse Char
digit = charP isDigit

-- | Since @`const` `True`@ is a function that always returns `True`,
-- this parses any single character
--
-- @
--  `anyChar` = `charP` (`const` `True`)
-- @
anyChar :: Parse Char
anyChar = charP (const True)


-- | Succeeds only if the provided parser fails
-- (does not consume any input).
-- What this does is run the given parser (@p@):
--
--  If @p@ succeeds, then this parser fails, rewinding the input to before @p@
--
--  If @p@ fails, then this parser succeeds and returns all of the input as leftovers
--
-- Note that we never defined `mapM`. That's because we get it for free by defining `return` and (`>>=`).
--
-- @
-- `notP` p = `Parser` $ `runParser` p `>>=` go
--   where
--     go (`Left` _) s = `Right` (s, `()`)
--     go       _  s = `Left`   s
-- @
notP :: Parse a -> Parse ()
notP p = Parser $ runParser p >>= go
  where
    go (Left _) s = Right (s, ())
    go       _  s = Left   s

-- | `mapM` maps the function and combines all the monads inside.
--
-- For example:
--
-- @
--  `map` `char` ['a', 'b', 'c'] = [`char` 'a', `char` 'b', `char` 'c']
-- @
--
-- This isn't too helpful since now we have to somehow run all the `char`
-- parsers and collect the results. `mapM` does exactly this (remember that @`String` = [`Char`]@).
--
-- @
--  `string` = `mapM` `char`
-- @
string :: String -> Parse String
string = mapM char

-- | Parse any `Char` in the given `String`
elemP :: String -> Parse Char
elemP = charP . flip elem


-- | One more function, this one is also free from `return` and (`>>=`):
--
-- @parseA `>>` parseB@ parses A, but tosses the result before parsing B
--
-- @
-- (>>) :: `Parse` a -> `Parse` b -> `Parse` b
-- @
--
-- This is like (`>>`), but backwards.
--
-- @parseA `<<` parseB@ parses A, then B, but tosses B
--
-- If you want to understand how it works:
--
-- @
-- `liftM2` :: (a -> b -> c) -> `Parse` a -> `Parse` b -> `Parse` c
-- `liftM2` func parser1 parser2 = run parser1, run parser2, return their results combined with func
-- @
--
-- Now @`liftM2` `const`@ makes sense. It runs both parsers, but @`const` a b = a@
-- so it tosses the result of the second parser.
--
-- @
-- (`<<`) = `liftM2` `const`
-- @
(<<) :: Monad m => m a -> m b -> m a
(<<) = liftM2 const

-- | Parse any single space
--
-- @
--  `space` = `charP` `isSpace`
-- @
space :: Parse Char
space = charP isSpace

-- | Isn't this pretty?
--
-- @
--  `inParens` p = `char` '(' `>>` p `<<` `char` ')'
-- @
inParens :: Parse a -> Parse a
inParens p = char '(' >> p << char ')'

-- | Parse a series of alpha characters
--
-- @
--  `word` = `some` `$` `charP` `isAlpha`
-- @
word :: Parse String
word = some $ charP isAlpha

-- | In words: "the input must start with 'Name ', but once we know it's there, just toss it and parse a word"
--
-- @
--  `name` = `string` "Name " `>>` `word`
-- @
name :: Parse String
name = string "Name " >> word


-- | Now let's write a simple parser for something like
--
-- @
--  "Class (Name NamedThing)"
-- @
--
-- where we only want "Class" and "NamedThing":
--
-- @
--  `classType` = do
--    className <- `word`            -- get a word and call it 'className'
--    `many` `space`                   -- get a bunch of spaces, but toss them, they're in the way
--    typeName  <- `inParens` `name`   -- get a name that's in parentheses and call it 'typeName'
--    `return` (className, typeName)  -- return what you've gotten in a nice tuple
-- @
--
-- examples: (almost exact)
--
-- @
-- parse classType "Spoken  (Name Kid)"      = ("Spoken", "Kid")
-- parse classType "Monad   (Name Function)" = ("Monad",  "Function")
-- parse classType "Word1   (Name Word2)"    = ("Word1",  "Word2")
-- @
classType :: Parse (String, String)
classType = do
  className <- word           -- get a word and call it 'className'
  many space                  -- get a bunch of spaces, but toss them, they're in the way
  typeName  <- inParens name  -- get a name that's in parentheses and call it 'typeName'
  return (className, typeName)     -- return what you've gotten in a nice tuple


-- $parse_lisp
-- Ok, so that wasn't bad. However, it's also not too hard to parse something like that in the first place.
-- How about parsing a lisp that only supports addition of natural numbers? Huh, that could be neat.
--
-- Here's what I invision it looking like:
--
-- @
--  1 -> 1
--  (+ 2 2) -> 4
--  (+ (+ 1 2) (+ 3 4)) -> 10
-- @
--
-- For completion sake, we define: @(+ x) -> x@ and make @(+)@ or @()@ a parse error.
--
--
-- First, let's define our expression type:
--
-- This is a GADT, but that's for another time. It could easily be defined as the following:
--
-- @
--  data `Expr` a = `Lit` a | `Sum` [`Expr` a]
-- @
--
-- But I prefer the GADT style:
--
-- @
--  data `Expr` a where
--    `Lit` ::       a  -> `Expr` a
--    `Sum` :: [`Expr` a] -> `Expr` a
-- @

-- | An Expression with type @a@ can be:
data Expr a where
  -- | A numeric literal, e.g. @1337@ or @42@
  Lit ::       a  -> Expr a

  -- | A sum of expressions
  Sum :: [Expr a] -> Expr a
  deriving (Eq, Ord)

-- | A literal is:
--
-- @
--  `parseLit` = do
--    `some` `space`                           -- toss some spaces
--    digits <- `some` `digit`                 -- the literal's digits are just "some digits"
--    `return` `.` `Lit` `.` toNum `.` `read` `$` digits -- return the digits, read and converted to a nice `Num` type, then wrapped in the `Lit` constructor
--      where
--        toNum :: `Num` a => `Int` -> a
--        toNum = `fromInteger` `.` `toEnum`
-- @
parseLit :: Num a => Parse (Expr a)
parseLit = do
  many space
  digits <- some digit
  return . Lit . toNum . read $ digits
    where
      toNum :: Num a => Int -> a
      toNum = fromInteger . toEnum

-- | A sum is:
--
-- @
--  `parseSum` = do
--    `char` \'(\'                     -- starts with an opening parentheses
--    `char` \'+\'                       -- then there's a '+'
--    subExprs <- `some` `parseExpr`   -- then there are some subexpressions, each one is either a literal or a sum
--    `char` \')\'                     -- end with a closing parentheses
--    `return` `$` `Sum` subExprs        -- return the subexpressions wrapped in our `Sum` constructor
-- @
parseSum :: Num a => Parse (Expr a)
parseSum = do
  char '('
  char '+'
  subExprs <- some (parseLit <|> parseSum)
  char ')'
  return $ Sum subExprs

-- | An expression is:
--
-- @
--  `parseExpr` = `parseLit` <|> `parseSum` -- either a literal or a sum
-- @
parseExpr :: Num a => Parse (Expr a)
parseExpr = parseLit <|> parseSum

-- $that_was_easy
-- Well, that was easy. Let's make a quick evaluator:

-- | To evaluate an expression:
--
-- @
--  `eval` (`Lit` x ) = x                  -- we unwrap literals
--  `eval` (`Sum` xs) = `sum` (`fmap` `eval` xs) -- and sum the evaluated subexpressions in a `Sum`
-- @
eval :: Num a => Expr a -> a
eval (Lit x ) = x
eval (Sum xs) = sum (map eval xs)

-- $conclusions
-- Ok, so that was easy too. I'm gonna go off and extend this until it gets hard.
