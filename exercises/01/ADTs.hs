{-# LANGUAGE EmptyDataDeriving #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

-- warnings

module ADTs where

-- TODO: talk about
-- * pls write on teams+join teams
-- * first homework coming soon :)
-- * waiting to get timetable?

-- show:
-- * pragmas on the top of files
-- * remind about sections

-- pattern matching

-- struct Point {
--   int x;
--   int y;
-- };
-- Point pt;
-- pt.x
-- pt.y
-- Point{3, 5}

-- analogue with C structs or something
-- example point
-- mention constructor name requirements, Mk convention
data Point = MkPoint Float Float
-- data <name1> = <name2> ...<field>....
  deriving Show

isInFirstQuadrant :: Point -> Bool
isInFirstQuadrant (MkPoint pesho gosho) =
  pesho > 0 && gosho > 0
  -- (&&) ((>) pesho 0) ((>) gosho 0)

-- isInFirstQuadrant (MkPoint 5.0 3.0)

invert :: Point -> Point
invert pt =
  case pt of
    MkPoint x y -> MkPoint (negate x) (negate y)


data MyBool
  = MyTrue
  | MyFalse
  deriving Show

myNot :: MyBool -> MyBool
--myNot b =
--  case b of
--    MyFalse -> MyTrue
--    MyTrue -> MyFalse
myNot MyFalse = MyTrue
myNot MyTrue = MyFalse

-- show Bool
-- show case here?
-- deriving Show

-- RPS - enum example
data RPS
  = Rock
  | Scissors
  | Paper
  deriving Show

-- show ignore pattern match
-- pattern evaluation order
beats :: RPS -> RPS -> Bool
beats Rock Scissors = True
beats Scissors Paper = True
beats Paper Rock = True
beats _ _ = False

-- cats n dogs
-- colours n breeds
data Animal
  = Dog Breed
  | Cat Colour
  deriving Show

data Colour = Orange | Black
  deriving Show
data Breed = Labrador | Husky | Borzoi
  deriving Show

showAnimal :: Animal -> String
showAnimal (Dog Borzoi) = "weird"
showAnimal (Dog d) =
 case d of
   Husky -> "bad boy"
   Labrador -> "very hungry boy"
showAnimal (Cat Orange) = "lasagna"
showAnimal (Cat Black) = "amorphous blob"

-- showAnimal (Dog Husky)
--
-- case Husky of
--   Husky -> "bad boy"
--   Labrador -> "very hungry boy"
--
-- "bad boy"


-- example for animal value
-- example with animal matching
-- showAnimal

-- explain the encoding (peano)
-- Zero ~ 0
-- Succ Zero ~ 1
-- Succ (Succ Zero) ~ 2
-- Succ (Succ (Succ Zero)) ~ 2
--
-- data Point = MkPoint Float Float
data Nat
  = Zero
  | Succ Nat
  -- successor
  deriving Show

-- implement
integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n =
  Succ (integerToNat (n - 1))


-- implement
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n') =
  1 + natToInteger n'

-- natToInteger (Succ (Succ (Succ Zero)))
-- 1 + (natToInteger (Succ (Succ Zero)))
-- 1 + 1 + (natToInteger (Succ Zero))
-- 1 + 1 + 1 + (natToInteger Zero)
-- 1 + 1 + 1 + 0
-- 3

-- evaluate manually?

-- implement
addNat :: Nat -> Nat -> Nat
addNat Zero m = m
addNat (Succ n) m =
  Succ (addNat n m)

-- addNat (Succ (Succ Zero)) (Succ Zero)
-- Succ (addNat (Succ Zero) (Succ Zero))
-- Succ (Succ (addNat Zero (Succ Zero)))
-- Succ (Succ (Succ Zero))

  --addNat n (Succ m)
-- addNat (Succ (Succ Zero)) (Succ Zero)
-- n == Succ Zero; m == Succ Zero
-- addNat n (Succ m)
-- addNat (Succ Zero) (Succ (Succ m))
-- addNat Zero (Succ (Succ (Succ m)))
-- Succ (Succ (Succ m))
-- addNat (Succ (Succ Zero)) (Succ Zero)

-- TASK
-- define what the "next" throw you can do is in the "usual" ordering of RPS
-- i.e. @next x@ should be the throw that beats x
next :: RPS -> RPS
next = undefined

-- TASK
-- define what it means for two RPS values to be equal
-- in general for a type, this would mean that the constructors must be equal
-- and all their contents should all so be equal (pointwise)
-- for an "enum" in particular, this only leaves the constructor check
-- use _ matches!
-- EXAMPLES
-- >>> eqRPS Rock Rock
-- True
-- >>> eqRPS Rock Paper
-- False
eqRPS :: RPS -> RPS -> Bool
eqRPS = undefined

-- TASK
-- define a shorter version of beats by uisng next and eqRPS
-- EXAMPLES
-- >>> beats' Rock Paper
-- True
-- >>> beats' Rock Scissors
-- False
-- >>> beats' Paper Scissors
-- True
beats' :: RPS -> RPS -> Bool
beats' = undefined

-- TASK
-- Your task is to model a few of the pieces of the game of Belote
-- * implement a data type for Ranks (7 8 9 10 J etc)
data Rank
  deriving Show
-- * implement a data type for Suits
data Suit
  deriving Show
-- * implement a data type for a Card
data Card
  deriving Show
-- * implement a data type for Contracts (all trump, no trump etc)
data Contract
  deriving Show

-- Given a Card and a Contract, implement a check whether the card is of a trump suit
isTrump :: Contract -> Card -> Bool
isTrump = undefined

-- Given a Card and a Contract, implement what the "power" of that card is as an Integer
-- When played, a card with higher power will "beat" one with lower power
-- You can use whatever numbers you like, as long as they reflect the rules of the game.
cardPower :: Contract -> Card -> Integer
cardPower = undefined

-- Given two Cards and a Contract, return the Card that would "win" (according to their power)
-- when playing under the given Contract
-- Assume that the first matched card is played first.
fight :: Contract -> Card -> Card -> Card
fight = undefined

-- TASK
-- multiply two @Nat@s recursively, much like we did with Ints last time
-- EXAMPLES
-- >>> multNat Zero (Suc (Suc (Suc Zero)))
-- Zero
-- >>> multNat (integerToNat 2) (integerToNat 3)
-- Suc (Suc (Suc (Suc (Suc (Suc Zero)))))
multNat :: Nat -> Nat -> Nat
multNat = undefined

-- TASK
-- calculate the larger of two @Nat@s recursively
-- EXAMPLES
-- >>> maxNat (Suc Zero) Zero
-- Suc Zero
-- >>> maxNat (Suc (Suc Zero)) Zero
-- Suc (Suc Zero)
-- >>> maxNat (Suc (Suc Zero)) (Suc (Suc (Suc (Suc Zero))))
-- Suc (Suc (Suc (Suc Zero)))
maxNat :: Nat -> Nat -> Nat
maxNat = undefined

-- TASK
-- Ordering is a datatype that is made to mean "the result of a comparison" or "the ordering between two things"
-- it's defined like so:
-- @data Ordering = LT | EQ | GT@
-- with the constructors being L(ess)T(han), EQ(ual) G(reater)T(han)
-- implement a comparison for @Nat@s, returning an @Ordering@
-- EXAMPLES
-- >>> compareNat (Suc Zero) (Suc Zero)
-- EQ
-- >>> compareNat Zero (Suc Zero)
-- LT
-- >>> compareNat (Suc Zero) Zero
-- GT
compareNat :: Nat -> Nat -> Ordering
compareNat = undefined

-- README
-- the "syntax" for a very basic "calculator" datatype
-- or alternatively, a very simple programming language
--
-- we can build up Expr(essions) by
-- * injecting integers as a value directly - Val
-- * stating that we want to add the result of two calculations - Plus
-- * stating that we want to multiply the result of two calculations - Mult
data Expr
  = Val Integer
  | Plus Expr Expr
  | Mult Expr Expr
  deriving Show

-- README - SECTIONS
--
-- Writing
-- Plus (Val 3) (Plus (Val 4) (Val 5))
-- is annoying - a lot of parens
-- we can abuse sections to write "prettier" expressions
-- Val 3 `Plus` Val 4
-- is the same as
-- Plus (Val 3) (Val 4)
-- But what would
-- Val 3 `Plus` Val 4 `Plus` Val 5
-- be?
-- We can use these pragmas
infixr 7 `Plus`
infixr 8 `Mult`
-- infixr(ight)
-- to tell the compiler that when used in a section/as operators
-- Mult has higher priority than Plus, e.g.
-- Val 3 `Plus` Val 4 `Mult` Val 5
-- means
-- Val 3 `Plus` (Val 4 `Mult` Val 5)
-- and
-- Plus and Mult are both right associative, e.g.
-- Val 3 `Plus` Val 4 `Plus` Val 5
-- means
-- Val 3 `Plus` (Val 4 `Plus` Val 5)

-- TASK
-- and now that we have the syntactic structure of the computation we want to make
-- we can implement its semantics by writing an evaluator for our calculator
-- or alternatively an interpreter for our programming language
-- EXAMPLES
-- >>> eval (Val 3)
-- 3
-- >>> eval (Plus (Val 3) (Val 4))
-- 7
-- >>> eval (Val 33 `Mult` Val 36)
-- 1188
-- >>> eval ((Val 3 `Plus` Val 3) `Mult` Val 7)
-- 42
-- >>> eval (Val 3 `Plus` Val 3 `Mult` Val 7)
-- 24
eval :: Expr -> Integer
eval = undefined

-- TASK
-- add an If expression to our Expr language
-- by using other calculations for our "condition value"
-- extend eval so that it also works with the new If construction
-- interpreting 0 as "false" and any other value as "true"

-- TASK
-- add a name to the Animal type
--
-- is there more than one way to add it to Animal?
-- which way would be more convenient for the implementation of the following function?
-- introduce :: Animal -> String
-- which shows the name of the animal
