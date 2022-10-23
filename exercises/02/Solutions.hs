{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

module Solutions where

-- RPS - enum example
data RPS
  = Rock
  | Scissors
  | Paper
  deriving (Show)

-- show ignore pattern match
-- pattern evaluation order
beats :: RPS -> RPS -> Bool
beats Rock Scissors = True
beats Scissors Paper = True
beats Paper Rock = True
beats _ _ = False

data Nat
  = Zero
  | Succ Nat
  deriving (Show)

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n =
  Succ (integerToNat (n - 1))

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n') =
  1 + natToInteger n'

addNat :: Nat -> Nat -> Nat
addNat Zero m = m
addNat (Succ n) m =
  Succ (addNat n m)

-- TASK
-- define what the "next" throw you can do is in the "usual" ordering of RPS
-- i.e. @next x@ should be the throw that beats x
next :: RPS -> RPS
next Rock = Paper
next Scissors = Rock
next Paper = Scissors

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
eqRPS Rock Rock = True
eqRPS Scissors Scissors = True
eqRPS Paper Paper = True
eqRPS _ _ = False

-- TASK
-- define a shorter version of beats by uisng next and eqRPS
-- EXAMPLES
-- >>> beats' Rock Paper
-- False
-- >>> beats' Rock Scissors
-- True
-- >>> Paper `beats'` Scissors
-- False
beats' :: RPS -> RPS -> Bool
beats' x y = next y `eqRPS` x

-- TASK
-- Your task is to model a few of the pieces of the game of Belote

-- * implement a data type for Ranks (7 8 9 10 J etc)

data Rank = R7 | R8 | R9 | R10 | J | Q | K | A
  deriving (Show)

-- * implement a data type for Suits

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Show)

eqSuit :: Suit -> Suit -> Bool
eqSuit Clubs Clubs = True
eqSuit Diamonds Diamonds = True
eqSuit Hearts Hearts = True
eqSuit Spades Spades = True
eqSuit _ _ = False

-- * implement a data type for a Card

data Card = MkCard
  { cardRank :: Rank,
    cardSuit :: Suit
  }
  deriving (Show)

-- * implement a data type for Contracts (all trump, no trump etc)

data Contract = SuitContract Suit | NoTrump | AllTrump
  deriving (Show)

-- Given a Card and a Contract, implement a check whether the card is of a trump suit
isTrump :: Contract -> Card -> Bool
isTrump NoTrump _ = False
isTrump AllTrump _ = True
-- requires NamedFieldPuns
isTrump (SuitContract contractSuit) MkCard {cardSuit} = contractSuit `eqSuit` cardSuit

-- Given a Card and a Contract, implement what the "power" of that card is as an Integer
-- When played, a card with higher power will "beat" one with lower power
-- You can use whatever numbers you like, as long as they reflect the rules of the game.
cardPower :: Contract -> Card -> Integer
cardPower contract card@MkCard {cardRank} =
  if isTrump contract card
    then trumpRankPower cardRank
    else noTrumpRankPower cardRank
  where
    noTrumpRankPower :: Rank -> Integer
    noTrumpRankPower R7 = 0
    noTrumpRankPower R8 = 1
    noTrumpRankPower R9 = 2
    noTrumpRankPower J = 3
    noTrumpRankPower Q = 4
    noTrumpRankPower K = 5
    noTrumpRankPower R10 = 6
    noTrumpRankPower A = 7

    trumpRankPower :: Rank -> Integer
    trumpRankPower R7 = 10
    trumpRankPower R8 = 11
    trumpRankPower Q = 12
    trumpRankPower K = 13
    trumpRankPower R10 = 14
    trumpRankPower A = 15
    trumpRankPower R9 = 16
    trumpRankPower J = 17

-- Given two Cards and a Contract, return the Card that would "win" (according to their power)
-- when playing under the given Contract
-- Assume that the first matched card is played first.
fight :: Contract -> Card -> Card -> Card
fight contract firstCard secondCard =
  let -- stay tuned ^^
      maxPowerCard = maxOn (cardPower contract) firstCard secondCard
   in case (matchCardSuits contract firstCard secondCard, contract) of
        (SameSuit, AllTrump) -> maxPowerCard
        (_, AllTrump) -> firstCard
        (SameSuit, NoTrump) -> maxPowerCard
        (_, NoTrump) -> firstCard
        (SameSuit, SuitContract _) -> maxPowerCard
        (NoTrumps, SuitContract _) -> firstCard
        (OneTrump card, SuitContract _) -> card
  where
    maxOn :: Ord b => (a -> b) -> a -> a -> a
    maxOn f x y =
      if f x < f y
        then y
        else x

data MatchCardSuitsResult = SameSuit | NoTrumps | OneTrump Card

matchCardSuits :: Contract -> Card -> Card -> MatchCardSuitsResult
matchCardSuits contract firstCard secondCard
  -- stay tuned ^^
  | firstCard `sameSuit` secondCard = SameSuit
  | isTrump contract firstCard = OneTrump firstCard
  | isTrump contract secondCard = OneTrump secondCard
  | otherwise = NoTrumps

sameSuit :: Card -> Card -> Bool
-- stay tuned ^^
sameSuit = eqSuit `on` cardSuit
  where
    on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
    on p f x y = p (f x) (f y)

-- TASK
-- multiply two @Nat@s recursively, much like we did with Ints last time
-- EXAMPLES
-- >>> multNat Zero (Succ (Succ (Succ Zero)))
-- Zero
-- >>> multNat (integerToNat 2) (integerToNat 3)
-- Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
multNat :: Nat -> Nat -> Nat
multNat Zero _ = Zero
multNat (Succ n) m = addNat m (multNat n m)

-- TASK
-- calculate the larger of two @Nat@s recursively
-- EXAMPLES
-- >>> maxNat (Succ Zero) Zero
-- Succ Zero
-- >>> maxNat (Succ (Succ Zero)) Zero
-- Succ (Succ Zero)
-- >>> maxNat (Succ (Succ Zero)) (Succ (Succ (Succ (Succ Zero))))
-- Succ (Succ (Succ (Succ Zero)))
maxNat :: Nat -> Nat -> Nat
maxNat Zero m = m
maxNat n Zero = n
maxNat (Succ n) (Succ m) = Succ $ maxNat n m

-- TASK
-- Ordering is a datatype that is made to mean "the result of a comparison" or "the ordering between two things"
-- it's defined like so:
-- @data Ordering = LT | EQ | GT@
-- with the constructors being L(ess)T(han), EQ(ual) G(reater)T(han)
-- implement a comparison for @Nat@s, returning an @Ordering@
-- EXAMPLES
-- >>> compareNat (Succ Zero) (Succ Zero)
-- EQ
-- >>> compareNat Zero (Succ Zero)
-- LT
-- >>> compareNat (Succ Zero) Zero
-- GT
compareNat :: Nat -> Nat -> Ordering
compareNat Zero Zero = EQ
compareNat Zero (Succ _) = GT
compareNat (Succ _) Zero = LT
compareNat (Succ n) (Succ m) = compareNat n m

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
  | If Expr Expr Expr
  deriving (Show)

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
eval (Val n) = n
eval (Plus ex1 ex2) = eval ex1 + eval ex2
eval (Mult ex1 ex2) = eval ex1 * eval ex2
eval (If cex tex eex) = if eval cex /= 0 then eval tex else eval eex

-- TASK
-- add an If expression to our Expr language
-- by using other calculations for our "condition value"
-- extend eval so that it also works with the new If construction
-- interpreting 0 as "false" and any other value as "true"

-- TASK
-- add a name to the Animal type

-- Author's notes:
-- the key bit is to put the name "outside" the Animal type, which we've now renamed to AnimalType
-- because it would be common to both the previous constructors, so we can take care to not repeat it instead.
data Animal = MkAnimal
  { animalName :: String,
    animalType :: AnimalType
  }

data AnimalType
  = Dog Breed
  | Cat Colour
  deriving (Show)

data Colour = Orange | Black
  deriving (Show)

data Breed = Labrador | Husky | Borzoi
  deriving (Show)

showAnimalType :: AnimalType -> String
showAnimalType (Dog Borzoi) = "weird"
showAnimalType (Dog d) =
  case d of
    Husky -> "bad boy"
    Labrador -> "very hungry boy"
showAnimalType (Cat Orange) = "lasagna"
showAnimalType (Cat Black) = "amorphous blob"

introduce :: Animal -> String
introduce MkAnimal {animalName, animalType} = "Yo, I'm " ++ animalName ++ " the " ++ showAnimalType animalType

-- is there more than one way to add it to Animal?
-- which way would be more convenient for the implementation of the following function?
-- introduce :: Animal -> String
-- which shows the name of the animal
