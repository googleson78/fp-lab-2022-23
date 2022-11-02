{-# LANGUAGE RankNTypes #-}

module Solutions where

import Prelude hiding (const, curry, id, log, on, swap, uncurry, ($))

-- talk about
-- MatchCardSuitsResult
-- @-bindings
-- guards
--

-- x :: Int
-- x = x

-- f True = False
-- f False = True
--
-- f x =
--  case x of
--    True -> False
--    False -> true
--
-- f = \x -> case x of
--            True -> False
--            False -> True
-- f x y = x * 2 + y
-- f = \x y -> x * 2 + y
--
-- \<names> -> <expr>
--

-- bool andBool(bool x, bool y)

-- x -> y -> z -> v
-- x -> (y -> -> (z -> v))

-- ML-like
-- ML
-- metalanguage
andBool :: Bool -> (Bool -> Bool)
andBool True True = True
andBool _ _ = False

apply :: (a -> b) -> a -> b
apply f x = f x

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- log level: WARN ERROR
log :: (String -> IO ()) -> String -> String -> IO ()
log prnt level msg = prnt ("level:" ++ level ++ ";" ++ msg)

logToStdout :: String -> String -> IO ()
logToStdout = log putStrLn

logToFile :: String -> String -> IO ()
logToFile = log (writeFile "pesho")

-- putStrLn          :: String -> IO ()
-- writeFile "pesho" :: String -> IO ()
-- writeFile :: FilePath -> String -> IO ()

-- lambdas, currying, HOF(arguments), add hof example, log hof example, ($), combinator "definition"
-- "game semantics" or "server/client semantics"
-- use holes

-- applyTwice :: (a -> a) -> a -> a
-- id
-- ($) - maybe go back to solutions and start rewriting stuff using ($)?
-- infixr 0 $
-- sections

data IntStringTuple = MkIntStringTuple Int String
  deriving (Show)

-- TODO live
data Tuple a b = MkTuple a b
  deriving (Show)

-- swap :: Tuple a b -> Tuple b a
fstTuple :: Tuple doycho gesho -> doycho
fstTuple (MkTuple x y) = x

xyzzy :: Tuple Int String
xyzzy = MkTuple 123 "asdf"

xyzzy1 :: Tuple Bool ()
xyzzy1 = MkTuple True ()

($) :: (a -> b) -> a -> b
($) f x = f x

-- (&) x f = f x

-- succ $ succ $ succ 10
-- (succ (succ (succ 10)))
-- succ $ (succ $ (succ 10))
-- f (g (h y))
infixr 0 $

-- EXERCISE
-- Take two arguments and return the second.
-- This is called const because if we think of it as a function
-- on one argument x, it returns a function that when called, always returns x
-- It is also practically always used partially applied.
-- EXAMPLES
-- >>> const 5 6
-- 5
-- >>> applyTwice (const 42) 1337
-- 42
const :: a -> b -> a
const x _ = x

-- EXERCISE
-- Compose two functions, very useful very often
-- there's a builtin (.) for this - the dot mimics mathematical notation f o g
-- EXAMPLES
-- >>> let f = compose (+3) (*5) in f 4
-- 23
-- >>> let f = compose (*5) (+5) in f 4
-- 45
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f $ g x

-- EXERCISE
-- Iterate a function f n times over a base value x.
-- EXAMPLES
-- >>> iterateN (+1) 1 10
-- 11
-- >>> iterateN (*2) 1 10
-- 1024
iterateN :: (a -> a) -> a -> Integer -> a
iterateN f x 0 = x
iterateN f x n = iterateN f (f x) (n - 1)

id :: a -> a
id x = x

-- Author's note: Another version. I moved around the args a little so that this works out
-- N.B.
iterateN' :: Integer -> (a -> a) -> a -> a
iterateN' 0 f = id
iterateN' n f = compose f (iterateN' (n - 1) f)

-- EXERCISE
-- Swap the two elements of a tuple
-- EXAMPLES
-- >>> swap $ MkTuple 42 69
-- MkTuple 69 42
swap :: Tuple a b -> Tuple b a
swap (MkTuple x y) = MkTuple y x

-- EXERCISE
-- Apply a function to only the first component of a tuple
-- EXAMPLES
-- >>> first (*2) $ MkTuple 21 1337
-- MkTuple 42 1337
first :: (a -> b) -> Tuple a c -> Tuple b c
first f (MkTuple x y) = MkTuple (f x) y

-- EXERCISE
-- Convert a function operating on a tuple, to one that takes two arguments.
-- Called Curry after Haskell Curry - inventor of lambda calculus.
-- EXAMPLES
-- >>> curryTuple (\(MkTuple x y) -> x * y) 23 3
-- 69
curry :: (Tuple a b -> c) -> a -> b -> c
curry f x y = f $ MkTuple x y

-- EXERCISE
-- Convert a function operating on a tuple, to one that takes two arguments.
-- Called Curry after Haskell Curry - inventor of lambda calculus.
-- EXAMPLES
-- >>> uncurryTuple (\x y -> x + y) $ MkTuple 23 46
-- 69
uncurry :: (a -> b -> c) -> Tuple a b -> c
uncurry f (MkTuple x y) = f x y

-- EXERCISE
-- > p `on` f
-- Implement a combinator that allows you to "preapply" a function f on the arguments of a function p
-- EXAMPLES
-- >>> let maxOnFirst = max `on` fstTuple in maxFirst (MkTuple 1 20) (MkTuple 2 100000)
-- 2
sumTuple :: Tuple Int Int -> Int
sumTuple (MkTuple x y) = x + y

-- >>> let maxOnSum = max `on` sumTuple in maxOnSum (MkTuple 20 39) (MkTuple 12 34)
-- 59
-- You can look at the `fight` from the solutions from last time for good actual usage of the function
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = f x `op` f y

-- EXERCISE
-- Apply two different functions to the two different arguments of a tuple
-- Think about what the type should be.
mapTuple :: (a -> c) -> (b -> d) -> Tuple a b -> Tuple c d
mapTuple f g (MkTuple x y) = MkTuple (f x) (g y)

data Nat
  = Zero
  | Suc Nat
  deriving (Show)

-- EXERCISE
-- Look at addNat and multNat from last time.
--
-- addNat :: Nat -> Nat -> Nat
-- addNat Zero m = m
-- addNat (Succ n) m = Succ (addNat n m)
--
-- multNat :: Nat -> Nat -> Nat
-- multNat Zero _ = Zero
-- multNat (Succ n) m = addNat m (multNat n m)
--
-- They look very similar.
-- Can you implement a general enough higher-order function that you can then use to
-- implement both of them by passing suitable arguments?
-- If your function is "good enough" you should also be able to implement exponentiation using it.
-- coolNat :: ???
-- coolNat = ???
-- Can you also implement the "predecessor" function using it? Yes/no, and why? Please do share
-- predNat :: Nat -> Nat
-- predNat Zero = Zero
-- predNat (Suc n) = n

-- wait, this is almost exactly like iterateN, because it is
coolNat :: a -> (a -> a) -> Nat -> a
coolNat z _ Zero = z
coolNat z s (Suc n) = s $ coolNat z s n

addNat :: Nat -> Nat -> Nat
addNat n m = coolNat m Suc n

multNat :: Nat -> Nat -> Nat
multNat n m = coolNat Zero (addNat m) n

expNat :: Nat -> Nat -> Nat
expNat n = coolNat (Suc Zero) (multNat n)

-- this one would be a bit beyond the reach of our coolNat *if* we didn't have tuples
-- we keep a Bool tag telling us if our "current" 2-tuple was generated by the initial value
-- whenever it was, we skip incrementing the number and turn the tag off, which allows us to skip exactly
-- one Suc
predNat :: Nat -> Nat
predNat = fst . coolNat x f
  where
    x :: (Nat, Bool)
    x = (Zero, True)

    f :: (Nat, Bool) -> (Nat, Bool)
    f (Zero, True) = (Zero, False)
    f (x, False) = (Suc x, False)

-- we can use a similar idea to implement minus, but instead of using a boolean tag, we keep the number of times we must skip the Suc
minusNat :: Nat -> Nat -> Nat
minusNat n m = fst $ coolNat x f n
  where
    x :: (Nat, Nat)
    x = (Zero, m)

    f :: (Nat, Nat) -> (Nat, Nat)
    f (Zero, Suc y) = (Zero, y)
    f (x, Zero) = (Suc x, Zero)

-- >>> minusNat (Suc $ Suc $ Suc $ Suc $ Suc Zero) (Suc $ Suc $ Suc Zero)
-- Suc (Suc Zero)
-- >>> minusNat (Suc $ Suc $ Suc $ Suc $ Suc Zero) Zero
-- Suc (Suc (Suc (Suc (Suc Zero))))
-- >>> minusNat (Suc $ Suc Zero) (Suc $ Suc $ Suc Zero)
-- Zero
