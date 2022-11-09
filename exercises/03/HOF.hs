{-# LANGUAGE RankNTypes #-}
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

module HOF where

import Prelude hiding (const, ($), curry, uncurry, id, swap, on, log)

-- talk about
-- MatchCardSuitsResult
-- @-bindings
-- guards
--

--x :: Int
--x = x

--f True = False
--f False = True
--
--f x =
--  case x of
--    True -> False
--    False -> true
--
--f = \x -> case x of
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
  deriving Show


-- TODO live
data Tuple a b = MkTuple a b
  deriving Show

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
const = undefined

-- EXERCISE
-- Compose two functions, very useful very often
-- there's a builtin (.) for this - the dot mimics mathematical notation f o g
-- EXAMPLES
-- >>> let f = compose (+3) (*5) in f 4
-- 23
-- >>> let f = compose (*5) (+5) in f 4
-- 45
compose :: (b -> c) -> (a -> b) -> a -> c
compose = undefined

-- EXERCISE
-- Iterate a function f n times over a base value x.
-- EXAMPLES
-- >>> iterateN (+1) 1 10
-- 11
-- >>> iterateN (*2) 1 10
-- 1024
iterateN :: (a -> a) -> a -> Integer -> a
iterateN = undefined

-- EXERCISE
-- Swap the two elements of a tuple
-- EXAMPLES
-- >>> swap $ MkTuple 42 69
-- MkTuple 69 42
swap :: Tuple a b -> Tuple b a
swap = undefined

-- EXERCISE
-- Apply a function to only the first component of a tuple
-- EXAMPLES
-- >>> first (*2) $ MkTuple 21 1337
-- MkTuple 42 1337
first :: (a -> b) -> Tuple a c -> Tuple b c
first = undefined

-- EXERCISE
-- Convert a function operating on a tuple, to one that takes two arguments.
-- Called Curry after Haskell Curry - inventor of lambda calculus.
-- EXAMPLES
-- >>> curryTuple (\(MkTuple x y) -> x * y) 23 3
-- 69
curry :: (Tuple a b -> c) -> a -> b -> c
curry = undefined

-- EXERCISE
-- Convert a function operating on a tuple, to one that takes two arguments.
-- Called Curry after Haskell Curry - inventor of lambda calculus.
-- EXAMPLES
-- >>> uncurryTuple (\x y -> x + y) $ MkTuple 23 46
-- 69
uncurry :: (a -> b -> c) -> Tuple a b -> c
uncurry = undefined

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
on = undefined

-- EXERCISE
-- Apply two different functions to the two different arguments of a tuple
-- Think about what the type should be.
-- mapTuple :: ???
-- mapTuple = undefined

data Nat
  = Zero
  | Suc Nat
  deriving Show

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
