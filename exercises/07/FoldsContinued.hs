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

module FoldsContinued where

import Prelude hiding (all, and, concat, drop, filter, foldl, foldr, length, map, null, or, product, reverse, subtract, sum, take, zip, zipWith)

-- let me resolve comments and close prs pls :)

-- talk about boolean blindness example from homework

-- fromMaybe :: Spot -> Marker
-- fromMaybe (Just value) = value
--
-- fromMaybe Nothing = X
--
--
-- checkThreeSpots :: Thrice Spot -> Result
-- checkThreeSpots t =
--   case (t Zero, t One, t Two) of
--      (Just x, Just y, Just z)
--        | x == y, y == z -> Wins x
--      ...
-- checkThreeSpots x
--     | x Zero == Nothing || x One == Nothing || x Two == Nothing = HasEmpty
--     | x Zero == x One,
--       x One == x Two,
--       x Two == x Zero
--          = Wins (fromMaybe (x Zero))
--     | otherwise = Full

--
-- foldr as "abstracting recursion"
--

-- minusFrom 5 [1,2,3]
-- foldr (-) 5 [1,2,3]
-- (:) 1 ((:) 2 ((:) 3 []))
-- (-) 1 ((-) 2 ((-) 3 5))
-- 1 - (2 - (3 - 5))

lastMaybe' :: [a] -> Maybe a
lastMaybe' [] = Nothing
lastMaybe' [x] = Just x
lastMaybe' (_ : xs) = lastMaybe' xs

-- >>> lastMaybe []
-- >>> lastMaybe [1]
-- >>> lastMaybe [1,2,3]
-- lastMaybe :: [a] -> Maybe a
-- lastMaybe = foldr (\x rec -> rec) Nothing

-- lastMaybe :: [a] -> Maybe a
minusFrom :: Integer -> [Integer] -> Integer
minusFrom m [] = m
minusFrom m (n : ns) =
  minusFrom (m - n) ns

sumImper :: [Integer] -> Integer
sumImper = go 0
  where
    go acc [] = acc
    go acc (n : ns) = go (n + acc) ns

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl op acc (x : xs) = foldl op (op acc x) xs

sumImper' = foldl (+) 0

-- foldl (+) 0 [1,2,3] ->
-- foldl (+) ((+) 0 1) [2,3] ->
-- foldl (+) ((+) ((+) 0 1) 2) [3] ->
-- foldl (+) ((+) ((+) ((+) 0 1) 2) 3) [] ->
-- ((+) ((+) ((+) 0 1) 2) 3) ->
-- ((0 + 1) + 2) + 3

-- 1 + (2 + (3 + 0))
-- foldr (+) 0 [1,2,3] ->
-- (+) 1 ((+) 2 ((+) 3 0))

-- write foldl
-- foldl = undefined

-- show how foldl acts, like with foldr:

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v [] = v
foldr f v (x : xs) = f x $ foldr f v xs

-- foldr (\x rec -> _)
-- foldl (\acc x -> _)
--
-- foldr :: (a -> b -> b)
-- foldl :: (b -> a -> b)
--
-- mention "mnemonic" for order of function args on foldr and foldl
-- mention "mnemonics" for function args

--
-- (+) is assoc -> foldl == foldr
--

-- ((0 + 1) + 2) + 3

-- 1 + (2 + (3 + 0))
--

-- sum - both foldl and foldr? why?
-- ditto concat

data Nat
  = Zero
  | Suc Nat
  deriving (Show)

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Suc $ integerToNat $ n - 1

foldNat :: (a -> a) -> a -> Nat -> a
foldNat _ v Zero = v
foldNat f v (Suc n) = f (foldNat f v n)

addNat' :: Nat -> Nat -> Nat
addNat' n m = foldNat Suc m n

-- EXERCISE
-- Implement natToInteger using foldNat.
-- EXAMPLES
-- >>> natToInteger $ Suc $ Suc $ Suc Zero
-- 3
natToInteger :: Nat -> Integer
natToInteger = undefined

-- EXERCISE
-- Implement exponentiation(n ^ m) using foldNat.
-- EXAMPLES
-- >>> natToInteger $ expNat (integerToNat 2) (integerToNat 10)
-- 1024
expNat :: Nat -> Nat -> Nat
expNat n m = undefined

-- EXERCISE
-- Implement and using foldr
-- EXAMPLES
-- >>> and [False]
-- False
-- >>> and [True, True]
-- True
and :: [Bool] -> Bool
and = undefined

-- EXERCISE
-- Implement or using foldr
-- EXAMPLES
-- >>> or [False]
-- False
-- >>> or [True, True]
-- True
or :: [Bool] -> Bool
or = undefined

-- EXERCISE
-- Implement length using foldr
-- EXAMPLES
-- >>> length [1,2,8]
-- 3
-- >>> length []
-- 0
length :: [a] -> Integer
length = undefined

-- EXERCISE
-- Implement concat using foldr
-- >>> concat [[1,2,3], [42,69], [5,7,8,9]]
-- [1,2,3,42,69,5,7,8,9]
-- >>> concat [[1,2,3], [], [5,7,8,9]]
-- [1,2,3,5,7,8,9]
-- >>> concat []
-- []
concat :: [[a]] -> [a]
concat = undefined

-- EXERCISE
-- Implement reverse using foldr (it's fine to do this in O(n^2)
-- EXAMPLES
-- >>> reverse [1,2,3]
-- [3,2,1]
-- >>> reverse []
-- []
reverse :: [a] -> [a]
reverse = undefined

-- EXERCISE
-- Implement map using foldr
-- EXAMPLES
-- >>> map succ [1,2,3]
-- [2,3,4]
-- >>> map (\x -> x * x) [1,2,3] -- same as squareList
-- [1,4,9]
-- >>> map (\x -> (3,x)) [1,2,3] -- same as megaPair 3
-- [(3,1),(3,2),(3,3)]
map :: (a -> b) -> [a] -> [b]
map f = undefined

-- EXERCISE
-- Implement filter using foldr
-- EXAMPLES
-- >>> even 2
-- True
-- >>> even 3
-- False
-- >>> filter even [1..10]
-- [2,4,6,8,10]
-- >>> filter isPrime [1..20]
-- [2,3,5,7,11,13,17,19]
filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

-- EXERCISE
-- Implement null using foldr
-- EXAMPLES
-- >>> null []
-- True
-- >>> null [1]
-- False
null :: [a] -> Bool
null = undefined

-- EXERCISE
-- Implement headMaybe using foldr
-- EXAMPLES
-- >>> headMaybe []
-- Nothing
-- >>> headMaybe [1,2,3]
-- Just 1
headMaybe :: [a] -> Maybe a
headMaybe = undefined

-- EXERCISE
-- Implement a function that splits a list into two based on a predicate p
-- those that satisfy p and those that don't.
-- EXAMPLES
-- >>> partition (<5) [1..10]
-- ([1,2,3,4],[5,6,7,8,9,10])
-- >>> partition even [1..10]
-- ([2,4,6,8,10],[1,3,5,7,9])
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p = undefined

-- EXERCISE
-- Implement partition using foldr
-- EXAMPLES
-- >>> partitionfoldr (<5) [1..10]
-- ([1,2,3,4],[5,6,7,8,9,10])
-- >>> partitionfoldr even [1..10]
-- ([2,4,6,8,10],[1,3,5,7,9])
partitionfoldr :: (a -> Bool) -> [a] -> ([a], [a])
partitionfoldr = undefined

-- EXERCISE
-- Implement validateList using foldr.
-- EXAMPLES
-- >>> validateList []
-- Just []
-- >>> validateList [Just 42, Just 6, Just 9]
-- Just [42,6,9]
-- >>> validateList [Nothing, Just 6, Just 9]
-- Nothing
-- >>> validateList [Just 42, Nothing, Just 9]
-- Nothing
-- >>> validateList [Just 42, Just 6, Nothing]
-- Nothing
validateList :: [Maybe a] -> Maybe [a]
validateList = undefined

-- EXERCISE
-- Reverse a list using foldl.
-- It might help to implement the "accumulating with a helper" version first, if you haven't already
-- What's the complexity for reverse'?
-- >>> reverse' [1,2,3]
-- [3,2,1]
reverse' :: [a] -> [a]
reverse' = undefined

-- EXERCISE
-- A smaller version of one of the tasks from the recent FP exam.
-- We have instructions for a "stack machine" - so something that keeps a stack for memory (so a list)
-- Push n is meant to push the value n on the stack
-- Map f is meant to apply f to *all* of the items in the stack in place, so without removing them
-- Oper f is meant to pop the top two items from the stack, apply f to them, and push the result back on the stack
data Instruction
  = Push Integer
  | Map (Integer -> Integer)
  | Oper (Integer -> Integer -> Integer)

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just $ f x

-- Imlement the interpreter for our stack machine, given a list of instructions.
-- You can use a helper for the "recursion" at first, if you want to, but implement the "recursive part" using foldl afterwards.
-- We return the final state of the stack.
-- Note the Maybe, because we can't be sure that Oper will always succeed. Once we fail, we shouldn't attemp to recover.
-- The maybeMap function is quite useful for some of the cases here.
-- EXAMPLES
-- >>> runMachine [Push 9, Push 6]
-- Just [6,9]
-- >>> runMachine [Oper (+)]
-- Nothing
-- >>> runMachine [Push 42, Oper (+)]
-- Nothing
-- >>> runMachine [Push 42, Push 69, Oper (+)]
-- Just [111]
-- >>> runMachine [Push 42, Oper (+), Push 69]
-- Nothing
-- >>> runMachine [Push 7, Push 2, Map (\x -> x * x), Push 5]
-- Just [5,4,49]
-- >>> runMachine [Push 7, Push 2, Map (\x -> x * x), Push 5, Oper (*), Oper (+)]
-- Just [69]
-- >>> runMachine [Push 7, Push 2, Oper (+), Oper (+)]
-- Nothing
runMachine :: [Instruction] -> Maybe [Integer]
runMachine = undefined
  where
    -- a function to execute one instruction
    go :: Maybe [Integer] -> Instruction -> Maybe [Integer]
    go = undefined

-- EXERCISE
-- Look at the recursor for nats - foldNat. In there we replaced Nats constructors, with things.
-- Again - in foldr, we replace the two constructors for lists, with a function and a value.
-- Think about how a recursor for tuples should look like, and implement it.
-- Reminder: Tuples have one constructor:
-- (,) :: a -> b -> (a, b)
--
--
-- foldTuple :: ?
-- foldTuple = undefined
--
-- Does this function look familiar?

-- EXERCISE
-- Same as above, but this time for Maybe
-- Reminder: Maybe is defined like so:
-- data Maybe a = Nothing | Just a
--
-- foldMaybe :: ?
-- foldMaybe = undefined

-- EXERCISE
-- Same as above, but this time for Either
-- Reminder: Either is defined like so:
-- data Either a b = Left a | Right b
--
-- foldEither :: ?
-- foldEither = undefined

-- EXERCISEs
-- A (binary) tree is either empty, or it has an element (a root) along with a left and right subtree
data Tree a
  = Empty
  | Node (Tree a) a (Tree a)
  deriving (Show)

-- A function to construct leaves (trees whose left and right subtree are Empty) more easily.
leaf :: a -> Tree a
leaf x = Node Empty x Empty

-- EXERCISE
-- Find the depth of a tree
-- EXAMPLES
-- >>> depth Empty
-- 0
-- >>> depth $ leaf 5
-- 1
-- >>> depth (Node (leaf 5) 6 Empty)
-- 2
-- >>> depth (Node (leaf 5) 6 (Node (leaf 7) 8 Empty))
-- 3
depth :: Tree a -> Integer
depth = undefined

-- EXERCISE
-- Reverse a tree
-- EXAMPLES
-- >>> reverseTree $ leaf 5
-- Node Empty 5 Empty
-- >>> reverseTree $ Node (leaf 5) 6 (leaf 7)
-- Node (Node Empty 7 Empty) 6 (Node Empty 5 Empty)
reverseTree :: Tree a -> Tree a
reverseTree = undefined

-- EXERCISE
-- Think about what a "fold" for a Tree would be and implement it.
-- Remember - you need to look at the constructors.
-- foldTree :: ???
-- foldTree = undefined

-- EXERCISE
-- Find the depth of a tree using foldTree
-- EXAMPLES
-- >>> depth' Empty
-- 0
-- >>> depth' $ leaf 5
-- 1
-- >>> depth' (Node (leaf 5) 6 Empty)
-- 2
-- >>> depth' (Node (leaf 5) 6 (Node (leaf 7) 8 Empty))
-- 3
depth' :: Tree a -> Integer
depth' = undefined

-- EXERCISE
-- Reverse a tree using foldTree
-- EXAMPLES
-- >>> reverseTree' $ leaf 5
-- Node Empty 5 Empty
-- >>> reverseTree' $ Node (leaf 5) 6 (leaf 7)
-- Node (Node Empty 7 Empty) 6 (Node Empty 5 Empty)
reverseTree' :: Tree a -> Tree a
reverseTree' = undefined

-- EXERCISE
-- Map over a tree using foldTree
-- EXAMPLES
-- >>> mapTree succ $ leaf 5
-- Node Empty 6 Empty
-- >>> mapTree (\x -> x * x) $ Node (leaf 5) 6 (leaf 7)
-- Node (Node Empty 25 Empty) 36 (Node Empty 49 Empty)
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined

-- EXERCISE
-- Insert into a binary search tree, keeping the "ordered" property.
-- BST definition:
-- Empty is a BST
-- Node l x r is a BST if

-- * l is a BST

-- * r is a BST

-- * x is larger than all of the elements in l

-- * x is smaller than all of the elements in r

-- EXAMPLES
-- >>> insert 5 Empty
-- Node Empty 5 Empty
-- >>> insert 5 $ leaf 6
-- Node (Node Empty 5 Empty) 6 Empty
-- >>> insert 5 $ leaf 4
-- Node Empty 4 (Node Empty 5 Empty)
-- >>> insert 5 $ (Node (leaf 4) 7 (leaf 8))
-- Node (Node Empty 4 (Node Empty 5 Empty)) 7 (Node Empty 8 Empty)
insert :: Integer -> Tree Integer -> Tree Integer
insert = undefined

-- EXERCISE
-- Use foldr to convert a list into a BST.
-- EXAMPLES
-- >>> listToTree [2,1,3]
-- Node (Node Empty 1 (Node Empty 2 Empty)) 3 Empty
-- >>> listToTree [1,2,3]
-- Node (Node (Node Empty 1 Empty) 2 Empty) 3 Empty
-- >>> listToTree [4,1,2,5]
-- Node (Node (Node Empty 1 Empty) 2 (Node Empty 4 Empty)) 5 Empty
listToTree :: [Integer] -> Tree Integer
listToTree = undefined

-- EXERCISE
-- Use foldTree to convert a tree into a list. You should be walking the tree in a "left root right" order.
-- EXAMPLES
-- >>> treeToList $ leaf 5
-- [5]
-- >>> treeToList $ (Node (leaf 6) 5 (leaf 3))
-- [6,5,3]
-- >>> treeToList $ (Node (Node (leaf 7) 8 (leaf 69)) 5 (leaf 3))
-- [7,8,69,5,3]
treeToList :: Tree a -> [a]
treeToList = undefined

-- EXERCISE
-- Sort a list using the above two functions.
-- Do the operations that this sort executes remind you of some other sorting technique?
-- EXAMPLES
-- >>>
sort :: [Integer] -> [Integer]
sort = undefined

-- EXERCISE
-- Use foldTree to check if an element is in a tree, assuming it's a BST.
-- EXAMPLES
-- >>> searchTree 5 $ listToTree [1,2,3]
-- False
-- >>> searchTree 2 $ listToTree [1,2,3]
-- True
searchTree :: Integer -> Tree Integer -> Bool
searchTree = undefined

-- EXERCISE
-- Use foldTree to delete all the occurences of an element from a tree.
-- In the case that the root "disappears", it's fine to merge the left tree into the right one (or vice versa).
-- I chose merging the left tree into the right one, so the examples might not be exactly the same for you,
-- if you choose the other direction. (or another strategy entirely to deal with missing roots)
-- EXAMPLES
-- >>> deleteTree 5 $ listToTree [5,5,5]
-- Empty
-- >>> deleteTree 5 $ listToTree [1,5,5,8,6]
-- Node (Node Empty 1 Empty) 6 (Node Empty 8 Empty)
-- >>> deleteTree 5 $ listToTree [1,5,67,5,8]
-- Node (Node Empty 1 Empty) 8 (Node Empty 67 Empty)
deleteTree :: Integer -> Tree Integer -> Tree Integer
deleteTree = undefined

-- EXERCISE
-- Trees also admit "a foldr"
-- Is there more than one way to write a foldr for trees? What's the difference?
foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree = undefined

-- EXERCISE
-- Sum a tree using foldrTree
sumTree :: Tree Integer -> Integer
sumTree = undefined

-- EXERCISE
-- Convert a tree into a list using foldrTree
treeToList' :: Tree a -> [a]
treeToList' = undefined

-- EXERCISE
-- foldr is more general than foldl. Indeed, we can even implement foldl using foldr
-- HINT: We're going to be constructing a function, which we then apply to our initial value v.
-- We want the function to emulate what foldl f v xs would normally do.
-- id and (.) are going to be useful.
-- It mighto also help to think about, if f' = flip f, how you can transform
-- f' x (f' y v))
-- into
-- f (f v x) y
-- Use type holes - if you write _ in a place where you want an argument, you'll get an error explaining what the type
-- that's expected there is.
foldlViaFoldr :: forall a b. (b -> a -> b) -> b -> [a] -> b
foldlViaFoldr = undefined

-- EXERCISE
-- Figure out how to extend foldNat so you can easily write factorial over nats.
-- You need some extra info at each step - what is it?
-- foldNat' :: _
-- foldNat' = undefined

-- EXERICSE
-- And then implement factorial using it.
fact :: Nat -> Nat
fact = undefined

-- EXERCISE
-- If Nats can be converted to "n times applications" via foldNat,
-- is it perhaps true that "n times applications" can also be converted to Nats somehow?
--
-- You can ignore this bit below if you want to - just assume the forall means "the passed function must be polymorphic over a"
-- START "forall explanation"
-- Usually when we have a polymorphic function, like id :: a -> a
-- the *caller* chooses what a will be - when I write id 'a', I instantiate a with Char, so id becomes id :: Char -> Char
-- However, here we will need our function to work for any a, and so we must *require* something of the caller -
-- that they provide a function working *for any* a - meaning *we*(the callee) can decide what a to apply it for.
-- As a further example, consider
-- f :: (a -> a) -> Bool
-- f g = g True
-- this does *not* compile - let's assume it did.
-- if we have
-- h :: Int -> Int
-- h x = x + 1
-- then the caller would be able to write f h, (as they pick what a is) which is not valid,
-- since h requires its argument and return types to be Int, and True :: Bool
-- instead
-- f :: (forall a. a -> a) -> Bool
-- f g = g True
-- compiles, and now the caller cannot do f h, since the passed h needs to work *for any* a, while h :: Int -> Int
-- END "forall explanation"
--
-- EXAMPLES
-- >>> iterateToNat (\f x -> f (f (f x)))
-- Suc (Suc (Suc Zero))
iterateToNat :: (forall a. (a -> a) -> a -> a) -> Nat
iterateToNat f = undefined

natToIterate :: Nat -> (a -> a) -> a -> a
natToIterate n f v = foldNat f v n

type Natural = forall a. (a -> a) -> a -> a

-- EXERCISE
-- Hey, if we can convert between Natural (the type argument to iterateToNat), now with a synonym) and Nat without losing information
-- wouldn't that mean that they are equivalent, and we can do the same things with both?
-- let's reimplement some of them, with Natural
-- These are called "church encoded" natural numbers - they're used to encode natural numbers, when the only thing you "have" is functions.
--
-- Here's some exposition:
-- As you saw in the iterateToNat example, these Naturals are essentially applying some function to some value a number of times.
-- The idea is that we represent the number n as applying a function f n times to a value v.
-- For example:
-- 0 is represented by \f v -> v
zero :: Natural
zero f v = v

-- 1 is represented by \f v -> f v
-- 2 is represented by \f v -> f (f v)
-- 3 is represented by \f v -> f (f (f v))
-- and so on
-- With this function, we need to somehow "add another f".
-- EXAMPLES
-- >>> iterateToNat zero
-- Zero
-- >>> iterateToNat $ suc $ suc zero
-- Suc (Suc Zero)
-- >>> natToInteger $ iterateToNat $ suc $ natToIterate $ integerToNat 5
-- 6
suc :: Natural -> Natural
suc n = undefined

-- EXERCISE
-- We can also add these. Here we need to think about how to add f n times to another Natural.
-- Note that usually
-- add n m = n suc m
-- Would be ok as a definition, however, due to some constraints in how ghc works, is not accepted (resolved in ghc 9.2 with ImpredicativeTypes)
-- EXAMPLES
-- >>> iterateToNat $ add (suc (suc zero)) zero
-- Suc (Suc Zero)
-- >>> iterateToNat $ add (suc (suc zero)) (suc (suc (suc zero)))
-- Suc (Suc (Suc (Suc (Suc Zero))))
-- >>> natToInteger $ iterateToNat $ add (suc (suc zero)) (suc (suc (suc (suc zero))))
-- 6
add :: Natural -> Natural -> Natural
add n m = undefined

-- EXERCISE
-- Now multiply them
-- >>> iterateToNat $ mult (suc (suc zero)) zero
-- Zero
-- >>> iterateToNat $ mult zero (suc (suc zero))
-- Zero
-- >>> iterateToNat $ mult (suc (suc zero)) (suc (suc zero))
-- Suc (Suc (Suc (Suc Zero)))
-- >>> natToInteger $ iterateToNat $ mult (suc (suc zero)) (suc (suc (suc zero)))
-- 6
mult :: Natural -> Natural -> Natural
mult n m = undefined

-- Is the same true for lists? Is there some function type that is "isomorphic" to lists - you can convert
-- back and forth between lists and the function, without losing data? Like how Natural is to Nat
-- (or if you prefer - can you express lists by only using lambdas?)
