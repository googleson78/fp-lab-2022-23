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

module ListMaybe where

-- [a]
-- data List a = Nil | Cons a (List a)
--
-- [1,2,3]
-- 1 : 2 : 3 : []

-- [Int]
--
--
-- [1, 2, 3]
--
-- [4, 5, 6]

-- data Maybe a = Nothing | Just a

--
-- int32
-- int64

-- int -> [Int32, Int64]

--
--
--
-- x :: Maybe a
--
-- safeDiv x y :: Maybe a

-- liftA2 (+) [1,2,3] [4,5,6]
-- liftA2 (+) :: [Int] -> [Int] -> [Int]
-- liftA2 ::
--   (a -> b -> c) ->
--   ([a] -> [b] -> [c])

-- liftA2 ::
--   (a       ->       b ->       c) ->
--   (Maybe a -> Maybe b -> Maybe c)

-- (.) ::
--   (b -> c) ->
--   (a -> b) ->
--   (a -> c)

-- (.) ::
--   (b -> Maybe c) ->
--   (a -> Maybe b) ->
--   (a -> Maybe c)

safeDiv ::
  Integer ->
  Integer ->
  Maybe Integer
safeDiv x 0 = Nothing
safeDiv x y = Just (x `div` y)
