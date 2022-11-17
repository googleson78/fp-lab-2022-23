{-# OPTIONS_GHC -Wall #-}

module BooleanBlindness where

import Text.Printf (printf)

-- >>> digitsToNumber [0, 6, 9, 0]
-- 690
digitsToNumber :: [Integer] -> Integer
digitsToNumber = go . reverse
  where
    go [] = 0
    go (x : xs) = x + go xs * 10

-- check
-- "adsf"
stringToNumber :: String -> Integer
stringToNumber str =
  if all isDigit str
    then digitsToNumber (map charToDigit str)
    else error $ printf "assert: input value %s should be string" str

-- f :: a -> Bool
-- f' :: a -> Maybe b

isDigit :: Char -> Bool
isDigit x = x `elem` ['0' .. '9']

-- pattern match
charToDigit :: Char -> Integer
charToDigit c =
  case c of
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    _ -> error $ printf "input char %c should be a digit" c

-- pattern match
parseDigit :: Char -> Maybe Integer
parseDigit c =
  case c of
    '0' -> Just 0
    '1' -> Just 1
    '2' -> Just 2
    '3' -> Just 3
    '4' -> Just 4
    '5' -> Just 5
    '6' -> Just 6
    '7' -> Just 7
    '8' -> Just 8
    '9' -> Just 9
    _ -> Nothing

-- "12a34" -> Nothing
parseNumber :: String -> Maybe Integer
parseNumber =
  maybeMap digitsToNumber . traverseListMaybe parseDigit

-- reading: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/

-- map ::            (a ->       b) -> [a] ->       [b]
traverseListMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseListMaybe _ [] = Just []
traverseListMaybe f (x : xs) =
  case (f x, traverseListMaybe f xs) of
    (Just x', Just xs') -> Just $ x' : xs'
    _ -> Nothing

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just $ f x
