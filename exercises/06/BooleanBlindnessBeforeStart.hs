{-# OPTIONS_GHC -Wall #-}

module BooleanBlindness where

import Text.Printf (printf)

-- TODO: don't do this together with folds, takes too much time
-- "boolean blindness" - prefer returning Maybe/"a proof", instead of a Bool and doing an if :
-- https://runtimeverification.com/blog/code-smell-boolean-blindness/
-- https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/

digitsToNumber :: [Integer] -> Integer
digitsToNumber = go . reverse
  where
    go [] = 0
    go (x : xs) = x + go xs * 10

isDigit :: Char -> Bool
isDigit = undefined

-- pattern match
charToDigit :: Char -> Integer
charToDigit = undefined

-- check
stringToNumber :: String -> Integer
stringToNumber str = undefined

-- pattern match
parseDigit :: Char -> Maybe Integer
parseDigit = undefined

parseNumber :: String -> Maybe Integer
parseNumber = undefined

-- reading: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/

traverseListMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseListMaybe _ [] = Just []
traverseListMaybe f (x : xs) =
  case (f x, traverseListMaybe f xs) of
    (Just x', Just xs') -> Just $ x' : xs'
    _ -> Nothing

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just $ f x
