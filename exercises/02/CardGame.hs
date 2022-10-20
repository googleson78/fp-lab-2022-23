{-# LANGUAGE EmptyDataDeriving #-}
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

-- warnings

module CardGame where

-- we want to model the following card Game
--
-- there are two Players
--
-- they each start with a fixed Hand of monster Cards (let's say 6)
--
-- monsters have Attack and Health values
--
-- there is a battle Field with a fixed number of Spots (let's say 3)
--
-- the players take turns - on each Turn the current player must play a card from their hand
-- play means putting it in an empty spot on the field
--
-- at the end of each turn, opposing monsters battle, i.e.
-- X Y Z
-- U V W
-- X fights U, Y attacks V, Z attacks W
--
-- battle means that they subtract their attack values from each others' health values, e.g. U loses X.attack health points and X loses U.attack health points
-- monsters whose attack drops below 0 die (disappear from the battle Field)
--
-- if a monster doesn't have anything to attack, it will instead deal direct damage earning Points for its controller equal to its attack
-- the player with the most points after all cards are played wins

-- your task is to interpret this description and model it using Haskell data types and functions as accurately as possible
--
-- the capitalised words are all hints for possible data types you could make
-- but at the very least there should be

-- * a core data type Game holding all the "current" state of the game

-- * a function to "play" a card affecting the Game

-- * a function to battle monsters affecting the Game
