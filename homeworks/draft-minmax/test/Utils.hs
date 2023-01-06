{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Utils
  ( matrixToList,
    foldThrice,
    foldMapThrice,
    threeToInt,
    listToMatrix,
    ixListAsIfMatrix,
    tupToThrice,
    foldMapMatrix,
    thriceToList,
    thriceToiList,
    matrixToiList,
    isWin,
  )
where

import Matrix (Matrix (..), Three (..), Thrice)
import TicTacToe (Result (..))

matrixToList :: Matrix a -> [a]
matrixToList = foldMapMatrix (: [])

matrixToiList :: Matrix a -> [(Three, Three, a)]
matrixToiList (MkMatrix m) =
  foldThrice \i ->
    foldThrice \j ->
      [(i, j, m i j)]

thriceToList :: Thrice a -> [a]
thriceToList = foldMapThrice (: [])

thriceToiList :: Thrice a -> [(Three, a)]
thriceToiList t = map (\i -> (i, t i)) [Zero, One, Two]

listToMatrix :: [a] -> Matrix a
listToMatrix xs = MkMatrix $ \i j -> ixListAsIfMatrix i j xs

ixListAsIfMatrix :: Three -> Three -> [a] -> a
ixListAsIfMatrix i j xs = xs !! (threeToInt i * 3 + threeToInt j)

tupToThrice :: (a, a, a) -> Thrice a
tupToThrice (x, y, z) = \case
  Zero -> x
  One -> y
  Two -> z

threeToInt :: Three -> Int
threeToInt Zero = 0
threeToInt One = 1
threeToInt Two = 2

foldThrice :: Semigroup m => Thrice m -> m
foldThrice = foldMapThrice id

foldMapThrice :: Semigroup a => (t -> a) -> Thrice t -> a
foldMapThrice f t = f (t Zero) <> f (t One) <> f (t Two)

foldMapMatrix :: Monoid m => (a -> m) -> Matrix a -> m
foldMapMatrix f (MkMatrix m) = foldThrice \i -> foldThrice \j -> f $ m i j

isWin :: Result -> Bool
isWin (Wins _) = True
isWin _ = False
