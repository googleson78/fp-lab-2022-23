{-# LANGUAGE LambdaCase #-}

module Matrix where

data Three = Zero | One | Two
  deriving (Eq, Show)

flipThree :: Three -> Three
flipThree Zero = Two
flipThree One = One
flipThree Two = Zero

type Thrice a = Three -> a

thriceToTriple :: Thrice a -> (a, a, a)
thriceToTriple t = (t Zero, t One, t Two)

thrice :: a -> a -> a -> Thrice a
thrice x y z t =
  case t of
    Zero -> x
    One -> y
    Two -> z

newtype Matrix a = MkMatrix {getMatrix :: Thrice (Thrice a)}

matrix ::
  a ->
  a ->
  a ->
  a ->
  a ->
  a ->
  a ->
  a ->
  a ->
  Matrix a
matrix x0 x1 x2 y0 y1 y2 z0 z1 z2 =
  MkMatrix $ \case
    Zero -> thrice x0 x1 x2
    One -> thrice y0 y1 y2
    Two -> thrice z0 z1 z2

-- MkMatrix $ \case
--   ...cases...
-- is exactly the same as
-- MkMatrix $ \i ->
--   case i of
--     ...cases...
-- It requires the `LambdaCase` language extension - it's enabled at the top of the file

constantMatrix :: a -> Matrix a
constantMatrix = error "not implemented"

diagonalMatrix :: a -> Matrix (Maybe a)
diagonalMatrix = error "not implemented"

otherDiagonalMatrix :: a -> Matrix (Maybe a)
otherDiagonalMatrix = error "not implemented"

addMatrix :: Matrix Integer -> Matrix Integer -> Matrix Integer
addMatrix = error "not implemented"

showMatrix :: (a -> String) -> Matrix a -> String
showMatrix = error "showMatrix is not implemented yet, hence you can't display matrices"

ix :: Three -> Three -> Matrix a -> a
ix = error "not implemented"

getRow :: Three -> Matrix a -> Thrice a
getRow = error "not implemented"

getCol :: Three -> Matrix a -> Thrice a
getCol = error "not implemented"

getDiag :: Matrix a -> Thrice a
getDiag = error "not implemented"

getOtherDiag :: Matrix a -> Thrice a
getOtherDiag = error "not implemented"

transpose :: Matrix a -> Matrix a
transpose = error "not implemented"

foldThriceWith :: (a -> a -> a) -> Thrice a -> a
foldThriceWith = error "not implemented"

foldMatrixWith :: (a -> a -> a) -> Matrix a -> a
foldMatrixWith = error "not implemented"

eqMatrix :: (a -> a -> Bool) -> Matrix a -> Matrix a -> Bool
eqMatrix = error "not implemented"

imapThrice :: (Three -> a -> b) -> Thrice a -> Thrice b
imapThrice = error "not implemented"

mapThrice :: (a -> b) -> Thrice a -> Thrice b
mapThrice = error "not implemented"

imapMatrix :: (Three -> Three -> a -> b) -> Matrix a -> Matrix b
imapMatrix = error "not implemented"

mapMatrix :: (a -> b) -> Matrix a -> Matrix b
mapMatrix = error "not implemented"

place :: Three -> Three -> a -> Matrix a -> Matrix a
place = error "not implemented"

concatThriceWith :: String -> Thrice String -> String
concatThriceWith = error "not implemented"

concatMatrixWith :: String -> String -> Matrix String -> String
concatMatrixWith = error "not implemented"

showMatrixComposition :: (a -> String) -> Matrix a -> String
showMatrixComposition = error "not implemented"

instance Show a => Show (Matrix a) where
  show = showMatrix show

{-
type GenericMatrix =

foldGenericMatrix ::
mapGenericMatrix ::
multGenericMatrix ::
-}
