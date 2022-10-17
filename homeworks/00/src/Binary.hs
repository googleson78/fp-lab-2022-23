module Binary where

data Binary
  = End
  | Binary :. Bit
  deriving (Show)

data Bit = Zero | One
  deriving (Show)

infixl 6 :.

succBinary :: Binary -> Binary
succBinary = undefined

integerToBinary :: Integer -> Binary
integerToBinary = undefined

binaryToInteger :: Binary -> Integer
binaryToInteger = undefined

hasLeadingZero :: Binary -> Bool
hasLeadingZero = undefined

isEnd :: Binary -> Bool
isEnd = undefined

canonicalise :: Binary -> Binary
canonicalise = undefined

addBinary :: Binary -> Binary -> Binary
addBinary = undefined
