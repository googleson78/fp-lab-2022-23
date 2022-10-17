{-# LANGUAGE EmptyDataDeriving #-}

module Can where

import Binary (Binary (..), Bit (..))

data LeadingOne
  deriving (Show)

canOne :: LeadingOne
canOne = undefined

data Can
  deriving (Show)

canZero :: Can
canZero = undefined

snoc :: Can -> Bit -> Can
snoc = undefined

forgetLeadingOne :: LeadingOne -> Binary
forgetLeadingOne = undefined

forget :: Can -> Binary
forget = undefined

canonicalise :: Binary -> Can
canonicalise = undefined
