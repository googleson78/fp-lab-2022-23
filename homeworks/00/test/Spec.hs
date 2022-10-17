{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import BinarySpec (binarySpec)
import CanSpec (canSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
  binarySpec
  canSpec
