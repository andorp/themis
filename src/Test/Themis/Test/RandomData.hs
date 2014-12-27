module Test.Themis.Test.RandomData (
    RandomData(..)
  ) where

-- Reimports the quickcheck's arbitrary module.

import Test.QuickCheck.Gen

class RandomData a where
  positive :: Gen a
  negative :: Gen a

