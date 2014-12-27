module Test.Themis.Test.Arbitrary (
    alpha
  , alphaNum
  , num
  , Interval(..)
  , module Test.QuickCheck.Arbitrary
  ) where

-- Reimports the quickcheck's arbitrary module.

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

alpha = elements ['a' .. 'z']
num   = elements ['0' .. '9']

alphaNum = oneof [alpha, num]

class Interval n where
  interval :: n -> n -> Gen n

instance Interval Double where
  interval l u = do
    n <- choose (0,10000000000)
    m <- choose (0,n)
    return ((u - l) * (m / n))

instance Interval Int where
  interval l u = let n = u -l in fmap (l+) $ choose (0, n)
