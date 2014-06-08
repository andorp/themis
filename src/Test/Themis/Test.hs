{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Themis.Test (
    Assertion(..)
  , assertion
  , ioTest
  , TestName
  , Test
  , TestCase
  , TestSet
  , testCaseCata
  , testSetCata
  , test
  , shrink
  , group
  , runTest
  , module Test.QuickCheck.Gen
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Trans
import           Control.Monad.Identity
import           Control.Monad.Writer
import qualified Control.Monad.Trans.Writer as CMW
import           Test.QuickCheck.Gen

import           Test.Themis.Assertion

-- Test Name is a non-empty string for a test case
type TestName = String

data TestCase
  = forall a . (Show a, Eq a) => TestCase (TestName, Assertion a)
  -- ^ A test case that can be evaulated somehow
  | forall a e . (Show e, Error e) => TestCaseIO (TestName, IO (Either e a))
  -- ^ A test case that has some erroneous IO computation
  | Shrink TestCase [TestCase]
  -- ^ If the test case passes, the rest of the tests would skipped
  -- ^ otherwise they evaulated to locate the problem
  | TestGroup TestName [TestCase]
  -- ^ A named group of test cases

testCase name assertion = TestCase (name, assertion)

testCaseIO name assertion = TestCaseIO (name, assertion)

testCaseCata
  :: (TestName -> forall a . (Show a, Eq a) => Assertion a -> b)
  -> (TestName -> (forall a e . (Show e, Error e) => IO (Either e a) -> b))
  -> (b -> [b] -> b)
  -> (TestName -> [b] -> b)
  -> TestCase
  -> b
testCaseCata testCase testCaseIO shrink group t = case t of
  TestCase (name, assertion) -> testCase name assertion
  TestCaseIO (name, assertion) -> testCaseIO name assertion
  Shrink test tests          -> shrink (testCaseCata' test) (map testCaseCata' tests)
  TestGroup name tests       -> group name (map testCaseCata' tests)
  where
    testCaseCata' = testCaseCata testCase testCaseIO shrink group

-- Test Set is a set of test cases, which elements will be
-- evaulated some of the test case runners
type TestSet = [TestCase]

testSetCata f ts = f ts

-- Creates a singleton test set, from the given test case
singleton :: TestCase -> TestSet
singleton t = [t]

-- * Monadic interface

-- Monadic abstraction for building test set.
newtype Test a = Test { unTest :: CMW.WriterT TestSet Identity a }
  deriving ( Functor
           , Applicative
           , MonadWriter TestSet
           , Monad
           )

-- * Primitives

-- | Build a test set from the given test computation
buildTestSet :: Test b -> TestSet
buildTestSet = snd . runIdentity . runWriterT . unTest

-- | Builds a test set and evaulates with the given evaluator
runTest :: (TestSet -> a) -> Test b -> a
runTest eval = eval . buildTestSet

-- | Creates a test case with the given name and the given assertion
test :: (Eq a, Show a) => TestName -> Assertion a -> Test ()
test name assertion = tell . singleton $ testCase name assertion

-- | Creates a test case with the given name and the given
-- erroneous IO computation
ioTest :: (Show e, Error e) => TestName -> IO (Either e a) -> Test ()
ioTest name assertion = tell . singleton $ testCaseIO name assertion

-- * Combinators

-- Define the test case that can shrink to one or more sub cases
shrink :: (Eq a, Show a) => TestName -> Assertion a -> Test b -> Test ()
shrink name assertion shrinks = tell . singleton $ Shrink (testCase name assertion) (buildTestSet shrinks)

group :: TestName -> Test b -> Test ()
group name tests = tell . singleton $ TestGroup name (buildTestSet tests)
