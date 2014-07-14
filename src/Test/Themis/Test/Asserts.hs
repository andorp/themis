module Test.Themis.Test.Asserts where

import Test.Themis.Test
import Test.Themis.Assertion

{-
Collection of useful assertion for the Test monad
-}

-- | Creates a test case with the given name, expected, found result, and
-- a message which is displayed when the test fails.
assertEquals :: (Eq a, Show a) => TestName -> a -> a -> String -> Test ()
assertEquals name expected found msg = test name (Equals expected found msg)

-- | Creates a test case with the given name, a predicate to satisfy, found value,
-- and a message which is displayed when the test fails.
assertSatisfy :: (Eq a, Show a) => TestName -> (a -> Bool) -> a -> String -> Test ()
assertSatisfy name pred found msg = test name (Satisfies pred found msg)

-- | Creates a test case with the given name, a property to satisfy, a
-- quickcheck generator to produce values, and a message which is displayed
-- when the test fails.
assertProperty :: (Eq a, Show a) => TestName -> (a -> Bool) -> Gen a -> String -> Test ()
assertProperty name prop gen msg = test name (Property prop gen msg)

-- | Creates a test case with the given name, a value, and a message.
-- The evaluation of the value should fail. If no failure happens the
-- given message is displayed.
assertError :: (Eq a,Show a) => TestName -> a -> String -> Test ()
assertError name x msg = test name (Error x msg)

-- | Creates test cases of equality for the given partitions listed in test-name,
-- input value, output value, error message structure.
eqPartitions :: (Eq b, Show b) => (a -> b) -> [(TestName, a, b, String)] -> Test ()
eqPartitions underTest =
  mapM_ (\(name, input, output, msg) -> assertEquals name output (underTest input) msg)
