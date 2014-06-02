{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Themis.Provider.Interactive (
    runTests
  , module Test.Themis.Test
  ) where

import Control.Exception (SomeException, catch)

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import Test.Themis.Test


-- Simple test case evaluation that prints the "name: [PASSED]" message
-- to the standard out if the test passed, otherwise the "name: [FAILED] msg expected found"
-- message.
runTestCase :: TestCase -> IO ()
runTestCase t = testCaseCata eval evalIO shrink group t >> return ()
  where
    shrink test tests = do
      passed <- test
      if passed
        then return ()
        else sequence_ tests
      return passed

    group groupName tests = do
      putStrLn $ "[TEST GROUP]: " ++ groupName
      fmap and $ sequence tests

    eval testName assertion = do
      (passed, msg) <- evalAssertion assertion
      putStrLn $ concat [testName, ": ", msg]
      return passed

    evalIO testName = (>>= eval testName)

    evalAssertion :: (Show a, Eq a) => Assertion a -> IO (Bool,String)
    evalAssertion = assertion equals satisfies property err where

      equals expected found msg = return $
        if (expected == found)
          then (True, "[PASSED]")
          else (False, concat $ ["[FAILED] ", msg, " Expected: ", show expected, " Found: ", show found])

      satisfies property found msg = return $
        if (property found)
          then (True, "[PASSED]")
          else (False, concat $ ["[FAILED] ", msg, " Found: ", show found, " does not satisfy the given property"])

      property prop gen msg = do
        let args = stdArgs { chatty = False }
        result <- quickCheckWithResult args (forAll gen prop)
        return $ if (isSuccess result)
          then (True, "[PASSED]")
          else (False, concat $ ["[FAILED] Output:", output result, " ", msg])

      err value msg = catch
        (do return $! value
            return (False, "[FAILED] Exception is not occured: " ++ msg))
        (\msg' -> return (True, "[PASSED] Exception is occured: " ++ (show (msg'::SomeException))))

runTestsIO :: TestSet -> IO ()
runTestsIO = testSetCata (mapM_ runTestCase)

-- Use the simple framework to print out the results to the stdout
runTests = runTest runTestsIO
