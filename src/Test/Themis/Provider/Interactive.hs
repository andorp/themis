{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Themis.Provider.Interactive (
    runTests
  , module Test.Themis.Test
  ) where

import Control.Exception (SomeException, catch, evaluate)
import Control.Monad.Error (strMsg)

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import Test.Themis.Test


-- Simple test case evaluation that prints the "name: [PASSED]" message
-- to the standard out if the test passed, otherwise the "name: [FAILED] msg expected found"
-- message.
runTestCase :: TestCase -> IO ()
runTestCase t = testCaseCata eval evalIO shrink group t >> return ()
  where
    passed = "[PASSED]"
    failed = "[FAILED]"

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

    evalIO testName comp = do
      result <- comp
      case result of
        Left e -> do putStrLn $ concat [testName, ": ", failed, " ", show e]
                     return False
        Right x -> do putStrLn $ concat [testName, ": ", passed]
                      return True

    evalAssertion :: (Show a, Eq a) => Assertion a -> IO (Bool,String)
    evalAssertion = assertion equals satisfies property err where

      catchSomeEx :: IO a -> (SomeException -> IO a) -> IO a
      catchSomeEx = catch

      tryOut a = do
        catchSomeEx
          (evaluate a)
          (\ex -> return (False, concat [failed, " Unexpected exception: ", show ex]))

      equals expected found msg = tryOut $
        if (expected == found)
          then (True, passed)
          else (False, concat $ [failed, " ", msg, " Expected: ", show expected, " Found: ", show found])

      satisfies property found msg = tryOut $
        if (property found)
          then (True, passed)
          else (False, concat $ [failed, " ", msg, " Found: ", show found, " does not satisfy the given property"])

      property prop gen msg = do
        let args = stdArgs { chatty = False }
        result <- quickCheckWithResult args (forAll gen prop)
        tryOut $ if (isSuccess result)
          then (True, passed)
          else (False, concat $ [failed," Output:", output result, " ", msg])

      err value msg = catchSomeEx
        (do return $! value
            return (False, concat [failed, " Exception is not occured: ", msg]))
        (\msg' -> return (True, concat [passed, " Exception is occured: ", show (msg'::SomeException)]))

runTestsIO :: TestSet -> IO ()
runTestsIO = testSetCata (mapM_ runTestCase)

-- Use the simple framework to print out the results to the stdout
runTests = runTest runTestsIO
