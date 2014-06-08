module Test.Themis.Provider.TestFramework (
   buildTestSet
 , runTFTests
 , module Test.Themis.Test
 ) where

import Control.Exception (SomeException, catch)

import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFH
import qualified Test.Framework.Providers.QuickCheck2 as TFQ

import qualified Test.HUnit as HU
import qualified Test.QuickCheck as QC

import           Test.Themis.Test

buildTestCase :: TestCase -> TF.Test
buildTestCase = testCaseCata eval evalIO shrink group
  where
    shrink test tests = TF.testGroup "Shrink test group" (test:tests)

    group name tests = TF.testGroup name tests

    eval testName = assertion equals satisfies property err
      where
        equals expected found msg = TFH.testCase testName (HU.assertEqual msg expected found)

        satisfies prop found msg = TFH.testCase testName (HU.assertEqual msg True (prop found))

        property prop gen msg = TFQ.testProperty testName (QC.forAll gen prop)

        err value msg = TFH.testCase testName $ checkException value msg

    checkException value msg = do
          ok <- catch (do { return $! value; return False })
                      emptyCatch
          if ok
            then return ()
            else fail $ "Exception is not occured: " ++ msg
          where
            emptyCatch :: SomeException -> IO Bool
            emptyCatch _ = return True

    evalIO testName comp = TFH.testCase testName $ do
      result <- comp
      case result of
        Left e -> fail $ show e
        Right x -> return ()

buildTestSet :: TestSet -> [TF.Test]
buildTestSet = testSetCata (map buildTestCase)

runTFTests :: Test b -> IO ()
runTFTests = runTest (TF.defaultMain . buildTestSet)
