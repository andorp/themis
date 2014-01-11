module Test.Themis.Provider.TestFramework (
   buildTestSet
 , runTFTests
 , module Test.Themis.Test
 ) where

import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFH
import qualified Test.Framework.Providers.QuickCheck2 as TFQ

import qualified Test.HUnit as HU
import qualified Test.QuickCheck as QC

import           Test.Themis.Test

buildTestCase :: TestCase -> TF.Test
buildTestCase = testCaseCata eval shrink
  where
    shrink test tests = TF.testGroup "Shrink test group" (test:tests)
    
    eval testName = assertionCata equals satisfies property
      where
        equals expected found msg = TFH.testCase testName (HU.assertEqual msg expected found)
      
        satisfies prop found msg = TFH.testCase testName (HU.assertEqual msg True (prop found))
      
        property prop gen msg = TFQ.testProperty testName (QC.forAll gen prop)

buildTestSet :: TestSet -> [TF.Test]
buildTestSet = testSetCata (map buildTestCase)

runTFTests :: Test b -> IO ()
runTFTests = runTest (TF.defaultMain . buildTestSet)
