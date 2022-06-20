module Spec.Pricing
  ( spec
  )
  where

import Test.Hspec ( Spec
                  , describe
                  , expectationFailure
                  , it
                  , shouldThrow
                  )

-- LOCAL IMPORTS --
import qualified Client.Pricing as CP
import Spec.Common ( isHttpException )

spec :: Spec
spec = do
  describe "Pricing -> estimator" $ do
    it "should return priced raw line items after estimate" $ do
      res <- CP.getEstimate CP.testRlis
      case res of
           [] -> expectationFailure "Should not be empty list"
           _ -> return ()
    it "should throw HttpException and print error (unable to price) on invalid input" $ do
      CP.getEstimate CP.invalidRlis `shouldThrow` isHttpException
    it "should throw HttpException and print error (unable to parse time string) on invalid input" $ do
      CP.getEstimate CP.invalidTimeStringRlis `shouldThrow` isHttpException
