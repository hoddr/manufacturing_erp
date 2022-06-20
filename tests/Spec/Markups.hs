module Spec.Markups
  ( spec
  )
  where

import Test.Hspec ( Spec
                  , describe
                  , expectationFailure
                  , it
                  )

-- LOCAL IMPORTS --
import qualified Client.Markups as CM

spec :: Spec
spec = do
  describe "Markups" $ do
    describe "get pricing info" $ do
      it "should retrieve latest labor rate and margins" $ do
        res <- CM.getLatestMarkups
        case res of
             Nothing -> expectationFailure "Failed to retrieve latest labor rate and margins"
             _ -> pure ()
    describe "update labor rate" $ do
      it "should get through update route" $ do
        utc <- CM.getUTCTime
        res <- CM.updateLabor $ CM.testLabor utc
        case res of
             Nothing -> expectationFailure "Failed to update labor rate"
             _ -> pure ()
    describe "update margins" $ do
      it "should get through update route" $ do
        utc <- CM.getUTCTime
        res <- CM.updateMargins $ CM.testMargins utc
        case res of
             Nothing -> expectationFailure "Failed to update margins"
             _ -> pure ()
