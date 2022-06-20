module Spec.FabItems
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
import qualified Client.FabItems as CFI
import Spec.Common ( isHttpException )


spec :: Spec
spec = do
  describe "FabItems" $ do
    describe "post fab item" $ do
      it "should create new fab item entry with valid input" $ do
        res <- CFI.addFabItem CFI.testFabItem
        case res of
             Nothing -> expectationFailure "Failed to create new fab item"
             _ -> pure ()
    describe "edit fab item (+ list)" $ do
      it "should edit fab item from retrieved list" $ do
        fis <- CFI.listFabItems
        res <- CFI.editFabItem 1 (head fis)
        case res of
             Nothing -> expectationFailure "Failed to edit fab item"
             _ -> pure ()
      it "should throw HttpException if fab item body id mismatch with sent id" $ do
        fis <- CFI.listFabItems
        CFI.editFabItem (-3) (head fis) `shouldThrow` isHttpException
