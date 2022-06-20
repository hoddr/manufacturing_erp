module Spec.PurchaseItems
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
import qualified Client.PurchaseItems as CPI
import Spec.Common ( isHttpException )

spec :: Spec
spec = describe "PurchaseItems" $ do
  describe "post purchase item" $ do
    it "should create new purchase item with valid input" $ do
      res <- CPI.addPurchaseItem CPI.testPurchaseItem
      case res of
           Nothing -> expectationFailure "Failed to create new purchase item"
           _ -> pure ()
  describe "edit purchase item (+ list)" $ do
    it "should edit purchase item from retrieved list" $ do
      pis <- CPI.listPurchaseItems
      res <- CPI.editPurchaseItem (CPI.getId $ head pis) (head pis)
      case res of
           Nothing -> expectationFailure "Failed to edit purchase item"
           _ -> pure ()
    it "should throw HttpException if purchase item body id mismatch with sent id" $ do
      pis <- CPI.listPurchaseItems
      CPI.editPurchaseItem (-3) (head pis) `shouldThrow` isHttpException
