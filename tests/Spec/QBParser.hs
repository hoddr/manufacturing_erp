module Spec.QBParser
  ( spec
  )
  where

import Test.Hspec ( Spec
                  , describe
                  , it
                  )


-- LOCAL IMPORTS --
import qualified Client.QBParser as CQB

spec :: Spec
spec = do
  describe "QBParser" $ do
    it "should load customers" $ do
      CQB.loadCustomers >> pure ()
    it "should load vendors" $ do
      CQB.loadVendors >> pure ()
    it "should load purchase items" $ do
      CQB.loadPurchaseItems >> pure ()
    it "should load materials" $ do
      CQB.loadMaterials >> pure ()
    it "should load stock fab items" $ do
      CQB.loadFabItems >> pure ()
