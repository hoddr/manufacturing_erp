module Spec.Inventory
  ( spec
  )
  where

import Test.Hspec ( Spec
                  , describe
                  , it
                  )

-- LOCAL IMPORTS --
import qualified Client.Inventory as CI

spec :: Spec
spec = do
  describe "Inventory" $ do
    describe "-> list and edit" $ do
      it "should get list of inv items" $ do
        _ <- CI.listInventory
        pure ()
      it "should edit successfully" $ do
        invs <- CI.listInventory
        _ <- CI.editInventory $ head invs
        pure ()
