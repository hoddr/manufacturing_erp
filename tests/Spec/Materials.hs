module Spec.Materials
  ( spec
  )
  where

import Test.Hspec ( Spec
                  , describe
                  , expectationFailure
                  , it
                  )

-- LOCAL IMPORTS --
import qualified Client.Materials as CM

spec :: Spec
spec = do
  describe "Materials" $ do
    describe "post material" $ do
      it "should create new material entry with valid input" $ do
        res <- CM.addMaterial CM.testMaterial
        case res of
             Nothing -> expectationFailure "Failed to create new material"
             _ -> pure ()
    describe "edit material (+ list)" $ do
      it "should edit material from retrieved list" $ do
        mats <- CM.listMaterials
        res <- CM.editMaterial $ head mats
        case res of
             Nothing -> expectationFailure "Failed to edit material"
             _ -> pure ()
    describe "get material (+ list)" $ do
      it "should retrieve individual material from retrieved list" $ do
        mats <- CM.listMaterials
        res <- CM.getMaterial $ head mats
        case res of
             Nothing -> expectationFailure "Failed to get material that should exist"
             _ -> pure ()
