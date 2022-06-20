module Spec.Vendors
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
import qualified Client.Vendors as CV
import Spec.Common ( isHttpException )


spec :: Spec
spec = do
  describe "Vendors" $ do
    describe "post vendor" $ do
      it "should create new vendor entry with valid input" $ do
        res <- CV.addVendor CV.testVendor
        case res of
             Nothing -> expectationFailure "Failed to create new vendor"
             _ -> pure ()
    describe "edit vendor (+ list)" $ do
      it "should edit vendor from retrieved list" $ do
        vends <- CV.listVendors
        res <- CV.mainEdit $ head vends
        case res of
             Nothing -> expectationFailure "Failed to edit vendor"
             _ -> pure ()
      it "should throw HttpException if vendor body id mismatch with sent id" $ do
        vends <- CV.listVendors
        CV.badEdit (head vends) `shouldThrow` isHttpException
    describe "get vendor (+ list)" $ do
      it "should retrieve individual vendor from retrieved list" $ do
        vends <- CV.listVendors
        res <- CV.getVendor $ head vends
        case res of
             Nothing -> expectationFailure "Failed to get vendor that should exist"
             _ -> pure ()
      it "should throw an HttpException for not-found vendor" $ do
        CV.getVendor CV.badIdVendor `shouldThrow` isHttpException
