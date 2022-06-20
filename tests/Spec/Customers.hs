module Spec.Customers
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
import qualified Client.Customers as CCUS
import Spec.Common ( isHttpException )


spec :: Spec
spec = do
  describe "Customers" $ do
    describe "post customer" $ do
      it "should create new customer entry with valid input" $ do
        res <- CCUS.addCustomer CCUS.testCustomer
        case res of
             Nothing -> expectationFailure "Failed to create new customer"
             _ -> pure ()
    describe "edit customer (+ list)" $ do
      it "should edit customer from retrieved list" $ do
        cs <- CCUS.listCustomers
        res <- CCUS.editCustomer 0 (head cs)
        case res of
             Nothing -> expectationFailure "Failed to edit customer"
             _ -> pure ()
      it "should throw HttpException if customer body id mismatch with sent id" $ do
        cs <- CCUS.listCustomers
        CCUS.editCustomer (-3) (head cs) `shouldThrow` isHttpException
    describe "get customer (+ list)" $ do
      it "should retrieve individual customer from retrieved list" $ do
        cs <- CCUS.listCustomers
        res <- CCUS.getCustomer $ head cs
        case res of
             Nothing -> expectationFailure "Failed to get customer that should exist"
             _ -> pure ()
      it "should throw an HttpException for not-found customer" $ do
        CCUS.getCustomer CCUS.badIdCustomer `shouldThrow` isHttpException
