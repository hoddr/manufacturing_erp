module Spec.Orders
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
import qualified Client.Orders as CO
import Spec.Common ( isHttpException )

spec :: Spec
spec = do
  describe "Orders -> postOrder" $ do
    it "should post order and return priced line items" $ do
      res <- CO.postOrder "FS" CP.testRlis
      case res of
           [] -> expectationFailure "Should not be empty list"
           _ -> return ()
    it "should throw HttpException for no line items" $ do
      CO.postOrder "FS" [] `shouldThrow` isHttpException
    it "should throw HttpException for invalid order type" $ do
      CO.postOrder "FL" CP.testRlis `shouldThrow` isHttpException

  describe "Orders -> listOrders" $ do
    it "should always return a list (empty or not) of orders" $ do
      _ <- CO.listOrders
      return ()
