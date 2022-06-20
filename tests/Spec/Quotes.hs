module Spec.Quotes
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
import qualified Client.Orders as CO
-- import qualified Client.Projects as CP
import qualified Client.Quotes as CQ
import Spec.Common ( isHttpException )


spec :: Spec
spec = do
  describe "Quotes" $ do
    describe "list quotes" $ do
      it "should retrieve list of quotes (empty on first run!)" $ do
        _ <- CQ.listQuotes
        pure ()
    describe "add + delete" $ do
      it "should add quote (sans line items) and then permit deletion" $ do
        res <- CQ.addQuoteNoLineItems $ CQ.testQuoteNoLineItems "SQS-349"
        case res of
             Nothing -> expectationFailure "Failed to add quote w/o line items"
             _ -> do
               qs <- CQ.listQuotes
               _ <- CQ.deleteQuote $ head qs
               pure ()
    describe "add QL quote with line items, set to order" $ do
      it "add order quote, set to order, attempt delete (last step should throw HttpException)" $ do
        res <- CQ.addQuote "QL" $ CQ.testRlis "SP-919"
        case res of
             Nothing -> expectationFailure "Failed to add quote w/ line items"
             _ -> do
               qs <- CQ.listQuotes
               _ <- CQ.quoteToOrder (last qs) CQ.testOrder
               os <- CO.listOrders
               _ <- CO.deleteOrder (last os)
               pure ()

      it "add order quote, set to order, attempt delete (last step should throw HttpException)" $ do
        res <- CQ.addQuote "QL" $ CQ.testRlis "SP-101"
        case res of
             Nothing -> expectationFailure "Failed to add quote w/ line items"
             _ -> do
               qs <- CQ.listQuotes
               _ <- CQ.quoteToOrder (last qs) CQ.testOrder
               CQ.deleteQuote (last qs) `shouldThrow` isHttpException
      it "should throw HttpException if no line items given" $ do
        CQ.addQuote "QL" [] `shouldThrow` isHttpException
    describe "add project quote, set to project, attempt delete " $ do
      it "should work with valid input, (last step should throw HttpException)" $ do
        res <- CQ.addQuoteNoLineItems $ CQ.testQuoteNoLineItems "SIF-932"
        case res of
             Nothing -> expectationFailure "Failed to add quote w/o line items"
             _ -> do
               qs <- CQ.listQuotes
               _ <- CQ.quoteToProject (last qs) $ CQ.testProject (last qs) "Meijer 123" 1413
               CQ.deleteQuote (last qs) `shouldThrow` isHttpException

    describe "edit quote" $ do
      it "should edit new quote with valid input" $ do
        res <- CQ.addQuoteNoLineItems CQ.testQuote
        case res of
             Nothing -> expectationFailure "Failed to add quote w/o line items"
             _ -> do
               qs <- CQ.listQuotes
               _ <- CQ.editQuote (last qs)
               pure ()
