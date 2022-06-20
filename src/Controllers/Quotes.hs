{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.Quotes
Description: Controllers and handles for all quote routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all quote routes.

-}
module Controllers.Quotes
  ( -- * Quotes
    gProjectEstimate
  , gQuotes
  , gQuoteSkeletons
  , pQuote
  , pQuoteCatalog
  , pQuoteSans
  , eQuote
  , dQuote
  , gQuote
  , pQuoteToOrder
  , pQuoteToProject
  , gQuoteReport
  , gQuoteLineItemDetailReport
  )
  where

import Control.Monad ( (>=>) )
import Data.Aeson ( eitherDecode )
import Data.Text ( Text
                 , toUpper
                 )
import Network.Wai ( Response
                   , strictRequestBody
                   )
import Network.HTTP.Types ( status200
                          , status201
                          , status204
                          )

-- LOCAL --
import APITypes ( Context(..)
                , Order
                , Project(..)
                , Quote(..)
                , RawLineItem
                )
import Controllers.Utils ( EIntParam
                         , errJsonRes
                         , errReqBody
                         , errUriParam
                         , getPagQS
                         , reqBodyReader
                         , sendCsvResp
                         , sendHtmlGenResp
                         , sendResp
                         )
import Orders ( addOrderShell )
import Pricing ( getMarkupFlag
               , priceViaList
               , runEstimator
               , sanitizeRawLineItems
               )
import Projects ( addProject )
import Quotes ( addQuote
              , addQuoteSansLineItems
              , deleteQuote
              , editQuote
              , getProjectEstimate
              , getQuoteById
              , getQuoteDetailedSalesData
              , getQuoteReport
              , listQuoteSkeletons
              , listQuotes
              , setQuoteOrderId
              , setQuoteProjectId
              )
import Utils ( apiError400
             , apiMsg201
             , apiMsg204
             , eitherPassErr
             , fst5
             , isValidQuoteType
             )

-- | GET List quotes based on querystring. Returns 200 'APITypes.Quote' list on success.
gQuotes :: Context -> IO Response
gQuotes c = listQuotes c >>= sendResp status200

-- | GET List quotes sans line items based on querystring. Returns 200 'APITypes.Quote' list on success.
gQuoteSkeletons :: Context -> IO Response
gQuoteSkeletons ctx@(Context (req, _)) =
  let pagQS = getPagQS req in
    listQuoteSkeletons ctx pagQS >>= sendResp status200

-- | POST Creates new quote. Validates quote type first. Check pQuoteSteps for details. Returns 201 on success.
pQuote :: Text -> Context -> IO Response
pQuote quoteType ctx@(Context (req, _))
  | isValidQuoteType quoteType = reqBodyReader req
    >>= either errReqBody (pQuoteSteps ctx (toUpper quoteType))
  | otherwise = errJsonRes (apiError400 "Invalid quote type received. Must be one of QP, QL, or QB")

{- | POST Create quote helper.

First prices the line items, then adds quote with correct markups/wrap flag.

-}
pQuoteSteps :: Context -> Text -> [RawLineItem] -> IO Response
pQuoteSteps ctx quoteType rlis = do
  epricedRlis <- runEstimator ctx "QL" rlis
  case epricedRlis of
       (Left e) -> errJsonRes e
       (Right pricedRlis) ->
         pure (sanitizeRawLineItems False pricedRlis)
         >>= (\lisPlus ->
               addQuote ctx quoteType (getMarkupFlag $ map fst5 pricedRlis) lisPlus
               >>= eitherPassErr (\_ -> pure $ Right $ map fst5 pricedRlis)
                 >>= sendResp status201
             )

{- | POST Creates quote with pricing via a fixed pricing list.

Retrieves and prices via 'priceViaList' prior to passing on to pQuoteSteps. Returns 201 on success.

-}
pQuoteCatalog :: Text -> EIntParam -> Context -> IO Response
pQuoteCatalog _ (Left _) _ = errUriParam "pricing list"
pQuoteCatalog quoteType (Right catid) ctx@(Context (req, _))
  | isValidQuoteType quoteType = reqBodyReader req
    >>= either errReqBody (priceViaList ctx catid
      >=> either errJsonRes (pQuoteSteps ctx (toUpper quoteType)))
  | otherwise = errJsonRes (apiError400 "Invalid quote type received. Must be one of QP, QL, or QB")

-- | POST Creates a quote (QP) sans line items. Soon to be deprecated.
pQuoteSans :: Context -> IO Response
pQuoteSans c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addQuoteSansLineItems c >=> sendResp status201)

-- | GET Retrieves quote by id. Returns 200 'APITypes.Quote' on success.
gQuote :: EIntParam -> Context -> IO Response
gQuote (Left _) _ = errUriParam "quote"
gQuote (Right quoteId) c = getQuoteById c quoteId >>= sendResp status200

-- | PUT Edits the specified quote by id. Returns 200 'APITypes.Quote' on success.
eQuote :: EIntParam -> Context -> IO Response
eQuote (Left _) _ = errUriParam "quote"
eQuote (Right quoteId) c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\q -> editQuote c q quoteId
    >>= eitherPassErr (\_ -> pure $ Right q)
    >>= sendResp status200)

-- | DELETE Deletes the specified quote. Checks for associated orders or projects prior to deletion. Removes
dQuote :: EIntParam -> Context -> IO Response
dQuote (Left _) _ = errUriParam "quote"
dQuote (Right quoteId) ctx = getQuoteById ctx quoteId
  >>= eitherPassErr (\q ->
    case (qordId q, qprjId q) of
         (Just _, _) -> pure $ Left $ apiError400 "Quote has associated order; cannot delete"
         (_, Just _) -> pure $ Left $ apiError400 "Quote has associated project; cannot delete"
         (_, _) -> deleteQuote ctx quoteId
           >>= eitherPassErr (\_ -> pure $ Right $ apiMsg204 "Quote deleted")
  ) >>= sendResp status204

{- | POST Adds an order mapping to the quote.

Checks for a pre-existing FL order mapped to this quote - fails if exists.
Checks for pre-existing project mapped to this quote - fails if exists.
Checks for line items to create FL - fails if no line items are present.
Adds the order shell with fields that match the quote and passed order number and order po.
Maps the quote line items to the order line items.
Inventory is adjusted when order is marked as complete only.
Returns 201 'APITypes.APIMsg' on success.

-}
pQuoteToOrder :: EIntParam -> Context -> IO Response
pQuoteToOrder (Left _) _ = errUriParam "quote"
pQuoteToOrder (Right quoteId) ctx@(Context (req, _)) = getQuoteById ctx quoteId
  >>= eitherPassErr (\q -> do
    body <- strictRequestBody req
    case (qordId q, qprjId q, qlineItems q, eitherDecode body :: Either String Order) of
         (Just _, _, _, _) -> pure (Left $ apiError400 "FL order from quote already exists")
         (_, Just _, _, _) -> pure (Left $ apiError400 "Project from quote already exists - cannot make into an order")
         (_, _, [], _) -> pure (Left $ apiError400 "Cannot create FL order with no line items on quote")
         (_, _, _, Left e) -> print e >> pure (Left $ apiError400 "Invalid or empty request body")
         (_, _, _, Right o) -> addOrderShell ctx o (qmarkups q) (qlaborRate q)
         >>= eitherPassErr (setQuoteOrderId ctx q
           >=> eitherPassErr (\_ -> pure $ Right $ apiMsg201 "Quote order set: order generated")
         )
  )
  >>= sendResp status201

-- | POST Creates a new project mapped to the original quote information. Returns 201 on success.
pQuoteToProject :: EIntParam -> Context -> IO Response
pQuoteToProject (Left _) _ = errUriParam "quote"
pQuoteToProject (Right quoteId) ctx@(Context (req, _)) = getQuoteById ctx quoteId
  >>= eitherPassErr (\q -> do
    body <- strictRequestBody req
    case (qordId q, qprjId q, eitherDecode body :: Either String Project) of
         (Just _, _, _) -> pure (Left $ apiError400 "FL order from quote already exists")
         (_, Just _, _) -> pure (Left $ apiError400 "Project from quote already exists")
         (_, _, Left e) -> print e >> pure (Left $ apiError400 "Invalid or empty request body")
         (_, _, Right p) -> addProject ctx p
           >>= eitherPassErr (\newp -> setQuoteProjectId ctx q (prjid newp)
                                    >>= eitherPassErr (\_ -> pure $ Right newp))
  )
  >>= sendResp status201

-- | GET Generates a quote report. Returns 200 and HTML page on success.
gQuoteReport :: EIntParam -> Context -> IO Response
gQuoteReport (Left _) _ = errUriParam "quote"
gQuoteReport (Right quoteId) ctx = getQuoteReport ctx quoteId >>= sendHtmlGenResp

-- | GET Generates project estimate cover page. Returns 200 and HTML page on success.
gProjectEstimate :: EIntParam -> Context -> IO Response
gProjectEstimate (Left _) _ = errUriParam "quote"
gProjectEstimate (Right quoteId) ctx = getProjectEstimate ctx quoteId >>= sendHtmlGenResp

-- | GET Retrieves line item detailed quote report (CSV) for the given quote. Returns 200 and CSV on success.
gQuoteLineItemDetailReport :: EIntParam -> Context -> IO Response
gQuoteLineItemDetailReport (Left _) _ = errUriParam "quote"
gQuoteLineItemDetailReport (Right quoteId) ctx = getQuoteDetailedSalesData ctx quoteId >>= sendCsvResp
