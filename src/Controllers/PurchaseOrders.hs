{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.PurchaseOrders
Description: Controllers and handles for all purchase order routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all purchase order routes.

-}
module Controllers.PurchaseOrders
  ( -- * Purchase Orders
    gPurchaseOrders
  , gPurchaseOrderById
  , gPurchaseOrderByNumber
  , pPurchaseOrder
  , ePurchaseOrder
  , dPurchaseOrder
  , ePurchaseOrderQBSet
  , ePurchaseOrderAsSent
  , ePurchaseOrderAsReceived
  , ePurchaseOrderPricesVerified
  , ePurchaseOrderPricesAdjusted
  , ePurchaseOrderAsClosed
  , pPurchaseOrderItem
  , dPurchaseOrderItem
  , ePurchaseOrderItem
  , gPurchaseOrderRfq
  , gPurchaseOrderReport
  , pBackOrder
  )
  where

import Control.Monad ( (>=>) )
import Data.Text ( Text )
import Network.Wai ( Response )
import Network.HTTP.Types ( status200
                          , status201
                          , status204
                          )

-- LOCAL --
import APITypes ( Context(..) )
import Controllers.Utils ( EIntParam
                         , errReqBody
                         , errUriParam
                         , getPagQS
                         , reqBodyReader
                         , sendHtmlGenResp
                         , sendResp
                         )
import PurchaseOrders ( addPurchaseOrder
                      , addPOItem
                      , createBackOrder
                      , deletePurchaseOrder
                      , editPOItem
                      , editPurchaseOrder
                      , getPOReport
                      , getPORfq
                      , getPurchaseOrderById
                      , getPurchaseOrderByNumber
                      , listPurchaseOrderSkeletons
                      , removePOItem
                      , setPOAsPriceAdjusted
                      , setPOAsPriceVerified
                      , setPOAsQBDone
                      , setPOAsSent
                      , setPOAsReceived
                      , setPOAsClosed
                      )

-- | GET List purchase orders based on querystring and closed/open flag. Returns 200 'APITypes.PurchaseOrder' list on success.
gPurchaseOrders :: Bool -> Context -> IO Response
gPurchaseOrders showClosed ctx@(Context (req, _)) = listPurchaseOrderSkeletons ctx showClosed (getPagQS req) >>= sendResp status200

-- | GET Retrieve purchase order by id. Returns 200 'APITypes.PurchaseOrder' on success.
gPurchaseOrderById :: EIntParam -> Context -> IO Response
gPurchaseOrderById (Left _) _ = errUriParam "purchase order"
gPurchaseOrderById (Right poid) ctx = getPurchaseOrderById ctx poid >>= sendResp status200

-- | GET Retrieves purchase order by po number. Returns 'APITypes.PurchaseOrder' on success.
gPurchaseOrderByNumber :: Text -> Context -> IO Response
gPurchaseOrderByNumber poNum ctx = getPurchaseOrderByNumber ctx poNum >>= sendResp status200

-- | POST Creates a new purchase order. Returns 201 on success.
pPurchaseOrder :: Context -> IO Response
pPurchaseOrder ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addPurchaseOrder ctx >=> sendResp status201)

-- | PUT Edits the purchase order. Returns 200 on success.
ePurchaseOrder :: EIntParam -> Context -> IO Response
ePurchaseOrder (Left _) _ = errUriParam "purchase order"
ePurchaseOrder (Right poid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (editPurchaseOrder ctx poid >=> sendResp status200)

-- | DELETE Deletes the purchase order. Returns 204 on success.
dPurchaseOrder :: EIntParam -> Context -> IO Response
dPurchaseOrder (Left _) _ = errUriParam "purchase order"
dPurchaseOrder (Right poid) ctx = deletePurchaseOrder ctx poid >>= sendResp status204

-- | PUT Marks the purchase order as sent to QB. Returns 201 on success.
ePurchaseOrderQBSet :: EIntParam -> Context -> IO Response
ePurchaseOrderQBSet (Left _) _ = errUriParam "purchase order"
ePurchaseOrderQBSet (Right poid) ctx = setPOAsQBDone ctx poid >>= sendResp status201

-- | PUT Marks the purchase order as sent. Marks items as ON ORDER Returns 201 on success.
ePurchaseOrderAsSent :: EIntParam -> Context -> IO Response
ePurchaseOrderAsSent (Left _) _ = errUriParam "purchase order"
ePurchaseOrderAsSent (Right poid) ctx = setPOAsSent ctx poid >>= sendResp status201

-- | PUT Marks the purchase order as received. Moves items from ON ORDER to ON HAND. Returns 201 on success.
ePurchaseOrderAsReceived :: EIntParam -> Context -> IO Response
ePurchaseOrderAsReceived (Left _) _ = errUriParam "purchase order"
ePurchaseOrderAsReceived (Right poid) ctx = setPOAsReceived ctx poid >>= sendResp status201

-- | PUT Marks the purchase order as price(s) verified. Returns 201 on success.
ePurchaseOrderPricesVerified :: EIntParam -> Context -> IO Response
ePurchaseOrderPricesVerified (Left _) _ = errUriParam "purchase order"
ePurchaseOrderPricesVerified (Right poid) ctx = setPOAsPriceVerified ctx poid >>= sendResp status201

-- | PUT Marks the purchase order as price(s) adjusted. This performs the pricing merge with the database. Returns 201 on success.
ePurchaseOrderPricesAdjusted :: EIntParam -> Context -> IO Response
ePurchaseOrderPricesAdjusted (Left _) _ = errUriParam "purchase order"
ePurchaseOrderPricesAdjusted (Right poid) ctx = setPOAsPriceAdjusted ctx poid >>= sendResp status201

-- | PUT Marks the purchase order as closed. Returns 201 on success.
ePurchaseOrderAsClosed :: EIntParam -> Context -> IO Response
ePurchaseOrderAsClosed (Left _) _ = errUriParam "purchase order"
ePurchaseOrderAsClosed (Right poid) ctx = setPOAsClosed ctx poid >>= sendResp status201

-- | POST Adds purchase order line item. Returns 201 on success.
pPurchaseOrderItem :: EIntParam -> Context -> IO Response
pPurchaseOrderItem (Left _) _ = errUriParam "purchase order"
pPurchaseOrderItem (Right poid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addPOItem ctx poid False >=> sendResp status201)

-- | DELETE Deletes the purchase order item. Returns 204 on success.
dPurchaseOrderItem :: EIntParam -> EIntParam -> Context -> IO Response
dPurchaseOrderItem (Left _) _ _ = errUriParam "purchase order"
dPurchaseOrderItem _ (Left _) _ = errUriParam "purchase order item"
dPurchaseOrderItem (Right poid) (Right poiid) ctx = removePOItem ctx poid poiid >>= sendResp status204

-- | PUT Edits the purchase order item. Some fields cannot be adjusted. Returns 201 on success.
ePurchaseOrderItem :: EIntParam -> EIntParam -> Context -> IO Response
ePurchaseOrderItem (Left _) _ _ = errUriParam "purchase order"
ePurchaseOrderItem _ (Left _) _ = errUriParam "purchase order item"
ePurchaseOrderItem (Right poid) (Right poiid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (editPOItem ctx poid poiid >=> sendResp status201)

-- | GET Generates an RFQ HTML template for the PO. Returns 200 and HTML on success.
gPurchaseOrderRfq :: EIntParam -> Context -> IO Response
gPurchaseOrderRfq (Left _) _ = errUriParam "purchase order"
gPurchaseOrderRfq (Right poid) ctx = getPORfq ctx poid >>= sendHtmlGenResp

-- | GET Generates a PO HTML template. Returns 200 and HTML on success.
gPurchaseOrderReport :: EIntParam -> Context -> IO Response
gPurchaseOrderReport (Left _) _ = errUriParam "purchase order"
gPurchaseOrderReport (Right poid) ctx = getPOReport ctx poid >>= sendHtmlGenResp

-- | POST Generates a back order, splitting items off from the main PO. Returns 201 on success.
pBackOrder :: EIntParam -> Context -> IO Response
pBackOrder (Left _) _ = errUriParam "purchase order"
pBackOrder (Right poid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (createBackOrder ctx poid >=> sendResp status201)
