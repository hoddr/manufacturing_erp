{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Data.PurchaseOrders
Description: Data type declarations for purchase orders.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Data type declarations for purchase orders, including purchase order items and back orders.

-}

module Data.PurchaseOrders
  ( POItem(..)
  , PurchaseBackOrder(..)
  , PurchaseOrder(..)
  )
  where

import Control.Monad ( liftM3 )
import Data.Aeson ( FromJSON
                  , ToJSON
                  , object
                  , parseJSON
                  , toJSON
                  , toJSONList
                  , withObject
                  , (.=)
                  , (.:)
                  , (.:?)
                  )
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Data.Time ( UTCTime(..) )
import Database.PostgreSQL.Simple.FromRow ( FromRow
                                          , field
                                          , fromRow
                                          )
import Database.PostgreSQL.Simple.ToField ( toField )
import Database.PostgreSQL.Simple.ToRow ( ToRow
                                        , toRow
                                        )

-- LOCAL --
import Data.CustomersVendors ( Vendor(..) )

-- | Non line-item fields for purchase orders.
data PurchaseOrder = PurchaseOrder { po_id :: Int
                                   , po_number :: Text
                                   , po_vendor :: Vendor
                                   , po_memo :: Text
                                   , po_expectedReceiptDate :: Maybe UTCTime
                                   , po_isOrderInQB :: Bool
                                   , po_inQBDate :: Maybe UTCTime
                                   , po_isOrderSent :: Bool
                                   , po_sentDate :: Maybe UTCTime
                                   , po_isOrderReceived :: Bool
                                   , po_receiptDate :: Maybe UTCTime
                                   , po_isPriceVerified :: Bool
                                   , po_priceVerifyDate :: Maybe UTCTime
                                   , po_isPriceAdjusted :: Bool
                                   , po_priceAdjustDate :: Maybe UTCTime
                                   , po_isOrderClosed :: Bool
                                   , po_closeDate :: Maybe UTCTime
                                   , po_isBackOrder :: Bool
                                   , po_idAdj :: Maybe Int
                                   , po_items :: [POItem]
                                   } deriving (Show)

instance ToJSON PurchaseOrder where
  toJSON po = object [ "id" .= po_id po
                     , "number" .= po_number po
                     , "vendor" .= toJSON (po_vendor po)
                     , "memo" .= po_memo po
                     , "expectedReceiptDate" .= po_expectedReceiptDate po
                     , "isOrderInQB" .= po_isOrderInQB po
                     , "inQBDate" .= po_inQBDate po
                     , "isOrderSent" .= po_isOrderSent po
                     , "sentDate" .= po_sentDate po
                     , "isOrderReceived" .= po_isOrderReceived po
                     , "receiptDate" .= po_receiptDate po
                     , "isPriceVerified" .= po_isPriceVerified po
                     , "priceVerifyDate" .= po_priceVerifyDate po
                     , "isPriceAdjusted" .= po_isPriceAdjusted po
                     , "priceAdjustDate" .= po_priceAdjustDate po
                     , "isOrderClosed" .= po_isOrderClosed po
                     , "closeDate" .= po_closeDate po
                     , "isBackOrder" .= po_isBackOrder po
                     , "items" .= toJSONList (po_items po)
                     ]

instance FromJSON PurchaseOrder where
  parseJSON = withObject "purchaseOrder" $ \po -> do
    poid <- po .:? "id"
    number <- po .: "number"
    vobj <- po .: "vendor"
    v <- parseJSON vobj
    memo <- po .: "memo"
    expDate <- po .:? "expectedReceiptDate"
    isInQB <- po .: "isOrderInQB"
    inQBDate <- po .:? "inQBDate"
    isSent <- po .: "isOrderSent"
    sentDate <- po .:? "sentDate"
    isReceived <- po .: "isOrderReceived"
    receiptDate <- po .:? "receiptDate"
    isPriceVerified <- po .: "isPriceVerified"
    pvDate <- po .:? "priceVerifyDate"
    isPriceAdjusted <- po .: "isPriceAdjusted"
    paDate <- po .:? "priceAdjustDate"
    isClosed <- po .: "isOrderClosed"
    closeDate <- po .:? "closeDate"
    isBackOrder <- po .: "isBackOrder"
    idAdj <- po .:? "idAdj"
    items <- po .:? "items"
    pure $ PurchaseOrder { po_id = fromMaybe 0 poid
                         , po_number = number
                         , po_vendor = v
                         , po_memo = memo
                         , po_expectedReceiptDate = expDate
                         , po_isOrderInQB = isInQB
                         , po_inQBDate = inQBDate
                         , po_isOrderSent = isSent
                         , po_sentDate = sentDate
                         , po_isOrderReceived = isReceived
                         , po_receiptDate = receiptDate
                         , po_isPriceVerified = isPriceVerified
                         , po_priceVerifyDate = pvDate
                         , po_isPriceAdjusted = isPriceAdjusted
                         , po_priceAdjustDate = paDate
                         , po_isOrderClosed = isClosed
                         , po_closeDate = closeDate
                         , po_isBackOrder = isBackOrder
                         , po_idAdj = idAdj
                         , po_items = fromMaybe [] items
                         }

instance FromRow PurchaseOrder where
  fromRow = PurchaseOrder <$> field
                          <*> field
                          <*> liftM3 Vendor field field field
                          <*> field
                          <*> field
                          <*> field
                          <*> field
                          <*> field
                          <*> field
                          <*> field
                          <*> field
                          <*> field
                          <*> field
                          <*> field
                          <*> field
                          <*> field
                          <*> field
                          <*> field
                          <*> pure Nothing
                          <*> pure []

instance ToRow PurchaseOrder where
  toRow po = [ toField (po_number po)
             , toField (vdid $ po_vendor po)
             , toField (po_memo po)
             , toField (po_expectedReceiptDate po)
             , toField (po_isOrderInQB po)
             , toField (po_isOrderSent po)
             , toField (po_isOrderReceived po)
             , toField (po_isOrderClosed po)
             , toField (po_isBackOrder po)
             ]

-- | Purchase order sub items. Retains key fields for populating RFQs, POs, and updating costs/inventory.
data POItem = POItem { poi_id :: Int
                     , poi_poId :: Int -- foreign key
                     , poi_name :: Text
                     , poi_desc :: Text
                     , poi_refId :: Int
                     , poi_refType :: Text
                     , poi_balanceCategory :: Text
                     , poi_quantity :: Double
                     , poi_unitsPerQuantity :: Maybe Double
                     , poi_purchasePriceEach :: Double
                     , poi_note :: Text
                     , poi_leadTime :: Maybe Text
                     , poi_vendorPartNumber :: Maybe Text
                     } deriving (Show)

instance ToJSON POItem where
  toJSON poi = object [ "id" .= poi_id poi
                      , "poId" .= poi_poId poi
                      , "name" .= poi_name poi
                      , "description" .= poi_desc poi
                      , "referenceId" .= poi_refId poi
                      , "referenceType" .= poi_refType poi
                      , "balanceCategory" .= poi_balanceCategory poi
                      , "quantity" .= poi_quantity poi
                      , "unitsPerQuantity" .= poi_unitsPerQuantity poi
                      , "purchasePriceEach" .= poi_purchasePriceEach poi
                      , "note" .= poi_note poi
                      , "leadTime" .= poi_leadTime poi
                      , "partNumber" .= poi_vendorPartNumber poi
                      ]

instance FromJSON POItem where
  parseJSON = withObject "item" $ \poi -> do
    poiid <- poi .:? "id"
    poId <- poi .: "poId"
    name <- poi .: "name"
    desc <- poi .: "description"
    refId <- poi .: "referenceId"
    refType <- poi .: "referenceType"
    balanceCat <- poi .: "balanceCategory"
    quant <- poi .: "quantity"
    unitsPerQuantity <- poi .:? "unitsPerQuantity"
    purchasePriceEach <- poi .: "purchasePriceEach"
    note <- poi .:? "note"
    leadTime <- poi .:? "leadTime"
    pure $ POItem { poi_id = fromMaybe 0 poiid
                  , poi_poId = poId
                  , poi_name = name
                  , poi_desc = desc
                  , poi_refId = refId
                  , poi_refType = refType
                  , poi_balanceCategory = balanceCat
                  , poi_quantity = quant
                  , poi_unitsPerQuantity = unitsPerQuantity
                  , poi_purchasePriceEach = purchasePriceEach
                  , poi_note = fromMaybe "" note
                  , poi_leadTime = leadTime
                  , poi_vendorPartNumber = Nothing -- retrieved from linked item in db
                  }

instance FromRow POItem where
  fromRow = POItem <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field

instance ToRow POItem where
  toRow poi = [ toField (poi_poId poi)
              , toField (poi_desc poi)
              , toField (poi_refId poi)
              , toField (poi_refType poi)
              , toField (poi_balanceCategory poi)
              , toField (poi_quantity poi)
              , toField (poi_unitsPerQuantity poi)
              , toField (poi_purchasePriceEach poi)
              , toField (poi_note poi)
              , toField (poi_leadTime poi)
              ]

-- | Purchase order back order temporary tracker. Back orders end up being generated as an additional PO.
data PurchaseBackOrder = PurchaseBackOrder { pbo_id :: Int
                                           , pbo_qty :: Double
                                           } deriving (Show)

instance FromJSON PurchaseBackOrder where
  parseJSON = withObject "purchaseBackOrder" $ \pbo -> do
    pboid <- pbo .: "id"
    qty <- pbo .: "quantity"
    pure $ PurchaseBackOrder { pbo_id = pboid
                             , pbo_qty = qty
                             }
