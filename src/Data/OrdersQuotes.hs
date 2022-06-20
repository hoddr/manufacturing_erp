{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Data.OrdersQuotes
Description: Data type declarations for orders and quotes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Data type declarations for both orders and quotes, as well as related reports.

-}

module Data.OrdersQuotes
  ( DetailedSalesData(..)
  , LineItem(..)
  , Order(..)
  , MembrainOrderData(..)
  , Quote(..)
  , RawLineItem(..)
  )
  where

import Data.Aeson ( FromJSON
                  , ToJSON
                  , Value(Object)
                  , object
                  , parseJSON
                  , toJSON
                  , toJSONList
                  , withObject
                  , (.=)
                  , (.:)
                  , (.:?)
                  )
import qualified Data.Csv as C
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
import Data.CustomersVendors ( Customer(..) )
import Data.Pricing ( LaborRate(..)
                    , Markup(..)
                    , defLabor
                    , defMarkup
                    )
import Data.Utils ( defDate
                  , formatDate
                  , isFitting
                  , liftM7
                  , liftM14
                  )

-- | Main order data structure. Contains quite a few fields that are used only for projects.
data Order = Order { oid :: Int
                   , otype :: Text
                   , opo :: Text
                   , ocreated :: Maybe UTCTime
                   , ocust :: Customer
                   , onum :: Text
                   , ototal :: Double
                   , oisWrapped :: Bool
                   , oisBilled :: Bool
                   , obilled :: Maybe UTCTime
                   , oinvoiceNumber :: Maybe Text
                   , oprojectId :: Maybe Int
                   , osectionId :: Maybe Int
                   , oisProgressiveBill :: Bool
                   , oisComplete :: Bool
                   , ocomplete :: Maybe UTCTime
                   , olabor :: LaborRate
                   , omarkups :: Markup
                   , ocustomerMarkup :: Maybe Double
                   , olineItems :: [LineItem]
                   } deriving (Show)

instance ToJSON Order where
  toJSON o = object [ "id" .= oid o
                    , "type" .= otype o
                    , "po" .= opo o
                    , "customer" .= toJSON (ocust o)
                    , "number" .= onum o
                    , "createdAt" .= ocreated o
                    , "total" .= ototal o
                    , "isWrapped" .= oisWrapped o
                    , "isBilled" .= oisBilled o
                    , "billedAt" .= obilled o
                    , "invoiceNumber" .= oinvoiceNumber o
                    , "projectId" .= oprojectId o
                    , "sectionId" .= osectionId o
                    , "isProgressiveBill" .= oisProgressiveBill o
                    , "isComplete" .= oisComplete o
                    , "completeAt" .= ocomplete o
                    , "laborRate" .= toJSON (olabor o)
                    , "markups" .= toJSON (omarkups o)
                    , "customerMarkup" .= ocustomerMarkup o
                    , "lineItems" .= toJSONList (olineItems o)
                    ]

instance FromJSON Order where
  parseJSON = withObject "order" $ \o -> do
    ordid <- o .:? "id"
    ordtype <- o .: "type"
    ordpo <- o .: "po"
    ocustomerObj <- o .: "customer"
    customer <- parseJSON ocustomerObj
    ordnum <- o .: "number"
    ordcreated <- o .:? "createdAt"
    isWrapped <- o .: "isWrapped"
    isBilled <- o .: "isBilled"
    billed <- o .:? "billedAt"
    invNum <- o .:? "invoiceNumber"
    projectId <- o .:? "projectId"
    sectionId <- o .:? "sectionId"
    isProgressiveBill <- o .:? "isProgressiveBill"
    isComplete <- o .:? "isComplete"
    complete <- o .:? "completeAt"
    ordLineItems <- o .:? "lineItems"
    pure $ Order { oid = fromMaybe 0 ordid
                 , otype = ordtype
                 , opo = ordpo
                 , ocust = customer
                 , onum = ordnum
                 , ocreated = ordcreated
                 , ototal = 0.00 -- placeholder
                 , oisWrapped = isWrapped
                 , oisBilled = isBilled
                 , obilled = billed
                 , oinvoiceNumber = invNum
                 , oprojectId = projectId
                 , osectionId = sectionId
                 , oisProgressiveBill = fromMaybe False isProgressiveBill
                 , oisComplete = fromMaybe False isComplete
                 , ocomplete = complete
                 , olabor = defLabor
                 , omarkups = defMarkup
                 , ocustomerMarkup = Nothing
                 , olineItems = fromMaybe [] ordLineItems
                 }

instance FromRow Order where
  fromRow = Order <$> field
                  <*> field
                  <*> field
                  <*> field
                  <*> liftM7 Customer field field field field field field field
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
                  <*> liftM7 LaborRate field field field field field field field
                  <*> liftM14 Markup field field field field field field field field field field field field field field
                  <*> field
                  <*> pure []


-- ORDER / SALES REPORTS
-- | newtype instance for reporting sales data within date range for Membrain.
newtype MembrainOrderData = MembrainOrderData Order deriving (Show)

instance C.ToNamedRecord MembrainOrderData where
  toNamedRecord (MembrainOrderData o) =
    C.namedRecord [ "Name" C..= opo o
                  , "Company: Name" C..= cname (ocust o)
                  , "orderNum" C..= onum o
                  , "Deal Type" C..= otype o
                  , "Value" C..= ototal o
                  , "Starting Date" C..= formatDate (fromMaybe defDate (ocreated o))
                  , "Closing Date" C..= formatDate (fromMaybe defDate (ocreated o))
                  , "Owner" C..= ("DC" :: Text)
                  , "Status" C..= ("won" :: Text)
                  ]

instance C.DefaultOrdered MembrainOrderData where
  headerOrder _ = C.header [ "Name"
                           , "Company: Name"
                           , "orderNum"
                           , "Deal Type"
                           , "Value"
                           , "Starting Date"
                           , "Closing Date"
                           , "Owner"
                           , "Status"
                           ]

-- | newtype instance for reporting detailed sales data (by line item) for Membrain.
newtype DetailedSalesData = DetailedSalesData ( Maybe UTCTime -- created at
                                              , Customer
                                              , Text -- number (quote/order)
                                              , LaborRate -- labor rate
                                              , Markup -- markups
                                              , Maybe Double -- customer markup
                                              , LineItem
                                              ) deriving (Show)

instance C.ToNamedRecord DetailedSalesData where
  toNamedRecord (DetailedSalesData (createdDate, customer, number, lr, mkups, mcustMarkup, li)) =
    C.namedRecord [ "Date" C..= formatDate (fromMaybe defDate createdDate)
                  , "Sales Order" C..= number
                  , "Customer" C..= cname customer
                  , "Description" C..= ldesc li
                  , "Quantity" C..= lquant li
                  , "Category" C..= lcategory li
                  , "IsExtra" C..= if lisExtra li then ("T" :: String) else ("F" :: String)
                  , "IsFitting" C..= if isFitting (lcategory li) (ldesc li) then ("T" :: String) else ("F" :: String)
                  , "Weight" C..= fromMaybe 0.00 (lweight li)
                  , "Base Material" C..= lmatName li
                  , "Base Material Weight" C..= lmatWeight li
                  , "Base Material Cost" C..= lmatCost li
                  , "Liner" C..= llinerName li
                  , "Liner Area" C..= llinerArea li
                  , "Liner Cost" C..= llinerCost li
                  , "Skin" C..= lskinName li
                  , "Skin Weight" C..= lskinWeight li
                  , "Skin Cost" C..= lskinCost li
                  , "Accessory Cost" C..= laccessoryCost li
                  , "Labor" C..= llabor li
                  , "Labor Cost" C..= llaborCost li
                  , "Material Cost" C..= ( fromMaybe 0.00 (lmatCost li)
                                         + fromMaybe 0.00 (llinerCost li)
                                         + fromMaybe 0.00 (lskinCost li)
                                         + fromMaybe 0.00 (laccessoryCost li)
                                         )
                  , "Total Cost" C..= ( fromMaybe 0.00 (lmatCost li)
                                      + fromMaybe 0.00 (llinerCost li)
                                      + fromMaybe 0.00 (lskinCost li)
                                      + fromMaybe 0.00 (laccessoryCost li)
                                      + fromMaybe 0.00 (llaborCost li)
                                      )
                  , "Price Each" C..= lprice li
                  , "Length" C..= llength li
                  , "Purchase Markup" C..= mkuppurchase mkups
                  , "Fabrication Markup" C..= mkupfabrication mkups
                  , "Rectangular Markup" C..= mkuprectangular mkups
                  , "Round Markup" C..= mkupround mkups
                  , "Oval Markup" C..= mkupoval mkups
                  , "Stock Markup" C..= mkupstock mkups
                  , "Assembly Markup" C..= mkupassembly mkups
                  , "Shop Labor Rate" C..= lrshopRate lr
                  , "Overhead Labor Rate" C..= lroverheadRate lr
                  , "Total Labor Rate" C..= lrrate lr
                  , "Customer Markup" C..= fromMaybe 1.00 mcustMarkup
                  ]

instance C.DefaultOrdered DetailedSalesData where
  headerOrder _  = C.header [ "Date"
                            , "Sales Order"
                            , "Customer"
                            , "Description"
                            , "Quantity"
                            , "Category"
                            , "IsExtra"
                            , "IsFitting"
                            , "Weight"
                            , "Base Material"
                            , "Base Material Weight"
                            , "Base Material Cost"
                            , "Liner"
                            , "Liner Area"
                            , "Liner Cost"
                            , "Skin"
                            , "Skin Weight"
                            , "Skin Cost"
                            , "Accessory Cost"
                            , "Labor"
                            , "Labor Cost"
                            , "Material Cost"
                            , "Total Cost"
                            , "Price Each"
                            , "Length"
                            , "Purchase Markup"
                            , "Fabrication Markup"
                            , "Rectangular Markup"
                            , "Round Markup"
                            , "Oval Markup"
                            , "Stock Markup"
                            , "Assembly Markup"
                            , "Shop Labor Rate"
                            , "Overhead Labor Rate"
                            , "Total Labor Rate"
                            , "Customer Markup"
                            ]

-- | Quote structure (may hold line items) with relevant fields. A decent amount of quote information is stored in Membrain.
data Quote = Quote { qid :: Int
                   , qtype :: Text
                   , qcreated :: Maybe UTCTime
                   , qnum :: Text
                   , qpo :: Maybe Text
                   , qordId :: Maybe Int
                   , qordNum :: Maybe Text
                   , qprjId :: Maybe Int
                   , qprjNum :: Maybe Int
                   , qsetPrice :: Maybe Double
                   , qcust :: Customer
                   , qtotal :: Double
                   , qisWrapped :: Bool
                   , qlaborRate :: LaborRate
                   , qmarkups :: Markup
                   , qcustomerMarkup :: Maybe Double
                   , qlineItems :: [LineItem]
                   } deriving (Show)

instance ToJSON Quote where
  toJSON q = object [ "id" .= qid q
                    , "type" .= qtype q
                    , "customer" .= toJSON (qcust q)
                    , "number" .= qnum q
                    , "quotepo" .= qpo q
                    , "orderId" .= qordId q
                    , "orderNumber" .= qordNum q
                    , "projectId" .= qprjId q
                    , "projectNumber" .= qprjNum q
                    , "createdAt" .= qcreated q
                    , "setPrice" .= qsetPrice q
                    , "total" .= qtotal q
                    , "isWrapped" .= qisWrapped q
                    , "laborRate" .= qlaborRate q
                    , "markups" .= qmarkups q
                    , "customerMarkup" .= qcustomerMarkup q
                    , "lineItems" .= toJSONList (qlineItems q)
                    ]

instance FromJSON Quote where
  parseJSON = withObject "quote" $ \q -> do
    quoteid <- q .:? "id"
    quotetype <- q .: "type"
    customerObj <- q .: "customer"
    customer <- parseJSON customerObj
    quotenum <- q .: "number"
    quotepo <- q .:? "quotepo"
    quoteordId <- q .:? "orderId"
    quoteordNum <- q .:? "orderNumber"
    quoteprjId <- q .:? "projectId"
    quoteprjNum <- q .:? "projectNumber"
    quotesetPrice <- q .:? "setPrice"
    isWrapped <- q .: "isWrapped"
    quotelineItems <- q .:? "lineItems"
    quotecreated <- q .:? "createdAt"
    pure $ Quote { qid = fromMaybe 0 quoteid
                 , qtype = quotetype
                 , qcust = customer
                 , qnum = quotenum
                 , qpo = quotepo
                 , qordId = quoteordId
                 , qordNum = quoteordNum
                 , qprjId = quoteprjId
                 , qprjNum = quoteprjNum
                 , qsetPrice = quotesetPrice
                 , qcreated = quotecreated
                 , qtotal = 0.00 -- placeholder
                 , qisWrapped = isWrapped
                 , qlaborRate = defLabor
                 , qmarkups = defMarkup
                 , qcustomerMarkup = Nothing
                 , qlineItems = fromMaybe [] quotelineItems
                 }

instance FromRow Quote where
  fromRow = Quote <$> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> liftM7 Customer field field field field field field field
                  <*> field
                  <*> field
                  <*> liftM7 LaborRate field field field field field field field
                  <*> liftM14 Markup field field field field field field field field field field field field field field
                  <*> field
                  <*> pure []

-- | Individual line items for quotes, projects, and orders. They store all information necessary
-- to replicate their pricing.
data LineItem = LineItem { lid :: Int
                         , ldesc :: Text
                         , lquant :: Double
                         , llabor :: Double
                         , llaborCost :: Maybe Double
                         , lweight :: Maybe Double
                         , lisExtra :: Bool
                         , lprice :: Double
                         , lorderId :: Maybe Int
                         , lorderNumber :: Maybe Text
                         , lquoteId :: Maybe Int
                         , lquoteNumber :: Maybe Text
                         , lmatId :: Maybe Int
                         , lmatName :: Maybe Text
                         , lmatWeight :: Maybe Double
                         , lmatCost :: Maybe Double
                         , llinerId :: Maybe Int
                         , llinerName :: Maybe Text
                         , llinerArea :: Maybe Double
                         , llinerCost :: Maybe Double
                         , lskinId :: Maybe Int
                         , lskinName :: Maybe Text
                         , lskinWeight :: Maybe Double
                         , lskinCost :: Maybe Double
                         , laccessoryCost :: Maybe Double
                         , lcategory :: Text
                         , llength :: Double
                         , lquantFabbed :: Double -- for bd pieces
                         , lisFabbed :: Bool -- for bd pieces
                         } deriving (Show)

instance ToJSON LineItem where
  toJSON l = object [ "id" .= lid l
                    , "description" .= ldesc l
                    , "quantity" .= lquant l
                    , "labor" .= llabor l
                    , "laborCost" .= llaborCost l
                    , "weight" .= lweight l
                    , "isExtra" .= lisExtra l
                    , "price" .= lprice l
                    , "orderId" .= lorderId l
                    , "orderNumber" .= lorderNumber l
                    , "quoteId" .= lquoteId l
                    , "quoteNumber" .= lquoteNumber l
                    , "materialid" .= lmatId l
                    , "materialName" .= lmatName l
                    , "materialWeight" .= lmatWeight l
                    , "materialCost" .= lmatCost l
                    , "linerId" .= llinerId l
                    , "linerName" .= llinerName l
                    , "linerArea" .= llinerArea l
                    , "linerCost" .= llinerCost l
                    , "skinId" .= lskinId l
                    , "skinName" .= lskinName l
                    , "skinWeight" .= lskinWeight l
                    , "skinCost" .= lskinCost l
                    , "accessoryCost" .= laccessoryCost l
                    , "category" .= lcategory l
                    , "length" .= llength l
                    , "quantFabbed" .= lquantFabbed l
                    , "isFabbed" .= lisFabbed l
                    ]

instance FromJSON LineItem where
  parseJSON = withObject "lineItem" $ \l -> do
    liid <- l .:? "id"
    lidesc <- l .: "description"
    liquant <- l .: "quantity"
    lilabor <- l .: "labor"
    liweight <- l .:? "weight"
    liisExtra <- l .: "isExtra"
    liprice <- l .: "price"
    liorderId <- l .:? "orderId"
    liquoteId <- l .:? "quoteId"
    limatId <- l .:? "materialId"
    limatName <- l .:? "materialName"
    limatWeight <- l .:? "materialWeight"
    lilinerId <- l .:? "linerId"
    lilinerName <- l .:? "linerName"
    lilinerArea <- l .:? "linerArea"
    liskinId <- l .:? "skinId"
    liskinName <- l .:? "skinName"
    liskinWeight <- l .:? "skinWeight"
    liaccessoryCost <- l .:? "accessoryCost"
    licategory <- l .: "category"
    lilength <- l .:? "length"
    liquantFabbed <- l .:? "quantFabbed"
    liisFabbed <- l .:? "isFabbed"
    pure $ LineItem { lid = fromMaybe 0 liid
                    , ldesc = lidesc
                    , lquant = liquant
                    , llabor = lilabor
                    , llaborCost = Nothing
                    , lweight = liweight
                    , lisExtra = liisExtra
                    , lprice = liprice
                    , lorderId = liorderId
                    , lorderNumber = Nothing
                    , lquoteId = liquoteId
                    , lquoteNumber = Nothing
                    , lmatId = limatId
                    , lmatName = limatName
                    , lmatWeight = limatWeight
                    , lmatCost = Nothing
                    , llinerId = lilinerId
                    , llinerName = lilinerName
                    , llinerArea = lilinerArea
                    , llinerCost = Nothing
                    , lskinId = liskinId
                    , lskinName = liskinName
                    , lskinWeight = liskinWeight
                    , lskinCost = Nothing
                    , laccessoryCost = liaccessoryCost
                    , lcategory = licategory
                    , llength = fromMaybe 0.00 lilength
                    , lquantFabbed = fromMaybe 0.00 liquantFabbed
                    , lisFabbed = fromMaybe True liisFabbed
                    }

instance FromRow LineItem where
  fromRow = LineItem <$> field
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

instance ToRow LineItem where
  toRow l = [ toField (ldesc l)
            , toField (lquant l)
            , toField (llabor l)
            , toField (fromMaybe 0.00 $ llaborCost l)
            , toField (fromMaybe 0.00 $ lweight l)
            , toField (lisExtra l)
            , toField (lprice l)
            , toField (fromMaybe 0 $ lorderId l)
            , toField (fromMaybe 0 $ lquoteId l)
            , toField (lmatId l)
            , toField (lmatName l)
            , toField (lmatWeight l)
            , toField (lmatCost l)
            , toField (llinerId l)
            , toField (llinerName l)
            , toField (llinerArea l)
            , toField (llinerCost l)
            , toField (lskinId l)
            , toField (lskinName l)
            , toField (lskinWeight l)
            , toField (lskinCost l)
            , toField (laccessoryCost l)
            , toField (lcategory l)
            , toField (llength l)
            , toField (lquantFabbed l)
            , toField (lisFabbed l)
            ]

-- RAW LINE ITEM --
-- | External line item strucutre (e.g. from CAM and to QB).
data RawLineItem = RawLineItem { rli_customer :: Text
                               , rli_orderNumber :: Text
                               , rli_po :: Text
                               , rli_category :: Text
                               , rli_quantity :: Double
                               , rli_name :: Text
                               , rli_gauge :: Text
                               , rli_material :: Text
                               , rli_materialWeight :: Double
                               , rli_weight :: Double
                               , rli_linerMaterial :: Text
                               , rli_linerArea :: Double
                               , rli_skinGauge :: Text
                               , rli_skinMaterial :: Text
                               , rli_skinWeight :: Double
                               , rli_accessoryCost :: Double
                               , rli_rate :: Text
                               , rli_isWrapped :: Bool
                               , rli_priceEach :: Double
                               , rli_length :: Double
                               , rli_isCatalogue :: Bool
                               , rli_labor :: Maybe Double
                               } deriving (Show)

instance ToJSON RawLineItem where
  toJSON rli = object [ "customer" .= rli_customer rli
                      , "orderNumber" .= rli_orderNumber rli
                      , "po" .= rli_po rli
                      , "category" .= rli_category rli
                      , "quantity" .= rli_quantity rli
                      , "name" .= rli_name rli
                      , "gauge" .= rli_gauge rli
                      , "material" .= rli_material rli
                      , "materialWeight" .= rli_materialWeight rli
                      , "weight" .= rli_weight rli
                      , "linerMaterial" .= rli_linerMaterial rli
                      , "linerArea" .= rli_linerArea rli
                      , "skinGauge" .= rli_skinGauge rli
                      , "skinMaterial" .= rli_skinMaterial rli
                      , "skinWeight" .= rli_skinWeight rli
                      , "accessoryCost" .= rli_accessoryCost rli
                      , "rate" .= rli_rate rli
                      , "isWrapped" .= rli_isWrapped rli
                      , "priceEach" .= rli_priceEach rli
                      , "length" .= rli_length rli
                      , "isCatalogue" .= rli_isCatalogue rli
                      , "labor" .= rli_labor rli
                      ]

instance FromJSON RawLineItem where
  parseJSON (Object r) = RawLineItem
    <$> r .: "customer"
    <*> r .: "orderNumber"
    <*> r .: "po"
    <*> r .: "category"
    <*> r .: "quantity"
    <*> r .: "name"
    <*> r .: "gauge"
    <*> r .: "material"
    <*> r .: "materialWeight"
    <*> r .: "weight"
    <*> r .: "linerMaterial"
    <*> r .: "linerArea"
    <*> r .: "skinGauge"
    <*> r .: "skinMaterial"
    <*> r .: "skinWeight"
    <*> r .: "accessoryCost"
    <*> r .: "rate"
    <*> r .: "isWrapped"
    <*> r .: "priceEach"
    <*> r .: "length"
    <*> r .: "isCatalogue"
    <*> r .: "labor"
  parseJSON _ = mempty
