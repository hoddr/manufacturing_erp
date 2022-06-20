{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Data.Items
Description: Type declarations for fabrication items, purchase items, and assemblies.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Data type declarations for fabrication items, purchase items, assemblies, and sub-items within assemblies. Also includes fixed pricing lists for these items.

-}

module Data.Items
  ( Assembly(..)
  , BuildAssemblyReport(..)
  , FabItem(..)
  , FixedPriceItem(..)
  , FlangeReportEntry(..)
  , PricingList(..)
  , PurchaseItem(..)
  , SubItem(..)
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
import qualified Data.Csv as C
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Time ( UTCTime(..) )
import Database.PostgreSQL.Simple.FromRow ( FromRow
                                          , field
                                          , fromRow
                                          )
import Database.PostgreSQL.Simple.ToField ( toField )
import Database.PostgreSQL.Simple.ToRow ( ToRow
                                        , toRow
                                        )
import Prelude hiding ( id
                      , pi
                      )

-- LOCAL --
import Data.CustomersVendors ( Vendor(..) )
import Data.Inventory ( Inventory(..) )
import Data.Utils ( liftM7 )

-- FAB ITEMS --
-- | Data type for fabricated items (e.g. fittings, stock spiral, etc). Each one has a mapped 'Inventory' entry.
data FabItem = FabItem { fiid :: Int
                       , finame :: Text
                       , fidescription :: Text
                       , fiunitQuantity :: Double
                       , filabor :: Double
                       , fimaterialId :: Int
                       , fimaterialName :: Text
                       , fimaterialCost :: Double -- calculated
                       , filaborCost :: Double -- calculated
                       , ficost :: Double -- calculated from (mat + labor)
                       , fiprice :: Double -- calculated from (mat + labor) * markup
                       , fiisStock :: Bool
                       , fibalanceCategory :: Text
                       , fiinventory :: Inventory
                       } deriving (Show)

instance ToJSON FabItem where
  toJSON fi = object [ "id" .= fiid fi
                     , "name" .= finame fi
                     , "description" .= fidescription fi
                     , "unitQuantity" .= fiunitQuantity fi
                     , "labor" .= filabor fi
                     , "materialId" .= fimaterialId fi
                     , "materialName" .= fimaterialName fi
                     , "materialCost" .= fimaterialCost fi
                     , "laborCost" .= filaborCost fi
                     , "cost" .= ficost fi
                     , "price" .= fiprice fi
                     , "isStock" .= fiisStock fi
                     , "balanceCategory" .= fibalanceCategory fi
                     , "inventory" .= toJSON (fiinventory fi)
                     ]

instance FromJSON FabItem where
  parseJSON = withObject "fabItem" $ \fi -> do
    id <- fi .:? "id"
    name <- fi .: "name"
    description <- fi .: "description"
    unitQuantity <- fi .: "unitQuantity"
    labor <- fi .: "labor"
    materialId <- fi .: "materialId"
    materialName <- fi .: "materialName"
    isStock <- fi .: "isStock"
    balanceCategory <- fi .: "balanceCategory"
    i <- fi .: "inventory"
    inv <- parseJSON i
    pure $ FabItem { fiid = fromMaybe 0 id
                   , finame = name
                   , fidescription = description
                   , fiunitQuantity = unitQuantity
                   , filabor = labor
                   , fimaterialId = materialId
                   , fimaterialName = materialName
                   , fimaterialCost = 0.00 -- placeholder, calculated
                   , filaborCost = 0.00 -- placeholder, calculated
                   , ficost = 0.00 -- placeholder, calculated at query time
                   , fiprice = 0.00 -- placeholder, calculated at query time with markup
                   , fiisStock = isStock
                   , fibalanceCategory = balanceCategory
                   , fiinventory = inv
                   }

instance FromRow FabItem where
  fromRow = FabItem <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> pure 0.00
                    <*> field
                    <*> field
                    <*> liftM7 Inventory field field field field field field field

instance ToRow FabItem where
  toRow f = [ toField (finame f)
            , toField (fidescription f)
            , toField (fiunitQuantity f)
            , toField (filabor f)
            , toField (fimaterialId f)
            , toField (fiisStock f)
            , toField (fibalanceCategory f)
            ]

-- PURCHASE ITEM --
-- | Data type for items purchased and resold (e.g. HETOs, flanges, etc).
-- Each one has a mapped 'Inventory' entry, whether or not inventory is tracked.
data PurchaseItem = PurchaseItem { piid :: Int
                                 , piname :: Text
                                 , pigenericName :: Text
                                 , piautodeskId :: Maybe Text
                                 , pidescription :: Text
                                 , piprice :: Double
                                 , pipreferredVendor :: Text
                                 , picost :: Double
                                 , pivendorPartNumber :: Maybe Text
                                 , pivendorCategory :: Maybe Text
                                 , pimarkup :: Double
                                 , pibalanceCategory :: Text
                                 , piisLocked :: Bool
                                 , pileadTime :: Maybe Text
                                 , pivendor :: Vendor
                                 , piinventory :: Inventory
                                 } deriving (Show)

instance ToJSON PurchaseItem where
  toJSON pi = object [ "id" .= piid pi
                     , "name" .= piname pi
                     , "genericName" .= pigenericName pi
                     , "autodeskId" .= piautodeskId pi
                     , "description" .= pidescription pi
                     , "price" .= piprice pi
                     , "preferredVendor" .= pipreferredVendor pi
                     , "cost" .= picost pi
                     , "vendorPartNumber" .= pivendorPartNumber pi
                     , "vendorCategory" .= pivendorCategory pi
                     , "markup" .= pimarkup pi
                     , "balanceCategory" .= pibalanceCategory pi
                     , "isLocked" .= piisLocked pi
                     , "leadTime" .= pileadTime pi
                     , "vendor" .= toJSON (pivendor pi)
                     , "inventory" .= toJSON (piinventory pi)
                     ]

instance FromJSON PurchaseItem where
  parseJSON = withObject "purchaseItem" $ \pi -> do
    id <- pi .:? "id"
    name <- pi .: "name"
    genericName <- pi .: "genericName"
    autodeskId <- pi .:? "autodeskId"
    description <- pi .: "description"
    price <- pi .: "price"
    preferredVendor <- pi .: "preferredVendor"
    cost <- pi .: "cost"
    vendorPartNumber <- pi .:? "vendorPartNumber"
    vendorCategory <- pi .:? "vendorCategory"
    markup <- pi .:? "markup"
    balanceCategory <- pi .: "balanceCategory"
    isLocked <- pi .: "isLocked"
    leadTime <- pi .: "leadTime"
    v <- pi .: "vendor"
    vendor <- parseJSON v
    i <- pi .: "inventory"
    inv <- parseJSON i
    pure $ PurchaseItem { piid = fromMaybe 0 id
                        , piname = name
                        , pigenericName = genericName
                        , piautodeskId = autodeskId
                        , pidescription = description
                        , piprice = price
                        , pipreferredVendor = preferredVendor
                        , picost = cost
                        , pivendorPartNumber = vendorPartNumber
                        , pivendorCategory = vendorCategory
                        , pimarkup = fromMaybe 1.00 markup -- default
                        , pibalanceCategory = balanceCategory
                        , piisLocked = isLocked
                        , pileadTime = leadTime
                        , pivendor = vendor
                        , piinventory = inv
                        }

instance FromRow PurchaseItem where
  fromRow = PurchaseItem <$> field
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
                         <*> liftM3 Vendor field field field
                         <*> liftM7 Inventory field field field field field field field

instance ToRow PurchaseItem where
  toRow p = [ toField (piname p)
            , toField (T.toUpper $ pigenericName p)
            , toField (T.toUpper <$> piautodeskId p)
            , toField (pidescription p)
            , toField (pipreferredVendor p)
            , toField (picost p)
            , toField (pivendorPartNumber p)
            , toField (pivendorCategory p)
            , toField (pimarkup p)
            , toField (pibalanceCategory p)
            , toField (piisLocked p)
            , toField (pileadTime p)
            , toField (vdid (pivendor p))
            ]

-- ASSEMBLIES AND SUB ITEMS --
-- | Assembly sub item entry.
data SubItem = SubItem { sub_id :: Int
                       , sub_assemblyId :: Int
                       , sub_inv :: Inventory
                       , sub_quantity :: Double
                       , sub_priceEach :: Double
                       } deriving (Show)

instance ToJSON SubItem where
  toJSON s = object [ "id" .= sub_id s
                    , "assemblyId" .= sub_assemblyId s
                    , "inventory" .= toJSON (sub_inv s)
                    , "quantity" .= sub_quantity s
                    , "priceEach" .= sub_priceEach s
                    ]

instance FromRow SubItem where
  fromRow = SubItem <$> field
                    <*> field
                    <*> liftM7 Inventory field field field field field field field
                    <*> field
                    <*> pure 0.00

instance FromJSON SubItem where
  parseJSON = withObject "subItem" $ \s -> do
    id <- s .:? "id"
    assemblyId <- s .: "assemblyId"
    invObj <- s .: "inventory"
    inv <- parseJSON invObj
    quantity <- s .: "quantity"
    p <- s .: "priceEach"
    pure $ SubItem { sub_id = fromMaybe 0 id
                   , sub_assemblyId = assemblyId
                   , sub_inv = inv
                   , sub_quantity = quantity
                   , sub_priceEach = p
                   }

instance ToRow SubItem where
  toRow s = [ toField (sub_assemblyId s)
            , toField (invid $ sub_inv s)
            , toField (sub_quantity s)
            ]

-- | Main assembly entry. Labor is only at the top-level. Sub items do not have labor.
data Assembly = Assembly { a_id :: Int
                         , a_name :: Text
                         , a_description :: Text
                         , a_labor :: Double
                         , a_laborPrice :: Double
                         , a_inv :: Inventory
                         , a_weight :: Double -- calc field
                         , a_price :: Double -- calc field
                         , a_subItems :: [SubItem]
                         } deriving (Show)

instance ToJSON Assembly where
  toJSON a = object [ "id" .= a_id a
                    , "name" .= a_name a
                    , "description" .= a_description a
                    , "labor" .= a_labor a
                    , "laborPrice" .= a_laborPrice a
                    , "inventory" .= a_inv a
                    , "weight" .= a_weight a
                    , "price" .= a_price a
                    , "subItems" .= toJSONList (a_subItems a)
                    ]

instance FromRow Assembly where
  fromRow = Assembly <$> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
                     <*> liftM7 Inventory field field field field field field field
                     <*> pure 0.00 -- calculated field -> weight
                     <*> pure 0.00 -- calculated field -> price
                     <*> pure []

instance FromJSON Assembly where
  parseJSON = withObject "assembly" $ \a -> do
    id <- a .:? "id"
    name <- a .: "name"
    description <- a .: "description"
    labor <- a .: "labor"
    lp <- a .: "laborPrice"
    invObj <- a .: "inventory"
    i <- parseJSON invObj
    subs <- a .: "subItems"
    pure $ Assembly { a_id = fromMaybe 0 id
                    , a_name = name
                    , a_description = description
                    , a_labor = labor
                    , a_laborPrice = lp
                    , a_inv = i
                    , a_weight = 0.00 -- calculated field
                    , a_price = 0.00 -- calculated field
                    , a_subItems = subs
                    }

instance ToRow Assembly where
  toRow a = [ toField (a_name a)
            , toField (a_description a)
            , toField (a_labor a)
            ]

-- | Helper to accumulate build assembly report data (name, total cost, material cost, labor cost, quantity).
newtype BuildAssemblyReport = BuildAssemblyReport ( Text, Double, Double, Double, Double ) deriving Show

instance C.ToNamedRecord BuildAssemblyReport where
  toNamedRecord (BuildAssemblyReport ( n, tot, mat, lab, qty )) =
    C.namedRecord [ "Name" C..= n
                  , "Total Cost" C..= tot
                  , "Material Cost" C..= mat
                  , "Labor Cost" C..= lab
                  , "Quantity" C..= qty
                  ]

instance C.DefaultOrdered BuildAssemblyReport where
  headerOrder _ = C.header [ "Name"
                           , "Total Cost"
                           , "Material Cost"
                           , "Labor Cost"
                           , "Quantity"
                           ]

instance Semigroup BuildAssemblyReport where
  (<>) (BuildAssemblyReport (n1, t1, m1, l1, q1))
       (BuildAssemblyReport (_, t2, m2, l2, q2)) =
    BuildAssemblyReport (n1, t1 + t2, m1 + m2, l1 + l2, q1 + q2)

-- | Generator type for flange counts (name, count).
newtype FlangeReportEntry = FlangeReportEntry (Text, Double)

instance ToJSON FlangeReportEntry where
  toJSON (FlangeReportEntry (t, d)) = object [ "name" .= t
                                             , "quant" .= d
                                             ]

-- | Fixed pricing list structure. Supported customers are mapped elsewhere. Check 'FixedPriceItem' for the actual
-- pricing list items.
data PricingList = PricingList { pl_id :: Int
                               , pl_description :: Text
                               , pl_effectiveAsOf :: UTCTime
                               , pl_effectiveUntil :: UTCTime
                               , pl_items :: [FixedPriceItem]
                               } deriving (Show)

instance ToJSON PricingList where
  toJSON pl = object [ "id" .= pl_id pl
                     , "description" .= pl_description pl
                     , "effectiveAsOf" .= pl_effectiveAsOf pl
                     , "effectiveUntil" .= pl_effectiveUntil pl
                     , "items" .= toJSONList (pl_items pl)
                     ]

instance FromJSON PricingList where
  parseJSON = withObject "pricingList" $ \pl -> do
    plid <- pl .:? "id"
    desc <- pl .: "description"
    effAsOf <- pl .: "effectiveAsOf"
    effUntil <- pl .: "effectiveUntil"
    items <- pl .:? "items"
    pure $ PricingList { pl_id = fromMaybe 0 plid
                       , pl_description = desc
                       , pl_effectiveAsOf = effAsOf
                       , pl_effectiveUntil = effUntil
                       , pl_items = fromMaybe [] items
                       }

instance ToRow PricingList where
  toRow pl = [ toField (pl_description pl)
             , toField (pl_effectiveAsOf pl)
             , toField (pl_effectiveUntil pl)
             ]

instance FromRow PricingList where
  fromRow = PricingList <$> field
                        <*> field
                        <*> field
                        <*> field
                        <*> pure []

-- | 'PricingList' items.
data FixedPriceItem = FixedPriceItem { fpi_id :: Int
                                     , fpi_refId :: Int
                                     , fpi_refName :: Text
                                     , fpi_refType :: Text
                                     , fpi_refDescription :: Text
                                     , fpi_fixedPrice :: Double
                                     , fpi_fkPlId :: Int
                                     } deriving (Show)

instance ToJSON FixedPriceItem where
  toJSON fpi = object [ "id" .= fpi_id fpi
                      , "referenceId" .= fpi_refId fpi
                      , "referenceName" .= fpi_refName fpi
                      , "referenceType" .= fpi_refType fpi
                      , "referenceDescription" .= fpi_refDescription fpi
                      , "fixedPrice" .= fpi_fixedPrice fpi
                      , "pricingListId" .= fpi_fkPlId fpi
                      ]

instance FromJSON FixedPriceItem where
  parseJSON = withObject "fixedPriceItem" $ \fpi -> do
    fpiid <- fpi .:? "id"
    refId <- fpi .: "referenceId"
    refName <- fpi .: "referenceName"
    refType <- fpi .: "referenceType"
    refDescription <- fpi .: "referenceDescription"
    fixedPrice <- fpi .: "fixedPrice"
    pricingListId <- fpi .: "pricingListId"
    pure $ FixedPriceItem { fpi_id = fromMaybe 0 fpiid
                          , fpi_refId = refId
                          , fpi_refName = refName
                          , fpi_refType = refType
                          , fpi_refDescription = refDescription
                          , fpi_fixedPrice = fixedPrice
                          , fpi_fkPlId = pricingListId
                          }

instance FromRow FixedPriceItem where
  fromRow = FixedPriceItem <$> field
                           <*> field
                           <*> field
                           <*> field
                           <*> field
                           <*> field
                           <*> field

instance ToRow FixedPriceItem where
  toRow fpi = [ toField (fpi_refId fpi)
              , toField (fpi_refName fpi)
              , toField (fpi_refType fpi)
              , toField (fpi_refDescription fpi)
              , toField (fpi_fixedPrice fpi)
              , toField (fpi_fkPlId fpi)
              ]
