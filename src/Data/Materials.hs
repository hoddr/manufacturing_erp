{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Data.Materials
Description: Data type declarations for materials.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Data type declarations for materials, material purchase options, and storing/passing material
pricing information around during pricing logic.

-}

module Data.Materials
  ( Material(..)
  , MatPurchase(..)
  , MatInfo(..)
  )
  where

import Data.Aeson ( FromJSON
                  , ToJSON
                  , object
                  , parseJSON
                  , toJSON
                  , withObject
                  , (.=)
                  , (.:)
                  , (.:?)
                  )
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Database.PostgreSQL.Simple.FromRow ( FromRow
                                          , field
                                          , fromRow
                                          )
import Database.PostgreSQL.Simple.ToField ( toField )
import Database.PostgreSQL.Simple.ToRow ( ToRow
                                        , toRow
                                        )

-- LOCAL --
import Data.Inventory ( Inventory(..) )
import Data.Utils ( liftM7 )

-- | Base materials. Any material that requires pricing should be present here.
data Material = Material { mid :: Int
                         , mname :: Text
                         , mdescription :: Text
                         , munit :: Text
                         , mcostPerUnit :: Double
                         , mpreferredVendor :: Text
                         , mbalanceCategory :: Text
                         , misLocked :: Bool
                         , msurfaceDensity :: Maybe Double
                         , minventory :: Inventory
                         } deriving (Show)

instance ToJSON Material where
  toJSON m = object [ "id" .= mid m
                    , "name" .= mname m
                    , "description" .= mdescription m
                    , "unit" .= munit m
                    , "costPerUnit" .= mcostPerUnit m
                    , "preferredVendor" .= mpreferredVendor m
                    , "balanceCategory" .= mbalanceCategory m
                    , "isLocked" .= misLocked m
                    , "surfaceDensity" .= msurfaceDensity m
                    , "inventory" .= toJSON (minventory m)
                    ]

instance FromJSON Material where
  parseJSON = withObject "material" $ \m -> do
    mtid <- m .:? "id"
    mtname <- m .: "name"
    mtdesc <- m .: "description"
    mtunit <- m .: "unit"
    mtcostPerUnit <- m .: "costPerUnit"
    mtpreferredVendor <- m .:? "preferredVendor"
    mtbalanceCategory <- m .: "balanceCategory"
    mtisLocked <- m .: "isLocked"
    mtsurfDen <- m .:? "surfaceDensity"
    i <- m .: "inventory"
    inv <- parseJSON i
    pure $ Material { mid = fromMaybe 0 mtid
                    , mname = mtname
                    , mdescription = mtdesc
                    , munit = mtunit
                    , mcostPerUnit = mtcostPerUnit
                    , mpreferredVendor = fromMaybe "" mtpreferredVendor
                    , mbalanceCategory = mtbalanceCategory
                    , misLocked = mtisLocked
                    , msurfaceDensity = mtsurfDen
                    , minventory = inv
                    }

instance FromRow Material where
  fromRow = Material <$> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
                     <*> liftM7 Inventory field field field field field field field

instance ToRow Material where
  toRow m = [ toField (mname m)
            , toField (mdescription m)
            , toField (munit m)
            , toField (mcostPerUnit m)
            , toField (mpreferredVendor m)
            , toField (mbalanceCategory m)
            , toField (misLocked m)
            , toField (msurfaceDensity m)
            ]

-- | Helper during pricing to attach material information to line items.
newtype MatInfo = MatInfo (Int, Text, Double, Double)

-- MAT PURCHASES (POs only) --
-- | Material purchase options. These map to a base 'APITypes.Material', permitting multiple purchase options for each base material.
-- Take special care with units per quantity (e.g. 36 lbs / sheet).
data MatPurchase = MatPurchase { mp_id :: Int
                               , mp_name :: Text
                               , mp_description :: Text
                               , mp_unitsPerQuantity :: Double
                               , mp_fkMaterialId :: Int
                               , mp_fkMaterialName :: Text
                               , mp_leadTime :: Maybe Text
                               } deriving (Show)

instance ToJSON MatPurchase where
  toJSON mp = object [ "id" .= mp_id mp
                     , "name" .= mp_name mp
                     , "description" .= mp_description mp
                     , "unitsPerQuantity" .= mp_unitsPerQuantity mp
                     , "fkMaterialId" .= mp_fkMaterialId mp
                     , "fkMaterialName" .= mp_fkMaterialName mp
                     , "leadTime" .= mp_leadTime mp
                     ]

instance FromJSON MatPurchase where
  parseJSON = withObject "materialPurchase" $ \mp -> do
    mpid <- mp .:? "id"
    mpname <- mp .: "name"
    mpdesc <- mp .: "description"
    mpupq <- mp .: "unitsPerQuantity"
    mpfkmid <- mp .: "fkMaterialId"
    em <- mp .:? "fkMaterialName" -- not needed for import
    lt <- mp .:? "leadTime"
    pure $ MatPurchase { mp_id = fromMaybe 0 mpid
                       , mp_name = mpname
                       , mp_description = mpdesc
                       , mp_unitsPerQuantity = mpupq
                       , mp_fkMaterialId = mpfkmid
                       , mp_fkMaterialName = fromMaybe "" em
                       , mp_leadTime = lt
                       }

instance FromRow MatPurchase where
  fromRow = MatPurchase <$> field
                        <*> field
                        <*> field
                        <*> field
                        <*> field
                        <*> field
                        <*> field

instance ToRow MatPurchase where
  toRow mp = [ toField (mp_name mp)
             , toField (mp_description mp)
             , toField (mp_unitsPerQuantity mp)
             , toField (mp_fkMaterialId mp)
             , toField (mp_leadTime mp)
             ]
