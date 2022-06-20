{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Data.Pricing
Description: Data types for labor rates and markups related to internal pricing omdels.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Data types for labor rates and markups related to internal pricing omdels.

-}

module Data.Pricing
  ( LaborRate(..)
  , Markup(..)
  , PricingInfo(..)
  , defLabor
  , defMarkup
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
import Prelude hiding ( pi )

-- LOCAL --
import Data.Utils ( defDate )

-- | Structure for the labor rate. Hours are used for the base unit! Overhead + shop rate = total rate.
data LaborRate = LaborRate { lrid :: Int
                           , lrshopRate :: Double
                           , lroverheadRate :: Double
                           , lrrate :: Double
                           , lrisCurrent :: Bool
                           , lrbaseUnit :: Text -- m for min, h for hrs
                           , lraddedAt :: UTCTime
                           } deriving (Show)

instance ToJSON LaborRate where
  toJSON lr = object [ "id" .= lrid lr
                     , "shopRate" .= lrshopRate lr
                     , "overheadRate" .= lroverheadRate lr
                     , "rate" .= lrrate lr
                     , "isCurrent" .= lrisCurrent lr
                     , "baseUnit" .= lrbaseUnit lr
                     , "addedAt" .= lraddedAt lr
                     ]

instance FromJSON LaborRate where
  parseJSON = withObject "laborRate" $ \l -> LaborRate
    <$> l .: "id"
    <*> l .: "shopRate"
    <*> l .: "overheadRate"
    <*> l .: "rate"
    <*> l .: "isCurrent"
    <*> l .: "baseUnit"
    <*> l .: "addedAt"

instance FromRow LaborRate where
  fromRow = LaborRate <$> field
                      <*> field
                      <*> field
                      <*> field
                      <*> field
                      <*> field
                      <*> field

instance ToRow LaborRate where
  toRow l = [ toField (lrshopRate l)
            , toField (lroverheadRate l)
            ]

-- MARKUPS/MARGINS --
-- | Main price factors controlling margin. See external docs for proper values to ensure desired margins in conjunction with customer markups.
data Markup = Markup { mkupid :: Int
                     , mkuppurchase :: Double -- purchase items (resale)
                     , mkupfabrication :: Double -- custom duct not rect, round, oval (e.g. misc, or otherwise)
                     , mkuprectangular :: Double -- custom rect duct
                     , mkupround :: Double -- custom round duct
                     , mkupoval :: Double -- custom oval duct
                     , mkupstock :: Double -- stock fab duct
                     , mkupassembly :: Double
                     , mkupmaterial :: Double -- markup for resale of mats (direct)
                     , mkupquote :: Double -- (optional) markup for QL/FL (applied at end!)
                     , mkupproject :: Double -- (optional) markup for QP (applied at end!)
                     , mkuporder :: Double -- (optional) markup for FS (applied at end!)
                     , mkupisCurrent :: Bool
                     , mkupaddedAt :: Maybe UTCTime
                     } deriving (Show)

instance ToJSON Markup where
  toJSON m = object [ "id" .= mkupid m
                    , "purchase" .= mkuppurchase m
                    , "fabrication" .= mkupfabrication m
                    , "rectangular" .= mkuprectangular m
                    , "round" .= mkupround m
                    , "oval" .= mkupoval m
                    , "stock" .= mkupstock m
                    , "assembly" .= mkupassembly m
                    , "material" .= mkupmaterial m
                    , "quote" .= mkupquote m
                    , "project" .= mkupproject m
                    , "order" .= mkuporder m
                    , "isCurrent" .= mkupisCurrent m
                    , "addedAt" .= mkupaddedAt m
                    ]

instance FromJSON Markup where
  parseJSON = withObject "markup" $ \m -> Markup
    <$> m .: "id"
    <*> m .: "purchase"
    <*> m .: "fabrication"
    <*> m .: "rectangular"
    <*> m .: "round"
    <*> m .: "oval"
    <*> m .: "stock"
    <*> m .: "assembly"
    <*> m .: "material"
    <*> m .: "quote"
    <*> m .: "project"
    <*> m .: "order"
    <*> m .: "isCurrent"
    <*> m .:? "addedAt"

instance FromRow Markup where
  fromRow = Markup <$> field
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

instance ToRow Markup where
  toRow m = [ toField (mkuppurchase m)
            , toField (mkupfabrication m)
            , toField (mkuprectangular m)
            , toField (mkupround m)
            , toField (mkupoval m)
            , toField (mkupstock m)
            , toField (mkupassembly m)
            , toField (mkupmaterial m)
            , toField (mkupquote m)
            , toField (mkupproject m)
            , toField (mkuporder m)
            ]

-- PRICING INFO --
-- | Wrapper that contains all pricing info - labor rate and markups.
newtype PricingInfo = PricingInfo (LaborRate, Markup)

instance ToJSON PricingInfo where
  toJSON (PricingInfo (l, m)) = object [ "laborRate" .= toJSON l
                                       , "markups" .= toJSON m
                                       ]

instance FromJSON PricingInfo where
  parseJSON = withObject "pricingInfo" $ \pi -> do
    lrRaw <- pi .: "laborRate"
    mkupRaw <- pi .: "markups"
    lr <- parseJSON lrRaw
    mkup <- parseJSON mkupRaw
    pure $ PricingInfo (lr, mkup)

-- | Default 'LaborRate'.
defLabor :: LaborRate
defLabor = LaborRate { lrid = 0
                     , lrshopRate = 0.00
                     , lroverheadRate = 0.00
                     , lrrate = 0.00
                     , lrisCurrent = False
                     , lrbaseUnit = "h"
                     , lraddedAt = defDate
                     }

-- | Default 'Markup'.
defMarkup :: Markup
defMarkup = Markup { mkupid = 0
                   , mkuppurchase = 1.00
                   , mkupfabrication = 1.00
                   , mkuprectangular = 1.00
                   , mkupround = 1.00
                   , mkupoval = 1.00
                   , mkupstock = 1.00
                   , mkupassembly = 1.00
                   , mkupmaterial = 1.00
                   , mkupquote = 1.00 -- QL/FL
                   , mkupproject = 1.00 -- QP
                   , mkuporder = 1.00 -- FS
                   , mkupisCurrent = False
                   , mkupaddedAt = Nothing
                   }
