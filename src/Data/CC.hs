{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Data.CC
Description: Data type declarations for CC.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Data type declarations for CC functionality.

-}

module Data.CC
  ( CCCurb(..)
  , CCQuote(..)
  , CustomerTypeLookup(..)
  )
  where

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
import Database.PostgreSQL.Simple.FromRow ( FromRow
                                          , field
                                          , fromRow
                                          )
import Database.PostgreSQL.Simple.ToField ( toField )
import Database.PostgreSQL.Simple.ToRow ( ToRow
                                        , toRow
                                        )

-- LOCAL --
import Data.CustomersVendors ( Address(..)
                             , Customer(..)
                             )
import Data.Utils ( liftM7 )

{- | Curb Co Quotes. Only for use with quotes generated via Anytime system.
-}
data CCQuote = CCQuote { ccq_id :: Int
                               , ccq_pid :: Text -- anytime pid
                               , ccq_customer :: Customer
                               , ccq_shipAddress :: Maybe Address
                               , ccq_shippingCosts :: Double
                               , ccq_isConfirmed :: Bool
                               , ccq_fastPassCost :: Double
                               , ccq_curbs :: [CCCurb]
                               } deriving (Show)

instance ToJSON CCQuote where
  toJSON q = object [ "id" .= ccq_id q
                    , "pid" .= ccq_pid q
                    , "customer" .= toJSON (ccq_customer q)
                    , "address" .= toJSON (ccq_shipAddress q)
                    , "shippingCosts" .= ccq_shippingCosts q
                    , "isConfirmed" .= ccq_isConfirmed q
                    , "fastPassCost" .= ccq_fastPassCost q
                    , "curbs" .= toJSONList (ccq_curbs q)
                    ]

instance FromJSON CCQuote where
  parseJSON = withObject "quote" $ \q -> do
    ccqid <- q .:? "id"
    pid <- q .: "pid"
    cobj <- q .: "customer"
    c <- parseJSON cobj
    sc <- q .: "shippingCosts"
    ic <- q .: "isConfirmed"
    fpc <- q .: "fastPassCost"
    pure $ CCQuote { ccq_id = fromMaybe 0 ccqid
                       , ccq_pid = pid
                       , ccq_customer = c
                       , ccq_shipAddress = Nothing -- placeholder
                       , ccq_shippingCosts = sc
                       , ccq_isConfirmed = ic
                       , ccq_fastPassCost = fpc
                       , ccq_curbs = []
                       }

instance FromRow CCQuote where
  fromRow = CCQuote <$> field
                        <*> field
                        <*> liftM7 Customer field field field field field field field
                        <*> pure Nothing -- address retrieved later as needed
                        <*> field
                        <*> field
                        <*> field
                        <*> pure []

instance ToRow CCQuote where
  toRow q = [ toField (ccq_pid q)
            , toField (cid $ ccq_customer q)
            , toField (ccq_shippingCosts q)
            , toField (ccq_fastPassCost q)
            ]

{- | Curb Co curb line item. Stores pertinent information for generating
formal quote documents and billing curbs. TBD support for material usage in future.
-}
data CCCurb = CCCurb { ccc_id :: Int
                             , ccc_oldUnit :: Text
                             , ccc_newUnit :: Text
                             , ccc_adapter :: Text
                             , ccc_basePriceEach :: Double -- no discount applied priceEach
                             , ccc_quantity :: Double
                             , ccc_size :: Text -- S = small, M = medium, L = large
                             , ccc_metalId :: Int
                             , ccc_metalName :: Text
                             , ccc_metalWeight :: Double
                             , ccc_gasketFeet :: Double
                             , ccc_priceEach :: Double -- with discount/markup applied (generated)
                             } deriving (Show)

instance ToJSON CCCurb where
  toJSON c = object [ "id" .= ccc_id c
                    , "oldUnit" .= ccc_oldUnit c
                    , "newUnit" .= ccc_newUnit c
                    , "adapter" .= ccc_adapter c
                    , "basePriceEach" .= ccc_basePriceEach c
                    , "quantity" .= ccc_quantity c
                    , "size" .= ccc_size c
                    , "metalId" .= ccc_metalId c
                    , "metalName" .= ccc_metalName c
                    , "metalWeight" .= ccc_metalWeight c
                    , "gasketFeet" .= ccc_gasketFeet c
                    , "priceEach" .= ccc_priceEach c
                    ]

instance FromJSON CCCurb where
  parseJSON = withObject "curb" $ \c -> do
    cccid <- c .:? "id"
    ou <- c .: "oldUnit"
    nu <- c .: "newUnit"
    a <- c .: "adapter"
    pe <- c .: "basePriceEach"
    q <- c .: "quantity"
    s <- c .: "size"
    mid <- c .: "metalId"
    mw <- c .: "metalWeight"
    gf <- c .: "gasketFeet"
    pure $ CCCurb { ccc_id = fromMaybe 0 cccid
                      , ccc_oldUnit = ou
                      , ccc_newUnit = nu
                      , ccc_adapter = a
                      , ccc_basePriceEach = pe
                      , ccc_quantity = q
                      , ccc_size = s
                      , ccc_metalId = mid
                      , ccc_metalName = "" -- placeholder
                      , ccc_metalWeight = mw
                      , ccc_gasketFeet = gf
                      , ccc_priceEach = pe -- placeholder
                      }

instance FromRow CCCurb where
  fromRow = CCCurb <$> field
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
                       <*> pure 0.00 -- markup applied after retrieval

-- defined for update/edit instance
instance ToRow CCCurb where
  toRow c = [ toField (ccc_oldUnit c)
            , toField (ccc_newUnit c)
            , toField (ccc_adapter c)
            , toField (ccc_basePriceEach c)
            , toField (ccc_quantity c)
            , toField (ccc_size c)
            , toField (ccc_metalId c)
            , toField (ccc_metalWeight c)
            , toField (ccc_gasketFeet c)
            , toField (ccc_id c)
            ]

-- | Simple type to capture lookup table for CC designation -> markup mappings. See lu_cc_customer_types for info (migration #35).
data CustomerTypeLookup = CustomerTypeLookup { ctl_id :: Int
                                             , ctl_type :: Text
                                             , ctl_markup :: Double
                                             } deriving (Show)

instance FromRow CustomerTypeLookup where
  fromRow = CustomerTypeLookup <$> field
                               <*> field
                               <*> field
