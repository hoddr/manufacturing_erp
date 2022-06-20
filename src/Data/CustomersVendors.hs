{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Data.CustomersVendors
Description: Type declarations for both customers, vendors, and addresses.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Data type declarations for customers and vendors (e.g. all third-party members). Also includes
addresses as they are primarily used in conjunction with customers and vendors.

-}

module Data.CustomersVendors
  ( Address(..)
  , Customer(..)
  , Vendor(..)
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

-- | Contains customer information including markups. No customer contacts as of yet.
data Customer = Customer { cid :: Int
                         , cname :: Text
                         , ccompany :: Maybe Text
                         , ctype :: Text
                         , cmarkup :: Double
                         , cisTaxExempt :: Bool
                         , ccurbMarkup :: Double -- CC markup (from lookup table)
                         } deriving (Show)

instance ToJSON Customer where
  toJSON c = object [ "id" .= cid c
                    , "name" .= cname c
                    , "company" .= ccompany c
                    , "type" .= ctype c
                    , "markup" .= cmarkup c
                    , "isTaxExempt" .= cisTaxExempt c
                    ]

instance FromJSON Customer where
  parseJSON = withObject "customer" $ \c -> do
    custId <- c .:? "id"
    name <- c .: "name"
    company <- c .:? "company"
    customerType <- c .: "type"
    markup <- c .:? "markup"
    isTaxExempt <- c .:? "isTaxExempt"
    pure $ Customer { cid = fromMaybe 0 custId
                    , cname = name
                    , ccompany = company
                    , ctype = customerType
                    , cmarkup = fromMaybe 1.00 markup
                    , cisTaxExempt = fromMaybe False isTaxExempt
                    , ccurbMarkup = 1.00 -- placeholder
                    }

instance FromRow Customer where
  fromRow = Customer <$> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field

-- | Contains basic vendor information.
data Vendor = Vendor { vdid :: Int
                     , vdname :: Text
                     , vdcompany :: Text
                     } deriving (Show)

instance ToJSON Vendor where
  toJSON v = object [ "id" .= vdid v
                    , "name" .= vdname v
                    , "company" .= vdcompany v
                    ]

instance FromJSON Vendor where
  parseJSON = withObject "vendor" $ \v -> do
    vid <- v .:? "id"
    vname <- v .: "name"
    vcomp <- v .: "company"
    pure $ Vendor { vdid = fromMaybe 0 vid
                  , vdname = vname
                  , vdcompany = vcomp
                  }

instance FromRow Vendor where
  fromRow = Vendor <$> field
                   <*> field
                   <*> field

{- | Addresses for use with customers and vendors (for HTML report generation). Could be used for
shipping and the like at some point. Would need to confirm formatting rules for use with external apis...
-}
data Address = Address { add_id :: Int
                       , add_street :: Text
                       , add_street2 :: Maybe Text
                       , add_city :: Text
                       , add_state :: Text
                       , add_zip :: Text
                       } deriving (Show)

instance ToJSON Address where
  toJSON a = object [ "id" .= add_id a
                    , "street" .= add_street a
                    , "street2" .= add_street2 a
                    , "city" .= add_city a
                    , "state" .= add_state a
                    , "zip" .= add_zip a
                    ]

instance FromJSON Address where
  parseJSON = withObject "address" $ \a -> do
    aid <- a .:? "id"
    s <- a .: "street"
    s2 <- a .: "street2"
    c <- a .: "city"
    st <- a .: "state"
    z <- a .: "zip"
    pure $ Address { add_id = fromMaybe 0 aid
                   , add_street = s
                   , add_street2 = s2
                   , add_city = c
                   , add_state = st
                   , add_zip = z
                   }

instance FromRow Address where
  fromRow = Address <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field

instance ToRow Address where
  toRow a = [ toField (add_street a)
            , toField (fromMaybe ("" :: Text) $ add_street2 a)
            , toField (add_city a)
            , toField (add_state a)
            , toField (add_zip a)
            ]
