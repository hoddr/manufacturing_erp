{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Data.Inventory
Description: Data type declarations for inventory.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Data type declarations for inventory and shared inventory constants.

-}

module Data.Inventory
  ( Inventory(..)
  , assemblyInv
  , fabInv
  , materialInv
  , purchaseInv
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
import Prelude hiding ( id )

-- | Constant for purchaes inventory type.
purchaseInv :: Text
purchaseInv = "purchase"

-- | Constant for materials inventory type.
materialInv :: Text
materialInv = "material"

-- | Constant for fabrication items inventory type.
fabInv :: Text
fabInv = "fabrication"

-- | Constant for assemblies inventory type.
assemblyInv :: Text
assemblyInv = "assembly"

-- | Generic inventory record structure. Used for all item types.
data Inventory = Inventory { invid :: Int
                           , invreferenceId :: Int
                           , invtype :: Text
                           , invonHand :: Double
                           , invonOrder :: Double
                           , invminOnHand :: Double
                           , invname :: Text -- from join, not in table!
                           } deriving (Show)

instance ToJSON Inventory where
  toJSON inv = object [ "id" .= invid inv
                      , "referenceId" .= invreferenceId inv
                      , "type" .= invtype inv
                      , "onHand" .= invonHand inv
                      , "onOrder" .= invonOrder inv
                      , "minOnHand" .= invminOnHand inv
                      , "name" .= invname inv
                      ]

instance FromJSON Inventory where
  parseJSON = withObject "inventory" $ \i -> do
    id <- i .:? "id"
    refNumber <- i .: "referenceId"
    itype <- i .: "type"
    onHand <- i .: "onHand"
    onOrder <- i .: "onOrder"
    minOnHand <- i .: "minOnHand"
    pure $ Inventory { invid = fromMaybe 0 id
                     , invreferenceId = refNumber
                     , invtype = itype
                     , invonHand = onHand
                     , invonOrder = onOrder
                     , invminOnHand = minOnHand
                     , invname = ""
                     }

instance FromRow Inventory where
  fromRow = Inventory <$> field
                      <*> field
                      <*> field
                      <*> field
                      <*> field
                      <*> field
                      <*> field

instance ToRow Inventory where
  toRow i = [ toField (invreferenceId i)
            , toField (invtype i)
            , toField (invonHand i)
            , toField (invonOrder i)
            , toField (invminOnHand i)
            ]
