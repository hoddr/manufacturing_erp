{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Materials
Description: Steel, liner, PCD - any inventory-tracked material.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides functions for adding, managing, tracking, and costing of all primary raw materials.
These are the base materials. Specific purchase options must be created via
"MatPurchases", which will link back to the specified base material.
Materials that are NOT tracked as inventory/accounting totals should be added
as purchase items instead.

-}

module Materials
  ( -- * Functions
    addMaterial
  , editMaterial
  , getMaterialById
  , listMaterials
  , lookupIdByName
  , setMatCost
  -- * SQL
  , baseSql
  )
  where

import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Database.PostgreSQL.Simple ( Only(..)
                                  , Query
                                  )
import Prelude hiding ( zipWith )


-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , Context(..)
                , Inventory(..)
                , Material(..)
                , POItem(..)
                , QueryPag(..)
                , materialInv
                )
import Database ( runExecute
                , runQuery
                )
import Inventory ( addMaterialEntry
                 , updateInventory
                 )
import Utils ( badReq
             , calcWeightedCost
             , eitherPassErr
             , gen
             , inventorySql
             , passOnError
             , passSuccess
             , wasFound
             )


-- FUNCTIONS --
-- | Adds a new material entry. Passes result on to add the new inventory entry with
-- the new material primary key id ('Int').
addMaterial :: Context -> Material -> IO (Either APIError Material)
addMaterial c m = runQuery c insertSql m
  >>= eitherPassErr (gen (\mt nid -> mt { mid = nid })
                              "Failure to return new material id"
                              m
                    )
  >>= eitherPassErr (addToInv c)

-- | Adds the new material's inventory entry.
addToInv :: Context -> Material -> IO (Either APIError Material)
addToInv c m = do
  einv <- addMaterialEntry c (mid m) (minventory m)
  case einv of
       (Left e) -> pure $ Left e
       (Right ninv) -> passSuccess $ m { minventory = ninv }

-- | Edits the specified material. See source for fields that may be adjusted.
editMaterial :: Context -> Material -> Int -> IO (Either APIError Material)
editMaterial c m matid
  | mid m == matid = runExecute c updateSql ( mname m
                                            , mdescription m
                                            , munit m
                                            , mcostPerUnit m
                                            , mpreferredVendor m
                                            , mbalanceCategory m
                                            , misLocked m
                                            , msurfaceDensity m
                                            , mid m
                                            ) >>= toInv c >>= toRes
  | otherwise = badReq 400 "Material uri id and body id mismatch"
  where toInv :: Context -> Either APIError a -> IO (Either APIError Inventory)
        toInv _ (Left e) = passOnError e
        toInv ctx _ = updateInventory ctx (minventory m)
        toRes :: Either APIError a -> IO (Either APIError Material)
        toRes (Left e) = passOnError e
        toRes _ = passSuccess m

-- | Retrieves material by specified id ('Int'). Returns 404 error if not found.
getMaterialById :: Context -> Int -> IO (Either APIError Material)
getMaterialById c matid =
  runQuery c (baseSql <> " and materials.id = ? and inventory.reference_id = ?" <> sortSql)
             (materialInv, matid, matid)
  >>= eitherPassErr (wasFound "Material not found")

-- | List materials within bounds of querystring search and/or paginations.
-- Will return an empty list for no results.
listMaterials :: Context -> QueryPag -> IO (Either APIError [Material])
listMaterials ctx (QueryPag ("", l, o)) =
  runQuery ctx (baseSql <> sortSql <> paginationSql) (materialInv, l, o)
listMaterials ctx (QueryPag (s, l, o)) =
  runQuery ctx (baseSql <> searchSql <> sortSql <> paginationSql) (materialInv, s, s, s, s, l, o)

-- | Retrieves (maybe) material id ('Int') by the supplied name. Returns 404 error if not found.
-- Note that the returned value is wrapped in the 'Only' type.
lookupIdByName :: Context -> Text -> IO (Either APIError (Only Int))
lookupIdByName c name = runQuery c "select materials.id from materials where materials.name = ?" [name]
  >>= eitherPassErr (wasFound "Material id not found from given name")

-- | Updates the material cost. Used for purchase orders.
setMatCost :: Context -> POItem -> IO (Either APIError ())
setMatCost ctx poi = let
  matid = poi_refId poi
  uperq = fromMaybe 1.0 $ poi_unitsPerQuantity poi
  ppe = poi_purchasePriceEach poi / uperq
  bq = poi_quantity poi
  q = bq * uperq
  in
    getMaterialById ctx matid
    >>= eitherPassErr (\m ->
      -- have to subtract the 'new' quant here as this quant was already received
      -- this permits weighted averaging 'after' receipt without transactions or more complicated logic!
      let currQ = invonHand (minventory m) - q
          currPe = mcostPerUnit m
          newPe = calcWeightedCost currQ currPe q ppe
      in
        runExecute ctx setMatCostSql (newPe, matid))
    >>= eitherPassErr (\_ -> passSuccess ())


-- SQL
-- | Base query SQL. Note that the inventory portion is used in multiple places.
baseSql :: Query
baseSql = "select \
          \materials.id as id, \
          \materials.name as name, \
          \materials.description as description, \
          \materials.unit as unit, \
          \materials.cost_per_unit as cost, \
          \materials.preferred_vendor as preferredVendor, \
          \materials.balance_category as balanceCategory, \
          \materials.is_locked as isLocked, \
          \materials.surface_density as surfaceDensity, "
       <> inventorySql
       <> ", materials.name as invName \
          \from materials \
          \inner join inventory \
          \on inventory.reference_id = materials.id \
          \where inventory.reference_type = ?"

-- | Base insertion SQL. Check database for any default values.
insertSql :: Query
insertSql = "insert into materials (\
              \name, \
              \description, \
              \unit, \
              \cost_per_unit, \
              \preferred_vendor, \
              \balance_category, \
              \is_locked, \
              \surface_density \
            \) values \
            \(?, ?, ?, ?, ?, ?, ?, ?) \
            \returning id"

-- | Search SQL for naive, case-insensitive search on provided fields.
searchSql :: Query
searchSql = " and (\
            \materials.name like ? or \
            \materials.description like ? or \
            \materials.preferred_vendor like ? or \
            \materials.balance_category like ?)"

-- | Base update/edit SQL.
updateSql :: Query
updateSql = "update materials \
          \set name = ?, \
          \description = ?, \
          \unit = ?, \
          \cost_per_unit = ?, \
          \preferred_vendor = ?, \
          \balance_category = ?, \
          \is_locked = ?, \
          \surface_density = ? \
          \where materials.id = ?"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by materials.name asc"

-- | Pagination SQL.
paginationSql :: Query
paginationSql = " limit ? offset ?"

-- | SQL for updating the cost per unit for the specified material by id ('Int').
setMatCostSql :: Query
setMatCostSql = "update materials \
                \set cost_per_unit = ? \
                \where materials.id = ?"
