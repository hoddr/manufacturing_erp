{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: MatPurchases
Description: Material purchase options which map to base material.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides functions related to specific instances of material purchase options.
For example, steel coils and steel sheets, which both can point to the same base "Materials" instance.
The units per quantity is provided to specify things like square feet per roll and
pounds per sheet.

The naming convention is set to match the exports from CAMDuct (material abbreviation). Any new variants must match that expected pattern. Steel slit is added to spiral pipe instances.

-}

module MatPurchases
  ( -- * Functions
    addMatPurchase
  , editMatPurchase
  , getMatPurchaseById
  , listMappedMaterialOptions
  , listMatPurchases
  , removeMatPurchase
  )
  where

import Database.PostgreSQL.Simple ( Query )


-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , APIMsg(..)
                , Context(..)
                , MatPurchase(..)
                , QueryPag(..)
                )
import Database ( runExecute
                , runQuery
                )
import Utils ( apiMsg204
             , badReq
             , eitherPassErr
             , gen
             , passSuccess
             , wasFound
             )


-- FUNCTIONS --
-- | Adds a new material purchase option, linked to the base material.
addMatPurchase :: Context -> MatPurchase -> IO (Either APIError MatPurchase)
addMatPurchase ctx mp = runQuery ctx insertSql mp
  >>= eitherPassErr (gen (\nmp nid -> nmp { mp_id = nid })
                         "Failure to return new material purchase id"
                         mp)

-- | Removes a material purchase option.
removeMatPurchase :: Context -> Int -> IO (Either APIError APIMsg)
removeMatPurchase ctx mpid = runExecute ctx delSql [mpid]
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Material purchase deleted")

-- | Edits the specified material purchase options by id ('Int').
editMatPurchase :: Context -> Int -> MatPurchase -> IO (Either APIError MatPurchase)
editMatPurchase ctx mpid mp
  | mp_id mp == mpid = runExecute ctx updateSql ( mp_name mp
                                                , mp_description mp
                                                , mp_unitsPerQuantity mp
                                                , mp_fkMaterialId mp
                                                , mp_leadTime mp
                                                , mp_id mp
                                                )
    >>= eitherPassErr (\_ -> passSuccess mp)
  | otherwise = badReq 400 "Material purchase uri id and body id mismatch"

-- | Retrieves the specified material purchase option by id ('Int').
getMatPurchaseById :: Context -> Int -> IO (Either APIError MatPurchase)
getMatPurchaseById ctx mpid = runQuery ctx (baseSql <> " where mat_purchases.id = ?") [mpid]
  >>= eitherPassErr (wasFound "Material purchase not found")

-- | List material purchase options within bounds of querystring and/or paginations.
-- Will return an empty list for no results.
listMatPurchases :: Context -> QueryPag -> IO (Either APIError [MatPurchase])
listMatPurchases ctx (QueryPag ("", l, o)) =
  runQuery ctx (baseSql <> sortSql <> paginationSql) (l, o)
listMatPurchases ctx (QueryPag (s, l, o)) =
  runQuery ctx (baseSql <> searchSql <> sortSql <> paginationSql) (s, s, s, l, o)

-- | Returns a list of material purchase options for the specified material by id ('Int').
listMappedMaterialOptions :: Context -> Int -> IO (Either APIError [MatPurchase])
listMappedMaterialOptions ctx matid = runQuery ctx (baseSql <> " where mat_purchases.fk_material_id = ?") [matid]


-- SQL --
-- | Base SQL query.
baseSql :: Query
baseSql = "select \
          \mat_purchases.id as id, \
          \mat_purchases.name as name, \
          \mat_purchases.description as description, \
          \mat_purchases.units_per_quantity as unitsPerQuantity, \
          \mat_purchases.fk_material_id as matId, \
          \materials.name as matName, \
          \mat_purchases.lead_time as leadTime \
          \from mat_purchases \
          \inner join materials \
          \on mat_purchases.fk_material_id = materials.id"

-- | Base insertion SQL. Check database for default values if present.
insertSql :: Query
insertSql = "insert into mat_purchases (\
            \name, \
            \description, \
            \units_per_quantity, \
            \fk_material_id, \
            \lead_time \
            \) values \
            \(?, ?, ?, ?, ?) \
            \returning id"

-- | Naive, case-insensitive search on provided fields.
searchSql :: Query
searchSql = " where (\
            \mat_purchases.name like ? or \
            \mat_purchases.description like ? \
            \or materials.name like ?)"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by mat_purchases.name asc"

-- | Pagination SQL.
paginationSql :: Query
paginationSql = " limit ? offset ?"

-- | Base update/edit SQL.
updateSql :: Query
updateSql = "update mat_purchases \
            \set name = ?, \
            \description = ?, \
            \units_per_quantity = ?, \
            \fk_material_id = ?, \
            \lead_time = ? \
            \where mat_purchases.id = ?"

-- | Delete SQL.
delSql :: Query
delSql = "delete from mat_purchases where mat_purchases.id = ?"
