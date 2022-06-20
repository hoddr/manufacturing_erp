{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Vendors
Description: Vendor management.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides functions for managing vendors. Primarily used in conjunction with "PurchaseOrders".

-}

module Vendors
  ( -- * Functions
    addVendor
  , editVendor
  , getVendorById
  , listVendors
  )
  where

import Database.PostgreSQL.Simple ( Query )


-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , Context(..)
                , QueryPag(..)
                , Vendor(..)
                )
import Database ( runExecute
                , runQuery
                )
import Utils ( badReq
             , eitherPassErr
             , gen
             , passSuccess
             , wasFound
             )


-- FUNCTIONS --
-- | Adds new vendor instance.
addVendor :: Context -> Vendor -> IO (Either APIError Vendor)
addVendor c v = runQuery c insertSql ( vdname v
                                     , vdcompany v
                                     )
  >>= eitherPassErr (gen (\vd nid -> vd { vdid = nid })
                              "Failure to return new vendor id"
                              v
                    )

-- | Retrieves vendor by the given id ('Int').
getVendorById :: Context -> Int -> IO (Either APIError Vendor)
getVendorById c vid = runQuery c (baseSql <> " where vendors.id = ?" <> sortSql) [vid]
  >>= eitherPassErr (wasFound "Vendor not found")

-- | Edits the vendor instance.
editVendor :: Context -> Vendor -> Int -> IO (Either APIError Vendor)
editVendor c v vid
  | vdid v == vid = runExecute c updateSql ( vdname v
                                           , vdcompany v
                                           , vdid v
                                           )
                      >>= eitherPassErr (\_ -> passSuccess v)
  | otherwise = badReq 400 "Vendor uri id and body id mismatch"

-- | List vendors within bounds of querystring search and/or paginations. Will return an empty list
-- for no results.
listVendors :: Context -> QueryPag -> IO (Either APIError [Vendor])
listVendors ctx (QueryPag ("", l, o)) =
  runQuery ctx (baseSql <> sortSql <> paginationSql) (l, o)
listVendors ctx (QueryPag (s, l, o)) =
  runQuery ctx (baseSql <> searchSql <> sortSql <> paginationSql) (s, s, l, o)


-- SQL
-- | Base query SQL.
baseSql :: Query
baseSql = "select \
          \vendors.id as id, \
          \vendors.name as name, \
          \vendors.company as company \
          \from vendors"

-- | Main insertion SQL.
insertSql :: Query
insertSql = "insert into vendors (name, company) values (?, ?) returning id"

-- | Search SQL for naive, case-insensitive search on provided fields.
searchSql :: Query
searchSql = " where \
            \vendors.name like ? or \
            \vendors.company like ?"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by vendors.id asc"

-- | Pagination SQL.
paginationSql :: Query
paginationSql = " limit ? offset ?"

-- | Edit/update SQL.
updateSql :: Query
updateSql = "update vendors \
            \set name = ?, \
            \company = ? \
            \where vendors.id = ?"
