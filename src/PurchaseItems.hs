{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: PurchaseItems
Description: Purchase and resale items.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides functions related to items that are purchased via third-party and then resold by ERP. Note that although every item has a corresonding inventory entry, not all items "care" about inventory. Those that do include: HETOs, flanges, and snaplock/warm-air pipe and fittings.

A separate markup can be applied on a per-item basis (e.g. snaplock pipe and fittings have a 30% markup by default).

Purchase items have the category denotation of "purchase". Check the "APITypes" module for more info and for the data declaration.

-}

module PurchaseItems
  ( -- * Functions
    addPurchaseItem
  , deletePurchaseItem
  , editPurchaseItem
  , getPurchaseItemById
  , listPurchaseItems
  , setPurchaseItemCost
  -- * SQL
  , baseSql
  )
  where

import Database.PostgreSQL.Simple ( Only(..)
                                  , Query
                                  )
import qualified Data.Text as T
import Prelude hiding ( pi
                      , zipWith
                      )

-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , APIMsg(..)
                , Context(..)
                , Inventory
                , POItem(..)
                , PurchaseItem(..)
                , QueryPag(..)
                , Vendor(..)
                , purchaseInv
                )
import Database ( runExecute
                , runQuery
                )
import Inventory ( addPurchaseEntry
                 , updateInventory
                 )
import Utils ( apiMsg204
             , badReq
             , eitherPassErr
             , gen
             , inventorySql
             , passOnError
             , passSuccess
             , wasFound
             )


-- FUNCTIONS --
-- | Adds a new purchase/resale item with coupled inventory entry (addToInv in source).
addPurchaseItem :: Context -> PurchaseItem -> IO (Either APIError PurchaseItem)
addPurchaseItem c pi = runQuery c insertSql pi
  >>= eitherPassErr (gen (\p nid -> p { piid = nid })
                              "Failure to return new purchase item id"
                              pi
                    )
  >>= eitherPassErr (addToInv c)

-- | Second portion of add purchase item. Adds the inventory entry for the new purchase item.
addToInv :: Context -> PurchaseItem -> IO (Either APIError PurchaseItem)
addToInv c pi = do
  einv <- addPurchaseEntry c (piid pi) (piinventory pi)
  case einv of
       (Left e) -> pure $ Left e
       (Right ninv) -> passSuccess $ pi { piinventory = ninv }

-- | List purchase items within bounds of querystring search and/or paginations.
-- Will return an empty list for no results.
listPurchaseItems :: Context -> QueryPag -> IO (Either APIError [PurchaseItem])
listPurchaseItems c (QueryPag ("", l, o)) =
  runQuery c (baseSql <> sortSql <> paginationSql) (purchaseInv, l, o)
listPurchaseItems c (QueryPag (s, l, o)) =
  runQuery c (baseSql <> searchSql <> sortSql <> paginationSql)
             (purchaseInv, s, s, s, s, s, s, s, s, s, l, o)

-- | Retrieves purchase item by the provided id ('Int'). 404 error if not found.
getPurchaseItemById :: Context -> Int -> IO (Either APIError PurchaseItem)
getPurchaseItemById c purid = runQuery c (baseSql <> " and purchase_items.id = ?") (purchaseInv, purid)
  >>= eitherPassErr (wasFound "Purchase item not found")

-- | Edits the specified purchase item. See source for fields that may be adjusted.
editPurchaseItem :: Context -> Int -> PurchaseItem -> IO (Either APIError PurchaseItem)
editPurchaseItem c pid p
  | piid p == pid = runExecute c updateSql ( piname p
                                           , T.toUpper (pigenericName p)
                                           , T.toUpper <$> piautodeskId p
                                           , pidescription p
                                           , pipreferredVendor p
                                           , picost p
                                           , pivendorPartNumber p
                                           , pivendorCategory p
                                           , pimarkup p
                                           , pibalanceCategory p
                                           , piisLocked p
                                           , pileadTime p
                                           , vdid (pivendor p)
                                           , piid p
                                           ) >>= toInv c >>= toRes
  | otherwise = badReq 400 "Purchase item uri id and body id mismatch"
  where toInv :: Context -> Either APIError a -> IO (Either APIError Inventory)
        toInv _ (Left e) = passOnError e
        toInv ctx _ = updateInventory ctx (piinventory p)
        toRes :: Either APIError a -> IO (Either APIError PurchaseItem)
        toRes (Left e) = passOnError e
        toRes _ = passSuccess p

-- | Deletes specified purchase item by id ('Int'). First verifies that purchase item information is
-- not required via link in an order or quote.
deletePurchaseItem :: Context -> Int -> IO (Either APIError APIMsg)
deletePurchaseItem ctx pid = getPurchaseItemById ctx pid
  >>= eitherPassErr (\p -> runQuery ctx searchForPurchaseItem ( purchaseInv, piname p ))
  >>= eitherPassErr (\(count :: [Only Int]) -> if head count > Only 0
                                  then badReq 400 "This purchase item is linked to a line item in an order/quote."
                                  else passSuccess ())
  >>= eitherPassErr (\_ -> runExecute ctx delSql [pid])
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Purchase item deleted")

-- TODO weighted average?
-- | Adjusts base cost of the given purchase item. Price = cost * markup.
setPurchaseItemCost :: Context -> POItem -> IO (Either APIError ())
setPurchaseItemCost ctx poi = let
  pid = poi_refId poi
  ppe = poi_purchasePriceEach poi
 in
   runExecute ctx setPurCostSql (ppe, pid)
   >>= eitherPassErr (\_ -> passSuccess ())

-- SQL --
{- | Base SQL string for retrieving purchase item from database. Note that in the "from"
section, the order of the tables listed matters. It MUST be pricing THEN purchase_items, or the query
will not work correctly.
-}
baseSql :: Query
baseSql = "with pricing as (\
            \select markups.purchase as rate from markups where markups.is_current limit 1) \
          \select \
          \purchase_items.id as id, \
          \purchase_items.name as name, \
          \purchase_items.generic_name as genericName, \
          \purchase_items.autodesk_id as autodeskId, \
          \purchase_items.description as description, \
          \(case when purchase_items.markup = 1.00 then pricing.rate * purchase_items.cost \
            \else purchase_items.markup * purchase_items.cost end) as price, \
          \purchase_items.preferred_vendor as preferredVendor, \
          \purchase_items.cost as cost, \
          \purchase_items.vendor_part_number as vendorPartNumber, \
          \purchase_items.vendor_category as vendorCategory, \
          \purchase_items.markup as markup, \
          \purchase_items.balance_category as balanceCategory, \
          \purchase_items.is_locked as isLocked, \
          \purchase_items.lead_time as leadTime, \
          \vendors.id as vendorId, \
          \vendors.name as vendorName, \
          \vendors.company as company, "
       <> inventorySql
       <> ", purchase_items.name as invName \
          \from pricing, purchase_items \
          \inner join vendors \
          \on purchase_items.fk_vendor_id = vendors.id \
          \inner join inventory \
          \on inventory.reference_id = purchase_items.id \
          \where inventory.reference_type = ?"

-- | Base insertion SQL. Default values in database are provided for other fields.
insertSql :: Query
insertSql = "insert into purchase_items (\
              \name, \
              \generic_name, \
              \autodesk_id, \
              \description, \
              \preferred_vendor, \
              \cost, \
              \vendor_part_number, \
              \vendor_category, \
              \markup, \
              \balance_category, \
              \is_locked, \
              \lead_time, \
              \fk_vendor_id \
            \) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
            \returning id"

-- | Naive search for matching, case-sensitive strings.
searchSql :: Query
searchSql = " and (\
            \purchase_items.name like ? or \
            \purchase_items.generic_name like ? or \
            \purchase_items.autodesk_id like ? or \
            \purchase_items.description like ? or \
            \purchase_items.balance_category like ? or \
            \vendors.name like ? or \
            \purchase_items.vendor_part_number like ? or \
            \purchase_items.vendor_category like ? or \
            \purchase_items.preferred_vendor like ?)"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by purchase_items.name asc"

-- | Pagination SQL portion.
paginationSql :: Query
paginationSql = " limit ? offset ?"

-- | Base update/edit SQL.
updateSql :: Query
updateSql = "update purchase_items set \
            \name = ?, \
            \generic_name = ?, \
            \autodesk_id = ?, \
            \description = ?, \
            \preferred_vendor = ?, \
            \cost = ?, \
            \vendor_part_number = ?, \
            \vendor_category = ?, \
            \markup = ?, \
            \balance_category = ?, \
            \is_locked = ?, \
            \lead_time = ?, \
            \fk_vendor_id = ? \
            \where purchase_items.id = ?"

-- | Simple query for checking of existence by category and description/name.
searchForPurchaseItem :: Query
searchForPurchaseItem = "select count(line_items.id) from line_items \
                        \where line_items.category = ? \
                        \and line_items.description = ?"

-- | Deletion SQL.
delSql :: Query
delSql = "delete from purchase_items where purchase_items.id = ?"

-- | Update for only affecting cost. Used with 'setPurchaseItemCost'.
setPurCostSql :: Query
setPurCostSql = "update purchase_items \
                \set cost = ? \
                \where purchase_items.id = ?"
