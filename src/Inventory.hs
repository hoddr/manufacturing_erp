{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Inventory
Description: Inventory management.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides generic and item-type specific functions for inventory. All purchase,
fabrication, material, and assembly entries have a corresponding inventory entry,whether or not they are
relevant to company use, accounting, or records.

Note: there may be a lingering, weird bug with negative inventory values, but seems to be more an issue with purchase orders.

-}
module Inventory
  ( addAssemblyEntry
  , addFabEntry
  , addMaterialEntry
  , addMaterialWeight
  , addPurchase
  , addPurchaseEntry
  , addStock
  , adjustMatById
  , editInventory
  , listInventory
  , queryFabInventory
  , queryMaterialInventory
  , queryPurchaseInventory
  , removeInventory
  , subtractMaterialWeight
  , subtractPurchase
  , subtractStock
  , updateInventory
  )
  where

import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Database.PostgreSQL.Simple ( Query )


-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , APIMsg(..)
                , Context(..)
                , Inventory(..)
                , LineItem(..)
                , QueryPag(..)
                , assemblyInv
                , fabInv
                , materialInv
                , purchaseInv
                )
import Database ( runExecute
                , runQuery
                )
import Utils ( apiMsg204
             , badReq
             , eitherPassErr
             , gen
             , insertInventorySql
             , inventorySql
             , passOnError
             , passSuccess
             )


-- FUNCTIONS --
-- | Adds new inventory entry (generic).
addInventoryEntry :: Context
                  -> Text
                  -> Int
                  -> Inventory
                  -> IO (Either APIError Inventory)
addInventoryEntry c refType refId inv =
  runQuery c insertInventorySql ( refId
                                , refType
                                , invonHand inv
                                , invonOrder inv
                                , invminOnHand inv
                                )
  >>= eitherPassErr (gen (\i nid -> i { invid = nid })
                              "Failure to return new inventory id"
                              inv
                    )

-- | Wrapper for material inventory entry.
addMaterialEntry :: Context -> Int -> Inventory -> IO (Either APIError Inventory)
addMaterialEntry c = addInventoryEntry c materialInv

-- | Wrapper for fabrication item inventory entry.
addFabEntry :: Context -> Int -> Inventory -> IO (Either APIError Inventory)
addFabEntry c = addInventoryEntry c fabInv

-- | Wrapper for purchase inventory entry.
addPurchaseEntry :: Context -> Int -> Inventory -> IO (Either APIError Inventory)
addPurchaseEntry c = addInventoryEntry c purchaseInv

-- | Wrapper for assembly inventory entry.
addAssemblyEntry :: Context -> Int -> Inventory -> IO (Either APIError Inventory)
addAssemblyEntry c = addInventoryEntry c assemblyInv

-- | Edits the inventory entry specified by the inventory entry and id ('Int').
editInventory :: Context -> Inventory -> Int -> IO (Either APIError Inventory)
editInventory c i iid
  | invid i == iid = updateInventory c i
  | otherwise = badReq 400 "Inventory uri id and body id mismatch"

{- |
List inventory entries within bounds of querystring search and/or pagination.
These query parameters are applied to each inventory type (e.g. 25 limit will return 25 * n results, with
n being number of inventory types).
Will return an empty list for no results.
-}
listInventory :: Context -> QueryPag -> IO (Either APIError [Inventory])
listInventory c qp = do
  emats <- queryMaterialInventory c qp
  efabs <- queryFabInventory c qp
  epurs <- queryPurchaseInventory c qp
  easss <- queryAssemblyInventory c qp
  case (emats, efabs, epurs, easss) of
       (Left err, _, _, _) -> passOnError err
       (_, Left err, _, _) -> passOnError err
       (_, _, Left err, _) -> passOnError err
       (_, _, _, Left err) -> passOnError err
       (Right mats, Right fabs, Right purs, Right asss) ->
         passSuccess $ concat [mats, fabs, purs, asss]

-- | Generic inventory query wrapper used for 'listInventory'.
queryInventory :: Context
               -> QueryPag
               -> Text
               -> Query
               -> Query
               -> IO (Either APIError [Inventory])
queryInventory c (QueryPag ("", l, o)) refType tbName fName =
  runQuery c (baseSql (nameSql fName)
               <> joinSql tbName
               <> filterSql
               <> " order by inventory.id"
               <> paginationSql
             ) (refType, l, o)
queryInventory c (QueryPag (s, l, o)) refType tbName fName =
  runQuery c (baseSql (nameSql fName)
               <> joinSql tbName
               <> filterSql
               <> searchSql fName
               <> " order by inventory.id"
               <> paginationSql
             ) (refType, s, l, o)

-- | Wrapper for material inventory query.
queryMaterialInventory :: Context -> QueryPag -> IO (Either APIError [Inventory])
queryMaterialInventory c qp = queryInventory c qp materialInv "materials" "materials.name"

-- | Wrapper for fabrication item inventory query.
queryFabInventory :: Context -> QueryPag -> IO (Either APIError [Inventory])
queryFabInventory c qp = queryInventory c qp fabInv "fab_items" "fab_items.name"

-- | Wrapper for purchase item inventory query.
queryPurchaseInventory :: Context -> QueryPag -> IO (Either APIError [Inventory])
queryPurchaseInventory c qp = queryInventory c qp purchaseInv "purchase_items" "purchase_items.name"

-- | Wrapper for assembly inventory query.
queryAssemblyInventory :: Context -> QueryPag -> IO (Either APIError [Inventory])
queryAssemblyInventory c qp = queryInventory c qp assemblyInv "assemblies" "assemblies.name"

-- | Edits the invnetory entry specified by id ('Int'). See source for editable fields.
updateInventory :: Context -> Inventory -> IO (Either APIError Inventory)
updateInventory c i = runExecute c updateSql ( invonHand i
                                             , invonOrder i
                                             , invminOnHand i
                                             , invid i
                                             )
                    >>= eitherPassErr (\_ -> passSuccess i)

-- | Helper for adjusting the material weight from a line item entry.
adjustMaterialWeight :: Query -> Context -> LineItem -> IO (Either APIError ())
adjustMaterialWeight op ctx li =
  runExecute ctx (adjustSqlById op) ( lquant li * fromMaybe 0 (lmatWeight li)
                                    , fromMaybe 0 (lmatId li)
                                    , materialInv
                                    )
  >>= eitherPassErr (\_ -> passSuccess ())

-- | Helper for subtracting the material weight from a line item entry.
subtractMaterialWeight :: Context -> LineItem -> IO (Either APIError ())
subtractMaterialWeight = adjustMaterialWeight "-"

-- | Helper for adding the material weight from a line item entry.
addMaterialWeight :: Context -> LineItem -> IO (Either APIError ())
addMaterialWeight = adjustMaterialWeight "+"

-- | Adjusts stock quantity for the given line item (query by name).
adjustStock :: Query -> Context -> LineItem -> IO (Either APIError ())
adjustStock op ctx li =
  runExecute ctx (adjustSqlByFabName op) ( lquant li
                                         , ldesc li
                                         , fabInv
                                         )
  >>= eitherPassErr (\_ -> passSuccess ())

-- | Wrapper for removing stock.
subtractStock :: Context -> LineItem -> IO (Either APIError ())
subtractStock = adjustStock "-"

-- | Wrapper for adding stock.
addStock :: Context -> LineItem -> IO (Either APIError ())
addStock = adjustStock "+"

-- | Adjusts material (quantity) by material id ('Int').
adjustMatById :: Context
              -> Query
              -> Int
              -> Double
              -> Maybe Double
              -> IO (Either APIError ())
adjustMatById ctx op matid quant mweight =
  runExecute ctx (adjustSqlById op) ( quant * fromMaybe 0 mweight
                                    , matid
                                    , materialInv
                                    )
  >>= eitherPassErr (\_ -> passSuccess ())

-- | Adjusts purchase item inventory for line item.
adjustPurchaseInventory :: Query -> Context -> LineItem -> IO (Either APIError ())
adjustPurchaseInventory op ctx li =
  runExecute ctx (adjustSqlByPurchaseName op) ( lquant li
                                              , ldesc li
                                              , purchaseInv
                                              )
  >>= eitherPassErr (\_ -> passSuccess ())

-- | Wrapper for adding to purchase item inventory.
addPurchase :: Context -> LineItem -> IO (Either APIError ())
addPurchase = adjustPurchaseInventory "+"

-- | Wrapper for removing purchase item inventory.
subtractPurchase :: Context -> LineItem -> IO (Either APIError ())
subtractPurchase = adjustPurchaseInventory "-"

-- | Delete inventory entry.
removeInventory :: Context -> Int -> Text -> IO (Either APIError APIMsg)
removeInventory c refid itype = runExecute c removeSql (refid, itype)
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Inventory removed")


-- SQL --
-- | Base query SQL.
baseSql :: Query -> Query
baseSql selectNameSql = "select"
  <> inventorySql
  <> selectNameSql
  <> " from inventory"

-- | Base filter SQL.
filterSql :: Query
filterSql = " where reference_type = ?"

-- | Base join SQL generator.
joinSql :: Query -> Query
joinSql tableName = " inner join "
                 <> tableName
                 <> " on "
                 <> "inventory.reference_id = "
                 <> tableName
                 <> ".id"

-- | Generator for field (database) name SQL.
nameSql :: Query -> Query
nameSql fieldName = ", " <> fieldName <> " as name"

-- | Default search sql for querystring.
searchSql :: Query -> Query
searchSql fieldName =
  " and ("
  <> fieldName
  <> " like ?)"

-- | Base edit/update SQL.
updateSql :: Query
updateSql = "update inventory set \
            \on_hand = ?, \
            \on_order = ?, \
            \min_on_hand = ? \
            \where inventory.id = ?"

-- | Generator for sql on hand adjustment.
adjustSqlById :: Query -> Query
adjustSqlById op = "update inventory set \
                   \on_hand = on_hand "
                   <> op
                   <> " ? \
                   \where inventory.reference_id = ? \
                   \and inventory.reference_type = ?"

-- | Generator for sql on hand adjustment by name.
adjustSqlByFabName :: Query -> Query
adjustSqlByFabName op = "update inventory as i \
                        \set on_hand = on_hand "
                        <> op <> " ? \
                        \from fab_items as f \
                        \where f.name = ? \
                        \and i.reference_type = ? \
                        \and f.id = i.reference_id \
                        \and f.is_stock"

-- | Generator for sql on hand by purchase name.
adjustSqlByPurchaseName :: Query -> Query
adjustSqlByPurchaseName op = "update inventory as i \
                             \set on_hand = on_hand "
                             <> op <> " ? \
                             \from purchase_items as p \
                             \where p.name = ? \
                             \and i.reference_type = ? \
                             \and p.id = i.reference_id"

-- | Pagination SQL portion.
paginationSql :: Query
paginationSql = " limit ? offset ?"

-- | Delete SQL.
removeSql :: Query
removeSql = "delete from inventory where inventory.reference_id = ? and inventory.reference_type = ?"
