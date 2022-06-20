{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: FabItems
Description: Fabricated item management.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides functions related fabrication items (i.e. items fabricated in shop that are NOT standard stock or repeated items). Primarily stock items and distributor pricing list items.

Each item should have one material and gauge (e.g. GALV 26) and labor.

-}

module FabItems
  ( -- * Functions
    addFabItem
  , baseSql
  , editFabItem
  , finishPricing
  , getById
  , getByName
  , listFabItems
  )
  where


import Data.Text ( Text )
import qualified Data.Text as T ( take )
import Data.Text.Encoding ( encodeUtf8 )
import Database.PostgreSQL.Simple ( Query )
import Prelude hiding ( zipWith )

-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , Context(..)
                , FabItem(..)
                , Inventory(..)
                , LaborRate(..)
                , Markup(..)
                , QueryPag(..)
                , fabInv
                )
import Database ( runExecute
                , runQuery
                )
import Inventory ( addFabEntry
                 , updateInventory
                 )
import Markups ( getLatestRate
               , getLatestMarkupRates
               )
import Utils ( badReq
             , eitherPassErr
             , gen
             , inventorySql
             , passOnError
             , passSuccess
             , wasFound
             )


-- FUNCTIONS --
-- | Adds new fabrication item entry. Then passes to addToInv to generate the corresponding inventory entry.
addFabItem :: Context -> FabItem -> IO (Either APIError FabItem)
addFabItem c fi = runQuery c insertSql fi
  >>= eitherPassErr (gen (\f nid -> f { fiid = nid })
                              "Failure to return new fabrication item id"
                              fi
                    )
  >>= eitherPassErr (addToInv c)

-- | Secondary function for adding a fabrication item inventory entry.
addToInv :: Context -> FabItem -> IO (Either APIError FabItem)
addToInv c fi = do
  einv <- addFabEntry c (fiid fi) (fiinventory fi)
  case einv of
       (Left e) -> pure $ Left e
       (Right ninv) -> passSuccess $ fi { fiinventory = ninv }

-- | Edits the specified fabrication item by id ('Int'). Then passes along to update the inventory entry as requried via `updateInventory`.
-- See source for fields that may be adjusted.
editFabItem :: Context -> FabItem -> Int -> IO (Either APIError FabItem)
editFabItem c f fabid
  | fiid f == fabid = runExecute c updateSql ( finame f
                                             , fidescription f
                                             , fiunitQuantity f
                                             , filabor f
                                             , fimaterialId f
                                             , fiisStock f
                                             , fibalanceCategory f
                                             , fiid f
                                             ) >>= toInv c >>= toRes
  | otherwise = badReq 400 "Fabrication item uri id and body id mismatch"
  where toInv :: Context -> Either APIError a -> IO (Either APIError Inventory)
        toInv _ (Left e) = passOnError e
        toInv ctx _ = updateInventory ctx (fiinventory f)
        toRes :: Either APIError a -> IO (Either APIError FabItem)
        toRes (Left e) = passOnError e
        toRes _ = passSuccess f

-- | List fabrication items within bounds of querystring search and/or pagination.
-- Will return an empty list for no results.
-- Pricing is appended after via 'finishPricing'.
listFabItems :: Context -> QueryPag -> IO (Either APIError [FabItem])
listFabItems ctx (QueryPag ("", l, o)) =
  runQuery ctx (baseSql <> sortSql <> paginationSql) (fabInv, l, o)
  >>= eitherPassErr (finishPricing ctx)
listFabItems ctx (QueryPag (s, l, o)) =
  runQuery ctx (baseSql <> searchSql <> sortSql <> paginationSql) (fabInv, s, s, s, l, o)
  >>= eitherPassErr (finishPricing ctx)

-- | Adds pricing information to the retrieved fabrication item. The rate adjustment is based on the stock flag (actual stock items) and/or the naming that indicates
-- spiral duct liner, which requires a separate rate adjustment.
finishPricing :: Context -> [FabItem] -> IO (Either APIError [FabItem])
finishPricing c fs = do
  elab <- getLatestRate c
  emkups <- getLatestMarkupRates c
  case (elab, emkups) of
       (Left e, _) -> passOnError e
       (_, Left e) -> passOnError e
       (Right lab, Right mkups) -> passSuccess $ addRate fs lab mkups
  where addRate :: [FabItem] -> LaborRate -> Markup -> [FabItem]
        addRate fis lr mkups =
          map (\f -> let mkupToUse
                           | fiisStock f = mkupstock mkups
                           | T.take 4 (finame f) == "SLD1" = 2.25 -- this gives close to distributor pricing (~2 factor)
                           | otherwise = mkupfabrication mkups
                         laborCost = filabor f * lrrate lr
                         cost = fimaterialCost f + laborCost
                     in
                       f { fiprice = cost * mkupToUse
                         , ficost = cost
                         , filaborCost = laborCost
                         }
              )
              fis

-- | Retrieves fabrication item by id ('Int'). Adds pricing.
getById :: Context -> Int -> IO (Either APIError FabItem)
getById c fabid = runQuery c (baseSql <> " and fab_items.id = ?") (fabInv, fabid)
  >>= eitherPassErr (wasFound "Fabrication item not found")
  >>= eitherPassErr (finishPricing c . (:[]))
  >>= eitherPassErr (passSuccess . head)

-- | Retrieves fabrication item by name. Does NOT add pricing.
getByName :: Context -> Text -> IO (Either APIError FabItem)
getByName c n = runQuery c (baseSql <> " and fab_items.name = ?")
                           ( fabInv, n )
  >>= eitherPassErr (wasFound $ encodeUtf8 $ "Fab item: " <> n <> " not found")


-- SQL --
-- | Base query SQL to retrieve fabrication items.
baseSql :: Query
baseSql = "select \
          \fab_items.id as id, \
          \fab_items.name as name, \
          \fab_items.description as description, \
          \fab_items.unit_quantity as unitQuantity, \
          \fab_items.labor as labor, \
          \fab_items.fk_material_id as materialId, \
          \materials.name as materialName, \
          \(materials.cost_per_unit * fab_items.unit_quantity) as matCost, \
          \0.00::real as laborDefaultCost, \
          \0.00::real as totalDefaultCost, \
          \fab_items.is_stock as isStock, \
          \fab_items.balance_category as balanceCategory, " <>
          inventorySql <>
          ", fab_items.name as invName \
          \from fab_items \
          \inner join materials \
          \on fab_items.fk_material_id = materials.id \
          \inner join inventory \
          \on inventory.reference_id = fab_items.id \
          \where inventory.reference_type = ?"

-- | Default insertion SQL. Check database for any default fields and values.
insertSql :: Query
insertSql = "insert into fab_items (\
            \name, \
            \description, \
            \unit_quantity, \
            \labor, \
            \fk_material_id, \
            \is_stock, \
            \balance_category \
            \) values (?, ?, ?, ?, ?, ?, ?) \
            \returning id"

-- | Naive search for matching, case-sensitive strings.
searchSql :: Query
searchSql = " and (\
            \fab_items.name like ? or \
            \fab_items.description like ? or \
            \materials.name like ?)"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by fab_items.id asc"

-- | Base update/edit SQL.
updateSql :: Query
updateSql = "update fab_items set \
            \name = ?, \
            \description = ?, \
            \unit_quantity = ?, \
            \labor = ?, \
            \fk_material_id = ?, \
            \is_stock = ?, \
            \balance_category = ? \
            \where fab_items.id = ?"

-- | Pagination SQL portion.
paginationSql :: Query
paginationSql = " limit ? offset ?"
