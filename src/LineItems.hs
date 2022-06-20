{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: LineItems
Description: Quote and sales order line items.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides functions related to all order, quote, and project line items.
These line items now include the base costs and markups to store for historical
purposes and for accounting/sales purposes.

Line items are shared between a linked quote and order, as in they are not
duplicated in the database.

-}

module LineItems
  ( -- * Functions
    addOrderLineItems
  , addQuoteLineItems
  , alterFabbedQuant
  , deleteOrderLineItems
  , deleteQuoteLineItems
  , insertOrderLineItems
  , insertQuoteLineItems
  , setExtraFlag
  , toggleIsFabbedFlag
  -- * SQL
  , setExtraFlagSql
  , setLineItemOrderIdSql
  )
  where

import Database.PostgreSQL.Simple ( Only(..)
                                  , Query
                                  )

-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , APIMsg(..)
                , Context(..)
                , LineItem(..)
                , Order(..)
                , Quote(..)
                )
import Database ( runExecute
                , runQuery
                , runReturning
                )
import Utils ( apiMsg201
             , apiMsg204
             , eitherPassErr
             , passSuccess
             )


-- FUNCTIONS --
-- | Adds new line item(s) after being priced for the quote/order.
addLineItems :: ([LineItem] -> a)
             -> Query
             -> (a -> Int)
             -> Context
             -> a
             -> IO (Either APIError a)
addLineItems rfn filterSql getId ctx a =
  runQuery ctx (baseLineItemSql <> filterSql <> sortSql) [getId a]
  >>= eitherPassErr (passSuccess . rfn)

-- | Adds line items for the given order. Line items are carried within the Order type.
addOrderLineItems :: Context -> Order -> IO (Either APIError Order)
addOrderLineItems ctx o =
  addLineItems (\lis -> o { olineItems = lis })
               orderFilterSql
               oid
               ctx
               o

-- | Adds line items for the given quote. Line items are carried within the Quote type.
addQuoteLineItems :: Context -> Quote -> IO (Either APIError Quote)
addQuoteLineItems ctx q =
  addLineItems (\lis -> q { qlineItems = lis })
               quoteFilterSql
               qid
               ctx
               q

-- | Deletes line items. Higher-order to support order/quote deletion of line items.
deleteLineItems :: Query -> Context -> Int -> IO (Either APIError ())
deleteLineItems sql ctx delid =
  runExecute ctx sql [delid] >>= eitherPassErr (\_ -> passSuccess ())

-- | Deletes line items for the specified order id ('Int'). Passes the necessary sql query.
deleteOrderLineItems :: Context -> Int -> IO (Either APIError ())
deleteOrderLineItems = deleteLineItems deleteOrderLineItemsSql

-- | Deletes line items for the specified quote id ('Int'). Passes the necessary sql query.
deleteQuoteLineItems :: Context -> Int -> IO (Either APIError ())
deleteQuoteLineItems = deleteLineItems deleteQuoteLineItemsSql

-- | Higher-order function to add order/quote id to a list of line items. Returns the order/quote id ('Int').
insertLineItems :: (LineItem -> LineItem)
                -> Context
                -> [LineItem]
                -> Int
                -> IO (Either APIError Int)
insertLineItems mapFn ctx lis addId = let adjLis = map mapFn lis in
  runReturning ctx insertLineItemSql adjLis
  >>= eitherPassErr (\(_ :: [Only Int]) -> passSuccess addId)

-- | Adds order id ('Int') to the given line items.
insertOrderLineItems :: Context -> [LineItem] -> Int -> IO (Either APIError Int)
insertOrderLineItems ctx lis ordId = insertLineItems (\l -> l { lorderId = Just ordId }) ctx lis ordId

-- | Adds quote id ('Int') to the given line items.
insertQuoteLineItems :: Context -> [LineItem] -> Int -> IO (Either APIError Int)
insertQuoteLineItems ctx lis quoteId = insertLineItems (\l -> l { lquoteId = Just quoteId }) ctx lis quoteId

-- | Toggles the extra flag for the line item (project billing only).
setExtraFlag :: Context -> Int -> Bool -> IO (Either APIError APIMsg)
setExtraFlag ctx liid f = runExecute ctx (setter f) [liid]
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Extra flag toggled.")

-- | Toggles the is fabricated flag for the line item (Big BD orders only).
toggleIsFabbedFlag :: Context -> LineItem -> Bool -> IO (Either APIError APIMsg)
toggleIsFabbedFlag ctx li flag = runExecute ctx toggleIsFabbedSql (flag, lid li)
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg201 "Fabrication flag set")

-- TODO needs work (UI/UX is awful).
-- | Alters the fabricated quantity (+/- 1) for Big BD orders only.
alterFabbedQuant :: Context -> LineItem -> IO (Either APIError ())
alterFabbedQuant ctx li = runExecute ctx updateFabbedQuantSql ( lquantFabbed li
                                                              , lid li
                                                              )
  >>= eitherPassErr (\_ -> passSuccess ())


-- SQL --
-- | Base SQL for retrieving line items (with quote/order numbers) from database.
baseLineItemSql :: Query
baseLineItemSql = "select \
          \line_items.id as id, \
          \line_items.description as desc, \
          \line_items.quantity as quant, \
          \line_items.labor as labor, \
          \line_items.labor_cost as laborCost, \
          \line_items.weight as totWeight, \
          \line_items.is_extra as isExtra, \
          \line_items.price as price, \
          \line_items.order_id as orderId, \
          \orders.order_number as orderNumber, \
          \line_items.quote_id as quoteId, \
          \quotes.quote_number as quoteNumber, \
          \line_items.material_id as matId, \
          \line_items.material_name as matName, \
          \line_items.material_weight as matWeight, \
          \line_items.material_cost as matCost, \
          \line_items.liner_id as linerId, \
          \line_items.liner_name as linerName, \
          \line_items.liner_area as linerArea, \
          \line_items.liner_cost as linerCost, \
          \line_items.skin_id as skinId, \
          \line_items.skin_name as skinName, \
          \line_items.skin_weight as skinWeight, \
          \line_items.skin_cost as skinCost, \
          \line_items.accessory_cost as accessoryCost, \
          \line_items.category as category, \
          \line_items.piece_length as length, \
          \line_items.quant_fabbed as quantFabbed, \
          \line_items.is_fabbed as isFabbed \
          \from line_items \
          \left outer join orders \
          \on line_items.order_id = orders.id \
          \left outer join quotes \
          \on line_items.quote_id = quotes.id"

-- | Filter SQL for orders id ('Int').
orderFilterSql :: Query
orderFilterSql = " where line_items.order_id = ?"

-- | Filter SQL for quotes id ('Int').
quoteFilterSql :: Query
quoteFilterSql = " where line_items.quote_id = ?"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by line_items.id asc"

-- | Base insertion SQL. Check database for default fields not present here.
insertLineItemSql :: Query
insertLineItemSql = "insert into line_items (\
                    \description, \
                    \quantity, \
                    \labor, \
                    \labor_cost, \
                    \weight, \
                    \is_extra, \
                    \price, \
                    \order_id, \
                    \quote_id, \
                    \material_id, \
                    \material_name, \
                    \material_weight, \
                    \material_cost, \
                    \liner_id, \
                    \liner_name, \
                    \liner_area, \
                    \liner_cost, \
                    \skin_id, \
                    \skin_name, \
                    \skin_weight, \
                    \skin_cost, \
                    \accessory_cost, \
                    \category, \
                    \piece_length, \
                    \quant_fabbed, \
                    \is_fabbed \
                    \) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
                    \returning id"

-- | SQL for deleting line items by order id ('Int').
deleteOrderLineItemsSql :: Query
deleteOrderLineItemsSql = "delete from line_items where line_items.order_id = ?"

-- | SQL for deleting line items by quote id ('Int').
deleteQuoteLineItemsSql :: Query
deleteQuoteLineItemsSql = "delete from line_items where line_items.quote_id = ?"

-- | SQL for setting the line item order id ('Int').
setLineItemOrderIdSql :: Query
setLineItemOrderIdSql = "update line_items set order_id = ? where line_items.quote_id = ?"

-- | SQL to toggle is extra flag. For list of ids.
setExtraFlagSql :: Bool -> Query
setExtraFlagSql True = "update line_items set is_extra = true where line_items.id in ?"
setExtraFlagSql False = "update line_items set is_extra = false where line_items.id in ?"

-- | Internal. Sets flag for individual item.
setter :: Bool -> Query
setter True = "update line_items set is_extra = true where line_items.id = ?"
setter False = "update line_items set is_extra = false where line_items.id = ?"

-- | SQL to toggle is fabbed flag.
toggleIsFabbedSql :: Query
toggleIsFabbedSql = "update line_items set is_fabbed = ? where line_items.id = ?"

-- | Update SQL for the line items quantity fabricated.
updateFabbedQuantSql :: Query
updateFabbedQuantSql = "update line_items set quant_fabbed = ? where line_items.id = ?"
