{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: PricingLists
Description: Fixed pricing lists for non-custom fittings and items.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides functions related to creation, management, and use of fixed pricing lists.
Pricing lists can be assigned to more than one customer. The primary pricing lists are Big BD, J, and S. The J pricing list is not comprised of fixed items, but rather dimension categories for a given fitting type.
The BD pricing list is automatically used by proper script variant choice (BD - write). The other pricing lists
must be selected within the script at the start to be available.

-}
module PricingLists
  ( -- * Functions
    addPricingList
  , addPricingListCustomer
  , deletePricingList
  , deletePricingListCustomer
  , getBDList
  , getPricingListById
  , getPricingListsForCustomer
  , getPricingListReport
  , listPricingLists
  , listPricingListCustomers
  , removeItem
  , setPricingListItems
  )
  where


import Control.Monad ( forM )
import qualified Data.ByteString.Lazy as LBS
import Database.PostgreSQL.Simple ( Only(..)
                                  , Query
                                  )
import Data.UUID.V4 ( nextRandom )


-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , APIMsg
                , Context(..)
                , Customer(..)
                , PricingList(..)
                , FixedPriceItem(..)
                )
import Customers ( baseCustomerQuery
                 , getCustomerByName
                 )
import Database ( runExecute
                , runQuery
                , runQuery_
                , runReturning
                )
import Reports ( pricingListHtml )
import Utils ( apiMsg204
             , flattenData
             , eitherPassErr
             , gen
             , passSuccess
             , wasFound
             )


-- FUNCTIONS --
-- | List fixed pricing lists that are within the valid date range. The sub items are appended.
listPricingLists :: Context -> IO (Either APIError [PricingList])
listPricingLists c = runQuery_ c (baseSql <> validDateFilter)
  >>= eitherPassErr (appendItems c)

-- | Appends the items for the pricing list.
appendItems :: Context -> [PricingList] -> IO (Either APIError [PricingList])
appendItems c pls = forM pls (\pl ->
  runQuery c baseItemSql [pl_id pl]
  >>= eitherPassErr (\fpis -> passSuccess $ pl { pl_items = fpis }))
  -- lint suggestion of Data.Functor.(<&>) flattenData e.g. flip fmap . flattenData
  >>= (pure . flattenData)

-- | Retrieves the given pricing list by id ('Int'). Appends items. Returns 404 error if not found.
getPricingListById :: Context -> Int -> IO (Either APIError PricingList)
getPricingListById c plid = runQuery c (baseSql <> validDateFilter <> " and pricing_lists.id = ?") [plid]
  >>= eitherPassErr (wasFound "Pricing list not found")
  >>= eitherPassErr (appendItems c . (:[]))
  >>= eitherPassErr (passSuccess . head)

-- | Retrieves any valid pricing list(s) for the given customer. Returns an empty list for no results.
getPricingListsForCustomer :: Context -> Customer -> IO (Either APIError [PricingList])
getPricingListsForCustomer c cust = runQuery c (baseSql <> lookupSql <> validDateFilter <> lookupSql2) [cid cust]

-- | Creates a new pricing list skeleton (no customer(s) or items).
addPricingList :: Context -> PricingList -> IO (Either APIError PricingList)
addPricingList c pl = runQuery c insertSql ( pl_description pl
                                           , pl_effectiveAsOf pl
                                           , pl_effectiveUntil pl
                                           )
  >>= eitherPassErr (gen (\npl nid -> npl { pl_id = nid })
                              "Failure to return new pricing list id"
                              pl
                    )

-- | Sets the pricing list items for the given list by id ('Int').
-- First REMOVES the items then creates the updated mapping.
setPricingListItems :: Context -> Int -> [FixedPriceItem] -> IO (Either APIError [FixedPriceItem])
setPricingListItems c plid fpis = runExecute c removeItemsSql [plid]
  >>= eitherPassErr (\_ -> runReturning c itemsInsertSql fpis)
  >>= eitherPassErr (\(_ :: [Only Int]) -> passSuccess fpis)

-- | Removes item from the pricing list.
removeItem :: Context -> FixedPriceItem -> IO (Either APIError APIMsg)
removeItem c fpi = runExecute c removeItemSql ( fpi_id fpi
                                              , fpi_fkPlId fpi
                                              )
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "No Content")

-- | Deletes the pricing list. Items then customers then list for removal order.
deletePricingList :: Context -> Int -> IO (Either APIError APIMsg)
deletePricingList c plid = runExecute c removeItemsSql [plid]
  >>= eitherPassErr (\_ -> runExecute c custDelAllSql [plid])
  >>= eitherPassErr (\_ -> runExecute c deleteSql [plid])
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "No Content")

-- | Retrieves all customers for the given pricing list.
listPricingListCustomers :: Context -> Int -> IO (Either APIError [Customer])
listPricingListCustomers c plid = runQuery c (custBaseSql <> custBaseSearchSql) [plid]

-- | Adds a customer to the pricing list usage.
addPricingListCustomer :: Context -> Int -> Customer -> IO (Either APIError Customer)
addPricingListCustomer ctx plid cust = runQuery ctx custInsertSql (plid, cid cust)
  >>= eitherPassErr (\(_ :: [Only Int])-> passSuccess cust)

-- | Removes the customer from the pricing list supported list.
deletePricingListCustomer :: Context -> Int -> Int -> IO (Either APIError APIMsg)
deletePricingListCustomer ctx plid custid = runExecute ctx custDelSql (plid, custid)
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Customer mapping removed.")

-- only retrieves shell; remainder of bd list retrieved via priceViaList logic
{- |

Helper to retrieve the Big BD pricing list. This only retrieves the shell of the list;
the items are retrieved in the 'Pricing.priceViaList' logic.

-}
getBDList :: Context -> IO (Either APIError PricingList)
getBDList ctx = getCustomerByName ctx "Big BD Inc."
  >>= eitherPassErr (getPricingListsForCustomer ctx)
  >>= eitherPassErr (wasFound "Big BD pricing list not found")

-- | Generates an HTML file for the given pricing list (no customers specified). The items and
-- prices are included.
getPricingListReport :: Context -> Int -> IO (Either APIError LBS.ByteString)
getPricingListReport ctx plid = do
  rand <- nextRandom
  getPricingListById ctx plid
    >>= eitherPassErr (pricingListHtml rand)


-- SQL --
-- | Base query SQL.
baseSql :: Query
baseSql = "select \
          \pricing_lists.id as id, \
          \pricing_lists.description as description, \
          \pricing_lists.effective_as_of as effectiveAsOf, \
          \pricing_lists.effective_until as effectiveUntil \
          \from pricing_lists"

-- | Date filter for valid/current pricing list.
validDateFilter :: Query
validDateFilter = " where pricing_lists.effective_as_of <= current_timestamp \
                  \and pricing_lists.effective_until > current_timestamp"

-- | Main insertion SQL for pricing lists. Check database for default field values.
insertSql :: Query
insertSql = "insert into pricing_lists (\
            \description, \
            \effective_as_of, \
            \effective_until \
            \) values (?, ?, ?) \
            \returning id"

-- | Base query SQL for the pricing list items.
baseItemSql :: Query
baseItemSql = "select \
              \m_pricing_list_items.id as id, \
              \m_pricing_list_items.reference_id as refId, \
              \m_pricing_list_items.reference_name as refName, \
              \m_pricing_list_items.reference_type as refType, \
              \m_pricing_list_items.reference_description as refDescription, \
              \m_pricing_list_items.fixed_price as fixedPrice, \
              \m_pricing_list_items.fk_pricing_list_id as pricingListId \
              \from m_pricing_list_items \
              \where m_pricing_list_items.fk_pricing_list_id = ? \
              \order by m_pricing_list_items.id asc"

-- | SQL for removing all items for the pricing list.
removeItemsSql :: Query
removeItemsSql = "delete from m_pricing_list_items where m_pricing_list_items.fk_pricing_list_id = ?"

-- | Insertion SQL for pricing list item(s).
itemsInsertSql :: Query
itemsInsertSql = "insert into m_pricing_list_items (\
                 \reference_id, \
                 \reference_name, \
                 \reference_type, \
                 \reference_description, \
                 \fixed_price, \
                 \fk_pricing_list_id \
                 \) values (?, ?, ?, ?, ?, ?) \
                 \returning id"

-- | Deletion SQL for item removal.
removeItemSql :: Query
removeItemSql = "delete from m_pricing_list_items where \
                \m_pricing_list_items.id = ? \
                \and m_pricing_list_items.fk_pricing_list_id = ?"

-- | Deletion SQL for pricing list removal.
deleteSql :: Query
deleteSql = "delete from pricing_lists where pricing_lists.id = ?"

-- | Search SQL 1 for search by supported customers.
lookupSql :: Query
lookupSql = " inner join m_catalog_customers \
            \on pricing_lists.id = m_catalog_customers.fk_pricing_list_id"

-- | Search SQL 2 for search by specific customer id.
lookupSql2 :: Query
lookupSql2 = " and m_catalog_customers.fk_customer_id = ?"

-- | Select customers SQL for given pricing list.
custBaseSql :: Query
custBaseSql = "select \
          \m_catalog_customers.fk_customer_id as cid, "
       <> baseCustomerQuery
       <> "from m_catalog_customers \
          \inner join customers on \
          \m_catalog_customers.fk_customer_id = customers.id"

-- | Base query SQL for searching customers on pricing lists.
custBaseSearchSql :: Query
custBaseSearchSql = " where m_catalog_customers.fk_pricing_list_id = ?"

-- | Insertion SQL for new customer on the pricing list.
custInsertSql :: Query
custInsertSql = "insert into m_catalog_customers (\
                \fk_pricing_list_id, \
                \fk_customer_id \
                \) values (?, ?) \
                \returning id"

-- | Delete SQL for customers on the pricing list.
custDelAllSql :: Query
custDelAllSql = "delete from m_catalog_customers \
                \where m_catalog_customers.fk_pricing_list_id = ?"

-- | Delete SQL for singular customer on the pricing list.
custDelSql :: Query
custDelSql = custDelAllSql <> " and m_catalog_customers.fk_customer_id = ?"
