{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: CC
Description: Collection of functions and database ties to support CC quotes
and biling.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides all current functions needed to support CC quotes and billing in the ERP. This is not
intended to support CC's portal, but rather to provide the necessary linkages to bridge the gap between
CC generated material and ERP systems.

CC quotes (QC) will be created in three distinct steps post Anytime software results:
1) Add PID, customer, shipping costs to generate QC shell
2) Assign shipping address (add shipping address to database where necessary)
3) Add curb line item(s)

This will permit generation of "formal" ERP quote sheet for sending to customers, complete with hyperlink to confirm quote.

NOTE: CC quote shipping addresses receive a T designation in the address book mappings.

-}

module CC
  ( -- * Quote Functions
    addCCQuoteShell
  , deleteCCQuote
  , editCCQuote
  , getCCQuoteByPID
  , getCCSalesData
  , listCCQuoteShells
  , lookupCustomerMarkup
  , removeCCMapping
  , setOrderIdOnCCQuote
  , toggleCCQuoteConfirmationStatus
    -- * Curb Functions
  , addCurbToQuote
  , editCurb
  , deleteCurb
    -- * Report Functions
  , generateCCQuoteHTML
  )
  where

import Control.Monad ( forM )
import qualified Data.ByteString.Lazy as LBS
import Data.Text ( Text )
import Data.Time.Clock ( getCurrentTime )
import Data.UUID.V4 ( nextRandom )
import Database.PostgreSQL.Simple ( Only(..)
                                  , Query
                                  )

-- LOCAL IMPORTS --
import Addresses ( retrieveCCQuoteAddress )
import APITypes ( APIError
                , APIMsg
                , Context
                , CCCurb(..)
                , CCQuote(..)
                , Customer(..)
                , CustomerTypeLookup(..)
                , DateRange(..)
                , Order(..)
                , QueryPag(..)
                )
import Customers ( getCustomerByName )
import Database ( runExecute
                , runQuery
                , transactionWrapper
                )
import Orders ( getCCOrdersInDateRange )
import Reports ( curbCoQuoteHtml )
import Utils ( apiMsg201
             , apiMsg204
             , badReq
             , eitherPassErr
             , flattenData
             , gen
             , passSuccess
             , wasFound
             )

-- FUNCTIONS --
{- |

Adds new CC quote shell. Shipping address and actual curb line item(s) added later. Returns new
QC quote upon success. First action verifies existence of customer with given name.

-}
addCCQuoteShell :: Context
                    -> CCQuote
                    -> IO (Either APIError CCQuote)
addCCQuoteShell ctx ccq = getCustomerByName ctx (cname $ ccq_customer ccq)
  >>= eitherPassErr (\_ -> runQuery ctx insertSql ccq)
  >>= eitherPassErr (gen (\ccqShell nid -> ccqShell { ccq_id = nid })
                        "Failure to return new CC quote id"
                        ccq
                   )

-- | Retrieves specified quote by its PID (generated from CC software).
getCCQuoteByPID :: Context -> Text -> IO (Either APIError CCQuote)
getCCQuoteByPID ctx pid = runQuery ctx (baseSql <> " where cc_quotes.anytime_pid = ?") [pid]
  >>= eitherPassErr (wasFound "CC quote not found")
  >>= eitherPassErr (appendCurbsToQuote ctx)
  >>= eitherPassErr (appendAddressToQuote ctx)

-- | List quotes within bounds of querystring search and/or pagination. Will return empty list for no results.
listCCQuoteShells :: Context -> QueryPag -> IO (Either APIError [CCQuote])
listCCQuoteShells ctx (QueryPag ("", l, o)) = runQuery ctx (baseSql <> sortSql <> paginationSql) (l, o)
listCCQuoteShells ctx (QueryPag (s, l, o)) = runQuery ctx (baseSql <> searchSql <> sortSql <> paginationSql) (s, s, l, o)

-- | Edits the specified quote. See source for fields that may be adjsuted.
editCCQuote :: Context -> Int -> CCQuote -> IO (Either APIError CCQuote)
editCCQuote ctx qidCheck ccq
  | ccq_id ccq == qidCheck = runExecute ctx updateSql ( ccq_pid ccq
                                                      , cid $ ccq_customer ccq
                                                      , ccq_shippingCosts ccq
                                                      , ccq_isConfirmed ccq
                                                      , ccq_fastPassCost ccq
                                                      , ccq_id ccq
                                                      )
    >>= eitherPassErr  (\_ -> passSuccess ccq)
  | otherwise = badReq 400 "CC quote uri pid and body pid mismatch"

-- | Removes the CC quote and all line items from database. Test of transaction functionality.
deleteCCQuote :: Context -> Text -> IO (Either APIError APIMsg)
deleteCCQuote ctx pid = transactionWrapper ctx
  (
    getCCQuoteByPID ctx pid
      >>= eitherPassErr (deleteAllCurbsOnQuote ctx . ccq_id)
      >>= eitherPassErr (\qid -> runExecute ctx deleteQuoteSql [qid])
      >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "No Content")
  )

-- | Helper to remove all curb line item(s) on quote. Passes on quote id ('Int').
deleteAllCurbsOnQuote :: Context -> Int -> IO (Either APIError Int)
deleteAllCurbsOnQuote ctx qid = runExecute ctx deleteAllCurbsSql [qid]
  >>= eitherPassErr (\_ -> passSuccess qid)

-- | Sets the generated order id ('Int') to the CC quote.
setOrderIdOnCCQuote :: Context -> Int -> Int -> IO (Either APIError APIMsg)
setOrderIdOnCCQuote ctx qid ordId = runQuery ctx setOrderIdSql (qid, ordId)
  >>= eitherPassErr (\(_ :: [Only Int]) -> passSuccess $ apiMsg201 "CC quote converted to order")

-- | Removes quote to order mapping by order id ('Int').
removeCCMapping :: Context -> Int -> IO (Either APIError APIMsg)
removeCCMapping ctx ordId = runExecute ctx removeMapSql [ordId]
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Mapping deleted")

-- | Appends additional curb with quantity to the specified quote. Retrieves quote to verify existence.
addCurbToQuote :: Context -> Text -> CCCurb -> IO (Either APIError CCCurb)
addCurbToQuote ctx pid ccc = getCCQuoteByPID ctx pid
  >>= eitherPassErr (\ccq ->
    runQuery ctx curbInsertSql ( ccq_id ccq
                               , ccc_oldUnit ccc
                               , ccc_newUnit ccc
                               , ccc_adapter ccc
                               , ccc_priceEach ccc
                               , ccc_quantity ccc
                               , ccc_size ccc
                               , ccc_metalId ccc
                               , ccc_metalWeight ccc
                               , ccc_gasketFeet ccc
                               )
  )
  >>= eitherPassErr (gen (\cccPass nid -> cccPass { ccc_id = nid })
                         "Failure to return new curb id"
                         ccc
                    )

-- | Helper to append curbs to quote.
appendCurbsToQuote :: Context -> CCQuote -> IO (Either APIError CCQuote)
appendCurbsToQuote ctx ccq = listCurbsOnQuote ctx (ccq_id ccq) (ccurbMarkup $ ccq_customer ccq)
  >>= eitherPassErr (\cs -> passSuccess $ ccq { ccq_curbs = cs })

-- | Helper to append shipping addresses to quote.
appendAddressToQuote :: Context -> CCQuote -> IO (Either APIError CCQuote)
appendAddressToQuote ctx ccq = retrieveCCQuoteAddress ctx (ccq_id ccq)
  >>= eitherPassErr (\a -> passSuccess $ ccq { ccq_shipAddress = Just a })

-- | Helper to retrieve curbs on quote by quote id.
listCurbsOnQuote :: Context -> Int -> Double -> IO (Either APIError [CCCurb])
listCurbsOnQuote ctx qid curbMarkup = runQuery ctx curbBaseSql [qid]
  >>= eitherPassErr (passSuccess . map (\c -> c { ccc_priceEach = curbMarkup * ccc_basePriceEach c }))

-- | Edits the curb line item entry. Check SQL source for editable fields.
editCurb :: Context -> Int -> CCCurb -> IO (Either APIError CCCurb)
editCurb ctx curbId ccc
  | ccc_id ccc == curbId = runExecute ctx curbUpdateSql ccc
    >>= eitherPassErr (\_ -> passSuccess ccc)
  | otherwise = badReq 400 "CC curb uri id and body id mismatch"

-- | Removes curb line item entry from quote (not just single quantity!).
deleteCurb :: Context -> Int -> IO (Either APIError APIMsg)
deleteCurb ctx curbId = runExecute ctx deleteCurbSql [curbId]
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "No Content")

-- | Toggles confirmation status flag.
toggleCCQuoteConfirmationStatus :: Context -> Text -> Bool -> IO (Either APIError APIMsg)
toggleCCQuoteConfirmationStatus ctx pid setFlag =
  runExecute ctx toggleConfirmationStatusSql (setFlag, pid)
    >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "No Content")

-- | Generates quote from HTML template and quote/curb info.
-- TODO needs to apply markup/discount from customer lookup
generateCCQuoteHTML :: Context -> Text -> IO (Either APIError LBS.ByteString)
generateCCQuoteHTML ctx pid = do
  ct <- getCurrentTime
  rand <- nextRandom
  getCCQuoteByPID ctx pid
    >>= eitherPassErr (\ccq -> case ccq_shipAddress ccq of
                                    Nothing -> badReq 400 "CC quote must have shipping address set prior to quote generation"
                                    Just a -> curbCoQuoteHtml ct rand ccq a)

-- | Lookup markup/discount factor for customer based on CC customer designation.
lookupCustomerMarkup :: Context -> Text -> IO (Either APIError Double)
lookupCustomerMarkup ctx cdesig = runQuery ctx lookupSql [cdesig]
  >>= eitherPassErr (wasFound "CC customer designation not found")
  >>= eitherPassErr (passSuccess . ctl_markup)

-- | Retrieves sales data for curb co orders completed within date range.
getCCSalesData :: Context -> DateRange -> IO (Either APIError [CCQuote])
getCCSalesData ctx dr = getCCOrdersInDateRange ctx dr
  >>= eitherPassErr (\os -> do
    eccqs <- forM os (lookupQuoteByOrderId ctx . oid)
    pure $ flattenData eccqs
  )

lookupQuoteByOrderId :: Context -> Int -> IO (Either APIError CCQuote)
lookupQuoteByOrderId ctx ordId = runQuery ctx (baseSql <> mapSql) [ordId]
  >>= eitherPassErr (wasFound "CC quote not found")
  >>= eitherPassErr (appendCurbsToQuote ctx)

-- SQL --
-- | Base quote query string for retrieval. Performs customer type lookup to retrieve appropriate CC markup/discount factor.
baseSql :: Query
baseSql = "select \
          \cc_quotes.id as ccqId, \
          \cc_quotes.anytime_pid as ccqPID, \
          \customers.id as customerId, \
          \customers.name as customerName, \
          \customers.company as customerCompany, \
          \customers.customer_type as customerType, \
          \customers.markup as customerMarkup, \
          \customers.is_tax_exempt as customerIsTaxExempt, \
          \lu_cc_customer_types.markup as curbCoMarkup, \
          \cc_quotes.shipping_costs as shippingCosts, \
          \cc_quotes.is_confirmed as isConfirmed, \
          \cc_quotes.fast_pass_charge as fastPassCost \
          \from cc_quotes \
          \left join customers \
          \on cc_quotes.fk_customer_id = customers.id \
          \left join lu_cc_customer_types \
          \on customers.customer_type = lu_cc_customer_types.type_name"

-- | Main insertion SQL. Check database for default values.
insertSql :: Query
insertSql = "insert into cc_quotes (\
            \anytime_pid, \
            \fk_customer_id, \
            \shipping_costs, \
            \fast_pass_charge \
            \) values (?, ?, ?, ?) \
            \returning id"

-- | Base update/edit SQL.
updateSql :: Query
updateSql = "update cc_quotes \
            \set anytime_pid = ?, \
            \fk_customer_id = ?, \
            \shipping_costs = ?, \
            \is_confirmed = ?, \
            \fast_pass_charge = ? \
            \where cc_quotes.id = ?"

-- | Naive search for matching, case-sensitive strings.
searchSql :: Query
searchSql = " where \
            \cc_quotes.anytime_pid like ? or \
            \customers.name like ?"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by cc_quotes.anytime_pid desc"

-- | Pagination SQL portion.
paginationSql :: Query
paginationSql = " limit ? offset ?"

-- | Delete base quote SQL.
deleteQuoteSql :: Query
deleteQuoteSql = "delete from cc_quotes where cc_quotes.id = ?"

-- | Insert SQL for setting order id to quote.
setOrderIdSql :: Query
setOrderIdSql = "insert into m_curb_quote_orders (\
                \fk_curb_quote_id, \
                \fk_order_id \
                \) values (?, ?) \
                \returning id"

-- | Removes SQL for quote to order mapping.
removeMapSql :: Query
removeMapSql = "delete from m_curb_quote_orders where fk_order_id = ?"

-- | SQL for connecting order id to quote id via map table.
mapSql :: Query
mapSql = " join m_curb_quote_orders \
         \on cc_quotes.id = m_curb_quote_orders.fk_curb_quote_id \
         \where m_curb_quote_orders.fk_order_id = ?"

-- | Curb insert SQL (line item on quote).
curbInsertSql :: Query
curbInsertSql = "insert into m_curbs_quote (\
                \fk_cc_quote_id, \
                \old_unit, \
                \new_unit, \
                \adapter, \
                \price_each, \
                \quantity, \
                \curb_size, \
                \fk_metal_id, \
                \metal_weight, \
                \gasket_feet \
                \) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
                \returning id"

-- | Curb retrieval SQL (line item(s) on quote).
curbBaseSql :: Query
curbBaseSql = "select \
              \m_curbs_quote.id as curbId, \
              \m_curbs_quote.old_unit as oldUnit, \
              \m_curbs_quote.new_unit as newUnit, \
              \m_curbs_quote.adapter as adapter, \
              \m_curbs_quote.price_each as basePriceEach, \
              \m_curbs_quote.quantity as quantity, \
              \m_curbs_quote.curb_size as size, \
              \m_curbs_quote.fk_metal_id as metalId, \
              \materials.name as metalName, \
              \m_curbs_quote.metal_weight as metalWeight, \
              \m_curbs_quote.gasket_feet as gasketFeet \
              \from m_curbs_quote \
              \left join materials on \
              \m_curbs_quote.fk_metal_id = materials.id \
              \where m_curbs_quote.fk_cc_quote_id = ?"

-- | Base edit/update SQL for curb line items. Check source for editable fields.
curbUpdateSql :: Query
curbUpdateSql = "update m_curbs_quote set \
                \old_unit = ?, \
                \new_unit = ?, \
                \adapter = ?, \
                \price_each = ?, \
                \quantity = ?, \
                \curb_size = ?, \
                \fk_metal_id = ?, \
                \metal_weight = ?, \
                \gasket_feet = ? \
                \where m_curbs_quote.id = ?"

-- | Delete curb SQL.
deleteCurbSql :: Query
deleteCurbSql = "delete from m_curbs_quote where m_curbs_quote.id = ?"

-- | Delete all curbs on quoteSQL.
deleteAllCurbsSql :: Query
deleteAllCurbsSql = "delete from m_curbs_quote where m_curbs_quote.fk_cc_quote_id = ?"

-- | Toggle for confirmation status SQL.
toggleConfirmationStatusSql :: Query
toggleConfirmationStatusSql = "update cc_quotes set \
                              \is_confirmed = ? \
                              \where cc_quotes.anytime_pid = ?"

-- | Retrieves customer CC markup/discount base on type.
lookupSql :: Query
lookupSql = "select \
            \lu_cc_customer_types.markup as markup \
            \from lu_cc_customer_types \
            \where lu_cc_customer_types.type_name = ?"
