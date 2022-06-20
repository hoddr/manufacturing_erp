{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Quotes
Description: Quotes and estimates.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides functions related to quotes and project estimates. Quotes are a counterpart to orders,
providing a price estimate to customers prior to order confirmation.
Quote types include: QL - quoted list of item(s), QP - project estimate, and QT - quoted takeoff (TBD).
QL quotes are later linked to an order. QP estimates are dealt with through project billing processes.
TODO - more HTML work for generating a quote directly in the ERP, rather than passing on to QB.

Generated QL and QPs can be leveraged to generate a respective FL (fabricated list order) or
start a project. There are specific functions for this purpose. Currently,
project estimates (QP) do NOT contain line items. This will soon change.

-}

module Quotes
  ( -- * Functions
    addQuote
  , addQuoteSansLineItems
  , deleteQuote
  , editQuote
  , getProjectEstimate
  , getQuoteById
  , getQuoteDetailedSalesData
  , getQuoteReport
  , listQuoteSkeletons
  , listQuotes
  , removeQuoteOrderId
  , removeQuoteProjectId
  , setQuoteOrderId
  , setQuoteProjectId
  )
  where


import qualified Data.ByteString.Lazy as LBS
import Data.List ( foldl' )
import Data.Text ( Text )
import Data.Time.Clock ( getCurrentTime )
import Data.UUID.V4 ( nextRandom )
import Database.PostgreSQL.Simple ( Only(..)
                                  , Query
                                  )
import GHC.Int ( Int64 )


-- LOCAL IMPORTS --
import Addresses ( retrieveCustomerAddress )
import APITypes ( APIError(..)
                , Context(..)
                , Customer(..)
                , DetailedSalesData(..)
                , LaborRate(..)
                , LineItem(..)
                , Markup(..)
                , QueryPag(..)
                , Quote(..)
                , defQueryPag
                )
import Customers ( baseCustomerQuery
                 , getCustomerByName
                 )
import Database ( runExecute
                , runQuery
                )
import LineItems ( addQuoteLineItems
                 , deleteQuoteLineItems
                 , insertQuoteLineItems
                 , setLineItemOrderIdSql
                 )
import Markups ( getLatestMarkupRates
               , getLatestRate
               )
import Reports ( projectHtml
               , quoteHtml
               )
import Utils ( badReq
             , flattenData
             , eitherPassErr
             , gen
             , passSuccess
             , wasFound
             )


-- FUNCTIONS --
-- | Adds a QP estimate. Used for tracking project estimates prior to project start.
addQuoteSansLineItems :: Context
                      -> Quote
                      -> IO (Either APIError Quote)
addQuoteSansLineItems ctx q = do
  emkups <- getLatestMarkupRates ctx
  elabor <- getLatestRate ctx
  case (emkups, elabor) of
       (Left e, _) -> print e >> badReq 400 "Failed to retrieve markup rates"
       (_, Left e) -> print e >> badReq 400 "Failed to retrieve labor rate"
       (Right mkups, Right labor) ->
         runQuery ctx insertSql ( qtype q
                                , qnum q
                                , qsetPrice q
                                , cid $ qcust q
                                , False
                                , qpo q
                                , lrid labor
                                , mkupid mkups
                                , cmarkup $ qcust q
                                )
           >>= eitherPassErr genPartial
           >>= eitherPassErr (\quoteId -> passSuccess $ q { qid = quoteId })

-- | Adds a QL quote. The line items are attached after the quote id ('Int') is generated.
addQuote :: Context
         -> Text
         -> Bool
         -> ([LineItem], Text, Text, Text)
         -> IO (Either APIError Int)
addQuote _ _ _ ([], _, _, _) = badReq 400 "No line items provided"
addQuote ctx quoteType isWrapped (lis, custName, quoteNumber, po) = do
  ec <- getCustomerByName ctx custName
  emkups <- getLatestMarkupRates ctx
  elabor <- getLatestRate ctx
  case (ec, emkups, elabor) of
       (Left e, _, _) -> print e >> badReq 400 "Failed to retrieve customer"
       (_, Left e, _) -> print e >> badReq 400 "Failed to retrieve markup rates"
       (_, _, Left e) -> print e >> badReq 400 "Failed to retrieve labor rate"
       (Right c, Right mkups, Right labor) ->
         runQuery ctx insertSql ( quoteType
                                , quoteNumber
                                , Nothing :: Maybe Double
                                , cid c
                                , isWrapped
                                , po
                                , lrid labor
                                , mkupid mkups
                                , cmarkup c
                                )
           >>= eitherPassErr genPartial
           >>= eitherPassErr (insertQuoteLineItems ctx lis)

-- | Quote specific generation for new id passage.
genPartial :: [Only Int] -> IO (Either APIError Int)
genPartial = gen (\_ nid -> nid) "Failure to return new quote id" (123 :: Int) -- 123 is placeholder

-- | TODO should this change customer markup and reprice? Edits the specified quote by id ('Int').
editQuote :: Context -> Quote -> Int -> IO (Either APIError Quote)
editQuote ctx q quoteId
  | qid q == quoteId = runExecute ctx updateSql ( qtype q
                                                , cid $ qcust q
                                                , qnum q
                                                , case qlineItems q of
                                                       [] -> qsetPrice q
                                                       _ -> Nothing :: Maybe Double
                                                , Nothing :: Maybe Text -- TODO for quote po at some point
                                                , qid q
                                                )
                       >>= eitherPassErr (\(_ :: Int64) -> passSuccess q)
  | otherwise = badReq 400 "Quote uri id and body id mismatch"

{- |

List quotes without line items within bounds of querystring search and/or paginations. Will return
an empty list for no results.
Use this when accessign numerous quotes without immediate need for accessing their line item
information.

-}
listQuoteSkeletons :: Context -> QueryPag -> IO (Either APIError [Quote])
listQuoteSkeletons ctx (QueryPag ("", l, o)) =
  runQuery ctx (baseSql <> sortSql <> paginationSql) (l, o)
listQuoteSkeletons ctx (QueryPag (s, l, o)) =
  runQuery ctx (baseSql <> searchSql <> sortSql <> paginationSql) (s, s, s, l, o)

{- |

List quotes with line items within bounds of querystring search and/or paginations.
Will return an empty list for no results. Use this only when line item data is required.

-}
listQuotes :: Context -> IO (Either APIError [Quote])
listQuotes ctx = listQuoteSkeletons ctx defQueryPag
  >>= eitherPassErr (\qs -> do
    eqs <- mapM (addQuoteLineItems ctx) qs
    pure $ flattenData eqs
  )

-- | Removes quote and associated line items.
deleteQuote :: Context -> Int -> IO (Either APIError ())
deleteQuote ctx quoteId = runExecute ctx deleteQuoteSql [quoteId]
  >>= eitherPassErr (\_ -> deleteQuoteLineItems ctx quoteId)

-- | Retrieves quote and line items by specified quote id ('Int'). Returns a 404 error
-- if the quote is not found.
getQuoteById :: Context -> Int -> IO (Either APIError Quote)
getQuoteById ctx quoteId = runQuery ctx (baseSql <> " where quotes.id = ?") [quoteId]
  >>= eitherPassErr (wasFound "Quote not found")
  >>= eitherPassErr (addQuoteLineItems ctx)

-- | Generates HTML quote report (item, description, quantity, price each). See 'quoteHtml'.
getQuoteReport :: Context -> Int -> IO (Either APIError LBS.ByteString)
getQuoteReport ctx quoteId = do
  ct <- getCurrentTime
  rand <- nextRandom
  getQuoteById ctx quoteId
    >>= eitherPassErr (\q -> retrieveCustomerAddress ctx (cid $ qcust q)
      >>= eitherPassErr (quoteHtml ct rand q)
    )

-- | Generates a project estimate cover page. See 'projectHtml'.
getProjectEstimate :: Context -> Int -> IO (Either APIError LBS.ByteString)
getProjectEstimate ctx quoteId = do
  ct <- getCurrentTime
  rand <- nextRandom
  getQuoteById ctx quoteId
    >>= eitherPassErr (\q -> if qtype q /= "QP"
                                then badReq 400 "Project estimates cannot be generated for non-QP quotes"
                                else projectHtml ct rand q)

-- | Assigns the quote line items to an order with the given id. This permits sharing of the line
-- items (i.e. avoids duplicating data!).
setQuoteOrderId :: Context -> Quote -> Int -> IO (Either APIError ())
setQuoteOrderId ctx q newoid = runExecute ctx setOrderIdSql (newoid, qid q)
  >>= eitherPassErr (\_ ->
    runExecute ctx setLineItemOrderIdSql (newoid, qid q)
    >>= eitherPassErr (\_ -> passSuccess ())
  )

-- | Removes the quote to order line item id mapping.
removeQuoteOrderId :: Context -> Int -> IO (Either APIError ())
removeQuoteOrderId ctx ordId = runExecute ctx removeQuoteOrderAssocSql [ordId]
  >>= eitherPassErr (\_ -> passSuccess ())

-- | Creates a mapping from the quote to a project with the given id ('Int').
setQuoteProjectId :: Context -> Quote -> Int -> IO (Either APIError ())
setQuoteProjectId ctx q prjid = runExecute ctx setProjectIdSql (prjid, qid q)
  >>= eitherPassErr (\_ -> passSuccess ())

-- | Removes the mapping from the quote and the project with the given project id.
removeQuoteProjectId :: Context -> Int -> IO (Either APIError ())
removeQuoteProjectId ctx prjid = runExecute ctx removeQuoteProjectAssocSql [prjid]
  >>= eitherPassErr (\_ -> passSuccess ())

{- |
Retrieves full sales data by line item only for the specified quote id ('Int').
-}
getQuoteDetailedSalesData :: Context -> Int -> IO (Either APIError [DetailedSalesData])
getQuoteDetailedSalesData ctx quoteId = getQuoteById ctx quoteId
  >>= eitherPassErr (\q -> unnestLineItems [q])

-- | Helper to accumulate line items from quotes into detailed sales data format.
unnestLineItems :: [Quote] -> IO (Either APIError [DetailedSalesData])
unnestLineItems qs = passSuccess $
  foldl' (\outacc q -> outacc ++
    foldl' (\inacc li ->
      DetailedSalesData ( qcreated q
                        , qcust q
                        , qnum q
                        , qlaborRate q
                        , qmarkups q
                        , qcustomerMarkup q
                        , li
                        ) : inacc
    ) [] (qlineItems q)
  ) [] qs

-- SQL --
{- |

Base query SQL. The initial totals are done to provide a quick quote total value without having to
retrieve the line items in their entirety (for QLs).

-}
baseSql :: Query
baseSql = "with totals as (\
            \select \
            \line_items.quote_id as fk_quote_id, \
            \coalesce(sum(line_items.price * line_items.quantity), 0.00) as total \
            \from line_items \
            \group by fk_quote_id \
          \) \
          \select \
          \quotes.id as id, \
          \quotes.quote_type as type, \
          \quotes.created as created, \
          \quotes.quote_number as qnum, \
          \quotes.quote_po as qpo, \
          \quotes.order_id as orderId, \
          \orders.order_number as onum, \
          \quotes.project_id as projectId, \
          \projects.app_id as prjNum, \
          \quotes.set_quote_price as setPrice, "
       <> baseCustomerQuery -- this doesn't contain comma at end, needed here
       <> ", \
          \(case totals.total is null when true then 0.00 else totals.total end) as price, \
          \quotes.is_wrapped as isWrapped,\
          \labor_rate.id as laborRateId, \
          \labor_rate.shop_rate as laborShopRate, \
          \labor_rate.overhead_rate as overheadRate, \
          \labor_rate.rate as laborRate, \
          \labor_rate.is_current as laborIsCurrent, \
          \labor_rate.base_unit as laborBaseUnit, \
          \labor_rate.added_at as laborAddedAt, \
          \markups.id as markupId, \
          \markups.purchase as purchaseMarkup, \
          \markups.fabrication as fabricationMarkup, \
          \markups.rectangular_fab as rectMarkup, \
          \markups.round_fab as roundMarkup, \
          \markups.oval_fab as ovalMarkup, \
          \markups.stock as stockMarkup, \
          \markups.assembly as assemblyMarkup, \
          \markups.material as materialMarkup, \
          \markups.quote as quoteMarkup, \
          \markups.project as projectMarkup, \
          \markups.normal as orderMarkup, \
          \markups.is_current as isCurrentMarkup, \
          \markups.added_at as addedAtMarkup, \
          \quotes.customer_markup as customerMarkupSaved \
          \from quotes \
          \inner join customers \
          \on quotes.fk_customer_id = customers.id \
          \left join totals \
          \on quotes.id = totals.fk_quote_id \
          \left join orders \
          \on quotes.order_id = orders.id \
          \left join projects \
          \on quotes.project_id = projects.id \
          \left join labor_rate \
          \on quotes.fk_labor_id = labor_rate.id \
          \left join markups \
          \on quotes.fk_markups_id = markups.id"

-- | Main insertion SQL. Check database for default values.
insertSql :: Query
insertSql = "insert into quotes (\
            \quote_type, \
            \quote_number, \
            \set_quote_price, \
            \fk_customer_id, \
            \is_wrapped, \
            \quote_po, \
            \fk_labor_id, \
            \fk_markups_id, \
            \customer_markup) \
            \values (?, ?, ?, ?, ?, ?, ?, ?, ?) \
            \returning id"

-- | Search SQL for naive, case-insensitive search on provided fields.
searchSql :: Query
searchSql = " where \
            \quotes.quote_number like ? or \
            \customers.name like ? \
            \or orders.order_number like ?"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by quotes.quote_number desc"

-- | Pagination SQL.
paginationSql :: Query
paginationSql = " limit ? offset ?"

-- | Edit/update SQL.
updateSql :: Query
updateSql = "update quotes set \
            \quote_type = ?, \
            \fk_customer_id = ?, \
            \quote_number = ?, \
            \set_quote_price = ?, \
            \quote_po = ? \
            \where quotes.id = ?"

-- | Delete quote SQL.
deleteQuoteSql :: Query
deleteQuoteSql = "delete from quotes where quotes.id = ?"

-- | Sets quote to either order or project mapping base SQL generator.
setProjectOrderIdSql :: Query -> Query
setProjectOrderIdSql field = "update quotes set " <> field <> " = ? where quotes.id = ?"

-- | Wrapper to select order mapping.
setOrderIdSql :: Query
setOrderIdSql = setProjectOrderIdSql "order_id"

-- | Removes the quote to order mapping.
removeQuoteOrderAssocSql :: Query
removeQuoteOrderAssocSql = "update quotes set order_id = NULL where quotes.order_id = ?"

-- | Wrapper to select project mapping.
setProjectIdSql :: Query
setProjectIdSql = setProjectOrderIdSql "project_id"

-- | Removes the quote to project mapping.
removeQuoteProjectAssocSql :: Query
removeQuoteProjectAssocSql = "update quotes set project_id = NULL where quotes.project_id = ?"
