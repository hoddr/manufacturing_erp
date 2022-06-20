{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Orders
Description: Generation and management of orders, BD fabrication, project management, and more.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides functions related to order generation, editing, deletion, and status toggles.
Also a main area for report generation relating to sales, as the orders contain the date for line items.
The order types are: FS - standard fabrication, FP - project order, FL - order from quoted list,
PGB - progress building (i.e. BD, where assemblies must be built), and SP - internal stock generation order.

FS orders are written in via script during billing. FP orders are written in during billing as well, but are entered as
neither complete nor billed, as projects are billed in bunches. FL orders are generated from an already
written quote (via script); the line items are shared between the quote and the order.

PGB or BD orders are written
during the initial order processing phase, after any new parts are priced, generated in the ERP, added to the pricing list,
and added to CAM. The assemblies are then built as reported by the shop, at which point the material
and sub item usage is tracked. Finally, the order is billed and marked as such, removing the assemblies
from inventory. This tracking schedule is critical for accounting reasons.

FP orders are written in as neither complete nor billed. The line items are priced, but no inventory adjustments are made at that time.
As the orders are completed or shipped in the shop, they are marked as complete in the ERP,
performing the necessary inventory adjustments at that time. Only complete or shipped orders are considered
in any given cycle for project billing; see external documentation for more details. Finally, the orders are marked
as billed and invoiced, ending the main tracking the ERP for statuses.

Finally, a number of reports are generated from order data, including build assemblies and stock reports,
detailed sales data by date range, detailed sales data by order, packing slips, simple sales data, and more.
The detailed sales data reports are intended to provide all the base costing information that would be
necessary to inspect and track the pricing logic for the line item at any point in the future. As such,
the system tracks the system and customer specific markups at the time the order was written to the system.

-}

module Orders
  ( -- * Functions
    addOrder
  , addOrderShell
  , convertCCQuoteToOrder
  , deleteOrder
  , editOrder
  , getCCOrdersInDateRange
  , getOrderById
  , getOrderByNumber
  , getNotBilledOrderStatus
  , listNotBilledOrders
  , listNotDoneOrders
  , listNotDoneProjectOrders
  , listOrders
  , listOrderSkeletons
  , listProjectOrders
  , listProjectOrdersFilter
  , listSectionOrders
  , listSectionOrdersFilter
  , orderToRawLineItems
  , verifyStockOnly
  -- * Reports
  , getBuildAssemblyData
  , getDetailedSalesData
  , getOrderDetailedSalesData
  , getOrderPackingSlip
  , getOrderSalesData
  , getProjectDetailedSalesData
  -- * Status
  , runNowDoneCheck
  , setOrderToBilled
  , setOrderToNotBilled
  , setOrderToComplete
  , setOrderToNotComplete
  , setOrderToCompleteWithTime
  )
  where

import qualified Data.ByteString.Lazy as LBS
import Data.List ( foldl' )
import qualified Data.Map as MAP
import Data.Maybe ( fromMaybe )
import Data.Time ( UTCTime(..) )
import Data.Time.Clock ( getCurrentTime )
import Data.Text ( Text
                 , pack
                 )
import Data.UUID.V4 ( nextRandom )
import Database.PostgreSQL.Simple ( Only(..)
                                  , Query
                                  )
import GHC.Int ( Int64 )
import System.Environment ( getEnv )

-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , APIMsg
                , BuildAssemblyReport(..)
                , Context(..)
                , CCQuote(..)
                , CCCurb(..)
                , Customer(..)
                , DateRange(..)
                , DetailedSalesData(..)
                , JobExt(..)
                , LaborRate(..)
                , LineItem(..)
                , Markup(..)
                , MembrainOrderData(..)
                , Order(..)
                , QueryPag(..)
                , RawLineItem(..)
                , defQueryPag
                )
import Customers ( baseCustomerQuery
                 , getCustomerByName
                 )
import Database ( runExecute
                , runQuery
                , runQuery_
                )
import qualified ExternalClient as EC
import LineItems ( addOrderLineItems
                 , deleteOrderLineItems
                 , insertOrderLineItems
                 )
import Markups ( getLatestMarkupRates
               , getLatestRate
               )
import Reports ( packingSlipHtml )
import Utils ( badReq
             , apiMsg201
             , eitherPassErr
             , flattenData
             , gen
             , passSuccess
             , wasFound
             )

-- | Flag for BD or build assembly orders.
type IsProgressiveFlag = Bool

-- | Flag for a billed order.
type IsBilled = Bool

-- | Flag for a complete or shipped order.
type IsComplete = Bool

-- FUNCTIONS --
{- |

Adds a new order with the given flags and line items. Customer is retrieved to verify existence
and access the customer specific markup for storage. Current system markups and labor rates are
also retrieved for this purpose. The line items are inserted and linked to the new order after
database generation of the next sequential id.

-}
addOrder :: Context
         -> IsProgressiveFlag
         -> IsBilled
         -> IsComplete
         -> Text
         -> Maybe Text
         -> Bool
         -> ([LineItem], Text, Text, Text)
         -> IO (Either APIError Int)
addOrder _ _ _ _ _ _ _ ([], _, _, _) = badReq 400 "No line items provided"
addOrder ctx isProgressiveBill isBilled isComplete ordType mInvoiceNumber isWrapped (lis, custName, orderNumber, po) = do
  ct <- getCurrentTime
  ec <- getCustomerByName ctx custName
  emkups <- getLatestMarkupRates ctx
  elabor <- getLatestRate ctx
  case (ec, emkups, elabor) of
       (Left e, _, _) -> print e >> badReq 400 "Failed to retrieve customer"
       (_, Left e, _) -> print e >> badReq 400 "Failed to retrieve markup rates"
       (_, _, Left e) -> print e >> badReq 400 "Failed to retrieve labor rate"
       (Right c, Right mkups, Right labor) ->
         runQuery ctx insertSql ( ordType
                                , po
                                , cid c
                                , orderNumber
                                , isWrapped
                                , isBilled
                                , if isBilled then Just ct else Nothing
                                , mInvoiceNumber
                                , isProgressiveBill
                                , isComplete
                                , if isComplete then Just ct else Nothing
                                , lrid labor
                                , mkupid mkups
                                , cmarkup c
                                )
         >>= eitherPassErr genPartial
         >>= eitherPassErr (insertOrderLineItems ctx lis)

{- |

TODO Consider adding flags as pass in args.

Adds an order without adding any line items. At the moment, the order flags are fixed. This function
is intended only for use with the action of taking a quote to an order, the QL to FL process.
See 'Controllers.pQuoteToOrder' for usage.

-}
addOrderShell :: Context -> Order -> Markup -> LaborRate -> IO (Either APIError Int)
addOrderShell ctx o mkups labor =
  runQuery ctx insertSql ( otype o
                         , opo o
                         , cid $ ocust o
                         , onum o
                         , False
                         , False
                         , Nothing :: Maybe UTCTime
                         , Nothing :: Maybe Text
                         , False
                         , False
                         , Nothing :: Maybe UTCTime
                         , lrid labor
                         , mkupid mkups
                         , cmarkup $ ocust o
                         )
  >>= eitherPassErr genPartial

-- | Order specific generation for new id passage.
genPartial :: [Only Int] -> IO (Either APIError Int)
genPartial = gen (\_ nid -> nid) "Failure to return new order id" (123 :: Int) -- 123 is placeholder

-- | TODO should this change the customer markup and reprice? Edits the specified order by id ('Int').
editOrder :: Context -> Order -> Int -> IO (Either APIError Order)
editOrder c o ordId
  | oid o == ordId = runExecute c updateSql ( otype o
                                            , opo o
                                            , cid $ ocust o
                                            , onum o
                                            , oid o
                                            )
                     >>= eitherPassErr (\(_ :: Int64) -> passSuccess o)
  | otherwise = badReq 400 "Order uri id and body id mismatch"

{- |

List orders with line items within bounds of querystring search and/or paginations.
Will return an empty list for no results. Note that line items are added after the initial
database query. Use this when line item data is required.

-}
listOrders :: Context -> IO (Either APIError [Order])
listOrders c = listOrderSkeletons c defQueryPag
  >>= eitherPassErr (\os -> do
    eos <- mapM (addOrderLineItems c) os
    pure $ flattenData eos
  )

{- |

List orders without line items within bounds of querystring search and/or paginations. Will return an empty list for no results.
Use this when accessing numerous orders without immediate need for accessing their line item information.

-}
listOrderSkeletons :: Context -> QueryPag -> IO (Either APIError [Order])
listOrderSkeletons ctx (QueryPag ("", l, o)) =
  runQuery ctx (baseSql <> mainJoinSql <> sortSql <> paginationSql) (l, o)
listOrderSkeletons ctx (QueryPag (s, l, o)) =
  runQuery ctx (baseSql <> mainJoinSql <> searchSql <> sortSql <> paginationSql) (s, s, s, s, l, o)

-- | Retrieves order by specified id ('Int'). Returns 404 error if not found. Line items are included.
getOrderById :: Context -> Int -> IO (Either APIError Order)
getOrderById c ordid = runQuery c (baseSql <> mainJoinSql <> " where orders.id = ?") [ordid]
  >>= eitherPassErr (wasFound "Order not found")
  >>= eitherPassErr (addOrderLineItems c)

-- | Retrieves order by order number ('Text'). Returns 404 error if not found. Line items are included.
getOrderByNumber :: Context -> Text -> IO (Either APIError Order)
getOrderByNumber c ordnum = runQuery c (baseSql <> mainJoinSql <> " where orders.order_number = ?") [ordnum]
  >>= eitherPassErr (wasFound "Order not found")
  >>= eitherPassErr (addOrderLineItems c)

-- | Deletes the specified order. Line items are removed after with the proper inventory adjustments.
-- See 'LineItems.deleteOrderLineItems' for details.
deleteOrder :: Context -> Int -> IO (Either APIError ())
deleteOrder ctx ordId = runExecute ctx deleteOrderSql [ordId]
  >>= eitherPassErr (\_ -> deleteOrderLineItems ctx ordId)

-- | Base function for retrieving orders within a given group (project, section). Includes line items.
listOrdersByGroup :: Query -> Context -> Int -> IO (Either APIError [Order])
listOrdersByGroup q c groupid = runQuery c (baseSql <> q) [groupid]
  >>= eitherPassErr (addAndFlattenLineItems c)

-- | Wrapper for group retrieval on project level.
listProjectOrders :: Context -> Int -> IO (Either APIError [Order])
listProjectOrders = listOrdersByGroup projectJoinSql

-- | Wrapper for group retrieval on project section level.
listSectionOrders :: Context -> Int -> IO (Either APIError [Order])
listSectionOrders = listOrdersByGroup sectionJoinSql

-- | Base function for retrieving orders within a given group (project, section).
-- Includes line items. Also has added filter SQL.
listOrdersByGroupFilter :: Query -> Context -> Int -> IO (Either APIError [Order])
listOrdersByGroupFilter q c groupid =
  runQuery c (baseSql <> q <> filterSql <> sortSql) [groupid]
  >>= eitherPassErr (addAndFlattenLineItems c)

-- | Wrapper for group retrieval on project level with filter.
listProjectOrdersFilter :: Context -> Int -> IO (Either APIError [Order])
listProjectOrdersFilter = listOrdersByGroupFilter projectJoinSql

-- | Wrapper for group retrieval on project section level with filter.
listSectionOrdersFilter :: Context -> Int -> IO (Either APIError [Order])
listSectionOrdersFilter = listOrdersByGroupFilter sectionJoinSql

-- | Helper to add order line items and coalesce monadic actions.
addAndFlattenLineItems :: Context -> [Order] -> IO (Either APIError [Order])
addAndFlattenLineItems c os = do
  eos <- mapM (addOrderLineItems c) os
  pure $ flattenData eos

-- | Sets the order to billed.
setOrderToBilled :: Context -> Int -> Text -> IO (Either APIError APIMsg)
setOrderToBilled c ordId invoiceNumber = runExecute c setToBilledSql (invoiceNumber, ordId)
  >>= eitherPassErr (\(_ :: Int64) -> passSuccess $ apiMsg201 "Order set to billed")

-- | Sets the order to not billed.
setOrderToNotBilled :: Context -> Int -> IO (Either APIError APIMsg)
setOrderToNotBilled c ordId = runExecute c setToNotBilledSql [ordId]
  >>= eitherPassErr (\(_ :: Int64) -> passSuccess $ apiMsg201 "Order set to not billed")

-- | Sets the order to complete.
setOrderToComplete :: Context -> Int -> IO (Either APIError APIMsg)
setOrderToComplete c ordId = runExecute c setToCompleteSql [ordId]
  >>= eitherPassErr (\(_ :: Int64) -> passSuccess $ apiMsg201 "Order set to complete/shipped")

-- | Sets the order to not complete.
setOrderToNotComplete :: Context -> Int -> IO (Either APIError APIMsg)
setOrderToNotComplete c ordId = runExecute c setToNotCompleteSql [ordId]
  >>= eitherPassErr (\(_ :: Int64) -> passSuccess $ apiMsg201 "Order set to NOT complete/shipped")

-- | Sets the order to complete with the supplied UTCTime.
setOrderToCompleteWithTime :: Context -> Int -> UTCTime -> IO (Either APIError APIMsg)
setOrderToCompleteWithTime c ordId u = runExecute c setToCompleteTimeSql (u, ordId)
  >>= eitherPassErr (\(_ :: Int64) -> passSuccess $ apiMsg201 "Order set to complete/shipped")

-- | Helper to export line items of order to QB-ready import.
orderToRawLineItems :: Order -> [RawLineItem]
orderToRawLineItems o =
  map (\li -> RawLineItem { rli_customer = cname $ ocust o
                          , rli_orderNumber = onum o
                          , rli_po = opo o
                          , rli_category = lcategory li
                          , rli_quantity = lquant li
                          , rli_name = ldesc li
                          , rli_gauge = ""
                          , rli_material = ""
                          , rli_materialWeight = 0.00
                          , rli_weight = fromMaybe 0.00 (lweight li)
                          , rli_linerMaterial = ""
                          , rli_linerArea = 0.00
                          , rli_skinGauge = ""
                          , rli_skinMaterial = ""
                          , rli_skinWeight = 0.00
                          , rli_accessoryCost = 0.00
                          , rli_rate = ""
                          , rli_isWrapped = False
                          , rli_priceEach = lprice li
                          , rli_length = 0.00
                          , rli_isCatalogue = False
                          , rli_labor = Just $ llabor li
                          }) (olineItems o)

-- | Verifies that the line items for the order are all in the Stock category. For SP (stock) orders only.
verifyStockOnly :: [RawLineItem] -> IO (Either APIError [RawLineItem])
verifyStockOnly rlis =
  case filter ((/=) "Stock Pull" . rli_category) rlis of
       [] -> passSuccess rlis
       _ -> badReq 400 "Stock orders must not have non-stock items"

{- |

Retrieves basic sales data for orders within the specified 'DateRange'. Stock orders are filtered OUT of this.
TODO there is an open question for how to handle PGB/BD orders here. Can soon leverage billed date instead!

-}
getOrderSalesData :: Context -> DateRange -> IO (Either APIError [MembrainOrderData])
getOrderSalesData ctx dr = runQuery ctx (baseSql <> mainJoinSql <> orderDateSearchSql) (dr_start dr, dr_end dr, "SP" :: Text)
  >>= eitherPassErr (passSuccess . map MembrainOrderData)

{- |

Retrieves full sales data by line item from orders within the given date range. SP (stock) orders
are filtered OUT of this.
TODO there is an open question for how to handle PGB/BD orders here. Can soon leverage billed date instead!

-}
getDetailedSalesData :: Context -> DateRange -> IO (Either APIError [DetailedSalesData])
getDetailedSalesData ctx dr = runQuery ctx (baseSql <> mainJoinSql <> orderDateSearchSql) (dr_start dr, dr_end dr, "SP" :: Text)
  >>= eitherPassErr (\os -> do
    eos <- mapM (addOrderLineItems ctx) os
    pure $ flattenData eos
  )
  >>= eitherPassErr unnestLineItems

{- |

Retrieves full sales data by line item only for the specified order id ('Int').

-}
getOrderDetailedSalesData :: Context -> Int -> IO (Either APIError [DetailedSalesData])
getOrderDetailedSalesData ctx ordId = getOrderById ctx ordId
  >>= eitherPassErr (\o -> unnestLineItems [o])

-- | Helper to accumulate line items from orders into detailed sales data format.
unnestLineItems :: [Order] -> IO (Either APIError [DetailedSalesData])
unnestLineItems os = passSuccess $
  foldl' (\outacc o -> outacc ++
    foldl' (\inacc li ->
      DetailedSalesData ( ocreated o
                        , ocust o
                        , onum o
                        , olabor o
                        , omarkups o
                        , ocustomerMarkup o
                        , li
                        ) : inacc
    ) [] (olineItems o)
  ) [] os

getCCOrdersInDateRange :: Context -> DateRange -> IO (Either APIError [Order])
getCCOrdersInDateRange ctx dr = runQuery ctx (baseSql <> mainJoinSql <> curbCoOrderDateSearchSql) (dr_start dr, dr_end dr)

-- | Retrieves line item details for a given project.
getProjectDetailedSalesData :: Context -> Int -> IO (Either APIError [DetailedSalesData])
getProjectDetailedSalesData ctx pid = listProjectOrders ctx pid
  >>= eitherPassErr unnestLineItems

{- |

Retrieves all build assembly data needed for accounting from SP and PGB orders within
the given date range. TODO there is an open question for how to handle PGB orders here.
Can soon leverage billed date here!

-}
getBuildAssemblyData :: Context -> DateRange -> IO (Either APIError [BuildAssemblyReport])
getBuildAssemblyData ctx dr = runQuery ctx (baseSql <> mainJoinSql <> buildAssemblySearchSql) ( dr_start dr
                                                                                              , dr_end dr
                                                                                              , "SP" :: Text
                                                                                              , "PGB" :: Text
                                                                                              )
  >>= eitherPassErr (\os -> do
    eos <- mapM (addOrderLineItems ctx) os
    pure $ flattenData eos
  )
  >>= eitherPassErr accumBuilds

-- | Helper to accumulate the build reports for item type/name.
accumBuilds :: [Order] -> IO (Either APIError [BuildAssemblyReport])
accumBuilds os = passSuccess $ MAP.elems $
  foldl' (\outmap o ->
    foldl' (\inmap li ->
      let qtyToUse = if lcategory li /= "Assembly" || lisFabbed li
                        then lquant li -- not assembly OR fabricated in full
                        else lquantFabbed li -- assembly AND NOT fabricated in full
          matCost = qtyToUse * ( fromMaybe 0.00 (lmatCost li)
                               + fromMaybe 0.00 (llinerCost li)
                               + fromMaybe 0.00 (lskinCost li)
                               + fromMaybe 0.00 (laccessoryCost li)
                               )
          labCost = qtyToUse * fromMaybe 0.00 (llaborCost li)
          totCost = matCost + labCost

      in
        -- only include stock spiral and assemblies (BD)
        if lcategory li == "Stock Pull" || lcategory li == "Assembly"
           then MAP.insertWith (<>) (ldesc li) (BuildAssemblyReport ( ldesc li
                                                                    , totCost
                                                                    , matCost
                                                                    , labCost
                                                                    , qtyToUse
                                                                    )
                                               ) inmap
           else inmap
    ) outmap (olineItems o)
  ) startMap os
  where startMap :: MAP.Map Text BuildAssemblyReport
        startMap = MAP.empty

-- | Retrieves order information to generate packing slip HTML file.
getOrderPackingSlip :: Context -> Int -> IO (Either APIError LBS.ByteString)
getOrderPackingSlip ctx ordid = do
  ct <- getCurrentTime
  rand <- nextRandom
  getOrderById ctx ordid >>= eitherPassErr (packingSlipHtml ct rand)

-- | Retrieves list of non-complete FP project orders.
listNotDoneProjectOrders :: Context -> IO (Either APIError [Text])
listNotDoneProjectOrders ctx = runQuery_ ctx (baseSql <> mainJoinSql <> nonCompleteProjectSql)
  >>= eitherPassErr (passSuccess . map onum)

-- | Retrieves list of non-complete regular orders (SO, FP excluded).
listNotDoneOrders :: Context -> IO (Either APIError [Text])
listNotDoneOrders ctx = runQuery_ ctx (baseSql <> mainJoinSql <> nonCompleteSql <> sortSql)
  >>= eitherPassErr (passSuccess . map onum)

-- | Retrieves list of non-billed regular orders (SO, FP excluded).
listNotBilledOrders :: Context -> IO (Either APIError [Text])
listNotBilledOrders ctx = runQuery_ ctx (baseSql <> mainJoinSql <> nonBilledSql <> sortSql)
  >>= eitherPassErr (passSuccess . map onum)

-- | Retrieves list of non-billed regular orders (SO, PGB, FP excluded).
getNotBilledOrderStatus :: Context -> Text -> IO (Either APIError (Maybe JobExt))
getNotBilledOrderStatus = runNowDoneCheck

-- | Calls external app for order complete/shipped status check.
runNowDoneCheck :: Context -> Text -> IO (Either APIError (Maybe JobExt))
runNowDoneCheck _ ordnum = do
  key <- getEnv "ERP_PB_OWL_APP_KEY"
  EC.getOrderCompletionStatus (pack key) ordnum

-- | Converter from CC confirmed quote to sales order. Order type is FC, for fabricated curb (from quote).
convertCCQuoteToOrder :: Context
                          -> Text -- order number
                          -> Text -- po
                          -> CCQuote
                          -> IO (Either APIError Int)
convertCCQuoteToOrder ctx ordNum po ccq =
  addOrder ctx False False False "FC" Nothing False (lis, cname $ ccq_customer ccq, ordNum, po)
  where lisPre = map (\ccc -> LineItem { lid = 0 -- placeholder
                                       , ldesc = ccc_adapter ccc <> " - AT-" <> ccq_pid ccq
                                       , lquant = ccc_quantity ccc
                                       , llabor = 0.0
                                       , llaborCost = Nothing
                                       , lweight = Just $ ccc_metalWeight ccc
                                       , lisExtra = False
                                       , lprice = ccc_priceEach ccc
                                       , lorderId = Nothing
                                       , lorderNumber = Just ordNum
                                       , lquoteId = Nothing
                                       , lquoteNumber = Nothing
                                       , lmatId = Just $ ccc_metalId ccc
                                       , lmatName = Just $ ccc_metalName ccc
                                       , lmatWeight = Just $ ccc_metalWeight ccc
                                       , lmatCost = Nothing
                                       , llinerId = Nothing
                                       , llinerName = Nothing
                                       , llinerArea = Nothing
                                       , llinerCost = Nothing
                                       , lskinId = Nothing
                                       , lskinName = Nothing
                                       , lskinWeight = Nothing
                                       , lskinCost = Nothing
                                       , laccessoryCost = Nothing
                                       , lcategory = "CC"
                                       , llength = 0.0
                                       , lquantFabbed = 0.0
                                       , lisFabbed = True
                                       }
                     ) (ccq_curbs ccq)
        plis = (: lisPre) (LineItem { lid = 0 -- placeholder
                                    , ldesc = "Shipping & Handling - AT-" <> ccq_pid ccq
                                    , lquant = 1
                                    , llabor = 0.0
                                    , llaborCost = Nothing
                                    , lweight = Nothing
                                    , lisExtra = False
                                    , lprice = ccq_shippingCosts ccq
                                    , lorderId = Nothing
                                    , lorderNumber = Just ordNum
                                    , lquoteId = Nothing
                                    , lquoteNumber = Nothing
                                    , lmatId = Nothing
                                    , lmatName = Nothing
                                    , lmatWeight = Nothing
                                    , lmatCost = Nothing
                                    , llinerId = Nothing
                                    , llinerName = Nothing
                                    , llinerArea = Nothing
                                    , llinerCost = Nothing
                                    , lskinId = Nothing
                                    , lskinName = Nothing
                                    , lskinWeight = Nothing
                                    , lskinCost = Nothing
                                    , laccessoryCost = Nothing
                                    , lcategory = "Freight"
                                    , llength = 0.0
                                    , lquantFabbed = 0.0
                                    , lisFabbed = True
                                    }
                          )
        lis = if ccq_fastPassCost ccq <= 0.00
                 then plis
                 else (: plis) (LineItem { lid = 0 -- placeholder
                                         , ldesc = "Fast Pass Fee - AT-" <> ccq_pid ccq
                                         , lquant = 1
                                         , llabor = 0.0
                                         , llaborCost = Nothing
                                         , lweight = Nothing
                                         , lisExtra = False
                                         , lprice = ccq_fastPassCost ccq
                                         , lorderId = Nothing
                                         , lorderNumber = Just ordNum
                                         , lquoteId = Nothing
                                         , lquoteNumber = Nothing
                                         , lmatId = Nothing
                                         , lmatName = Nothing
                                         , lmatWeight = Nothing
                                         , lmatCost = Nothing
                                         , llinerId = Nothing
                                         , llinerName = Nothing
                                         , llinerArea = Nothing
                                         , llinerCost = Nothing
                                         , lskinId = Nothing
                                         , lskinName = Nothing
                                         , lskinWeight = Nothing
                                         , lskinCost = Nothing
                                         , laccessoryCost = Nothing
                                         , lcategory = "Fast Pass"
                                         , llength = 0.0
                                         , lquantFabbed = 0.0
                                         , lisFabbed = True
                                         }
                               )

-- SQL --
{- |

Base query SQL. There are quite a few joins present; be careful when modifying this query.
The initial totals are done to provide a quick order total value without having to
retrieve the line items in their entirety.

-}
baseSql :: Query
baseSql = "with totals as (\
            \select \
            \line_items.order_id as fk_order_id, \
            \coalesce(sum(line_items.price * line_items.quantity), 0) as total \
            \from line_items group by fk_order_id \
          \) \
          \select \
          \orders.id as id, \
          \orders.order_type as type, \
          \orders.po as po, \
          \orders.created as created, "
       <> baseCustomerQuery -- this doesn't contain comma at end, needed here
       <> ", \
          \orders.order_number as onum, \
          \totals.total as price, \
          \orders.is_wrapped as isWrapped, \
          \orders.is_billed as isBilled, \
          \orders.billed as billedAt, \
          \orders.invoice_number as invoiceNumber, \
          \m_project_orders.fk_project_id as projectId, \
          \m_project_orders.fk_section_id as sectionId, \
          \orders.is_progressive_bill as isProgressiveBill, \
          \orders.is_complete as isComplete, \
          \orders.complete as completeAt, \
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
          \orders.customer_markup as customerMarkupSaved \
          \from orders \
          \inner join customers \
          \on orders.fk_customer_id = customers.id \
          \inner join totals \
          \on orders.id = totals.fk_order_id \
          \left join labor_rate \
          \on orders.fk_labor_id = labor_rate.id \
          \left join markups \
          \on orders.fk_markups_id = markups.id"

-- | Join SQL for project mappings.
mainJoinSql :: Query
mainJoinSql = " left join m_project_orders \
              \on orders.id = m_project_orders.fk_order_id"

-- | Main insertion SQL for orders. Check database for default field values.
insertSql :: Query
insertSql = "insert into orders (\
            \order_type, \
            \po, \
            \fk_customer_id, \
            \order_number, \
            \is_wrapped, \
            \is_billed, \
            \billed, \
            \invoice_number, \
            \is_progressive_bill, \
            \is_complete, \
            \complete, \
            \fk_labor_id, \
            \fk_markups_id, \
            \customer_markup) \
            \values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
            \returning id"

-- | Search SQL for naive, case-insensitive search on provided fields.
searchSql :: Query
searchSql = " where \
            \orders.order_type like ? or \
            \orders.po like ? or \
            \orders.order_number like ? or \
            \customers.name like ?"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by orders.order_number desc"

-- | Pagination SQL.
paginationSql :: Query
paginationSql = " limit ? offset ?"

-- | Join SQL for connection to project by project id.
projectJoinSql :: Query
projectJoinSql = " inner join m_project_orders \
                 \on orders.id = m_project_orders.fk_order_id \
                 \where m_project_orders.fk_project_id = ?"

-- | Join SQL for connection to project section by section id.
sectionJoinSql :: Query
sectionJoinSql = " inner join m_project_orders \
                 \on orders.id = m_project_orders.fk_order_id \
                 \where m_project_orders.fk_section_id = ?"

-- | Filter SQL to ensure that FP orders are complete and not already billed for the current billing cycle.
filterSql :: Query
filterSql = " and orders.is_billed = false and orders.is_complete = true"

-- | Base update/edit SQL.
updateSql :: Query
updateSql = "update orders set \
            \order_type = ?, \
            \po = ?, \
            \fk_customer_id = ?, \
            \order_number = ? \
            \where orders.id = ?"

-- | Delete order SQL.
deleteOrderSql :: Query
deleteOrderSql = "delete from orders where orders.id = ?"

-- | SQL for to billed toggle.
setToBilledSql :: Query
setToBilledSql = "update orders set \
                 \is_billed = true, \
                 \billed = current_timestamp, \
                 \invoice_number = ? \
                 \where orders.id = ?"

-- | SQL for to not billed toggle.
setToNotBilledSql :: Query
setToNotBilledSql = "update orders set \
                    \is_billed = false, \
                    \billed = null, \
                    \invoice_number = null \
                    \where orders.id = ?"

-- | SQL for to complete toggle.
setToCompleteSql :: Query
setToCompleteSql = "update orders set \
                   \is_complete = true, \
                   \complete = current_timestamp \
                   \where orders.id = ?"

-- | SQL for to complete toggle with supplied timestamp.
setToCompleteTimeSql :: Query
setToCompleteTimeSql = "update orders set \
                       \is_complete = true, \
                       \complete = ? \
                       \where orders.id = ?"

-- | SQL for to not complete toggle.
setToNotCompleteSql :: Query
setToNotCompleteSql = "update orders set \
                      \is_complete = false, \
                      \complete = null \
                      \where orders.id = ?"

-- | SQL for date range searches on created datetime (when order was written into system). TODO
orderDateSearchSql :: Query
orderDateSearchSql = " where orders.created >= ? and orders.created <= ? \
                     \and orders.order_type <> ?"

-- | SQL for date range searches on created datetime for CC only (FC).
curbCoOrderDateSearchSql :: Query
curbCoOrderDateSearchSql = " where orders.created >= ? and orders.created <= ? \
                           \and orders.order_type = 'FC'"

-- | SQL for date range search and filtering in/out required order types. TODO
buildAssemblySearchSql :: Query
buildAssemblySearchSql = " where orders.created >= ? and orders.created <= ? \
                         \and (orders.order_type = ? or orders.order_type = ?)"

-- | SQL filter for getting all FS, FL orders that are not complete.
nonCompleteSql :: Query
nonCompleteSql = " where not orders.is_complete and (orders.order_type = 'FS' or orders.order_type = 'FL' or orders.order_type = 'FC' or orders.order_type = 'PGB')"

-- | SQL filter for getting all project orders that are not complete.
nonCompleteProjectSql :: Query
nonCompleteProjectSql = " where not orders.is_complete and orders.order_type = 'FP'"

nonBilledSql :: Query
nonBilledSql = " where orders.is_complete and not orders.is_billed and (orders.order_type = 'FS' or orders.order_type = 'FL' or orders.order_type = 'FC' or orders.order_type = 'PGB')"
