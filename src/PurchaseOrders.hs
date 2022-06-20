{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: PurchaseOrders
Description: Management of purchase orders and back orders.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides functions to create, edit, receive, and manage purchase orders. Currently only
materials and purchase items are purchased this way.

Materials are purchased through their material purchase options. Purchase items are purchased directly.
Inventory and item cost are managed upon receipt of the purchase order.
IMPORTANT: materials follow a weighted average costing model. Purchase items do NOT.
There is external documentation detailing the weighted material costing model.

CRITICAL: Workflow is a follows, with each previous number a forced gate for progress.
  1) Create PO
  2) Send to QB (for records/accounting)
  3) Send to customer (RFQ or o/w)
  4a) Parallel step with 4b - receive counts and/or create back order(s) as needed (inventory adjusted here on receipt)
  4b) Parallel step with 4a - perform price adjustments based on received invoices and shipping fees
  5) Merge pricing into database (here the price tweaks are done)
  6) Close PO

-}
module PurchaseOrders
  ( -- * Functions
    addPurchaseOrder
  , addPOItem
  , createBackOrder
  , deletePurchaseOrder
  , editPOItem
  , editPurchaseOrder
  , getPOReport
  , getPORfq
  , getPurchaseOrderById
  , getPurchaseOrderByNumber
  , listPurchaseOrderSkeletons
  , removePOItem
  , setPOAsPriceAdjusted
  , setPOAsPriceVerified
  , setPOAsQBDone
  , setPOAsSent
  , setPOAsReceived
  , setPOAsClosed
  )
  where


import Control.Monad ( forM )
import qualified Data.ByteString.Lazy as LBS
import Data.List ( foldl' )
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Data.Time.Clock ( getCurrentTime )
import Data.UUID.V4 ( nextRandom )
import Database.PostgreSQL.Simple ( Query )
import Database.PostgreSQL.Simple.ToField ( toField )
import Database.PostgreSQL.Simple.ToRow ( ToRow
                                        , toRow
                                        )


-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , APIMsg(..)
                , Context
                , Material(..)
                , MatPurchase(..)
                , PurchaseBackOrder(..)
                , PurchaseOrder(..)
                , POItem(..)
                , QueryPag(..)
                , Vendor(..)
                , materialInv
                )
import Database ( runExecute
                , runExecuteMany
                , runQuery
                )
import Mail ( emailPOMergeReady )
import Materials ( getMaterialById
                 , setMatCost
                 )
import MatPurchases ( getMatPurchaseById )
import PurchaseItems ( setPurchaseItemCost )
import Reports ( poHtml
               , rfqHtml
               )
import Utils ( apiMsg201
             , apiMsg204
             , badReq
             , eitherPassErr
             , flattenData
             , gen
             , passSuccess
             , wasFound
             )


-- TYPES --
-- | Helper for moving 'POItem' to the on order status (inventory). Tuple fields are quantity, reference id, and reference type.
newtype POItemToOrder = POItemToOrder (Double, Int, Text) deriving (Show)

instance ToRow POItemToOrder where
  toRow (POItemToOrder (q, ri, rt)) = [ toField q
                                      , toField ri
                                      , toField rt
                                      ]

-- | Helper for moving 'POItem' from the on order status to on hand (inventory).
-- Tuple fields are on hand, on hand, reference id, and reference type.
newtype POItemOnOrderToOnHand = POItemOnOrderToOnHand (Double, Double, Int, Text) deriving (Show)

instance ToRow POItemOnOrderToOnHand where
  toRow (POItemOnOrderToOnHand (q1, q2, ri, rt)) = [ toField q1
                                                   , toField q2
                                                   , toField ri
                                                   , toField rt
                                                   ]


-- FNS --
-- | Creates a new purchase order outline (sans po items).
addPurchaseOrder :: Context -> PurchaseOrder -> IO (Either APIError PurchaseOrder)
addPurchaseOrder ctx po = runQuery ctx insertSql po
  >>= eitherPassErr (gen (\npo npoid -> npo { po_id = npoid })
                         "Failure to return new purchase order id"
                         po
                    )

-- | Modifies the top-level purchase order information.
editPurchaseOrder :: Context -> Int -> PurchaseOrder -> IO (Either APIError PurchaseOrder)
editPurchaseOrder ctx poid po
  | po_id po == poid = runExecute ctx updateSql ( fromMaybe (po_id po) (po_idAdj  po)
                                                , po_number po
                                                , vdid $ po_vendor po
                                                , po_memo po
                                                , po_sentDate po
                                                , po_expectedReceiptDate po
                                                , po_isOrderInQB po
                                                , po_isOrderSent po
                                                , po_isOrderReceived po
                                                , po_isOrderClosed po
                                                , po_isBackOrder po
                                                , po_id po
                                                )
    >>= eitherPassErr (\_ -> passSuccess po)
  | otherwise = badReq 400 "Purchase order uri id and body id mismatch"

-- | Retrieves purchase order by id. Appends po items. Returns 404 if not found.
getPurchaseOrderById :: Context -> Int -> IO (Either APIError PurchaseOrder)
getPurchaseOrderById ctx poid = runQuery ctx (baseSql <> " and purchase_orders.id = ?") [poid]
  >>= eitherPassErr (wasFound "Purchase order not found")
  >>= eitherPassErr (appendItems ctx)

-- | Retrieves purchase order by number. Appends po items. Returns 404 if not found.
getPurchaseOrderByNumber :: Context -> Text -> IO (Either APIError PurchaseOrder)
getPurchaseOrderByNumber ctx poNum = runQuery ctx (baseSql <> " and purchase_orders.po_number = ?") [poNum]
  >>= eitherPassErr (wasFound "Purchase order not found")
  >>= eitherPassErr (appendItems ctx)

-- | Helper to lookup and append all 'POItem' for the given purchase order.
appendItems :: Context -> PurchaseOrder -> IO (Either APIError PurchaseOrder)
appendItems ctx po = runQuery ctx baseItemSql [po_id po]
  >>= eitherPassErr (\pois -> passSuccess $ po { po_items = pois })

{- |

List purchase orders without po items within bounds of querystring search, paginations, and open/closed flag.
Use this when accessing numerous purchase orders without immediate need for accessing their po item information.

-}
listPurchaseOrderSkeletons :: Context -> Bool -> QueryPag -> IO (Either APIError [PurchaseOrder])
listPurchaseOrderSkeletons ctx showClosed (QueryPag ("", l, o)) =
  runQuery ctx (baseSql <> notClosedSearchSql <> sortSql <> paginationSql) (showClosed, l, o)
listPurchaseOrderSkeletons ctx showClosed (QueryPag (s, l, o)) =
  runQuery ctx (baseSql <> notClosedSearchSql <> searchSql <> sortSql <> paginationSql) (showClosed, s, s, l, o)

-- | Delete purchase order. First checks for closed or received state - cannot delete these.
-- Else removes po items from on order, then deletes po items, then deletes the purchase order itself.
deletePurchaseOrder :: Context -> Int -> IO (Either APIError APIMsg)
deletePurchaseOrder ctx poid = getPurchaseOrderById ctx poid
  >>= eitherPassErr (\po ->
    if po_isOrderReceived po || po_isOrderClosed po || po_isPriceAdjusted po
       then badReq 400 "Cannot delete a purchase order that has been received"
       else itemsOffOrder ctx po)
  >>= eitherPassErr (\_ -> runExecute ctx delItemSql [poid])
  >>= eitherPassErr (\_ -> runExecute ctx delSql [poid])
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Purchase order and line items deleted")

{- |

Adds 'POItem' to purchase order. If PO is received or closed, returns 400 error.
If the item is NOT a material purchase option simply pass to next part.
If the item IS a material purchase option, retreive base material first.
Then add the item to the purchase order map.
Finally, pass on to checkAndSetOnOrder.

-}
addPOItem :: Context -> Int -> Bool -> POItem -> IO (Either APIError POItem)
addPOItem ctx poid isBackOrder poi
  | poid == poi_poId poi = getPurchaseOrderById ctx poid
    >>= eitherPassErr (\po ->
      if po_isOrderReceived po || po_isOrderClosed po
         then badReq 400 "Purchase order items cannot be added post receipt"
         else (if poi_refType poi /= "material purchase"
                 then passSuccess poi
                 else getBaseMaterialFromPurchaseOption ctx (poi_refId poi)
                   >>= eitherPassErr (\m -> passSuccess $ poi { poi_refId = mid m
                                                              , poi_refType = materialInv
                                                              }
                                     ))
           >>= eitherPassErr (\altpoi -> runQuery ctx insertItemSql altpoi
             >>= eitherPassErr (gen (\npoi nid -> npoi { poi_id = nid })
                                    "Failure to return new purchase order item id"
                                    altpoi
                               )
                             )
           >>= eitherPassErr (\npoi -> if isBackOrder then passSuccess npoi
                                                      else checkAndSetOnOrder ctx (po_isOrderSent po) npoi
                             )
    )
  | otherwise = badReq 400 "Purchase order uri id and body id mismatch"

-- | Helper to check that, if the PO is already sent, the newly added item must be moved to on order.
checkAndSetOnOrder :: Context -> Bool -> POItem -> IO (Either APIError POItem)
checkAndSetOnOrder _ False poi = passSuccess poi
checkAndSetOnOrder ctx _ poi = let fpois = filterOutGenerics [poi] in
  runExecuteMany ctx toOnOrderSql fpois (\npoi -> POItemToOrder ( getPOItemQuantity npoi
                                                                , poi_refId npoi
                                                                , poi_refType npoi
                                                                ))
  >>= eitherPassErr (\_ -> passSuccess poi)

-- | Removes po item. If po is received or closed it cannot be deleted. Takes the item off from on order where applicable.
removePOItem :: Context -> Int -> Int -> IO (Either APIError APIMsg)
removePOItem ctx poid poitid = getPurchaseOrderById ctx poid
  >>= eitherPassErr (\po ->
    if po_isOrderReceived po || po_isOrderClosed po
       then badReq 400 "Cannot delete a purchase order line item when the PO has been received"
       else runExecuteMany ctx toOffOrderSql (filter (\poi -> poi_id poi == poitid) $ po_items po)
         (\poi -> POItemToOrder ( getPOItemQuantity poi
                                , poi_refId poi
                                , poi_refType poi
                                ))
         >>= eitherPassErr (\_ -> runExecute ctx removePOItemSql (poid, poitid)))
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Purchase order item removed")

-- | Marks PO as sent to QB.
setPOAsQBDone :: Context -> Int -> IO (Either APIError APIMsg)
setPOAsQBDone ctx poid = runExecute ctx (qbDoneSql True) [poid]
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Purchase order marked as sent to QB")

-- | Marks PO as sent to vendor. Initiates setting all items to on order.
setPOAsSent :: Context -> Int -> IO (Either APIError APIMsg)
setPOAsSent ctx poid = getPurchaseOrderById ctx poid
  >>= eitherPassErr (itemsOnOrder ctx)
  >>= eitherPassErr (\_ -> runExecute ctx (sentSql True) [poid])
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Purchase order marked as sent and items added to on order in inventory")

-- | Marks the po items to on order and sets the sent datetime field.
itemsOnOrder :: Context -> PurchaseOrder -> IO (Either APIError ())
itemsOnOrder ctx po =
  let fpois = filterOutGenerics $ po_items po
      groupPois = groupItemsWithSameReference fpois
  in
    runExecuteMany ctx toOnOrderSql groupPois
      (\poi -> POItemToOrder ( getPOItemQuantity poi
                             , poi_refId poi
                             , poi_refType poi
                             ))
    >>= eitherPassErr (\_ -> passSuccess ())

-- | Takes item off from order.
itemsOffOrder :: Context -> PurchaseOrder -> IO (Either APIError ())
itemsOffOrder ctx po = let fpois = filterOutGenerics $ po_items po in
  runExecuteMany ctx toOffOrderSql fpois
    (\poi -> POItemToOrder ( getPOItemQuantity poi
                           , poi_refId poi
                           , poi_refType poi
                           ))
  >>= eitherPassErr (\_ -> passSuccess ())

-- | Sets PO as received (with timestamp) and moves quantities from on order to on hand. Does NOT perform price adjusts here.
setPOAsReceived :: Context -> Int -> IO (Either APIError APIMsg)
setPOAsReceived ctx poid = getPurchaseOrderById ctx poid
  >>= eitherPassErr (\po ->
    if and [ po_isOrderInQB po
           , po_isOrderSent po
           , not $ po_isOrderReceived po
           , not $ po_isPriceAdjusted po
           , not $ po_isOrderClosed po
           ]
      then itemsOnOrderToOnHand ctx po
        >>= eitherPassErr (\_ -> runExecute ctx (receivedSql True) [poid])
        >>= eitherPassErr (\_ -> if po_isPriceVerified po
                                then emailPOMergeReady (po_number po)
                                else passSuccess ())
      else badReq 400 "Purchase order is already past the receipt step. Invalid action."
  ) >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Purchase order marked as received and on order inventory moved to on hand.")

-- | Helper to move quantity from on order to same quantity on hand. Sets the receipt date here.
itemsOnOrderToOnHand :: Context -> PurchaseOrder -> IO (Either APIError PurchaseOrder)
itemsOnOrderToOnHand ctx po =
  let fpois = filterOutGenerics $ po_items po
      groupPois = groupItemsWithSameReference fpois
  in
    runExecuteMany ctx fromOnOrderToOnHandSql groupPois
      (\poi -> POItemOnOrderToOnHand ( getPOItemQuantity poi
      , getPOItemQuantity poi
      , poi_refId poi
      , poi_refType poi
      ))
    >>= eitherPassErr (\_ -> passSuccess po)

-- | Sets flag and datetime for price verification, part of parallel step to ensure accuracy of po numbers prior to price adjustments.
setPOAsPriceVerified :: Context -> Int -> IO (Either APIError APIMsg)
setPOAsPriceVerified ctx poid = getPurchaseOrderById ctx poid
  >>= eitherPassErr (\po ->
    if and [ po_isOrderInQB po
           , po_isOrderSent po
           , not $ po_isPriceAdjusted po
           , not $ po_isOrderClosed po
           ]
      then runExecute ctx (priceVerifiedSql True) [poid]
        >>= eitherPassErr (\_ -> if po_isOrderReceived po
                                    then emailPOMergeReady (po_number po)
                                    else passSuccess ())
      else badReq 400 "Purchase order is already past or not ready for the price verification step. Invalid action."
  )
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Purchase order prices verified.")

-- | Sets flag and datetime for price adjustments. Also performs actual adjustments of prices in database here.
setPOAsPriceAdjusted :: Context -> Int -> IO (Either APIError APIMsg)
setPOAsPriceAdjusted ctx poid = getPurchaseOrderById ctx poid
  >>= eitherPassErr (\po ->
    if and [ po_isOrderInQB po
           , po_isOrderSent po
           , po_isOrderReceived po
           , po_isPriceVerified po
           , not $ po_isPriceAdjusted po
           , not $ po_isOrderClosed po
           ]
      then setNewCosts ctx po
      else badReq 400 "Purchase order is already past or not ready for the price adjustment step. Invalid action."
  ) >>= eitherPassErr (\_ -> runExecute ctx (priceAdjustedSql True) [poid])
    >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Purchase order priced adjusted and merged with database. Ready to close.")

-- | Helper to set the new base costs for the purchase order items.
setNewCosts :: Context -> PurchaseOrder -> IO (Either APIError PurchaseOrder)
setNewCosts ctx po = forM (filterOutGenerics $ po_items po) (setPurchasePrice ctx)
  >>= (pure . flattenData)
  >>= eitherPassErr (\_ -> passSuccess po)

-- | Helper to pass on to correct sub function.
setPurchasePrice :: Context -> POItem -> IO (Either APIError ())
setPurchasePrice ctx poi
  | poi_refType poi == "material" = setMatCost ctx poi
  | poi_refType poi == "purchase" = setPurchaseItemCost ctx poi
  | otherwise = passSuccess ()

-- | Marks the PO as closed, hiding it from the initial main view.
setPOAsClosed :: Context -> Int -> IO (Either APIError APIMsg)
setPOAsClosed ctx poid = getPurchaseOrderById ctx poid
  >>= eitherPassErr (\po ->
    if and [ po_isOrderInQB po
           , po_isOrderSent po
           , po_isOrderReceived po
           , po_isPriceVerified po
           , po_isPriceAdjusted po
           , not $ po_isOrderClosed po
           ]
      then runExecute ctx (poClosedSql True) [poid]
      else badReq 400 "Purchase order not ready to be closed. Invalid action."
  ) >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Purchase order marked as closed.")

-- | Retrieves the purchase order items actual quantity (taking into account units per quantity).
getPOItemQuantity :: POItem -> Double
getPOItemQuantity poi = poi_quantity poi * fromMaybe 1.0 (poi_unitsPerQuantity poi)

-- | Filter out items that are deemed generic.
filterOutGenerics :: [POItem] -> [POItem]
filterOutGenerics = filter (\poi -> poi_name poi /= "Generic Material" && poi_name poi /= "Generic Purchase")

-- | Retrieves the base material from the material purchase option.
getBaseMaterialFromPurchaseOption :: Context -> Int -> IO (Either APIError Material)
getBaseMaterialFromPurchaseOption ctx mpoid = getMatPurchaseById ctx mpoid
  >>= eitherPassErr (getMaterialById ctx . mp_fkMaterialId)

-- | Edits the purchase order item (if not yet received/closed). Certain fields cannot be edited at this time.
editPOItem :: Context -> Int -> Int -> POItem -> IO (Either APIError POItem)
editPOItem ctx poid poiid poi
  | poiid /= poi_id poi = badReq 400 "Purchase order item uri id and id mismatch"
  | otherwise = getPurchaseOrderById ctx poid
    >>= eitherPassErr (\po ->
      if po_isOrderClosed po || po_isPriceAdjusted po
         then badReq 400 "Cannot modify the purchase order after database price adjustments or closure."
         else getPOItemById ctx poid poiid
           >>= eitherPassErr (\oldPoi ->
             runExecute ctx updateItemSql ( poi_quantity poi
                                          , poi_unitsPerQuantity poi
                                          , poi_purchasePriceEach poi
                                          , poi_leadTime poi
                                          , poi_note poi
                                          , poi_desc poi
                                          , poi_id poi
                                          )
             >>= eitherPassErr (\_ -> if po_isOrderReceived po
                                         -- this option is for modifying the received quantity (already in inventory)
                                         then modifyPOItemInventoryPostReceipt ctx oldPoi poi
                                         -- this option is for modifying orders that have not been received yet
                                         else if po_isOrderSent po
                                                 then modifyPOItemInventory ctx oldPoi poi
                                                 else passSuccess poi)
           )
    )
    >>= eitherPassErr (\_ -> passSuccess poi)

-- | Retrieve purchase order item by id. Returns 404 if not found.
getPOItemById :: Context -> Int -> Int -> IO (Either APIError POItem)
getPOItemById ctx poid poiid = runQuery ctx (baseItemSql <> " and m_items_purchase_order.id = ?") (poid, poiid)
  >>= eitherPassErr (wasFound "PO item not found")

-- | Edits the purchase order item inventory field.
modifyPOItemInventory :: Context -> POItem -> POItem -> IO (Either APIError POItem)
modifyPOItemInventory ctx oldPoi newPoi =
  runExecute ctx modifyOnOrderSql (getQuantDiff oldPoi newPoi, poi_refId oldPoi, poi_refType oldPoi)
    >>= eitherPassErr (\_ -> passSuccess newPoi)

-- | Edits the purchase order item inventory field post receipt.
modifyPOItemInventoryPostReceipt :: Context -> POItem -> POItem -> IO (Either APIError POItem)
modifyPOItemInventoryPostReceipt ctx oldPoi newPoi =
  runExecute ctx modifyOnHandSql (getQuantDiff oldPoi newPoi, poi_refId oldPoi, poi_refType oldPoi)
    >>= eitherPassErr (\_ -> passSuccess newPoi)

-- | Generates HTML report for the purchase order.
getPOReport :: Context -> Int -> IO (Either APIError LBS.ByteString)
getPOReport ctx poid = do
  ct <- getCurrentTime
  rand <- nextRandom
  getPurchaseOrderById ctx poid
    >>= eitherPassErr (poHtml ct rand)

-- | Generates an RFQ report for the purchase order.
getPORfq :: Context -> Int -> IO (Either APIError LBS.ByteString)
getPORfq ctx poid = do
  ct <- getCurrentTime
  rand <- nextRandom
  getPurchaseOrderById ctx poid
    >>= eitherPassErr (rfqHtml ct rand)

{- |

Generates a back order using the denoted quantities and items to put ON back order.
Note that this requires via the UI/UX to demark what items are NOT received, rather than marking what
HAS been received. If the PO is received or closed, cannot generate a back order.

This function handles the process of generating a brand new purchase order for the back order.
It adds the quantities and item(s) on back order to the new back order. It removes item from the original PO
if they are on back order in their entirety. Else, it adjusts the quantity correspondingly
so that the original amount is split between the original PO and the back order.

IMPORTANT: There has been an intermittent bug when creating back orders from back orders. Keep an eye on the results!
Any attempts to reproduce have failed at this time.

-}
createBackOrder :: Context -> Int -> [PurchaseBackOrder] -> IO (Either APIError APIMsg)
createBackOrder ctx poid pbos = getPurchaseOrderById ctx poid
  >>= eitherPassErr (\po ->
    if po_isOrderReceived po || po_isOrderClosed po || po_isPriceAdjusted po
       then badReq 400 "Cannot create back order from a received or closed order."
       else
         if not (all (\pbo -> any (\poi -> pbo_id pbo == poi_id poi) (po_items po)) pbos)
            then badReq 400 "List of purchase items to place on back order are not all present in original PO"
            else addPurchaseOrder ctx ( po { po_number = po_number po <> " BO"
                                      , po_items = []
                                      , po_isBackOrder = True
                                      }
                                      )
              >>= eitherPassErr (\newPOShell ->
                let (oldAdjPois, newAdjPois) = genPOILists (po_id newPOShell) pbos (po_items po) in
                  removeAllPOItems ctx (po_id po)
                  >>= eitherPassErr (\_ -> addPOItems ctx po { po_items = oldAdjPois })
                  >>= eitherPassErr (\_ -> addPOItems ctx newPOShell { po_items = newAdjPois })
              )
  ) >>= eitherPassErr (\_ -> passSuccess $ apiMsg201 "Back order created - verify correctness.")

-- | Helper for back orders. Generates the lists of old and new to adjust the two purchase orders.
genPOILists :: Int -> [PurchaseBackOrder] -> [POItem] -> ([POItem], [POItem])
genPOILists newPOId pbos = foldl' (\(oldAdjPois, newAdjPois) poi ->
  let pbomatches = filter (\pbocheck -> poi_id poi == pbo_id pbocheck) pbos in
    case length pbomatches of
         -- no matches on back order, keep old poi on "old" list
         0 -> (poi : oldAdjPois, newAdjPois)
         _ -> let pbo = head pbomatches in
                if abs (poi_quantity poi - pbo_qty pbo) < 1.0
                   -- if diff very small, assume exact (doubles), so only add to back order list
                   then (oldAdjPois, poi { poi_poId = newPOId } : newAdjPois)
                   else ( poi { poi_quantity = poi_quantity poi - pbo_qty pbo } : oldAdjPois
                        , poi { poi_quantity = pbo_qty pbo
                              , poi_poId = newPOId
                              } : newAdjPois
                        )
  ) ([], [])

-- | Removes all purchase order items in preparation for reentering the adjusted lists (for back orders only).
removeAllPOItems :: Context -> Int -> IO (Either APIError ())
removeAllPOItems ctx poid = runExecute ctx removePOItemsSql [poid]
  >>= eitherPassErr (\_ -> passSuccess ())

-- | Back orders only. Adds in all po items from the lists.
addPOItems :: Context -> PurchaseOrder -> IO (Either APIError PurchaseOrder)
addPOItems ctx po =
  forM (po_items po) (addPOItem ctx (po_id po) True)
  >>= (pure . flattenData)
  >>= eitherPassErr (\_ -> passSuccess po)

-- | Helper to avoid issues with material purchase options that point to same base
-- material clobbering each other during inventory adjustments.
groupItemsWithSameReference :: [POItem] -> [POItem]
groupItemsWithSameReference = map snd . Map.toList .
  foldl' (\accMap poi ->
    let refKey = (poi_refId poi, poi_refType poi)
        quant = fromMaybe 1.0 (poi_unitsPerQuantity poi) * poi_quantity poi
    in
      if Map.member refKey accMap
         then Map.adjust (addPOI (poi_quantity $ accMap Map.! refKey) quant) refKey accMap
         else Map.insert refKey (setPOI quant poi) accMap
  ) (Map.empty :: Map.Map (Int, Text) POItem)
  where setPOI q poi = poi { poi_quantity = q
                           , poi_unitsPerQuantity = Just 1.0
                           }
        addPOI prevQ q poi = poi { poi_quantity = prevQ + q }

-- SQL --
-- | Base purchase order query SQL. Attaches vendor information.
baseSql :: Query
baseSql = "select \
          \purchase_orders.id as id, \
          \purchase_orders.po_number as poNumber, \
          \vendors.id as vendorId, \
          \vendors.name as vendorName, \
          \vendors.company as company, \
          \purchase_orders.memo as memo, \
          \purchase_orders.expected_receipt_date as expectedReceiptDate, \
          \purchase_orders.is_order_in_qb as isOrderInQB, \
          \purchase_orders.in_qb_dt as inQBDate, \
          \purchase_orders.is_order_sent as isOrderSent, \
          \purchase_orders.sent_dt as sentDate, \
          \purchase_orders.is_order_received as isOrderReceived, \
          \purchase_orders.receipt_dt as receiptDate, \
          \purchase_orders.is_price_verified as isPriceVerified, \
          \purchase_orders.price_verify_dt as priceVerifyDate, \
          \purchase_orders.is_price_adjusted as isPriceAdjusted, \
          \purchase_orders.price_adjust_dt as priceAdjustDate, \
          \purchase_orders.is_order_closed as isOrderClosed, \
          \purchase_orders.close_dt as closeDate, \
          \purchase_orders.is_back_order as isBackOrder \
          \from purchase_orders \
          \inner join vendors \
          \on purchase_orders.fk_vendor_id = vendors.id"

-- | Main insertion SQL. Check database for default values.
insertSql :: Query
insertSql = "insert into purchase_orders (\
              \po_number, \
              \fk_vendor_id, \
              \memo, \
              \expected_receipt_date, \
              \is_order_in_qb, \
              \is_order_sent, \
              \is_order_received, \
              \is_order_closed, \
              \is_back_order \
            \) values (?, ?, ?, ?, ?, ?, ?, ?, ?) \
            \returning id"

-- | Filter for non-closed purchase orders SQL. Default on UI/UX.
notClosedSearchSql :: Query
notClosedSearchSql = " where purchase_orders.is_order_closed = ?"

-- | Search SQL for naive, case-insensitive search on provided fields.
searchSql :: Query
searchSql = " and (\
            \vendors.name = ? or \
            \purchase_orders.po_number like ?)"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by purchase_orders.po_number desc"

-- | Pagination SQL.
paginationSql :: Query
paginationSql = " limit ? offset ?"

-- | Edit/update SQL.
updateSql :: Query
updateSql = "update purchase_orders set \
            \id = ?, \
            \po_number = ?, \
            \fk_vendor_id = ?, \
            \memo = ?, \
            \sent_dt = ?, \
            \expected_receipt_date = ?, \
            \is_order_in_qb = ?, \
            \is_order_sent = ?, \
            \is_order_received = ?, \
            \is_order_closed = ?, \
            \is_back_order = ? \
            \where purchase_orders.id = ?"

-- | Delete purchase order SQL.
delSql :: Query
delSql = "delete from purchase_orders where purchase_orders.id = ?"

-- | Insertion SQL for purchase order items.
insertItemSql :: Query
insertItemSql = "insert into m_items_purchase_order (\
                \fk_purchase_order_id, \
                \description, \
                \reference_id, \
                \reference_type, \
                \balance_category, \
                \quantity, \
                \units_per_quantity, \
                \purchase_price_each, \
                \note, \
                \lead_time \
                \) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
                \returning m_items_purchase_order.id"

-- | Base SQL query for purchase order items. Note the case for retrieving the correct information based on reference type.
baseItemSql :: Query
baseItemSql = "select \
              \m_items_purchase_order.id as id, \
              \m_items_purchase_order.fk_purchase_order_id as poId, \
              \(case when m_items_purchase_order.reference_type = 'material' then materials.name \
                \when m_items_purchase_order.reference_type = 'purchase' then purchase_items.name \
                \when m_items_purchase_order.reference_type = 'material purchase' then mat_purchases.name \
                \else 'TBD' \
              \end) as name, \
              \m_items_purchase_order.description as description, \
              \m_items_purchase_order.reference_id as refId, \
              \m_items_purchase_order.reference_type as refType, \
              \m_items_purchase_order.balance_category as balanceCategory, \
              \m_items_purchase_order.quantity as quantity, \
              \m_items_purchase_order.units_per_quantity as unitsPerQuantity, \
              \m_items_purchase_order.purchase_price_each as purchasePriceEach, \
              \m_items_purchase_order.note as note, \
              \m_items_purchase_order.lead_time as leadTime, \
              \(case when m_items_purchase_order.reference_type = 'purchase' then purchase_items.vendor_part_number else null end) \
              \as vendorPartNumber \
              \from m_items_purchase_order \
              \left join materials \
              \on m_items_purchase_order.reference_id = materials.id \
              \left join purchase_items \
              \on m_items_purchase_order.reference_id = purchase_items.id \
              \left join mat_purchases \
              \on m_items_purchase_order.reference_id = mat_purchases.id \
              \where m_items_purchase_order.fk_purchase_order_id = ?"

-- | Removes all purchase order items. For back orders only.
removePOItemsSql :: Query
removePOItemsSql = "delete from m_items_purchase_order \
                   \where m_items_purchase_order.fk_purchase_order_id = ?"

-- | Removes single po item SQL.
removePOItemSql :: Query
removePOItemSql = removePOItemsSql
               <> " and m_items_purchase_order.id = ?"

-- | Deletes all purchase order items. TODO Duplicate?
delItemSql :: Query
delItemSql = "delete from m_items_purchase_order where m_items_purchase_order.fk_purchase_order_id = ?"

-- | SQL to toggle is in QB flag nad set in qb date.
qbDoneSql :: Bool -> Query
qbDoneSql = markFlag "is_order_in_qb" "in_qb_dt"

-- | SQL to toggle is purchase order sent flag and set sent date.
sentSql :: Bool -> Query
sentSql = markFlag "is_order_sent" "sent_dt"

-- | SQL to toggle is purchase order received flag and set receipt date.
receivedSql :: Bool -> Query
receivedSql = markFlag "is_order_received" "receipt_dt"

-- | SQL to toggle is purchase order price(s) verified and date.
priceVerifiedSql :: Bool -> Query
priceVerifiedSql = markFlag "is_price_verified" "price_verify_dt"

-- | SQL to toggle is purchase order price adjusted in db and date.
priceAdjustedSql :: Bool -> Query
priceAdjustedSql = markFlag "is_price_adjusted" "price_adjust_dt"

-- | SQL to toggle is purchase order closed flag and set closed date.
poClosedSql :: Bool -> Query
poClosedSql = markFlag "is_order_closed" "close_dt"

-- | Higher-order fn to handle toggling flags.
markFlag :: Query -> Query -> Bool -> Query
markFlag field dateField b = let flag = if b then "true" else "false" in
  "update purchase_orders set "
  <> field
  <> " = "
  <> flag
  <> ", "
  <> dateField
  <> " = current_timestamp "
  <> " where purchase_orders.id = ?"

-- | SQL for updating inventory to on order.
toOnOrderSql :: Query
toOnOrderSql = "update inventory set \
               \on_order = inventory.on_order + upd.on_order \
               \from (values (?, ?, ?)) as upd(on_order, reference_id, reference_type) \
               \where inventory.reference_id = upd.reference_id \
               \and inventory.reference_type = upd.reference_type"

-- | SQL for updating inventory to on order.
toOffOrderSql :: Query
toOffOrderSql = "update inventory set \
                \on_order = greatest(inventory.on_order - upd.on_order, 0) \
                \from (values (?, ?, ?)) as upd(on_order, reference_id, reference_type) \
                \where inventory.reference_id = upd.reference_id \
                \and inventory.reference_type = upd.reference_type"

-- | SQL for updating inventory from on order to on hand.
fromOnOrderToOnHandSql :: Query
fromOnOrderToOnHandSql = "update inventory set \
                         \on_order = greatest(0, abs(inventory.on_order - upd.on_order)), \
                         \on_hand = inventory.on_hand + upd.on_hand \
                         \from (values (?, ?, ?, ?)) as upd(on_order, on_hand, reference_id, reference_type) \
                         \where inventory.reference_id = upd.reference_id \
                         \and inventory.reference_type = upd.reference_type"

-- | SQL to update purchase order information.
updateItemSql :: Query
updateItemSql = "update m_items_purchase_order set \
                \quantity = ?, \
                \units_per_quantity = ?, \
                \purchase_price_each = ?, \
                \lead_time = ?, \
                \note = ?, \
                \description = ? \
                \where m_items_purchase_order.id = ?"

-- | SQL to modify the number on order.
modifyOnOrderSql :: Query
modifyOnOrderSql = "update inventory set \
                   \on_order = inventory.on_order + ? \
                   \where inventory.reference_id = ? \
                   \and inventory.reference_type = ?"

-- | SQL to modify the inventory number directly (adjustments to quantity made post-receipt but prior to price adjusts/closure).
modifyOnHandSql :: Query
modifyOnHandSql = "update inventory set \
                  \on_hand = inventory.on_hand + ? \
                  \where inventory.reference_id = ? \
                  \and inventory.reference_type = ?"

-- | Calculates the difference in quantity between the old and new purchase order item.
getQuantDiff :: POItem -> POItem -> Double
getQuantDiff oldPoi newPoi =
  let upqOld = fromMaybe 1.0 (poi_unitsPerQuantity oldPoi)
      upqNew = fromMaybe 1.0 (poi_unitsPerQuantity newPoi)
      qOld = poi_quantity oldPoi * upqOld
      qNew = poi_quantity newPoi * upqNew
  in
    qNew - qOld
