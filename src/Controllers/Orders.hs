{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.Orders
Description: Controllers and handles for all order routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all order and order-related routes.

-}
module Controllers.Orders
  ( -- * Orders
    pOrder
  , pOrderViaCatalog
  , pStockOrder
  , pProgressiveOrder
  , eProgressiveOrderStatus
  , gOrders
  , gOrderSkeletons
  , gOrder
  , hFLOrder
  , eOrder
  , dOrder
  , pProjectOrder
  , eProjectOrder
  , eOrderToBilled
  , eOrderToBilledByNumber
  , eOrderToNotBilled
  , eOrderToComplete
  , eOrderToNotComplete
  , sProjectOrderSection
  , gOrderLineItemDetailReport
  , gOrderPackingSlip
  , gDetailedSalesData
  , gMembrainOrderData
  , gBuildAssemblyReport
    -- * Line Item
  , pExtraFlag
    -- * Order Status
  , gNotBilledNormalOrders
  , gNotBilledNormalStatus
  , gNotDoneNormalOrders
  , pNowDoneChecker
  )
  where

import Control.Monad ( (>=>) )
import qualified Data.ByteString as BS
import Data.Text ( Text
                 , toUpper
                 )
import Network.Wai ( Request
                   , Response
                   , queryString
                   )
import Network.HTTP.Types ( status200
                          , status201
                          , status204
                          )

-- LOCAL --
import APITypes ( APIError(..)
                , APIMsg
                , Context(..)
                , JobExt(..)
                , LineItem(..)
                , MatInfo
                , Order(..)
                , PricingList(..)
                , Project(..)
                , ProjectSection(..)
                , RawLineItem
                )
import Assemblies ( handleAssemblyOrderBilled
                  , sanitizeBDOrder
                  , updateFabricatedQuantity
                  )
import Controllers.Utils ( EIntParam
                         , defOrderFlags
                         , defStockOrderFlags
                         , errJsonRes
                         , errReqBody
                         , errUriParam
                         , getPagQS
                         , lookupQS
                         , msgJsonRes
                         , reqBodyReader
                         , sendCsvResp
                         , sendHtmlGenResp
                         , sendResp
                         )
import CC ( removeCCMapping )
import LineItems ( setExtraFlag )
import Orders ( addOrder
              , deleteOrder
              , editOrder
              , getBuildAssemblyData
              , getDetailedSalesData
              , getOrderById
              , getOrderByNumber
              , getOrderDetailedSalesData
              , getOrderPackingSlip
              , getOrderSalesData
              , getNotBilledOrderStatus
              , listNotBilledOrders
              , listNotDoneOrders
              , listOrders
              , listOrderSkeletons
              , orderToRawLineItems
              , runNowDoneCheck
              , setOrderToBilled
              , setOrderToNotBilled
              , setOrderToComplete
              , setOrderToCompleteWithTime
              , setOrderToNotComplete
              , verifyStockOnly
              )
import Pricing ( adjustInventory
               , getMarkupFlag
               , priceViaList
               , runEstimator
               , sanitizeRawLineItems
               , undoInventoryAdjustments
               )
import PricingLists ( getBDList )
import Projects ( assignOrderToProject
                , getProjectByAppId
                , getProjectById
                , removeOrderProjectMapping
                , swapProjectOrderSection
                )
import Quotes ( removeQuoteOrderId )
import Utils ( apiError400
             , apiError404
             , apiMsg200
             , apiMsg201
             , apiMsg204
             , badReq
             , eitherPassErr
             , fst5
             , isValidOrderType
             , passSuccess
             )

{- | POST Creates new order.

Validates order type (FS, FL, FP). Runs through pOrderSteps. Returns 201 'APITypes.RawLineItem' list on success.
The order of the steps is critical.

-}
pOrder :: Text -> Context -> IO Response
pOrder ordType ctx@(Context (req, _))
  | isValidOrderType ordType = reqBodyReader req
    >>= either errReqBody (pOrderSteps ctx (toUpper ordType) defOrderFlags Nothing)
  | otherwise = errJsonRes (apiError400 "Invalid order type received. Must be one of FS or FP")

{- | POST Creates new order while pricing via a fixed pricing list.

Validates order type (FS, FL, FP). Runs first through 'priceViaList', then through pOrderSteps.
Returns 201 'APITypes.RawLineItem' list on success.
The order of steps is critical.

-}
pOrderViaCatalog :: Text -> EIntParam -> Context -> IO Response
pOrderViaCatalog _ (Left _) _ = errUriParam "pricing list"
pOrderViaCatalog ordType (Right catid) ctx@(Context (req, _))
  | isValidOrderType ordType = reqBodyReader req
    >>= either errReqBody (priceViaList ctx catid
      >=> either errJsonRes (pOrderSteps ctx (toUpper ordType) defOrderFlags Nothing))
  | otherwise = errJsonRes (apiError400 "Invalid order type received. Must be one of FS or FP")

{- | POST Creates new stock order (adding to stock inventory).

Checks that the line items are all stock items first, then runs through pOrderSteps with the special SP stock order flag.
Returns 201 'APITypes.RawLineItem' on success.

-}
pStockOrder :: Context -> IO Response
pStockOrder c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (verifyStockOnly >=> either errJsonRes (pOrderSteps c "SP" defStockOrderFlags (Just "Stock")))

{- | POST Continuation helper for posting new orders.

First finishes pricing the line items. Passes (on pricing success) to 'writeOrder'.

-}
pOrderSteps :: Context
            -> Text
            -> (Bool, Bool, Bool)
            -> Maybe Text
            -> [RawLineItem]
            -> IO Response
pOrderSteps ctx ordType flags mInvoiceNumber cds = do
  epricedRlis <- runEstimator ctx ordType cds
  case epricedRlis of
       (Left e) -> errJsonRes e
       (Right pricedRlis) -> writeOrder ctx ordType flags mInvoiceNumber pricedRlis >>= sendResp status201

{- | POST continuation helper for posting new orders part 2.

First cleans up the raw, priced line items into the internal line item storage structure.
Then adds the order with the order flags and isWrapped flag. If the order is complete, the necessary inventory adjustments are made.
Finally, the 'APITypes.RawLineItem' list is returned.

-}
writeOrder :: Context
           -> Text
           -> (Bool, Bool, Bool)
           -> Maybe Text
           -> [(RawLineItem, Maybe MatInfo, Maybe MatInfo, Maybe MatInfo, Maybe Double)]
           -> IO (Either APIError [RawLineItem])
writeOrder ctx ordType (b1, b2, b3) mInvoiceNumber pricedRlis =
  pure (sanitizeRawLineItems b1 pricedRlis)
  >>= (\lisPlus ->
        -- isProgressive, isBilled, isComplete => b1, b2, b3
        addOrder ctx b1 b2 b3 ordType mInvoiceNumber (getMarkupFlag $ map fst5 pricedRlis) lisPlus
        >>= eitherPassErr (\_ -> if b3
                                    then adjustInventory ctx ordType lisPlus
                                    else passSuccess ())
        >>= eitherPassErr (\_ -> pure $ Right $ map fst5 pricedRlis)
      )

{- | POST Adds new Big BD order.

Retrieves the BD fixed pricing list. Leverages the 'priceViaList' function to finish pricing.
Then passes the priced line items to pOrderSteps with the proper order type and flags to
ensure that the order is not complete or billed, but ready to build the assemblies.

-}
pProgressiveOrder :: Context -> IO Response
pProgressiveOrder ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\cds ->
    getBDList ctx
    >>= either errJsonRes (\pl ->
      priceViaList ctx (pl_id pl) cds
        >>= either errJsonRes (pOrderSteps ctx "PGB" (True, False, False) Nothing)
    )
  )

{- | POST Edit a Big BD order status.

Passes to 'updateFabricatedQuantity' to add or subtract quantity of one to the passed in item.
Returns 201 on success.

-}
eProgressiveOrderStatus :: EIntParam -> Bool -> Context -> IO Response
eProgressiveOrderStatus (Left _) _ _ = errUriParam "order"
eProgressiveOrderStatus (Right ordid) isAdd ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (updateFabricatedQuantity ctx ordid isAdd >=> sendResp status201)

-- | GET List orders. Returns 200 'APITypes.Order' list on success.
gOrders :: Context -> IO Response
gOrders c = listOrders c >>= sendResp status200

-- | GET List orders sans line items based on querystring. Returns 200 'APITypes.Order' list on success.
gOrderSkeletons :: Context -> IO Response
gOrderSkeletons ctx@(Context (req, _)) =
  let pagQS = getPagQS req in
    listOrderSkeletons ctx pagQS >>= sendResp status200

-- | GET Retrieves order by id with line items by id. Returns 200 'APITypes.Order' on success.
gOrder :: EIntParam -> Context -> IO Response
gOrder (Left _) _ = errUriParam "order"
gOrder (Right ordId) c = getOrderById c ordId >>= sendResp status200

-- | GET Retrieves a quoted list (QL/FL) order and returns 200 'APITypes.RawLineItem' list on success.
hFLOrder :: Text -> Context -> IO Response
hFLOrder on ctx = getOrderByNumber ctx on
  >>= eitherPassErr (sanitizeBDOrder ctx)
  >>= eitherPassErr (pure . Right . orderToRawLineItems)
  >>= sendResp status200

-- | PUT Edits the given order, including possibly mapping to/removing mapping to project.
-- Returns 200 on success.
eOrder :: EIntParam -> Context -> IO Response
eOrder (Left _) _ = errUriParam "order"
eOrder (Right ordId) c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\o -> editOrder c o ordId
    >> if otype o /= "FP"
          then removeOrderProjectMapping c ordId >> sendResp status200 (Right o)
          else sendResp status200 (Right o))

{- | DELETE Delets the given order if possible.

Checks if the order type is PGB (BD). If billed, fails. If any assemblies are built, they must be unbuilt prior to deletion.
Undoes the inventory adjustments for the order, removes any project mapping, deletes the order itself, removes
any quote mapping, and passes the 'APITypes.APIMsg' success message. Returns 204 on success.

Note that orders must be deleted before a mapped quote if that linkage exists.

-}
dOrder :: EIntParam -> Context -> IO Response
dOrder (Left _) _ = errUriParam "order"
dOrder (Right ordId) ctx = getOrderById ctx ordId
  >>= eitherPassErr (\o -> if otype o == "PGB" && oisBilled o
                              then badReq 400 "A billed BD order cannot be deleted."
                              else passSuccess o)
  >>= eitherPassErr (\o -> if any ((> 0) . lquantFabbed) (olineItems o)
                              then badReq 400 "All fabricated assemblies must be unmade prior to order deletion"
                              else passSuccess o)
  -- ONLY REMOVE INVENTORY IF ORDER IS COMPLETE
  >>= eitherPassErr (\o -> if oisComplete o
                              then undoInventoryAdjustments ctx (otype o) (olineItems o)
                              else passSuccess ()
                    )
  >>= eitherPassErr (\_ -> removeOrderProjectMapping ctx ordId)
  >>= eitherPassErr (\_ -> removeCCMapping ctx ordId)
  >>= eitherPassErr (\_ -> deleteOrder ctx ordId)
  >>= eitherPassErr (\_ -> removeQuoteOrderId ctx ordId)
  >>= eitherPassErr (\_ -> pure $ Right $ apiMsg204 "Order deleted")
  >>= sendResp status204

{- | POST Adds new order to project and section.

Runs through the same process as a normal order, but defaults to not complete and not billed status flags.
Also verifies that the project exists and the specified section exists.

-}
pProjectOrder :: EIntParam -> EIntParam -> Context -> IO Response
pProjectOrder (Left _) _ _ = errUriParam "project app"
pProjectOrder _ (Left _) _ = errUriParam "project section"
pProjectOrder (Right appId) (Right sid) ctx@(Context (req, _)) =
  reqBodyReader req
  >>= either errReqBody (\cds -> do
    epricedRlis <- runEstimator ctx "FP" cds
    eproject <- getProjectByAppId ctx appId
    case (epricedRlis, eproject) of
         (Left e, _) -> errJsonRes e
         (_, Left e) -> errJsonRes e
         (Right pricedRlis, Right project) ->
           if null $ prjsections project
              then errJsonRes (apiError400 "Selected project has no sections. Must have at least one.")
              else pure (sanitizeRawLineItems False pricedRlis) >>=
                (\lisPlus ->
                  addOrder ctx False False False "FP" Nothing (getMarkupFlag $ map fst5 pricedRlis) lisPlus
                  >>= eitherPassErr (assignOrderToProject ctx (prjid project) sid)
                  >>= eitherPassErr (\_ -> pure $ Right $ map fst5 pricedRlis)
                  >>= sendResp status201
                )
  )

-- | PUT Edits the project order, assigning the order to the specified section and project. Returns 200 on success.
eProjectOrder :: EIntParam -> EIntParam -> EIntParam -> Context -> IO Response
eProjectOrder (Left _) _ _ _ = errUriParam "project"
eProjectOrder _ (Left _) _ _ = errUriParam "project section"
eProjectOrder _ _ (Left _) _ = errUriParam "order"
eProjectOrder (Right pid) (Right sid) (Right ordId) c = assignOrderToProject c pid sid ordId
  >> msgJsonRes (apiMsg200 "Order assigned to project")

-- | PUT Sets the order to billed. Checks for PGB order type to handle differently. Returns 200 on success.
eOrderToBilled :: EIntParam -> Text -> Context -> IO Response
eOrderToBilled (Left _) _ _ = errUriParam "order"
eOrderToBilled (Right ordId) invoiceNumber c = getOrderById c ordId
  >>= eitherPassErr (handleOrderToBilled c invoiceNumber)
  >>= sendResp status200

-- | PUT Sets the order (by number) to billed. Checks for PGB order type to handle differently. Returns 200 on success.
eOrderToBilledByNumber :: Text -> Text -> Context -> IO Response
eOrderToBilledByNumber ordNum invoiceNumber c = getOrderByNumber c ordNum
  >>= eitherPassErr (handleOrderToBilled c invoiceNumber)
  >>= sendResp status200

-- | Helper to eliminate duplicate code for marking order to billed with invoice number.
handleOrderToBilled :: Context -> Text -> Order -> IO (Either APIError APIMsg)
handleOrderToBilled c invoiceNumber o
  | oisBilled o = pure $ Left $ apiError400 "Order is already set to billed"
  | otype o == "PGB" = handleAssemblyOrderBilled c o invoiceNumber
  | otherwise = setOrderToBilled c (oid o) invoiceNumber

-- | PUT Sets the order to not billed. Returns 200 on success.
eOrderToNotBilled :: EIntParam -> Context -> IO Response
eOrderToNotBilled (Left _) _ = errUriParam "order"
eOrderToNotBilled (Right ordId) c = setOrderToNotBilled c ordId >>= sendResp status200

-- | PUT Sets the order to complete. Inventory adjustements performed here. Returns 200 on success.
eOrderToComplete :: EIntParam -> Context -> IO Response
eOrderToComplete (Left _) _ = errUriParam "order"
eOrderToComplete (Right ordId) c = getOrderById c ordId
  >>= eitherPassErr (\o -> setOrderToComplete c ordId
    >>= eitherPassErr (\msg -> adjustInventory c (otype o) (olineItems o, "", "", "")
      >>= eitherPassErr (\_ -> passSuccess msg)
    )
  ) >>= sendResp status200

-- | PUT Sets the order to not complete. Removes inventory adjustments as necessary. Returns 200 on success.
eOrderToNotComplete :: EIntParam -> Context -> IO Response
eOrderToNotComplete (Left _) _ = errUriParam "order"
eOrderToNotComplete (Right ordId) c = getOrderById c ordId
  >>= eitherPassErr (\o -> setOrderToNotComplete c ordId
    >>= eitherPassErr (\msg -> undoInventoryAdjustments c (otype o) (olineItems o)
      >>= eitherPassErr (\_ -> passSuccess msg)
    )
  ) >>= sendResp status200

-- | PUT Swaps the order to a new section. Returns 200 on success.
sProjectOrderSection :: EIntParam -> EIntParam -> EIntParam -> Context -> IO Response
sProjectOrderSection (Left _) _ _ _ = errUriParam "project"
sProjectOrderSection _ (Left _) _ _ = errUriParam "order"
sProjectOrderSection _ _ (Left _) _ = errUriParam "section"
sProjectOrderSection (Right prjId) (Right ordId) (Right sid) c = getOrderById c ordId
  >>= eitherPassErr (\_ ->
    getProjectById c prjId
    >>= eitherPassErr (\p ->
      if not $ any (\s -> psid s == sid) (prjsections p)
         then pure $ Left $ apiError404 "Missing or invalid project section id"
         else swapProjectOrderSection c prjId ordId sid
     )
  )
  >>= sendResp status200

-- | GET Retrieves line item detailed sales data report (CSV) for the given order. Returns 200 and CSV on success.
gOrderLineItemDetailReport :: EIntParam -> Context -> IO Response
gOrderLineItemDetailReport (Left _) _ = errUriParam "order"
gOrderLineItemDetailReport (Right ordId) ctx = getOrderDetailedSalesData ctx ordId >>= sendCsvResp

-- | GET Retrieves simple packing slip template for the order (HTML). Returns 200 and HTML page on success.
gOrderPackingSlip :: EIntParam -> Context -> IO Response
gOrderPackingSlip (Left _) _ = errUriParam "order"
gOrderPackingSlip (Right ordId) ctx = getOrderPackingSlip ctx ordId >>= sendHtmlGenResp

-- | GET Retrieves simple sales data for Membrain import within date range. Returns 200 and CSV on success.
gMembrainOrderData :: Context -> IO Response
gMembrainOrderData ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (getOrderSalesData ctx >=> sendCsvResp)

-- | GET Retrieves detailed sales data within date range. Returns 200 and CSV on success.
gDetailedSalesData :: Context -> IO Response
gDetailedSalesData ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (getDetailedSalesData ctx >=> sendCsvResp)

-- | PUT Toggles the extra flag for the line item. Returns 204 on success.
pExtraFlag :: EIntParam -> Context -> IO Response
pExtraFlag (Left _) _ = errJsonRes $ apiError400 "Invalid line item id uri parameter provided"
pExtraFlag (Right liid) ctx@(Context (req, _)) =
  case getToggleFlag req of
       (Just "true") -> setExtraFlag ctx liid True >>= sendResp status204
       (Just "false") -> setExtraFlag ctx liid False >>= sendResp status204
       _ -> errJsonRes $ apiError400 "Invalid bool flag provided"

-- | Lookup for querystring field flag.
getToggleFlag :: Request -> Maybe BS.ByteString
getToggleFlag = lookupQS "flag" . queryString

-- | GET Retrieves build assembly report within date range. Returns 200 and CSV on success.
gBuildAssemblyReport :: Context -> IO Response
gBuildAssemblyReport ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (getBuildAssemblyData ctx >=> sendCsvResp)

-- | GET Checks all non-project, non-stock, non-BD orders that are not complete. Returns text list of number for said orders.
-- Returns 200 and list always unless errored out.
gNotDoneNormalOrders :: Context -> IO Response
gNotDoneNormalOrders = listNotDoneOrders >=> sendResp status200

-- | GET Checks all FS, FL orders that are complete but not billed. Returns text list of number for said orders.
-- REturns 200 and list alwasy unless errored out.
gNotBilledNormalOrders :: Context -> IO Response
gNotBilledNormalOrders = listNotBilledOrders >=> sendResp status200

-- | GET Checks all FS, FL orders that are complete but not billed. Returns list of 'APITypes.JobExt' for said orders.
gNotBilledNormalStatus :: Text -> Context -> IO Response
gNotBilledNormalStatus onumber ctx = getNotBilledOrderStatus ctx onumber >>= sendResp status200

-- | Actual external api call handle for checking order status and toggling as necessary. Return 201 'APITypes.APIMsg' on success.
pNowDoneChecker :: Text -> Context -> IO Response
pNowDoneChecker ordnum ctx = runNowDoneCheck ctx ordnum
  >>= eitherPassErr (\case
    Nothing -> passSuccess $ apiMsg201 "Order not yet done"
    Just jext -> getOrderByNumber ctx ordnum
      >>= eitherPassErr (\o -> setFnGen ctx jext o
        >>= eitherPassErr (\msg2 -> adjustInventory ctx (otype o) (olineItems o, "", "", "")
          >>= eitherPassErr (\_ -> passSuccess msg2)
        )
      )
  )
  >>= sendResp status201
  where setFnGen :: Context -> JobExt -> Order -> IO (Either APIError APIMsg)
        setFnGen c je o = case jobext_readyDate je of
                               Nothing -> setOrderToComplete c (oid o)
                               Just rd -> setOrderToCompleteWithTime c (oid o) rd
