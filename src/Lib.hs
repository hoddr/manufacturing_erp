{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Lib
Description: ERP entrypoint (main).
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Routing and entrypoint for the ERP server.

-}

module Lib ( main ) where

import Data.Text ( Text
                 , unpack
                 )
import Data.UUID.V4 ( nextRandom )
import Network.HTTP.Types ( Method )
import Network.Wai ( Request
                   , Response
                   , pathInfo
                   , requestMethod
                   )
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.Cors ( simpleCors )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )


-- LOCAL MODULES --
import APITypes ( Context(..)
                , UserPermissions(..)
                )
import Controllers.Index ( authMiddleware
                         , pcm -- permissions check middleware
                         , errJsonRes
                         , gAssemblies
                         , pAssembly
                         , eAssembly
                         , eAssemblyPartial
                         , dAssembly
                         , gAssemblyById
                         , dSubItem
                         , gCustomers
                         , pCustomer
                         , gCustomer
                         , eCustomer
                         , pCustomerTaxStatusOff
                         , pCustomerTaxStatusOn
                         , gOrders
                         , gOrderSkeletons
                         , gOrder
                         , hFLOrder
                         , pOrder
                         , pOrderViaCatalog
                         , pStockOrder
                         , pProgressiveOrder
                         , eProgressiveOrderStatus
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
                         , gDetailedSalesData
                         , gBuildAssemblyReport
                         , gBDFlangeCount
                         , gOrderLineItemDetailReport
                         , gOrderPackingSlip
                         , gMembrainOrderData
                         , gProjectEstimate
                         , gQuoteLineItemDetailReport
                         , gQuotes
                         , gQuoteSkeletons
                         , pQuote
                         , pQuoteCatalog
                         , pQuoteSans
                         , eQuote
                         , dQuote
                         , gQuote
                         , pQuoteToOrder
                         , pQuoteToProject
                         , gQuoteReport
                         , gVendors
                         , pVendor
                         , gVendor
                         , eVendor
                         , gProject
                         , gProjectSkeletons
                         , gProjects
                         , gProjectStatusReport
                         , gProjectAppId
                         , eProject
                         , dProject
                         , hProjectBilling
                         , eProjectOrdersAsBilled
                         , pProjectSection
                         , dProjectSection
                         , eProjectSection
                         , gProjectTracker
                         , gProjectSectionTracker
                         , gProjectOrderTracker
                         , gProjectExtras
                         , eProjectExtras
                         , dProjectExtra
                         , eProjectStatus
                         , gNotBilledNormalOrders
                         , gNotBilledNormalStatus
                         , gNotDoneNormalOrders
                         , gNotDoneProjectOrders
                         , pNowDoneChecker
                         , gProjectDetailedSalesData
                         , gInventory
                         , eInventory
                         , pExtraFlag
                         , gMaterials
                         , gMaterial
                         , eMaterial
                         , pMaterial
                         , gMaterialPurchases
                         , gMaterialPurchase
                         , eMaterialPurchase
                         , dMaterialPurchase
                         , pMaterialPurchase
                         , gMatPurchaseOptionsForMaterial
                         , gFabItems
                         , pFabItem
                         , eFabItem
                         , gPurchaseItems
                         , pPurchaseItem
                         , ePurchaseItem
                         , dPurchaseItem
                         , gPurchaseOrders
                         , gPurchaseOrderById
                         , gPurchaseOrderByNumber
                         , pPurchaseOrder
                         , ePurchaseOrder
                         , dPurchaseOrder
                         , ePurchaseOrderQBSet
                         , ePurchaseOrderAsSent
                         , ePurchaseOrderAsReceived
                         , ePurchaseOrderPricesVerified
                         , ePurchaseOrderPricesAdjusted
                         , ePurchaseOrderAsClosed
                         , pPurchaseOrderItem
                         , dPurchaseOrderItem
                         , ePurchaseOrderItem
                         , gPurchaseOrderRfq
                         , gPurchaseOrderReport
                         , pBackOrder
                         , gUsers
                         , pUser
                         , eUser
                         , dUser
                         , gUserPermissions
                         , eUserPermissions
                         , gMarkups
                         , pLaborRate
                         , pMargins
                         , loginCheck
                         , logoutCheck
                         , gPricingLists
                         , gPricingList
                         , gPricingListReport
                         , pPricingList
                         , dPricingList
                         , dPricingListItem
                         , ePricingListItems
                         , gPricingListCustomers
                         , pPricingListCustomer
                         , dPricingListCustomer
                         , pFeedbackComplaint
                         , dFeedbackComplaint
                         , gFeedbackComplaints
                         , eFeedbackComplaint
                         , gErrorLog
                         , pErrorLog
                         , dErrorLog
                         , eErrorLog
                         , dAddress
                         , eAddress
                         , gAddress
                         , gAddresses
                         , gCustomerAddress
                         , gVendorAddress
                         , gCCAddress
                         , pAddress
                         , pCustomerToAddress
                         , pVendorToAddress
                         , pCCQuoteToAddress
                         , gCCQuotes
                         , pCCQuote
                         , eCCData
                         , gCCQuote
                         , eCCQuote
                         , dCCQuote
                         , gCCQuoteReport
                         , pCurbToQuote
                         , eCurbOnQuote
                         , dCurbFromQuote
                         , eCCQuoteConfirm
                         , eCCQuoteNotConfirm
                         , eCCQuoteToOrder
                         -- static routes --
                         , cssRes
                         , htmlRes
                         , jsRes
                         , favIcon
                         , logo
                         , logoSmall
                         )
import Database ( getConn )
import Utils ( apiError401
             , apiError404
             , textToInt
             )

-- | ERP entrypoint server.
main :: IO ()
main = putStrLn "Listening on port: 4000"
    >> run 4000 (logStdoutDev $ simpleCors app)

-- | The WAI/warp app with middleware.
app :: Request -> (Response -> IO b) -> IO b
app req res = do
  conn <- getConn
  reqId <- nextRandom
  ctx@(Context (newReq, _)) <- authMiddleware conn reqId req
  innerApp (requestMethod newReq) (pathInfo newReq) ctx >>= res

-- | Router. Lists controllers and user permission levels required. This is set up to have a 404
-- catch all as the last fn instance. ORDER matters!
innerApp :: Method -> [Text] -> Context -> IO Response
innerApp "GET" ["customers"] = pcm 1 up_customers gCustomers
innerApp "POST" ["customers"] = pcm 2 up_customers pCustomer
innerApp "GET" ["customers", cid] = pcm 1 up_customers (gCustomer (textToInt cid))
innerApp "PUT" ["customers", cid] = pcm 2 up_customers (eCustomer (textToInt cid))
innerApp "PUT" ["customers", cid, "tax-exempt-off"] = pcm 2 up_customers (pCustomerTaxStatusOff (textToInt cid))
innerApp "PUT" ["customers", cid, "tax-exempt-on"] = pcm 2 up_customers (pCustomerTaxStatusOn (textToInt cid))

innerApp "GET" ["vendors"] = pcm 1 up_vendors gVendors
innerApp "POST" ["vendors"] = pcm 2 up_vendors pVendor
innerApp "GET" ["vendors", vid] = pcm 1 up_vendors (gVendor (textToInt vid))
innerApp "PUT" ["vendors", vid] = pcm 2 up_vendors (eVendor (textToInt vid))

innerApp "GET" ["inventory"] = pcm 1 up_inventory gInventory
innerApp "PUT" ["inventory", iid] = pcm 2 up_inventory (eInventory (textToInt iid))

innerApp "PUT" ["line-items", liid, "extras"] = pcm 2 up_orders (pExtraFlag (textToInt liid))

innerApp "GET" ["materials"] = pcm 1 up_materials gMaterials
innerApp "GET" ["materials", mid] = pcm 1 up_materials (gMaterial (textToInt mid))
innerApp "PUT" ["materials", mid] = pcm 2 up_materials (eMaterial (textToInt mid))
innerApp "POST" ["materials"] = pcm 2 up_materials pMaterial

innerApp "GET" ["mat-purchases"] = pcm 1 up_materials gMaterialPurchases
innerApp "GET" ["mat-purchases", mpid] = pcm 1 up_materials (gMaterialPurchase (textToInt mpid))
innerApp "PUT" ["mat-purchases", mpid] = pcm 2 up_materials (eMaterialPurchase (textToInt mpid))
innerApp "DELETE" ["mat-purchases", mpid] = pcm 3 up_materials (dMaterialPurchase (textToInt mpid))
innerApp "POST" ["mat-purchases"] = pcm 2 up_materials pMaterialPurchase
innerApp "GET" ["mat-purchases", "options", matid] = pcm 1 up_materials (gMatPurchaseOptionsForMaterial (textToInt matid))

innerApp "GET" ["fab-items"] = pcm 1 up_fabrication gFabItems
innerApp "POST" ["fab-items"] = pcm 2 up_fabrication pFabItem
innerApp "PUT" ["fab-items", fabid] = pcm 2 up_fabrication (eFabItem (textToInt fabid))

innerApp "GET" ["purchase-items"] = pcm 1 up_purchase gPurchaseItems
innerApp "POST" ["purchase-items"] = pcm 2 up_purchase pPurchaseItem
innerApp "PUT" ["purchase-items", pid] = pcm 2 up_purchase (ePurchaseItem (textToInt pid))
innerApp "DELETE" ["purchase-items", pid] = pcm 3 up_purchase (dPurchaseItem (textToInt pid))

innerApp "GET" ["purchase-orders"] = pcm 1 up_po (gPurchaseOrders False)
innerApp "GET" ["purchase-orders", "closed"] = pcm 1 up_po (gPurchaseOrders True)
innerApp "GET" ["purchase-orders", poid] = pcm 1 up_po (gPurchaseOrderById (textToInt poid))
innerApp "POST" ["purchase-orders"] = pcm 2 up_po pPurchaseOrder
innerApp "PUT" ["purchase-orders", poid] = pcm 2 up_po (ePurchaseOrder (textToInt poid))
innerApp "DELETE" ["purchase-orders", poid] = pcm 3 up_po (dPurchaseOrder (textToInt poid))
innerApp "PUT" ["purchase-orders", poid, "qbdone"] = pcm 2 up_po (ePurchaseOrderQBSet (textToInt poid))
innerApp "PUT" ["purchase-orders", poid, "as-sent"] = pcm 2 up_po (ePurchaseOrderAsSent (textToInt poid))
innerApp "PUT" ["purchase-orders", poid, "as-received"] = pcm 2 up_po (ePurchaseOrderAsReceived (textToInt poid))
innerApp "PUT" ["purchase-orders", poid, "as-prices-verified"] = pcm 2 up_po (ePurchaseOrderPricesVerified (textToInt poid))
innerApp "PUT" ["purchase-orders", poid, "as-prices-adjusted"] = pcm 2 up_po (ePurchaseOrderPricesAdjusted (textToInt poid))
innerApp "PUT" ["purchase-orders", poid, "as-closed"] = pcm 2 up_po (ePurchaseOrderAsClosed (textToInt poid))
innerApp "POST" ["purchase-orders", poid, "items"] = pcm 2 up_po (pPurchaseOrderItem (textToInt poid))
innerApp "DELETE" ["purchase-orders", poid, "items", poiid] = pcm 3 up_po (dPurchaseOrderItem (textToInt poid) (textToInt poiid))
innerApp "PUT" ["purchase-orders", poid, "items", poiid] = pcm 3 up_po (ePurchaseOrderItem (textToInt poid) (textToInt poiid))
innerApp "GET" ["purchase-orders", poid, "rfq"] = pcm 2 up_po (gPurchaseOrderRfq (textToInt poid))
innerApp "GET" ["purchase-orders", poid, "po"] = pcm 2 up_po (gPurchaseOrderReport (textToInt poid))
innerApp "POST" ["purchase-orders", poid, "back-order"] = pcm 2 up_po (pBackOrder (textToInt poid))
innerApp "GET" ["purchase-orders", "number", ponum] = pcm 1 up_po (gPurchaseOrderByNumber ponum)

innerApp "POST" ["stock", "order"] = pcm 2 up_orders pStockOrder
innerApp "POST" ["orders", otype] = pcm 2 up_orders (pOrder otype)
innerApp "POST" ["orders", otype, "catalog", catid] = pcm 2 up_orders (pOrderViaCatalog otype (textToInt catid))
innerApp "GET" ["orders"] = pcm 1 up_orders gOrders
innerApp "GET" ["orders", "skeleton"] = pcm 1 up_orders gOrderSkeletons
innerApp "GET" ["orders", "number", ordNum, "FL"] = pcm 3 up_orders (hFLOrder ordNum)
innerApp "GET" ["orders", "non-complete"] = pcm 2 up_orders gNotDoneNormalOrders
innerApp "GET" ["orders", "non-billed"] = pcm 2 up_orders gNotBilledNormalOrders
innerApp "GET" ["orders", onum, "non-complete-check"] = pcm 2 up_orders (gNotBilledNormalStatus onum)
innerApp "PUT" ["orders", "number", ordNum, "bill", invoiceNumber] = pcm 2 up_orders (eOrderToBilledByNumber ordNum invoiceNumber)
innerApp "PUT" ["orders", "membrain-sales"] = pcm 3 up_orders gMembrainOrderData
innerApp "PUT" ["orders", "detailed-sales"] = pcm 3 up_orders gDetailedSalesData
innerApp "PUT" ["orders", "build-assemblies-report"] = pcm 3 up_orders gBuildAssemblyReport
innerApp "GET" ["orders", oid] = pcm 1 up_orders (gOrder (textToInt oid))
innerApp "PUT" ["orders", oid] = pcm 2 up_orders (eOrder (textToInt oid))
innerApp "DELETE" ["orders", oid] = pcm 3 up_orders (dOrder (textToInt oid))
innerApp "PUT" ["orders", oid, "bill", invoiceNumber] = pcm 2 up_orders (eOrderToBilled (textToInt oid) invoiceNumber)
innerApp "PUT" ["orders", oid, "notbill"] = pcm 2 up_orders (eOrderToNotBilled (textToInt oid))
innerApp "PUT" ["orders", oid, "complete"] = pcm 2 up_orders (eOrderToComplete (textToInt oid))
innerApp "PUT" ["orders", oid, "notcomplete"] = pcm 2 up_orders (eOrderToNotComplete (textToInt oid))
innerApp "GET" ["orders", oid, "flange-count"] = pcm 1 up_orders (gBDFlangeCount (textToInt oid))
innerApp "GET" ["orders", oid, "detailed-sales"] = pcm 1 up_orders (gOrderLineItemDetailReport (textToInt oid))
innerApp "GET" ["orders", oid, "packing-slip"] = pcm 1 up_orders (gOrderPackingSlip (textToInt oid))
innerApp "POST" ["progressive", "orders"] = pcm 3 up_orders pProgressiveOrder
innerApp "PUT" ["progressive", "orders", oid, "add"] = pcm 2 up_orders (eProgressiveOrderStatus (textToInt oid) True)
innerApp "PUT" ["progressive", "orders", oid, "subtract"] = pcm 2 up_orders (eProgressiveOrderStatus (textToInt oid) False)

innerApp "GET" ["projects", "skeleton"] = pcm 1 up_projects (gProjectSkeletons False)
innerApp "GET" ["projects", "skeleton", "inactive"] = pcm 1 up_projects (gProjectSkeletons True)
innerApp "GET" ["projects", "orders", "non-complete"] = pcm 2 up_projects gNotDoneProjectOrders
innerApp "POST" ["projects", "orders", onum, "non-complete-check"] = pcm 2 up_projects (pNowDoneChecker onum)
innerApp "GET" ["projects"] = pcm 1 up_projects gProjects
innerApp "GET" ["projects", "status"] = pcm 1 up_projects gProjectStatusReport
innerApp "GET" ["projects", pid] = pcm 1 up_projects (gProject (textToInt pid))
innerApp "PUT" ["projects", pid] = pcm 1 up_projects (eProject (textToInt pid))
innerApp "DELETE" ["projects", pid] = pcm 3 up_projects (dProject (textToInt pid))
innerApp "GET" ["projects", pid, "detailed-sales"] = pcm 3 up_projects (gProjectDetailedSalesData (textToInt pid))
innerApp "POST" ["projects", pid, "sections"] = pcm 2 up_projects (pProjectSection (textToInt pid))
innerApp "PUT" ["projects", pid, "sections", sid, "orders", oid] = pcm 2 up_projects (eProjectOrder (textToInt pid) (textToInt sid) (textToInt oid))
innerApp "PUT" ["projects", pid, "sections", sid] = pcm 2 up_projects (eProjectSection (textToInt pid) (textToInt sid))
innerApp "DELETE" ["projects", pid, "sections", sid] = pcm 3 up_projects (dProjectSection (textToInt pid) (textToInt sid))
innerApp "GET" ["projects", pid, "tracker"] = pcm 1 up_projects (gProjectTracker (textToInt pid))
innerApp "GET" ["projects", pid, "sections", sid, "tracker"] = pcm 1 up_projects (gProjectSectionTracker (textToInt pid) (textToInt sid))
innerApp "GET" ["projects", pid, "orders", oid, "tracker"] = pcm 1 up_projects (gProjectOrderTracker (textToInt pid) (textToInt oid))
innerApp "GET" ["projects", "appId", appId] = pcm 1 up_projects (gProjectAppId (textToInt appId))
innerApp "POST" ["projects", "appId", appId, "billing"] = pcm 3 up_projects (hProjectBilling (textToInt appId))
innerApp "POST" ["projects", pid, "billed"] = pcm 3 up_projects (eProjectOrdersAsBilled (textToInt pid))
innerApp "POST" ["projects", "appId", appId, "sections", sid, "orders"] = pcm 2 up_projects (pProjectOrder (textToInt appId) (textToInt sid))
innerApp "PUT" ["projects", pid, "orders", oid, "sections", sid] =
  pcm 2 up_projects (sProjectOrderSection (textToInt pid) (textToInt oid) (textToInt sid))
innerApp "GET" ["projects", pid, "extras"] = pcm 1 up_projects (gProjectExtras (textToInt pid))
innerApp "POST" ["projects", pid, "extras"] = pcm 2 up_projects (eProjectExtras (textToInt pid))
innerApp "DELETE" ["projects", pid, "extras", eid] = pcm 3 up_projects (dProjectExtra (textToInt pid) (textToInt eid))
innerApp "POST" ["projects", pid, "active"] = pcm 2 up_projects (eProjectStatus True (textToInt pid))
innerApp "POST" ["projects", pid, "inactive"] = pcm 2 up_projects (eProjectStatus False (textToInt pid))

innerApp "POST" ["quotes"] = pcm 2 up_quotes pQuoteSans
innerApp "POST" ["quotes", quoteType] = pcm 2 up_quotes (pQuote quoteType)
innerApp "POST" ["quotes", quoteType, "catalog", catid] = pcm 2 up_quotes (pQuoteCatalog quoteType (textToInt catid))
innerApp "GET" ["quotes"] = pcm 1 up_quotes gQuotes
innerApp "GET" ["quotes", "skeleton"] = pcm 1 up_quotes gQuoteSkeletons
innerApp "PUT" ["quotes", qid] = pcm 2 up_quotes (eQuote (textToInt qid))
innerApp "DELETE" ["quotes", qid] = pcm 3 up_quotes (dQuote (textToInt qid))
innerApp "POST" ["quotes", qid, "order"] = pcm 2 up_quotes (pQuoteToOrder (textToInt qid))
innerApp "POST" ["quotes", qid, "project"] = pcm 2 up_quotes (pQuoteToProject (textToInt qid))
innerApp "GET" ["quotes", qid] = pcm 1 up_quotes (gQuote (textToInt qid))
innerApp "GET" ["quotes", qid, "report"] = pcm 1 up_quotes (gQuoteReport (textToInt qid))
innerApp "GET" ["quotes", qid, "project-estimate"] = pcm 2 up_quotes (gProjectEstimate (textToInt qid))
innerApp "GET" ["quotes", qid, "detailed-sales"] = pcm 2 up_quotes (gQuoteLineItemDetailReport (textToInt qid))

innerApp "GET" ["c-c"] = pcm 1 up_curbCo gCCQuotes
innerApp "POST" ["c-c"] = pcm 2 up_curbCo pCCQuote
innerApp "PUT" ["c-c", "sales-data"] = pcm 2 up_curbCo eCCData
innerApp "GET" ["c-c", "pid", pid] = pcm 1 up_curbCo (gCCQuote pid)
innerApp "PUT" ["c-c", qid] = pcm 2 up_curbCo (eCCQuote (textToInt qid))
innerApp "DELETE" ["c-c", "pid", pid] = pcm 3 up_curbCo (dCCQuote pid)
innerApp "GET" ["c-c", "pid", pid, "report"] = pcm 1 up_curbCo (gCCQuoteReport pid)
innerApp "POST" ["c-c", "pid", pid, "curbs"] = pcm 2 up_curbCo (pCurbToQuote pid)
innerApp "PUT" ["c-c", "pid", pid, "curbs", curbId] = pcm 2 up_curbCo (eCurbOnQuote pid (textToInt curbId))
innerApp "DELETE" ["c-c", "pid", pid, "curbs", curbId] = pcm 3 up_curbCo (dCurbFromQuote pid (textToInt curbId))
innerApp "PUT" ["c-c", "pid", pid, "is-confirmed"] = pcm 2 up_curbCo (eCCQuoteConfirm pid)
innerApp "PUT" ["c-c", "pid", pid, "not-confirmed"] = pcm 2 up_curbCo (eCCQuoteNotConfirm pid)
innerApp "PUT" ["c-c", "pid", pid, "order-number", ordNum, "po", po] = pcm 2 up_curbCo (eCCQuoteToOrder pid ordNum po)

innerApp "GET" ["assemblies"] = pcm 1 up_assemblies gAssemblies
innerApp "POST" ["assemblies"] = pcm 2 up_assemblies pAssembly
innerApp "PUT" ["assemblies", aid] = pcm 2 up_assemblies (eAssembly (textToInt aid))
innerApp "PUT" ["assemblies", aid, "partial"] = pcm 2 up_assemblies (eAssemblyPartial (textToInt aid))
innerApp "DELETE" ["assemblies", aid] = pcm 3 up_assemblies (dAssembly (textToInt aid))
innerApp "GET" ["assemblies", aid] = pcm 1 up_assemblies (gAssemblyById (textToInt aid))
innerApp "DELETE" ["assemblies", aid, "subItems", sid] = pcm 3 up_assemblies (dSubItem (textToInt aid) (textToInt sid))

innerApp "GET" ["users"] = pcm 1 up_users gUsers
innerApp "POST" ["users"] = pcm 2 up_users pUser
innerApp "PUT" ["users", uid] = pcm 2 up_users (eUser (textToInt uid))
innerApp "DELETE" ["users", uid] = pcm 3 up_users (dUser (textToInt uid))
innerApp "GET" ["users", uid, "permissions"] = pcm 3 up_users (gUserPermissions (textToInt uid))
innerApp "PUT" ["users", uid, "permissions"] = pcm 3 up_users (eUserPermissions (textToInt uid))

innerApp "GET" ["markups"] = pcm 1 up_pricing gMarkups
innerApp "POST" ["markups", "labor"] = pcm 2 up_pricing pLaborRate
innerApp "POST" ["markups", "margins"] = pcm 2 up_pricing pMargins

innerApp "PUT" ["login"] = loginCheck
innerApp "PUT" ["logout"] = logoutCheck
innerApp "PUT" ["unauthorized"] = \_ -> errJsonRes $ apiError401 "Unauthorized"

innerApp "GET" ["pricing-lists"] = pcm 1 up_pricingLists gPricingLists
innerApp "POST" ["pricing-lists"] = pcm 2 up_pricingLists pPricingList
innerApp "DELETE" ["pricing-lists", plid] = pcm 3 up_pricingLists (dPricingList (textToInt plid))
innerApp "DELETE" ["pricing-lists", plid, "items"] = pcm 3 up_pricingLists (dPricingListItem (textToInt plid))
innerApp "PUT" ["pricing-lists", plid, "items"] = pcm 2 up_pricingLists (ePricingListItems (textToInt plid))
innerApp "GET" ["pricing-lists", plid] = pcm 1 up_pricingLists (gPricingList (textToInt plid))
innerApp "GET" ["pricing-lists", plid, "report"] = pcm 2 up_pricingLists (gPricingListReport (textToInt plid))
innerApp "GET" ["pricing-lists", plid, "customers"] = pcm 1 up_pricingLists (gPricingListCustomers (textToInt plid))
innerApp "POST" ["pricing-lists", plid, "customers"] = pcm 2 up_pricingLists (pPricingListCustomer (textToInt plid))
innerApp "DELETE" ["pricing-lists", plid, "customers", cid] = pcm 3 up_pricingLists (dPricingListCustomer (textToInt plid) (textToInt cid))

innerApp "GET" ["feedback-complaints"] = pcm 1 up_feedbackComplaints gFeedbackComplaints
innerApp "POST" ["feedback-complaints"] = pcm 2 up_feedbackComplaints pFeedbackComplaint
innerApp "DELETE" ["feedback-complaints", fcid] = pcm 3 up_feedbackComplaints (dFeedbackComplaint (textToInt fcid))
innerApp "PUT" ["feedback-complaints", fcid] = pcm 2 up_feedbackComplaints (eFeedbackComplaint (textToInt fcid))

innerApp "GET" ["error-log"] = pcm 1 up_errLog gErrorLog
innerApp "POST" ["error-log"] = pcm 2 up_errLog pErrorLog
innerApp "DELETE" ["error-log", elid] = pcm 3 up_errLog (dErrorLog (textToInt elid))
innerApp "PUT" ["error-log", elid] = pcm 2 up_errLog (eErrorLog (textToInt elid))

innerApp "POST" ["addresses"] = pcm 2 up_customers pAddress
innerApp "GET" ["addresses"] = pcm 1 up_customers gAddresses
innerApp "GET" ["addresses", "customer", cid] = pcm 1 up_customers (gCustomerAddress (textToInt cid))
innerApp "GET" ["addresses", "vendor", vid] = pcm 1 up_vendors (gVendorAddress (textToInt vid))
innerApp "GET" ["addresses", "c-c", pid] = pcm 1 up_curbCo (gCCAddress pid)
innerApp "GET" ["addresses", aid] = pcm 1 up_customers (gAddress (textToInt aid))
innerApp "PUT" ["addresses", aid] = pcm 2 up_customers (eAddress (textToInt aid))
innerApp "DELETE" ["addresses", aid] = pcm 3 up_customers (dAddress (textToInt aid))
innerApp "POST" ["addresses", aid, "customer", cid] = pcm 2 up_customers (pCustomerToAddress (textToInt aid) (textToInt cid))
innerApp "POST" ["addresses", aid, "vendor", vid] = pcm 2 up_vendors (pVendorToAddress (textToInt aid) (textToInt vid))
innerApp "POST" ["addresses", aid, "c-c", cqpid] = pcm 2 up_curbCo (pCCQuoteToAddress (textToInt aid) (textToInt cqpid))

-- STATIC ROUTES --
innerApp "GET" ["views", name] = htmlRes (unpack name)
innerApp "GET" ["css", name] = cssRes (unpack name)
innerApp "GET" ["js", name] = jsRes (unpack name)
innerApp "GET" ["main"] = htmlRes "main_view.html"
innerApp "GET" [""] = htmlRes "main_view.html"
innerApp "GET" [] = htmlRes "main_view.html"
innerApp "GET" ["favicon.ico"] = favIcon
innerApp "GET" ["logo.png"] = logo
innerApp "GET" ["logo_small.png"] = logoSmall
innerApp _ _ = \_ -> errJsonRes $ apiError404 "Not found"
