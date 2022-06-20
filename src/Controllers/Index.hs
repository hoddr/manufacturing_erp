{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.Index
Description: Controllers and handles for all routing (re-export file).
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Re-export file.

Provides the controllers/handles for all routes of the ERP server. Most of the sanity checking and
input validation occurs here. Be very careful with the ordering of specific steps, as they are often linked to
key safety checks and ore critial logic flows.

Controllers either return static web files (HTML, JS, CSS) or a JSON response. Response status
codes follow typical HTTP standards in content and meaining.

Naming convention generally runs as follows: GET requests begin with g; POST request with p; PUT
requests with e, and DELETE requests with d.

-}
module Controllers.Index
  ( -- * Middleware
    authMiddleware
    -- *  Static Handles
  , cssRes
  , htmlRes
  , favIcon
  , logo
  , logoSmall
    -- * JSON Handles
  , errJsonRes
  , jsRes
    -- * Addresses
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
    -- * Assemblies
  , gAssemblies
  , pAssembly
  , eAssembly
  , eAssemblyPartial
  , dAssembly
  , gAssemblyById
  , dSubItem
    -- * Inventory
  , gInventory
  , eInventory
    -- * Line Items
  , pExtraFlag
    -- * Materials
  , gMaterials
  , gMaterial
  , eMaterial
  , pMaterial
    -- * Material Purchases
  , gMaterialPurchases
  , gMaterialPurchase
  , eMaterialPurchase
  , dMaterialPurchase
  , pMaterialPurchase
  , gMatPurchaseOptionsForMaterial
    -- * Fab Items
  , gFabItems
  , pFabItem
  , eFabItem
    -- * Purchase Items
  , gPurchaseItems
  , pPurchaseItem
  , ePurchaseItem
  , dPurchaseItem
    -- * Customers
  , gCustomers
  , pCustomer
  , gCustomer
  , eCustomer
  , pCustomerTaxStatusOff
  , pCustomerTaxStatusOn
    -- * Orders
  , pOrder
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
  , gBDFlangeCount
  , gOrderLineItemDetailReport
  , gOrderPackingSlip
  , gDetailedSalesData
  , gMembrainOrderData
  , gBuildAssemblyReport
    -- * Quotes
  , gProjectEstimate
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
  , gQuoteLineItemDetailReport
    -- * Vendors
  , gVendors
  , pVendor
  , gVendor
  , eVendor
    -- * Projects
  , gProjectSkeletons
  , gProjects
  , gProjectStatusReport
  , eProject
  , dProject
  , dProjectExtra
  , gProject
  , gProjectAppId
  , pProjectSection
  , dProjectSection
  , eProjectSection
  , hProjectBilling
  , eProjectOrdersAsBilled
  , gProjectTracker
  , gProjectSectionTracker
  , gProjectOrderTracker
  , gProjectExtras
  , eProjectExtras
  , eProjectStatus
  , gNotBilledNormalOrders
  , gNotBilledNormalStatus
  , gNotDoneNormalOrders
  , gNotDoneProjectOrders
  , pNowDoneChecker
  , gProjectDetailedSalesData
    -- * Users
  , gUsers
  , eUser
  , pUser
  , dUser
  , gUserPermissions
  , eUserPermissions
    -- * Markups and Labor Rates
  , gMarkups
  , pLaborRate
  , pMargins
    -- * Authentication
  , loginCheck
  , logoutCheck
  , pcm
    -- * Pricing Lists
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
    -- * Feedback Complaints
  , pFeedbackComplaint
  , dFeedbackComplaint
  , gFeedbackComplaints
  , eFeedbackComplaint
    -- * Purchase Orders
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
    -- * Error Log
  , gErrorLog
  , pErrorLog
  , dErrorLog
  , eErrorLog
    -- * CC
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
  )
  where

import Controllers.Auth
import Controllers.Addresses
import Controllers.Assemblies
import Controllers.CC
import Controllers.Customers
import Controllers.ErrorLog
import Controllers.FabItems
import Controllers.FeedbackComplaints
import Controllers.Inventory
import Controllers.MarkupLabor
import Controllers.Materials
import Controllers.Orders
import Controllers.PricingLists
import Controllers.Projects
import Controllers.PurchaseItems
import Controllers.PurchaseOrders
import Controllers.Quotes
import Controllers.Utils
import Controllers.Vendors
