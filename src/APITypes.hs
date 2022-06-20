{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: APITypes
Description: Module to collate and re-export all data type declarations and various utilities.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Module to collate and re-export all data type declarations and various utilities.

-}

module APITypes
  ( Address(..)
  , APIError(..)
  , APIMsg(..)
  , AppState(..)
  , Assembly(..)
  , BuildAssemblyReport(..)
  , Category(..)
  , Context(..)
  , CCCurb(..)
  , CCQuote(..)
  , Customer(..)
  , CustomerTypeLookup(..)
  , DateRange(..)
  , DetailedSalesData(..)
  , ErrorLogEntry(..)
  , Extra(..)
  , FabItem(..)
  , FeedbackComplaint(..)
  , FixedPriceItem(..)
  , FlangeReportEntry(..)
  , Inventory(..)
  , JobExt(..)
  , LaborRate(..)
  , LineItem(..)
  , Markup(..)
  , Material(..)
  , MatInfo(..)
  , MatPurchase(..)
  , MembrainOrderData(..)
  , NewUser(..)
  , Order(..)
  , PermissionSet(..)
  , PricingInfo(..)
  , PricingList(..)
  , Project(..)
  , ProjectBillingInfo(..)
  , ProjectSection(..)
  , ProjectStatusReport(..)
  , ProjectTracker(..)
  , PurchaseBackOrder(..)
  , PurchaseItem(..)
  , PurchaseOrder(..)
  , POItem(..)
  , QueryPag(..)
  , Quote(..)
  , RawLineItem(..)
  , SubItem(..)
  , Token(..)
  , User(..)
  , UserPermissions(..)
  , Vendor(..)
  , assemblyInv
  , contextConn
  , defQueryPag
  , fabInv
  , formatDate
  , genState
  , isFitting
  , isOvalPipe
  , isPipe
  , isStraightDuct
  , materialInv
  , purchaseInv
  , takeN
  )
  where

-- FOR RE-EXPORT IMPORTS --
import Data.Auth
import Data.CC
import Data.CustomersVendors
import Data.FeedbackLogs
import Data.Inventory
import Data.Items
import Data.Materials
import Data.Misc
import Data.OrdersQuotes
import Data.Pricing
import Data.Projects
import Data.PurchaseOrders
import Data.Utils
