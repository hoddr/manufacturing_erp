{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Reports
Description: HTML report generation.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Generators for various HTML (to be saved as PDF) reports for things like RFQs, quotes,
project estimates, and more. Leverages the "Lucid" library to create the HTML pages.

WARNING: take care with the indentation!

-}
module Reports
  ( curbCoQuoteHtml
  , packingSlipHtml
  , poHtml
  , pricingListHtml
  , projectHtml
  , quoteHtml
  , rfqHtml
  )
  where

import qualified Data.ByteString.Lazy as LBS
import Data.List ( foldl' )
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Data.UUID ( UUID
                 , toText
                 )
import Lucid
import Text.Printf ( printf )

-- LOCAL IMPORTS
import APITypes ( Address(..)
                , APIError
                , CCCurb(..)
                , CCQuote(..)
                , Customer(..)
                , FixedPriceItem(..)
                , LineItem(..)
                , Order(..)
                , POItem(..)
                , PricingList(..)
                , PurchaseOrder(..)
                , Quote(..)
                , Vendor(..)
                , formatDate
                )
import Utils ( passSuccess )

-- LOCAL FNS
-- | Creates a quote template.
quoteHtml :: UTCTime -> UUID -> Quote -> Address -> IO (Either APIError LBS.ByteString)
quoteHtml ct uuid q a = passSuccess $ renderBS $ do
  doctype_
  html_ [lang_ "en"] $ do
    genericHeader uuid ("Quote #: " <> qnum q) "doc_gen.css"
    body_ $ do
      div_ [class_ "gcont h10"] $ do
        div_ [class_ "lal"] $ do
          img_ [class_ "logo", src_ "https://erp.erp.com/logo.png", alt_ "hoddr Company"]
        div_ [class_ "lal col", style_ "grid-column: 2 / 6"] $ do
          sequence_ erpHeaderAddress
        div_ [class_ "ral col", style_ "grid-column: 7 / 9"] $ do
          h3_ $ toHtmlFromText "Quote"
          p_ $ toHtmlFromText $ formatDate (fromMaybe ct $ qcreated q) -- set to current date by default if created not set
          p_ $ toHtmlFromText $ "Quote #: " <> qnum q

      div_ [class_ "gcont h15"] $ do
        div_ [class_ "lal col", style_ "grid-column: 1 / 4"] $ do
          h4_ $ toHtmlFromText "Customer:"
          p_ $ toHtmlFromText $ cname $ qcust q
          p_ $ toHtmlFromText $ add_street a
          p_ $ toHtmlFromText $ formatAddressL2 a
          p_ $ toHtmlFromText $ formatAddressL3 a
--         div_ [class_ "lal col", style_ "grid-column: 4 / 7"] $ do
--           h4_ $ toHtmlFromText "Ship To:"
--           p_ $ toHtmlFromText $ cname $ qcust q
--           p_ $ toHtmlFromText "{{Ship Address Line 1}}"
--           p_ $ toHtmlFromText "{{Ship Address Line 2}}"
        div_ [class_ "lal col", style_ "grid-column: 7/10"] $ do
          h4_ $ toHtmlFromText "PO - Job Name: "
          p_ $ toHtmlFromText $ fromMaybe "TBD" $ qpo q

      div_ [class_ "gcont-items"] $ do
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Qty"
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Item"
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Description"
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Rate"
        h4_ [class_ "brd-top brd-bot brd-left brd-right talign-center small-font bgfill"] $ toHtmlFromText "Extended"

        sequence_ $ genLineItems (reverse $ qlineItems q)

        div_ [class_ "brd-top"] $ toHtmlFromText ""
        div_ [class_ "brd-top"] $ toHtmlFromText ""
        div_ [class_ "brd-top"] $ toHtmlFromText ""
        h5_ [class_ "brd-top brd-left"] $ toHtmlFromText "Subtotal"
        p_ [class_ "brd-top brd-left brd-right talign-right small-font"] $ toHtmlFromText $ "$" <> formatn 2 subTotal

        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        h5_ [class_ "brd-top brd-left"] $ toHtmlFromText "Sales Tax (6.0%)"
        p_ [class_ "brd-top brd-left brd-right talign-right small-font"] $ toHtmlFromText $ "$" <> formatn 2 taxTotal

        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        h3_ [class_ "brd-top brd-bot brd-left"] $ toHtmlFromText "Total"
        h4_ [class_ "brd-top brd-bot brd-left brd-right talign-right small-font"] $ toHtmlFromText $ "$" <> formatn 2 grandTotal

        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        h4_ [class_ "small-font", style_ "grid-column: 4 / 6"] $ toHtmlFromText "Price is guaranteed for 20 days"
  where subTotal = qtotal q
        taxSubTotal = getTaxableTotal (qlineItems q)
        taxTotal = if cisTaxExempt (qcust q)
                      then 0.00
                      else taxSubTotal * 0.06
        grandTotal = subTotal + taxTotal

-- | Generator for line item table.
genLineItems :: Monad m => [LineItem] -> [HtmlT m ()]
genLineItems = foldl' (\acc li ->
  [ p_ [class_ "brd-left talign-center small-font qline"] $ toHtmlFromText $ formatn 0 $ lquant li
  , p_ [class_ "brd-left small-font qline"] $ toHtmlFromText $ lcategory li
  , p_ [class_ "brd-left small-font qline"] $ toHtmlFromText $ ldesc li
  , p_ [class_ "brd-left talign-right small-font qline"] $ toHtmlFromText $ formatn 4 $ lprice li
  , p_ [class_ "brd-left brd-right talign-right small-font qline"] $ toHtmlFromText $ formatn 2 (lprice li * lquant li)
  ] ++ acc) []

-- | Generator for CC quote HTML page.
curbCoQuoteHtml :: UTCTime -> UUID -> CCQuote -> Address -> IO (Either APIError LBS.ByteString)
curbCoQuoteHtml ct uuid ccq a = passSuccess $ renderBS $ do
  doctype_
  html_ [lang_ "en"] $ do
    genericHeader uuid ("Quote #: AT-" <> ccq_pid ccq) "doc_gen.css"
    body_ $ do
      div_ [class_ "gcont h10"] $ do
        div_ [class_ "lal"] $ do
          img_ [class_ "logo", src_ "https://erp.erp.com/logo.png", alt_ "hoddr Company"]
        div_ [class_ "lal col", style_ "grid-column: 2 / 6"] $ do
          sequence_ erpHeaderAddress
        div_ [class_ "ral col", style_ "grid-column: 7 / 9"] $ do
          h3_ $ toHtmlFromText "Quote"
          p_ $ toHtmlFromText $ formatDate ct
          p_ $ toHtmlFromText $ "Quote #: AT-" <> ccq_pid ccq

      div_ [class_ "gcont h15"] $ do
        div_ [class_ "lal col", style_ "grid-column: 1 / 4"] $ do
          h4_ $ toHtmlFromText "Ship To:"
          p_ $ toHtmlFromText $ cname $ ccq_customer ccq
          p_ $ toHtmlFromText $ add_street a
          p_ $ toHtmlFromText $ formatAddressL2 a
          p_ $ toHtmlFromText $ formatAddressL3 a

      div_ [class_ "gcont-c-c-items"] $ do
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Qty"
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Old Unit/Bottom"
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "New Unit/Top"
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Adapter"
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Rate"
        h4_ [class_ "brd-top brd-bot brd-left brd-right talign-center small-font bgfill"] $ toHtmlFromText "Extended"

        sequence_ $ genCCLineItems $ reverse $ ccq_curbs ccq

        div_ [class_ "brd-top"] $ toHtmlFromText ""
        div_ [class_ "brd-top"] $ toHtmlFromText ""
        div_ [class_ "brd-top"] $ toHtmlFromText ""
        div_ [class_ "brd-top"] $ toHtmlFromText ""
        h5_ [class_ "brd-top brd-left"] $ toHtmlFromText "Subtotal"
        p_ [class_ "brd-top brd-left brd-right talign-right small-font"] $ toHtmlFromText $ "$" <> formatn 2 subTotal

        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        h5_ [class_ "brd-top brd-left"] $ toHtmlFromText "Sales Tax (6.0%)"
        p_ [class_ "brd-top brd-left brd-right talign-right small-font"] $ toHtmlFromText $ "$" <> formatn 2 taxTotal

        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        h5_ [class_ "brd-top brd-left"] $ toHtmlFromText "Shipping & Handling"
        p_ [class_ "brd-top brd-left brd-right talign-right small-font"] $ toHtmlFromText $ "$" <> formatn 2 (ccq_shippingCosts ccq)

        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        h5_ [class_ "brd-top brd-left"] $ toHtmlFromText "Fast Pass"
        p_ [class_ "brd-top brd-left brd-right talign-right small-font"] $ toHtmlFromText $ "$" <> formatn 2 (ccq_fastPassCost ccq)

        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        h3_ [class_ "brd-top brd-bot brd-left"] $ toHtmlFromText "Total"
        h4_ [class_ "brd-top brd-bot brd-left brd-right talign-right small-font"] $ toHtmlFromText $ "$" <> formatn 2 grandTotal

        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        div_ [] $ toHtmlFromText ""
        -- TODO price guarantee timing?
        h4_ [class_ "small-font", style_ "grid-column: 5 / 7"] $ toHtmlFromText "Price is guaranteed for 20 days"
  where subTotal = foldl' (\acc ccc -> acc + ccc_priceEach ccc * ccc_quantity ccc) 0.00 (ccq_curbs ccq)
        taxSubTotal = subTotal
        taxTotal = if cisTaxExempt (ccq_customer ccq)
                      then 0.00
                      else taxSubTotal * 0.06
        grandTotal = subTotal + taxTotal + ccq_shippingCosts ccq + ccq_fastPassCost ccq

-- | Generator for CC curb line item table.
genCCLineItems :: Monad m => [CCCurb] -> [HtmlT m ()]
genCCLineItems = foldl' (\acc ccc ->
  [ p_ [class_ "brd-left talign-center small-font line"] $ toHtmlFromText $ formatn 0 $ ccc_quantity ccc
  , p_ [class_ "brd-left small-font line"] $ toHtmlFromText $ ccc_oldUnit ccc
  , p_ [class_ "brd-left small-font line"] $ toHtmlFromText $ ccc_newUnit ccc
  , p_ [class_ "brd-left small-font line"] $ toHtmlFromText $ ccc_adapter ccc
  , p_ [class_ "brd-left talign-right small-font line"] $ toHtmlFromText $ formatn 2 $ ccc_priceEach ccc
  , p_ [class_ "brd-left brd-right talign-right small-font line"] $ toHtmlFromText $ formatn 2 (ccc_priceEach ccc * ccc_quantity ccc)
  ] ++ acc) []

-- | Creates an RFQ based on the purchase order.
rfqHtml :: UTCTime -> UUID -> PurchaseOrder -> IO (Either APIError LBS.ByteString)
rfqHtml ct uuid po = passSuccess $ renderBS $ do
  doctype_
  html_ [lang_ "en"] $ do
    genericHeader uuid "RFQ" "doc_gen.css"
    body_ $ do
      div_ [class_ "gcont h10"] $ do
        div_ [class_ "lal"] $ do
          img_ [class_ "logo", src_ "https://erp.erp.com/logo.png", alt_ "hoddr Company"]
        div_ [class_ "lal col", style_ "grid-column: 2 / 6"] $ do
          sequence_ erpHeaderAddress
        div_ [class_ "ral col", style_ "grid-column: 7 / 9"] $ do
          h3_ $ toHtmlFromText "RFQ"
          p_ $ toHtmlFromText $ formatDate ct -- set to current date by default

      div_ [class_ "gcont h15"] $ do
        div_ [class_ "lal col", style_ "grid-column: 1 / 4"] $ do
          h4_ $ toHtmlFromText "Ship To:"
          p_ $ toHtmlFromText "hoddr Co"
          p_ $ toHtmlFromText "4005 Blvd. SE"
          p_ $ toHtmlFromText "Grand, MI 49000"

      div_ [class_ "gcont-po-items"] $ do
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Part No."
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Qty"
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "U/M"
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Description"
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Rate"
        h4_ [class_ "brd-top brd-bot brd-left brd-right talign-center small-font bgfill"] $ toHtmlFromText "Amount"

        sequence_ $ genPOLineItems (reverse $ po_items po)

        div_ [class_ "brd-top"] $ toHtmlFromText ""
        div_ [class_ "brd-top"] $ toHtmlFromText ""
        div_ [class_ "brd-top"] $ toHtmlFromText ""
        div_ [class_ "brd-top"] $ toHtmlFromText ""
        h4_ [class_ "brd-top brd-left brd-bot talign-right"] $ toHtmlFromText "Total"
        p_ [class_ "brd-top brd-left brd-right brd-bot talign-right small-font"] $ toHtmlFromText $ "$" <> formatn 2 poTotal

      div_ [class_ "gcont-footer h10"] $ do
        div_ [] $ toHtmlFromText ""
        p_ [class_ "talign-center small-font"] $ toHtmlFromText "Please include freight charges if applicable."
        div_ [] $ toHtmlFromText ""
  where poTotal = getPOTotal $ po_items po

-- | Creates a purchase order template.
poHtml :: UTCTime -> UUID -> PurchaseOrder -> IO (Either APIError LBS.ByteString)
poHtml ct uuid po = passSuccess $ renderBS $ do
  doctype_
  html_ [lang_ "en"] $ do
    genericHeader uuid ("P.O. #: " <> toTextFromInt (po_id po)) "doc_gen.css"
    body_ $ do
      div_ [class_ "gcont h10"] $ do
        div_ [class_ "lal"] $ do
          img_ [class_ "logo", src_ "https://erp.erp.com/logo.png", alt_ "hoddr Company"]
        div_ [class_ "lal col", style_ "grid-column: 2 / 6"] $ do
          sequence_ erpHeaderAddress
        div_ [class_ "ral col", style_ "grid-column: 7 / 9"] $ do
          h3_ $ toHtmlFromText "Purchase Order"
          p_ $ toHtmlFromText $ formatDate ct -- set to current date by default
          p_ $ toHtmlFromText $ "P.O. #: " <> po_number po

      div_ [class_ "gcont h15"] $ do
        div_ [class_ "lal col", style_ "grid-column: 1 / 4"] $ do
          h4_ $ toHtmlFromText "Vendor"
          p_ $ toHtmlFromText $ vdname $ po_vendor po
          p_ $ toHtmlFromText "{{Customer Address Line 1}}"
          p_ $ toHtmlFromText "{{Customer Address Line 2}}"
        div_ [class_ "lal col", style_ "grid-column: 4 / 7"] $ do
          h4_ $ toHtmlFromText "Ship To:"
          p_ $ toHtmlFromText "hoddr Co"
          p_ $ toHtmlFromText "4005 Blvd. SE"
          p_ $ toHtmlFromText "Grand, MI 49000"
        div_ [class_ "lal col", style_ "grid-column: 7/10"] $ do
          p_ $ toHtmlFromText "Vendor Fax: {{vendor fax}}"
          p_ $ toHtmlFromText "Ship Via: {{ship via}}"
          p_ $ toHtmlFromText "Expected: {{po_expectedReceiptDate}}"
          p_ $ toHtmlFromText "FOB {{fob}}"

      div_ [class_ "gcont-po-items"] $ do
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Part No."
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Qty"
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "U/M"
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Description"
        h4_ [class_ "brd-top brd-bot brd-left talign-center small-font bgfill"] $ toHtmlFromText "Rate"
        h4_ [class_ "brd-top brd-bot brd-left brd-right talign-center small-font bgfill"] $ toHtmlFromText "Amount"

        sequence_ $ genPOLineItems (reverse $ po_items po)

        div_ [class_ "brd-top"] $ toHtmlFromText ""
        div_ [class_ "brd-top"] $ toHtmlFromText ""
        div_ [class_ "brd-top"] $ toHtmlFromText ""
        div_ [class_ "brd-top"] $ toHtmlFromText ""
        h4_ [class_ "brd-top brd-left brd-bot talign-right"] $ toHtmlFromText "Total"
        p_ [class_ "brd-top brd-left brd-right brd-bot talign-right small-font"] $ toHtmlFromText $ "$" <> formatn 2 poTotal

      div_ [class_ "gcont-footer h10"] $ do
        div_ [] $ toHtmlFromText ""
        p_ [class_ "talign-center small-font"] $ toHtmlFromText "Please confirm pricing and ship date"
        div_ [] $ toHtmlFromText ""
  where poTotal = getPOTotal $ po_items po

-- | Helper to get purchase order sum.
getPOTotal :: [POItem] -> Double
getPOTotal = foldl' (\acc poi -> acc + (poi_quantity poi * poi_purchasePriceEach poi)) 0.00

-- | Generator for purchase order line item table.
genPOLineItems :: Monad m => [POItem] -> [HtmlT m ()]
genPOLineItems = foldl' (\acc poi ->
  [ p_ [class_ "brd-left small-font line"] $ toHtmlFromText $ poi_name poi
  , p_ [class_ "brd-left small-font talign-right line"] $ toHtmlFromDbl $ poi_quantity poi
  , p_ [class_ "brd-left small-font line"] $ toHtmlFromText $ formatUnitsPerQuantity $ poi_unitsPerQuantity poi
  , p_ [class_ "brd-left small-font line"] $ toHtmlFromText $ poi_desc poi
  , p_ [class_ "brd-left talign-right small-font line"] $ toHtmlFromText $ formatn 4 $ poi_purchasePriceEach poi
  , p_ [class_ "brd-left brd-right talign-right small-font line"] $ toHtmlFromText $ formatn 2 (poi_purchasePriceEach poi * poi_quantity poi)
  ] ++ acc) []

-- | Creates a report for the pricing list.
pricingListHtml :: UUID -> PricingList -> IO (Either APIError LBS.ByteString)
pricingListHtml uuid pl = passSuccess $ renderBS $ do
  doctype_
  html_ [lang_ "en"] $ do
    genericHeader uuid "Pricing List" "doc_gen.css"
    body_ $ do
      div_ [class_ "gcont h10"] $ do
        div_ [class_ "lal"] $ do
          img_ [class_ "logo", src_ "https://erp.erp.com/logo.png", alt_ "hoddr Company"]
        div_ [class_ "lal col", style_ "grid-column: 2 / 6"] $ do
          sequence_ erpHeaderAddress
        div_ [class_ "ral col", style_ "grid-column: 7 / 9"] $ do
          h3_ $ toHtmlFromText "Pricing List"
          p_ $ toHtmlFromText $ "Valid From: " <> formatDate (pl_effectiveAsOf pl)
          p_ $ toHtmlFromText $ "Valid Until: " <> formatDate (pl_effectiveUntil pl)
          p_ $ toHtmlFromText "All pricing is confidential."

      div_ [class_ "gcont-pricing-list-items"] $ do
        h4_ [class_ "brd-top brd-bot brd-left talign-center bgfill"] $ toHtmlFromText "Name"
        h4_ [class_ "brd-top brd-bot brd-left talign-center bgfill"] $ toHtmlFromText "Description"
        h4_ [class_ "brd-top brd-bot brd-left brd-right talign-center bgfill"] $ toHtmlFromText "Fixed Price"

        sequence_ $ genPricingListItems (reverse $ pl_items pl)

        div_ [class_ "brd-top"] $ toHtmlFromText ""
        div_ [class_ "brd-top"] $ toHtmlFromText ""
        div_ [class_ "brd-top"] $ toHtmlFromText ""

      div_ [class_ "gcont-footer h10"] $ do
        div_ [] $ toHtmlFromText ""
        p_ [class_ "talign-center small-font"] $ toHtmlFromText "All pricing is confidential."
        div_ [] $ toHtmlFromText ""

-- | Generator for pricing list item table.
genPricingListItems :: Monad m => [FixedPriceItem] -> [HtmlT m ()]
genPricingListItems = foldl' (\acc fpi ->
  [ p_ [class_ "brd-left plline"] $ toHtmlFromText $ fpi_refName fpi
  , p_ [class_ "brd-left plline"] $ toHtmlFromText $ fpi_refDescription fpi
  , p_ [class_ "brd-left brd-right talign-right plline"] $ toHtmlFromText $ formatn 2 (fpi_fixedPrice fpi)
  ] ++ acc) []

-- | Generates the project estimate cover page report.
projectHtml :: UTCTime -> UUID -> Quote -> IO (Either APIError LBS.ByteString)
projectHtml ct uuid _ = passSuccess $ renderBS $ do
  doctype_
  html_ [lang_ "en"] $ do
    genericHeader uuid "Project Estimate" "doc_gen.css"
    body_ $ do
      div_ [class_ "gcont h10"] $ do
        div_ [class_ "lal"] $ do
          img_ [class_ "logo", src_ "https://erp.erp.com/logo.png", alt_ "hoddr Company"]
        div_ [class_ "lal col", style_ "grid-column: 2 / 7"] $ do
          sequence_ erpHeaderAddress

      div_ [class_ "gcont h10"] $ do
        div_ [class_ "lal col", style_ "grid-column: 1 / 4"] $ do
          h5_ $ toHtmlFromText "Company: <customer-name>"
          h5_ $ toHtmlFromText "Contact: <contact name>"
          h5_ $ toHtmlFromText "Address: <first-line>"
          h5_ $ toHtmlFromText "City: <city>"
          h5_ $ toHtmlFromText "State Zip: <MI #####>"
          h5_ $ toHtmlFromText "Phone: (###) ###-####"
        div_ [class_ "lal col", style_ "grid-column: 5 / 8"] $ do
          h5_ $ toHtmlFromText "Project ID: <quote_number>"
          h5_ $ toHtmlFromText "Project Name: <project_name>"
          h5_ $ toHtmlFromText "Project Address: <project_address>"
          h5_ $ toHtmlFromText "City: <city>"
          h5_ $ toHtmlFromText "State Zip: <State #####>"

      div_ [class_ "gcont-est-summary"] $ do
        div_ [] $ toHtmlFromText ""
        h3_ [class_ "talign-center"] $ toHtmlFromText "Estimate Summary"
        div_ [] $ toHtmlFromText ""

        h4_ [class_ "talign-center brd-top brd-left brd-bot bgfill"] $ toHtmlFromText "Rectangular"
        h4_ [class_ "talign-center brd-top brd-left brd-bot bgfill"] $ toHtmlFromText "Round"
        h4_ [class_ "talign-center brd-top brd-left brd-bot brd-right bgfill"] $ toHtmlFromText "Oval"

        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Total Linear Ft.: 1458"
        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Total Linear Ft.: 615"
        p_ [class_ "small-font brd-right brd-left"] $ toHtmlFromText "Total Linear Ft.: 1360"

        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Linear Ft. of Duct: 963"
        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Linear Ft. of Pipe: 483"
        p_ [class_ "small-font brd-right brd-left"] $ toHtmlFromText "Linear Ft. of Pipe: 998"

        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Qty. of Fittings: 291"
        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Qty. of Fittings: 100"
        p_ [class_ "small-font brd-right brd-left"] $ toHtmlFromText "Qty. of Fittings: 112"

        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Total Lbs.: 9001"
        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Total Lbs.: 1309"
        p_ [class_ "small-font brd-right brd-left"] $ toHtmlFromText "Total Lbs.: 18642"

        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Total Square Ft.: 8061"
        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Total Square Ft.: 1413"
        p_ [class_ "small-font brd-right brd-left"] $ toHtmlFromText "Total Square Ft.: 11537"

        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Square Ft. of Liner: 183"
        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Square Ft. of Liner: 0"
        p_ [class_ "small-font brd-right brd-left"] $ toHtmlFromText "Square Ft. of Liner: 0"

        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Total Sq-Ft of Double Wall: 0"
        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Total Sq-Ft of Double Wall: 0"
        p_ [class_ "small-font brd-right brd-left"] $ toHtmlFromText "Total Sq-Ft of Double Wall: 0"

        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Qty. of Volume Dampers: 4"
        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Qty. of Volume Dampers: 4"
        p_ [class_ "small-font brd-right brd-left"] $ toHtmlFromText "Qty. of Volume Dampers: 0"

        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Qty. of Flex Connectors: 0"
        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Qty. of Flex Connectors: 0"
        p_ [class_ "small-font brd-right brd-left"] $ toHtmlFromText "Qty. of Flex Connectors: 3"

        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Qty. of Grille Boxes: 7"
        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Qty. of Rect. Saddle Taps: 7"
        p_ [class_ "small-font brd-right brd-left"] $ toHtmlFromText "Qty. of Rect. Saddle Taps: 0"

        p_ [class_ "small-font brd-left"] $ toHtmlFromText "HETO(s): 25"
        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Linear Ft. of L.S. pipe: 0"
        p_ [class_ "small-font brd-right brd-left"] $ toHtmlFromText "Linear Ft. of L.S. pipe: 0"

        h5_ [class_ "small-font brd-left brd-bot brd-top"] $ toHtmlFromText "Rectangular Price: $29,835.05"
        h5_ [class_ "small-font brd-left brd-bot brd-top"] $ toHtmlFromText "Round Price: $4,683.96"
        h5_ [class_ "small-font brd-left brd-bot brd-right brd-top"] $ toHtmlFromText "Oval Price: $77,127.55"

      div_ [class_ "gcont-est-summary"] $ do
        div_ [] $ toHtmlFromText ""
        h3_ [class_ "talign-center"] $ toHtmlFromText "Base Bid"
        div_ [] $ toHtmlFromText ""

        p_ [class_ "small-font brd-left brd-top"] $ toHtmlFromText "Total of Rectangular, Round, and Oval Duct Fittings"
        p_ [class_ "small-font brd-top"] $ toHtmlFromText ""
        p_ [class_ "small-font brd-top brd-right talign-right"] $ toHtmlFromText "$34,519.01"

        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Optional Accessories and Catalog Items"
        p_ [] $ toHtmlFromText ""
        p_ [class_ "small-font brd-right talign-right"] $ toHtmlFromText "$1001.61"

        p_ [class_ "small-font brd-left"] $ toHtmlFromText "Freight"
        p_ [] $ toHtmlFromText ""
        p_ [class_ "small-font brd-right talign-right"] $ toHtmlFromText "$275.00"

        p_ [class_ "small-font brd-left brd-bot"] $ toHtmlFromText "Full Straight Truck(s)"
        p_ [class_ "small-font brd-bot"] $ toHtmlFromText ""
        p_ [class_ "small-font brd-bot brd-right talign-right"] $ toHtmlFromText "5"

        p_ [class_ "brd-left brd-bot"] $ toHtmlFromText ""
        h4_ [class_ "brd-bot brd-right talign-right"] $ toHtmlFromText "Total Estimate"
        h4_ [class_ "brd-bot brd-right talign-right"] $ toHtmlFromText "$35,795.63"

      div_ [class_ "gcont-est-summary"] $ do
        div_ [] $ toHtmlFromText ""
        h3_ [class_ "talign-center"] $ toHtmlFromText "Alternates"
        div_ [] $ toHtmlFromText ""

        p_ [class_ "small-font brd-left brd-top"] $ toHtmlFromText "Add to toal for LEED covering"
        p_ [class_ "small-font brd-top"] $ toHtmlFromText ""
        p_ [class_ "small-font brd-top brd-right talign-right"] $ toHtmlFromText "$1,380.76"

        p_ [class_ "brd-left brd-bot"] $ toHtmlFromText ""
        p_ [class_ "brd-bot"] $ toHtmlFromText ""
        p_ [class_ "brd-bot brd-right"] $ toHtmlFromText ""

      div_ [class_ "gcont-est-summary"] $ do
        div_ [class_ "lal col", style_ "grid-column: 1 / 3"] $ do
          p_ [class_ "small-font"] $ toHtmlFromText "1. Sales tax and use tax are NOT included."
          p_ [class_ "small-font"] $ toHtmlFromText "2. Estimate is firm for 30 day(s) and then subject to recalculation based on current material pricing."
          p_ [class_ "small-font"] $ toHtmlFromText "3. Payment terms are Net 30 from invoice date of each release. No retainages allowed."
          p_ [class_ "small-font"] $ toHtmlFromText "4. Lead time subject to backlog after credit approval and acceptance of PO."

      div_ [class_ "gcont-est-summary page-break"] $ do
        p_ [class_ "small-font"] $ toHtmlFromText "Estimator: Ryan Coe"
        p_ [class_ "small-font talign-center"] $ toHtmlFromText $ "Date: " <> formatDate ct
        p_ [class_ "small-font talign-right"] $ toHtmlFromText ""

-- | Generates a packing slip for the order (sans prices).
packingSlipHtml :: UTCTime -> UUID -> Order -> IO (Either APIError LBS.ByteString)
packingSlipHtml ct uuid o = passSuccess $ renderBS $ do
  doctype_
  html_ [lang_ "en"] $ do
    genericHeader uuid ("Order #: " <> onum o) "doc_gen.css"
    body_ $ do
      div_ [class_ "gcont h10"] $ do
        div_ [class_ "lal"] $ do
          img_ [class_ "logo", src_ "https://erp.erp.com/logo.png", alt_ "hoddr Company"]
        div_ [class_ "lal col", style_ "grid-column: 2 / 6"] $ do
          sequence_ erpHeaderAddress
        div_ [class_ "ral col", style_ "grid-column: 7 / 9"] $ do
          h3_ $ toHtmlFromText "Packing Slip"
          p_ $ toHtmlFromText $ formatDate (fromMaybe ct $ ocreated o) -- set to current date by default if created not set
          p_ $ toHtmlFromText $ "Order #: " <> onum o

      div_ [class_ "gcont h15"] $ do
        div_ [class_ "lal col", style_ "grid-column: 1 / 4"] $ do
          h4_ $ toHtmlFromText "Customer:"
          p_ $ toHtmlFromText $ cname $ ocust o
          h4_ $ toHtmlFromText "PO / Job Name: "
          p_ $ toHtmlFromText $ opo o

      div_ [class_ "gcont-footer"] $ do
        h4_ [class_ "brd-top brd-bot brd-left talign-left small-font bgfill"] $ toHtmlFromText "Qty"
        h4_ [class_ "brd-top brd-bot brd-left talign-left small-font bgfill"] $ toHtmlFromText "Item"
        h4_ [class_ "brd-top brd-bot brd-left brd-right talign-left small-font bgfill"] $ toHtmlFromText "Description"

        sequence_ $ genSlipLineItems (reverse $ olineItems o)

        div_ [class_ "brd-top"] $ toHtmlFromText ""
        div_ [class_ "brd-top"] $ toHtmlFromText ""
        div_ [class_ "brd-top"] $ toHtmlFromText ""

-- | Generates packing slip item table section.
genSlipLineItems :: Monad m => [LineItem] -> [HtmlT m ()]
genSlipLineItems = foldl' (\acc li ->
  [ p_ [class_ "brd-left small-font plline"] $ toHtmlFromText $ formatn 0 $ lquant li
  , p_ [class_ "brd-left small-font plline"] $ toHtmlFromText $ lcategory li
  , p_ [class_ "brd-left brd-right small-font plline"] $ toHtmlFromText $ ldesc li
  ] ++ acc) []

-- | Helper to generate the header for each report.
genericHeader :: Monad m => UUID -> Text -> Text -> HtmlT m ()
genericHeader uuid tit cssSheet = head_ $ do
  meta_ [charset_ "utf-8"]
  meta_ [name_ "viewport", content_ "width: device-width"]
  title_ $ toHtmlFromText tit
  link_ [rel_ "stylesheet", type_ "text/css", href_ ("https://erp.erp.com/css/" <> cssSheet <> "?" <> toText uuid)]
  link_ [rel_ "icon", type_ "image/x-icon", href_ "https://erp.erp.com/favicon.ico"]

-- | Helper for type conversion.
toHtmlFromText :: Monad m => Text -> HtmlT m ()
toHtmlFromText = toHtml

-- | Helper for type conversion.
toHtmlFromDbl :: Monad m => Double -> HtmlT m ()
toHtmlFromDbl = toHtmlFromText . toTextFromDbl

-- | Helper for type conversion.
toTextFromDbl :: Double -> Text
toTextFromDbl = T.pack . show

-- | Helper for type conversion.
toTextFromInt :: Int -> Text
toTextFromInt = T.pack . show

-- | Helper for formatting and rounding doubles.
ceilingn :: Integer -> Double -> Double
ceilingn n num = (fromIntegerIn . ceiling $ num * fac) / fac
  where fac = 10^n
        fromIntegerIn :: Integer -> Double
        fromIntegerIn = fromIntegral

-- | Helper for formatting doubles.
formatn :: Integer -> Double -> Text
formatn n = commaFormatter (fromInteger n) "" . T.pack . printf ("%." <> show n <> "f") . ceilingn n

-- | Final format double to include commas as expected.
commaFormatter :: Int -> Text -> Text -> Text
commaFormatter decims new rest
  | decims == 0 = if T.length rest > 3
                     then commaFormatter 0 ("," <> T.takeEnd 3 rest <> new) (T.dropEnd 3 rest)
                     else rest <> new
  | otherwise = commaFormatter 0 (T.takeEnd (decims + 1) rest <> new) (T.dropEnd (decims + 1) rest)

-- | Helper for formatting maybe double for units per quantity.
formatUnitsPerQuantity :: Maybe Double -> Text
formatUnitsPerQuantity Nothing = ""
formatUnitsPerQuantity (Just 1.0) = "ea"
formatUnitsPerQuantity (Just d) = toTextFromDbl d

formatAddressL2 :: Address -> Text
formatAddressL2 a = case add_street2 a of
                         Nothing -> formatCityLine a
                         Just "" -> formatCityLine a
                         Just s2 -> s2

formatAddressL3 :: Address -> Text
formatAddressL3 a = case add_street2 a of
                         Nothing -> ""
                         Just "" -> ""
                         Just _ -> formatCityLine a

formatCityLine :: Address -> Text
formatCityLine a = add_city a <> ", " <> add_state a <> " " <> add_zip a

getTaxableTotal :: [LineItem] -> Double
getTaxableTotal = foldl' (\acc li -> if lcategory li /= "Freight" && lcategory li /= "Labor"
                                        then lprice li * lquant li + acc
                                        else acc
                         ) 0.00

-- | Helper to reduce boiler-plate.
erpHeaderAddress :: Monad m => [HtmlT m ()]
erpHeaderAddress = [ h3_ $ toHtmlFromText "hoddr Company"
                   , h4_ $ toHtmlFromText "4005 Blvd SE"
                   , h4_ $ toHtmlFromText "Grand, MI 49000"
                   , p_ $ toHtmlFromText "P: (101) 111-3121  F: (101) 111-4990"
                   ]
