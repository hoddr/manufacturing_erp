{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Pricing
Description: Handles pricing of all items coming from CAMDuct.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Main pricing module, dictating core logic for pricing different fittings, purchase items, assemblies,
and much more. Take great care modifying the logic here, as they could have immediate and drastic impact on the business's bottom line!

Markups are based on ending up with a 35% margin for AJE and Pleune level customers. As these customers are NOT the majority,
the customer specific markup was left to default at 1.00 for standard level customers. There is external documentation detailing
the math, but this sets ERP base level markups at a factor of 1.41 and customer markup to 0.96 for these customers, getting the
desired 35% margin. Distributors are lower yet. Cash customers have a customer markup above 1.00 to provide incentive to open
accounts as well as to cover the costs of billing via credit cards and the like.

-}

module Pricing
  ( adjustInventory
  , getMarkupFlag
  , priceViaList
  , runEstimator
  , sanitizeRawLineItems
  , undoInventoryAdjustments
  )
  where

import Control.Monad ( forM )
import Data.Either ( lefts
                   , rights
                   )
import Data.List ( foldl' )
import Data.Maybe ( fromMaybe )
import Data.Text ( Text
                 , splitOn
                 , unpack
                 )
import qualified Data.Text as T
import Data.Text.Encoding ( encodeUtf8 )
import Database.PostgreSQL.Simple ( Query )
import Network.HTTP.Types ( Status(..) )
import Prelude hiding ( pi )
import Text.Read ( readEither )


-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , Assembly(..)
                , Context(..)
                , Customer(..)
                , FabItem(..)
                , FixedPriceItem(..)
                , LaborRate(..)
                , LineItem(..)
                , Markup(..)
                , Material(..)
                , MatInfo(..)
                , PricingList(..)
                , PurchaseItem(..)
                , RawLineItem(..)
                , SubItem(..)
                , fabInv
                , materialInv
                , purchaseInv
                )
import Assemblies ( getAssemblyCosts
                  , searchAssembliesByName
                  )
import Customers ( getCustomerByName )
import Database ( runQuery )
import Inventory ( addMaterialWeight
                 , addPurchase
                 , addStock
                 , adjustMatById
                 , subtractMaterialWeight
                 , subtractPurchase
                 , subtractStock
                 )
import qualified FabItems as FAB
import qualified Materials as MAT
import qualified Markups as MKUP
import PricingLists ( getPricingListById )
import qualified PurchaseItems as PUR
import Utils ( apiError400
             , apiError404
             , badReq
             , flattenErrors
             , fst5
             , eitherPassErr
             , passOnError
             , passSuccess
             , wasFound
             )


-- | Synonym for customer name.
type CustomerName = Text

-- | Synonym for order number.
type OrderNumber = Text

-- | Synonym for purchase order number.
type PO = Text


-- FUNCTIONS
-- | Transformer for moving 'RawLineItem' to an internal 'LineItem'.
sanitizeRawLineItems :: Bool
                     -> [(RawLineItem, Maybe MatInfo, Maybe MatInfo, Maybe MatInfo, Maybe Double)]
                     -> ([LineItem], CustomerName, OrderNumber, PO)
sanitizeRawLineItems _ [] = ([], "", "", "")
sanitizeRawLineItems isProgressiveBill rlis@((rli, _, _, _, _):_) =
  let
    lis = map (\
     (r, mmi, mlinMat, mskinMat, mLaborCost) ->
       LineItem { lid = 0
                , ldesc = rli_name r
                , lquant = rli_quantity r
                , llabor = fromMaybe 0.00 (rli_labor r)
                , llaborCost = mLaborCost
                , lweight = Just $ rli_weight r
                , lisExtra = False
                , lprice = rli_priceEach r
                , lorderId = Nothing
                , lorderNumber = Nothing
                , lquoteId = Nothing
                , lquoteNumber = Nothing
                , lmatId = case mmi of
                                Nothing -> Nothing
                                Just (MatInfo (i, _, _, _)) -> Just i
                , lmatName = case mmi of
                                  Nothing -> Nothing
                                  Just (MatInfo (_, n, _, _)) -> Just n
                , lmatWeight = case mmi of
                                    Nothing -> Nothing
                                    Just (MatInfo (_, _, w, _)) -> Just w
                , lmatCost = case mmi of
                                  Nothing -> Nothing
                                  Just (MatInfo (_, _, _, c)) -> Just c
                , llinerId = case mlinMat of
                                  Nothing -> Nothing
                                  Just (MatInfo (i, _, _, _)) -> Just i
                , llinerName = case mlinMat of
                                    Nothing -> Nothing
                                    Just (MatInfo (_, n, _, _)) -> Just n
                , llinerArea = case mlinMat of
                                    Nothing -> Nothing
                                    Just (MatInfo (_, _, a, _)) -> Just a
                , llinerCost = case mlinMat of
                                    Nothing -> Nothing
                                    Just (MatInfo (_, _, _, c)) -> Just c
                , lskinId = case mskinMat of
                                 Nothing -> Nothing
                                 Just (MatInfo (i, _, _, _)) -> Just i
                , lskinName = case mskinMat of
                                   Nothing -> Nothing
                                   Just (MatInfo (_, n, _, _)) -> Just n
                , lskinWeight = case mskinMat of
                                     Nothing -> Nothing
                                     Just (MatInfo (_, _, w, _)) -> Just w
                , lskinCost = case mskinMat of
                                   Nothing -> Nothing
                                   Just (MatInfo (_, _, _, c)) -> Just c
                , laccessoryCost = Just $ rli_accessoryCost r
                , lcategory = rli_category r
                , llength = rli_length r
                , lquantFabbed = 0.00
                , lisFabbed = not isProgressiveBill
                }
              ) rlis
    po = rli_po rli
  in (lis, rli_customer rli, rli_orderNumber rli, po)

-- only for leeds flag now!
-- | Retrieves markup flag(s) from the 'RawLineItem' list.
getMarkupFlag :: [RawLineItem] -> Bool
getMarkupFlag [] = False -- default
getMarkupFlag (rli:_) = rli_isWrapped rli

{- |

Main pricing function and core pricing logic. Each external raw line item gets filtered into the pricing
categories (see getPrice). Customer specific markup is applied after each item is priced. Finally,
if the order/quote is wrapped, LEEDS will be calculated and attached as a Misc Charge line item.

-}
runEstimator :: Context
             -> Text
             -> [RawLineItem]
             -> IO (Either APIError [(RawLineItem, Maybe MatInfo, Maybe MatInfo, Maybe MatInfo, Maybe Double)])
runEstimator _ _ [] = badReq 400 "No line items to price"
runEstimator ctx ordType rlis@(rli:_) = let isWrapped = getMarkupFlag rlis in do
  ecustomer <- getCustomerByName ctx (rli_customer rli)
  emkups <- MKUP.getLatestMarkupRates ctx
  case (ecustomer, emkups) of
       (Left e, _) -> passOnError e
       (_, Left e) -> passOnError e
       (Right cust, Right mkups) -> do
         epricedRlis <- forM rlis (getPrice ctx)
         case lefts epricedRlis of
              [] -> passSuccess (map (addCustMarkup (cmarkup cust)) $ rights epricedRlis)
                >>= eitherPassErr (passSuccess . map (addOrderTypeMarkup ordType mkups))
                >>= eitherPassErr (\prlis -> if isWrapped
                                                then passSuccess $ attachLeeds prlis
                                                else passSuccess prlis)
              ls -> print ls >> badReq 400 "Unable to price line item(s). Check formatting/item format or contact system admin."

-- | Simple helper fn to multiply each item price by the order type specific markup factor QL, QP, FS).
addOrderTypeMarkup :: Text
                   -> Markup
                   -> (RawLineItem, Maybe MatInfo, Maybe MatInfo, Maybe MatInfo, Maybe Double)
                   -> (RawLineItem, Maybe MatInfo, Maybe MatInfo, Maybe MatInfo, Maybe Double)
addOrderTypeMarkup ordType mkups (rli, m1, m2, m3, m4) =
  case ordType of
       "QL" -> (rli { rli_priceEach = rli_priceEach rli * mkupquote mkups }, m1, m2, m3, m4)
       "QP" -> (rli { rli_priceEach = rli_priceEach rli * mkupproject mkups }, m1, m2, m3, m4)
       "FS" -> (rli { rli_priceEach = rli_priceEach rli * mkuporder mkups }, m1, m2, m3, m4)
       _ -> (rli, m1, m2, m3, m4)

-- | Simple helper fn to multiply each item price by the customer specific markup factor.
addCustMarkup :: Double
              -> (RawLineItem, Maybe MatInfo, Maybe MatInfo, Maybe MatInfo, Maybe Double)
              -> (RawLineItem, Maybe MatInfo, Maybe MatInfo, Maybe MatInfo, Maybe Double)
addCustMarkup cmark (rli, m1, m2, m3, m4) =
  if rli_isCatalogue rli
     then (rli, m1, m2, m3, m4)
     else (rli { rli_priceEach = rli_priceEach rli * cmark }, m1, m2, m3, m4)

-- | Helper to sum up price of all line items to calculate the required LEEDS charge (4% surcharge).
-- Adds as a separate line item.
attachLeeds :: [(RawLineItem, Maybe MatInfo, Maybe MatInfo, Maybe MatInfo, Maybe Double)]
            -> [(RawLineItem, Maybe MatInfo, Maybe MatInfo, Maybe MatInfo, Maybe Double)]
attachLeeds [] = []
attachLeeds prlis =
  let baseRli = fst5 $ head prlis
      -- ensure that freight and pure labor are not counted toward LEEDS cost
      tot = foldr (\(rli, _, _, _, _) acc -> if rli_category rli /= "Freight" && rli_category rli /= "Labor"
                                                then rli_quantity rli * rli_priceEach rli + acc
                                                else acc
                  ) 0.00 prlis
      toAdd = (baseRli { rli_category = "Misc Charge"
                       , rli_quantity = 1.0
                       , rli_name = "LEEDS"
                       , rli_weight = 0.00
                       , rli_priceEach = tot * 0.04
                       , rli_isCatalogue = False
                       , rli_labor = Nothing
                       }, Nothing, Nothing, Nothing, Nothing)
  in
    toAdd : prlis

{- |

Core logic for sending raw line items to the correct pricing logic function.

-}
getPrice :: Context
         -> RawLineItem
         -> IO (Either APIError (RawLineItem, Maybe MatInfo, Maybe MatInfo, Maybe MatInfo, Maybe Double))
getPrice ctx rli =
  case rli_category rli of
       "Rectangular Duct" ->
         priceCustomFitting ctx (getRectMat (rli_material rli) (rli_gauge rli))
                                (rli_linerMaterial rli)
                                (rli_skinGauge rli, rli_skinMaterial rli)
                                rli
       "Round Duct" ->
         priceCustomFitting ctx (getRoundMat (rli_name rli) (rli_material rli) (rli_gauge rli))
                                (rli_linerMaterial rli)
                                (rli_skinGauge rli, rli_skinMaterial rli)
                                rli
       "Oval Duct" ->
         priceCustomFitting ctx (getOvalMat (rli_name rli) (rli_material rli) (rli_gauge rli))
                                (rli_linerMaterial rli)
                                (rli_skinGauge rli, rli_skinMaterial rli)
                                rli
       "Stock Pull" -> searchFabItemByName ctx (rli_name rli)
         >>= eitherPassErr (\fab -> passSuccess
                                  (rli { rli_priceEach = checkForCatalog rli (fiprice fab)
                                       , rli_labor = Just $ filabor fab
                                       }
                                  , Just $ MatInfo (fimaterialId fab, fimaterialName fab, fiunitQuantity fab, fimaterialCost fab)
                                  , Nothing
                                  , Nothing
                                  , Just $ filaborCost fab
                                  )
                           )
       "Warm Air Stock" -> do
         epi <- searchPurchasesByName ctx (rli_name rli)
         case epi of
              (Left e) -> passOnError e
              (Right (pi:_)) -> passSuccess (rli { rli_priceEach = checkForCatalog rli (piprice pi) }, Nothing, Nothing, Nothing, Nothing)
              (Right []) -> passSuccess (rli, Nothing, Nothing, Nothing, Nothing)
       "Purchased" -> do
         epi <- searchPurchasesByName ctx (rli_name rli)
         case epi of
              (Left e) -> passOnError e
              (Right []) -> passOnError $ apiError404 $ encodeUtf8 $ "Purchase item not found by name: " <> rli_name rli
              (Right (pi:_)) -> passSuccess (rli { rli_priceEach = checkForCatalog rli (piprice pi) }, Nothing, Nothing, Nothing, Nothing)
       "Misc Rectangular" ->
         priceCustomFitting ctx (getRectMat (rli_material rli) (rli_gauge rli))
                                (rli_linerMaterial rli)
                                (rli_skinGauge rli, rli_skinMaterial rli)
                                rli
        >>= eitherPassErr (\(r, m1, m2, m3, m4) ->
          let chaseTopMkup = if T.take 9 (rli_name rli) == "CHASE TOP"
                                then 1.50
                                else 1.00
              basePrice = rli_priceEach r
          in
            passSuccess (r { rli_priceEach = basePrice * chaseTopMkup }, m1, m2, m3, m4))
       "Misc Round Oval" ->
         priceCustomFitting ctx (getRoundMat (rli_name rli) (rli_material rli) (rli_gauge rli))
                                (rli_linerMaterial rli)
                                (rli_skinGauge rli, rli_skinMaterial rli)
                                rli
       "Assembly" -> priceAssemblyHandler ctx rli
       -- DUCT ACCESSORY, MISC CHARGE, FREIGHT are included in the below!
       "Misc Charge" -> passSuccess (rli, Nothing, Nothing, Nothing, Nothing)
       "Freight" -> passSuccess (rli, Nothing, Nothing, Nothing, Nothing)
       "Labor" -> passSuccess (rli, Nothing, Nothing, Nothing, Nothing)
       "Duct Accessory" -> passSuccess (rli, Nothing, Nothing, Nothing, Nothing)
       _ -> print rli >> badReq 400 "Invalid/not supported billing category found - see logs"

{- |

Core logic for pricing shop fittings. Any non-assembly fitting that requires materials and labor should be
priced in this manner. Support for liner and perforated materials is also in place. Accessory costs
are simply summed up in CAM and reported as a value at the moment (e.g. clips, nuts, bolts, vanes, etc).

The labor time is also parsed via 'parseTimeString'. Labor should be calculated to hours.

Note that the fabrication (custom fitting) specific markups are split into different categories.

-}
priceCustomFitting :: Context
                   -> Text
                   -> Text
                   -> (Text, Text)
                   -> RawLineItem
                   -> IO (Either APIError (RawLineItem, Maybe MatInfo, Maybe MatInfo, Maybe MatInfo, Maybe Double))
priceCustomFitting ctx matName linerName (skinGa, skinName) rli = do
  emat <- searchMatByName ctx matName
  emlinerWithFactor <- searchLinerByName ctx linerName
  emskin <- searchSkinByName ctx $ skinName <> " " <> skinGa
  elab <- MKUP.getLatestRate ctx
  emkups <- MKUP.getLatestMarkupRates ctx
  let etime = parseTimeString $ rli_rate rli
  case (emat, emlinerWithFactor, emskin, elab, etime, emkups) of
       (Left e, _, _, _, _, _) -> passOnError e
       (_, Left e, _, _, _, _) -> passOnError e
       (_, _, Left e, _, _, _) -> passOnError e
       (_, _, _, Left e, _, _) -> passOnError e
       (_, _, _, _, Left e, _) -> passOnError e
       (_, _, _, _, _, Left e) -> passOnError e
       (Right mat, Right mliner, Right mskin, Right lab, Right time, Right mkups) ->
         let fn Nothing = (Nothing, 0.00)
             fn (Just (lmat, factor)) = (Just $ MatInfo ( mid lmat
                                                        , mname lmat
                                                        , factor * rli_linerArea rli
                                                        , mcostPerUnit lmat * rli_linerArea rli * factor
                                                        )
                , mcostPerUnit lmat * rli_linerArea rli * factor
                )
             (linerMatInfo, linerCost) = fn mliner
             skinFn Nothing = (Nothing, 0.00)
             skinFn (Just smat) = (Just $ MatInfo ( mid smat
                                                  , mname smat
                                                  , rli_skinWeight rli
                                                  , mcostPerUnit smat * rli_skinWeight rli
                                                  )
                                  , mcostPerUnit smat * rli_skinWeight rli
                                  )
             (skinMatInfo, skinCost) = skinFn mskin
             matCost = rli_materialWeight rli * mcostPerUnit mat
             laborCost = lrrate lab * time
         in
           passSuccess ( rli { rli_priceEach = ( matCost
                                               + laborCost
                                               + linerCost
                                               + skinCost
                                               + rli_accessoryCost rli
                                               )
                                               * mkupfabrication mkups
                             , rli_labor = Just time
                             }
                       , Just $ MatInfo (mid mat, mname mat, rli_materialWeight rli, matCost)
                       , linerMatInfo
                       , skinMatInfo
                       , Just laborCost
                       )

{- |

Logic for pricing assemblies. Base price for assemblies is retrieved as part of querying
for the assembly's data. This primarily retrieves the markups, assembly information, and then
applies the factor. There is also a check for fixed pricing catalogue usage.

-}
priceAssemblyHandler :: Context
                     -> RawLineItem
                     -> IO (Either APIError ( RawLineItem
                                            , Maybe MatInfo
                                            , Maybe MatInfo
                                            , Maybe MatInfo
                                            , Maybe Double
                                            )
                           )
priceAssemblyHandler ctx rli =
  searchAssembliesByName ctx (rli_name rli)
  >>= eitherPassErr (\a -> do
    emkups <- MKUP.getLatestMarkupRates ctx
    ead <- getAssemblyCosts ctx a
    case (emkups, ead) of
         (Left e, _) -> passOnError e
         (_, Left e) -> passOnError e
         (Right mkups, Right assemblyData) -> passSuccess (a, assemblyData, mkups)
  ) >>= eitherPassErr (\(a, (_, matCost, labCost), mkups) ->
    if rli_isCatalogue rli
       then passSuccess (rli, Just $ MatInfo (0, "", 0.00, matCost), Nothing, Nothing, Just labCost)
       else passSuccess ( rli { rli_priceEach = priceAssembly mkups a }
                        , Just $ MatInfo (0, "", 0.00, matCost)
                        , Nothing
                        , Nothing
                        , Just labCost
                        )
  )

{- |

Helper to sum up sub items and labor for assembly base cost.

-}
priceAssembly :: Markup -> Assembly -> Double
priceAssembly mkups a =
  let subItemTot = foldl' (\acc si -> acc + (sub_priceEach si * sub_quantity si)) 0.00 (a_subItems a) in
      mkupassembly mkups * (subItemTot + a_laborPrice a)

-- | Searches for a fabrication item by name and applies pricing. Returns 404 if not found.
searchFabItemByName :: Context
                    -> Text
                    -> IO (Either APIError FabItem)
searchFabItemByName c n =
  runQuery c (FAB.baseSql <> " and fab_items.name = ?") (fabInv, n)
    >>= eitherPassErr (FAB.finishPricing c)
    >>= eitherPassErr (wasFound "Stock fabrication item not found")

-- | Searches for a base material by name. Returns 404 if not found.
searchMatByName :: Context -> Text -> IO (Either APIError Material)
searchMatByName c matName =
  runQuery c (MAT.baseSql <> " and materials.name = ?")
             (materialInv, matName)
  >>= eitherPassErr (wasFound "Material not found.")

-- | Searches for a liner (and double-wall insulation) by name. Returns Nothing if not found.
-- TODO Should this actually cause pricing failure if the liner is not found?
searchLinerByName :: Context -> Text -> IO (Either APIError (Maybe (Material, Double)))
searchLinerByName c ln =
  let (nameToSearch, factor) = case ln of
                                    "DW 1lb x 1.500" -> ("DW 1lb x 1.000", 1.5)
                                    "DW 1lb x 2.000" -> ("DW 1lb x 1.000", 2.0)
                                    _ -> (ln, 1.0)
  in
  searchMatByName c nameToSearch
    >>= either (\e@(APIError st) -> case statusCode st of
                 404 -> passSuccess Nothing
                 _ -> pure (Left e))
               (\lmat -> passSuccess $ Just (lmat, factor))

-- | Searches for perforation by name. Returns Nothing if not found.
-- TODO Should this actually cause pricing failure if the perf is not found (how to know it was expecting to find it?)
searchSkinByName :: Context -> Text -> IO (Either APIError (Maybe Material))
searchSkinByName c n = searchMatByName c n
  >>= either (\e@(APIError st) -> case statusCode st of
               404 -> passSuccess Nothing
               _ -> pure (Left e))
             (passSuccess . Just)

-- | Helper to parse time strings from CAM. Returns hours as `Double`.
parseTimeString :: Text -> Either APIError Double
parseTimeString s = case map unpack $ splitOn ":" s of
                         [hh, mm, ss] -> parseTime hh mm ss
                         [mm, ss] -> parseTime "00" mm ss
                         _ -> Left $ apiError400 "Unable to parse time string"
  where parseTime eh em es = case (readEither eh, readEither em, readEither es) of
                               (Right hrs, Right mins, Right secs) -> Right $ hrs + (mins / 60.0) + (secs / 3600.0)
                               _ -> Left $ apiError400 "Unable to parse time string"

-- | Helper to generate rectangular material names.
getRectMat :: Text -> Text -> Text
getRectMat = buildMatName ""

-- | Helper to generate round material names. take 2 check is needed for CAMDuct, where name comes in with dimension info.
-- Note the STEEL SLIT addition when spiral pipe is confirmed.
getRoundMat :: Text -> Text -> Text -> Text
getRoundMat name mat gauge
  | name == "SP" || T.take 2 name == "SP" = buildMatName "STEEL SLIT" mat gauge
  | otherwise = buildMatName "" mat gauge

-- | Helper to generate oval material names. Take 3 check is needed for CAMDuct, where name comes in with dimension info.
-- Note the STEEL SLIT addition when oval spiral pipe is confirmed.
getOvalMat :: Text -> Text -> Text -> Text
getOvalMat name mat gauge
  | name == "OSP" || T.take 3 name == "OSP" = buildMatName "STEEL SLIT" mat gauge
  | otherwise = buildMatName "" mat gauge

-- | Generic material name builder.
buildMatName :: Text -> Text -> Text -> Text
buildMatName prefix mat gauge
  | prefix == "" = mat <> " " <> gauge
  | otherwise = prefix <> " " <> mat <> " " <> gauge

-- | Helper to search for purchase items by name. TODO does this need an update to return 404 if not found?
searchPurchasesByName :: Context -> Text -> IO (Either APIError [PurchaseItem])
searchPurchasesByName c purName =
  runQuery c (PUR.baseSql <> " and purchase_items.name = ?") (purchaseInv, purName)
  >>= eitherPassErr passSuccess

-- | Main logic to initiate subtraction of materials, purchase items, etc from inventory on order.
-- Note that assemblies are NOT used here, as they should be handled as PGB to first fabricate the assemblies
-- THEN bill and subtract.
adjustInventory :: Context
                -> Text
                -> ([LineItem], Text, Text, Text)
                -> IO (Either APIError ())
adjustInventory ctx ordType (lis, _, _, _)
  | ordType == "PGB" = passSuccess () -- BD/assembly orders are handled elsewhere
  | ordType /= "SP" =
    forM lis (\li -> case lcategory li of
               "Rectangular Duct" -> subtractMaterialWeight ctx li
               "Round Duct" -> subtractMaterialWeight ctx li
               "Oval Duct" -> subtractMaterialWeight ctx li
               "Stock Pull" -> subtractStock ctx li
               "Warm Air Stock" -> subtractPurchase ctx li
               "Purchased" -> subtractPurchase ctx li
               "Misc Rectangular" -> subtractMaterialWeight ctx li
               "Misc Round Oval" -> subtractMaterialWeight ctx li
               "CC" -> subtractMaterialWeight ctx li
               _ -> passSuccess ()
             ) >>= flattenCallback
  | otherwise = forM lis (createStock ctx) >>= flattenCallback

-- | Helper to undo an order, adding back in materials, purchase items, etc upon cancelation.
undoInventoryAdjustments :: Context -> Text -> [LineItem] -> IO (Either APIError ())
undoInventoryAdjustments ctx ordType lis
  | ordType == "PGB" = passSuccess () -- BD/assembly orders are handled elsewhere
  | ordType /= "SP" =
    forM lis (\li -> case lcategory li of
               "Rectangular Duct" -> addMaterialWeight ctx li
               "Round Duct" -> addMaterialWeight ctx li
               "Oval Duct" -> addMaterialWeight ctx li
               "Stock Pull" -> addStock ctx li
               "Warm Air Stock" -> addPurchase ctx li
               "Purchased" -> addPurchase ctx li
               "Misc Rectangular" -> addMaterialWeight ctx li
               "Misc Round Oval" -> addMaterialWeight ctx li
               "CC" -> addMaterialWeight ctx li
               _ -> passSuccess ()
             ) >>= flattenCallback
  | otherwise = forM lis (deleteStock ctx) >>= flattenCallback

-- | Helper to adjust inventory for stock orders (e.g. internal stock generation orders).
-- This can ONLY support orders that have all stock items.
adjustStock :: Query
            -> (Context -> LineItem -> IO (Either APIError()))
            -> Context
            -> LineItem
            -> IO (Either APIError ())
adjustStock op stockFn ctx li =
  case lcategory li of
       "Stock Pull" ->
         FAB.getByName ctx (ldesc li)
         >>= eitherPassErr (\f ->
           adjustMatById ctx op (fimaterialId f) (lquant li) (lweight li))
         >>= eitherPassErr (\_ -> stockFn ctx li)
       _ -> badReq 400 "Stock push orders must not have non-stock items"

-- | Wrapper to create stock, thus removing the base materials required from inventory and adding stock.
createStock :: Context -> LineItem -> IO (Either APIError ())
createStock = adjustStock "-" addStock

-- | Wrapper to delete stock, thus adding back base materials required to inventory and subtracting stock.
deleteStock :: Context -> LineItem -> IO (Either APIError ())
deleteStock = adjustStock "+" subtractStock

{- |

Core logic for pricing orders against a fixed pricing list. Basically checks if item is present in list;
if yes, uses the price list with no extra markup. If not, prices like normal. Note that the
is pricing list item flag has to be set to ensure that customer markup doesn't adjust the fixed price.

-}
priceViaList :: Context
             -> Int
             -> [RawLineItem]
             -> IO (Either APIError [RawLineItem])
priceViaList ctx catid rlis = getPricingListById ctx catid
  >>= eitherPassErr (\pl ->
    passSuccess $ map (\rli ->
      let catCheck = case rli_category rli of
                          "Stock Pull" -> "fabrication"
                          "Warm Air Stock" -> "purchased"
                          "Assembly" -> "assembly"
                          _ -> rli_category rli
          matches = filter (\fpi -> fpi_refType fpi == catCheck
                                 && fpi_refName fpi == rli_name rli)
                           (pl_items pl)
      in
        case length matches of
             0 -> rli { rli_isCatalogue = False }
             _ -> rli { rli_isCatalogue = True
                      , rli_priceEach = fpi_fixedPrice $ head matches
                      }
    ) rlis)

-- UTILITY --
-- | Helper to coallesce errors or pass monadic success.
flattenCallback :: [Either APIError ()] -> IO (Either APIError ())
flattenCallback ers = case lefts ers of
                           [] -> passSuccess ()
                           es -> print es >> pure (Left $ flattenErrors es)

-- | Helper to check if item was priced as a fixed price catalog item.
checkForCatalog :: RawLineItem -> Double -> Double
checkForCatalog rli lookupPrice = if rli_isCatalogue rli
                                     then rli_priceEach rli
                                     else lookupPrice
