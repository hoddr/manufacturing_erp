{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Assemblies
Description: Generation and management of fabricated items that require 1+ sub items.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides functions and management of assemblies (primarily Big BD pieces). These items
contain or require more than material or purchase item to produce. An overall labor value is
added to the top-level assembly (i.e. not the sub items). The assembly logic is set up
to manage the inventory for all sub items as well (when assemblies are built).
The assembly inventory is removed when the order is marked as billed.

-}

module Assemblies
  ( -- * Functions
    addAssembly
  , createAssemblyInv
  , deleteAssembly
  , deleteAssemblyInv
  , deleteSubItem
  , editAssembly
  , getAssemblyById
  , getAssemblyCosts
  , getBDFlangeCount
  , handleAssemblyOrderBilled
  , listAssemblyShells
  , sanitizeBDOrder
  , searchAssembliesByName
  , updateFabricatedQuantity
  )
  where


import Control.Monad ( (>=>)
                     , forM
                     )
import Data.List ( foldl' )
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import qualified Data.Text as T
import Database.PostgreSQL.Simple ( Only(..)
                                  , Query
                                  )

-- LOCAL IMPORTS --
import APITypes ( APIError
                , APIMsg
                , Assembly(..)
                , Context
                , FabItem(..)
                , FlangeReportEntry(..)
                , Inventory(..)
                , LineItem(..)
                , Markup(..)
                , Material(..)
                , Order(..)
                , PurchaseItem(..)
                , QueryPag(..)
                , SubItem(..)
                , assemblyInv
                )
import Database ( runExecute
                , runQuery
                , runReturning
                )
import Inventory ( addAssemblyEntry
                 , addPurchase
                 , removeInventory
                 , subtractPurchase
                 , updateInventory
                 )
import qualified FabItems as FAB ( getById )
import LineItems ( alterFabbedQuant
                 , toggleIsFabbedFlag
                 )
import Markups ( getLatestMarkupRates )
import Materials ( getMaterialById )
import Orders ( setOrderToBilled )
import PurchaseItems ( getPurchaseItemById )
import Utils ( apiMsg201
             , apiMsg204
             , badReq
             , eitherPassErr
             , flattenData
             , gen
             , inventorySql
             , passSuccess
             , wasFound
             )

-- FUNCTIONS --
-- | Creates new assembly (sans sub items). Also generates the inventory entry.
addAssembly :: Context -> Assembly -> IO (Either APIError Assembly)
addAssembly ctx a =
  runQuery ctx insertSql a
  >>= eitherPassErr (gen (\aprev nid -> aprev { a_id = nid })
                              "Failure to return new assembly id"
                              a
                    )
  >>= eitherPassErr (addToInv ctx)

-- | Helper for adding mapped assembly inventory entry.
addToInv :: Context -> Assembly -> IO (Either APIError Assembly)
addToInv ctx a = addAssemblyEntry ctx (a_id a) (a_inv a)
  >>= eitherPassErr (\ninv -> passSuccess $ a { a_inv = ninv })

-- | Adds mappings from assembly to new sub items.
addSubItems :: Context -> Assembly -> IO (Either APIError Assembly)
addSubItems ctx a = runReturning ctx insertSubItemsSql adjSis
  >>= eitherPassErr (\(_ :: [Only Int]) -> passSuccess a)
  where adjSis = map (\si -> si { sub_id = a_id a }) (a_subItems a)

-- | Deletes the assembly. First removes sub items, then assembly, then inventory entry.
deleteAssembly :: Context -> Int -> IO (Either APIError APIMsg)
deleteAssembly ctx aid = runExecute ctx removeSubSql [aid]
  >>= eitherPassErr (\_ -> runExecute ctx removeSql [aid])
  >>= eitherPassErr (\_ -> removeInventory ctx aid assemblyInv)
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Assembly deleted")

-- | Edits the top-level assembly information. If a flag is passed, will also alter the sub items.
-- Edits the inventory regardless.
editAssembly :: Bool -> Context -> Assembly -> Int -> IO (Either APIError Assembly)
editAssembly subItemsFlag ctx a aid
  | a_id a == aid = runExecute ctx updateSql ( a_name a
                                             , a_description a
                                             , a_labor a
                                             , a_id a
                                             )
    >>= eitherPassErr (\_ -> if subItemsFlag then updateSubItems ctx a else passSuccess a)
    >>= eitherPassErr (\_ -> updateInventory ctx (a_inv a))
    >>= eitherPassErr (\_ -> passSuccess a)
  | otherwise = badReq 400 "Assembly uri id and body id mismatch"

-- | Edits the sub items by removing the mappings then re-adding with the changed/new sub items.
updateSubItems :: Context -> Assembly -> IO (Either APIError Assembly)
updateSubItems ctx a = runExecute ctx removeSubSql [a_id a]
  >>= eitherPassErr (\_ -> addSubItems ctx a)

-- | Retrieves the assembly by id with sub items.
-- TODO add weight calc after retrieving sub items (work) lookup each sub item at moment
getAssemblyById :: Context -> Int -> IO (Either APIError Assembly)
getAssemblyById ctx aid = runQuery ctx (baseSql <> " where assemblies.id = ?") [aid]
  >>= eitherPassErr (wasFound "Assembly not found")
  >>= eitherPassErr (listSubItems ctx)

-- | Retrieves list of all sub items with their prices.
listSubItems :: Context -> Assembly -> IO (Either APIError Assembly)
listSubItems ctx a = getLatestMarkupRates ctx
  >>= eitherPassErr (\mkups -> runQuery ctx baseSubSql [a_id a]
    >>= eitherPassErr (priceSubItems ctx)
    >>= eitherPassErr (\sis -> passSuccess $ a { a_subItems = sis })
    >>= eitherPassErr (passSuccess . priceAssembly mkups)
  )

-- | Applies markups to the assembly and returns assembly with correct price.
priceAssembly :: Markup -> Assembly -> Assembly
priceAssembly mkups a =
  a { a_price = (*) (mkupassembly mkups) $ foldl' (\acc si -> acc + (sub_priceEach si * sub_quantity si)) (a_laborPrice a) (a_subItems a) }

-- | Retrieves data and prices all sub items.
priceSubItems :: Context -> [SubItem] -> IO (Either APIError [SubItem])
priceSubItems ctx sis = mapM (\si ->
  case invtype $ sub_inv si of
       "purchase" ->
         getPurchaseItemById ctx (invreferenceId $ sub_inv si)
         >>= eitherPassErr (\pit -> let i = sub_inv si in
           passSuccess $ si { sub_priceEach = picost pit
                            , sub_inv = i { invname = piname pit }
                            })

       "fabrication" ->
         FAB.getById ctx (invreferenceId $ sub_inv si)
         >>= eitherPassErr (\fi -> let i = sub_inv si in
           passSuccess $ si { sub_priceEach = ficost fi
                            , sub_inv = i { invname = finame fi }
                            })
       "material" ->
         getMaterialById ctx (invreferenceId $ sub_inv si)
         >>= eitherPassErr (\m -> let i = sub_inv si in
           passSuccess $ si { sub_priceEach = mcostPerUnit m
                            , sub_inv = i { invname = mname m }
                            })

       _ -> passSuccess $ si { sub_priceEach = 0.00 }
  ) sis >>= (pure . flattenData)

{- |

List assemblies without sub items within bounds of querystring search and/or paginations.
Will return an empty list for no results. Use this when accessing numerous assemblies without
immediate need for accessing their sub items.

-}
listAssemblyShells :: Context -> QueryPag -> IO (Either APIError [Assembly])
listAssemblyShells ctx (QueryPag ("", l, o)) =
  runQuery ctx (baseSql <> sortSql <> paginationSql) (l, o)
listAssemblyShells ctx (QueryPag (s, l, o)) =
  runQuery ctx (baseSql <> searchSql <> sortSql <> paginationSql) (s, s, l, o)

-- | Removes sub item mapping from assembly.
deleteSubItem :: Context -> Int -> Int -> IO (Either APIError APIMsg)
deleteSubItem ctx aid sid = runExecute ctx removeSubItemSql (aid, sid)
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "Sub item removed")

-- | Query for assembly by name ('Text'). Returns 404 if not found.
searchAssembliesByName :: Context -> Text -> IO (Either APIError Assembly)
searchAssembliesByName c n =
  runQuery c (baseSql <> " where assemblies.name = ?") [n]
    >>= eitherPassErr (wasFound "Assembly with specified name not found")
    >>= eitherPassErr (listSubItems c)

-- | Increments or decrements the fabrication count for an assembly line item in an order.
updateFabricatedQuantity :: Context
                         -> Int
                         -> Bool
                         -> LineItem
                         -> IO (Either APIError LineItem)
updateFabricatedQuantity ctx ordid isAdd li
  | fromMaybe 0 (lorderId li) == ordid =
    case lcategory li of
         "Assembly" ->
           let fn = if isAdd then createAssemblyInv else deleteAssemblyInv in
             fn ctx li >>= cb
         "Purchased" -> let fn = if isAdd then subPurchase else plusPurchase in
           fn ctx li >>= cb
         _ -> badReq 400 "Only assembly and purchased items are supported at this time."
  | otherwise = badReq 400 "Order uri id and line item order id mismatch"
  where op = if isAdd then (+) 1 else (-) 1
        isDone = lquant li <= op (lquantFabbed li)
        cb en = eitherPassErr (\_ -> toggleIsFabbedFlag ctx li isDone) en
             >>= eitherPassErr (\_ -> passSuccess li)

-- | Wrapper for adjusting assembly inventory (increments).
createAssemblyInv :: Context -> LineItem -> IO (Either APIError ())
createAssemblyInv = adjustAssembly (-) (1 +)

-- | Wrapper for adjusting assembly inventory (decrements).
deleteAssemblyInv :: Context -> LineItem -> IO (Either APIError ())
deleteAssemblyInv = adjustAssembly (+) ((-1) +)

-- | Higher-order helper for adjusting the inventory of both the sub items and the assembly itself.
adjustAssembly :: (Double -> Double -> Double)
               -> (Double -> Double)
               -> Context
               -> LineItem
               -> IO (Either APIError ())
adjustAssembly op op2 ctx li = searchAssembliesByName ctx (ldesc li)
  >>= eitherPassErr (\a ->
    forM (a_subItems a) (\si ->
      -- adjust sub item(s) inventory
      let i = sub_inv si in
        updateInventory ctx (i { invonHand = op (invonHand $ sub_inv si) (sub_quantity si) })
    )
    >>= (pure . flattenData)
    >>= eitherPassErr (\_ ->
      -- adjust assembly inv
      let i = a_inv a in
        updateInventory ctx (i { invonHand = op2 (invonHand $ a_inv a) })
        >>= eitherPassErr (\_ -> passSuccess ())
    )
  )
  >>= eitherPassErr (\_ -> let oldFabbedQuant = lquantFabbed li in
                                    alterFabbedQuant ctx (li { lquantFabbed = op2 oldFabbedQuant }))
  >>= eitherPassErr (\_ -> passSuccess ())

-- | Adding a purchase item assembled, i.e. taking one out of inventory.
subPurchase :: Context -> LineItem -> IO (Either APIError ())
subPurchase = adjustPurchase (1 +) subtractPurchase

-- | Subtracting a purchase item assembly, i.e. putting one into inventory.
plusPurchase :: Context -> LineItem -> IO (Either APIError ())
plusPurchase = adjustPurchase ((-1) +) addPurchase

-- | Higher-order helper for adjusting purchase order line items counts.
adjustPurchase :: (Double -> Double)
               -> (Context -> LineItem -> IO (Either APIError ()))
               -> Context
               -> LineItem
               -> IO (Either APIError ())
adjustPurchase op fn ctx li = fn ctx li
  >>= eitherPassErr (\_ -> let oldFabbedQuant = lquantFabbed li in
                      alterFabbedQuant ctx (li { lquantFabbed = op oldFabbedQuant }))
  >>= eitherPassErr (\_ -> passSuccess ())

-- | Marks the order as billed. Removes the assemblies from inventory.
handleAssemblyOrderBilled :: Context -> Order -> Text -> IO (Either APIError APIMsg)
handleAssemblyOrderBilled ctx o invoiceNumber
  | oisBilled o = badReq 400 "Order cannot be billed more than once."
  | (not . all lisFabbed) (olineItems o) = badReq 400 "All line items must be fabricated in their entirety prior to closing the billing on this order."
  | otherwise = forM flis (\li ->
     searchAssembliesByName ctx (ldesc li)
     >>= eitherPassErr (\a -> let i = a_inv a in
       updateInventory ctx (i { invonHand = invonHand (a_inv a) - lquant li } )
     )
   ) >>= (pure . flattenData)
   >>= eitherPassErr (\_ -> setOrderToBilled ctx (oid o) invoiceNumber)
   >>= eitherPassErr (\_ -> passSuccess $ apiMsg201 "BD order billed with inventory adjustments.")
  where flis = filter (\li -> lcategory li == "Assembly") (olineItems o)

-- | Retrieves a flange count from the BD order.
getBDFlangeCount :: Context -> Order -> IO (Either APIError [FlangeReportEntry])
getBDFlangeCount ctx o
  | otype o == "PGB" = do
    emapfca <- flangeCountAssemblies ctx o
    emappca <- flangeCountPurchases ctx o
    case (emapfca, emappca) of
         (Left e, _) -> pure $ Left e
         (_, Left e) -> pure $ Left e
         (Right mapfca, Right mappca) ->
           passSuccess $ map FlangeReportEntry $ Map.toList $ Map.unionWith (+) mapfca mappca
  | otherwise = badReq 400 "Order is not designated as a BD order"

-- | Helper to flanges working through each assembly and its sub item(s).
flangeCountAssemblies :: Context -> Order -> IO (Either APIError (Map.Map Text Double))
flangeCountAssemblies ctx o =
  forM flis (\li -> searchAssembliesByName ctx (ldesc li)
    >>= eitherPassErr (\a -> passSuccess (li, a)))
  >>= (pure . flattenData)
  >>= eitherPassErr (passSuccess .
    foldl' (\accmap (li, a) ->
      foldl' (\accmapinner si -> let name = invname $ sub_inv si in
        if filterFlanges name
           then if Map.member name accmapinner
                then Map.adjust ((lquant li * sub_quantity si) +) name accmapinner
                else Map.insert name (lquant li * sub_quantity si) accmapinner
           else accmapinner
      ) accmap (a_subItems a)
    ) Map.empty)
  where flis = filter (\li -> lcategory li == "Assembly") $ olineItems o

-- | Helper to count number of flanges in purchase items.
flangeCountPurchases :: Context -> Order -> IO (Either APIError (Map.Map Text Double))
flangeCountPurchases _ o =
  let flis = filter (\li -> lcategory li == "Purchased" && filterFlanges (ldesc li)) $ olineItems o in
    passSuccess $ foldl' (\accmap li -> if Map.member (ldesc li) accmap
                             then Map.adjust (lquant li +) (ldesc li) accmap
                             else Map.insert (ldesc li) (lquant li) accmap
                         ) Map.empty flis

-- | Helper to filter flanges only.
filterFlanges :: Text -> Bool
filterFlanges v = T.take 7 v == "FLANGES"

-- | Sums up material, labor, and purchase items to get total cost of the assembly.
getAssemblyCosts :: Context -> Assembly -> IO (Either APIError (Double, Double, Double))
getAssemblyCosts ctx a =
  forM (a_subItems a) (\si ->
    case invtype (sub_inv si) of
         "material" -> getMaterialById ctx (invreferenceId $ sub_inv si)
           >>= eitherPassErr (\m -> passSuccess (0.00 :: Double, mcostPerUnit m * sub_quantity si, 0.00))
         _ -> passSuccess (0.00, 0.00, 0.00)
  )
  >>= (pure . flattenData)
  >>= eitherPassErr (passSuccess . foldl' (\(tacc, macc, lacc) (_, mnext, lnext) ->
    (tacc + mnext, macc + mnext, lacc + lnext)) (0.00, 0.00, a_laborPrice a))

{- |
Runs through the FL order to be billed, checking for BD assembly pieces that are nothing more than
DCC/DCF clamp/flanges. If so, changes billing category and appends DCC/DCF descrption to name. This is
for billing/accounting purposes only.
-}
sanitizeBDOrder :: Context -> Order -> IO (Either APIError Order)
sanitizeBDOrder ctx o =
  forM (olineItems o) (\li -> let d = ldesc li in
    if lcategory li == "Assembly"
       then searchRingSetAssemblies ctx d
         >>= eitherPassErr (\t -> passSuccess $ li { ldesc = d <> t
                                                   , lcategory = if t == "" then "Assembly"
                                                                            else "Purchased"
                                                   }
                           )
       else passSuccess li
  ) >>= (pure . flattenData)
    >>= eitherPassErr (\newlis -> passSuccess $ o { olineItems = newlis })

searchRingSetAssemblies :: Context -> Text -> IO (Either APIError Text)
searchRingSetAssemblies ctx n = runQuery ctx (baseSql <> clampSetSearchSql) [n]
  >>= eitherPassErr (wasFound "Not found" >=> either (\_ -> passSuccess "")
                                                     (\a -> passSuccess $ " - " <> a_description a))

-- SQL --
-- | Base query SQL. Note the calculation for total labor rate and the join for the inventory information.
baseSql :: Query
baseSql = "select \
          \assemblies.id as id, \
          \assemblies.name as name, \
          \assemblies.description as description, \
          \assemblies.labor as labor, \
          \(assemblies.labor * (\
            \select (labor_rate.shop_rate + labor_rate.overhead_rate) \
            \as total_rate from labor_rate where labor_rate.is_current = true limit 1)\
          \) as laborPrice, "
       <> inventorySql
       <> ", assemblies.name as invName \
          \from assemblies \
          \inner join inventory on \
          \assemblies.id = inventory.reference_id \
          \and inventory.reference_type = 'assembly'"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by name desc"

-- | Search SQL for naive, case-insensitive search on provided fields.
searchSql :: Query
searchSql = " where \
            \assemblies.name like ? or \
            \assemblies.description like ?"

-- | Main insertion SQL. Check database for default field values.
insertSql :: Query
insertSql = "insert into assemblies (\
            \name, \
            \description, \
            \labor \
            \) values (?, ?, ?) \
            \returning id"

-- | Pagination SQL.
paginationSql :: Query
paginationSql = " limit ? offset ?"

-- | Delete SQL for assembly by id.
removeSql :: Query
removeSql = "delete from assemblies where id = ?"

-- | Delete SQL for all mapped sub items.
removeSubSql :: Query
removeSubSql = "delete from m_assembly_subitems where fk_assembly_id = ?"

-- | Edit/update SQL for assembly information.
updateSql :: Query
updateSql = "update assemblies set \
            \name = ?, \
            \description = ?, \
            \labor = ? \
            \where assemblies.id = ?"

-- | Insertion SQL for adding sub items.
insertSubItemsSql :: Query
insertSubItemsSql = "insert into m_assembly_subitems (\
                    \fk_assembly_id, \
                    \fk_inventory_id, \
                    \quantity \
                    \) values (?, ?, ?) \
                    \returning id"

-- | Base query for sub items, also retrieving their inventory entry.
baseSubSql :: Query
baseSubSql = "select \
             \m_assembly_subitems.id as id, \
             \m_assembly_subitems.fk_assembly_id as assemblyId, "
          <> inventorySql
          <> ", 'na' as inventoryName, \
             \m_assembly_subitems.quantity as quant \
             \from m_assembly_subitems \
             \inner join inventory on \
             \m_assembly_subitems.fk_inventory_id = inventory.id \
             \where m_assembly_subitems.fk_assembly_id = ? \
             \order by m_assembly_subitems.id asc"

-- | Removes specific sub item mapping by assembly and sub item id.
removeSubItemSql :: Query
removeSubItemSql = "delete from m_assembly_subitems where fk_assembly_id = ? and id = ?"

-- | Search filters for DCC/DCF sets
clampSetSearchSql :: Query
clampSetSearchSql = "where assemblies.name = ? \
                    \and (assemblies.description like 'DCC__' \
                      \or assemblies.description like 'DCF__')"
