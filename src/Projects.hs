{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Projects
Description: Quoted project management.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides functions and logic central to managing all aspects of projects, from
creation, editing, assigning orders to a project, creating sections, billing, and more.
Projects are one of the more complicated sections of the ERP, so take care with understanding
the multitude of mappings that connect projects, sections, orders, and line items.

-}

module Projects
  ( -- * Functions
    addProject
  , addSection
  , assignOrderToProject
  , deleteProject
  , deleteSection
  , doProjectBill
  , editProject
  , editSection
  , genProjectStatusReport
  , getProjectByAppId
  , getProjectById
  , listCategories
  , listProjectExtras
  , listProjectSkeletons
  , listProjects
  , markProjectOrdersAsBilled
  , removeExtra
  , removeOrderProjectMapping
  , removeProjectExtraMapping
  , setProjectExtras
  , swapProjectOrderSection
  , toggleProjectStatus
  )
  where


import Data.List ( foldl' )
import Data.Maybe ( fromMaybe
                  , isJust
                  )
import Data.Text ( toUpper )
import Database.PostgreSQL.Simple ( In(..)
                                  , Only(..)
                                  , Query
                                  )
import GHC.Int ( Int64 )


-- LOCAL IMPORTS --
import APITypes ( APIError(..)
                , APIMsg(..)
                , Category
                , Context(..)
                , Customer(..)
                , Extra(..)
                , LineItem(..)
                , Order(..)
                , QueryPag(..)
                , Project(..)
                , ProjectBillingInfo(..)
                , ProjectSection(..)
                , ProjectStatusReport(..)
                , defQueryPag
                )
import Customers ( baseCustomerQuery )
import Database ( runExecute
                , runQuery
                , runQuery_
                , runReturning
                )
import LineItems ( setExtraFlagSql )
import Orders ( getOrderById
              , listProjectOrders
              , listProjectOrdersFilter
              , listSectionOrders
              , listSectionOrdersFilter
              )
import Utils ( apiMsg201
             , apiMsg204
             , badReq
             , eitherPassErr
             , flattenData
             , gen
             , passOnError
             , passSuccess
             , wasFound
             )


-- FUNCTIONS --
-- | Creates a new project instance. This links the project to the provided quote id ('Int').
addProject :: Context -> Project -> IO (Either APIError Project)
addProject c p = runQuery c insertSql ( prjname p
                                      , prjappId p
                                      , prjpo p
                                      , cid $ prjcustomer p
                                      , prjtotEstWeight p
                                      , prjquoteId p
                                      )
  >>= eitherPassErr (gen (\pj nid -> pj { prjid = nid })
                              "Failure to return new project id"
                              p
                    )

-- | Deletes the specified project by id ('Int').
deleteProject :: Context -> Int -> IO (Either APIError ())
deleteProject c pid = runExecute c deleteSql [pid]
  >>= eitherPassErr (\_ -> passSuccess ())

-- | Edits the given project. Check the source for the supported edit fields.
editProject :: Context -> Project -> Int -> IO (Either APIError Project)
editProject c p pid
  | pid /= prjid p = badReq 400 "Project uri id and body id mismatch"
  | otherwise = runExecute c updateSql ( prjname p
                                       , prjappId p
                                       , prjpo p
                                       , prjtotEstWeight p
                                       , prjisActive p
                                       , cid $ prjcustomer p
                                       , prjquoteId p
                                       , prjid p
                                       )
                >>= eitherPassErr (\_ -> passSuccess p)

{- |

List projects without sections, orders, and line items within bounds of querystring
search and/or paginations. Will return an empty list for no results.
Use this when accessing numerous projects without immediate need for accessing their sub part information.

-}
listProjectSkeletons :: Context -> Bool -> QueryPag -> IO (Either APIError [Project])
listProjectSkeletons ctx showInactive (QueryPag ("", l, o)) =
  runQuery ctx (baseSql <> activeSearchSql <> sortSql <> paginationSql) (not showInactive, l, o)
listProjectSkeletons ctx showInactive (QueryPag (s, l, o)) =
  runQuery ctx (baseSql <> activeSearchSql <> searchSql <> sortSql <> paginationSql) (not showInactive, s, s, s, s, s, l, o)

{- |

List projects with sections, orders, and line items within bounds of querystring
search and/or paginations. Will return an empty list for no results.
Use this when accessing numerous projects with immediate need for accessing their sub part information.

-}
listProjects :: Context -> IO (Either APIError [Project])
listProjects c = listProjectSkeletons c False defQueryPag
  >>= eitherPassErr (\prs -> do
    eps <- mapM (addExtrasAndOrders c) prs
    pure $ flattenData eps
  )

-- | Retrieves valid categories. TODO For Extras? Maybe deprecated.
listCategories :: Context -> IO (Either APIError [Category])
listCategories c = runQuery_ c categoriesSql

-- | Retrieves project by field specified in query. Returns 404 error if not found.
getProjectBy :: (Context -> Project -> IO (Either APIError Project))
             -> Query
             -> Context
             -> Int
             -> IO (Either APIError Project)
getProjectBy cbfn whereQuery c queryVal = runQuery c (baseSql <> " " <> whereQuery) [queryVal]
  >>= eitherPassErr (wasFound "Project not found")
  >>= eitherPassErr (cbfn c)

-- | Wrapper to retrieve project by id ('Int').
getProjectById :: Context -> Int -> IO (Either APIError Project)
getProjectById = getProjectBy addExtrasAndOrders "where projects.id = ?"

-- | Wrapper to retrieve project by app id ('Int').
getProjectByAppId :: Context -> Int -> IO (Either APIError Project)
getProjectByAppId = getProjectBy addExtrasAndOrders "where projects.app_id = ?"

-- | Wrapper to retieve project by app id ('Int') with filtered orders.
getProjectByAppIdFiltered :: Context -> Int -> IO (Either APIError Project)
getProjectByAppIdFiltered = getProjectBy addExtrasAndFilteredOrders "where projects.app_id = ?"

-- | Appends extras, sections, and orders to the retrieved project.
addExtrasAndOrders :: Context -> Project -> IO (Either APIError Project)
addExtrasAndOrders c p = do
  eos <- listProjectOrders c (prjid p)
  eextras <- listProjectExtras c (prjid p)
  esecs <- listSections c (prjid p)
  combine p eos eextras esecs

-- | Appends extras, sections, and orders (filtered) to the retrieved project.
addExtrasAndFilteredOrders :: Context -> Project -> IO (Either APIError Project)
addExtrasAndFilteredOrders c p = do
  eos <- listProjectOrdersFilter c (prjid p)
  eextras <- listProjectExtras c (prjid p)
  esecs <- listSectionsFilter c (prjid p)
  combine p eos eextras esecs

-- | Helper to do error-handling on project sub item retrieval.
combine :: Project
        -> Either APIError [Order]
        -> Either APIError [Extra]
        -> Either APIError [ProjectSection]
        -> IO (Either APIError Project)
combine _ (Left e) _ _ = passOnError e
combine _ _ (Left e) _ = passOnError e
combine _ _ _ (Left e) = passOnError e
combine p (Right os) (Right exs) (Right secs) =
  passSuccess $ p { prjorders = os
                  , prjextras = exs
                  , prjsections = secs
                  }

-- | Retrieves valid categories. TODO For Extras? Maybe deprecated.
listProjectExtras :: Context -> Int -> IO (Either APIError [Extra])
listProjectExtras c pid = runQuery c extrasSql [pid]

-- | Retrieves valid categories. TODO For Extras? Maybe deprecated.
setProjectExtras :: Context -> Int -> [Extra] -> IO (Either APIError [Extra])
setProjectExtras c pid exs = runExecute c removeExtrasSql [pid]
  >>= eitherPassErr (\_ -> runReturning c extrasInsertSql exs)
  >>= eitherPassErr (\(_ :: [Only Int]) -> passSuccess exs)

-- | Retrieves valid categories. TODO For Extras? Maybe deprecated.
removeExtra :: Context -> Int -> Int -> IO (Either APIError APIMsg)
removeExtra c pid eid = runExecute c removeExtraSql (eid, pid)
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "No content")

-- | Assigns an order to the project (section assignment as well).
assignOrderToProject :: Context -> Int -> Int -> Int -> IO (Either APIError ())
assignOrderToProject c pid sid ordId = do
  eo <- getOrderById c ordId
  ep <- getProjectById c pid
  case (eo, ep) of
       (Left e, _) -> pure $ Left e
       (_, Left e) -> pure $ Left e
       (Right o, Right p) -> runQuery c insertProjectOrderSql ( pid, sid, ordId )
         >>= eitherPassErr (\(_ :: [Only Int]) -> setExtraFlags c o p)

-- | Sets extra flag on line items of the given order and project info.
setExtraFlags :: Context -> Order -> Project -> IO (Either APIError ())
setExtraFlags c o p = let (exs, nonexs) = filterExtras (prjextras p) (olineItems o) in
  runExecute c (setExtraFlagSql True) (Only $ In $ map lid exs)
  >>= eitherPassErr (\(_ :: Int64) ->
    runExecute c (setExtraFlagSql False) (Only $ In $ map lid nonexs)
    >>= eitherPassErr (\(_ :: Int64) -> passSuccess ())
  )

-- | Filters extras and non-extras into two groups.
filterExtras :: [Extra] -> [LineItem] -> ([LineItem], [LineItem])
filterExtras exs = foldl' (\(accexs, accnonexs) li ->
  if toUpper (lcategory li) == "MISC CHARGE"
  || any (matchesExtraCategory li) exs
       then (li:accexs, accnonexs)
       else (accexs, li:accnonexs)) ([], [])

-- | Name check for verifying extra or not.
matchesExtraCategory :: LineItem -> Extra -> Bool
matchesExtraCategory li e =
  case exnameCheck e of
       Nothing ->
         case excatName e of
              "RECTANGULAR DUCT" -> uc == "RECTANGULAR DUCT"
              "ROUND DUCT" -> uc == "ROUND DUCT"
              "OVAL DUCT" -> uc == "OVAL DUCT"
              "MISC RECTANGULAR" -> uc == "MISC RECTANGULAR"
              "MISC ROUND OVAL" -> uc == "MISC ROUND OVAL"
              "STOCK PULL" -> uc == "STOCK PULL"
              "WARM AIR STOCK" -> uc == "WARM AIR STOCK"
              "SW LINED RECTANGULAR" -> uc == "RECTANGULAR DUCT" && isLined
              "SW LINED ROUND" -> uc == "ROUND DUCT" && isLined
              "SW LINED OVAL" -> uc == "OVAL DUCT" && isLined
              "DW RECTANGULAR" -> uc == "RECTANGULAR DUCT" && isDblWall
              "DW ROUND" -> uc == "ROUND DUCT" && isDblWall
              "DW OVAL" -> uc == "OVAL DUCT" && isDblWall
              _ -> False
       (Just nc) -> uc == excatName e && nc == ldesc li
  where uc = toUpper $ lcategory li
        isLined = isJust (llinerId li)
               && isJust (llinerName li)
               && isJust (llinerArea li)
        isDblWall = isJust (lskinId li)
                 && isJust (lskinName li)
                 && isJust (lskinWeight li)

-- | Removes mapping from order to project (and section).
removeOrderProjectMapping :: Context -> Int -> IO (Either APIError ())
removeOrderProjectMapping c ordId = runExecute c removeProjectOrderSql [ordId]
  >>= eitherPassErr (\_ -> passSuccess ())

-- | Retrieves valid categories. TODO For Extras? Maybe deprecated.
removeProjectExtraMapping :: Context -> Int -> IO (Either APIError ())
removeProjectExtraMapping c pid = runExecute c removeProjectExtraSql [pid]
  >>= eitherPassErr (\_ -> passSuccess ())

-- | Retrieves information related to project billing (extras, weight, value).
doProjectBill :: Context -> Int -> IO (Either APIError ProjectBillingInfo)
doProjectBill c appId = getProjectByAppIdFiltered c appId
  >>= eitherPassErr ( passSuccess
                    . foldl' runSection (ProjectBillingInfo ([], 0.00, 0.00))
                    . prjsections
                    )

-- | Section level accumulator for extras, weight, value for project billing.
runSection :: ProjectBillingInfo -> ProjectSection -> ProjectBillingInfo
runSection (ProjectBillingInfo (lis, nonExtrasTotal, weightTotal)) prjsec =
  let (exs, weight) = runOrders (psorders prjsec)
      price = (weight / psweight prjsec * psprice prjsec) in
    ProjectBillingInfo (lis ++ exs, nonExtrasTotal + price, weightTotal + weight)

-- | Order level accumulator for extras, weight, value for project billing.
runOrders :: [Order] -> ([LineItem], Double)
runOrders =
  foldl' (\(acclis, accweight) o -> let (newlis, weight) = runLineItems (olineItems o) in
           (newlis ++ acclis, accweight + weight)
         )
         ([], 0.00)

-- | Line item level accumulator for extras, weight, value for project billing.
runLineItems :: [LineItem] -> ([LineItem], Double)
runLineItems lis = let (extras, nonexs) = separateExtras lis in
  (extras, foldl' (\acc li -> acc + lquant li * fromMaybe 0.00 (lweight li)) 0.00 nonexs)

-- | Splits apart line item extras and non-extras.
separateExtras :: [LineItem] -> ([LineItem], [LineItem])
separateExtras =
  foldl' (\(exs, nonexs) li -> if lisExtra li then (li:exs, nonexs) else (exs, li:nonexs)) ([], [])

-- | Lists the project's sections - also retrieves orders and line items.
listSections :: Context -> Int -> IO (Either APIError [ProjectSection])
listSections c pid = runQuery c sectionsSql [pid]
  >>= eitherPassErr (\ps -> do
                      eps <- mapM (\pjs ->
                        listSectionOrders c (psid pjs)
                          >>= eitherPassErr (\os -> passSuccess $ pjs { psorders = os })
                                  ) ps
                      pure $ flattenData eps
                    )

-- | Lists project sections (filtered).
listSectionsFilter :: Context -> Int -> IO (Either APIError [ProjectSection])
listSectionsFilter c pid = runQuery c sectionsSql [pid]
  >>= eitherPassErr (\ps -> do
                      eps <- mapM (\pjs ->
                        listSectionOrdersFilter c (psid pjs)
                          >>= eitherPassErr (\os -> passSuccess $ pjs { psorders = os })
                                  ) ps
                      pure $ flattenData eps
                    )

-- | Adds a section to the project.
addSection :: Context -> ProjectSection -> Int -> IO (Either APIError ProjectSection)
addSection c ps pid
  | pid == psprojectId ps = runQuery c addSectionSql ps
    >>= eitherPassErr (gen (\pjs newid -> pjs { psid = newid })
                                "Failure to return new project section id"
                                ps
                      )
  | otherwise = badReq 400 "Project id uri parameter and section entry mismatch"

-- | Removes a section from the given projet.
deleteSection :: Context -> Int -> Int -> IO (Either APIError APIMsg)
deleteSection c pid sid = runExecute c deleteSectionSql (pid, sid)
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg204 "No content")

-- | Edits the project section's information.
editSection :: Context -> ProjectSection -> Int -> Int -> IO (Either APIError ProjectSection)
editSection c ps pid sid
  | pid /= psprojectId ps = badReq 400 "Project uri id and body id mismatch"
  | sid /= psid ps = badReq 400 "Section uri id and body id mismatch"
  | otherwise = runExecute c sectionUpdateSql ( psname ps
                                              , psprice ps
                                              , psweight ps
                                              , psid ps
                                              )
    >>= eitherPassErr (\_ -> passSuccess ps)

-- | Moves the order to project mapping to a new project and section.
swapProjectOrderSection :: Context -> Int -> Int -> Int -> IO (Either APIError APIMsg)
swapProjectOrderSection c prjId ordid sid = runExecute c swapProjectOrderSectionSql (sid, ordid, prjId)
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg201 "Project order section swapped.")

-- | Generates a high-level overview of all project statuses.
genProjectStatusReport :: Context -> IO (Either APIError [ProjectStatusReport])
genProjectStatusReport c = listProjects c
  >>= eitherPassErr
    (passSuccess . map (\p ->
      let totEstPrice = foldl' (\acc s -> acc + psprice s) 0.00 (prjsections p)
          (totPrice, totWeight) =
            foldl' (\(op, ow) o ->
              let (ordp, ordw) = foldl' (\(lp, lw) li -> if lisExtra li
                                   then ( lp, lw )
                                   else ( lp + lquant li * lprice li
                                        , lw + lquant li * fromMaybe 0.00 (lweight li)
                                        )
                                        ) (0.00, 0.00) (olineItems o)
              in (ordp + op, ordw + ow)) (0.00, 0.00) (prjorders p)
      in ProjectStatusReport { psr_appId = prjappId p
                             , psr_estWeight = prjtotEstWeight p
                             , psr_currWeight = totWeight
                             , psr_percWeight = 100.0 * totWeight / prjtotEstWeight p
                             , psr_estPrice = totEstPrice
                             , psr_currDBPrice = totPrice
                             , psr_percPrice = 100.0 * totPrice / totEstPrice
                             }
               )
    )

-- | Sets all complete orders for the project as billed.
markProjectOrdersAsBilled :: Context -> Int -> IO (Either APIError APIMsg)
markProjectOrdersAsBilled c pid = runExecute c markAsBilledSql [pid]
  >>= eitherPassErr (\_ -> passSuccess $ apiMsg201 "Orders marked as billed")

-- | Toggle for in/active project status.
toggleProjectStatus :: Context -> Int -> Bool -> IO (Either APIError APIMsg)
toggleProjectStatus c pid toActiveFlag =
  let msg = if toActiveFlag then "Project set to active"
                            else "Project set to inactive"
            in
              runExecute c toggleActiveStatusSql (toActiveFlag, pid)
              >>= eitherPassErr (\_ -> passSuccess $ apiMsg201 msg)


-- SQL --
-- | Base query SQL.
baseSql :: Query
baseSql = "select \
          \projects.id as id, \
          \projects.app_id as appId, \
          \projects.name as name, \
          \projects.po as po, "
--           \projects.fk_customer_id as cid, "
       <> baseCustomerQuery -- this doesn't contain comma at end, needed here
       <> ", \
          \projects.total_estimated_weight as totalEstimatedWeight, \
          \projects.is_active as isActive, \
          \projects.fk_quote_id as quoteId, \
          \quotes.quote_number as quoteNumber \
          \from projects \
          \inner join customers \
          \on projects.fk_customer_id = customers.id \
          \inner join quotes \
          \on projects.fk_quote_id = quotes.id"

-- | Delete project SQL.
deleteSql :: Query
deleteSql = "delete from projects where projects.id = ?"

-- | Main insertion SQL. Check database for default values.
insertSql :: Query
insertSql = "insert into projects (\
            \name, \
            \app_id, \
            \po, \
            \fk_customer_id, \
            \total_estimated_weight, \
            \fk_quote_id \
            \) values (?, ?, ?, ?, ?, ?) \
            \returning id"

-- | Search for active status flag SQL.
activeSearchSql :: Query
activeSearchSql = " where projects.is_active = ?"

-- | Search SQL for naive, case-insensitive search on provided fields. Take care with the grouping!
searchSql :: Query
searchSql = " and (\
            \projects.app_id::varchar like ? or \
            \projects.name like ? or \
            \projects.po like ? or \
            \customers.name like ? or \
            \quotes.quote_number like ?)"

-- | Default sort SQL.
sortSql :: Query
sortSql = " order by projects.app_id desc"

-- | Pagination SQL.
paginationSql :: Query
paginationSql = " limit ? offset ?"

-- | Base update/edit SQL.
updateSql :: Query
updateSql = "update projects set \
            \name = ?, \
            \app_id = ?, \
            \po = ?, \
            \total_estimated_weight = ?, \
            \is_active = ?, \
            \fk_customer_id = ?, \
            \fk_quote_id = ? \
            \where projects.id = ?"

-- | Base section query SQL.
sectionsSql :: Query
sectionsSql = "select \
              \project_sections.id as id, \
              \project_sections.name as name, \
              \project_sections.section_price as price, \
              \project_sections.section_weight as weight, \
              \project_sections.fk_project_id as projectId \
              \from project_sections \
              \where project_sections.fk_project_id = ? \
              \order by project_sections.name asc"

-- | Main section insertion SQL. Check database for default values.
addSectionSql :: Query
addSectionSql = "insert into project_sections (\
                \name, \
                \section_price, \
                \section_weight, \
                \fk_project_id \
                \) values (?, ?, ?, ?) \
                \returning id"

-- | Delete section SQL.
deleteSectionSql :: Query
deleteSectionSql = "delete from project_sections \
                   \where project_sections.fk_project_id = ? \
                   \and project_sections.id = ?"

-- | Base section edit SQL.
sectionUpdateSql :: Query
sectionUpdateSql = "update project_sections set \
                   \name = ?, \
                   \section_price = ?, \
                   \section_weight = ? \
                   \where project_sections.id = ?"

-- | Retrieves valid categories. TODO For Extras? Maybe deprecated.
extrasSql :: Query
extrasSql = "select \
            \m_project_extras.id as id, \
            \m_project_extras.fk_category_id as categoryId, \
            \categories.name as categoryName, \
            \m_project_extras.name_check as nameCheck, \
            \m_project_extras.fk_project_id as projectId \
            \from m_project_extras \
            \inner join \
            \categories on m_project_extras.fk_category_id = categories.id \
            \where m_project_extras.fk_project_id = ?"

-- | Retrieves valid categories. TODO For Extras? Maybe deprecated.
extrasInsertSql :: Query
extrasInsertSql = "insert into m_project_extras (\
                  \fk_category_id, \
                  \name_check, \
                  \fk_project_id \
                  \) values (?, ?, ?) \
                  \returning id"

-- | Retrieves valid categories. TODO For Extras? Maybe deprecated.
removeExtrasSql :: Query
removeExtrasSql = "delete from m_project_extras where m_project_extras.fk_project_id = ?"

-- | Retrieves valid categories. TODO For Extras? Maybe deprecated.
removeExtraSql :: Query
removeExtraSql = "delete from m_project_extras where m_project_extras.id = ? and m_project_extras.fk_project_id = ?"

-- | Adds mapping of order to project and section.
insertProjectOrderSql :: Query
insertProjectOrderSql = "insert into m_project_orders (\
                        \fk_project_id, \
                        \fk_section_id, \
                        \fk_order_id \
                        \) values (?, ?, ?) \
                        \returning id"

-- | Removes mapping of order to project and section.
removeProjectOrderSql :: Query
removeProjectOrderSql = "delete from m_project_orders where m_project_orders.fk_order_id = ?"

-- | Retrieves valid categories. TODO For Extras? Maybe deprecated.
removeProjectExtraSql :: Query
removeProjectExtraSql = "delete from m_project_extras where m_project_extras.fk_project_id = ?"

-- | Retrieves valid categories. TODO For Extras? Maybe deprecated.
categoriesSql :: Query
categoriesSql = "select \
                \categories.id as id, \
                \categories.name as name \
                \from categories"

-- | Modifies order to project/section mapping.
swapProjectOrderSectionSql :: Query
swapProjectOrderSectionSql = "update m_project_orders \
                             \set fk_section_id = ? \
                             \where fk_order_id = ? \
                             \and fk_project_id = ?"

-- | Toggles All orders within project to billed where also complete SQL.
markAsBilledSql :: Query
markAsBilledSql = "update orders \
                  \set is_billed = true \
                  \where orders.id in (\
                    \select fk_order_id \
                    \from m_project_orders \
                    \where m_project_orders.fk_project_id = ?) \
                  \and orders.is_complete = true"

-- | Toggle for active flag SQL.
toggleActiveStatusSql :: Query
toggleActiveStatusSql = "update projects \
                        \set is_active = ? \
                        \where projects.id = ?"
