{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.Projects
Description: Controllers and handles for all project routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all project and project tracking routes.

-}

module Controllers.Projects
  ( -- * Projects
    gProjectSkeletons
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
  , gNotDoneProjectOrders
  , gProjectDetailedSalesData
  )
  where

import Control.Monad ( (>=>) )
import Network.Wai ( Response )
import Network.HTTP.Types ( status200
                          , status201
                          , status204
                          )

-- LOCAL --
import APITypes ( Context(..)
                , Order(..)
                , Project(..)
                , ProjectSection(..)
                )
import Controllers.Utils ( EIntParam
                         , errReqBody
                         , errUriParam
                         , getPagQS
                         , reqBodyReader
                         , sendCsvResp
                         , sendResp
                         )
import Orders ( getProjectDetailedSalesData
              , listNotDoneProjectOrders
              )
import Projects ( addSection
                , deleteProject
                , deleteSection
                , doProjectBill
                , editProject
                , editSection
                , getProjectByAppId
                , getProjectById
                , genProjectStatusReport
                , listProjectExtras
                , listProjectSkeletons
                , listProjects
                , markProjectOrdersAsBilled
                , removeExtra
                , removeProjectExtraMapping
                , setProjectExtras
                , toggleProjectStatus
                )
import ProjectTracker ( getTrackerByOrder
                      , getTrackerByProject
                      , getTrackerBySection
                      )
import Quotes ( removeQuoteProjectId )
import Utils ( apiError400
             , apiMsg204
             , eitherPassErr
             )

{- | GET List project skeletons by querystring (sans sections and orders).
Returns 200 and 'APITypes.Project' list on success.
-}
gProjectSkeletons :: Bool -> Context -> IO Response
gProjectSkeletons showInactive ctx@(Context (req, _)) =
  let pagQS = getPagQS req in
    listProjectSkeletons ctx showInactive pagQS >>= sendResp status200

{- | GET List projects by querystring (with sections and orders).
Returns 200 and 'APITypes.Project' list on success.
-}
gProjects :: Context -> IO Response
gProjects c = listProjects c >>= sendResp status200

-- | GET Retrieves full project by id. Returns 200 'APITypes.Project' on success.
gProject :: EIntParam -> Context -> IO Response
gProject (Left _) _ = errUriParam "project"
gProject (Right pid) c = getProjectById c pid >>= sendResp status200

-- | GET Retrieves full project by app id. Returns 200 'APITypes.Project' on success.
gProjectAppId :: EIntParam -> Context -> IO Response
gProjectAppId (Left _) _ = errUriParam "project app"
gProjectAppId (Right appId) c = getProjectByAppId c appId >>= sendResp status200

-- | PUT Edits the specified project. Returns 200 'APITypes.Project' on success.
eProject :: EIntParam -> Context -> IO Response
eProject (Left _) _ = errUriParam "project"
eProject (Right pid) c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\p -> editProject c p pid >>= sendResp status200)

{- | DELETE Deletes the specified project.
If project has orders, fails to delete.
Else deletes the project, removes extras mapping, and removes the quote to project mapping.
Returns 204 on success.
-}
dProject :: EIntParam -> Context -> IO Response
dProject (Left _) _ = errUriParam "project"
dProject (Right pid) ctx = getProjectById ctx pid
  >>= eitherPassErr (\p -> case prjorders p of
    [] -> deleteProject ctx pid
      >>= eitherPassErr (\_ -> removeProjectExtraMapping ctx pid)
      >>= eitherPassErr (\_ -> removeQuoteProjectId ctx pid)
      >>= eitherPassErr (\_ -> pure $ Right $ apiMsg204 "Project deleted")
    _ -> pure $ Left $ apiError400 "Project has associated orders. Cannot delete"
  )
  >>= sendResp status204

{- | POST Performs project billing (retrieves sums and necessary numbers).
Retrieves extras, value and weight of non-extras for billing. Does NOT mark any of the complete, non-billed
orders as billed during this step. This is done later.
Returns 200 and 'APITypes.ProjectBillingInfo' on success.
-}
hProjectBilling :: EIntParam -> Context -> IO Response
hProjectBilling (Left _) _ = errUriParam "project app"
hProjectBilling (Right appId) c = doProjectBill c appId >>= sendResp status200

-- | PUT Marks all complete, non-billed orders as billed for the given project. Returns 201 on success.
eProjectOrdersAsBilled :: EIntParam -> Context -> IO Response
eProjectOrdersAsBilled (Left _) _ = errUriParam "project"
eProjectOrdersAsBilled (Right pid) c = markProjectOrdersAsBilled c pid >>= sendResp status201

-- | POST Creates a new project section. Returns 201 on success.
pProjectSection :: EIntParam -> Context -> IO Response
pProjectSection (Left _) _ = errUriParam "order"
pProjectSection (Right pid) c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\ps -> addSection c ps pid >>= sendResp status201)

-- | DELETE Delete the project section. Returns 204 on success.
dProjectSection :: EIntParam -> EIntParam -> Context -> IO Response
dProjectSection (Left _) _ _ = errUriParam "project"
dProjectSection _ (Left _) _ = errUriParam "project section"
dProjectSection (Right pid) (Right sid) ctx = deleteSection ctx pid sid >>= sendResp status204

-- | PUT Edits the project section information. Returns 200 on success.
eProjectSection :: EIntParam -> EIntParam -> Context -> IO Response
eProjectSection (Left _) _ _ = errUriParam "project"
eProjectSection _ (Left _) _ = errUriParam "project section"
eProjectSection (Right pid) (Right sid) c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\ps -> editSection c ps pid sid >>= sendResp status200)

-- | GET Retrieves project tracker info on the project level. Returns 200 on success.
gProjectTracker :: EIntParam -> Context -> IO Response
gProjectTracker (Left _) _ = errUriParam "project"
gProjectTracker (Right pid) c = getProjectById c pid
  >>= eitherPassErr (pure . Right . getTrackerByProject)
  >>= sendResp status200

-- | GET Retrieves project tracker info on the section level. Returns 200 on success.
gProjectSectionTracker :: EIntParam -> EIntParam -> Context -> IO Response
gProjectSectionTracker (Left _) _ _ = errUriParam "project"
gProjectSectionTracker _ (Left _) _ = errUriParam "project section"
gProjectSectionTracker (Right pid) (Right sid) c = getProjectById c pid
  >>= eitherPassErr (\p ->
    case filter (\s -> psid s == sid) $ prjsections p of
         [] -> pure $ Left $ apiError400 "Provided section id not part of project"
         ss -> pure $ Right $ getTrackerBySection $ head ss
  )
  >>= sendResp status200

-- | GET Retrieves project tracker info on the order level. Returns 200 on success.
gProjectOrderTracker :: EIntParam -> EIntParam -> Context -> IO Response
gProjectOrderTracker (Left _) _ _ = errUriParam "project"
gProjectOrderTracker _ (Left _) _ = errUriParam "order"
gProjectOrderTracker (Right pid) (Right ordid) c = getProjectById c pid
  >>= eitherPassErr (\p ->
    case filter (\o -> oid o == ordid) $ prjorders p of
         [] -> pure $ Left $ apiError400 "Provided order id not part of project"
         os -> pure $ Right $ getTrackerByOrder $ head os
  )
  >>= sendResp status200

-- | GET Retrieves project extras. Returns 200 on success. Deprecated.
gProjectExtras :: EIntParam -> Context -> IO Response
gProjectExtras (Left _) _ = errUriParam "project"
gProjectExtras (Right pid) c = listProjectExtras c pid >>= sendResp status200

-- | PUT Edits the project extras. Returns 200 on success. Deprecated.
eProjectExtras :: EIntParam -> Context -> IO Response
eProjectExtras (Left _) _ = errUriParam "project"
eProjectExtras (Right pid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (setProjectExtras ctx pid >=> sendResp status200)

-- | DELETE Removes the project extra mapping. Returns 204 on success. Deprecated.
dProjectExtra :: EIntParam -> EIntParam -> Context -> IO Response
dProjectExtra (Left _) _ _ = errUriParam "project"
dProjectExtra _ (Left _) _ = errUriParam "extra"
dProjectExtra (Right pid) (Right eid) c = removeExtra c pid eid >>= sendResp status204

-- | GET Retrieves project status report for all projects. Returns 200 and CSV on success.
gProjectStatusReport :: Context -> IO Response
gProjectStatusReport c = genProjectStatusReport c >>= sendCsvResp

-- | PUT Edits the project status (toggle archived/not). Returns 201 on success.
eProjectStatus :: Bool -> EIntParam -> Context -> IO Response
eProjectStatus _ (Left _) _ = errUriParam "project"
eProjectStatus toActive (Right pid) c = toggleProjectStatus c pid toActive >>= sendResp status201

-- | Checks projects for open orders that are not complete. Returns text list of order numbers that are not complete/shipped yet.
-- Returns 200 and list always unless errored out.
gNotDoneProjectOrders :: Context -> IO Response
gNotDoneProjectOrders = listNotDoneProjectOrders >=> sendResp status200

-- | GET Retrieves line item details for all orders in the given project. Returns 200 CSV report.
gProjectDetailedSalesData :: EIntParam -> Context -> IO Response
gProjectDetailedSalesData (Left _) _ = errUriParam "project"
gProjectDetailedSalesData (Right pid) ctx = getProjectDetailedSalesData ctx pid >>= sendCsvResp
