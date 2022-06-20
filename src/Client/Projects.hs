module Client.Projects
  ( addSection
  , deleteProject
  , deleteSection
  , editProject
  , editSection
  , getProject
  , getProjectById
  , listCategories
  , listProjects
  , testSection
  )
  where


import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( eitherDecode )
import Network.HTTP.Req


-- LOCAL IMPORTS --
import APITypes ( Category(..)
                , Project(..)
                , ProjectSection(..)
                )
import qualified Client.Common as CC


listProjects :: IO [Project]
listProjects = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "projects")
             NoReqBody
             lbsResponse
             CC.opts
  let eps = (eitherDecode (responseBody res) :: Either String [Project]) in
    case eps of
         (Left e) -> liftIO (print e) >> pure []
         (Right ps) -> pure ps

editProject :: Project -> IO (Maybe Project)
editProject p = liftIO $ runReq defaultHttpConfig $ do
  res <- req PUT
             (http CC.host /: "projects" /~ prjid p)
             (ReqBodyJson p)
             lbsResponse
             CC.opts
  let ep = (eitherDecode (responseBody res) :: Either String Project) in
    case ep of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right _) -> pure $ Just p

deleteProject :: Project -> IO ()
deleteProject p = liftIO $ runReq defaultHttpConfig $ do
  _ <- req DELETE
           (http CC.host /: "projects" /~ prjid p)
           NoReqBody
           lbsResponse
           CC.opts
  pure ()

getProject :: Project -> IO (Maybe Project)
getProject p = getProjectById $ prjid p

getProjectById :: Int -> IO (Maybe Project)
getProjectById projectid = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "projects" /~ projectid)
             NoReqBody
             lbsResponse
             CC.opts
  let ep = (eitherDecode (responseBody res) :: Either String Project) in
    case ep of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right p) -> pure $ Just p

listCategories :: IO [Category]
listCategories  = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "categories")
             NoReqBody
             lbsResponse
             CC.opts
  let ecats = (eitherDecode (responseBody res) :: Either String [Category]) in
    case ecats of
         (Left e) -> liftIO (print e) >> pure []
         (Right cats) -> pure cats

addSection :: Project -> ProjectSection -> IO (Maybe ProjectSection)
addSection p ps = liftIO $ runReq defaultHttpConfig $ do
  res <- req POST
             (http CC.host /: "projects" /~ prjid p /: "sections")
             (ReqBodyJson ps)
             lbsResponse
             CC.opts
  let eps = (eitherDecode (responseBody res) :: Either String ProjectSection) in
    case eps of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right newPs) -> pure $ Just newPs

deleteSection :: Project -> ProjectSection -> IO ()
deleteSection p ps = liftIO $ runReq defaultHttpConfig $ do
  _ <- req DELETE
           (http CC.host /: "projects" /~ prjid p /: "sections" /~ psid ps)
           NoReqBody
           lbsResponse
           CC.opts
  pure ()

editSection :: Project -> ProjectSection -> IO (Maybe ProjectSection)
editSection p ps = liftIO $ runReq defaultHttpConfig $ do
  res <- req PUT
             (http CC.host /: "projects" /~ prjid p /: "sections" /~ psid ps)
             (ReqBodyJson ps)
             lbsResponse
             CC.opts
  let eps = (eitherDecode (responseBody res) :: Either String ProjectSection) in
    case eps of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right editedPs) -> pure $ Just editedPs

-- get project billing
-- GET projects/billing (qs params, from, to)

testSection :: Project -> ProjectSection
testSection p = ProjectSection { psid = 0
                               , psname = "MA-1"
                               , psprice = 125.00
                               , psweight = 55.3
                               , psprojectId = prjid p
                               , psorders = []
                               }
