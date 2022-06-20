module Client.Materials
  ( addMaterial
  , editMaterial
  , getMaterial
  , listMaterials
  , testMaterial
  )
  where

import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( eitherDecode )
import Network.HTTP.Req


-- LOCAL IMPORTS --
import APITypes ( Inventory(..)
                , Material(..)
                , materialInv
                )
import qualified Client.Common as CC

addMaterial :: Material -> IO (Maybe Material)
addMaterial m = liftIO $ runReq defaultHttpConfig $ do
  res <- req POST
             (http CC.host /: "materials")
             (ReqBodyJson m)
             lbsResponse
             CC.opts
  let emat = (eitherDecode (responseBody res) :: Either String Material) in
    case emat of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right newm) -> pure $ Just newm

editMaterial :: Material -> IO (Maybe Material)
editMaterial m = liftIO $ runReq defaultHttpConfig $ do
  res <- req PUT
             (http CC.host /: "materials" /~ mid m)
             (ReqBodyJson m)
             lbsResponse
             CC.opts
  let emat = (eitherDecode (responseBody res) :: Either String Material) in
    case emat of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right _) -> pure $ Just m

getMaterial :: Material -> IO (Maybe Material)
getMaterial m = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "materials" /~ mid m)
             NoReqBody
             lbsResponse
             CC.opts
  let emat = (eitherDecode (responseBody res) :: Either String Material) in
    case emat of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right ret) -> pure $ Just ret

listMaterials :: IO [Material]
listMaterials = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "materials")
             NoReqBody
             lbsResponse
             CC.opts
  let emats = (eitherDecode (responseBody res) :: Either String [Material]) in
    case emats of
         (Left e) -> liftIO (print e) >> pure []
         (Right mats) -> pure mats

testMaterial :: Material
testMaterial = Material { mid = 0
                        , mname = "Test material"
                        , mdescription = "for testing steel"
                        , munit = "lbs"
                        , mcostPerUnit = 0.65
                        , mpreferredVendor = ""
                        , mbalanceCategory = "Steel"
                        , misLocked = True
                        , minventory = Inventory { invid = 0
                                                 , invreferenceId = 0
                                                 , invtype = materialInv
                                                 , invonHand = 10
                                                 , invonOrder = 2
                                                 , invminOnHand = 5
                                                 , invname = ""
                                                 }
                        }
