module Client.Vendors
  ( addVendor
  , badEdit
  , badIdVendor
  , editVendor
  , getVendor
  , listVendors
  , mainEdit
  , testVendor
  )
  where

import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( eitherDecode )
import Network.HTTP.Req


-- LOCAL IMPORTS --
import APITypes ( Vendor(..) )
import qualified Client.Common as CC


listVendors :: IO [Vendor]
listVendors = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "vendors")
             NoReqBody
             lbsResponse
             CC.opts
  let evs = (eitherDecode (responseBody res) :: Either String [Vendor]) in
    case evs of
         (Left e) -> liftIO (print e) >> pure []
         (Right vs) -> pure vs

addVendor :: Vendor -> IO (Maybe Vendor)
addVendor v = liftIO $ runReq defaultHttpConfig $ do
  res <- req POST
             (http CC.host /: "vendors")
             (ReqBodyJson v)
             lbsResponse
             CC.opts
  let evs = (eitherDecode (responseBody res) :: Either String Vendor) in
    case evs of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right newV) -> pure $ Just newV

getVendor :: Vendor -> IO (Maybe Vendor)
getVendor v = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "vendors" /~ vdid v)
             NoReqBody
             lbsResponse
             CC.opts
  let evs = (eitherDecode (responseBody res) :: Either String Vendor) in
    case evs of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right vend) -> pure $ Just vend

mainEdit :: Vendor -> IO (Maybe Vendor)
mainEdit v = editVendor (vdid v) v

badEdit :: Vendor -> IO (Maybe Vendor)
badEdit = editVendor (-3)

editVendor :: Int -> Vendor -> IO (Maybe Vendor)
editVendor vid v = liftIO $ runReq defaultHttpConfig $ do
  res <- req PUT
             (http CC.host /: "vendors" /~ vid)
             (ReqBodyJson v)
             lbsResponse
             CC.opts
  let evs = (eitherDecode (responseBody res) :: Either String Vendor) in
    case evs of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right sameV) -> pure $ Just sameV

testVendor :: Vendor
testVendor = Vendor { vdid = 0
                    , vdname = "Test vendor"
                    , vdcompany = "Test company"
                    }

badIdVendor :: Vendor
badIdVendor = Vendor { vdid = -1
                     , vdname = "Test vendor"
                     , vdcompany = "Test company"
                     }
