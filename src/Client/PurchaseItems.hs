module Client.PurchaseItems
  ( addPurchaseItem
  , editPurchaseItem
  , getId
  , listPurchaseItems
  , testPurchaseItem
  )
  where

import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( eitherDecode )
import Network.HTTP.Req
import Prelude hiding ( pi )


-- LOCAL IMPORTS --
import APITypes ( Inventory(..)
                , PurchaseItem(..)
                , Vendor(..)
                , purchaseInv
                )
import qualified Client.Common as CC


listPurchaseItems :: IO [PurchaseItem]
listPurchaseItems = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "purchase-items")
             NoReqBody
             lbsResponse
             CC.opts
  let epis = (eitherDecode (responseBody res) :: Either String [PurchaseItem]) in
    case epis of
         (Left e) -> liftIO (print e) >> pure []
         (Right pis) -> pure pis

addPurchaseItem :: PurchaseItem -> IO (Maybe PurchaseItem)
addPurchaseItem p = liftIO $ runReq defaultHttpConfig $ do
  res <- req POST
             (http CC.host /: "purchase-items")
             (ReqBodyJson p)
             lbsResponse
             CC.opts
  let epi = (eitherDecode (responseBody res) :: Either String PurchaseItem) in
    case epi of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right pi) -> pure $ Just pi

editPurchaseItem :: Int -> PurchaseItem -> IO (Maybe PurchaseItem)
editPurchaseItem purid p = liftIO $ runReq defaultHttpConfig $ do
  res <- req PUT
             (http CC.host /: "purchase-items" /~ purid)
             (ReqBodyJson p)
             lbsResponse
             CC.opts
  let epi = (eitherDecode (responseBody res) :: Either String PurchaseItem) in
    case epi of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right pi) -> pure $ Just pi

getId :: PurchaseItem -> Int
getId = piid

testVendor :: Vendor
testVendor = Vendor { vdid = 1
                    , vdname = "3Cstudio"
                    , vdcompany = "3Cstudio"
                    }

testPurchaseItem :: PurchaseItem
testPurchaseItem = PurchaseItem { piid = 0
                                , piname = "Test purchase"
                                , pidescription = "Test item"
                                , piprice = 125.32
                                , pipreferredVendor = "account"
                                , picost = 83.24
                                , pivendorPartNumber = Nothing
                                , pivendorCategory = Nothing
                                , pimarkup = 1.00
                                , pibalanceCategory = "Resale"
                                , piisLocked = True
                                , pileadTime = Nothing
                                , pivendor = testVendor
                                , piinventory = Inventory { invid = 0
                                                          , invreferenceId = 0
                                                          , invtype = purchaseInv
                                                          , invonHand = 10
                                                          , invonOrder = 2
                                                          , invminOnHand = 5
                                                          , invname = ""
                                                          }
                        }
