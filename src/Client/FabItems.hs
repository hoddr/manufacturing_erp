module Client.FabItems
  ( addFabItem
  , editFabItem
  , listFabItems
  , testFabItem
  )
  where

import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( eitherDecode )
import Network.HTTP.Req


-- LOCAL IMPORTS --
import APITypes ( FabItem(..)
                , Inventory(..)
                , fabInv
                )
import qualified Client.Common as CC


listFabItems :: IO [FabItem]
listFabItems = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "fab-items")
             NoReqBody
             lbsResponse
             CC.opts
  let efis = (eitherDecode (responseBody res) :: Either String [FabItem]) in
    case efis of
         (Left e) -> liftIO (print e) >> pure []
         (Right fis) -> pure fis

addFabItem :: FabItem -> IO (Maybe FabItem)
addFabItem f = liftIO $ runReq defaultHttpConfig $ do
  res <- req POST
             (http CC.host /: "fab-items")
             (ReqBodyJson f)
             lbsResponse
             CC.opts
  let efi = (eitherDecode (responseBody res) :: Either String FabItem) in
    case efi of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right newF) -> pure $ Just newF

editFabItem :: Int -> FabItem -> IO (Maybe FabItem)
editFabItem fabid f = liftIO $ runReq defaultHttpConfig $ do
  res <- req PUT
             (http CC.host /: "fab-items" /~ fabid)
             (ReqBodyJson f)
             lbsResponse
             CC.opts
  let efi = (eitherDecode (responseBody res) :: Either String FabItem) in
    case efi of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right sameF) -> pure $ Just sameF

testFabItem :: FabItem
testFabItem = FabItem { fiid = 0
                      , finame = "Test fab item"
                      , fidescription = "Test fab item desc"
                      , fiunitQuantity = 12.0
                      , filabor = 32.4
                      , fimaterialId = 1
                      , fimaterialName = "GALV 90"
                      , fimaterialCost = 0.00
                      , filaborCost = 0.00
                      , ficost = 0.00
                      , fiprice = 0.00
                      , fiisStock = True
                      , fibalanceCategory = "Stock Spiral"
                      , fiinventory = Inventory { invid = 0
                                                , invreferenceId = 0
                                                , invtype = fabInv
                                                , invonHand = 10
                                                , invonOrder = 2
                                                , invminOnHand = 5
                                                , invname = ""
                                                }
                      }
