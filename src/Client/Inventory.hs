module Client.Inventory
  ( editInventory
  , listInventory
  )
  where

import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( eitherDecode )
import Network.HTTP.Req


-- LOCAL IMPORTS --
import APITypes ( Inventory(..) )
import qualified Client.Common as CC


listInventory :: IO [Inventory]
listInventory = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "inventory")
             NoReqBody
             lbsResponse
             CC.opts
  let einvs = (eitherDecode (responseBody res) :: Either String [Inventory]) in
    case einvs of
         (Left e) -> liftIO (print e) >> pure []
         (Right invs) -> pure invs

editInventory :: Inventory -> IO Inventory
editInventory i = liftIO $ runReq defaultHttpConfig $ do
  res <- req PUT
             (http CC.host /: "inventory" /~ invid i)
             (ReqBodyJson i)
             lbsResponse
             CC.opts
  let einv = (eitherDecode (responseBody res) :: Either String Inventory) in
    case einv of
         (Left e) -> liftIO (print e) >> pure i
         (Right _) -> pure i
