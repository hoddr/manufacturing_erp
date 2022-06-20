module Client.Orders
  ( deleteOrder
  , listOrders
  , postOrder
  )
  where

import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( eitherDecode )
import Data.Text ( Text )
import Network.HTTP.Req


-- LOCAL IMPORTS --
import APITypes ( Order(..)
                , RawLineItem(..)
                )
import qualified Client.Common as CC

postOrder :: Text -> [RawLineItem] -> IO [RawLineItem]
postOrder orderType rlis = liftIO $ runReq defaultHttpConfig $ do
  res <- req POST
             (http CC.host /: "orders" /~ orderType)
             (ReqBodyJson rlis)
             lbsResponse
             CC.opts
  let elis = (eitherDecode (responseBody res) :: Either String [RawLineItem]) in
    case elis of
         (Left e) -> liftIO (print e) >> pure []
         -- (Right lis) -> liftIO (CC.printJsonRes lis) >> pure lis
         (Right lis) -> pure lis

listOrders :: IO [Order]
listOrders = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "orders")
             NoReqBody
             lbsResponse
             CC.opts
  let eos = (eitherDecode (responseBody res) :: Either String [Order]) in
    case eos of
         (Left e) -> liftIO (print e) >> pure []
         (Right os) -> pure os

deleteOrder :: Order -> IO ()
deleteOrder o = liftIO $ runReq defaultHttpConfig $ do
  _ <- req DELETE
           (http CC.host /: "orders" /~ oid o)
           NoReqBody
           lbsResponse
           CC.opts
  pure ()
