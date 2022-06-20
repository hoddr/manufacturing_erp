module Client.Customers
  ( addCustomer
  , badIdCustomer
  , editCustomer
  , getCustomer
  , listCustomers
  , testCustomer
  )
  where

import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( eitherDecode )
import Network.HTTP.Req


-- LOCAL IMPORTS --
import APITypes ( Customer(..) )
import qualified Client.Common as CC


listCustomers :: IO [Customer]
listCustomers = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "customers")
             NoReqBody
             lbsResponse
             CC.opts
  let ecs = (eitherDecode (responseBody res) :: Either String [Customer]) in
    case ecs of
         (Left e) -> liftIO (print e) >> pure []
         (Right cs) -> pure cs


addCustomer :: Customer -> IO (Maybe Customer)
addCustomer c = liftIO $ runReq defaultHttpConfig $ do
  res <- req POST
             (http CC.host /: "customers")
             (ReqBodyJson c)
             lbsResponse
             CC.opts
  let ecs = (eitherDecode (responseBody res) :: Either String Customer) in
    case ecs of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right newC) -> pure $ Just newC

getCustomer :: Customer -> IO (Maybe Customer)
getCustomer c = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (http CC.host /: "customers" /~ cid c)
             NoReqBody
             lbsResponse
             CC.opts
  let ecs = (eitherDecode (responseBody res) :: Either String Customer) in
    case ecs of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right cust) -> pure $ Just cust

editCustomer :: Int -> Customer -> IO (Maybe Customer)
editCustomer custId c = liftIO $ runReq defaultHttpConfig $ do
  res <- req PUT
             (http CC.host /: "customers" /~ custId)
             (ReqBodyJson c)
             lbsResponse
             CC.opts
  let ecs = (eitherDecode (responseBody res) :: Either String Customer) in
    case ecs of
         (Left e) -> liftIO (print e) >> pure Nothing
         (Right sameC) -> pure $ Just sameC

testCustomer :: Customer
testCustomer = Customer { cid = 0
                        , cname = "Test customer"
                        , ccompany = Just "Test company"
                        , ctype = Nothing
                        , cmarkup = 1.00
                        }

badIdCustomer :: Customer
badIdCustomer = Customer { cid = -1
                         , cname = "Test customer 2"
                         , ccompany = Just "Test company"
                         , ctype = Nothing
                         , cmarkup = 1.00
                         }
