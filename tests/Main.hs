module Main ( main ) where

import Control.Concurrent ( ThreadId
                          , forkIO
                          , killThread
                          , threadDelay
                          )
import Control.Exception ( bracket )
import Test.Hspec ( hspec )


-- LOCAL IMPORTS --
import qualified Lib
import qualified Spec.Auth as Auth
import qualified Spec.Customers as Customers
import qualified Spec.FabItems as FabItems
import qualified Spec.Inventory as Inventory
import qualified Spec.Markups as Markups
import qualified Spec.Materials as Materials
import qualified Spec.Orders as Orders
import qualified Spec.Pricing as Pricing
import qualified Spec.Projects as Projects
import qualified Spec.PurchaseItems as PurchaseItems
import qualified Spec.QBParser as QBParser
import qualified Spec.Quotes as Quotes
import qualified Spec.Vendors as Vendors


-- QBParser MUST COME FIRST
main :: IO ()
main = withEnv . hspec $ do
  QBParser.spec
  Auth.spec
  Inventory.spec
  Markups.spec
  Materials.spec
  Orders.spec
  Pricing.spec
  Customers.spec
  Vendors.spec
  FabItems.spec
  Quotes.spec
  PurchaseItems.spec
  Projects.spec

withEnv :: IO () -> IO ()
withEnv = bracket setupEnv cleanEnv . const

-- additional setup here
setupEnv :: IO ThreadId
setupEnv = do
  tid <- forkIO Lib.main
  threadDelay 1000000
  return tid

-- additional cleanup here
cleanEnv :: ThreadId -> IO ()
cleanEnv tid = do
  killThread tid
  putStrLn $ "Server killed (" <> show tid <> ")"
