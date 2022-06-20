module Client.QBParser
  ( loadCustomers
  , loadFabItems
  , loadMaterials
  , loadPurchaseItems
  , loadVendors
  )
  where


import Data.Text ( Text )

-- LOCAL IMPORTS --
import APITypes ( APIError )
import QBParser ( importQBCustomers
                , importQBItems
                , importQBMaterials
                , importQBStock
                , importQBVendors
                , importWrapper
                )
import qualified Customers as CUS ( importFromList )
import qualified FabItems as FAB ( importFromList )
import qualified Materials as MAT ( importFromList )
import qualified PurchaseItems as PUR ( importFromList )
import qualified Vendors as VEN ( importFromList )

loadCustomers :: IO (Either APIError Text)
loadCustomers = importWrapper importQBCustomers CUS.importFromList "./tests/load_files/customers.csv"

loadVendors :: IO (Either APIError Text)
loadVendors = importWrapper importQBVendors VEN.importFromList "./tests/load_files/vendors.csv"

loadFabItems :: IO (Either APIError Text)
loadFabItems = importWrapper importQBStock FAB.importFromList "./tests/load_files/stock.csv"

loadMaterials :: IO (Either APIError Text)
loadMaterials = importWrapper importQBMaterials MAT.importFromList "./tests/load_files/materials.csv"

loadPurchaseItems :: IO (Either APIError Text)
loadPurchaseItems = importWrapper importQBItems PUR.importFromList "./tests/load_files/purchase_items.csv"
