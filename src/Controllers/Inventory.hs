{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.Inventory
Description: Controllers and handles for all inventory routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all inventory routes.

-}
module Controllers.Inventory
  ( -- * Inventory
    gInventory
  , eInventory
  )
  where

import Network.Wai ( Response )
import Network.HTTP.Types ( status200 )

-- LOCAL --
import APITypes ( Context(..) )
import Controllers.Utils ( EIntParam
                         , errJsonRes
                         , errReqBody
                         , getPagQS
                         , reqBodyReader
                         , sendResp
                         )
import Inventory ( editInventory
                 , listInventory
                 )
import Utils ( apiError400 )

-- | GET List inventory query. Returns 200 on success.
gInventory :: Context -> IO Response
gInventory ctx@(Context (req, _)) =
  let pagQS = getPagQS req in
    listInventory ctx pagQS >>= sendResp status200

-- | PUT Edits the inventory instance. Returns 200 on success.
eInventory :: EIntParam -> Context -> IO Response
eInventory (Left _) _ = errJsonRes $ apiError400 "Invalid inventory id uri parameter provided"
eInventory (Right iid) c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\i -> editInventory c i iid >>= sendResp status200)
