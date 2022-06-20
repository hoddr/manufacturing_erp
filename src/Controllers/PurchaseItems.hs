{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.PurchaseItems
Description: Controllers and handles for all purchase item routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all purchase item routes.

-}
module Controllers.PurchaseItems
  ( -- * Purchase Items
    gPurchaseItems
  , pPurchaseItem
  , ePurchaseItem
  , dPurchaseItem
  )
  where

import Control.Monad ( (>=>) )
import Network.Wai ( Response )
import Network.HTTP.Types ( status200
                          , status201
                          , status204
                          )

-- LOCAL --
import APITypes ( Context(..) )
import Controllers.Utils ( EIntParam
                         , errReqBody
                         , errUriParam
                         , getPagQS
                         , reqBodyReader
                         , sendResp
                         )
import PurchaseItems ( addPurchaseItem
                     , deletePurchaseItem
                     , editPurchaseItem
                     , listPurchaseItems
                     )

-- | GET List purchase items based on querystring. Returns 200 'APITypes.PurchaseItem' list on success.
gPurchaseItems :: Context -> IO Response
gPurchaseItems ctx@(Context (req, _)) =
  let pagQS = getPagQS req in
    listPurchaseItems ctx pagQS >>= sendResp status200

-- | POST Creates new purchase item. Returns 201 'APITypes.PurchaseItem' on success.
pPurchaseItem :: Context -> IO Response
pPurchaseItem c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addPurchaseItem c >=> sendResp status201)

-- | PUT Edits the specified purchase item. Returns 200 'APITypes.PurchaseItem' on success.
ePurchaseItem :: EIntParam -> Context -> IO Response
ePurchaseItem (Left _) _ = errUriParam "purchase item"
ePurchaseItem (Right pid) c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (editPurchaseItem c pid >=> sendResp status200)

-- | DELETE Deletes the specified purchase item. Returns 204 on success.
dPurchaseItem :: EIntParam -> Context -> IO Response
dPurchaseItem (Left _) _ = errUriParam "purchase item"
dPurchaseItem (Right pid) ctx = deletePurchaseItem ctx pid >>= sendResp status204
