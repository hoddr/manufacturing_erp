{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.FabItems
Description: Controllers and handles for all fabrication item routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all fabrication item (stock) routes.

-}
module Controllers.FabItems
  ( -- * Fab Items
    gFabItems
  , pFabItem
  , eFabItem
  )
  where

import Control.Monad ( (>=>) )
import Network.Wai ( Response )
import Network.HTTP.Types ( status200
                          , status201
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
import FabItems ( addFabItem
                , editFabItem
                , listFabItems
                )

-- | GET List fabrication items based on querystring. Returns 200 'APITypes.FabItem' list on success.
gFabItems :: Context -> IO Response
gFabItems ctx@(Context (req, _)) =
  let pagQS = getPagQS req in
    listFabItems ctx pagQS >>= sendResp status200

-- | POST Creates new fabrication item. Returns 201 'APITypes.FabItem' on success.
pFabItem :: Context -> IO Response
pFabItem c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addFabItem c >=> sendResp status201)

-- | PUT Edits the given fabrication item. Returns 200 'APITypes.FabItem' on success.
eFabItem :: EIntParam -> Context -> IO Response
eFabItem (Left _) _ = errUriParam "fabrication item"
eFabItem (Right fabid) c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\f -> editFabItem c f fabid >>= sendResp status200)
