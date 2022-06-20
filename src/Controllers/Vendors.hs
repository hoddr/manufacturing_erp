{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.Vendors
Description: Controllers and handles for all vendor routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all vendor routes.

-}
module Controllers.Vendors
  ( -- * Vendors
    gVendors
  , pVendor
  , gVendor
  , eVendor
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
import Vendors ( addVendor
               , editVendor
               , getVendorById
               , listVendors
               )

-- | GET List vendors based on querystring. Returns 200 and 'APITypes.Vendor' list on success.
gVendors :: Context -> IO Response
gVendors ctx@(Context (req, _)) =
  let pagQS = getPagQS req in
    listVendors ctx pagQS >>= sendResp status200

-- | POST Create new vendor. Returns 201 and 'APITypes.Vendor' on success.
pVendor :: Context -> IO Response
pVendor c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addVendor c >=> sendResp status201)

-- | GET Retrieves vendor by id. Returns 200 and 'APITypes.Vendor' on success.
gVendor :: EIntParam -> Context -> IO Response
gVendor (Left _) _ = errUriParam "vendor"
gVendor (Right vid) c = getVendorById c vid >>= sendResp status200

-- | PUT Edits the specified vendor. Returns 200 and 'APITypes.Vendor' on success.
eVendor :: EIntParam -> Context -> IO Response
eVendor (Left _) _ = errUriParam "vendor"
eVendor (Right vid) c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\v -> editVendor c v vid >>= sendResp status200)
