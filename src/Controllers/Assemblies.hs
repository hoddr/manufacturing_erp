{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.Assemblies
Description: Controllers and handles for all assembly routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all assembly routes.

-}
module Controllers.Assemblies
  ( -- * Assemblies
    gAssemblies
  , pAssembly
  , eAssembly
  , eAssemblyPartial
  , dAssembly
  , gAssemblyById
  , dSubItem
    -- * Assembly-related Routes
  , gBDFlangeCount
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
import Assemblies ( addAssembly
                  , deleteAssembly
                  , deleteSubItem
                  , editAssembly
                  , getAssemblyById
                  , getBDFlangeCount
                  , listAssemblyShells
                  )
import Controllers.Utils ( EIntParam
                         , errJsonRes
                         , errReqBody
                         , errUriParam
                         , getPagQS
                         , reqBodyReader
                         , sendResp
                         )
import Orders ( getOrderById )
import Utils ( apiError400
             , eitherPassErr
             )

-- | GET List assemblies query. Returns 200 on success.
gAssemblies :: Context -> IO Response
gAssemblies ctx@(Context (req, _)) = let pagQS = getPagQS req in
  listAssemblyShells ctx pagQS >>= sendResp status200

-- | POST Create new assembly. Returns 201 on success.
pAssembly :: Context -> IO Response
pAssembly ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addAssembly ctx >=> sendResp status201)

-- | PUT Edits the specified assembly with the sub items. Returns 201 on success.
eAssembly :: EIntParam -> Context -> IO Response
eAssembly (Left _) _ = errJsonRes $ apiError400 "Invalid assembly id uri parameter provided"
eAssembly (Right aid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\a -> editAssembly True ctx a aid >>= sendResp status201)

-- | PUT Edits the specified assembly but NOT the sub items. Returns 201 on success.
eAssemblyPartial :: EIntParam -> Context -> IO Response
eAssemblyPartial (Left _) _ = errJsonRes $ apiError400 "Invalid assembly id uri parameter provided"
eAssemblyPartial (Right aid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\a -> editAssembly False ctx a aid >>= sendResp status201)

-- | DELETE Deletes the specified assembly. Returns 204 on success.
dAssembly :: EIntParam -> Context -> IO Response
dAssembly (Left _) _ = errJsonRes $ apiError400 "Invalid assembly id uri parameter provided"
dAssembly (Right aid) ctx = deleteAssembly ctx aid >>= sendResp status204

-- | GET Retrieves the specified assembly by its id ('Int'). Returns 200 on success.
gAssemblyById :: EIntParam -> Context -> IO Response
gAssemblyById (Left _) _ = errJsonRes $ apiError400 "Invalid assembly id uri parameter provided"
gAssemblyById (Right aid) ctx = getAssemblyById ctx aid >>= sendResp status200

-- | DELETE Deletes the specified sub item. Returns 204 on success.
dSubItem :: EIntParam -> EIntParam -> Context -> IO Response
dSubItem (Left _) _ _ = errJsonRes $ apiError400 "Invalid assembly id uri parameter provided"
dSubItem _ (Left _) _ = errJsonRes $ apiError400 "Invalid sub item id uri parameter provided"
dSubItem (Right aid) (Right sid) ctx = deleteSubItem ctx aid sid >>= sendResp status204

-- | GET Retrieves bd flange count for the order. Returns 200 and CSV file on success.
gBDFlangeCount :: EIntParam -> Context -> IO Response
gBDFlangeCount (Left _) _ = errUriParam "order"
gBDFlangeCount (Right ordId) ctx = getOrderById ctx ordId
  >>= eitherPassErr (getBDFlangeCount ctx)
  >>= sendResp status200
