{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.Materials
Description: Controllers and handles for all material routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all material routes, including material purchases.

-}
module Controllers.Materials
  ( -- * Materials
    gMaterials
  , gMaterial
  , eMaterial
  , pMaterial
  -- * Material Purchases
  , gMaterialPurchases
  , gMaterialPurchase
  , eMaterialPurchase
  , dMaterialPurchase
  , pMaterialPurchase
  , gMatPurchaseOptionsForMaterial
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
                         , errJsonRes
                         , errReqBody
                         , errUriParam
                         , getPagQS
                         , reqBodyReader
                         , sendResp
                         )
import Materials ( addMaterial
                 , editMaterial
                 , getMaterialById
                 , listMaterials
                 )
import MatPurchases ( addMatPurchase
                    , editMatPurchase
                    , getMatPurchaseById
                    , listMappedMaterialOptions
                    , listMatPurchases
                    , removeMatPurchase
                    )
import Utils ( apiError400 )

-- | GET List materials query. Returns 200 list of 'APITypes.Material' on success.
gMaterials :: Context -> IO Response
gMaterials ctx@(Context (req, _)) =
  let pagQS = getPagQS req in
    listMaterials ctx pagQS >>= sendResp status200

-- | GET Retrieve material by id ('Int'). Returns 200 'APITypes.Material' on success.
gMaterial :: EIntParam -> Context -> IO Response
gMaterial (Left _) _ = errJsonRes $ apiError400 "Invalid material id uri parameter provided"
gMaterial (Right mid) c = getMaterialById c mid >>= sendResp status200

-- | PUT Edits the specified material. Returns 200 'APITypes.Material' on success.
eMaterial :: EIntParam -> Context -> IO Response
eMaterial (Left _) _ = errJsonRes $ apiError400 "Invalid material id uri parameter provided"
eMaterial (Right matid) c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\m -> editMaterial c m matid >>= sendResp status200)

-- | POST Adds a new material instance. Returns 201 'APITypes.Material' on success.
pMaterial :: Context -> IO Response
pMaterial c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addMaterial c >=> sendResp status201)

-- MAT PURCHASES
-- | GET List material purchase options query. Returns 200 'APITypes.MatPurchase' on success.
gMaterialPurchases :: Context -> IO Response
gMaterialPurchases ctx@(Context (req, _)) =
  let pagQS = getPagQS req in
    listMatPurchases ctx pagQS >>= sendResp status200

-- | GET Retrieves material purchase option by id. Returns 200 'APITypes.MatPurchase' on success.
gMaterialPurchase :: EIntParam -> Context -> IO Response
gMaterialPurchase (Left _) _ = errUriParam "material purchase"
gMaterialPurchase (Right mpid) ctx = getMatPurchaseById ctx mpid >>= sendResp status200

-- | PUT Edits the material purchase option. Returns 200 'APITypes.MatPurchase' on success.
eMaterialPurchase :: EIntParam -> Context -> IO Response
eMaterialPurchase (Left _) _ = errUriParam "material purchase"
eMaterialPurchase (Right mpid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (editMatPurchase ctx mpid >=> sendResp status200)

-- | DELETE Deletes material purchase option. Returns 204 on success.
dMaterialPurchase :: EIntParam -> Context -> IO Response
dMaterialPurchase (Left _) _ = errUriParam "material purchase"
dMaterialPurchase (Right mpid) ctx = removeMatPurchase ctx mpid >>= sendResp status204

-- | POST Adds new material purchase option. Returns 201 'APITypes.MatPurchase' on success.
pMaterialPurchase :: Context -> IO Response
pMaterialPurchase ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addMatPurchase ctx >=> sendResp status201)

-- | GET Retrieves all material purchase options for the selected material. Returns 200 'APITypes.MatPurchase' list on success.
gMatPurchaseOptionsForMaterial :: EIntParam -> Context -> IO Response
gMatPurchaseOptionsForMaterial (Left _) _ = errUriParam "material"
gMatPurchaseOptionsForMaterial (Right matid) ctx = listMappedMaterialOptions ctx matid >>= sendResp status200
