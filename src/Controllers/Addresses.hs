{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.Addresses
Description: Controllers and handles for all address routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all address routes.

-}
module Controllers.Addresses
  ( -- * Addresses
    dAddress
  , eAddress
  , gAddress
  , gAddresses
  , gCustomerAddress
  , gVendorAddress
  , gCCAddress
  , pAddress
  , pCustomerToAddress
  , pVendorToAddress
  , pCCQuoteToAddress
  )
  where

import Control.Monad ( (>=>) )
import Data.Text ( Text )
import Network.Wai ( Response )
import Network.HTTP.Types ( status200
                          , status201
                          , status204
                          )

-- LOCAL --
import Addresses ( addAddress
                 , assignCCQuoteToAddress
                 , assignCustomerToAddress
                 , assignVendorToAddress
                 , deleteAddress
                 , editAddress
                 , getAddressById
                 , listAddresses
                 , retrieveCCQuoteAddress
                 , retrieveCustomerAddress
                 , retrieveVendorAddress
                 )
import APITypes ( Context(..)
                , CCQuote(..)
                )
import Controllers.Utils ( EIntParam
                         , errReqBody
                         , errUriParam
                         , getPagQS
                         , reqBodyReader
                         , sendResp
                         )
import CC ( getCCQuoteByPID )
import Utils ( eitherPassErr )

-- | GET List addresses based on querystring. Returns 200 'APITypes.Address' list on success.
gAddresses :: Context -> IO Response
gAddresses ctx@(Context (req, _)) =
  let pagQS = getPagQS req in
    listAddresses ctx pagQS >>= sendResp status200

-- | GET Retrieves address by id. Returns 200 'APITypes.Address' on success.
gAddress :: EIntParam -> Context -> IO Response
gAddress (Left _) _ = errUriParam "address"
gAddress (Right aid) c = getAddressById c aid >>= sendResp status200

-- | PUT Edits the associated address entry. Returns 200 'APITypes.Address' on success.
eAddress :: EIntParam -> Context -> IO Response
eAddress (Left _) _ = errUriParam "address"
eAddress (Right aid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (editAddress ctx aid >=> sendResp status200)

-- | DELETE Deletes the associated address entry. Returns 204 on success.
dAddress :: EIntParam -> Context -> IO Response
dAddress (Left _) _ = errUriParam "address"
dAddress (Right aid) ctx = deleteAddress ctx aid >>= sendResp status204

-- | POST Adds new address entry. Customer/vendor mapping done in separate route(s). Returns 201 'APITypes.Address' on success.
pAddress :: Context -> IO Response
pAddress ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addAddress ctx >=> sendResp status201)

-- | POST Assigns customer to address entry. Returns 201 on success. TODO consider checking for valid address/customer?
pCustomerToAddress :: EIntParam -> EIntParam -> Context -> IO Response
pCustomerToAddress (Left _) _ _ = errUriParam "address"
pCustomerToAddress _ (Left _) _ = errUriParam "customer"
pCustomerToAddress (Right aid) (Right cid) ctx = assignCustomerToAddress ctx aid cid >>= sendResp status201

-- | POST Assigns vendor to address entry. Returns 201 on success. TODO consider checking for valid address/vendor?
pVendorToAddress :: EIntParam -> EIntParam -> Context -> IO Response
pVendorToAddress (Left _) _ _ = errUriParam "address"
pVendorToAddress _ (Left _) _ = errUriParam "vendor"
pVendorToAddress (Right aid) (Right vid) ctx = assignVendorToAddress ctx aid vid >>= sendResp status201

-- | POST Assigns curb co quote to address entry. Returns 201 on success. TODO consider checking for valid address?
pCCQuoteToAddress :: EIntParam -> EIntParam -> Context -> IO Response
pCCQuoteToAddress (Left _) _ _ = errUriParam "address"
pCCQuoteToAddress _ (Left _) _ = errUriParam "CC quote"
pCCQuoteToAddress (Right aid) (Right cqid) ctx = assignCCQuoteToAddress ctx aid cqid >>= sendResp status201

-- | GET Retrieves customer mapped address if exists. Returns 200 'APITypes.Address' on success, 404 on failure.
gCustomerAddress :: EIntParam -> Context -> IO Response
gCustomerAddress (Left _) _ = errUriParam "customer"
gCustomerAddress (Right cid) ctx = retrieveCustomerAddress ctx cid >>= sendResp status200

-- | GET Retrieves vendor mapped address if exists. Returns 200 'APITypes.Address' on success, 404 on failure.
gVendorAddress :: EIntParam -> Context -> IO Response
gVendorAddress (Left _) _ = errUriParam "vendor"
gVendorAddress (Right vid) ctx = retrieveVendorAddress ctx vid >>= sendResp status200

-- | GET Retrieves CC quote mapped shipping address if exists. Returns 200 'APITypes.Address' on success, 404 on failure.
gCCAddress :: Text -> Context -> IO Response
gCCAddress pid ctx = getCCQuoteByPID ctx pid
  >>= eitherPassErr (retrieveCCQuoteAddress ctx . ccq_id)
  >>= sendResp status200
