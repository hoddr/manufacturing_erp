{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.Customers
Description: Controllers and handles for all customer routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all customer routes.

-}
module Controllers.Customers
  ( -- * Customers
    gCustomers
  , pCustomer
  , gCustomer
  , eCustomer
  , pCustomerTaxStatusOff
  , pCustomerTaxStatusOn
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
import Customers ( addCustomer
                 , editCustomer
                 , getCustomerById
                 , listCustomers
                 , setTaxExemptFlag
                 )
import Utils ( apiError400 )

-- | GET List customers based on querystring. Returns 200 'APITypes.Customer' list on success.
gCustomers :: Context -> IO Response
gCustomers ctx@(Context (req, _)) =
  let pagQS = getPagQS req in
    listCustomers ctx pagQS >>= sendResp status200

-- | PUT Creates new customer. Returns 201 'APITypes.Customer' on success.
-- TODO consider adding in check for valid curb co customer designation.
pCustomer :: Context -> IO Response
pCustomer ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addCustomer ctx >=> sendResp status201)

-- | GET Retrieves customer by id. Returns 200 'APITypes.Customer' on success.
gCustomer :: EIntParam -> Context -> IO Response
gCustomer (Left _) _ = errJsonRes $ apiError400 "Invalid customer id uri parameter provided"
gCustomer (Right cid) ctx = getCustomerById ctx cid >>= sendResp status200

-- | PUT Edits the specified customer. Returns 200 'APITypes.Customer' on success.
---- TODO consider adding in check for valid curb co customer designation.
eCustomer :: EIntParam -> Context -> IO Response
eCustomer (Left _) _ = errJsonRes $ apiError400 "Invalid customer id uri parameter provided"
eCustomer (Right cid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\c -> editCustomer ctx c cid >>= sendResp status200)

-- | PUT Route to toggle off tax exemption flag status. Returns 204 on success.
pCustomerTaxStatusOff :: EIntParam -> Context -> IO Response
pCustomerTaxStatusOff (Left _) _ = errUriParam "customer"
pCustomerTaxStatusOff (Right cid) ctx = setTaxExemptFlag ctx cid False >>= sendResp status204

-- | PUT Route to toggle ON tax exemption flag status. Returns 204 on success.
pCustomerTaxStatusOn :: EIntParam -> Context -> IO Response
pCustomerTaxStatusOn (Left _) _ = errUriParam "customer"
pCustomerTaxStatusOn (Right cid) ctx = setTaxExemptFlag ctx cid True >>= sendResp status204
