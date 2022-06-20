{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.PricingLists
Description: Controllers and handles for all pricing list routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all pricing list routes.

-}
module Controllers.PricingLists
  ( -- * Pricing Lists
    gPricingLists
  , gPricingList
  , gPricingListReport
  , pPricingList
  , dPricingList
  , dPricingListItem
  , ePricingListItems
  , gPricingListCustomers
  , pPricingListCustomer
  , dPricingListCustomer
  )
  where

import Control.Monad ( (>=>) )
import Network.Wai ( Response )
import Network.HTTP.Types ( status200
                          , status201
                          , status204
                          )

-- LOCAL --
import APITypes ( Context(..)
                , FixedPriceItem(..)
                )
import Controllers.Utils ( EIntParam
                         , errJsonRes
                         , errReqBody
                         , errUriParam
                         , reqBodyReader
                         , sendHtmlGenResp
                         , sendResp
                         )
import PricingLists ( addPricingList
                    , addPricingListCustomer
                    , deletePricingList
                    , deletePricingListCustomer
                    , getPricingListReport
                    , getPricingListById
                    , listPricingLists
                    , listPricingListCustomers
                    , removeItem
                    , setPricingListItems
                    )
import Utils ( apiError400 )

-- | GET List pricing lists. Returns 200 'APITypes.PricingList' list on success.
gPricingLists :: Context -> IO Response
gPricingLists c = listPricingLists c >>= sendResp status200

-- | GET Retrieves pricing list by id. Returns 200 'APITypes.PricingList' on success.
gPricingList :: EIntParam -> Context -> IO Response
gPricingList (Left _ ) _ = errUriParam "pricing list"
gPricingList (Right plid) c = getPricingListById c plid >>= sendResp status200

-- | GET Generate pricing list HTML report. Returns 200 and HTML on success.
gPricingListReport :: EIntParam -> Context -> IO Response
gPricingListReport (Left _) _ = errUriParam "pricing list"
gPricingListReport (Right plid) c = getPricingListReport c plid >>= sendHtmlGenResp

-- | POST Creates new pricing list (no items). Returns 201 on success.
pPricingList :: Context -> IO Response
pPricingList ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addPricingList ctx >=> sendResp status201)

-- | DELETE Deletes the specified pricing list. Returns 204 on success.
dPricingList :: EIntParam -> Context -> IO Response
dPricingList (Left _) _ = errUriParam "pricing list"
dPricingList (Right plid) c = deletePricingList c plid >>= sendResp status204

-- | DELETE Removes the given pricing list item. Returns 204 on success.
dPricingListItem :: EIntParam -> Context -> IO Response
dPricingListItem (Left _) _ = errUriParam "pricing list"
dPricingListItem (Right plid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\fpi ->
    if fpi_fkPlId fpi == plid
       then removeItem ctx fpi >>= sendResp status204
       else errJsonRes $ apiError400 "Fixed price item pricing list id and pricing list uri id mismatch")

-- | PUT Edits the given pricing list information. Returns 200 on success.
ePricingListItems :: EIntParam -> Context -> IO Response
ePricingListItems (Left _) _ = errUriParam "pricing list"
ePricingListItems (Right plid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (setPricingListItems ctx plid >=> sendResp status200)

-- | GET Retrieves pricing list customers. Returns 200 'APITypes.Customer' list on success.
gPricingListCustomers :: EIntParam -> Context -> IO Response
gPricingListCustomers (Left _) _ = errUriParam "pricing list"
gPricingListCustomers (Right plid) ctx = listPricingListCustomers ctx plid >>= sendResp status200

-- | POST Adds new customer to the pricing list. Returns 201 on success.
pPricingListCustomer :: EIntParam -> Context -> IO Response
pPricingListCustomer (Left _) _ = errUriParam "pricing list"
pPricingListCustomer (Right plid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addPricingListCustomer ctx plid >=> sendResp status201)

-- | DELETE Deletes the customer from the pricing list. Returns 204 on success.
dPricingListCustomer :: EIntParam -> EIntParam -> Context -> IO Response
dPricingListCustomer (Left _) _ _ = errUriParam "pricing list"
dPricingListCustomer _ (Left _) _ = errUriParam "customer"
dPricingListCustomer (Right plid) (Right custid) ctx = deletePricingListCustomer ctx plid custid >>= sendResp status204


