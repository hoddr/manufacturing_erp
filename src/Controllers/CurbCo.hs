{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.CC
Description: Controllers and handles for all CC routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all CC routes.

-}
module Controllers.CC
  ( -- * CC
    gCCQuotes
  , pCCQuote
  , eCCData
  , gCCQuote
  , eCCQuote
  , dCCQuote
  , gCCQuoteReport
  , pCurbToQuote
  , eCurbOnQuote
  , dCurbFromQuote
  , eCCQuoteConfirm
  , eCCQuoteNotConfirm
  , eCCQuoteToOrder
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
import APITypes ( Context(..)
                , CCQuote(..)
                )
import Controllers.Utils ( EIntParam
                         , errReqBody
                         , errUriParam
                         , getPagQS
                         , reqBodyReader
                         , sendHtmlGenResp
                         , sendResp
                         )
import CC ( addCCQuoteShell
              , addCurbToQuote
              , deleteCCQuote
              , deleteCurb
              , editCurb
              , editCCQuote
              , generateCCQuoteHTML
              , getCCQuoteByPID
              , getCCSalesData
              , listCCQuoteShells
              , setOrderIdOnCCQuote
              , toggleCCQuoteConfirmationStatus
              )
import Orders ( convertCCQuoteToOrder )
import Utils ( badReq
             , eitherPassErr
             )

-- | GET Retrieves skeletons (sans line items) of CC quotes. Returns 200 'APITypes.CCQuote' list on success.
gCCQuotes :: Context -> IO Response
gCCQuotes ctx@(Context (req, _)) = let pagQS = getPagQS req in
  listCCQuoteShells ctx pagQS >>= sendResp status200

-- | POST Creates new CC quote shell (sans address, curb line item(s)). Returns 201 'APITypes.CCQuote' on success.
pCCQuote :: Context -> IO Response
pCCQuote ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addCCQuoteShell ctx >=> sendResp status201)

-- | GET Retrieves CC quote with line items based on PID. Returns 200 'APITypes.CCQuote' on success.
gCCQuote :: Text -> Context -> IO Response
gCCQuote pid ctx = getCCQuoteByPID ctx pid >>= sendResp status200

-- | PUT Edits the specified CC quote. Returns 201 'APITypes.CCQuote' on success.
eCCQuote :: EIntParam -> Context -> IO Response
eCCQuote (Left _) _ = errUriParam "CC quote"
eCCQuote (Right cqid) ctx@(Context(req, _)) = reqBodyReader req
  >>= either errReqBody (editCCQuote ctx cqid >=> sendResp status201)

-- | DELETE Deletes the specified CC quote and its line item(s). Returns 204 on success.
dCCQuote :: Text -> Context -> IO Response
dCCQuote pid ctx = deleteCCQuote ctx pid >>= sendResp status204

-- | GET Generates and sends HTML template for CC quote. Returns 200 and HTML page on success.
gCCQuoteReport :: Text -> Context -> IO Response
gCCQuoteReport pid ctx = generateCCQuoteHTML ctx pid >>= sendHtmlGenResp

-- | POST Adds new line item curb to the quote. Returns 201 and 'APITypes.CCCurb' on success.
pCurbToQuote :: Text -> Context -> IO Response
pCurbToQuote pid ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addCurbToQuote ctx pid >=> sendResp status201)

-- | PUT Edits the curb line item entry on a quote. Returns 201 and 'APITypes.CCCurb' on success.
eCurbOnQuote :: Text -> EIntParam -> Context -> IO Response
eCurbOnQuote _ (Left _) _ = errUriParam "curb"
eCurbOnQuote _ (Right curbId) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (editCurb ctx curbId >=> sendResp status201)

-- | DELETE Removes the curb line item from the quote. Returns 204 on success.
dCurbFromQuote :: Text -> EIntParam -> Context -> IO Response
dCurbFromQuote _ (Left _) _ = errUriParam "curb"
dCurbFromQuote _ (Right curbId) ctx = deleteCurb ctx curbId >>= sendResp status204

-- | PUT Sets the CC quote to confirmed. Returns 204 on success.
eCCQuoteConfirm :: Text -> Context -> IO Response
eCCQuoteConfirm pid ctx = toggleCCQuoteConfirmationStatus ctx pid True >>= sendResp status204

-- | PUT Sets the CC quote to NOT confirmed. Returns 204 on success.
eCCQuoteNotConfirm :: Text -> Context -> IO Response
eCCQuoteNotConfirm pid ctx = toggleCCQuoteConfirmationStatus ctx pid False >>= sendResp status204

-- | POST Converts a confirmed CC quote to an order. Returns 201 on success.
eCCQuoteToOrder :: Text -- pid
                    -> Text -- ordNumb
                    -> Text -- po
                    -> Context
                    -> IO Response
eCCQuoteToOrder pid ordNum po ctx = getCCQuoteByPID ctx pid
  >>= eitherPassErr (\ccq ->
    if ccq_isConfirmed ccq
       then convertCCQuoteToOrder ctx ordNum po ccq
         >>= eitherPassErr (setOrderIdOnCCQuote ctx (ccq_id ccq))
       else badReq 400 "CC quote must be confirmed prior to converting to an order"
  )
  >>= sendResp status201

-- | PUT Retrieves CC information within given date ranges.
eCCData :: Context -> IO Response
eCCData ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (getCCSalesData ctx >=> sendResp status200)
