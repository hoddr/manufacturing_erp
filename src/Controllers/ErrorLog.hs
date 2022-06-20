{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.ErrorLog
Description: Controllers and handles for all error log routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all error log routes.

-}
module Controllers.ErrorLog
  ( -- * Error Log
    gErrorLog
  , pErrorLog
  , dErrorLog
  , eErrorLog
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
import ErrorLog ( addErrorLogEntry
                , deleteErrorLogEntry
                , editErrorLogEntry
                , listErrorLogEntries
                )

-- | GET List error log entries. Returns 200 'APITypes.ErrorLog' list on success.
gErrorLog :: Context -> IO Response
gErrorLog ctx@(Context (req, _)) = let pagQS = getPagQS req in
  listErrorLogEntries ctx pagQS >>= sendResp status200

-- | POST Creates new error log entry. Returns 201 on success.
pErrorLog :: Context -> IO Response
pErrorLog ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addErrorLogEntry ctx >=> sendResp status201)

-- | DELETE Deletes the error log entry. Returns 204 on success.
dErrorLog :: EIntParam -> Context -> IO Response
dErrorLog (Left _) _ = errUriParam "error log entry"
dErrorLog (Right elid) ctx = deleteErrorLogEntry ctx elid >>= sendResp status204

-- | PUT Edits the error log. Returns 200 on success.
eErrorLog :: EIntParam -> Context -> IO Response
eErrorLog (Left _) _ = errUriParam "error log entry"
eErrorLog (Right elid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (editErrorLogEntry ctx elid >=> sendResp status200)
