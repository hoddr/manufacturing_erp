{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.FeedbackComplaints
Description: Controllers and handles for all feedback complaint routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all feedback complaint routes.

-}
module Controllers.FeedbackComplaints
  ( -- * Feedback Complaints
    pFeedbackComplaint
  , dFeedbackComplaint
  , gFeedbackComplaints
  , eFeedbackComplaint
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
import FeedbackComplaints ( addFeedbackComplaint
                          , deleteFeedbackComplaint
                          , editFeedbackComplaint
                          , listFeedbackComplaints
                          )

-- | POST Creates new feedback complaint. Also emails the ERP employee assigned to the complaint. Returns 201 on success.
pFeedbackComplaint :: Context -> IO Response
pFeedbackComplaint ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addFeedbackComplaint ctx >=> sendResp status201)

-- | DELETE Deletes the specified feedback complaint. Returns 204 on success.
dFeedbackComplaint :: EIntParam -> Context -> IO Response
dFeedbackComplaint (Left _) _ = errUriParam "feedback complaint"
dFeedbackComplaint (Right fcid) c = deleteFeedbackComplaint c fcid >>= sendResp status204

-- | GET List feedback complaints based on querystring. Returns 200 'APITypes.FeedbackComplaint' list on success.
gFeedbackComplaints :: Context -> IO Response
gFeedbackComplaints ctx@(Context (req, _)) =
  let pagQS = getPagQS req in
    listFeedbackComplaints ctx pagQS >>= sendResp status200

-- | PUT Edits the feedback complaint. Returns 200 on success.
eFeedbackComplaint :: EIntParam -> Context -> IO Response
eFeedbackComplaint (Left _) _ = errUriParam "feedback complaint"
eFeedbackComplaint (Right fcid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\fc -> editFeedbackComplaint ctx fc fcid >>= sendResp status200)
