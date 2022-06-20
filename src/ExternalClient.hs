{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: ExternalClient
Description: External HTTP calls.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Provides functions and calls for any external HTTP requests (e.g. to old app). Current purpose is to bridge the transition gap, easing repetitive tasks requiring information in both apps.

-}

module ExternalClient
  ( -- * Functions
    getOrderCompletionStatus
  )
  where


import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( eitherDecode )
import Data.ByteString.Char8 ( pack )
import Data.Text ( Text )
import Network.HTTP.Req


-- LOCAL IMPORTS --
import APITypes ( APIError
                , JobExt(..)
                )
import Utils ( badReq
             , passSuccess
             )

-- | OWL app server base uri.
owlAppHost :: Text
owlAppHost = "app.erp.com"

-- | Retrieves basic job (i.e. order) information from Owl app, primarily for getting state of job.
getOrderCompletionStatus :: Text -> Text -> IO (Either APIError (Maybe JobExt))
getOrderCompletionStatus key ordNumber = liftIO $ runReq defaultHttpConfig $ do
  res <- req GET
             (https owlAppHost /: "api" /: "job" /: ordNumber /: "is-complete-shipped")
             NoReqBody
             lbsResponse
             ("key" =: key)
  let ej = (eitherDecode (responseBody res) :: Either String JobExt) in
    case ej of
         (Left e) -> liftIO $ badReq 400 (pack e)
         (Right j) -> let flag = case (jobext_shippedDate j, jobext_readyDate j) of
                                      (Nothing, Nothing) -> Nothing
                                      _ -> Just j
                      in liftIO $ passSuccess flag
