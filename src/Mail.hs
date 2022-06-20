{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Mail
Description: SMTP email wrapper.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Wrapper that provides SMTP mail support via the orders@erp.com account.
Specific environment variables need to be set appropriately prior to use. Check the production server
for the current values.

-}

module Mail
  ( -- * Functions
    emailFeedbackAssignment
  , emailPOMergeReady
  )
  where


import Data.Text ( Text )
import Data.Text.Lazy ( fromStrict
                      , pack
                      )
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Network.HaskellNet.SMTP.SSL ( AuthType(..)
                                   , Settings(..)
                                   , authenticate
                                   , defaultSettingsSMTPSTARTTLS
                                   , doSMTPSTARTTLSWithSettings
                                   , sendMail
                                   )
import Network.Mail.Mime ( Address(..)
                         , Disposition(..)
                         , Encoding(..)
                         , Mail(..)
                         , Part(..)
                         , PartContent(..)
                         , simpleMail'
                         )
import Network.Socket ( PortNumber )
import System.Environment ( lookupEnv )


-- LOCAL IMPORTS
import APITypes ( APIError )
import Utils ( apiError500
             , badReq
             , eitherPassErr
             , passSuccess
             )


-- LOCAL FNS
-- | Sets the base 'Address' data type.
ordersAddress :: Address
ordersAddress = Address { addressName = Just "ERP ERP", addressEmail = "orders@erp.com" }

-- | Basic, internal, automated message to denote that someone is assigned to a customer feedback complaint.
emailFeedbackAssignment :: Text -> Int -> IO (Either APIError ())
emailFeedbackAssignment em i =
  let mail = simpleMail' (Address { addressName = Nothing, addressEmail = em })
                         ordersAddress
                         "Feedback Complaint Assignment"
                         ("You have been assigned to feedback complaint #"
                           <> pack (show i)
                           <> ". Please check for details in the ERP."
                         )
  in getHostPortUserPassSettings >>= eitherPassErr (sender mail)

-- | Basic, internal, automated message to notify PO is received in full and prices have been verified.
emailPOMergeReady :: Text -> IO (Either APIError ())
emailPOMergeReady poNumber =
  let mail = Mail { mailFrom = ordersAddress
                  , mailTo = [ Address { addressName = Nothing, addressEmail = "hoddr@outlook.com" }
                             , Address { addressName = Nothing, addressEmail = "accounting@erp.com" }
                             ]
                  , mailCc = []
                  , mailBcc = []
                  , mailHeaders = [("Subject", "PO#: " <> poNumber <> " ready to merge pricing")]
                  , mailParts = [[Part "text/plain; charset=utf-8" QuotedPrintableText DefaultDisposition []
                    $ PartContent $ encodeUtf8 $ "PO# " <> fromStrict poNumber <> " is ready to merge pricing into the database."]]
                  }
  in getHostPortUserPassSettings >>= eitherPassErr (sender mail)

-- | Internal. Send wrapper. Over TLS.
sender :: Mail -> (String, Settings, String, String) -> IO (Either APIError ())
sender m (sn, set, un, up) = doSMTPSTARTTLSWithSettings sn set $ \conn -> do
  isAuthSuccess <- authenticate LOGIN un up conn
  if isAuthSuccess
     then sendMail m conn >> passSuccess ()
     else badReq 400 "Failed to authenticate to SMTP server"

-- | Internal. Retrieves env vars for SMTP settings.
getHostPortUserPassSettings :: IO (Either APIError (String, Settings, String, String))
getHostPortUserPassSettings = do
  msn <- lookupEnv "ERP_PB_SMTP_SERVER"
  msp <- lookupEnv "ERP_PB_SMTP_PORT"
  mun <- lookupEnv "ERP_PB_SMTP_USERNAME"
  mup <- lookupEnv "ERP_PB_SMTP_PASSWORD"
  case (msn, msp, mun, mup) of
       (Nothing, _, _, _) -> cb
       (_, Nothing, _, _) -> cb
       (_, _, Nothing, _) -> cb
       (_, _, _, Nothing) -> cb
       (Just sn, Just p, Just un, Just up) ->
         passSuccess (sn, defaultSettingsSMTPSTARTTLS { sslPort = read p :: PortNumber }, un, up)
  where cb = pure (Left apiError500)
