{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Data.Misc
Description: Miscellaneous data type declarations.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Miscellaneous data type declarations.

-}

module Data.Misc
  ( APIMsg(..)
  , APIError(..)
  , AppState(..)
  , Context(..)
  , JobExt(..)
  , contextConn
  , genState
  )
  where

import Data.Aeson ( FromJSON
                  , ToJSON
                  , object
                  , parseJSON
                  , toJSON
                  , withObject
                  , (.=)
                  , (.:?)
                  )
import Data.ByteString.Conversion ( fromByteString )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Data.Time.Format ( parseTimeM
                        , defaultTimeLocale
                        )
import Data.UUID ( UUID )
import Database.PostgreSQL.Simple ( Connection )
import Network.HTTP.Types ( Status(..) )
import Network.Wai ( Request )

-- LOCAL --
import Data.Auth ( Token
                 , UserPermissions
                 )

-- | Structure to hold app state (connection, authentication, request id) per request.
data AppState = AppState { appstateconn :: Connection
                         , appstatetoken :: Maybe Token
                         , appstateuserpermissions :: Maybe UserPermissions
                         , appstaterequestId :: UUID
                         }

-- | Helper to generate an app state instance.
genState :: Connection -> UUID -> Maybe Token -> Maybe UserPermissions -> AppState
genState conn uuid mtkn mup = AppState { appstateconn = conn
                                       , appstatetoken = mtkn
                                       , appstateuserpermissions = mup
                                       , appstaterequestId = uuid
                                       }

-- | Wrapper to hold 'Request' and 'AppState'.
newtype Context = Context (Request, AppState)

-- | Helper to retrieve the app database connection.
contextConn :: Context -> Connection
contextConn (Context (_, s)) = appstateconn s

-- API ERROR --
-- | Wrapper for an error response. Includes status code and message.
newtype APIError = APIError Status

instance ToJSON APIError where
  toJSON (APIError st) = object [ "status" .= statusCode st
                                , "msg" .= case fromByteString $ statusMessage st of
                                                  Nothing -> "Internal server error" :: Text
                                                  (Just txt) -> txt :: Text
                                ]

instance Show APIError where
  show (APIError st) = show st

-- API MSG (GENERIC) --
-- | Wrapper for generic JSON text response.
newtype APIMsg = APIMsg Status

instance ToJSON APIMsg where
  toJSON (APIMsg st) = object [ "status" .= statusCode st
                              , "msg" .= case fromByteString $ statusMessage st of
                                              Nothing -> "Internal server error" :: Text
                                              (Just txt) -> txt :: Text
                              ]

instance Show APIMsg where
  show (APIMsg st) = show st

{- | External job type (Owl app) for retrieving order status in other app. -}
data JobExt = JobExt { jobext_shippedDate :: Maybe UTCTime
                     , jobext_readyDate :: Maybe UTCTime -- completion
                     , jobext_shipType :: Maybe Text
                     } deriving (Show)

instance FromJSON JobExt where
  parseJSON = withObject "job" $ \j -> do
    sd <- j .:? "shippedDate"
    rd <- j .:? "readyDate" -- completion date
    st <- j .:? "shipType"
    pure $ JobExt { jobext_shippedDate = case sd of
                                              Nothing -> Nothing
                                              Just "0001-01-01T00:00:00" -> Nothing
                                              Just s -> parseExtAppDateTime s
                  , jobext_readyDate = case rd of
                                            Nothing -> Nothing
                                            Just "0001-01-01T00:00:00" -> Nothing
                                            Just r -> parseExtAppDateTime r
                  , jobext_shipType = st
                  }

instance ToJSON JobExt where
  toJSON j = object [ "shippedDate" .= jobext_shippedDate j
                    , "readyDate" .= jobext_readyDate j
                    , "shipType" .= jobext_shipType j
                    ]

-- | Parser for Owl app date time to UTCTime.
parseExtAppDateTime :: String -> Maybe UTCTime
parseExtAppDateTime = parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
