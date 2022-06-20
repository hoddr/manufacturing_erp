{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.Utils
Description: Helpers and utility functions for controllers.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Helpers and utility functions for controllers.

-}
module Controllers.Utils
  ( EIntParam
  , jsonRes
  , errJsonRes
  , msgJsonRes
  , sendResp
  , sendCsvResp
  , sendHtmlGenResp
  , getPagQS
  , cssRes
  , htmlRes
  , favIcon
  , logo
  , logoSmall
    -- * Utils
  , defOrderFlags
  , defStockOrderFlags
  , errReqBody
  , errUriParam
  , lookupQS
  , reqBodyReader
    -- * Responses
  , jsRes
  )
  where

import Control.Monad ( join )
import Data.Aeson ( FromJSON
                  , ToJSON
                  , eitherDecode
                  , encode
                  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as C
import Data.Maybe ( fromMaybe )
import Network.Wai ( Request
                   , Response
                   , queryString
                   , responseFile
                   , responseLBS
                   , strictRequestBody
                   )
import Network.HTTP.Types ( Status
                          , hContentType
                          , status200
                          )

-- LOCAL --
import APITypes ( APIError(..)
                , APIMsg(..)
                , Context(..)
                , QueryPag(..)
                )
import Utils ( apiError400 )

-- | Wrapper to help format any JSON responses.
jsonRes :: ToJSON a => Status -> a -> IO Response
jsonRes st dat = pure $ responseLBS st [(hContentType, "application/json")] (encode dat)

-- | Wrapper to help format any 'APIError' JSON responses.
errJsonRes :: APIError -> IO Response
errJsonRes err@(APIError st) = print (encode err) >> jsonRes st err

-- | Wrapper to help format any 'APIMsg' JSON responses.
msgJsonRes :: APIMsg -> IO Response
msgJsonRes msg@(APIMsg st) = jsonRes st msg

-- | Wrapper to send JSON responses (error or success).
sendResp :: ToJSON a => Status -> Either APIError a -> IO Response
sendResp st = either errJsonRes (jsonRes st)

-- | Wrapper to help send CSV response (error JSON or success CSV).
sendCsvResp :: (C.ToNamedRecord a, C.DefaultOrdered a) => Either APIError [a] -> IO Response
sendCsvResp = either errJsonRes csvRes

-- | Wrapper to help send HTML response (error JSON or success HTML).
sendHtmlGenResp :: Either APIError LBS.ByteString -> IO Response
sendHtmlGenResp = either errJsonRes htmlGenRes

-- | Retrieves the pagination querystring fields from the 'Request' type.
getPagQS :: Request -> QueryPag
getPagQS req = QueryPag ( getPagSearch req
                        , getPagLimit req
                        , getPagOffset req
                        )

-- | Helper to lookup querystring fields.
lookupQS :: Eq a1 => a1 -> [(a1, Maybe a2)] -> Maybe a2
lookupQS c qs = join $ lookup c qs

-- | Retrieves the pagination querystring field. 100 is default pagination.
getPagLimit :: Request -> BS.ByteString
getPagLimit = fromMaybe "100" . lookupQS "limit" . queryString

-- | Retrieves the pagination offset querystring field.
getPagOffset :: Request -> BS.ByteString
getPagOffset = fromMaybe "0" . lookupQS "offset" . queryString

-- | Retrieves the pagination search querystring field.
getPagSearch :: Request -> BS.ByteString
getPagSearch req =
  case fromMaybe "" $ lookupQS "search" $ queryString req of
       "" -> ""
       s -> "%" <> s <> "%"

-- | Wrapper to send 400 'APIError' JSON response.
errReqBody :: String -> IO Response
errReqBody e = print e >> errJsonRes (apiError400 "Invalid or empty request body")

-- | Wrapper to send invalid uri parameter 400 error.
errUriParam :: BS.ByteString -> IO Response
errUriParam bs = errJsonRes $ apiError400 $ "Invalid or missing " <> bs <> " id uri parameter"

-- | Helper to decode the request body into valid ERP data type.
reqBodyReader :: (FromJSON a) => Request -> IO (Either String a)
reqBodyReader req = strictRequestBody req >>= (pure . eitherDecode)

-- | Set of default order flags.
defOrderFlags :: (Bool, Bool, Bool)
defOrderFlags = (False, False, False)

-- | Set of stock order default flags.
defStockOrderFlags :: (Bool, Bool, Bool)
defStockOrderFlags = (False, True, True)

-- saving for potential use in parsing date strings...
-- parseDateString :: BS.ByteString -> Maybe UTCTime
-- parseDateString = parseTimeM True defaultTimeLocale "%-d-%-m-%Y" . unpack
-- import Data.Time ( UTCTime
--                      , defaultTimeLocale
--                      , parseTimeM
--                      )

-- | Wrapper for static HTML response.
htmlRes :: String -> Context -> IO Response
htmlRes fname _ = pure $ responseFile status200
                                      [(hContentType, "text/html")]
                                      ("./web/min/html/" <> fname)
                                      Nothing

-- | Wrapper for non-static HTML response.
htmlGenRes :: LBS.ByteString -> IO Response
htmlGenRes lbs = pure $ responseLBS status200
                                      [(hContentType, "text/html")]
                                      lbs

-- | Wrapper for static CSS response.
cssRes :: String -> Context -> IO Response
cssRes fname _ = pure $ responseFile status200
                                     [(hContentType, "text/css")]
                                     ("./web/min/css/" <> fname)
                                     Nothing

-- | Wrapper for static Javascript response.
jsRes :: String -> Context -> IO Response
jsRes fname _ = pure $ responseFile status200
                                    [(hContentType, "text/javascript")]
                                    ("./web/min/js/" <> fname)
                                    Nothing

-- | Wrapper for non-static CSV response.
csvRes :: (C.ToNamedRecord a, C.DefaultOrdered a) => [a] -> IO Response
csvRes d = pure
         $ responseLBS status200 [(hContentType, "text/csv")]
         $ C.encodeDefaultOrderedByName d

-- | Wrapper for icon static response.
favIcon :: Context -> IO Response
favIcon _ = pure $ responseFile status200
                                [(hContentType, "image/x-icon")]
                                "./web/favicon.png"
                                Nothing

-- | Wrapper for static logo response.
logo :: Context -> IO Response
logo _ = logoGeneric "logo.png"

-- | Wrapper for static small logo response.
logoSmall :: Context -> IO Response
logoSmall _ = logoGeneric "logo_small.png"

-- | Wrapper for generic static image response.
logoGeneric :: String -> IO Response
logoGeneric fn = pure $ responseFile status200
                        [(hContentType, "image/png")]
                        ("./web/" <> fn)
                        Nothing

-- | Type synonym for shorter signatures.
type EIntParam = Either String Int
