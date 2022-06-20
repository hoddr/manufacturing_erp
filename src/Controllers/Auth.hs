{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Module: Controllers.Auth
Description: Controllers and handles for all authentication routes.
Copyright: (c) hoddr, 2022
License: None
Maintainer: hoddr@outlook.com

Controllers and handles for all authentication routes.

-}
module Controllers.Auth
  ( -- * Middleware
    authMiddleware
    -- * Users
  , gUsers
  , eUser
  , pUser
  , dUser
  , gUserPermissions
  , eUserPermissions
    -- * Authentication
  , loginCheck
  , logoutCheck
  , pcm
  )
  where

import Control.Monad ( (>=>) )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS64 -- ( decode )
import Database.PostgreSQL.Simple ( Connection )
import Data.Text ( Text
                 , splitOn
                 )
import Data.Text.Encoding ( decodeUtf8
                          , encodeUtf8
                          )
import Data.UUID ( UUID )
import Network.Wai ( Request
                   , Response
                   , pathInfo
                   , requestHeaders
                   )
import Network.HTTP.Types ( status200
                          , status201
                          , status204
                          )

-- LOCAL --
import APITypes ( APIError(..)
                , Context(..)
                , Token
                , UserPermissions
                , appstateuserpermissions
                , genState
                )
import Auth ( addUser
            , checkToken
            , editUser
            , editUserPassword
            , editUserPermissions
            , getUserPermissions
            , listUsers
            , login
            , logout
            , removeUser
            )
import Controllers.Utils ( EIntParam
                         , errJsonRes
                         , errReqBody
                         , errUriParam
                         , reqBodyReader
                         , sendResp
                         )
import Utils ( apiError401
             , apiError403
             , eitherPassErr
             )

{- | Authentication middleware.
THE ORDER OF ROUTES MATTERS. Any routes that do NOT get permission-checks must come first as shown.
All other routes must pass an initial Bearer token check and then pass the required user permission
level for the route.
-}
authMiddleware :: Connection
               -> UUID
               -> Request
               -> IO Context
authMiddleware conn reqId req =
  case pathInfo req of
       ["login"] -> forwardS
       ["views", _] -> forwardS
       ["js", _] -> forwardS
       ["css", _] -> forwardS
       ["favicon.ico"] -> forwardS
       ["home"] -> forwardS
       ["main"] -> forwardS
       ["index"] -> forwardS
       ["logo.png"] -> forwardS
       ["logo_small.png"] -> forwardS
       [""] -> forwardS
       [] -> forwardS
       _ -> do
         etknup <- tokenValidateHandler $ Context (req, genState conn reqId Nothing Nothing)
         case etknup of
              (Left _) -> do
                let newReq = req { pathInfo = ["unauthorized"] :: [Text] }
                pure $ Context (newReq, genState conn reqId Nothing Nothing)
              (Right (tkn, up)) -> pure $ Context (req, genState conn reqId (Just tkn) (Just up))
  where forwardS = pure $ Context (req, genState conn reqId Nothing Nothing)

-- | Helper to decode and validate the token, returning the token and the user's permissions.
tokenValidateHandler :: Context -> IO (Either APIError (Token, UserPermissions))
tokenValidateHandler c@(Context (req, _)) =
  case lookup "Authorization" $ requestHeaders req of
       Nothing -> pure $ Left $ apiError401 "Invalid token"
       (Just bs) -> checkToken c (decodeUtf8 $ BS.drop 7 bs)

-- | Permissions check middleware. Validates that the user has sufficient permissions for this route.
pcm :: Int
    -> (UserPermissions -> Int)
    -> (Context -> IO Response)
    -> Context
    -> IO Response
pcm lv upfn next ctx@(Context (_, as)) =
  let uv = case appstateuserpermissions as of
                Nothing -> -1 -- ensure failure
                Just up -> upfn up
  in
    if lv <= uv
       then next ctx
       else errJsonRes apiError403

-- | Performs the Basic authorization check for valid login username and password.
loginCheck :: Context -> IO Response
loginCheck c@(Context (req, _)) =
  case lookup "Authorization" $ requestHeaders req of
       Nothing -> errJsonRes (apiError401 "Invalid login request")
       (Just bs) -> loginHelper c (BS.drop 6 bs)

-- | Helper for login check.
loginHelper :: Context -> BS.ByteString -> IO Response
loginHelper c rawbs =
  case BS64.decode rawbs of
       (Left s) -> print s >> errJsonRes (apiError401 "Invalid login request")
       (Right bs) -> case map encodeUtf8 $ splitOn ":" $ decodeUtf8 bs of
                          [uname, rawPass] -> login c uname rawPass
                            >>= sendResp status200
                          _ -> errJsonRes (apiError401 "Invalid login request")

-- | Validates logout check (Bearer token). Passes on to 'logout'.
logoutCheck :: Context -> IO Response
logoutCheck c@(Context (req, _)) =
  case lookup "Authorization" $ requestHeaders req of
       Nothing -> errJsonRes (apiError401 "Unauthorized")
       (Just bs) ->
         checkToken c (decodeUtf8 $ BS.drop 7 bs)
         >>= eitherPassErr (\(t, _) -> logout c t)
         >>= sendResp status201

-- | GET Lists users. Returns 200 'APITypes.User' list on success.
gUsers :: Context -> IO Response
gUsers c = listUsers c >>= sendResp status200

-- | PUT Edits the user and (possibly) the password. Returns 200 on success.
eUser :: EIntParam -> Context -> IO Response
eUser (Left _) _ = errUriParam "user"
eUser (Right uid) c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (\u -> editUser c u uid
    >>= eitherPassErr (editUserPassword c)
    >>= sendResp status200)

-- | POST Creates new user. Returns 201 on success. Sets all permissions to 0 by default.
pUser :: Context -> IO Response
pUser c@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (addUser c >=> sendResp status201)

-- | DELETE Deletes the specified user. Returns 204 on success.
dUser :: EIntParam -> Context -> IO Response
dUser (Left _) _ = errUriParam "user"
dUser (Right uid) c = removeUser c uid >>= sendResp status204

-- | GET Retrieves the user's permissions. Returns 200 on success.
gUserPermissions :: EIntParam -> Context -> IO Response
gUserPermissions (Left _) _ = errUriParam "user"
gUserPermissions (Right uid) ctx = getUserPermissions ctx uid >>= sendResp status200

-- | PUT Edits the user's permissions. Returns 200 on success.
eUserPermissions :: EIntParam -> Context -> IO Response
eUserPermissions (Left _) _ = errUriParam "user"
eUserPermissions (Right uid) ctx@(Context (req, _)) = reqBodyReader req
  >>= either errReqBody (editUserPermissions ctx uid >=> sendResp status200)
